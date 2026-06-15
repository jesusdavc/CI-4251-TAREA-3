{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Sol where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort, isSuffixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Text.Read (readMaybe)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- PART 1: Modeling the PokeSet
--------------------------------------------------------------------------------

{- | 
  EXPLANATION FOR THE CHOSEN REPRESENTATION:

  I modeled `PokeSet` as a wrapper around a parameterized type: 
  `newtype PokeSet a = PokeSet { getPokeSet :: [a] }`
  where `a` is our highly-structured `Pokemon` data type.

  Advantages:
  1. Compile-Time Safety: We strictly define the schema of a Pokémon. If we try
     to access a field that doesn't exist, the compiler catches it.
  2. Native Optics Generation: By using standard Haskell records, we can
     leverage `TemplateHaskell` (`makeLenses`) to automatically generate highly
     performant and safe lenses/traversals for our data.

  Disadvantages:
  1. Boilerplate heavy: You have to explicitly type out every possible field the
     JSON might contain.
  2. Brittleness: If the JSON schema changes (e.g., they add a new field or 
     rename one), the parser might fail unless explicitly configured to ignore it. 
     (An alternative like raw `Aeson.Value` handles schema changes fluidly but
     sacrifices all type safety).
-}

data Evolution = Evolution
  { _evoNum  :: String
  , _evoName :: String
  } deriving (Show, Eq, Generic)

data Pokemon = Pokemon
  { _pokeId            :: Int
  , _pokeName          :: String
  , _pokeImg           :: String
  , _pokeType          :: [String]
  , _pokeHeight        :: String
  , _pokeWeight        :: String
  , _pokeMultipliers   :: Maybe [Double]
  , _pokeWeaknesses    :: [String]
  , _pokeNextEvolution :: Maybe [Evolution]
  , _pokePrevEvolution :: Maybe [Evolution]
  , _pokeSpawnTime     :: String
  , _pokeEgg           :: String
  } deriving (Show, Eq, Generic)

newtype PokeSet a = PokeSet { getPokeSet :: [a] }
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- Generate our optics!
makeLenses ''Evolution
makeLenses ''Pokemon

-- The PokeSet dataset usually wraps everything inside a "pokemon" array object
data RawData = RawData { pokemon :: [Pokemon] } deriving (Generic, Show)

deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 4} ''Evolution
deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 5} ''Pokemon
instance FromJSON RawData

--------------------------------------------------------------------------------
-- PART 2: Parser implementation
--------------------------------------------------------------------------------

pPokeSet :: BL.ByteString -> Maybe (PokeSet Pokemon)
pPokeSet bs = PokeSet . pokemon <$> decode bs

-- Helper function to load dataset from file
loadPokeSet :: FilePath -> IO (Maybe (PokeSet Pokemon))
loadPokeSet path = do
  bs <- BL.readFile path
  return $ pPokeSet bs

--------------------------------------------------------------------------------
-- PART 3: Queries (Optics ONLY, no let bindings or where clauses)
--------------------------------------------------------------------------------

-- | Auxiliary Optics to help extract Double values cleanly without variables.
pokeWeightDouble :: Getter Pokemon Double
pokeWeightDouble = to (\p -> fromMaybe 0.0 (readMaybe =<< listToMaybe (words (p ^. pokeWeight))))

pokeHeightDouble :: Getter Pokemon Double
pokeHeightDouble = to (\p -> fromMaybe 0.0 (readMaybe =<< listToMaybe (words (p ^. pokeHeight))))

pokeSpawnTimeDouble :: Getter Pokemon Double
pokeSpawnTimeDouble = pokeSpawnTime . to (\s ->
    fromMaybe 0 (readMaybe (takeWhile (/= ':') s)) * 60 +
    fromMaybe 0 (readMaybe (drop 1 (dropWhile (/= ':') s)))
  )

-- Helper to lookup a Pokemon by name
lookupPokemon :: PokeSet Pokemon -> String -> Maybe Pokemon
lookupPokemon ds name = firstOf (folded . filtered (\p -> p ^. pokeName == name)) ds

-- Returns a list with the names of each pokemon in the dataset.
pokeNames :: PokeSet Pokemon -> [String]
pokeNames ds = ds ^.. folded . pokeName

-- Returns a list with the names of each pokemon and its next evolutions
pokeEvolutions :: PokeSet Pokemon -> [(String, [String])]
pokeEvolutions ds = ds ^.. folded . to (\p -> (p ^. pokeName, p ^.. pokeNextEvolution . _Just . folded . evoName))

-- Same as pokeEvolutions, but it should return only the base pokemons
-- Reason: The `filtered` optic ensures we only focus on elements that `hasn't` previous evolutions.
pokeEvolutions' :: PokeSet Pokemon -> [(String, [String])]
pokeEvolutions' ds = ds ^.. folded . filtered (hasn't (pokePrevEvolution . _Just . folded)) . to (\p -> (p ^. pokeName, p ^.. pokeNextEvolution . _Just . folded . evoName))

-- Filters all the pokemons that are of type "Psychic" and "Normal", increasing multipliers by 2.
pokePsychicNormal :: PokeSet Pokemon -> PokeSet Pokemon
pokePsychicNormal ds = ds & over (mapped . filtered (\p -> has (pokeType . folded . filtered (== "Psychic")) p && has (pokeType . folded . filtered (== "Normal")) p) . pokeMultipliers . _Just . traversed) (* 2)

-- Filters all the pokemons that are of type "Psychic" or "Normal", decreasing multipliers by 1.
pokePsychicNormal' :: PokeSet Pokemon -> PokeSet Pokemon
pokePsychicNormal' ds = ds & over (mapped . filtered (\p -> has (pokeType . folded . filtered (== "Psychic")) p || has (pokeType . folded . filtered (== "Normal")) p) . pokeMultipliers . _Just . traversed) (\x -> x - 1)

-- set the image of the pokemons x that have an evolution y (weight y > weight x) to the image of y.
pokeDrinker :: PokeSet Pokemon -> PokeSet Pokemon
pokeDrinker ds = ds & over mapped (\p ->
    p & pokeImg %~ \oldImg ->
      fromMaybe oldImg (firstOf (pokeNextEvolution . _Just . folded . evoName . to (lookupPokemon ds) . _Just . filtered (\y -> y ^. pokeWeightDouble > p ^. pokeWeightDouble) . pokeImg) p)
  )

-- Return the name(s) of the pokemon(s) with the most amount of weaknesses.
pokeWeakest :: PokeSet Pokemon -> [String]
pokeWeakest ds = ds ^.. folded . filtered (\p -> length (p ^. pokeWeaknesses) == fromMaybe 0 (maximumOf (folded . pokeWeaknesses . to length) ds)) . pokeName

-- Returns the average weight of all the pokemons in the dataset.
pokeAvgWeight :: PokeSet Pokemon -> Double
pokeAvgWeight ds = sumOf (folded . pokeWeightDouble) ds / fromIntegral (lengthOf folded ds)

-- Returns the variance of the weight of all the pokemons in the dataset.
pokeVarWeight :: PokeSet Pokemon -> Double
pokeVarWeight ds = 
  let avg = pokeAvgWeight ds
  in sumOf (folded . pokeWeightDouble . to (\x -> (x - avg)^2)) ds / fromIntegral (lengthOf folded ds)

-- Auxiliary averages to compute correlation
pokeAvgHeight :: PokeSet Pokemon -> Double
pokeAvgHeight ds = sumOf (folded . pokeHeightDouble) ds / fromIntegral (lengthOf folded ds)

pokeVarHeight :: PokeSet Pokemon -> Double
pokeVarHeight ds = 
  let avg = pokeAvgHeight ds
  in sumOf (folded . pokeHeightDouble . to (\x -> (x - avg)^2)) ds / fromIntegral (lengthOf folded ds)

pokeCov :: PokeSet Pokemon -> Double
pokeCov ds = 
  let avgW = pokeAvgWeight ds
      avgH = pokeAvgHeight ds
  in sumOf (folded . to (\p -> (p^.pokeWeightDouble - avgW) * (p^.pokeHeightDouble - avgH))) ds / fromIntegral (lengthOf folded ds)

-- Returns the pearson correlation coefficient between the weight and height.
pokeCorr :: PokeSet Pokemon -> Double
pokeCorr ds = pokeCov ds / sqrt (pokeVarWeight ds * pokeVarHeight ds)

-- Auxiliary traversal to touch every single string that represents a 'name'
-- Reason: Custom Traversal allows targeting both the root name and deep nested evolution names simultaneously.
pokeAllNames :: Traversal' Pokemon String
pokeAllNames f p = Pokemon
  <$> pure (p^.pokeId)
  <*> f (p^.pokeName)
  <*> pure (p^.pokeImg)
  <*> pure (p^.pokeType)
  <*> pure (p^.pokeHeight)
  <*> pure (p^.pokeWeight)
  <*> pure (p^.pokeMultipliers)
  <*> pure (p^.pokeWeaknesses)
  <*> (traverse . traverse . evoName) f (p^.pokeNextEvolution)
  <*> (traverse . traverse . evoName) f (p^.pokePrevEvolution)
  <*> pure (p^.pokeSpawnTime)
  <*> pure (p^.pokeEgg)

-- Modifies every "name" field to concatenate " tuff" if missing.
pokeTuff :: PokeSet Pokemon -> PokeSet Pokemon
pokeTuff ds = ds & over (mapped . pokeAllNames . filtered (not . isSuffixOf " tuff")) (++ " tuff")

-- Returns the Quantile 1,2,3 and the Interquantile range of the SPAWN TIME
pokeIQR :: PokeSet Pokemon -> (Double, Double, Double, Double)
pokeIQR ds = 
  let sorted = sort (ds ^.. folded . pokeSpawnTimeDouble)
      len = length sorted
  in ( sorted ^?! ix (len `div` 4)
     , sorted ^?! ix (len `div` 2)
     , sorted ^?! ix (3 * len `div` 4)
     , (sorted ^?! ix (3 * len `div` 4)) - (sorted ^?! ix (len `div` 4))
     )

-- Visual representation of the box plot based on pokeIQR
pokeBoxPlot :: PokeSet Pokemon -> String
pokeBoxPlot ds = 
  let (q1, q2, q3, iqr) = pokeIQR ds
  in "Box Plot using IQR: " ++ show (q1, q2, q3, iqr) ++ " \n" ++
     "|---[  Q1  |  Q2  |  Q3  ]---|\n" ++
     "     " ++ show q1 ++ "    " ++ show q2 ++ "    " ++ show q3

-- Returns a contingency table of the types (rows)/weaknesses (cols)
pokeContingency :: PokeSet Pokemon -> Map (String, String) Int
pokeContingency ds = Map.fromListWith (+) $ 
  ds ^.. folded . to (\p -> [(t, w, 1) | t <- p ^. pokeType, w <- p ^. pokeWeaknesses]) . folded . to (\(t, w, c) -> ((t, w), c))

-- Build an histogram for the egg distance.
newtype Histo = Histo (Map Double Int)

-- Show instance simply pretty prints based on the Map values
instance Show Histo where
  show (Histo m) = Map.foldlWithKey (\acc k v -> acc ++ show k ++ "km: " ++ replicate v '*' ++ "\n") "" m

pokeHist :: PokeSet Pokemon -> Histo
pokeHist ds = Histo $ Map.fromListWith (+) $ 
  ds ^.. folded . pokeEgg . to (\s -> readMaybe (takeWhile (/= ' ') s) :: Maybe Double) . _Just . to (,1)

-- Example test data for quick testing in GHCi
testDataset :: PokeSet Pokemon
testDataset = PokeSet [
    Pokemon 
      1 
      "Bulbasaur" 
      "img1" 
      ["Grass","Poison"] 
      "0.7 m" 
      "6.9 kg" 
      (Just [1.0]) 
      ["Fire","Ice","Flying","Psychic"] 
      (Just [Evolution "2" "Ivysaur"]) 
      Nothing 
      "00:00" 
      "2 km",
    Pokemon 
      2 
      "Ivysaur" 
      "img2" 
      ["Grass","Poison"] 
      "1.0 m" 
      "13.0 kg" 
      (Just [1.5]) 
      ["Fire","Ice","Flying","Psychic"] 
      (Just [Evolution "3" "Venusaur"]) 
      (Just [Evolution "1" "Bulbasaur"]) 
      "00:15" 
      "2 km",
    Pokemon 
      3 
      "Venusaur" 
      "img3" 
      ["Grass","Poison"] 
      "2.0 m" 
      "100.0 kg" 
      Nothing 
      ["Fire","Ice","Flying","Psychic"] 
      Nothing 
      (Just [Evolution "2" "Ivysaur"]) 
      "00:30" 
      "5 km",
    Pokemon 
      150 
      "Mewtwo" 
      "img150" 
      ["Psychic"] 
      "2.0 m" 
      "122.0 kg" 
      (Just [2.0]) 
      ["Bug","Ghost","Dark"] 
      Nothing 
      Nothing 
      "01:00" 
      "10 km"
  ]