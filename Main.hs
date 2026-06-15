module Main where

import Sol

main :: IO ()
main = do
  maybeDataset <- loadPokeSet "pokemon.json"
  case maybeDataset of
    Nothing -> putStrLn "Failed to load pokemon.json"
    Just ds -> do
      putStrLn $ "Total Pokemon: " ++ show (length (getPokeSet ds))
      putStrLn $ "Average weight: " ++ show (pokeAvgWeight ds)
      putStrLn $ "Correlation between weight and height: " ++ show (pokeCorr ds)
      putStrLn $ "Pokemon with most weaknesses: " ++ show (pokeWeakest ds)
