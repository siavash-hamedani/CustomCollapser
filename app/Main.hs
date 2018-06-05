module Main where

import System.Environment
import Text.PrettyPrint.CustomCollapser
import Options.Applicative
import Data.Semigroup ((<>))

data CollapseOption = CollapseOption {
  expandables :: [Int],
  leftSymbol :: String,
  rightSymbol :: String,
  outLeftSymbol :: String,
  outRightSymbol :: String
  } deriving Show


collapseOption :: Parser CollapseOption
collapseOption = CollapseOption
                 <$> ((option (auto :: ReadM [Int])) (long "expandables"
                                      <> short 'e'
                                      <> metavar "TARGET"
                                      <> help "expandables") <|> (pure [0]))
                 <*> (strOption (long "left"
                                      <> short 'l'
                                      <> metavar "TARGET"
                                      <> help "left") <|> pure "{{{")
                 <*> (strOption (long "right"
                                      <> short 'r'
                                      <> metavar "TARGET"
                                      <> help "Right") <|> pure "}}}")
                 <*> (strOption (long "outleft"
                                      <> short 'L'
                                      <> metavar "TARGET"
                                      <> help "Outleft") <|> pure "{{{")
                 <*> (strOption (long "outright"
                                      <> short 'R'
                                      <> metavar "TARGET"
                                      <> help "OutRight") <|> pure "}}}")


main =
  let
  opts = info (collapseOption <**> helper) (fullDesc)
  in
    do
      inp <- execParser opts
      cont <- getContents
      case runCollapse (leftSymbol inp) (rightSymbol inp) cont of
        Left e -> print e
        Right tf -> putStrLn $ collapseShow (outLeftSymbol inp) (outRightSymbol inp) (expandables inp) tf
