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


data Mode = Collapse CollapseOption | TakeOut Int CollapseOption deriving Show

collapseModeOption :: Parser Mode
collapseModeOption = Collapse <$> collapseOption

takeOutModeOption :: Parser Mode
takeOutModeOption = TakeOut <$> (option auto (long "TakeOutMode"
                                 <> short 'T'
                                 <> metavar "TARGET"
                                 <> help "TakeOutMode") <|> pure 0)
                    <*> collapseOption



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
  opts = info ((collapseModeOption <|> takeOutModeOption) <**> helper) (fullDesc)
  in
    do
      inp <- execParser opts
      cont <- getContents
      case inp of
        TakeOut tid colopts -> case runCollapse (leftSymbol colopts) (rightSymbol colopts) cont of
            Left e -> print e
            Right tf -> case takeCollapseOut tid tf of
                          Nothing -> print $ "bad index "++ show tid
                          Just inner -> putStrLn $ collapseShow True (outLeftSymbol colopts) (outRightSymbol colopts) (expandables colopts) inner
        Collapse colopts ->
          case runCollapse (leftSymbol colopts) (rightSymbol colopts) cont of
            Left e -> print e
            Right tf -> putStrLn $ collapseShow False (outLeftSymbol colopts) (outRightSymbol colopts) (expandables colopts) tf
