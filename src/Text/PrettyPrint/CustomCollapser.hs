module Text.PrettyPrint.CustomCollapser where

import Text.Parsec
import qualified Data.Map as DM
import qualified Data.List as DL
import qualified Control.Monad.State as ST


data TextFold
  = TF Int [TextFold]  -- ^ TF is TextFold's parsed tree
  | TN String          -- ^ TN is a Node of a TextFold tree that contains the content
  deriving Show


collapseShow' lft rht mk (TF mid mbod) = case DM.lookup mid mk of
  Nothing -> lft++(show mid)++rht
  Just _ -> lft++ (DL.foldr (++) "" $ map (collapseShow' lft rht mk) mbod) ++rht
collapseShow' lft rht mk (TN s)= s

collapseShow :: String -> String
  -> [Int] -- ^ list of the nodes you want expanded
  ->  TextFold ->  String
collapseShow lft rht mk tf = collapseShow' lft rht (DM.fromList (zip mk (repeat ())))  tf

untilMatching :: Int -> String -> String -> String -> ParsecT String () (ST.State Int) String
untilMatching curi mres surs sure = do
  case curi of
    0 -> return mres
    _ -> do
      a <- manyTill anyChar (choice [lookAhead $ string surs,lookAhead $ string sure])
      b <- (choice [string surs,string sure])
      case b of
        tt | tt == surs -> untilMatching (curi + 1) (mres ++ a ++ tt) surs sure
           | tt == sure -> case curi of
               1 -> untilMatching (curi - 1) (mres ++ a) surs sure
               _ -> untilMatching (curi - 1) (mres ++ a ++ tt) surs sure
           | otherwise ->  untilMatching curi (mres ++ a ++ tt) surs sure

splitterTF :: String -> String -> ParsecT  String () (ST.State Int) [TextFold]
splitterTF surs sure = do
  a <- manyTill anyChar (choice [try $ lookAhead $ string surs,eof >> string ""])
  b <- optionMaybe eof
  case b of
    Just _ -> return $ [TN a]
    Nothing -> do
      headtn <- parseCollapse surs sure
      rst <- splitterTF surs sure
      return $ [TN a] ++ [headtn]  ++ rst

parseCollapse :: String -> String -> ParsecT  String () (ST.State Int) TextFold
parseCollapse surs sure = do
  _ <- string surs
  q <- untilMatching 1 "" surs sure
  za <- ST.get
  let (Right ss,newst) = flip ST.runState (za+1) $ runParserT (splitterTF surs sure) () "" q
  ST.put newst
  return $ TF za ss

runCollapse ::
  String     -- ^ left symbol of the text holder
  -> String  -- ^ right symbol of the text holder
  -> String  -- ^ input that you want collapsed
  -> Either ParseError TextFold 
runCollapse lft rht cont = fst $ flip ST.runState 0 $ runParserT (parseCollapse lft rht) () "" cont


