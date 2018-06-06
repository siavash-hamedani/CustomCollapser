module Text.PrettyPrint.CustomCollapser where

import Text.Parsec
import qualified Data.Map as DM
import qualified Data.List as DL
import qualified Control.Monad.State as ST
import Data.Maybe


data TextFold
  = TF Int [TextFold]  -- ^ TF is TextFold's parsed tree
  | TN String          -- ^ TN is a Node of a TextFold tree that contains the content
  deriving Show


collapseShow' stt lft rht mk (TF mid mbod) = case DM.lookup mid mk  of
  Nothing -> case stt of
    False -> lft++(show mid)++rht
    True -> dbmode
  Just _ -> dbmode
  where
    dbmode = lft++ (DL.foldr (++) "" $ map (collapseShow' stt lft rht mk) mbod) ++rht
collapseShow' stt lft rht mk (TN s)= s

collapseShow :: Bool      -- ^ True : show all , False : use exapndable list
  -> String -> String
  -> [Int]                -- ^ list of the nodes you want expanded
  ->  TextFold ->  String
collapseShow stt lft rht mk tf = collapseShow' stt lft rht (DM.fromList (zip mk (repeat ())))  tf




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




takeCollapseOut :: Int -> TextFold -> Maybe TextFold
takeCollapseOut mid x@(TN _) = Nothing
takeCollapseOut mid x@(TF tid []) = Nothing
takeCollapseOut mid x@(TF tid y) | mid == tid = Just x
                                 | otherwise  = case DL.filter isJust $ DL.map (takeCollapseOut mid) y of
                                     [Just j] -> Just j
                                     [] -> Nothing
                                     _ -> error "should not happend --- something wrong in indexing that multiple nodes found"
