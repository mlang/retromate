{-# LANGUAGE LambdaCase #-}
module Main (main) where
import           Control.Monad       (forM_, mapM_, when)
import           Data.List           (foldl')
import           Data.Tree           (Tree (..))
import qualified Data.Vector.Unboxed as Vector
import           Game.Chess          (Ply, Position, color, inCheck,
                                      legalPlies', startpos, unsafeDoPly)
import           Game.Chess.PGN      (Annotated (..), Game (..), Outcome (..),
                                      PGN (..), readPGNFile)
import           Game.Chess.SAN      (varToSAN)
import           System.Directory    (listDirectory)
import           System.FilePath     (takeExtension, (</>))



pgnFiles :: FilePath -> IO [FilePath]
pgnFiles dir = map (dir </>) . filter isPGN <$> listDirectory dir where
  isPGN file = takeExtension file == ".pgn"

process :: Game -> IO ()
process g = do
  let line = mainline $ _cgForest g
  let pos = foldl' unsafeDoPly startpos line
  when (checkmate pos) $
    putStrLn . varToSAN startpos $ line

main :: IO ()
main = pgnFiles "/home/mlang/Chess/polytwic/twic" >>= mapM_ go where
  go fp = readPGNFile fp >>= \case
    Left err -> putStrLn err
    Right (PGN games) -> forM_ games $ \game -> case _cgOutcome game of
      Win _ -> process game
      _     -> pure ()

mainline :: [Tree (Annotated Ply)] -> [Ply]
mainline (Node x xs:_) = _annPly x : mainline xs
mainline []            = []

checkmate :: Position -> Bool
checkmate p = inCheck (color p) p && Vector.length (legalPlies' p) == 0
