{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Data.IORef (newIORef, readIORef, writeIORef)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan
import qualified Game.Chess.UCI as UCI
import           Control.Monad       (void, forever, forM_, mapM_, when)
import           Data.List           (find, foldl')
import           Data.Tree           (Tree (..))
import qualified Data.Vector.Unboxed as Vector
import           Game.Chess          (Ply, Position, color, inCheck,
                                      legalPlies', startpos, unsafeDoPly)
import           Game.Chess.PGN      (Annotated (..), Game (..), Outcome (..),
                                      PGN (..), readPGNFile)
import           Game.Chess.SAN      (varToSAN)
import           System.Directory    (listDirectory)
import           System.FilePath     (takeExtension, (</>))
import System.Timeout (timeout)


process :: UCI.Engine -> ([Ply], Ply) -> IO ()
process engine (line, bm) = search engine (init . init $ line) >>= \case
  Nothing -> putStrLn . varToSAN startpos $ line <> [bm]
  Just (line', bm') -> process engine (line', bm')

search :: UCI.Engine -> [Ply] -> IO (Maybe ([Ply], Ply))
search engine line = do
  UCI.setPosition engine startpos line
  result <- newIORef Nothing
  (_, ic) <- UCI.search engine [UCI.infinite]
  timeout 20000000 $ findMate ic >>= \ply -> writeIORef result $ Just (line, ply)
  UCI.stop engine
  UCI.isready engine
  readIORef result

findMate ic = do
  i <- atomically (readTChan ic)
  case (find isScore i, find isPV i) of
    (Just (UCI.Score (UCI.MateIn mi) _), Just (UCI.PV pv)) -> do
      putStrLn $ show mi <> " " <> show pv
      pure $ pv Vector.! 0
    _ -> findMate ic

main :: IO ()
main = UCI.start "stockfish" [] >>= \case
  Nothing -> putStrLn "Failed to start engine"
  Just engine -> do
    UCI.setOptionSpinButton "Threads" 8 engine
    pgnFiles "/home/mlang/Chess/polytwic/twic" >>= mapM_ (scanPGN engine)

scanPGN :: UCI.Engine -> FilePath -> IO ()
scanPGN engine fp = readPGNFile fp >>= \case
  Left err -> putStrLn err
  Right (PGN games) -> forM_ games $ \game -> case _cgOutcome game of
    Win _ -> do
      let line = mainline $ _cgForest game
      let pos = foldl' unsafeDoPly startpos line
      when (checkmate pos) $ process engine (init line, last line)
    _     -> pure ()

mainline :: [Tree (Annotated Ply)] -> [Ply]
mainline (Node x xs:_) = _annPly x : mainline xs
mainline []            = []

checkmate :: Position -> Bool
checkmate p = inCheck (color p) p && Vector.length (legalPlies' p) == 0

pgnFiles :: FilePath -> IO [FilePath]
pgnFiles dir = map (dir </>) . filter isPGN <$> listDirectory dir where
  isPGN fp = takeExtension fp == ".pgn"

isScore UCI.Score{} = True
isScore _ = False

isPV UCI.PV{} = True
isPV _ = False
