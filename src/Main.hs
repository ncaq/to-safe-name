import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           System.Directory
import           System.FilePath.Posix
import           System.IO
import           System.Posix.Files
import           System.Process

main :: IO ()
main = do
       judge <- check >>= affirmation
       when judge allOrderRename

affirmation :: Bool -> IO Bool
affirmation False = putStrLn "no jobs" >> return False
affirmation True = do
  putStr "Do you want to run ? (y/n):"
  hFlush stdout
  input <- getChar
  case input of
    'y' -> return True
    'n' -> return False
    _   -> return False

check :: IO Bool
check  = do
  currdir <- getCurrentDirectory
  putStr "max depth = "
  maxDepth currdir >>= print
  a <- shellFind currdir
  mapM_ putStrLn a
  putStrLn "↓"
  mapM_ putStrLn $ fmap toSafeString a
  case a of
    [] -> return False
    _  -> return True

allOrderRename :: IO ()
allOrderRename = do
  currdir <- getCurrentDirectory
  maxdepth <- maxDepth currdir
  recursion currdir 1 maxdepth
  where recursion currdir depth maxdepth = do
          shellFindDepth currdir depth >>= mapM_ renameToSafe
          when (depth < maxdepth) $ recursion currdir (depth + 1) maxdepth

maxDepth :: FilePath -> IO Int
maxDepth dir = do
                      dirList <- lines <$> readProcess "find" [dir] []
                      return $ foldr (\s n -> max (countSlash s) n) 0 dirList - countSlash dir

countSlash :: String -> Int
countSlash = foldr (\s n -> case s of
                       '/' -> n + 1
                       _ -> n)
             0

shellFind :: FilePath -> IO [String]
shellFind dir = lines <$> readProcess "find" [dir, "-regex", illRegex] []

shellFindDepth :: FilePath -> Int -> IO [String]
shellFindDepth dir depthLevel = lines <$> readProcess "find" [dir, "-maxdepth", show depthLevel, "-regex", illRegex] []

illRegex :: String
illRegex = ".*[" ++ foldr ((:) . fst) "" replaceTable ++ "].*"

renameToSafe :: FilePath -> IO ()
renameToSafe name = rename name (pathToSafeFile name)

pathToSafeFile :: FilePath -> FilePath
pathToSafeFile path =
  let safeFileName = toSafeString $ takeFileName path
      dirName = takeDirectory path
      in
   dirName ++ "/" ++ safeFileName

toSafeString :: String -> String
toSafeString = map toSafeChar

toSafeChar :: Char -> Char
toSafeChar key = fromMaybe key (lookup key replaceTable)

replaceTable :: [(Char, Char)]
replaceTable = [ ('\\', '＼')
               , ('?', '？')
               , (':', '：')
               , ('*', '＊')
               , ('"', '”')
               , ('>', '＞')
               , ('<', '＜')
               , ('|', '｜')
               , ('　', ' ')
               ]
