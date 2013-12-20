{-# LANGUAGE OverloadedStrings #-}
import Text.Pandoc
import System.Directory
import System.Environment
import Data.List(isSuffixOf)

readDoc   = readHtml def
writeDoc  = writeMarkdown def

main = do
  contents <- getDirectoryContents "posts/"
  htmls    <- mapM (readFile) $ map  ("posts/" ++) $ filter (isSuffixOf ".html") contents
  mds      <- mapM (writeDoc . readDoc) htmls
  mapM_ print mds