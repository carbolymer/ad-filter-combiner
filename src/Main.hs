module Main where

import           Control.Exception (try)
import           Control.Monad (forM, liftM)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import qualified Data.ByteString.Lazy as L
import           Data.List
import           Network.HTTP.Conduit
import           System.IO

import           FilterListsParser (loadFilterLists, FilterList, url, enabled)

lf = BS.c2w '\n'
putStr' arg = putStr arg >> hFlush stdout
putStrLn' arg = putStrLn arg >> hFlush stdout
print' arg    = print arg >> hFlush stdout

-- | Loads filter list from a file
loadFilterListsFromFile :: String -> IO (Either String [FilterList])
loadFilterListsFromFile fileName = do
    filterListJson <- L.readFile fileName
    return $ loadFilterLists filterListJson

-- | Downloads and merges filter list
downloadAndMergeLists :: Either String [FilterList] -> IO (Either String L.ByteString)
downloadAndMergeLists filterLists = do
    listOfLists <- downloadLists' filterListsUrls
    return $ mergeLists <$> listOfLists
    where
        enabledLists    = filter enabled <$> filterLists
        filterListsUrls = map url <$> enabledLists
        downloadLists'  = mapM downloadLists


-- | Downloads lists of rules from provided list of urls
downloadLists :: [String] -> IO [[L.ByteString]]
downloadLists urlList   = extractLines responses where
    extractLines        = liftM $ map ( L.split lf )
    responses           = getResponses urlList
    getResponses        = mapM safeDownload


-- | Downloads rules list from URL, prints error if any occurs
safeDownload :: String -> IO L.ByteString
safeDownload url = do
        putStr' $ "downloading rules from: " ++ url
        response <- try $ simpleHttp url
        case response of
            Left e -> do
                putStrLn' ""
                print' (e :: HttpException)
                return L.empty
            Right response -> do
                putStrLn' " [DONE]"
                return response


-- | Merges rules to unique list
-- takes List of list of rules, merges them, removes comments and creates unique list, which can be written to the file
mergeLists :: [[L.ByteString]] -> L.ByteString
mergeLists = L.intercalate (L.singleton lf) . concat


listsFileName = "filter-lists.json"
outputFileName = "rules.txt"


main = do
    putStrLn' "adblock filters combiner"
    filterLists <- loadFilterListsFromFile listsFileName
    putStrLn' $ "loaded lists from " ++ listsFileName
    outputList <- downloadAndMergeLists filterLists
    case outputList of
        Left e              -> putStrLn' e
        Right mergedList    -> do
            L.writeFile outputFileName mergedList
            putStrLn' "lists download completed"

