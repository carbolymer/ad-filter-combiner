module Main where

import           Control.Exception (try)
import           Control.Monad (forM, liftM)
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import qualified Data.ByteString.Lazy as L
import           Data.List
import           Network.HTTP.Conduit

import           FilterListsParser (JsonFilterList, loadFilterLists)


lf = BS.c2w '\n'

-- | takes List of list of rules, merges them, removes comments and creates unique list, which can be written to
-- the file
mergeLists :: [[L.ByteString]] -> L.ByteString
mergeLists = L.intercalate (L.singleton lf) . concat

-- | Downloads rules list from URL, prints error if any occurs
safeRetrieve :: String -> IO L.ByteString
safeRetrieve url = do
        response <- try $ simpleHttp url
        case response of
            Left e -> do
                print (e :: HttpException)
                return L.empty
            Right response -> do
                putStrLn $ "downloaded rules from: " ++ url
                return response

-- | Retrieves lists of rules from provided list of urls
retrieveLists :: [String] -> IO [[L.ByteString]]
retrieveLists urlList   = extractLines responses where
    extractLines        = liftM $ map ( L.split lf )
    responses           = getResponses listWithoutComments
    getResponses        = mapM safeRetrieve
    listWithoutComments = filter isNotComment urlList
    isNotComment        = not . isPrefixOf "#"


getFilterListsUrls :: [FilterList] -> [String]
getFilterListsUrls = undefined


listsFileName = "filter-lists.json"
outputFileName = "rules.txt"

main = do
    lists <- L.readFile listsFileName >>= ( return . loadFilterLists )
    filterLists <- retrieveLists $ getFilterListsUrls lists
    L.writeFile outputFileName $ mergeLists filterLists
    putStrLn "done"
