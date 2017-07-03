{-# LANGUAGE DeriveGeneric #-}

module FilterListsParser
(
    loadFilterLists,
    -- accessors
    FilterList  ,
    url         ,
    enabled     ,
    name        ,
    filterGroup ,
    language    ,
    supportUrl  ,
)
where

import           Data.Aeson (FromJSON, Object, decode, eitherDecode)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import           Data.Maybe (fromMaybe)
import           Data.Map (Map, assocs)
import           Data.Text (Text)
import           GHC.Generics (Generic)

data FilterList = FL {
    url         :: !String,
    enabled     :: !Bool,
    name        :: !Text,
    filterGroup :: !Text,
    language    :: Maybe Text,
    supportUrl  :: Maybe Text
}

data JsonFilterList = JFL {
    off         :: Maybe Bool,
    title       :: !Text,
    group       :: !Text,
    lang        :: Maybe Text,
    supportURL  :: Maybe Text
} deriving (Show, Generic)

instance FromJSON JsonFilterList

createDto :: (String, JsonFilterList) -> FilterList
createDto (filterListUrl, jfl) = FL {
        url         = filterListUrl,
        enabled     = fromMaybe False (off jfl),
        name        = title jfl,
        filterGroup = group jfl,
        language    = lang jfl,
        supportUrl  = supportURL jfl
    }

decodeJson :: L.ByteString -> Either String (Map String JsonFilterList)
decodeJson = eitherDecode

loadFilterLists :: L.ByteString -> Either String [FilterList]
loadFilterLists jsonString = mapToList <$> (decodeJson jsonString)


mapToList :: Map String JsonFilterList -> [FilterList]
mapToList = ((map createDto) . assocs)


