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
import qualified Data.List as DL
import           Data.Maybe (fromMaybe)
import           Data.Map (Map, assocs)
import           Data.Text (Text)
import           GHC.Generics (Generic)

type JSONString = L.ByteString

-- | Data object returned from this module
data FilterList = FL {
    url         :: !String,
    enabled     :: !Bool,
    name        :: !Text,
    filterGroup :: !Text,
    language    :: Maybe Text,
    supportUrl  :: Maybe Text
}

-- | Single JSON entry in the filter lists file
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
        url         = fixUrl filterListUrl,
        enabled     = not $ fromMaybe False (off jfl),
        name        = title jfl,
        filterGroup = group jfl,
        language    = lang jfl,
        supportUrl  = supportURL jfl
    } where
        fixUrl urlFromJson | "http" `DL.isPrefixOf` urlFromJson = urlFromJson
                           | otherwise                       = "http://" ++ urlFromJson

decodeJson :: JSONString -> Either String (Map String JsonFilterList)
decodeJson = eitherDecode

-- | Transforms JSON file bytestring into either with list of filter lists
loadFilterLists :: JSONString -> Either String [FilterList]
loadFilterLists jsonString = mapToFilterList <$> (decodeJson jsonString) where
    mapToFilterList = map createDto . assocs
