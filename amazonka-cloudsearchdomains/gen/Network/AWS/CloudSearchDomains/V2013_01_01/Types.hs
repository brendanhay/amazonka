{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearchDomains.V2013_01_01.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Pending.
module Network.AWS.CloudSearchDomains.V2013_01_01.Types where

import Control.Lens.TH (makeLenses)
import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2013-01-01@) of the
-- @Amazon CloudSearch Domain@ service.
data CloudSearchDomains deriving (Typeable)

instance AWSService CloudSearchDomains where
    type Sg CloudSearchDomains = V4
    data Er CloudSearchDomains
        = CloudSearchDomainsClient HttpException
        | CloudSearchDomainsSerializer String
        | CloudSearchDomainsService String
        | DocumentServiceException
            { _dseStatus :: Maybe Text
            , _dseMessage :: Maybe Text
            }
        | SearchException
            { _seMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "cloudsearchdomain"
        , _svcVersion  = "2013-01-01"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er CloudSearchDomains)
deriving instance Generic (Er CloudSearchDomains)

instance AWSError (Er CloudSearchDomains) where
    awsError = const "CloudSearchDomainsError"

instance AWSServiceError (Er CloudSearchDomains) where
    serviceError    = CloudSearchDomainsService
    clientError     = CloudSearchDomainsClient
    serializerError = CloudSearchDomainsSerializer

instance Exception (Er CloudSearchDomains)

-- | The format of the batch you are uploading. Amazon CloudSearch supports two
-- document batch formats: application/json application/xml.
data ContentType
    = ContentTypeApplicationJson -- ^ application/json
    | ContentTypeApplicationXml -- ^ application/xml
      deriving (Eq, Show, Generic)

instance Hashable ContentType

instance FromText ContentType where
    parser = match "application/json" ContentTypeApplicationJson
         <|> match "application/xml" ContentTypeApplicationXml

instance ToText ContentType where
    toText ContentTypeApplicationJson = "application/json"
    toText ContentTypeApplicationXml = "application/xml"

instance ToByteString ContentType

instance ToJSON ContentType

-- | Specifies which query parser to use to process the request. If queryParser
-- is not specified, Amazon CloudSearch uses the simple query parser. Amazon
-- CloudSearch supports four query parsers: simple: perform simple searches of
-- text and text-array fields. By default, the simple query parser searches
-- all text and text-array fields. You can specify which fields to search by
-- with the queryOptions parameter. If you prefix a search term with a plus
-- sign (+) documents must contain the term to be considered a match. (This is
-- the default, unless you configure the default operator with the
-- queryOptions parameter.) You can use the - (NOT), | (OR), and * (wildcard)
-- operators to exclude particular terms, find results that match any of the
-- specified terms, or search for a prefix. To search for a phrase rather than
-- individual terms, enclose the phrase in double quotes. For more
-- information, see Searching for Text in the Amazon CloudSearch Developer
-- Guide. structured: perform advanced searches by combining multiple
-- expressions to define the search criteria. You can also search within
-- particular fields, search for values and ranges of values, and use advanced
-- options such as term boosting, matchall, and near. For more information,
-- see Constructing Compound Queries in the Amazon CloudSearch Developer
-- Guide. lucene: search using the Apache Lucene query parser syntax. For more
-- information, see Apache Lucene Query Parser Syntax. dismax: search using
-- the simplified subset of the Apache Lucene query parser syntax defined by
-- the DisMax query parser. For more information, see DisMax Query Parser
-- Syntax.
data QueryParser
    = QueryParserDismax -- ^ dismax
    | QueryParserLucene -- ^ lucene
    | QueryParserSimple -- ^ simple
    | QueryParserStructured -- ^ structured
      deriving (Eq, Show, Generic)

instance Hashable QueryParser

instance FromText QueryParser where
    parser = match "dismax" QueryParserDismax
         <|> match "lucene" QueryParserLucene
         <|> match "simple" QueryParserSimple
         <|> match "structured" QueryParserStructured

instance ToText QueryParser where
    toText QueryParserDismax = "dismax"
    toText QueryParserLucene = "lucene"
    toText QueryParserSimple = "simple"
    toText QueryParserStructured = "structured"

instance ToByteString QueryParser

instance ToJSON QueryParser

-- | A container for the calculated facet values and counts.
newtype BucketInfo = BucketInfo
    { _biBuckets :: [Bucket]
      -- ^ A list of the calculated facet values and counts.
    } deriving (Show, Generic)

instance FromJSON BucketInfo

-- | A warning returned by the document service when an issue is discovered
-- while processing an upload request.
newtype DocumentServiceWarning = DocumentServiceWarning
    { _dsxMessage :: Maybe Text
      -- ^ The description for a warning returned by the document service.
    } deriving (Show, Generic)

instance FromJSON DocumentServiceWarning

-- | A container for facet information.
data Bucket = Bucket
    { _iValue :: Maybe Text
      -- ^ The facet value being counted.
    , _iCount :: Maybe Integer
      -- ^ The number of hits that contain the facet value in the specified
      -- facet field.
    } deriving (Show, Generic)

instance FromJSON Bucket

instance ToJSON Bucket

-- | Information about a document that matches the search request.
data Hit = Hit
    { _kId :: Maybe Text
      -- ^ The document ID of a document that matches the search request.
    , _kHighlights :: Map Text Text
      -- ^ The highlights returned from a document that matches the search
      -- request.
    , _kFields :: Map Text [Text]
      -- ^ The fields returned from a document that matches the search
      -- request.
    } deriving (Show, Generic)

instance FromJSON Hit

-- | The documents that match the search criteria.
data Hits = Hits
    { _jCursor :: Maybe Text
      -- ^ A cursor that can be used to retrieve the next set of matching
      -- documents when you want to page through a large result set.
    , _jHit :: [Hit]
      -- ^ A document that matches the search request.
    , _jStart :: Maybe Integer
      -- ^ The index of the first matching document.
    , _jFound :: Maybe Integer
      -- ^ The total number of documents that match the search request.
    } deriving (Show, Generic)

instance FromJSON Hits

-- | The status information returned for the search request.
data SearchStatus = SearchStatus
    { _szRid :: Maybe Text
      -- ^ The encrypted resource ID for the request.
    , _szTimems :: Maybe Integer
      -- ^ How long it took to process the request, in milliseconds.
    } deriving (Show, Generic)

instance FromJSON SearchStatus

-- | Container for the matching search suggestion information.
data SuggestModel = SuggestModel
    { _smFound :: Maybe Integer
      -- ^ The number of documents that were found to match the query
      -- string.
    , _smSuggestions :: [SuggestionMatch]
      -- ^ The documents that match the query string.
    , _smQuery :: Maybe Text
      -- ^ The query string specified in the suggest request.
    } deriving (Show, Generic)

instance FromJSON SuggestModel

-- | The status of a SuggestRequest. Contains the resource ID (rid) and how long
-- it took to process the request (timems).
data SuggestStatus = SuggestStatus
    { _suRid :: Maybe Text
      -- ^ The encrypted resource ID for the request.
    , _suTimems :: Maybe Integer
      -- ^ How long it took to process the request, in milliseconds.
    } deriving (Show, Generic)

instance FromJSON SuggestStatus

-- | An autocomplete suggestion that matches the query string specified in a
-- SuggestRequest.
data SuggestionMatch = SuggestionMatch
    { _snSuggestion :: Maybe Text
      -- ^ The string that matches the query string specified in the
      -- SuggestRequest.
    , _snScore :: Maybe Integer
      -- ^ The relevance score of a suggested match.
    , _snId :: Maybe Text
      -- ^ The document ID of the suggested document.
    } deriving (Show, Generic)

instance FromJSON SuggestionMatch

-- Newtypes
makeLenses ''BucketInfo
makeLenses ''DocumentServiceWarning

-- Products
makeLenses ''Bucket
makeLenses ''Hit
makeLenses ''Hits
makeLenses ''SearchStatus
makeLenses ''SuggestModel
makeLenses ''SuggestStatus
makeLenses ''SuggestionMatch
