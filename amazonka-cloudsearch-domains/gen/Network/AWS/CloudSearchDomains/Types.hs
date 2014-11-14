{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

-- Module      : Network.AWS.CloudSearchDomains.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudSearchDomains.Types
    (
    -- * Service
      CloudSearchDomains
    -- ** Error
    , RESTError

    -- * SearchStatus
    , SearchStatus
    , searchStatus
    , ssRid
    , ssTimems

    -- * QueryParser
    , QueryParser (..)

    -- * Hit
    , Hit
    , hit
    , hitFields
    , hitHighlights
    , hitId

    -- * SuggestStatus
    , SuggestStatus
    , suggestStatus
    , ss1Rid
    , ss1Timems

    -- * Bucket
    , Bucket
    , bucket
    , bCount
    , bValue

    -- * SuggestionMatch
    , SuggestionMatch
    , suggestionMatch
    , smId
    , smScore
    , smSuggestion

    -- * BucketInfo
    , BucketInfo
    , bucketInfo
    , biBuckets

    -- * DocumentServiceWarning
    , DocumentServiceWarning
    , documentServiceWarning
    , dswMessage

    -- * SuggestModel
    , SuggestModel
    , suggestModel
    , smFound
    , smQuery
    , smSuggestions

    -- * Hits
    , Hits
    , hits
    , hCursor
    , hFound
    , hHit
    , hStart

    -- * ContentType
    , ContentType (..)
    ) where

import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V4
import qualified GHC.Exts

-- | Supported version (@2013-01-01@) of the Amazon CloudSearch Domain.
data CloudSearchDomains deriving (Typeable)

instance AWSService CloudSearchDomains where
    type Sg CloudSearchDomains = V4
    type Er CloudSearchDomains = RESTError

    service = Service
        { _svcEndpoint = regional
        , _svcAbbrev   = "CloudSearchDomains"
        , _svcPrefix   = "cloudsearchdomain"
        , _svcVersion  = "2013-01-01"
        , _svcTarget   = Nothing
        }

    handle = restError alwaysFail

data SearchStatus = SearchStatus
    { _ssRid    :: Maybe Text
    , _ssTimems :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | 'SearchStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssRid' @::@ 'Maybe' 'Text'
--
-- * 'ssTimems' @::@ 'Maybe' 'Integer'
--
searchStatus :: SearchStatus
searchStatus = SearchStatus
    { _ssTimems = Nothing
    , _ssRid    = Nothing
    }

-- | The encrypted resource ID for the request.
ssRid :: Lens' SearchStatus (Maybe Text)
ssRid = lens _ssRid (\s a -> s { _ssRid = a })

-- | How long it took to process the request, in milliseconds.
ssTimems :: Lens' SearchStatus (Maybe Integer)
ssTimems = lens _ssTimems (\s a -> s { _ssTimems = a })

instance FromJSON SearchStatus

instance ToJSON SearchStatus

data QueryParser
    = Dismax     -- ^ dismax
    | Lucene     -- ^ lucene
    | Simple     -- ^ simple
    | Structured -- ^ structured
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable QueryParser

instance FromText QueryParser where
    parser = match "dismax"     Dismax
         <|> match "lucene"     Lucene
         <|> match "simple"     Simple
         <|> match "structured" Structured

instance ToText QueryParser where
    toText = \case
        Dismax     -> "dismax"
        Lucene     -> "lucene"
        Simple     -> "simple"
        Structured -> "structured"

instance FromJSON QueryParser

instance ToJSON QueryParser

data Hit = Hit
    { _hitFields     :: Map Text [Text]
    , _hitHighlights :: Map Text Text
    , _hitId         :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Hit' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hitFields' @::@ 'HashMap' 'Text' ['Text']
--
-- * 'hitHighlights' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'hitId' @::@ 'Maybe' 'Text'
--
hit :: Hit
hit = Hit
    { _hitId         = Nothing
    , _hitFields     = mempty
    , _hitHighlights = mempty
    }

-- | The fields returned from a document that matches the search request.
hitFields :: Lens' Hit (HashMap Text [Text])
hitFields = lens _hitFields (\s a -> s { _hitFields = a })
    . _Map

-- | The highlights returned from a document that matches the search request.
hitHighlights :: Lens' Hit (HashMap Text Text)
hitHighlights = lens _hitHighlights (\s a -> s { _hitHighlights = a })
    . _Map

-- | The document ID of a document that matches the search request.
hitId :: Lens' Hit (Maybe Text)
hitId = lens _hitId (\s a -> s { _hitId = a })

instance FromJSON Hit

instance ToJSON Hit

data SuggestStatus = SuggestStatus
    { _ss1Rid    :: Maybe Text
    , _ss1Timems :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | 'SuggestStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ss1Rid' @::@ 'Maybe' 'Text'
--
-- * 'ss1Timems' @::@ 'Maybe' 'Integer'
--
suggestStatus :: SuggestStatus
suggestStatus = SuggestStatus
    { _ss1Timems = Nothing
    , _ss1Rid    = Nothing
    }

-- | The encrypted resource ID for the request.
ss1Rid :: Lens' SuggestStatus (Maybe Text)
ss1Rid = lens _ss1Rid (\s a -> s { _ss1Rid = a })

-- | How long it took to process the request, in milliseconds.
ss1Timems :: Lens' SuggestStatus (Maybe Integer)
ss1Timems = lens _ss1Timems (\s a -> s { _ss1Timems = a })

instance FromJSON SuggestStatus

instance ToJSON SuggestStatus

data Bucket = Bucket
    { _bCount :: Maybe Integer
    , _bValue :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Bucket' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bCount' @::@ 'Maybe' 'Integer'
--
-- * 'bValue' @::@ 'Maybe' 'Text'
--
bucket :: Bucket
bucket = Bucket
    { _bValue = Nothing
    , _bCount = Nothing
    }

-- | The number of hits that contain the facet value in the specified facet
-- field.
bCount :: Lens' Bucket (Maybe Integer)
bCount = lens _bCount (\s a -> s { _bCount = a })

-- | The facet value being counted.
bValue :: Lens' Bucket (Maybe Text)
bValue = lens _bValue (\s a -> s { _bValue = a })

instance FromJSON Bucket

instance ToJSON Bucket

data SuggestionMatch = SuggestionMatch
    { _smId         :: Maybe Text
    , _smScore      :: Maybe Integer
    , _smSuggestion :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SuggestionMatch' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smId' @::@ 'Maybe' 'Text'
--
-- * 'smScore' @::@ 'Maybe' 'Integer'
--
-- * 'smSuggestion' @::@ 'Maybe' 'Text'
--
suggestionMatch :: SuggestionMatch
suggestionMatch = SuggestionMatch
    { _smSuggestion = Nothing
    , _smScore      = Nothing
    , _smId         = Nothing
    }

-- | The document ID of the suggested document.
smId :: Lens' SuggestionMatch (Maybe Text)
smId = lens _smId (\s a -> s { _smId = a })

-- | The relevance score of a suggested match.
smScore :: Lens' SuggestionMatch (Maybe Integer)
smScore = lens _smScore (\s a -> s { _smScore = a })

-- | The string that matches the query string specified in the SuggestRequest.
smSuggestion :: Lens' SuggestionMatch (Maybe Text)
smSuggestion = lens _smSuggestion (\s a -> s { _smSuggestion = a })

instance FromJSON SuggestionMatch

instance ToJSON SuggestionMatch

newtype BucketInfo = BucketInfo
    { _biBuckets :: [Bucket]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList BucketInfo where
    type Item BucketInfo = Bucket

    fromList = BucketInfo . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _biBuckets

-- | 'BucketInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'biBuckets' @::@ ['Bucket']
--
bucketInfo :: BucketInfo
bucketInfo = BucketInfo
    { _biBuckets = mempty
    }

-- | A list of the calculated facet values and counts.
biBuckets :: Lens' BucketInfo [Bucket]
biBuckets = lens _biBuckets (\s a -> s { _biBuckets = a })

instance FromJSON BucketInfo

instance ToJSON BucketInfo

newtype DocumentServiceWarning = DocumentServiceWarning
    { _dswMessage :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DocumentServiceWarning' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dswMessage' @::@ 'Maybe' 'Text'
--
documentServiceWarning :: DocumentServiceWarning
documentServiceWarning = DocumentServiceWarning
    { _dswMessage = Nothing
    }

-- | The description for a warning returned by the document service.
dswMessage :: Lens' DocumentServiceWarning (Maybe Text)
dswMessage = lens _dswMessage (\s a -> s { _dswMessage = a })

instance FromJSON DocumentServiceWarning

instance ToJSON DocumentServiceWarning

data SuggestModel = SuggestModel
    { _smFound       :: Maybe Integer
    , _smQuery       :: Maybe Text
    , _smSuggestions :: [SuggestionMatch]
    } deriving (Eq, Show, Generic)

-- | 'SuggestModel' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smFound' @::@ 'Maybe' 'Integer'
--
-- * 'smQuery' @::@ 'Maybe' 'Text'
--
-- * 'smSuggestions' @::@ ['SuggestionMatch']
--
suggestModel :: SuggestModel
suggestModel = SuggestModel
    { _smQuery       = Nothing
    , _smFound       = Nothing
    , _smSuggestions = mempty
    }

-- | The number of documents that were found to match the query string.
smFound :: Lens' SuggestModel (Maybe Integer)
smFound = lens _smFound (\s a -> s { _smFound = a })

-- | The query string specified in the suggest request.
smQuery :: Lens' SuggestModel (Maybe Text)
smQuery = lens _smQuery (\s a -> s { _smQuery = a })

-- | The documents that match the query string.
smSuggestions :: Lens' SuggestModel [SuggestionMatch]
smSuggestions = lens _smSuggestions (\s a -> s { _smSuggestions = a })

instance FromJSON SuggestModel

instance ToJSON SuggestModel

data Hits = Hits
    { _hCursor :: Maybe Text
    , _hFound  :: Maybe Integer
    , _hHit    :: [Hit]
    , _hStart  :: Maybe Integer
    } deriving (Eq, Show, Generic)

-- | 'Hits' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hCursor' @::@ 'Maybe' 'Text'
--
-- * 'hFound' @::@ 'Maybe' 'Integer'
--
-- * 'hHit' @::@ ['Hit']
--
-- * 'hStart' @::@ 'Maybe' 'Integer'
--
hits :: Hits
hits = Hits
    { _hFound  = Nothing
    , _hStart  = Nothing
    , _hCursor = Nothing
    , _hHit    = mempty
    }

-- | A cursor that can be used to retrieve the next set of matching documents
-- when you want to page through a large result set.
hCursor :: Lens' Hits (Maybe Text)
hCursor = lens _hCursor (\s a -> s { _hCursor = a })

-- | The total number of documents that match the search request.
hFound :: Lens' Hits (Maybe Integer)
hFound = lens _hFound (\s a -> s { _hFound = a })

-- | A document that matches the search request.
hHit :: Lens' Hits [Hit]
hHit = lens _hHit (\s a -> s { _hHit = a })

-- | The index of the first matching document.
hStart :: Lens' Hits (Maybe Integer)
hStart = lens _hStart (\s a -> s { _hStart = a })

instance FromJSON Hits

instance ToJSON Hits

data ContentType
    = ApplicationJson -- ^ application/json
    | ApplicationXml  -- ^ application/xml
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ContentType

instance FromText ContentType where
    parser = match "application/json" ApplicationJson
         <|> match "application/xml"  ApplicationXml

instance ToText ContentType where
    toText = \case
        ApplicationJson -> "application/json"
        ApplicationXml  -> "application/xml"

instance FromJSON ContentType

instance ToJSON ContentType
