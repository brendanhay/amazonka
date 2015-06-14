{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.CloudSearchDomains.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.CloudSearchDomains.Types
    (
    -- * Service
      CloudSearchDomains
    -- ** Errors
    , JSONError

    -- * Bucket
    , Bucket
    , bucket
    , bucValue
    , bucCount

    -- * BucketInfo
    , BucketInfo
    , bucketInfo
    , biBuckets

    -- * ContentType
    , ContentType (..)

    -- * DocumentServiceWarning
    , DocumentServiceWarning
    , documentServiceWarning
    , dswMessage

    -- * Hit
    , Hit
    , hit
    , hitExprs
    , hitId
    , hitHighlights
    , hitFields

    -- * Hits
    , Hits
    , hits
    , hitCursor
    , hitHit
    , hitStart
    , hitFound

    -- * QueryParser
    , QueryParser (..)

    -- * SearchStatus
    , SearchStatus
    , searchStatus
    , seaRid
    , seaTimems

    -- * SuggestModel
    , SuggestModel
    , suggestModel
    , smFound
    , smSuggestions
    , smQuery

    -- * SuggestStatus
    , SuggestStatus
    , suggestStatus
    , ssRid
    , ssTimems

    -- * SuggestionMatch
    , SuggestionMatch
    , suggestionMatch
    , smSuggestion
    , smScore
    , smId
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2013-01-01@ of the Amazon CloudSearch Domain SDK.
data CloudSearchDomains

instance AWSService CloudSearchDomains where
    type Sg CloudSearchDomains = V4
    type Er CloudSearchDomains = JSONError

    service = service'
      where
        service' :: Service CloudSearchDomains
        service' = Service
            { _svcAbbrev  = "CloudSearchDomains"
            , _svcPrefix  = "cloudsearchdomain"
            , _svcVersion = "2013-01-01"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry CloudSearchDomains
        retry = undefined

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

-- | /See:/ 'bucket' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bucValue'
--
-- * 'bucCount'
data Bucket = Bucket'{_bucValue :: Maybe Text, _bucCount :: Maybe Integer} deriving (Eq, Read, Show)

-- | 'Bucket' smart constructor.
bucket :: Bucket
bucket = Bucket'{_bucValue = Nothing, _bucCount = Nothing};

-- | The facet value being counted.
bucValue :: Lens' Bucket (Maybe Text)
bucValue = lens _bucValue (\ s a -> s{_bucValue = a});

-- | The number of hits that contain the facet value in the specified facet
-- field.
bucCount :: Lens' Bucket (Maybe Integer)
bucCount = lens _bucCount (\ s a -> s{_bucCount = a});

instance FromJSON Bucket where
        parseJSON
          = withObject "Bucket"
              (\ x -> Bucket' <$> x .:? "value" <*> x .:? "count")

-- | /See:/ 'bucketInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'biBuckets'
newtype BucketInfo = BucketInfo'{_biBuckets :: Maybe [Bucket]} deriving (Eq, Read, Show)

-- | 'BucketInfo' smart constructor.
bucketInfo :: BucketInfo
bucketInfo = BucketInfo'{_biBuckets = Nothing};

-- | A list of the calculated facet values and counts.
biBuckets :: Lens' BucketInfo (Maybe [Bucket])
biBuckets = lens _biBuckets (\ s a -> s{_biBuckets = a});

instance FromJSON BucketInfo where
        parseJSON
          = withObject "BucketInfo"
              (\ x -> BucketInfo' <$> x .:? "buckets" .!= mempty)

data ContentType = ApplicationJSON | ApplicationXML deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ContentType where
    parser = takeLowerText >>= \case
        "application/json" -> pure ApplicationJSON
        "application/xml" -> pure ApplicationXML
        e -> fail ("Failure parsing ContentType from " ++ show e)

instance ToText ContentType where
    toText = \case
        ApplicationJSON -> "application/json"
        ApplicationXML -> "application/xml"

instance Hashable ContentType
instance ToQuery ContentType
instance ToHeader ContentType

instance ToJSON ContentType where
    toJSON = toJSONText

-- | /See:/ 'documentServiceWarning' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dswMessage'
newtype DocumentServiceWarning = DocumentServiceWarning'{_dswMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DocumentServiceWarning' smart constructor.
documentServiceWarning :: DocumentServiceWarning
documentServiceWarning = DocumentServiceWarning'{_dswMessage = Nothing};

-- | The description for a warning returned by the document service.
dswMessage :: Lens' DocumentServiceWarning (Maybe Text)
dswMessage = lens _dswMessage (\ s a -> s{_dswMessage = a});

instance FromJSON DocumentServiceWarning where
        parseJSON
          = withObject "DocumentServiceWarning"
              (\ x -> DocumentServiceWarning' <$> x .:? "message")

-- | /See:/ 'hit' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hitExprs'
--
-- * 'hitId'
--
-- * 'hitHighlights'
--
-- * 'hitFields'
data Hit = Hit'{_hitExprs :: Maybe (HashMap Text Text), _hitId :: Maybe Text, _hitHighlights :: Maybe (HashMap Text Text), _hitFields :: Maybe (HashMap Text [Text])} deriving (Eq, Read, Show)

-- | 'Hit' smart constructor.
hit :: Hit
hit = Hit'{_hitExprs = Nothing, _hitId = Nothing, _hitHighlights = Nothing, _hitFields = Nothing};

-- | The expressions returned from a document that matches the search
-- request.
hitExprs :: Lens' Hit (Maybe (HashMap Text Text))
hitExprs = lens _hitExprs (\ s a -> s{_hitExprs = a}) . mapping _Coerce;

-- | The document ID of a document that matches the search request.
hitId :: Lens' Hit (Maybe Text)
hitId = lens _hitId (\ s a -> s{_hitId = a});

-- | The highlights returned from a document that matches the search request.
hitHighlights :: Lens' Hit (Maybe (HashMap Text Text))
hitHighlights = lens _hitHighlights (\ s a -> s{_hitHighlights = a}) . mapping _Coerce;

-- | The fields returned from a document that matches the search request.
hitFields :: Lens' Hit (Maybe (HashMap Text [Text]))
hitFields = lens _hitFields (\ s a -> s{_hitFields = a}) . mapping _Coerce;

instance FromJSON Hit where
        parseJSON
          = withObject "Hit"
              (\ x ->
                 Hit' <$>
                   x .:? "exprs" .!= mempty <*> x .:? "id" <*>
                     x .:? "highlights" .!= mempty
                     <*> x .:? "fields" .!= mempty)

-- | /See:/ 'hits' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hitCursor'
--
-- * 'hitHit'
--
-- * 'hitStart'
--
-- * 'hitFound'
data Hits = Hits'{_hitCursor :: Maybe Text, _hitHit :: Maybe [Hit], _hitStart :: Maybe Integer, _hitFound :: Maybe Integer} deriving (Eq, Read, Show)

-- | 'Hits' smart constructor.
hits :: Hits
hits = Hits'{_hitCursor = Nothing, _hitHit = Nothing, _hitStart = Nothing, _hitFound = Nothing};

-- | A cursor that can be used to retrieve the next set of matching documents
-- when you want to page through a large result set.
hitCursor :: Lens' Hits (Maybe Text)
hitCursor = lens _hitCursor (\ s a -> s{_hitCursor = a});

-- | A document that matches the search request.
hitHit :: Lens' Hits (Maybe [Hit])
hitHit = lens _hitHit (\ s a -> s{_hitHit = a});

-- | The index of the first matching document.
hitStart :: Lens' Hits (Maybe Integer)
hitStart = lens _hitStart (\ s a -> s{_hitStart = a});

-- | The total number of documents that match the search request.
hitFound :: Lens' Hits (Maybe Integer)
hitFound = lens _hitFound (\ s a -> s{_hitFound = a});

instance FromJSON Hits where
        parseJSON
          = withObject "Hits"
              (\ x ->
                 Hits' <$>
                   x .:? "cursor" <*> x .:? "hit" .!= mempty <*>
                     x .:? "start"
                     <*> x .:? "found")

data QueryParser = Lucene | Dismax | Simple | Structured deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText QueryParser where
    parser = takeLowerText >>= \case
        "dismax" -> pure Dismax
        "lucene" -> pure Lucene
        "simple" -> pure Simple
        "structured" -> pure Structured
        e -> fail ("Failure parsing QueryParser from " ++ show e)

instance ToText QueryParser where
    toText = \case
        Dismax -> "dismax"
        Lucene -> "lucene"
        Simple -> "simple"
        Structured -> "structured"

instance Hashable QueryParser
instance ToQuery QueryParser
instance ToHeader QueryParser

instance ToJSON QueryParser where
    toJSON = toJSONText

-- | /See:/ 'searchStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'seaRid'
--
-- * 'seaTimems'
data SearchStatus = SearchStatus'{_seaRid :: Maybe Text, _seaTimems :: Maybe Integer} deriving (Eq, Read, Show)

-- | 'SearchStatus' smart constructor.
searchStatus :: SearchStatus
searchStatus = SearchStatus'{_seaRid = Nothing, _seaTimems = Nothing};

-- | The encrypted resource ID for the request.
seaRid :: Lens' SearchStatus (Maybe Text)
seaRid = lens _seaRid (\ s a -> s{_seaRid = a});

-- | How long it took to process the request, in milliseconds.
seaTimems :: Lens' SearchStatus (Maybe Integer)
seaTimems = lens _seaTimems (\ s a -> s{_seaTimems = a});

instance FromJSON SearchStatus where
        parseJSON
          = withObject "SearchStatus"
              (\ x ->
                 SearchStatus' <$> x .:? "rid" <*> x .:? "timems")

-- | /See:/ 'suggestModel' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smFound'
--
-- * 'smSuggestions'
--
-- * 'smQuery'
data SuggestModel = SuggestModel'{_smFound :: Maybe Integer, _smSuggestions :: Maybe [SuggestionMatch], _smQuery :: Maybe Text} deriving (Eq, Read, Show)

-- | 'SuggestModel' smart constructor.
suggestModel :: SuggestModel
suggestModel = SuggestModel'{_smFound = Nothing, _smSuggestions = Nothing, _smQuery = Nothing};

-- | The number of documents that were found to match the query string.
smFound :: Lens' SuggestModel (Maybe Integer)
smFound = lens _smFound (\ s a -> s{_smFound = a});

-- | The documents that match the query string.
smSuggestions :: Lens' SuggestModel (Maybe [SuggestionMatch])
smSuggestions = lens _smSuggestions (\ s a -> s{_smSuggestions = a});

-- | The query string specified in the suggest request.
smQuery :: Lens' SuggestModel (Maybe Text)
smQuery = lens _smQuery (\ s a -> s{_smQuery = a});

instance FromJSON SuggestModel where
        parseJSON
          = withObject "SuggestModel"
              (\ x ->
                 SuggestModel' <$>
                   x .:? "found" <*> x .:? "suggestions" .!= mempty <*>
                     x .:? "query")

-- | /See:/ 'suggestStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssRid'
--
-- * 'ssTimems'
data SuggestStatus = SuggestStatus'{_ssRid :: Maybe Text, _ssTimems :: Maybe Integer} deriving (Eq, Read, Show)

-- | 'SuggestStatus' smart constructor.
suggestStatus :: SuggestStatus
suggestStatus = SuggestStatus'{_ssRid = Nothing, _ssTimems = Nothing};

-- | The encrypted resource ID for the request.
ssRid :: Lens' SuggestStatus (Maybe Text)
ssRid = lens _ssRid (\ s a -> s{_ssRid = a});

-- | How long it took to process the request, in milliseconds.
ssTimems :: Lens' SuggestStatus (Maybe Integer)
ssTimems = lens _ssTimems (\ s a -> s{_ssTimems = a});

instance FromJSON SuggestStatus where
        parseJSON
          = withObject "SuggestStatus"
              (\ x ->
                 SuggestStatus' <$> x .:? "rid" <*> x .:? "timems")

-- | /See:/ 'suggestionMatch' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smSuggestion'
--
-- * 'smScore'
--
-- * 'smId'
data SuggestionMatch = SuggestionMatch'{_smSuggestion :: Maybe Text, _smScore :: Maybe Integer, _smId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'SuggestionMatch' smart constructor.
suggestionMatch :: SuggestionMatch
suggestionMatch = SuggestionMatch'{_smSuggestion = Nothing, _smScore = Nothing, _smId = Nothing};

-- | The string that matches the query string specified in the
-- @SuggestRequest@.
smSuggestion :: Lens' SuggestionMatch (Maybe Text)
smSuggestion = lens _smSuggestion (\ s a -> s{_smSuggestion = a});

-- | The relevance score of a suggested match.
smScore :: Lens' SuggestionMatch (Maybe Integer)
smScore = lens _smScore (\ s a -> s{_smScore = a});

-- | The document ID of the suggested document.
smId :: Lens' SuggestionMatch (Maybe Text)
smId = lens _smId (\ s a -> s{_smId = a});

instance FromJSON SuggestionMatch where
        parseJSON
          = withObject "SuggestionMatch"
              (\ x ->
                 SuggestionMatch' <$>
                   x .:? "suggestion" <*> x .:? "score" <*> x .:? "id")
