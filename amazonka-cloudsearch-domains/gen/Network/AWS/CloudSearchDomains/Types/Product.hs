{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearchDomains.Types.Product where

import           Network.AWS.CloudSearchDomains.Types.Sum
import           Network.AWS.Prelude

-- | A container for facet information.
--
-- /See:/ 'bucket' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bValue'
--
-- * 'bCount'
data Bucket = Bucket'
    { _bValue :: !(Maybe Text)
    , _bCount :: !(Maybe Integer)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Bucket' smart constructor.
bucket :: Bucket
bucket =
    Bucket'
    { _bValue = Nothing
    , _bCount = Nothing
    }

-- | The facet value being counted.
bValue :: Lens' Bucket (Maybe Text)
bValue = lens _bValue (\ s a -> s{_bValue = a});

-- | The number of hits that contain the facet value in the specified facet
-- field.
bCount :: Lens' Bucket (Maybe Integer)
bCount = lens _bCount (\ s a -> s{_bCount = a});

instance FromJSON Bucket where
        parseJSON
          = withObject "Bucket"
              (\ x ->
                 Bucket' <$> (x .:? "value") <*> (x .:? "count"))

-- | A container for the calculated facet values and counts.
--
-- /See:/ 'bucketInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'biBuckets'
newtype BucketInfo = BucketInfo'
    { _biBuckets :: Maybe [Bucket]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BucketInfo' smart constructor.
bucketInfo :: BucketInfo
bucketInfo =
    BucketInfo'
    { _biBuckets = Nothing
    }

-- | A list of the calculated facet values and counts.
biBuckets :: Lens' BucketInfo [Bucket]
biBuckets = lens _biBuckets (\ s a -> s{_biBuckets = a}) . _Default . _Coerce;

instance FromJSON BucketInfo where
        parseJSON
          = withObject "BucketInfo"
              (\ x -> BucketInfo' <$> (x .:? "buckets" .!= mempty))

-- | A warning returned by the document service when an issue is discovered
-- while processing an upload request.
--
-- /See:/ 'documentServiceWarning' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dswMessage'
newtype DocumentServiceWarning = DocumentServiceWarning'
    { _dswMessage :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DocumentServiceWarning' smart constructor.
documentServiceWarning :: DocumentServiceWarning
documentServiceWarning =
    DocumentServiceWarning'
    { _dswMessage = Nothing
    }

-- | The description for a warning returned by the document service.
dswMessage :: Lens' DocumentServiceWarning (Maybe Text)
dswMessage = lens _dswMessage (\ s a -> s{_dswMessage = a});

instance FromJSON DocumentServiceWarning where
        parseJSON
          = withObject "DocumentServiceWarning"
              (\ x ->
                 DocumentServiceWarning' <$> (x .:? "message"))

-- | Information about a document that matches the search request.
--
-- /See:/ 'hit' smart constructor.
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
data Hit = Hit'
    { _hitExprs      :: !(Maybe (Map Text Text))
    , _hitId         :: !(Maybe Text)
    , _hitHighlights :: !(Maybe (Map Text Text))
    , _hitFields     :: !(Maybe (Map Text [Text]))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Hit' smart constructor.
hit :: Hit
hit =
    Hit'
    { _hitExprs = Nothing
    , _hitId = Nothing
    , _hitHighlights = Nothing
    , _hitFields = Nothing
    }

-- | The expressions returned from a document that matches the search
-- request.
hitExprs :: Lens' Hit (HashMap Text Text)
hitExprs = lens _hitExprs (\ s a -> s{_hitExprs = a}) . _Default . _Map;

-- | The document ID of a document that matches the search request.
hitId :: Lens' Hit (Maybe Text)
hitId = lens _hitId (\ s a -> s{_hitId = a});

-- | The highlights returned from a document that matches the search request.
hitHighlights :: Lens' Hit (HashMap Text Text)
hitHighlights = lens _hitHighlights (\ s a -> s{_hitHighlights = a}) . _Default . _Map;

-- | The fields returned from a document that matches the search request.
hitFields :: Lens' Hit (HashMap Text [Text])
hitFields = lens _hitFields (\ s a -> s{_hitFields = a}) . _Default . _Map;

instance FromJSON Hit where
        parseJSON
          = withObject "Hit"
              (\ x ->
                 Hit' <$>
                   (x .:? "exprs" .!= mempty) <*> (x .:? "id") <*>
                     (x .:? "highlights" .!= mempty)
                     <*> (x .:? "fields" .!= mempty))

-- | The collection of documents that match the search request.
--
-- /See:/ 'hits' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hCursor'
--
-- * 'hHit'
--
-- * 'hStart'
--
-- * 'hFound'
data Hits = Hits'
    { _hCursor :: !(Maybe Text)
    , _hHit    :: !(Maybe [Hit])
    , _hStart  :: !(Maybe Integer)
    , _hFound  :: !(Maybe Integer)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Hits' smart constructor.
hits :: Hits
hits =
    Hits'
    { _hCursor = Nothing
    , _hHit = Nothing
    , _hStart = Nothing
    , _hFound = Nothing
    }

-- | A cursor that can be used to retrieve the next set of matching documents
-- when you want to page through a large result set.
hCursor :: Lens' Hits (Maybe Text)
hCursor = lens _hCursor (\ s a -> s{_hCursor = a});

-- | A document that matches the search request.
hHit :: Lens' Hits [Hit]
hHit = lens _hHit (\ s a -> s{_hHit = a}) . _Default . _Coerce;

-- | The index of the first matching document.
hStart :: Lens' Hits (Maybe Integer)
hStart = lens _hStart (\ s a -> s{_hStart = a});

-- | The total number of documents that match the search request.
hFound :: Lens' Hits (Maybe Integer)
hFound = lens _hFound (\ s a -> s{_hFound = a});

instance FromJSON Hits where
        parseJSON
          = withObject "Hits"
              (\ x ->
                 Hits' <$>
                   (x .:? "cursor") <*> (x .:? "hit" .!= mempty) <*>
                     (x .:? "start")
                     <*> (x .:? "found"))

-- | Contains the resource id (@rid@) and the time it took to process the
-- request (@timems@).
--
-- /See:/ 'searchStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssRid'
--
-- * 'ssTimems'
data SearchStatus = SearchStatus'
    { _ssRid    :: !(Maybe Text)
    , _ssTimems :: !(Maybe Integer)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SearchStatus' smart constructor.
searchStatus :: SearchStatus
searchStatus =
    SearchStatus'
    { _ssRid = Nothing
    , _ssTimems = Nothing
    }

-- | The encrypted resource ID for the request.
ssRid :: Lens' SearchStatus (Maybe Text)
ssRid = lens _ssRid (\ s a -> s{_ssRid = a});

-- | How long it took to process the request, in milliseconds.
ssTimems :: Lens' SearchStatus (Maybe Integer)
ssTimems = lens _ssTimems (\ s a -> s{_ssTimems = a});

instance FromJSON SearchStatus where
        parseJSON
          = withObject "SearchStatus"
              (\ x ->
                 SearchStatus' <$> (x .:? "rid") <*> (x .:? "timems"))

-- | Container for the suggestion information returned in a
-- @SuggestResponse@.
--
-- /See:/ 'suggestModel' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smFound'
--
-- * 'smSuggestions'
--
-- * 'smQuery'
data SuggestModel = SuggestModel'
    { _smFound       :: !(Maybe Integer)
    , _smSuggestions :: !(Maybe [SuggestionMatch])
    , _smQuery       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SuggestModel' smart constructor.
suggestModel :: SuggestModel
suggestModel =
    SuggestModel'
    { _smFound = Nothing
    , _smSuggestions = Nothing
    , _smQuery = Nothing
    }

-- | The number of documents that were found to match the query string.
smFound :: Lens' SuggestModel (Maybe Integer)
smFound = lens _smFound (\ s a -> s{_smFound = a});

-- | The documents that match the query string.
smSuggestions :: Lens' SuggestModel [SuggestionMatch]
smSuggestions = lens _smSuggestions (\ s a -> s{_smSuggestions = a}) . _Default . _Coerce;

-- | The query string specified in the suggest request.
smQuery :: Lens' SuggestModel (Maybe Text)
smQuery = lens _smQuery (\ s a -> s{_smQuery = a});

instance FromJSON SuggestModel where
        parseJSON
          = withObject "SuggestModel"
              (\ x ->
                 SuggestModel' <$>
                   (x .:? "found") <*> (x .:? "suggestions" .!= mempty)
                     <*> (x .:? "query"))

-- | Contains the resource id (@rid@) and the time it took to process the
-- request (@timems@).
--
-- /See:/ 'suggestStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sRid'
--
-- * 'sTimems'
data SuggestStatus = SuggestStatus'
    { _sRid    :: !(Maybe Text)
    , _sTimems :: !(Maybe Integer)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SuggestStatus' smart constructor.
suggestStatus :: SuggestStatus
suggestStatus =
    SuggestStatus'
    { _sRid = Nothing
    , _sTimems = Nothing
    }

-- | The encrypted resource ID for the request.
sRid :: Lens' SuggestStatus (Maybe Text)
sRid = lens _sRid (\ s a -> s{_sRid = a});

-- | How long it took to process the request, in milliseconds.
sTimems :: Lens' SuggestStatus (Maybe Integer)
sTimems = lens _sTimems (\ s a -> s{_sTimems = a});

instance FromJSON SuggestStatus where
        parseJSON
          = withObject "SuggestStatus"
              (\ x ->
                 SuggestStatus' <$>
                   (x .:? "rid") <*> (x .:? "timems"))

-- | An autocomplete suggestion that matches the query string specified in a
-- @SuggestRequest@.
--
-- /See:/ 'suggestionMatch' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smSuggestion'
--
-- * 'smScore'
--
-- * 'smId'
data SuggestionMatch = SuggestionMatch'
    { _smSuggestion :: !(Maybe Text)
    , _smScore      :: !(Maybe Integer)
    , _smId         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SuggestionMatch' smart constructor.
suggestionMatch :: SuggestionMatch
suggestionMatch =
    SuggestionMatch'
    { _smSuggestion = Nothing
    , _smScore = Nothing
    , _smId = Nothing
    }

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
                   (x .:? "suggestion") <*> (x .:? "score") <*>
                     (x .:? "id"))
