{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearchDomains.Types.Product where

import Network.AWS.CloudSearchDomains.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A container for facet information.
--
--
--
-- /See:/ 'bucket' smart constructor.
data Bucket = Bucket'
  { _bValue :: !(Maybe Text)
  , _bCount :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Bucket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bValue' - The facet value being counted.
--
-- * 'bCount' - The number of hits that contain the facet value in the specified facet field.
bucket
    :: Bucket
bucket = Bucket' {_bValue = Nothing, _bCount = Nothing}


-- | The facet value being counted.
bValue :: Lens' Bucket (Maybe Text)
bValue = lens _bValue (\ s a -> s{_bValue = a})

-- | The number of hits that contain the facet value in the specified facet field.
bCount :: Lens' Bucket (Maybe Integer)
bCount = lens _bCount (\ s a -> s{_bCount = a})

instance FromJSON Bucket where
        parseJSON
          = withObject "Bucket"
              (\ x ->
                 Bucket' <$> (x .:? "value") <*> (x .:? "count"))

instance Hashable Bucket where

instance NFData Bucket where

-- | A container for the calculated facet values and counts.
--
--
--
-- /See:/ 'bucketInfo' smart constructor.
newtype BucketInfo = BucketInfo'
  { _biBuckets :: Maybe [Bucket]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BucketInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'biBuckets' - A list of the calculated facet values and counts.
bucketInfo
    :: BucketInfo
bucketInfo = BucketInfo' {_biBuckets = Nothing}


-- | A list of the calculated facet values and counts.
biBuckets :: Lens' BucketInfo [Bucket]
biBuckets = lens _biBuckets (\ s a -> s{_biBuckets = a}) . _Default . _Coerce

instance FromJSON BucketInfo where
        parseJSON
          = withObject "BucketInfo"
              (\ x -> BucketInfo' <$> (x .:? "buckets" .!= mempty))

instance Hashable BucketInfo where

instance NFData BucketInfo where

-- | A warning returned by the document service when an issue is discovered while processing an upload request.
--
--
--
-- /See:/ 'documentServiceWarning' smart constructor.
newtype DocumentServiceWarning = DocumentServiceWarning'
  { _dswMessage :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DocumentServiceWarning' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dswMessage' - The description for a warning returned by the document service.
documentServiceWarning
    :: DocumentServiceWarning
documentServiceWarning = DocumentServiceWarning' {_dswMessage = Nothing}


-- | The description for a warning returned by the document service.
dswMessage :: Lens' DocumentServiceWarning (Maybe Text)
dswMessage = lens _dswMessage (\ s a -> s{_dswMessage = a})

instance FromJSON DocumentServiceWarning where
        parseJSON
          = withObject "DocumentServiceWarning"
              (\ x ->
                 DocumentServiceWarning' <$> (x .:? "message"))

instance Hashable DocumentServiceWarning where

instance NFData DocumentServiceWarning where

-- | The statistics for a field calculated in the request.
--
--
--
-- /See:/ 'fieldStats' smart constructor.
data FieldStats = FieldStats'
  { _fsMax          :: !(Maybe Text)
  , _fsMean         :: !(Maybe Text)
  , _fsCount        :: !(Maybe Integer)
  , _fsMissing      :: !(Maybe Integer)
  , _fsStddev       :: !(Maybe Double)
  , _fsMin          :: !(Maybe Text)
  , _fsSumOfSquares :: !(Maybe Double)
  , _fsSum          :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FieldStats' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsMax' - The maximum value found in the specified field in the result set. If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @max@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @max@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
--
-- * 'fsMean' - The average of the values found in the specified field in the result set. If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @mean@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @mean@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
--
-- * 'fsCount' - The number of documents that contain a value in the specified field in the result set.
--
-- * 'fsMissing' - The number of documents that do not contain a value in the specified field in the result set.
--
-- * 'fsStddev' - The standard deviation of the values in the specified field in the result set.
--
-- * 'fsMin' - The minimum value found in the specified field in the result set. If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @min@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @min@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
--
-- * 'fsSumOfSquares' - The sum of all field values in the result set squared.
--
-- * 'fsSum' - The sum of the field values across the documents in the result set. @null@ for date fields.
fieldStats
    :: FieldStats
fieldStats =
  FieldStats'
    { _fsMax = Nothing
    , _fsMean = Nothing
    , _fsCount = Nothing
    , _fsMissing = Nothing
    , _fsStddev = Nothing
    , _fsMin = Nothing
    , _fsSumOfSquares = Nothing
    , _fsSum = Nothing
    }


-- | The maximum value found in the specified field in the result set. If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @max@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @max@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
fsMax :: Lens' FieldStats (Maybe Text)
fsMax = lens _fsMax (\ s a -> s{_fsMax = a})

-- | The average of the values found in the specified field in the result set. If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @mean@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @mean@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
fsMean :: Lens' FieldStats (Maybe Text)
fsMean = lens _fsMean (\ s a -> s{_fsMean = a})

-- | The number of documents that contain a value in the specified field in the result set.
fsCount :: Lens' FieldStats (Maybe Integer)
fsCount = lens _fsCount (\ s a -> s{_fsCount = a})

-- | The number of documents that do not contain a value in the specified field in the result set.
fsMissing :: Lens' FieldStats (Maybe Integer)
fsMissing = lens _fsMissing (\ s a -> s{_fsMissing = a})

-- | The standard deviation of the values in the specified field in the result set.
fsStddev :: Lens' FieldStats (Maybe Double)
fsStddev = lens _fsStddev (\ s a -> s{_fsStddev = a})

-- | The minimum value found in the specified field in the result set. If the field is numeric (@int@ , @int-array@ , @double@ , or @double-array@ ), @min@ is the string representation of a double-precision 64-bit floating point value. If the field is @date@ or @date-array@ , @min@ is the string representation of a date with the format specified in <http://tools.ietf.org/html/rfc3339 IETF RFC3339> : yyyy-mm-ddTHH:mm:ss.SSSZ.
fsMin :: Lens' FieldStats (Maybe Text)
fsMin = lens _fsMin (\ s a -> s{_fsMin = a})

-- | The sum of all field values in the result set squared.
fsSumOfSquares :: Lens' FieldStats (Maybe Double)
fsSumOfSquares = lens _fsSumOfSquares (\ s a -> s{_fsSumOfSquares = a})

-- | The sum of the field values across the documents in the result set. @null@ for date fields.
fsSum :: Lens' FieldStats (Maybe Double)
fsSum = lens _fsSum (\ s a -> s{_fsSum = a})

instance FromJSON FieldStats where
        parseJSON
          = withObject "FieldStats"
              (\ x ->
                 FieldStats' <$>
                   (x .:? "max") <*> (x .:? "mean") <*> (x .:? "count")
                     <*> (x .:? "missing")
                     <*> (x .:? "stddev")
                     <*> (x .:? "min")
                     <*> (x .:? "sumOfSquares")
                     <*> (x .:? "sum"))

instance Hashable FieldStats where

instance NFData FieldStats where

-- | Information about a document that matches the search request.
--
--
--
-- /See:/ 'hit' smart constructor.
data Hit = Hit'
  { _hitExprs      :: !(Maybe (Map Text Text))
  , _hitId         :: !(Maybe Text)
  , _hitHighlights :: !(Maybe (Map Text Text))
  , _hitFields     :: !(Maybe (Map Text [Text]))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Hit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hitExprs' - The expressions returned from a document that matches the search request.
--
-- * 'hitId' - The document ID of a document that matches the search request.
--
-- * 'hitHighlights' - The highlights returned from a document that matches the search request.
--
-- * 'hitFields' - The fields returned from a document that matches the search request.
hit
    :: Hit
hit =
  Hit'
    { _hitExprs = Nothing
    , _hitId = Nothing
    , _hitHighlights = Nothing
    , _hitFields = Nothing
    }


-- | The expressions returned from a document that matches the search request.
hitExprs :: Lens' Hit (HashMap Text Text)
hitExprs = lens _hitExprs (\ s a -> s{_hitExprs = a}) . _Default . _Map

-- | The document ID of a document that matches the search request.
hitId :: Lens' Hit (Maybe Text)
hitId = lens _hitId (\ s a -> s{_hitId = a})

-- | The highlights returned from a document that matches the search request.
hitHighlights :: Lens' Hit (HashMap Text Text)
hitHighlights = lens _hitHighlights (\ s a -> s{_hitHighlights = a}) . _Default . _Map

-- | The fields returned from a document that matches the search request.
hitFields :: Lens' Hit (HashMap Text [Text])
hitFields = lens _hitFields (\ s a -> s{_hitFields = a}) . _Default . _Map

instance FromJSON Hit where
        parseJSON
          = withObject "Hit"
              (\ x ->
                 Hit' <$>
                   (x .:? "exprs" .!= mempty) <*> (x .:? "id") <*>
                     (x .:? "highlights" .!= mempty)
                     <*> (x .:? "fields" .!= mempty))

instance Hashable Hit where

instance NFData Hit where

-- | The collection of documents that match the search request.
--
--
--
-- /See:/ 'hits' smart constructor.
data Hits = Hits'
  { _hCursor :: !(Maybe Text)
  , _hHit    :: !(Maybe [Hit])
  , _hStart  :: !(Maybe Integer)
  , _hFound  :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Hits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hCursor' - A cursor that can be used to retrieve the next set of matching documents when you want to page through a large result set.
--
-- * 'hHit' - A document that matches the search request.
--
-- * 'hStart' - The index of the first matching document.
--
-- * 'hFound' - The total number of documents that match the search request.
hits
    :: Hits
hits =
  Hits'
    {_hCursor = Nothing, _hHit = Nothing, _hStart = Nothing, _hFound = Nothing}


-- | A cursor that can be used to retrieve the next set of matching documents when you want to page through a large result set.
hCursor :: Lens' Hits (Maybe Text)
hCursor = lens _hCursor (\ s a -> s{_hCursor = a})

-- | A document that matches the search request.
hHit :: Lens' Hits [Hit]
hHit = lens _hHit (\ s a -> s{_hHit = a}) . _Default . _Coerce

-- | The index of the first matching document.
hStart :: Lens' Hits (Maybe Integer)
hStart = lens _hStart (\ s a -> s{_hStart = a})

-- | The total number of documents that match the search request.
hFound :: Lens' Hits (Maybe Integer)
hFound = lens _hFound (\ s a -> s{_hFound = a})

instance FromJSON Hits where
        parseJSON
          = withObject "Hits"
              (\ x ->
                 Hits' <$>
                   (x .:? "cursor") <*> (x .:? "hit" .!= mempty) <*>
                     (x .:? "start")
                     <*> (x .:? "found"))

instance Hashable Hits where

instance NFData Hits where

-- | Contains the resource id (@rid@ ) and the time it took to process the request (@timems@ ).
--
--
--
-- /See:/ 'searchStatus' smart constructor.
data SearchStatus = SearchStatus'
  { _sRid    :: !(Maybe Text)
  , _sTimems :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sRid' - The encrypted resource ID for the request.
--
-- * 'sTimems' - How long it took to process the request, in milliseconds.
searchStatus
    :: SearchStatus
searchStatus = SearchStatus' {_sRid = Nothing, _sTimems = Nothing}


-- | The encrypted resource ID for the request.
sRid :: Lens' SearchStatus (Maybe Text)
sRid = lens _sRid (\ s a -> s{_sRid = a})

-- | How long it took to process the request, in milliseconds.
sTimems :: Lens' SearchStatus (Maybe Integer)
sTimems = lens _sTimems (\ s a -> s{_sTimems = a})

instance FromJSON SearchStatus where
        parseJSON
          = withObject "SearchStatus"
              (\ x ->
                 SearchStatus' <$> (x .:? "rid") <*> (x .:? "timems"))

instance Hashable SearchStatus where

instance NFData SearchStatus where

-- | Container for the suggestion information returned in a @SuggestResponse@ .
--
--
--
-- /See:/ 'suggestModel' smart constructor.
data SuggestModel = SuggestModel'
  { _smFound       :: !(Maybe Integer)
  , _smSuggestions :: !(Maybe [SuggestionMatch])
  , _smQuery       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SuggestModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smFound' - The number of documents that were found to match the query string.
--
-- * 'smSuggestions' - The documents that match the query string.
--
-- * 'smQuery' - The query string specified in the suggest request.
suggestModel
    :: SuggestModel
suggestModel =
  SuggestModel'
    {_smFound = Nothing, _smSuggestions = Nothing, _smQuery = Nothing}


-- | The number of documents that were found to match the query string.
smFound :: Lens' SuggestModel (Maybe Integer)
smFound = lens _smFound (\ s a -> s{_smFound = a})

-- | The documents that match the query string.
smSuggestions :: Lens' SuggestModel [SuggestionMatch]
smSuggestions = lens _smSuggestions (\ s a -> s{_smSuggestions = a}) . _Default . _Coerce

-- | The query string specified in the suggest request.
smQuery :: Lens' SuggestModel (Maybe Text)
smQuery = lens _smQuery (\ s a -> s{_smQuery = a})

instance FromJSON SuggestModel where
        parseJSON
          = withObject "SuggestModel"
              (\ x ->
                 SuggestModel' <$>
                   (x .:? "found") <*> (x .:? "suggestions" .!= mempty)
                     <*> (x .:? "query"))

instance Hashable SuggestModel where

instance NFData SuggestModel where

-- | Contains the resource id (@rid@ ) and the time it took to process the request (@timems@ ).
--
--
--
-- /See:/ 'suggestStatus' smart constructor.
data SuggestStatus = SuggestStatus'
  { _ssRid    :: !(Maybe Text)
  , _ssTimems :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SuggestStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssRid' - The encrypted resource ID for the request.
--
-- * 'ssTimems' - How long it took to process the request, in milliseconds.
suggestStatus
    :: SuggestStatus
suggestStatus = SuggestStatus' {_ssRid = Nothing, _ssTimems = Nothing}


-- | The encrypted resource ID for the request.
ssRid :: Lens' SuggestStatus (Maybe Text)
ssRid = lens _ssRid (\ s a -> s{_ssRid = a})

-- | How long it took to process the request, in milliseconds.
ssTimems :: Lens' SuggestStatus (Maybe Integer)
ssTimems = lens _ssTimems (\ s a -> s{_ssTimems = a})

instance FromJSON SuggestStatus where
        parseJSON
          = withObject "SuggestStatus"
              (\ x ->
                 SuggestStatus' <$>
                   (x .:? "rid") <*> (x .:? "timems"))

instance Hashable SuggestStatus where

instance NFData SuggestStatus where

-- | An autocomplete suggestion that matches the query string specified in a @SuggestRequest@ .
--
--
--
-- /See:/ 'suggestionMatch' smart constructor.
data SuggestionMatch = SuggestionMatch'
  { _smSuggestion :: !(Maybe Text)
  , _smScore      :: !(Maybe Integer)
  , _smId         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SuggestionMatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smSuggestion' - The string that matches the query string specified in the @SuggestRequest@ .
--
-- * 'smScore' - The relevance score of a suggested match.
--
-- * 'smId' - The document ID of the suggested document.
suggestionMatch
    :: SuggestionMatch
suggestionMatch =
  SuggestionMatch'
    {_smSuggestion = Nothing, _smScore = Nothing, _smId = Nothing}


-- | The string that matches the query string specified in the @SuggestRequest@ .
smSuggestion :: Lens' SuggestionMatch (Maybe Text)
smSuggestion = lens _smSuggestion (\ s a -> s{_smSuggestion = a})

-- | The relevance score of a suggested match.
smScore :: Lens' SuggestionMatch (Maybe Integer)
smScore = lens _smScore (\ s a -> s{_smScore = a})

-- | The document ID of the suggested document.
smId :: Lens' SuggestionMatch (Maybe Text)
smId = lens _smId (\ s a -> s{_smId = a})

instance FromJSON SuggestionMatch where
        parseJSON
          = withObject "SuggestionMatch"
              (\ x ->
                 SuggestionMatch' <$>
                   (x .:? "suggestion") <*> (x .:? "score") <*>
                     (x .:? "id"))

instance Hashable SuggestionMatch where

instance NFData SuggestionMatch where
