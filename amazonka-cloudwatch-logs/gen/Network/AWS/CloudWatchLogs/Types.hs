{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.CloudWatchLogs.Types
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

module Network.AWS.CloudWatchLogs.Types
    (
    -- * Service
      CloudWatchLogs
    -- ** Errors
    , JSONError

    -- * FilteredLogEvent
    , FilteredLogEvent
    , filteredLogEvent
    , fleIngestionTime
    , fleTimestamp
    , fleEventId
    , fleLogStreamName
    , fleMessage

    -- * InputLogEvent
    , InputLogEvent
    , inputLogEvent
    , ileTimestamp
    , ileMessage

    -- * LogGroup
    , LogGroup
    , logGroup
    , lgCreationTime
    , lgMetricFilterCount
    , lgArn
    , lgRetentionInDays
    , lgStoredBytes
    , lgLogGroupName

    -- * LogStream
    , LogStream
    , logStream
    , lsCreationTime
    , lsArn
    , lsFirstEventTimestamp
    , lsStoredBytes
    , lsLastIngestionTime
    , lsLastEventTimestamp
    , lsUploadSequenceToken
    , lsLogStreamName

    -- * MetricFilter
    , MetricFilter
    , metricFilter
    , mfCreationTime
    , mfFilterPattern
    , mfFilterName
    , mfMetricTransformations

    -- * MetricFilterMatchRecord
    , MetricFilterMatchRecord
    , metricFilterMatchRecord
    , mfmrExtractedValues
    , mfmrEventNumber
    , mfmrEventMessage

    -- * MetricTransformation
    , MetricTransformation
    , metricTransformation
    , mtMetricName
    , mtMetricNamespace
    , mtMetricValue

    -- * OrderBy
    , OrderBy (..)

    -- * OutputLogEvent
    , OutputLogEvent
    , outputLogEvent
    , oleIngestionTime
    , oleTimestamp
    , oleMessage

    -- * RejectedLogEventsInfo
    , RejectedLogEventsInfo
    , rejectedLogEventsInfo
    , rleiTooOldLogEventEndIndex
    , rleiTooNewLogEventStartIndex
    , rleiExpiredLogEventEndIndex

    -- * SearchedLogStream
    , SearchedLogStream
    , searchedLogStream
    , slsSearchedCompletely
    , slsLogStreamName

    -- * SubscriptionFilter
    , SubscriptionFilter
    , subscriptionFilter
    , sfCreationTime
    , sfFilterPattern
    , sfFilterName
    , sfDestinationARN
    , sfLogGroupName
    , sfRoleARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2014-03-28@ of the Amazon CloudWatch Logs SDK.
data CloudWatchLogs

instance AWSService CloudWatchLogs where
    type Sg CloudWatchLogs = V4
    type Er CloudWatchLogs = JSONError

    service = service'
      where
        service' :: Service CloudWatchLogs
        service' = Service
            { _svcAbbrev  = "CloudWatchLogs"
            , _svcPrefix  = "logs"
            , _svcVersion = "2014-03-28"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry CloudWatchLogs
        retry = undefined

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

-- | /See:/ 'filteredLogEvent' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fleIngestionTime'
--
-- * 'fleTimestamp'
--
-- * 'fleEventId'
--
-- * 'fleLogStreamName'
--
-- * 'fleMessage'
data FilteredLogEvent = FilteredLogEvent'{_fleIngestionTime :: Maybe Nat, _fleTimestamp :: Maybe Nat, _fleEventId :: Maybe Text, _fleLogStreamName :: Text, _fleMessage :: Text} deriving (Eq, Read, Show)

-- | 'FilteredLogEvent' smart constructor.
filteredLogEvent :: Text -> Text -> FilteredLogEvent
filteredLogEvent pLogStreamName pMessage = FilteredLogEvent'{_fleIngestionTime = Nothing, _fleTimestamp = Nothing, _fleEventId = Nothing, _fleLogStreamName = pLogStreamName, _fleMessage = pMessage};

-- | FIXME: Undocumented member.
fleIngestionTime :: Lens' FilteredLogEvent (Maybe Natural)
fleIngestionTime = lens _fleIngestionTime (\ s a -> s{_fleIngestionTime = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
fleTimestamp :: Lens' FilteredLogEvent (Maybe Natural)
fleTimestamp = lens _fleTimestamp (\ s a -> s{_fleTimestamp = a}) . mapping _Nat;

-- | A unique identifier for this event.
fleEventId :: Lens' FilteredLogEvent (Maybe Text)
fleEventId = lens _fleEventId (\ s a -> s{_fleEventId = a});

-- | The name of the log stream this event belongs to.
fleLogStreamName :: Lens' FilteredLogEvent Text
fleLogStreamName = lens _fleLogStreamName (\ s a -> s{_fleLogStreamName = a});

-- | The data contained in the log event.
fleMessage :: Lens' FilteredLogEvent Text
fleMessage = lens _fleMessage (\ s a -> s{_fleMessage = a});

instance FromJSON FilteredLogEvent where
        parseJSON
          = withObject "FilteredLogEvent"
              (\ x ->
                 FilteredLogEvent' <$>
                   x .:? "ingestionTime" <*> x .:? "timestamp" <*>
                     x .:? "eventId"
                     <*> x .: "logStreamName"
                     <*> x .: "message")

-- | /See:/ 'inputLogEvent' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ileTimestamp'
--
-- * 'ileMessage'
data InputLogEvent = InputLogEvent'{_ileTimestamp :: Nat, _ileMessage :: Text} deriving (Eq, Read, Show)

-- | 'InputLogEvent' smart constructor.
inputLogEvent :: Natural -> Text -> InputLogEvent
inputLogEvent pTimestamp pMessage = InputLogEvent'{_ileTimestamp = _Nat # pTimestamp, _ileMessage = pMessage};

-- | FIXME: Undocumented member.
ileTimestamp :: Lens' InputLogEvent Natural
ileTimestamp = lens _ileTimestamp (\ s a -> s{_ileTimestamp = a}) . _Nat;

-- | FIXME: Undocumented member.
ileMessage :: Lens' InputLogEvent Text
ileMessage = lens _ileMessage (\ s a -> s{_ileMessage = a});

instance ToJSON InputLogEvent where
        toJSON InputLogEvent'{..}
          = object
              ["timestamp" .= _ileTimestamp,
               "message" .= _ileMessage]

-- | /See:/ 'logGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgCreationTime'
--
-- * 'lgMetricFilterCount'
--
-- * 'lgArn'
--
-- * 'lgRetentionInDays'
--
-- * 'lgStoredBytes'
--
-- * 'lgLogGroupName'
data LogGroup = LogGroup'{_lgCreationTime :: Maybe Nat, _lgMetricFilterCount :: Maybe Int, _lgArn :: Maybe Text, _lgRetentionInDays :: Maybe Int, _lgStoredBytes :: Maybe Nat, _lgLogGroupName :: Text} deriving (Eq, Read, Show)

-- | 'LogGroup' smart constructor.
logGroup :: Text -> LogGroup
logGroup pLogGroupName = LogGroup'{_lgCreationTime = Nothing, _lgMetricFilterCount = Nothing, _lgArn = Nothing, _lgRetentionInDays = Nothing, _lgStoredBytes = Nothing, _lgLogGroupName = pLogGroupName};

-- | FIXME: Undocumented member.
lgCreationTime :: Lens' LogGroup (Maybe Natural)
lgCreationTime = lens _lgCreationTime (\ s a -> s{_lgCreationTime = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
lgMetricFilterCount :: Lens' LogGroup (Maybe Int)
lgMetricFilterCount = lens _lgMetricFilterCount (\ s a -> s{_lgMetricFilterCount = a});

-- | FIXME: Undocumented member.
lgArn :: Lens' LogGroup (Maybe Text)
lgArn = lens _lgArn (\ s a -> s{_lgArn = a});

-- | FIXME: Undocumented member.
lgRetentionInDays :: Lens' LogGroup (Maybe Int)
lgRetentionInDays = lens _lgRetentionInDays (\ s a -> s{_lgRetentionInDays = a});

-- | FIXME: Undocumented member.
lgStoredBytes :: Lens' LogGroup (Maybe Natural)
lgStoredBytes = lens _lgStoredBytes (\ s a -> s{_lgStoredBytes = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
lgLogGroupName :: Lens' LogGroup Text
lgLogGroupName = lens _lgLogGroupName (\ s a -> s{_lgLogGroupName = a});

instance FromJSON LogGroup where
        parseJSON
          = withObject "LogGroup"
              (\ x ->
                 LogGroup' <$>
                   x .:? "creationTime" <*> x .:? "metricFilterCount"
                     <*> x .:? "arn"
                     <*> x .:? "retentionInDays"
                     <*> x .:? "storedBytes"
                     <*> x .: "logGroupName")

-- | /See:/ 'logStream' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsCreationTime'
--
-- * 'lsArn'
--
-- * 'lsFirstEventTimestamp'
--
-- * 'lsStoredBytes'
--
-- * 'lsLastIngestionTime'
--
-- * 'lsLastEventTimestamp'
--
-- * 'lsUploadSequenceToken'
--
-- * 'lsLogStreamName'
data LogStream = LogStream'{_lsCreationTime :: Maybe Nat, _lsArn :: Maybe Text, _lsFirstEventTimestamp :: Maybe Nat, _lsStoredBytes :: Maybe Nat, _lsLastIngestionTime :: Maybe Nat, _lsLastEventTimestamp :: Maybe Nat, _lsUploadSequenceToken :: Text, _lsLogStreamName :: Text} deriving (Eq, Read, Show)

-- | 'LogStream' smart constructor.
logStream :: Text -> Text -> LogStream
logStream pUploadSequenceToken pLogStreamName = LogStream'{_lsCreationTime = Nothing, _lsArn = Nothing, _lsFirstEventTimestamp = Nothing, _lsStoredBytes = Nothing, _lsLastIngestionTime = Nothing, _lsLastEventTimestamp = Nothing, _lsUploadSequenceToken = pUploadSequenceToken, _lsLogStreamName = pLogStreamName};

-- | FIXME: Undocumented member.
lsCreationTime :: Lens' LogStream (Maybe Natural)
lsCreationTime = lens _lsCreationTime (\ s a -> s{_lsCreationTime = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
lsArn :: Lens' LogStream (Maybe Text)
lsArn = lens _lsArn (\ s a -> s{_lsArn = a});

-- | FIXME: Undocumented member.
lsFirstEventTimestamp :: Lens' LogStream (Maybe Natural)
lsFirstEventTimestamp = lens _lsFirstEventTimestamp (\ s a -> s{_lsFirstEventTimestamp = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
lsStoredBytes :: Lens' LogStream (Maybe Natural)
lsStoredBytes = lens _lsStoredBytes (\ s a -> s{_lsStoredBytes = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
lsLastIngestionTime :: Lens' LogStream (Maybe Natural)
lsLastIngestionTime = lens _lsLastIngestionTime (\ s a -> s{_lsLastIngestionTime = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
lsLastEventTimestamp :: Lens' LogStream (Maybe Natural)
lsLastEventTimestamp = lens _lsLastEventTimestamp (\ s a -> s{_lsLastEventTimestamp = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
lsUploadSequenceToken :: Lens' LogStream Text
lsUploadSequenceToken = lens _lsUploadSequenceToken (\ s a -> s{_lsUploadSequenceToken = a});

-- | FIXME: Undocumented member.
lsLogStreamName :: Lens' LogStream Text
lsLogStreamName = lens _lsLogStreamName (\ s a -> s{_lsLogStreamName = a});

instance FromJSON LogStream where
        parseJSON
          = withObject "LogStream"
              (\ x ->
                 LogStream' <$>
                   x .:? "creationTime" <*> x .:? "arn" <*>
                     x .:? "firstEventTimestamp"
                     <*> x .:? "storedBytes"
                     <*> x .:? "lastIngestionTime"
                     <*> x .:? "lastEventTimestamp"
                     <*> x .: "uploadSequenceToken"
                     <*> x .: "logStreamName")

-- | /See:/ 'metricFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mfCreationTime'
--
-- * 'mfFilterPattern'
--
-- * 'mfFilterName'
--
-- * 'mfMetricTransformations'
data MetricFilter = MetricFilter'{_mfCreationTime :: Maybe Nat, _mfFilterPattern :: Maybe Text, _mfFilterName :: Text, _mfMetricTransformations :: List1 MetricTransformation} deriving (Eq, Read, Show)

-- | 'MetricFilter' smart constructor.
metricFilter :: Text -> NonEmpty MetricTransformation -> MetricFilter
metricFilter pFilterName pMetricTransformations = MetricFilter'{_mfCreationTime = Nothing, _mfFilterPattern = Nothing, _mfFilterName = pFilterName, _mfMetricTransformations = _List1 # pMetricTransformations};

-- | FIXME: Undocumented member.
mfCreationTime :: Lens' MetricFilter (Maybe Natural)
mfCreationTime = lens _mfCreationTime (\ s a -> s{_mfCreationTime = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
mfFilterPattern :: Lens' MetricFilter (Maybe Text)
mfFilterPattern = lens _mfFilterPattern (\ s a -> s{_mfFilterPattern = a});

-- | FIXME: Undocumented member.
mfFilterName :: Lens' MetricFilter Text
mfFilterName = lens _mfFilterName (\ s a -> s{_mfFilterName = a});

-- | FIXME: Undocumented member.
mfMetricTransformations :: Lens' MetricFilter (NonEmpty MetricTransformation)
mfMetricTransformations = lens _mfMetricTransformations (\ s a -> s{_mfMetricTransformations = a}) . _List1;

instance FromJSON MetricFilter where
        parseJSON
          = withObject "MetricFilter"
              (\ x ->
                 MetricFilter' <$>
                   x .:? "creationTime" <*> x .:? "filterPattern" <*>
                     x .: "filterName"
                     <*> x .: "metricTransformations")

-- | /See:/ 'metricFilterMatchRecord' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mfmrExtractedValues'
--
-- * 'mfmrEventNumber'
--
-- * 'mfmrEventMessage'
data MetricFilterMatchRecord = MetricFilterMatchRecord'{_mfmrExtractedValues :: HashMap Text Text, _mfmrEventNumber :: Maybe Integer, _mfmrEventMessage :: Text} deriving (Eq, Read, Show)

-- | 'MetricFilterMatchRecord' smart constructor.
metricFilterMatchRecord :: Text -> MetricFilterMatchRecord
metricFilterMatchRecord pEventMessage = MetricFilterMatchRecord'{_mfmrExtractedValues = mempty, _mfmrEventNumber = Nothing, _mfmrEventMessage = pEventMessage};

-- | FIXME: Undocumented member.
mfmrExtractedValues :: Lens' MetricFilterMatchRecord (HashMap Text Text)
mfmrExtractedValues = lens _mfmrExtractedValues (\ s a -> s{_mfmrExtractedValues = a}) . _Coerce;

-- | FIXME: Undocumented member.
mfmrEventNumber :: Lens' MetricFilterMatchRecord (Maybe Integer)
mfmrEventNumber = lens _mfmrEventNumber (\ s a -> s{_mfmrEventNumber = a});

-- | FIXME: Undocumented member.
mfmrEventMessage :: Lens' MetricFilterMatchRecord Text
mfmrEventMessage = lens _mfmrEventMessage (\ s a -> s{_mfmrEventMessage = a});

instance FromJSON MetricFilterMatchRecord where
        parseJSON
          = withObject "MetricFilterMatchRecord"
              (\ x ->
                 MetricFilterMatchRecord' <$>
                   x .:? "extractedValues" .!= mempty <*>
                     x .:? "eventNumber"
                     <*> x .: "eventMessage")

-- | /See:/ 'metricTransformation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mtMetricName'
--
-- * 'mtMetricNamespace'
--
-- * 'mtMetricValue'
data MetricTransformation = MetricTransformation'{_mtMetricName :: Text, _mtMetricNamespace :: Text, _mtMetricValue :: Text} deriving (Eq, Read, Show)

-- | 'MetricTransformation' smart constructor.
metricTransformation :: Text -> Text -> Text -> MetricTransformation
metricTransformation pMetricName pMetricNamespace pMetricValue = MetricTransformation'{_mtMetricName = pMetricName, _mtMetricNamespace = pMetricNamespace, _mtMetricValue = pMetricValue};

-- | FIXME: Undocumented member.
mtMetricName :: Lens' MetricTransformation Text
mtMetricName = lens _mtMetricName (\ s a -> s{_mtMetricName = a});

-- | FIXME: Undocumented member.
mtMetricNamespace :: Lens' MetricTransformation Text
mtMetricNamespace = lens _mtMetricNamespace (\ s a -> s{_mtMetricNamespace = a});

-- | FIXME: Undocumented member.
mtMetricValue :: Lens' MetricTransformation Text
mtMetricValue = lens _mtMetricValue (\ s a -> s{_mtMetricValue = a});

instance FromJSON MetricTransformation where
        parseJSON
          = withObject "MetricTransformation"
              (\ x ->
                 MetricTransformation' <$>
                   x .: "metricName" <*> x .: "metricNamespace" <*>
                     x .: "metricValue")

instance ToJSON MetricTransformation where
        toJSON MetricTransformation'{..}
          = object
              ["metricName" .= _mtMetricName,
               "metricNamespace" .= _mtMetricNamespace,
               "metricValue" .= _mtMetricValue]

data OrderBy = LogStreamName | LastEventTime deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText OrderBy where
    parser = takeLowerText >>= \case
        "LastEventTime" -> pure LastEventTime
        "LogStreamName" -> pure LogStreamName
        e -> fail ("Failure parsing OrderBy from " ++ show e)

instance ToText OrderBy where
    toText = \case
        LastEventTime -> "LastEventTime"
        LogStreamName -> "LogStreamName"

instance Hashable OrderBy
instance ToQuery OrderBy
instance ToHeader OrderBy

instance ToJSON OrderBy where
    toJSON = toJSONText

-- | /See:/ 'outputLogEvent' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oleIngestionTime'
--
-- * 'oleTimestamp'
--
-- * 'oleMessage'
data OutputLogEvent = OutputLogEvent'{_oleIngestionTime :: Maybe Nat, _oleTimestamp :: Maybe Nat, _oleMessage :: Text} deriving (Eq, Read, Show)

-- | 'OutputLogEvent' smart constructor.
outputLogEvent :: Text -> OutputLogEvent
outputLogEvent pMessage = OutputLogEvent'{_oleIngestionTime = Nothing, _oleTimestamp = Nothing, _oleMessage = pMessage};

-- | FIXME: Undocumented member.
oleIngestionTime :: Lens' OutputLogEvent (Maybe Natural)
oleIngestionTime = lens _oleIngestionTime (\ s a -> s{_oleIngestionTime = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
oleTimestamp :: Lens' OutputLogEvent (Maybe Natural)
oleTimestamp = lens _oleTimestamp (\ s a -> s{_oleTimestamp = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
oleMessage :: Lens' OutputLogEvent Text
oleMessage = lens _oleMessage (\ s a -> s{_oleMessage = a});

instance FromJSON OutputLogEvent where
        parseJSON
          = withObject "OutputLogEvent"
              (\ x ->
                 OutputLogEvent' <$>
                   x .:? "ingestionTime" <*> x .:? "timestamp" <*>
                     x .: "message")

-- | /See:/ 'rejectedLogEventsInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rleiTooOldLogEventEndIndex'
--
-- * 'rleiTooNewLogEventStartIndex'
--
-- * 'rleiExpiredLogEventEndIndex'
data RejectedLogEventsInfo = RejectedLogEventsInfo'{_rleiTooOldLogEventEndIndex :: Maybe Int, _rleiTooNewLogEventStartIndex :: Maybe Int, _rleiExpiredLogEventEndIndex :: Maybe Int} deriving (Eq, Read, Show)

-- | 'RejectedLogEventsInfo' smart constructor.
rejectedLogEventsInfo :: RejectedLogEventsInfo
rejectedLogEventsInfo = RejectedLogEventsInfo'{_rleiTooOldLogEventEndIndex = Nothing, _rleiTooNewLogEventStartIndex = Nothing, _rleiExpiredLogEventEndIndex = Nothing};

-- | FIXME: Undocumented member.
rleiTooOldLogEventEndIndex :: Lens' RejectedLogEventsInfo (Maybe Int)
rleiTooOldLogEventEndIndex = lens _rleiTooOldLogEventEndIndex (\ s a -> s{_rleiTooOldLogEventEndIndex = a});

-- | FIXME: Undocumented member.
rleiTooNewLogEventStartIndex :: Lens' RejectedLogEventsInfo (Maybe Int)
rleiTooNewLogEventStartIndex = lens _rleiTooNewLogEventStartIndex (\ s a -> s{_rleiTooNewLogEventStartIndex = a});

-- | FIXME: Undocumented member.
rleiExpiredLogEventEndIndex :: Lens' RejectedLogEventsInfo (Maybe Int)
rleiExpiredLogEventEndIndex = lens _rleiExpiredLogEventEndIndex (\ s a -> s{_rleiExpiredLogEventEndIndex = a});

instance FromJSON RejectedLogEventsInfo where
        parseJSON
          = withObject "RejectedLogEventsInfo"
              (\ x ->
                 RejectedLogEventsInfo' <$>
                   x .:? "tooOldLogEventEndIndex" <*>
                     x .:? "tooNewLogEventStartIndex"
                     <*> x .:? "expiredLogEventEndIndex")

-- | /See:/ 'searchedLogStream' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slsSearchedCompletely'
--
-- * 'slsLogStreamName'
data SearchedLogStream = SearchedLogStream'{_slsSearchedCompletely :: Maybe Bool, _slsLogStreamName :: Text} deriving (Eq, Read, Show)

-- | 'SearchedLogStream' smart constructor.
searchedLogStream :: Text -> SearchedLogStream
searchedLogStream pLogStreamName = SearchedLogStream'{_slsSearchedCompletely = Nothing, _slsLogStreamName = pLogStreamName};

-- | Indicates whether all the events in this log stream were searched or
-- more data exists to search by paginating further.
slsSearchedCompletely :: Lens' SearchedLogStream (Maybe Bool)
slsSearchedCompletely = lens _slsSearchedCompletely (\ s a -> s{_slsSearchedCompletely = a});

-- | The name of the log stream.
slsLogStreamName :: Lens' SearchedLogStream Text
slsLogStreamName = lens _slsLogStreamName (\ s a -> s{_slsLogStreamName = a});

instance FromJSON SearchedLogStream where
        parseJSON
          = withObject "SearchedLogStream"
              (\ x ->
                 SearchedLogStream' <$>
                   x .:? "searchedCompletely" <*> x .: "logStreamName")

-- | /See:/ 'subscriptionFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sfCreationTime'
--
-- * 'sfFilterPattern'
--
-- * 'sfFilterName'
--
-- * 'sfDestinationARN'
--
-- * 'sfLogGroupName'
--
-- * 'sfRoleARN'
data SubscriptionFilter = SubscriptionFilter'{_sfCreationTime :: Maybe Nat, _sfFilterPattern :: Maybe Text, _sfFilterName :: Text, _sfDestinationARN :: Text, _sfLogGroupName :: Text, _sfRoleARN :: Text} deriving (Eq, Read, Show)

-- | 'SubscriptionFilter' smart constructor.
subscriptionFilter :: Text -> Text -> Text -> Text -> SubscriptionFilter
subscriptionFilter pFilterName pDestinationARN pLogGroupName pRoleARN = SubscriptionFilter'{_sfCreationTime = Nothing, _sfFilterPattern = Nothing, _sfFilterName = pFilterName, _sfDestinationARN = pDestinationARN, _sfLogGroupName = pLogGroupName, _sfRoleARN = pRoleARN};

-- | FIXME: Undocumented member.
sfCreationTime :: Lens' SubscriptionFilter (Maybe Natural)
sfCreationTime = lens _sfCreationTime (\ s a -> s{_sfCreationTime = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
sfFilterPattern :: Lens' SubscriptionFilter (Maybe Text)
sfFilterPattern = lens _sfFilterPattern (\ s a -> s{_sfFilterPattern = a});

-- | FIXME: Undocumented member.
sfFilterName :: Lens' SubscriptionFilter Text
sfFilterName = lens _sfFilterName (\ s a -> s{_sfFilterName = a});

-- | FIXME: Undocumented member.
sfDestinationARN :: Lens' SubscriptionFilter Text
sfDestinationARN = lens _sfDestinationARN (\ s a -> s{_sfDestinationARN = a});

-- | FIXME: Undocumented member.
sfLogGroupName :: Lens' SubscriptionFilter Text
sfLogGroupName = lens _sfLogGroupName (\ s a -> s{_sfLogGroupName = a});

-- | FIXME: Undocumented member.
sfRoleARN :: Lens' SubscriptionFilter Text
sfRoleARN = lens _sfRoleARN (\ s a -> s{_sfRoleARN = a});

instance FromJSON SubscriptionFilter where
        parseJSON
          = withObject "SubscriptionFilter"
              (\ x ->
                 SubscriptionFilter' <$>
                   x .:? "creationTime" <*> x .:? "filterPattern" <*>
                     x .: "filterName"
                     <*> x .: "destinationArn"
                     <*> x .: "logGroupName"
                     <*> x .: "roleArn")
