{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.Product where

import           Network.AWS.CloudWatchLogs.Types.Sum
import           Network.AWS.Prelude

-- | /See:/ 'destination' smart constructor.
data Destination = Destination'
    { _dTargetARN       :: !(Maybe Text)
    , _dCreationTime    :: !(Maybe Nat)
    , _dArn             :: !(Maybe Text)
    , _dAccessPolicy    :: !(Maybe Text)
    , _dDestinationName :: !(Maybe Text)
    , _dRoleARN         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Destination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dTargetARN'
--
-- * 'dCreationTime'
--
-- * 'dArn'
--
-- * 'dAccessPolicy'
--
-- * 'dDestinationName'
--
-- * 'dRoleARN'
destination
    :: Destination
destination =
    Destination'
    { _dTargetARN = Nothing
    , _dCreationTime = Nothing
    , _dArn = Nothing
    , _dAccessPolicy = Nothing
    , _dDestinationName = Nothing
    , _dRoleARN = Nothing
    }

-- | Undocumented member.
dTargetARN :: Lens' Destination (Maybe Text)
dTargetARN = lens _dTargetARN (\ s a -> s{_dTargetARN = a});

-- | Undocumented member.
dCreationTime :: Lens' Destination (Maybe Natural)
dCreationTime = lens _dCreationTime (\ s a -> s{_dCreationTime = a}) . mapping _Nat;

-- | Undocumented member.
dArn :: Lens' Destination (Maybe Text)
dArn = lens _dArn (\ s a -> s{_dArn = a});

-- | Undocumented member.
dAccessPolicy :: Lens' Destination (Maybe Text)
dAccessPolicy = lens _dAccessPolicy (\ s a -> s{_dAccessPolicy = a});

-- | Undocumented member.
dDestinationName :: Lens' Destination (Maybe Text)
dDestinationName = lens _dDestinationName (\ s a -> s{_dDestinationName = a});

-- | Undocumented member.
dRoleARN :: Lens' Destination (Maybe Text)
dRoleARN = lens _dRoleARN (\ s a -> s{_dRoleARN = a});

instance FromJSON Destination where
        parseJSON
          = withObject "Destination"
              (\ x ->
                 Destination' <$>
                   (x .:? "targetArn") <*> (x .:? "creationTime") <*>
                     (x .:? "arn")
                     <*> (x .:? "accessPolicy")
                     <*> (x .:? "destinationName")
                     <*> (x .:? "roleArn"))

-- | Represents a matched event from a 'FilterLogEvents' request.
--
-- /See:/ 'filteredLogEvent' smart constructor.
data FilteredLogEvent = FilteredLogEvent'
    { _fleIngestionTime :: !(Maybe Nat)
    , _fleLogStreamName :: !(Maybe Text)
    , _fleMessage       :: !(Maybe Text)
    , _fleTimestamp     :: !(Maybe Nat)
    , _fleEventId       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FilteredLogEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fleIngestionTime'
--
-- * 'fleLogStreamName'
--
-- * 'fleMessage'
--
-- * 'fleTimestamp'
--
-- * 'fleEventId'
filteredLogEvent
    :: FilteredLogEvent
filteredLogEvent =
    FilteredLogEvent'
    { _fleIngestionTime = Nothing
    , _fleLogStreamName = Nothing
    , _fleMessage = Nothing
    , _fleTimestamp = Nothing
    , _fleEventId = Nothing
    }

-- | Undocumented member.
fleIngestionTime :: Lens' FilteredLogEvent (Maybe Natural)
fleIngestionTime = lens _fleIngestionTime (\ s a -> s{_fleIngestionTime = a}) . mapping _Nat;

-- | The name of the log stream this event belongs to.
fleLogStreamName :: Lens' FilteredLogEvent (Maybe Text)
fleLogStreamName = lens _fleLogStreamName (\ s a -> s{_fleLogStreamName = a});

-- | The data contained in the log event.
fleMessage :: Lens' FilteredLogEvent (Maybe Text)
fleMessage = lens _fleMessage (\ s a -> s{_fleMessage = a});

-- | Undocumented member.
fleTimestamp :: Lens' FilteredLogEvent (Maybe Natural)
fleTimestamp = lens _fleTimestamp (\ s a -> s{_fleTimestamp = a}) . mapping _Nat;

-- | A unique identifier for this event.
fleEventId :: Lens' FilteredLogEvent (Maybe Text)
fleEventId = lens _fleEventId (\ s a -> s{_fleEventId = a});

instance FromJSON FilteredLogEvent where
        parseJSON
          = withObject "FilteredLogEvent"
              (\ x ->
                 FilteredLogEvent' <$>
                   (x .:? "ingestionTime") <*> (x .:? "logStreamName")
                     <*> (x .:? "message")
                     <*> (x .:? "timestamp")
                     <*> (x .:? "eventId"))

-- | A log event is a record of some activity that was recorded by the
-- application or resource being monitored. The log event record that
-- Amazon CloudWatch Logs understands contains two properties: the
-- timestamp of when the event occurred, and the raw event message.
--
-- /See:/ 'inputLogEvent' smart constructor.
data InputLogEvent = InputLogEvent'
    { _ileTimestamp :: !Nat
    , _ileMessage   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InputLogEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ileTimestamp'
--
-- * 'ileMessage'
inputLogEvent
    :: Natural -- ^ 'ileTimestamp'
    -> Text -- ^ 'ileMessage'
    -> InputLogEvent
inputLogEvent pTimestamp_ pMessage_ =
    InputLogEvent'
    { _ileTimestamp = _Nat # pTimestamp_
    , _ileMessage = pMessage_
    }

-- | Undocumented member.
ileTimestamp :: Lens' InputLogEvent Natural
ileTimestamp = lens _ileTimestamp (\ s a -> s{_ileTimestamp = a}) . _Nat;

-- | Undocumented member.
ileMessage :: Lens' InputLogEvent Text
ileMessage = lens _ileMessage (\ s a -> s{_ileMessage = a});

instance ToJSON InputLogEvent where
        toJSON InputLogEvent'{..}
          = object
              (catMaybes
                 [Just ("timestamp" .= _ileTimestamp),
                  Just ("message" .= _ileMessage)])

-- | /See:/ 'logGroup' smart constructor.
data LogGroup = LogGroup'
    { _lgCreationTime      :: !(Maybe Nat)
    , _lgMetricFilterCount :: !(Maybe Int)
    , _lgArn               :: !(Maybe Text)
    , _lgLogGroupName      :: !(Maybe Text)
    , _lgRetentionInDays   :: !(Maybe Int)
    , _lgStoredBytes       :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LogGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgCreationTime'
--
-- * 'lgMetricFilterCount'
--
-- * 'lgArn'
--
-- * 'lgLogGroupName'
--
-- * 'lgRetentionInDays'
--
-- * 'lgStoredBytes'
logGroup
    :: LogGroup
logGroup =
    LogGroup'
    { _lgCreationTime = Nothing
    , _lgMetricFilterCount = Nothing
    , _lgArn = Nothing
    , _lgLogGroupName = Nothing
    , _lgRetentionInDays = Nothing
    , _lgStoredBytes = Nothing
    }

-- | Undocumented member.
lgCreationTime :: Lens' LogGroup (Maybe Natural)
lgCreationTime = lens _lgCreationTime (\ s a -> s{_lgCreationTime = a}) . mapping _Nat;

-- | Undocumented member.
lgMetricFilterCount :: Lens' LogGroup (Maybe Int)
lgMetricFilterCount = lens _lgMetricFilterCount (\ s a -> s{_lgMetricFilterCount = a});

-- | Undocumented member.
lgArn :: Lens' LogGroup (Maybe Text)
lgArn = lens _lgArn (\ s a -> s{_lgArn = a});

-- | Undocumented member.
lgLogGroupName :: Lens' LogGroup (Maybe Text)
lgLogGroupName = lens _lgLogGroupName (\ s a -> s{_lgLogGroupName = a});

-- | Undocumented member.
lgRetentionInDays :: Lens' LogGroup (Maybe Int)
lgRetentionInDays = lens _lgRetentionInDays (\ s a -> s{_lgRetentionInDays = a});

-- | Undocumented member.
lgStoredBytes :: Lens' LogGroup (Maybe Natural)
lgStoredBytes = lens _lgStoredBytes (\ s a -> s{_lgStoredBytes = a}) . mapping _Nat;

instance FromJSON LogGroup where
        parseJSON
          = withObject "LogGroup"
              (\ x ->
                 LogGroup' <$>
                   (x .:? "creationTime") <*>
                     (x .:? "metricFilterCount")
                     <*> (x .:? "arn")
                     <*> (x .:? "logGroupName")
                     <*> (x .:? "retentionInDays")
                     <*> (x .:? "storedBytes"))

-- | A log stream is sequence of log events from a single emitter of logs.
--
-- /See:/ 'logStream' smart constructor.
data LogStream = LogStream'
    { _lsCreationTime        :: !(Maybe Nat)
    , _lsUploadSequenceToken :: !(Maybe Text)
    , _lsArn                 :: !(Maybe Text)
    , _lsFirstEventTimestamp :: !(Maybe Nat)
    , _lsLogStreamName       :: !(Maybe Text)
    , _lsStoredBytes         :: !(Maybe Nat)
    , _lsLastIngestionTime   :: !(Maybe Nat)
    , _lsLastEventTimestamp  :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LogStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsCreationTime'
--
-- * 'lsUploadSequenceToken'
--
-- * 'lsArn'
--
-- * 'lsFirstEventTimestamp'
--
-- * 'lsLogStreamName'
--
-- * 'lsStoredBytes'
--
-- * 'lsLastIngestionTime'
--
-- * 'lsLastEventTimestamp'
logStream
    :: LogStream
logStream =
    LogStream'
    { _lsCreationTime = Nothing
    , _lsUploadSequenceToken = Nothing
    , _lsArn = Nothing
    , _lsFirstEventTimestamp = Nothing
    , _lsLogStreamName = Nothing
    , _lsStoredBytes = Nothing
    , _lsLastIngestionTime = Nothing
    , _lsLastEventTimestamp = Nothing
    }

-- | Undocumented member.
lsCreationTime :: Lens' LogStream (Maybe Natural)
lsCreationTime = lens _lsCreationTime (\ s a -> s{_lsCreationTime = a}) . mapping _Nat;

-- | Undocumented member.
lsUploadSequenceToken :: Lens' LogStream (Maybe Text)
lsUploadSequenceToken = lens _lsUploadSequenceToken (\ s a -> s{_lsUploadSequenceToken = a});

-- | Undocumented member.
lsArn :: Lens' LogStream (Maybe Text)
lsArn = lens _lsArn (\ s a -> s{_lsArn = a});

-- | Undocumented member.
lsFirstEventTimestamp :: Lens' LogStream (Maybe Natural)
lsFirstEventTimestamp = lens _lsFirstEventTimestamp (\ s a -> s{_lsFirstEventTimestamp = a}) . mapping _Nat;

-- | Undocumented member.
lsLogStreamName :: Lens' LogStream (Maybe Text)
lsLogStreamName = lens _lsLogStreamName (\ s a -> s{_lsLogStreamName = a});

-- | Undocumented member.
lsStoredBytes :: Lens' LogStream (Maybe Natural)
lsStoredBytes = lens _lsStoredBytes (\ s a -> s{_lsStoredBytes = a}) . mapping _Nat;

-- | Undocumented member.
lsLastIngestionTime :: Lens' LogStream (Maybe Natural)
lsLastIngestionTime = lens _lsLastIngestionTime (\ s a -> s{_lsLastIngestionTime = a}) . mapping _Nat;

-- | Undocumented member.
lsLastEventTimestamp :: Lens' LogStream (Maybe Natural)
lsLastEventTimestamp = lens _lsLastEventTimestamp (\ s a -> s{_lsLastEventTimestamp = a}) . mapping _Nat;

instance FromJSON LogStream where
        parseJSON
          = withObject "LogStream"
              (\ x ->
                 LogStream' <$>
                   (x .:? "creationTime") <*>
                     (x .:? "uploadSequenceToken")
                     <*> (x .:? "arn")
                     <*> (x .:? "firstEventTimestamp")
                     <*> (x .:? "logStreamName")
                     <*> (x .:? "storedBytes")
                     <*> (x .:? "lastIngestionTime")
                     <*> (x .:? "lastEventTimestamp"))

-- | Metric filters can be used to express how Amazon CloudWatch Logs would
-- extract metric observations from ingested log events and transform them
-- to metric data in a CloudWatch metric.
--
-- /See:/ 'metricFilter' smart constructor.
data MetricFilter = MetricFilter'
    { _mfCreationTime          :: !(Maybe Nat)
    , _mfFilterName            :: !(Maybe Text)
    , _mfFilterPattern         :: !(Maybe Text)
    , _mfMetricTransformations :: !(Maybe (List1 MetricTransformation))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MetricFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mfCreationTime'
--
-- * 'mfFilterName'
--
-- * 'mfFilterPattern'
--
-- * 'mfMetricTransformations'
metricFilter
    :: MetricFilter
metricFilter =
    MetricFilter'
    { _mfCreationTime = Nothing
    , _mfFilterName = Nothing
    , _mfFilterPattern = Nothing
    , _mfMetricTransformations = Nothing
    }

-- | Undocumented member.
mfCreationTime :: Lens' MetricFilter (Maybe Natural)
mfCreationTime = lens _mfCreationTime (\ s a -> s{_mfCreationTime = a}) . mapping _Nat;

-- | Undocumented member.
mfFilterName :: Lens' MetricFilter (Maybe Text)
mfFilterName = lens _mfFilterName (\ s a -> s{_mfFilterName = a});

-- | Undocumented member.
mfFilterPattern :: Lens' MetricFilter (Maybe Text)
mfFilterPattern = lens _mfFilterPattern (\ s a -> s{_mfFilterPattern = a});

-- | Undocumented member.
mfMetricTransformations :: Lens' MetricFilter (Maybe (NonEmpty MetricTransformation))
mfMetricTransformations = lens _mfMetricTransformations (\ s a -> s{_mfMetricTransformations = a}) . mapping _List1;

instance FromJSON MetricFilter where
        parseJSON
          = withObject "MetricFilter"
              (\ x ->
                 MetricFilter' <$>
                   (x .:? "creationTime") <*> (x .:? "filterName") <*>
                     (x .:? "filterPattern")
                     <*> (x .:? "metricTransformations"))

-- | /See:/ 'metricFilterMatchRecord' smart constructor.
data MetricFilterMatchRecord = MetricFilterMatchRecord'
    { _mfmrExtractedValues :: !(Maybe (Map Text Text))
    , _mfmrEventMessage    :: !(Maybe Text)
    , _mfmrEventNumber     :: !(Maybe Integer)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MetricFilterMatchRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mfmrExtractedValues'
--
-- * 'mfmrEventMessage'
--
-- * 'mfmrEventNumber'
metricFilterMatchRecord
    :: MetricFilterMatchRecord
metricFilterMatchRecord =
    MetricFilterMatchRecord'
    { _mfmrExtractedValues = Nothing
    , _mfmrEventMessage = Nothing
    , _mfmrEventNumber = Nothing
    }

-- | Undocumented member.
mfmrExtractedValues :: Lens' MetricFilterMatchRecord (HashMap Text Text)
mfmrExtractedValues = lens _mfmrExtractedValues (\ s a -> s{_mfmrExtractedValues = a}) . _Default . _Map;

-- | Undocumented member.
mfmrEventMessage :: Lens' MetricFilterMatchRecord (Maybe Text)
mfmrEventMessage = lens _mfmrEventMessage (\ s a -> s{_mfmrEventMessage = a});

-- | Undocumented member.
mfmrEventNumber :: Lens' MetricFilterMatchRecord (Maybe Integer)
mfmrEventNumber = lens _mfmrEventNumber (\ s a -> s{_mfmrEventNumber = a});

instance FromJSON MetricFilterMatchRecord where
        parseJSON
          = withObject "MetricFilterMatchRecord"
              (\ x ->
                 MetricFilterMatchRecord' <$>
                   (x .:? "extractedValues" .!= mempty) <*>
                     (x .:? "eventMessage")
                     <*> (x .:? "eventNumber"))

-- | /See:/ 'metricTransformation' smart constructor.
data MetricTransformation = MetricTransformation'
    { _mtMetricName      :: !Text
    , _mtMetricNamespace :: !Text
    , _mtMetricValue     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MetricTransformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtMetricName'
--
-- * 'mtMetricNamespace'
--
-- * 'mtMetricValue'
metricTransformation
    :: Text -- ^ 'mtMetricName'
    -> Text -- ^ 'mtMetricNamespace'
    -> Text -- ^ 'mtMetricValue'
    -> MetricTransformation
metricTransformation pMetricName_ pMetricNamespace_ pMetricValue_ =
    MetricTransformation'
    { _mtMetricName = pMetricName_
    , _mtMetricNamespace = pMetricNamespace_
    , _mtMetricValue = pMetricValue_
    }

-- | Undocumented member.
mtMetricName :: Lens' MetricTransformation Text
mtMetricName = lens _mtMetricName (\ s a -> s{_mtMetricName = a});

-- | Undocumented member.
mtMetricNamespace :: Lens' MetricTransformation Text
mtMetricNamespace = lens _mtMetricNamespace (\ s a -> s{_mtMetricNamespace = a});

-- | Undocumented member.
mtMetricValue :: Lens' MetricTransformation Text
mtMetricValue = lens _mtMetricValue (\ s a -> s{_mtMetricValue = a});

instance FromJSON MetricTransformation where
        parseJSON
          = withObject "MetricTransformation"
              (\ x ->
                 MetricTransformation' <$>
                   (x .: "metricName") <*> (x .: "metricNamespace") <*>
                     (x .: "metricValue"))

instance ToJSON MetricTransformation where
        toJSON MetricTransformation'{..}
          = object
              (catMaybes
                 [Just ("metricName" .= _mtMetricName),
                  Just ("metricNamespace" .= _mtMetricNamespace),
                  Just ("metricValue" .= _mtMetricValue)])

-- | /See:/ 'outputLogEvent' smart constructor.
data OutputLogEvent = OutputLogEvent'
    { _oleIngestionTime :: !(Maybe Nat)
    , _oleMessage       :: !(Maybe Text)
    , _oleTimestamp     :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'OutputLogEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oleIngestionTime'
--
-- * 'oleMessage'
--
-- * 'oleTimestamp'
outputLogEvent
    :: OutputLogEvent
outputLogEvent =
    OutputLogEvent'
    { _oleIngestionTime = Nothing
    , _oleMessage = Nothing
    , _oleTimestamp = Nothing
    }

-- | Undocumented member.
oleIngestionTime :: Lens' OutputLogEvent (Maybe Natural)
oleIngestionTime = lens _oleIngestionTime (\ s a -> s{_oleIngestionTime = a}) . mapping _Nat;

-- | Undocumented member.
oleMessage :: Lens' OutputLogEvent (Maybe Text)
oleMessage = lens _oleMessage (\ s a -> s{_oleMessage = a});

-- | Undocumented member.
oleTimestamp :: Lens' OutputLogEvent (Maybe Natural)
oleTimestamp = lens _oleTimestamp (\ s a -> s{_oleTimestamp = a}) . mapping _Nat;

instance FromJSON OutputLogEvent where
        parseJSON
          = withObject "OutputLogEvent"
              (\ x ->
                 OutputLogEvent' <$>
                   (x .:? "ingestionTime") <*> (x .:? "message") <*>
                     (x .:? "timestamp"))

-- | /See:/ 'rejectedLogEventsInfo' smart constructor.
data RejectedLogEventsInfo = RejectedLogEventsInfo'
    { _rleiTooOldLogEventEndIndex   :: !(Maybe Int)
    , _rleiTooNewLogEventStartIndex :: !(Maybe Int)
    , _rleiExpiredLogEventEndIndex  :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RejectedLogEventsInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rleiTooOldLogEventEndIndex'
--
-- * 'rleiTooNewLogEventStartIndex'
--
-- * 'rleiExpiredLogEventEndIndex'
rejectedLogEventsInfo
    :: RejectedLogEventsInfo
rejectedLogEventsInfo =
    RejectedLogEventsInfo'
    { _rleiTooOldLogEventEndIndex = Nothing
    , _rleiTooNewLogEventStartIndex = Nothing
    , _rleiExpiredLogEventEndIndex = Nothing
    }

-- | Undocumented member.
rleiTooOldLogEventEndIndex :: Lens' RejectedLogEventsInfo (Maybe Int)
rleiTooOldLogEventEndIndex = lens _rleiTooOldLogEventEndIndex (\ s a -> s{_rleiTooOldLogEventEndIndex = a});

-- | Undocumented member.
rleiTooNewLogEventStartIndex :: Lens' RejectedLogEventsInfo (Maybe Int)
rleiTooNewLogEventStartIndex = lens _rleiTooNewLogEventStartIndex (\ s a -> s{_rleiTooNewLogEventStartIndex = a});

-- | Undocumented member.
rleiExpiredLogEventEndIndex :: Lens' RejectedLogEventsInfo (Maybe Int)
rleiExpiredLogEventEndIndex = lens _rleiExpiredLogEventEndIndex (\ s a -> s{_rleiExpiredLogEventEndIndex = a});

instance FromJSON RejectedLogEventsInfo where
        parseJSON
          = withObject "RejectedLogEventsInfo"
              (\ x ->
                 RejectedLogEventsInfo' <$>
                   (x .:? "tooOldLogEventEndIndex") <*>
                     (x .:? "tooNewLogEventStartIndex")
                     <*> (x .:? "expiredLogEventEndIndex"))

-- | An object indicating the search status of a log stream in a
-- 'FilterLogEvents' request.
--
-- /See:/ 'searchedLogStream' smart constructor.
data SearchedLogStream = SearchedLogStream'
    { _slsLogStreamName      :: !(Maybe Text)
    , _slsSearchedCompletely :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SearchedLogStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slsLogStreamName'
--
-- * 'slsSearchedCompletely'
searchedLogStream
    :: SearchedLogStream
searchedLogStream =
    SearchedLogStream'
    { _slsLogStreamName = Nothing
    , _slsSearchedCompletely = Nothing
    }

-- | The name of the log stream.
slsLogStreamName :: Lens' SearchedLogStream (Maybe Text)
slsLogStreamName = lens _slsLogStreamName (\ s a -> s{_slsLogStreamName = a});

-- | Indicates whether all the events in this log stream were searched or
-- more data exists to search by paginating further.
slsSearchedCompletely :: Lens' SearchedLogStream (Maybe Bool)
slsSearchedCompletely = lens _slsSearchedCompletely (\ s a -> s{_slsSearchedCompletely = a});

instance FromJSON SearchedLogStream where
        parseJSON
          = withObject "SearchedLogStream"
              (\ x ->
                 SearchedLogStream' <$>
                   (x .:? "logStreamName") <*>
                     (x .:? "searchedCompletely"))

-- | /See:/ 'subscriptionFilter' smart constructor.
data SubscriptionFilter = SubscriptionFilter'
    { _sfCreationTime   :: !(Maybe Nat)
    , _sfFilterName     :: !(Maybe Text)
    , _sfDestinationARN :: !(Maybe Text)
    , _sfLogGroupName   :: !(Maybe Text)
    , _sfFilterPattern  :: !(Maybe Text)
    , _sfRoleARN        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SubscriptionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfCreationTime'
--
-- * 'sfFilterName'
--
-- * 'sfDestinationARN'
--
-- * 'sfLogGroupName'
--
-- * 'sfFilterPattern'
--
-- * 'sfRoleARN'
subscriptionFilter
    :: SubscriptionFilter
subscriptionFilter =
    SubscriptionFilter'
    { _sfCreationTime = Nothing
    , _sfFilterName = Nothing
    , _sfDestinationARN = Nothing
    , _sfLogGroupName = Nothing
    , _sfFilterPattern = Nothing
    , _sfRoleARN = Nothing
    }

-- | Undocumented member.
sfCreationTime :: Lens' SubscriptionFilter (Maybe Natural)
sfCreationTime = lens _sfCreationTime (\ s a -> s{_sfCreationTime = a}) . mapping _Nat;

-- | Undocumented member.
sfFilterName :: Lens' SubscriptionFilter (Maybe Text)
sfFilterName = lens _sfFilterName (\ s a -> s{_sfFilterName = a});

-- | Undocumented member.
sfDestinationARN :: Lens' SubscriptionFilter (Maybe Text)
sfDestinationARN = lens _sfDestinationARN (\ s a -> s{_sfDestinationARN = a});

-- | Undocumented member.
sfLogGroupName :: Lens' SubscriptionFilter (Maybe Text)
sfLogGroupName = lens _sfLogGroupName (\ s a -> s{_sfLogGroupName = a});

-- | Undocumented member.
sfFilterPattern :: Lens' SubscriptionFilter (Maybe Text)
sfFilterPattern = lens _sfFilterPattern (\ s a -> s{_sfFilterPattern = a});

-- | Undocumented member.
sfRoleARN :: Lens' SubscriptionFilter (Maybe Text)
sfRoleARN = lens _sfRoleARN (\ s a -> s{_sfRoleARN = a});

instance FromJSON SubscriptionFilter where
        parseJSON
          = withObject "SubscriptionFilter"
              (\ x ->
                 SubscriptionFilter' <$>
                   (x .:? "creationTime") <*> (x .:? "filterName") <*>
                     (x .:? "destinationArn")
                     <*> (x .:? "logGroupName")
                     <*> (x .:? "filterPattern")
                     <*> (x .:? "roleArn"))
