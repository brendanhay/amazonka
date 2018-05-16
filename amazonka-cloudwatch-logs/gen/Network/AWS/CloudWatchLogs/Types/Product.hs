{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.Product where

import Network.AWS.CloudWatchLogs.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a cross-account destination that receives subscription log events.
--
--
--
-- /See:/ 'destination' smart constructor.
data Destination = Destination'
  { _dTargetARN       :: !(Maybe Text)
  , _dCreationTime    :: !(Maybe Nat)
  , _dArn             :: !(Maybe Text)
  , _dAccessPolicy    :: !(Maybe Text)
  , _dDestinationName :: !(Maybe Text)
  , _dRoleARN         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Destination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dTargetARN' - The Amazon Resource Name (ARN) of the physical target to where the log events are delivered (for example, a Kinesis stream).
--
-- * 'dCreationTime' - The creation time of the destination, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'dArn' - The ARN of this destination.
--
-- * 'dAccessPolicy' - An IAM policy document that governs which AWS accounts can create subscription filters against this destination.
--
-- * 'dDestinationName' - The name of the destination.
--
-- * 'dRoleARN' - A role for impersonation, used when delivering log events to the target.
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


-- | The Amazon Resource Name (ARN) of the physical target to where the log events are delivered (for example, a Kinesis stream).
dTargetARN :: Lens' Destination (Maybe Text)
dTargetARN = lens _dTargetARN (\ s a -> s{_dTargetARN = a})

-- | The creation time of the destination, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
dCreationTime :: Lens' Destination (Maybe Natural)
dCreationTime = lens _dCreationTime (\ s a -> s{_dCreationTime = a}) . mapping _Nat

-- | The ARN of this destination.
dArn :: Lens' Destination (Maybe Text)
dArn = lens _dArn (\ s a -> s{_dArn = a})

-- | An IAM policy document that governs which AWS accounts can create subscription filters against this destination.
dAccessPolicy :: Lens' Destination (Maybe Text)
dAccessPolicy = lens _dAccessPolicy (\ s a -> s{_dAccessPolicy = a})

-- | The name of the destination.
dDestinationName :: Lens' Destination (Maybe Text)
dDestinationName = lens _dDestinationName (\ s a -> s{_dDestinationName = a})

-- | A role for impersonation, used when delivering log events to the target.
dRoleARN :: Lens' Destination (Maybe Text)
dRoleARN = lens _dRoleARN (\ s a -> s{_dRoleARN = a})

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

instance Hashable Destination where

instance NFData Destination where

-- | Represents an export task.
--
--
--
-- /See:/ 'exportTask' smart constructor.
data ExportTask = ExportTask'
  { _etDestinationPrefix :: !(Maybe Text)
  , _etDestination       :: !(Maybe Text)
  , _etStatus            :: !(Maybe ExportTaskStatus)
  , _etTaskName          :: !(Maybe Text)
  , _etTaskId            :: !(Maybe Text)
  , _etTo                :: !(Maybe Nat)
  , _etFrom              :: !(Maybe Nat)
  , _etLogGroupName      :: !(Maybe Text)
  , _etExecutionInfo     :: !(Maybe ExportTaskExecutionInfo)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etDestinationPrefix' - The prefix that was used as the start of Amazon S3 key for every object exported.
--
-- * 'etDestination' - The name of Amazon S3 bucket to which the log data was exported.
--
-- * 'etStatus' - The status of the export task.
--
-- * 'etTaskName' - The name of the export task.
--
-- * 'etTaskId' - The ID of the export task.
--
-- * 'etTo' - The end time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a time stamp later than this time are not exported.
--
-- * 'etFrom' - The start time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a time stamp before this time are not exported.
--
-- * 'etLogGroupName' - The name of the log group from which logs data was exported.
--
-- * 'etExecutionInfo' - Execution info about the export task.
exportTask
    :: ExportTask
exportTask =
  ExportTask'
    { _etDestinationPrefix = Nothing
    , _etDestination = Nothing
    , _etStatus = Nothing
    , _etTaskName = Nothing
    , _etTaskId = Nothing
    , _etTo = Nothing
    , _etFrom = Nothing
    , _etLogGroupName = Nothing
    , _etExecutionInfo = Nothing
    }


-- | The prefix that was used as the start of Amazon S3 key for every object exported.
etDestinationPrefix :: Lens' ExportTask (Maybe Text)
etDestinationPrefix = lens _etDestinationPrefix (\ s a -> s{_etDestinationPrefix = a})

-- | The name of Amazon S3 bucket to which the log data was exported.
etDestination :: Lens' ExportTask (Maybe Text)
etDestination = lens _etDestination (\ s a -> s{_etDestination = a})

-- | The status of the export task.
etStatus :: Lens' ExportTask (Maybe ExportTaskStatus)
etStatus = lens _etStatus (\ s a -> s{_etStatus = a})

-- | The name of the export task.
etTaskName :: Lens' ExportTask (Maybe Text)
etTaskName = lens _etTaskName (\ s a -> s{_etTaskName = a})

-- | The ID of the export task.
etTaskId :: Lens' ExportTask (Maybe Text)
etTaskId = lens _etTaskId (\ s a -> s{_etTaskId = a})

-- | The end time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a time stamp later than this time are not exported.
etTo :: Lens' ExportTask (Maybe Natural)
etTo = lens _etTo (\ s a -> s{_etTo = a}) . mapping _Nat

-- | The start time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a time stamp before this time are not exported.
etFrom :: Lens' ExportTask (Maybe Natural)
etFrom = lens _etFrom (\ s a -> s{_etFrom = a}) . mapping _Nat

-- | The name of the log group from which logs data was exported.
etLogGroupName :: Lens' ExportTask (Maybe Text)
etLogGroupName = lens _etLogGroupName (\ s a -> s{_etLogGroupName = a})

-- | Execution info about the export task.
etExecutionInfo :: Lens' ExportTask (Maybe ExportTaskExecutionInfo)
etExecutionInfo = lens _etExecutionInfo (\ s a -> s{_etExecutionInfo = a})

instance FromJSON ExportTask where
        parseJSON
          = withObject "ExportTask"
              (\ x ->
                 ExportTask' <$>
                   (x .:? "destinationPrefix") <*> (x .:? "destination")
                     <*> (x .:? "status")
                     <*> (x .:? "taskName")
                     <*> (x .:? "taskId")
                     <*> (x .:? "to")
                     <*> (x .:? "from")
                     <*> (x .:? "logGroupName")
                     <*> (x .:? "executionInfo"))

instance Hashable ExportTask where

instance NFData ExportTask where

-- | Represents the status of an export task.
--
--
--
-- /See:/ 'exportTaskExecutionInfo' smart constructor.
data ExportTaskExecutionInfo = ExportTaskExecutionInfo'
  { _eteiCreationTime   :: !(Maybe Nat)
  , _eteiCompletionTime :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportTaskExecutionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eteiCreationTime' - The creation time of the export task, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'eteiCompletionTime' - The completion time of the export task, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
exportTaskExecutionInfo
    :: ExportTaskExecutionInfo
exportTaskExecutionInfo =
  ExportTaskExecutionInfo'
    {_eteiCreationTime = Nothing, _eteiCompletionTime = Nothing}


-- | The creation time of the export task, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
eteiCreationTime :: Lens' ExportTaskExecutionInfo (Maybe Natural)
eteiCreationTime = lens _eteiCreationTime (\ s a -> s{_eteiCreationTime = a}) . mapping _Nat

-- | The completion time of the export task, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
eteiCompletionTime :: Lens' ExportTaskExecutionInfo (Maybe Natural)
eteiCompletionTime = lens _eteiCompletionTime (\ s a -> s{_eteiCompletionTime = a}) . mapping _Nat

instance FromJSON ExportTaskExecutionInfo where
        parseJSON
          = withObject "ExportTaskExecutionInfo"
              (\ x ->
                 ExportTaskExecutionInfo' <$>
                   (x .:? "creationTime") <*> (x .:? "completionTime"))

instance Hashable ExportTaskExecutionInfo where

instance NFData ExportTaskExecutionInfo where

-- | Represents the status of an export task.
--
--
--
-- /See:/ 'exportTaskStatus' smart constructor.
data ExportTaskStatus = ExportTaskStatus'
  { _etsCode    :: !(Maybe ExportTaskStatusCode)
  , _etsMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportTaskStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etsCode' - The status code of the export task.
--
-- * 'etsMessage' - The status message related to the status code.
exportTaskStatus
    :: ExportTaskStatus
exportTaskStatus = ExportTaskStatus' {_etsCode = Nothing, _etsMessage = Nothing}


-- | The status code of the export task.
etsCode :: Lens' ExportTaskStatus (Maybe ExportTaskStatusCode)
etsCode = lens _etsCode (\ s a -> s{_etsCode = a})

-- | The status message related to the status code.
etsMessage :: Lens' ExportTaskStatus (Maybe Text)
etsMessage = lens _etsMessage (\ s a -> s{_etsMessage = a})

instance FromJSON ExportTaskStatus where
        parseJSON
          = withObject "ExportTaskStatus"
              (\ x ->
                 ExportTaskStatus' <$>
                   (x .:? "code") <*> (x .:? "message"))

instance Hashable ExportTaskStatus where

instance NFData ExportTaskStatus where

-- | Represents a matched event.
--
--
--
-- /See:/ 'filteredLogEvent' smart constructor.
data FilteredLogEvent = FilteredLogEvent'
  { _fleIngestionTime :: !(Maybe Nat)
  , _fleLogStreamName :: !(Maybe Text)
  , _fleMessage       :: !(Maybe Text)
  , _fleTimestamp     :: !(Maybe Nat)
  , _fleEventId       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FilteredLogEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fleIngestionTime' - The time the event was ingested, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'fleLogStreamName' - The name of the log stream this event belongs to.
--
-- * 'fleMessage' - The data contained in the log event.
--
-- * 'fleTimestamp' - The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'fleEventId' - The ID of the event.
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


-- | The time the event was ingested, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
fleIngestionTime :: Lens' FilteredLogEvent (Maybe Natural)
fleIngestionTime = lens _fleIngestionTime (\ s a -> s{_fleIngestionTime = a}) . mapping _Nat

-- | The name of the log stream this event belongs to.
fleLogStreamName :: Lens' FilteredLogEvent (Maybe Text)
fleLogStreamName = lens _fleLogStreamName (\ s a -> s{_fleLogStreamName = a})

-- | The data contained in the log event.
fleMessage :: Lens' FilteredLogEvent (Maybe Text)
fleMessage = lens _fleMessage (\ s a -> s{_fleMessage = a})

-- | The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
fleTimestamp :: Lens' FilteredLogEvent (Maybe Natural)
fleTimestamp = lens _fleTimestamp (\ s a -> s{_fleTimestamp = a}) . mapping _Nat

-- | The ID of the event.
fleEventId :: Lens' FilteredLogEvent (Maybe Text)
fleEventId = lens _fleEventId (\ s a -> s{_fleEventId = a})

instance FromJSON FilteredLogEvent where
        parseJSON
          = withObject "FilteredLogEvent"
              (\ x ->
                 FilteredLogEvent' <$>
                   (x .:? "ingestionTime") <*> (x .:? "logStreamName")
                     <*> (x .:? "message")
                     <*> (x .:? "timestamp")
                     <*> (x .:? "eventId"))

instance Hashable FilteredLogEvent where

instance NFData FilteredLogEvent where

-- | Represents a log event, which is a record of activity that was recorded by the application or resource being monitored.
--
--
--
-- /See:/ 'inputLogEvent' smart constructor.
data InputLogEvent = InputLogEvent'
  { _ileTimestamp :: !Nat
  , _ileMessage   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputLogEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ileTimestamp' - The time the event occurred, expressed as the number of milliseconds fter Jan 1, 1970 00:00:00 UTC.
--
-- * 'ileMessage' - The raw event message.
inputLogEvent
    :: Natural -- ^ 'ileTimestamp'
    -> Text -- ^ 'ileMessage'
    -> InputLogEvent
inputLogEvent pTimestamp_ pMessage_ =
  InputLogEvent' {_ileTimestamp = _Nat # pTimestamp_, _ileMessage = pMessage_}


-- | The time the event occurred, expressed as the number of milliseconds fter Jan 1, 1970 00:00:00 UTC.
ileTimestamp :: Lens' InputLogEvent Natural
ileTimestamp = lens _ileTimestamp (\ s a -> s{_ileTimestamp = a}) . _Nat

-- | The raw event message.
ileMessage :: Lens' InputLogEvent Text
ileMessage = lens _ileMessage (\ s a -> s{_ileMessage = a})

instance Hashable InputLogEvent where

instance NFData InputLogEvent where

instance ToJSON InputLogEvent where
        toJSON InputLogEvent'{..}
          = object
              (catMaybes
                 [Just ("timestamp" .= _ileTimestamp),
                  Just ("message" .= _ileMessage)])

-- | Represents a log group.
--
--
--
-- /See:/ 'logGroup' smart constructor.
data LogGroup = LogGroup'
  { _lgCreationTime      :: !(Maybe Nat)
  , _lgMetricFilterCount :: !(Maybe Int)
  , _lgArn               :: !(Maybe Text)
  , _lgLogGroupName      :: !(Maybe Text)
  , _lgRetentionInDays   :: !(Maybe Int)
  , _lgKmsKeyId          :: !(Maybe Text)
  , _lgStoredBytes       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LogGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgCreationTime' - The creation time of the log group, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'lgMetricFilterCount' - The number of metric filters.
--
-- * 'lgArn' - The Amazon Resource Name (ARN) of the log group.
--
-- * 'lgLogGroupName' - The name of the log group.
--
-- * 'lgRetentionInDays' - Undocumented member.
--
-- * 'lgKmsKeyId' - The Amazon Resource Name (ARN) of the CMK to use when encrypting log data.
--
-- * 'lgStoredBytes' - The number of bytes stored.
logGroup
    :: LogGroup
logGroup =
  LogGroup'
    { _lgCreationTime = Nothing
    , _lgMetricFilterCount = Nothing
    , _lgArn = Nothing
    , _lgLogGroupName = Nothing
    , _lgRetentionInDays = Nothing
    , _lgKmsKeyId = Nothing
    , _lgStoredBytes = Nothing
    }


-- | The creation time of the log group, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
lgCreationTime :: Lens' LogGroup (Maybe Natural)
lgCreationTime = lens _lgCreationTime (\ s a -> s{_lgCreationTime = a}) . mapping _Nat

-- | The number of metric filters.
lgMetricFilterCount :: Lens' LogGroup (Maybe Int)
lgMetricFilterCount = lens _lgMetricFilterCount (\ s a -> s{_lgMetricFilterCount = a})

-- | The Amazon Resource Name (ARN) of the log group.
lgArn :: Lens' LogGroup (Maybe Text)
lgArn = lens _lgArn (\ s a -> s{_lgArn = a})

-- | The name of the log group.
lgLogGroupName :: Lens' LogGroup (Maybe Text)
lgLogGroupName = lens _lgLogGroupName (\ s a -> s{_lgLogGroupName = a})

-- | Undocumented member.
lgRetentionInDays :: Lens' LogGroup (Maybe Int)
lgRetentionInDays = lens _lgRetentionInDays (\ s a -> s{_lgRetentionInDays = a})

-- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log data.
lgKmsKeyId :: Lens' LogGroup (Maybe Text)
lgKmsKeyId = lens _lgKmsKeyId (\ s a -> s{_lgKmsKeyId = a})

-- | The number of bytes stored.
lgStoredBytes :: Lens' LogGroup (Maybe Natural)
lgStoredBytes = lens _lgStoredBytes (\ s a -> s{_lgStoredBytes = a}) . mapping _Nat

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
                     <*> (x .:? "kmsKeyId")
                     <*> (x .:? "storedBytes"))

instance Hashable LogGroup where

instance NFData LogGroup where

-- | Represents a log stream, which is a sequence of log events from a single emitter of logs.
--
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LogStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsCreationTime' - The creation time of the stream, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'lsUploadSequenceToken' - The sequence token.
--
-- * 'lsArn' - The Amazon Resource Name (ARN) of the log stream.
--
-- * 'lsFirstEventTimestamp' - The time of the first event, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'lsLogStreamName' - The name of the log stream.
--
-- * 'lsStoredBytes' - The number of bytes stored.
--
-- * 'lsLastIngestionTime' - The ingestion time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'lsLastEventTimestamp' - the time of the most recent log event in the log stream in CloudWatch Logs. This number is expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. lastEventTime updates on an eventual consistency basis. It typically updates in less than an hour from ingestion, but may take longer in some rare situations.
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


-- | The creation time of the stream, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
lsCreationTime :: Lens' LogStream (Maybe Natural)
lsCreationTime = lens _lsCreationTime (\ s a -> s{_lsCreationTime = a}) . mapping _Nat

-- | The sequence token.
lsUploadSequenceToken :: Lens' LogStream (Maybe Text)
lsUploadSequenceToken = lens _lsUploadSequenceToken (\ s a -> s{_lsUploadSequenceToken = a})

-- | The Amazon Resource Name (ARN) of the log stream.
lsArn :: Lens' LogStream (Maybe Text)
lsArn = lens _lsArn (\ s a -> s{_lsArn = a})

-- | The time of the first event, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
lsFirstEventTimestamp :: Lens' LogStream (Maybe Natural)
lsFirstEventTimestamp = lens _lsFirstEventTimestamp (\ s a -> s{_lsFirstEventTimestamp = a}) . mapping _Nat

-- | The name of the log stream.
lsLogStreamName :: Lens' LogStream (Maybe Text)
lsLogStreamName = lens _lsLogStreamName (\ s a -> s{_lsLogStreamName = a})

-- | The number of bytes stored.
lsStoredBytes :: Lens' LogStream (Maybe Natural)
lsStoredBytes = lens _lsStoredBytes (\ s a -> s{_lsStoredBytes = a}) . mapping _Nat

-- | The ingestion time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
lsLastIngestionTime :: Lens' LogStream (Maybe Natural)
lsLastIngestionTime = lens _lsLastIngestionTime (\ s a -> s{_lsLastIngestionTime = a}) . mapping _Nat

-- | the time of the most recent log event in the log stream in CloudWatch Logs. This number is expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. lastEventTime updates on an eventual consistency basis. It typically updates in less than an hour from ingestion, but may take longer in some rare situations.
lsLastEventTimestamp :: Lens' LogStream (Maybe Natural)
lsLastEventTimestamp = lens _lsLastEventTimestamp (\ s a -> s{_lsLastEventTimestamp = a}) . mapping _Nat

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

instance Hashable LogStream where

instance NFData LogStream where

-- | Metric filters express how CloudWatch Logs would extract metric observations from ingested log events and transform them into metric data in a CloudWatch metric.
--
--
--
-- /See:/ 'metricFilter' smart constructor.
data MetricFilter = MetricFilter'
  { _mfCreationTime          :: !(Maybe Nat)
  , _mfFilterName            :: !(Maybe Text)
  , _mfLogGroupName          :: !(Maybe Text)
  , _mfFilterPattern         :: !(Maybe Text)
  , _mfMetricTransformations :: !(Maybe (List1 MetricTransformation))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mfCreationTime' - The creation time of the metric filter, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'mfFilterName' - The name of the metric filter.
--
-- * 'mfLogGroupName' - The name of the log group.
--
-- * 'mfFilterPattern' - Undocumented member.
--
-- * 'mfMetricTransformations' - The metric transformations.
metricFilter
    :: MetricFilter
metricFilter =
  MetricFilter'
    { _mfCreationTime = Nothing
    , _mfFilterName = Nothing
    , _mfLogGroupName = Nothing
    , _mfFilterPattern = Nothing
    , _mfMetricTransformations = Nothing
    }


-- | The creation time of the metric filter, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
mfCreationTime :: Lens' MetricFilter (Maybe Natural)
mfCreationTime = lens _mfCreationTime (\ s a -> s{_mfCreationTime = a}) . mapping _Nat

-- | The name of the metric filter.
mfFilterName :: Lens' MetricFilter (Maybe Text)
mfFilterName = lens _mfFilterName (\ s a -> s{_mfFilterName = a})

-- | The name of the log group.
mfLogGroupName :: Lens' MetricFilter (Maybe Text)
mfLogGroupName = lens _mfLogGroupName (\ s a -> s{_mfLogGroupName = a})

-- | Undocumented member.
mfFilterPattern :: Lens' MetricFilter (Maybe Text)
mfFilterPattern = lens _mfFilterPattern (\ s a -> s{_mfFilterPattern = a})

-- | The metric transformations.
mfMetricTransformations :: Lens' MetricFilter (Maybe (NonEmpty MetricTransformation))
mfMetricTransformations = lens _mfMetricTransformations (\ s a -> s{_mfMetricTransformations = a}) . mapping _List1

instance FromJSON MetricFilter where
        parseJSON
          = withObject "MetricFilter"
              (\ x ->
                 MetricFilter' <$>
                   (x .:? "creationTime") <*> (x .:? "filterName") <*>
                     (x .:? "logGroupName")
                     <*> (x .:? "filterPattern")
                     <*> (x .:? "metricTransformations"))

instance Hashable MetricFilter where

instance NFData MetricFilter where

-- | Represents a matched event.
--
--
--
-- /See:/ 'metricFilterMatchRecord' smart constructor.
data MetricFilterMatchRecord = MetricFilterMatchRecord'
  { _mfmrExtractedValues :: !(Maybe (Map Text Text))
  , _mfmrEventNumber     :: !(Maybe Integer)
  , _mfmrEventMessage    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricFilterMatchRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mfmrExtractedValues' - The values extracted from the event data by the filter.
--
-- * 'mfmrEventNumber' - The event number.
--
-- * 'mfmrEventMessage' - The raw event data.
metricFilterMatchRecord
    :: MetricFilterMatchRecord
metricFilterMatchRecord =
  MetricFilterMatchRecord'
    { _mfmrExtractedValues = Nothing
    , _mfmrEventNumber = Nothing
    , _mfmrEventMessage = Nothing
    }


-- | The values extracted from the event data by the filter.
mfmrExtractedValues :: Lens' MetricFilterMatchRecord (HashMap Text Text)
mfmrExtractedValues = lens _mfmrExtractedValues (\ s a -> s{_mfmrExtractedValues = a}) . _Default . _Map

-- | The event number.
mfmrEventNumber :: Lens' MetricFilterMatchRecord (Maybe Integer)
mfmrEventNumber = lens _mfmrEventNumber (\ s a -> s{_mfmrEventNumber = a})

-- | The raw event data.
mfmrEventMessage :: Lens' MetricFilterMatchRecord (Maybe Text)
mfmrEventMessage = lens _mfmrEventMessage (\ s a -> s{_mfmrEventMessage = a})

instance FromJSON MetricFilterMatchRecord where
        parseJSON
          = withObject "MetricFilterMatchRecord"
              (\ x ->
                 MetricFilterMatchRecord' <$>
                   (x .:? "extractedValues" .!= mempty) <*>
                     (x .:? "eventNumber")
                     <*> (x .:? "eventMessage"))

instance Hashable MetricFilterMatchRecord where

instance NFData MetricFilterMatchRecord where

-- | Indicates how to transform ingested log events in to metric data in a CloudWatch metric.
--
--
--
-- /See:/ 'metricTransformation' smart constructor.
data MetricTransformation = MetricTransformation'
  { _mtDefaultValue    :: !(Maybe Double)
  , _mtMetricName      :: !Text
  , _mtMetricNamespace :: !Text
  , _mtMetricValue     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricTransformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtDefaultValue' - (Optional) The value to emit when a filter pattern does not match a log event. This value can be null.
--
-- * 'mtMetricName' - The name of the CloudWatch metric.
--
-- * 'mtMetricNamespace' - The namespace of the CloudWatch metric.
--
-- * 'mtMetricValue' - The value to publish to the CloudWatch metric when a filter pattern matches a log event.
metricTransformation
    :: Text -- ^ 'mtMetricName'
    -> Text -- ^ 'mtMetricNamespace'
    -> Text -- ^ 'mtMetricValue'
    -> MetricTransformation
metricTransformation pMetricName_ pMetricNamespace_ pMetricValue_ =
  MetricTransformation'
    { _mtDefaultValue = Nothing
    , _mtMetricName = pMetricName_
    , _mtMetricNamespace = pMetricNamespace_
    , _mtMetricValue = pMetricValue_
    }


-- | (Optional) The value to emit when a filter pattern does not match a log event. This value can be null.
mtDefaultValue :: Lens' MetricTransformation (Maybe Double)
mtDefaultValue = lens _mtDefaultValue (\ s a -> s{_mtDefaultValue = a})

-- | The name of the CloudWatch metric.
mtMetricName :: Lens' MetricTransformation Text
mtMetricName = lens _mtMetricName (\ s a -> s{_mtMetricName = a})

-- | The namespace of the CloudWatch metric.
mtMetricNamespace :: Lens' MetricTransformation Text
mtMetricNamespace = lens _mtMetricNamespace (\ s a -> s{_mtMetricNamespace = a})

-- | The value to publish to the CloudWatch metric when a filter pattern matches a log event.
mtMetricValue :: Lens' MetricTransformation Text
mtMetricValue = lens _mtMetricValue (\ s a -> s{_mtMetricValue = a})

instance FromJSON MetricTransformation where
        parseJSON
          = withObject "MetricTransformation"
              (\ x ->
                 MetricTransformation' <$>
                   (x .:? "defaultValue") <*> (x .: "metricName") <*>
                     (x .: "metricNamespace")
                     <*> (x .: "metricValue"))

instance Hashable MetricTransformation where

instance NFData MetricTransformation where

instance ToJSON MetricTransformation where
        toJSON MetricTransformation'{..}
          = object
              (catMaybes
                 [("defaultValue" .=) <$> _mtDefaultValue,
                  Just ("metricName" .= _mtMetricName),
                  Just ("metricNamespace" .= _mtMetricNamespace),
                  Just ("metricValue" .= _mtMetricValue)])

-- | Represents a log event.
--
--
--
-- /See:/ 'outputLogEvent' smart constructor.
data OutputLogEvent = OutputLogEvent'
  { _oleIngestionTime :: !(Maybe Nat)
  , _oleMessage       :: !(Maybe Text)
  , _oleTimestamp     :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputLogEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oleIngestionTime' - The time the event was ingested, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'oleMessage' - The data contained in the log event.
--
-- * 'oleTimestamp' - The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
outputLogEvent
    :: OutputLogEvent
outputLogEvent =
  OutputLogEvent'
    { _oleIngestionTime = Nothing
    , _oleMessage = Nothing
    , _oleTimestamp = Nothing
    }


-- | The time the event was ingested, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
oleIngestionTime :: Lens' OutputLogEvent (Maybe Natural)
oleIngestionTime = lens _oleIngestionTime (\ s a -> s{_oleIngestionTime = a}) . mapping _Nat

-- | The data contained in the log event.
oleMessage :: Lens' OutputLogEvent (Maybe Text)
oleMessage = lens _oleMessage (\ s a -> s{_oleMessage = a})

-- | The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
oleTimestamp :: Lens' OutputLogEvent (Maybe Natural)
oleTimestamp = lens _oleTimestamp (\ s a -> s{_oleTimestamp = a}) . mapping _Nat

instance FromJSON OutputLogEvent where
        parseJSON
          = withObject "OutputLogEvent"
              (\ x ->
                 OutputLogEvent' <$>
                   (x .:? "ingestionTime") <*> (x .:? "message") <*>
                     (x .:? "timestamp"))

instance Hashable OutputLogEvent where

instance NFData OutputLogEvent where

-- | Represents the rejected events.
--
--
--
-- /See:/ 'rejectedLogEventsInfo' smart constructor.
data RejectedLogEventsInfo = RejectedLogEventsInfo'
  { _rleiTooOldLogEventEndIndex   :: !(Maybe Int)
  , _rleiTooNewLogEventStartIndex :: !(Maybe Int)
  , _rleiExpiredLogEventEndIndex  :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RejectedLogEventsInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rleiTooOldLogEventEndIndex' - The log events that are too old.
--
-- * 'rleiTooNewLogEventStartIndex' - The log events that are too new.
--
-- * 'rleiExpiredLogEventEndIndex' - The expired log events.
rejectedLogEventsInfo
    :: RejectedLogEventsInfo
rejectedLogEventsInfo =
  RejectedLogEventsInfo'
    { _rleiTooOldLogEventEndIndex = Nothing
    , _rleiTooNewLogEventStartIndex = Nothing
    , _rleiExpiredLogEventEndIndex = Nothing
    }


-- | The log events that are too old.
rleiTooOldLogEventEndIndex :: Lens' RejectedLogEventsInfo (Maybe Int)
rleiTooOldLogEventEndIndex = lens _rleiTooOldLogEventEndIndex (\ s a -> s{_rleiTooOldLogEventEndIndex = a})

-- | The log events that are too new.
rleiTooNewLogEventStartIndex :: Lens' RejectedLogEventsInfo (Maybe Int)
rleiTooNewLogEventStartIndex = lens _rleiTooNewLogEventStartIndex (\ s a -> s{_rleiTooNewLogEventStartIndex = a})

-- | The expired log events.
rleiExpiredLogEventEndIndex :: Lens' RejectedLogEventsInfo (Maybe Int)
rleiExpiredLogEventEndIndex = lens _rleiExpiredLogEventEndIndex (\ s a -> s{_rleiExpiredLogEventEndIndex = a})

instance FromJSON RejectedLogEventsInfo where
        parseJSON
          = withObject "RejectedLogEventsInfo"
              (\ x ->
                 RejectedLogEventsInfo' <$>
                   (x .:? "tooOldLogEventEndIndex") <*>
                     (x .:? "tooNewLogEventStartIndex")
                     <*> (x .:? "expiredLogEventEndIndex"))

instance Hashable RejectedLogEventsInfo where

instance NFData RejectedLogEventsInfo where

-- | A policy enabling one or more entities to put logs to a log group in this account.
--
--
--
-- /See:/ 'resourcePolicy' smart constructor.
data ResourcePolicy = ResourcePolicy'
  { _rpPolicyName      :: !(Maybe Text)
  , _rpPolicyDocument  :: !(Maybe Text)
  , _rpLastUpdatedTime :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourcePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpPolicyName' - The name of the resource policy.
--
-- * 'rpPolicyDocument' - The details of the policy.
--
-- * 'rpLastUpdatedTime' - Time stamp showing when this policy was last updated, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
resourcePolicy
    :: ResourcePolicy
resourcePolicy =
  ResourcePolicy'
    { _rpPolicyName = Nothing
    , _rpPolicyDocument = Nothing
    , _rpLastUpdatedTime = Nothing
    }


-- | The name of the resource policy.
rpPolicyName :: Lens' ResourcePolicy (Maybe Text)
rpPolicyName = lens _rpPolicyName (\ s a -> s{_rpPolicyName = a})

-- | The details of the policy.
rpPolicyDocument :: Lens' ResourcePolicy (Maybe Text)
rpPolicyDocument = lens _rpPolicyDocument (\ s a -> s{_rpPolicyDocument = a})

-- | Time stamp showing when this policy was last updated, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
rpLastUpdatedTime :: Lens' ResourcePolicy (Maybe Natural)
rpLastUpdatedTime = lens _rpLastUpdatedTime (\ s a -> s{_rpLastUpdatedTime = a}) . mapping _Nat

instance FromJSON ResourcePolicy where
        parseJSON
          = withObject "ResourcePolicy"
              (\ x ->
                 ResourcePolicy' <$>
                   (x .:? "policyName") <*> (x .:? "policyDocument") <*>
                     (x .:? "lastUpdatedTime"))

instance Hashable ResourcePolicy where

instance NFData ResourcePolicy where

-- | Represents the search status of a log stream.
--
--
--
-- /See:/ 'searchedLogStream' smart constructor.
data SearchedLogStream = SearchedLogStream'
  { _slsLogStreamName      :: !(Maybe Text)
  , _slsSearchedCompletely :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchedLogStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slsLogStreamName' - The name of the log stream.
--
-- * 'slsSearchedCompletely' - Indicates whether all the events in this log stream were searched.
searchedLogStream
    :: SearchedLogStream
searchedLogStream =
  SearchedLogStream'
    {_slsLogStreamName = Nothing, _slsSearchedCompletely = Nothing}


-- | The name of the log stream.
slsLogStreamName :: Lens' SearchedLogStream (Maybe Text)
slsLogStreamName = lens _slsLogStreamName (\ s a -> s{_slsLogStreamName = a})

-- | Indicates whether all the events in this log stream were searched.
slsSearchedCompletely :: Lens' SearchedLogStream (Maybe Bool)
slsSearchedCompletely = lens _slsSearchedCompletely (\ s a -> s{_slsSearchedCompletely = a})

instance FromJSON SearchedLogStream where
        parseJSON
          = withObject "SearchedLogStream"
              (\ x ->
                 SearchedLogStream' <$>
                   (x .:? "logStreamName") <*>
                     (x .:? "searchedCompletely"))

instance Hashable SearchedLogStream where

instance NFData SearchedLogStream where

-- | Represents a subscription filter.
--
--
--
-- /See:/ 'subscriptionFilter' smart constructor.
data SubscriptionFilter = SubscriptionFilter'
  { _sfCreationTime   :: !(Maybe Nat)
  , _sfFilterName     :: !(Maybe Text)
  , _sfDistribution   :: !(Maybe Distribution)
  , _sfDestinationARN :: !(Maybe Text)
  , _sfLogGroupName   :: !(Maybe Text)
  , _sfFilterPattern  :: !(Maybe Text)
  , _sfRoleARN        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubscriptionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfCreationTime' - The creation time of the subscription filter, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'sfFilterName' - The name of the subscription filter.
--
-- * 'sfDistribution' - Undocumented member.
--
-- * 'sfDestinationARN' - The Amazon Resource Name (ARN) of the destination.
--
-- * 'sfLogGroupName' - The name of the log group.
--
-- * 'sfFilterPattern' - Undocumented member.
--
-- * 'sfRoleARN' -
subscriptionFilter
    :: SubscriptionFilter
subscriptionFilter =
  SubscriptionFilter'
    { _sfCreationTime = Nothing
    , _sfFilterName = Nothing
    , _sfDistribution = Nothing
    , _sfDestinationARN = Nothing
    , _sfLogGroupName = Nothing
    , _sfFilterPattern = Nothing
    , _sfRoleARN = Nothing
    }


-- | The creation time of the subscription filter, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
sfCreationTime :: Lens' SubscriptionFilter (Maybe Natural)
sfCreationTime = lens _sfCreationTime (\ s a -> s{_sfCreationTime = a}) . mapping _Nat

-- | The name of the subscription filter.
sfFilterName :: Lens' SubscriptionFilter (Maybe Text)
sfFilterName = lens _sfFilterName (\ s a -> s{_sfFilterName = a})

-- | Undocumented member.
sfDistribution :: Lens' SubscriptionFilter (Maybe Distribution)
sfDistribution = lens _sfDistribution (\ s a -> s{_sfDistribution = a})

-- | The Amazon Resource Name (ARN) of the destination.
sfDestinationARN :: Lens' SubscriptionFilter (Maybe Text)
sfDestinationARN = lens _sfDestinationARN (\ s a -> s{_sfDestinationARN = a})

-- | The name of the log group.
sfLogGroupName :: Lens' SubscriptionFilter (Maybe Text)
sfLogGroupName = lens _sfLogGroupName (\ s a -> s{_sfLogGroupName = a})

-- | Undocumented member.
sfFilterPattern :: Lens' SubscriptionFilter (Maybe Text)
sfFilterPattern = lens _sfFilterPattern (\ s a -> s{_sfFilterPattern = a})

-- |
sfRoleARN :: Lens' SubscriptionFilter (Maybe Text)
sfRoleARN = lens _sfRoleARN (\ s a -> s{_sfRoleARN = a})

instance FromJSON SubscriptionFilter where
        parseJSON
          = withObject "SubscriptionFilter"
              (\ x ->
                 SubscriptionFilter' <$>
                   (x .:? "creationTime") <*> (x .:? "filterName") <*>
                     (x .:? "distribution")
                     <*> (x .:? "destinationArn")
                     <*> (x .:? "logGroupName")
                     <*> (x .:? "filterPattern")
                     <*> (x .:? "roleArn"))

instance Hashable SubscriptionFilter where

instance NFData SubscriptionFilter where
