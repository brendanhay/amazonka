{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.Product where

import           Network.AWS.CloudWatchLogs.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | A cross account destination that is the recipient of subscription log events.
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Destination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dTargetARN' - ARN of the physical target where the log events will be delivered (eg. ARN of a Kinesis stream).
--
-- * 'dCreationTime' - A point in time expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC specifying when this destination was created.
--
-- * 'dArn' - ARN of this destination.
--
-- * 'dAccessPolicy' - An IAM policy document that governs which AWS accounts can create subscription filters against this destination.
--
-- * 'dDestinationName' - Name of the destination.
--
-- * 'dRoleARN' - A role for impersonation for delivering log events to the target.
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

-- | ARN of the physical target where the log events will be delivered (eg. ARN of a Kinesis stream).
dTargetARN :: Lens' Destination (Maybe Text)
dTargetARN = lens _dTargetARN (\ s a -> s{_dTargetARN = a});

-- | A point in time expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC specifying when this destination was created.
dCreationTime :: Lens' Destination (Maybe Natural)
dCreationTime = lens _dCreationTime (\ s a -> s{_dCreationTime = a}) . mapping _Nat;

-- | ARN of this destination.
dArn :: Lens' Destination (Maybe Text)
dArn = lens _dArn (\ s a -> s{_dArn = a});

-- | An IAM policy document that governs which AWS accounts can create subscription filters against this destination.
dAccessPolicy :: Lens' Destination (Maybe Text)
dAccessPolicy = lens _dAccessPolicy (\ s a -> s{_dAccessPolicy = a});

-- | Name of the destination.
dDestinationName :: Lens' Destination (Maybe Text)
dDestinationName = lens _dDestinationName (\ s a -> s{_dDestinationName = a});

-- | A role for impersonation for delivering log events to the target.
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

instance Hashable Destination

instance NFData Destination

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ExportTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etDestinationPrefix' - Prefix that was used as the start of Amazon S3 key for every object exported.
--
-- * 'etDestination' - Name of Amazon S3 bucket to which the log data was exported.
--
-- * 'etStatus' - Status of the export task.
--
-- * 'etTaskName' - The name of the export task.
--
-- * 'etTaskId' - Id of the export task.
--
-- * 'etTo' - A point in time expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time are not exported.
--
-- * 'etFrom' - A point in time expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC. Events with a timestamp prior to this time are not exported.
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

-- | Prefix that was used as the start of Amazon S3 key for every object exported.
etDestinationPrefix :: Lens' ExportTask (Maybe Text)
etDestinationPrefix = lens _etDestinationPrefix (\ s a -> s{_etDestinationPrefix = a});

-- | Name of Amazon S3 bucket to which the log data was exported.
etDestination :: Lens' ExportTask (Maybe Text)
etDestination = lens _etDestination (\ s a -> s{_etDestination = a});

-- | Status of the export task.
etStatus :: Lens' ExportTask (Maybe ExportTaskStatus)
etStatus = lens _etStatus (\ s a -> s{_etStatus = a});

-- | The name of the export task.
etTaskName :: Lens' ExportTask (Maybe Text)
etTaskName = lens _etTaskName (\ s a -> s{_etTaskName = a});

-- | Id of the export task.
etTaskId :: Lens' ExportTask (Maybe Text)
etTaskId = lens _etTaskId (\ s a -> s{_etTaskId = a});

-- | A point in time expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time are not exported.
etTo :: Lens' ExportTask (Maybe Natural)
etTo = lens _etTo (\ s a -> s{_etTo = a}) . mapping _Nat;

-- | A point in time expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC. Events with a timestamp prior to this time are not exported.
etFrom :: Lens' ExportTask (Maybe Natural)
etFrom = lens _etFrom (\ s a -> s{_etFrom = a}) . mapping _Nat;

-- | The name of the log group from which logs data was exported.
etLogGroupName :: Lens' ExportTask (Maybe Text)
etLogGroupName = lens _etLogGroupName (\ s a -> s{_etLogGroupName = a});

-- | Execution info about the export task.
etExecutionInfo :: Lens' ExportTask (Maybe ExportTaskExecutionInfo)
etExecutionInfo = lens _etExecutionInfo (\ s a -> s{_etExecutionInfo = a});

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

instance Hashable ExportTask

instance NFData ExportTask

-- | Represents the status of an export task.
--
--
--
-- /See:/ 'exportTaskExecutionInfo' smart constructor.
data ExportTaskExecutionInfo = ExportTaskExecutionInfo'
    { _eteiCreationTime   :: !(Maybe Nat)
    , _eteiCompletionTime :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ExportTaskExecutionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eteiCreationTime' - A point in time when the export task got created.
--
-- * 'eteiCompletionTime' - A point in time when the export task got completed.
exportTaskExecutionInfo
    :: ExportTaskExecutionInfo
exportTaskExecutionInfo =
    ExportTaskExecutionInfo'
    { _eteiCreationTime = Nothing
    , _eteiCompletionTime = Nothing
    }

-- | A point in time when the export task got created.
eteiCreationTime :: Lens' ExportTaskExecutionInfo (Maybe Natural)
eteiCreationTime = lens _eteiCreationTime (\ s a -> s{_eteiCreationTime = a}) . mapping _Nat;

-- | A point in time when the export task got completed.
eteiCompletionTime :: Lens' ExportTaskExecutionInfo (Maybe Natural)
eteiCompletionTime = lens _eteiCompletionTime (\ s a -> s{_eteiCompletionTime = a}) . mapping _Nat;

instance FromJSON ExportTaskExecutionInfo where
        parseJSON
          = withObject "ExportTaskExecutionInfo"
              (\ x ->
                 ExportTaskExecutionInfo' <$>
                   (x .:? "creationTime") <*> (x .:? "completionTime"))

instance Hashable ExportTaskExecutionInfo

instance NFData ExportTaskExecutionInfo

-- | Represents the status of an export task.
--
--
--
-- /See:/ 'exportTaskStatus' smart constructor.
data ExportTaskStatus = ExportTaskStatus'
    { _etsCode    :: !(Maybe ExportTaskStatusCode)
    , _etsMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ExportTaskStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etsCode' - Status code of the export task.
--
-- * 'etsMessage' - Status message related to the @code@ .
exportTaskStatus
    :: ExportTaskStatus
exportTaskStatus =
    ExportTaskStatus'
    { _etsCode = Nothing
    , _etsMessage = Nothing
    }

-- | Status code of the export task.
etsCode :: Lens' ExportTaskStatus (Maybe ExportTaskStatusCode)
etsCode = lens _etsCode (\ s a -> s{_etsCode = a});

-- | Status message related to the @code@ .
etsMessage :: Lens' ExportTaskStatus (Maybe Text)
etsMessage = lens _etsMessage (\ s a -> s{_etsMessage = a});

instance FromJSON ExportTaskStatus where
        parseJSON
          = withObject "ExportTaskStatus"
              (\ x ->
                 ExportTaskStatus' <$>
                   (x .:? "code") <*> (x .:? "message"))

instance Hashable ExportTaskStatus

instance NFData ExportTaskStatus

-- | Represents a matched event from a @FilterLogEvents@ request.
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FilteredLogEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fleIngestionTime' - Undocumented member.
--
-- * 'fleLogStreamName' - The name of the log stream this event belongs to.
--
-- * 'fleMessage' - The data contained in the log event.
--
-- * 'fleTimestamp' - Undocumented member.
--
-- * 'fleEventId' - A unique identifier for this event.
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

instance Hashable FilteredLogEvent

instance NFData FilteredLogEvent

-- | A log event is a record of some activity that was recorded by the application or resource being monitored. The log event record that CloudWatch Logs understands contains two properties: the timestamp of when the event occurred, and the raw event message.
--
--
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
-- * 'ileTimestamp' - Undocumented member.
--
-- * 'ileMessage' - Undocumented member.
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

instance Hashable InputLogEvent

instance NFData InputLogEvent

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
-- * 'lgCreationTime' - Undocumented member.
--
-- * 'lgMetricFilterCount' - Undocumented member.
--
-- * 'lgArn' - Undocumented member.
--
-- * 'lgLogGroupName' - Undocumented member.
--
-- * 'lgRetentionInDays' - Undocumented member.
--
-- * 'lgStoredBytes' - Undocumented member.
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

instance Hashable LogGroup

instance NFData LogGroup

-- | A log stream is sequence of log events from a single emitter of logs.
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LogStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsCreationTime' - Undocumented member.
--
-- * 'lsUploadSequenceToken' - Undocumented member.
--
-- * 'lsArn' - Undocumented member.
--
-- * 'lsFirstEventTimestamp' - Undocumented member.
--
-- * 'lsLogStreamName' - Undocumented member.
--
-- * 'lsStoredBytes' - Undocumented member.
--
-- * 'lsLastIngestionTime' - Undocumented member.
--
-- * 'lsLastEventTimestamp' - Undocumented member.
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

instance Hashable LogStream

instance NFData LogStream

-- | Metric filters can be used to express how CloudWatch Logs would extract metric observations from ingested log events and transform them to metric data in a CloudWatch metric.
--
--
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
-- * 'mfCreationTime' - Undocumented member.
--
-- * 'mfFilterName' - Undocumented member.
--
-- * 'mfFilterPattern' - Undocumented member.
--
-- * 'mfMetricTransformations' - Undocumented member.
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

instance Hashable MetricFilter

instance NFData MetricFilter

-- | /See:/ 'metricFilterMatchRecord' smart constructor.
data MetricFilterMatchRecord = MetricFilterMatchRecord'
    { _mfmrExtractedValues :: !(Maybe (Map Text Text))
    , _mfmrEventNumber     :: !(Maybe Integer)
    , _mfmrEventMessage    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MetricFilterMatchRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mfmrExtractedValues' - Undocumented member.
--
-- * 'mfmrEventNumber' - Undocumented member.
--
-- * 'mfmrEventMessage' - Undocumented member.
metricFilterMatchRecord
    :: MetricFilterMatchRecord
metricFilterMatchRecord =
    MetricFilterMatchRecord'
    { _mfmrExtractedValues = Nothing
    , _mfmrEventNumber = Nothing
    , _mfmrEventMessage = Nothing
    }

-- | Undocumented member.
mfmrExtractedValues :: Lens' MetricFilterMatchRecord (HashMap Text Text)
mfmrExtractedValues = lens _mfmrExtractedValues (\ s a -> s{_mfmrExtractedValues = a}) . _Default . _Map;

-- | Undocumented member.
mfmrEventNumber :: Lens' MetricFilterMatchRecord (Maybe Integer)
mfmrEventNumber = lens _mfmrEventNumber (\ s a -> s{_mfmrEventNumber = a});

-- | Undocumented member.
mfmrEventMessage :: Lens' MetricFilterMatchRecord (Maybe Text)
mfmrEventMessage = lens _mfmrEventMessage (\ s a -> s{_mfmrEventMessage = a});

instance FromJSON MetricFilterMatchRecord where
        parseJSON
          = withObject "MetricFilterMatchRecord"
              (\ x ->
                 MetricFilterMatchRecord' <$>
                   (x .:? "extractedValues" .!= mempty) <*>
                     (x .:? "eventNumber")
                     <*> (x .:? "eventMessage"))

instance Hashable MetricFilterMatchRecord

instance NFData MetricFilterMatchRecord

-- | /See:/ 'metricTransformation' smart constructor.
data MetricTransformation = MetricTransformation'
    { _mtDefaultValue    :: !(Maybe Double)
    , _mtMetricName      :: !Text
    , _mtMetricNamespace :: !Text
    , _mtMetricValue     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MetricTransformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtDefaultValue' - (Optional) A default value to emit when a filter pattern does not match a log event. Can be null.
--
-- * 'mtMetricName' - Name of the metric.
--
-- * 'mtMetricNamespace' - Namespace to which the metric belongs.
--
-- * 'mtMetricValue' - A string representing a value to publish to this metric when a filter pattern matches a log event.
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

-- | (Optional) A default value to emit when a filter pattern does not match a log event. Can be null.
mtDefaultValue :: Lens' MetricTransformation (Maybe Double)
mtDefaultValue = lens _mtDefaultValue (\ s a -> s{_mtDefaultValue = a});

-- | Name of the metric.
mtMetricName :: Lens' MetricTransformation Text
mtMetricName = lens _mtMetricName (\ s a -> s{_mtMetricName = a});

-- | Namespace to which the metric belongs.
mtMetricNamespace :: Lens' MetricTransformation Text
mtMetricNamespace = lens _mtMetricNamespace (\ s a -> s{_mtMetricNamespace = a});

-- | A string representing a value to publish to this metric when a filter pattern matches a log event.
mtMetricValue :: Lens' MetricTransformation Text
mtMetricValue = lens _mtMetricValue (\ s a -> s{_mtMetricValue = a});

instance FromJSON MetricTransformation where
        parseJSON
          = withObject "MetricTransformation"
              (\ x ->
                 MetricTransformation' <$>
                   (x .:? "defaultValue") <*> (x .: "metricName") <*>
                     (x .: "metricNamespace")
                     <*> (x .: "metricValue"))

instance Hashable MetricTransformation

instance NFData MetricTransformation

instance ToJSON MetricTransformation where
        toJSON MetricTransformation'{..}
          = object
              (catMaybes
                 [("defaultValue" .=) <$> _mtDefaultValue,
                  Just ("metricName" .= _mtMetricName),
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
-- * 'oleIngestionTime' - Undocumented member.
--
-- * 'oleMessage' - Undocumented member.
--
-- * 'oleTimestamp' - Undocumented member.
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

instance Hashable OutputLogEvent

instance NFData OutputLogEvent

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
-- * 'rleiTooOldLogEventEndIndex' - Undocumented member.
--
-- * 'rleiTooNewLogEventStartIndex' - Undocumented member.
--
-- * 'rleiExpiredLogEventEndIndex' - Undocumented member.
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

instance Hashable RejectedLogEventsInfo

instance NFData RejectedLogEventsInfo

-- | An object indicating the search status of a log stream in a @FilterLogEvents@ request.
--
--
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
-- * 'slsLogStreamName' - The name of the log stream.
--
-- * 'slsSearchedCompletely' - Indicates whether all the events in this log stream were searched or more data exists to search by paginating further.
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

-- | Indicates whether all the events in this log stream were searched or more data exists to search by paginating further.
slsSearchedCompletely :: Lens' SearchedLogStream (Maybe Bool)
slsSearchedCompletely = lens _slsSearchedCompletely (\ s a -> s{_slsSearchedCompletely = a});

instance FromJSON SearchedLogStream where
        parseJSON
          = withObject "SearchedLogStream"
              (\ x ->
                 SearchedLogStream' <$>
                   (x .:? "logStreamName") <*>
                     (x .:? "searchedCompletely"))

instance Hashable SearchedLogStream

instance NFData SearchedLogStream

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
-- * 'sfCreationTime' - Undocumented member.
--
-- * 'sfFilterName' - Undocumented member.
--
-- * 'sfDestinationARN' - Undocumented member.
--
-- * 'sfLogGroupName' - Undocumented member.
--
-- * 'sfFilterPattern' - Undocumented member.
--
-- * 'sfRoleARN' - Undocumented member.
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

instance Hashable SubscriptionFilter

instance NFData SubscriptionFilter
