{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.Product where

import Network.AWS.CloudWatchEvents.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. This parameter is used only if the target is an AWS Batch job.
--
--
--
-- /See:/ 'batchArrayProperties' smart constructor.
newtype BatchArrayProperties = BatchArrayProperties'
  { _bapSize :: Maybe Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchArrayProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bapSize' - The size of the array, if this is an array batch job. Valid values are integers between 2 and 10,000.
batchArrayProperties
    :: BatchArrayProperties
batchArrayProperties = BatchArrayProperties' {_bapSize = Nothing}


-- | The size of the array, if this is an array batch job. Valid values are integers between 2 and 10,000.
bapSize :: Lens' BatchArrayProperties (Maybe Int)
bapSize = lens _bapSize (\ s a -> s{_bapSize = a})

instance FromJSON BatchArrayProperties where
        parseJSON
          = withObject "BatchArrayProperties"
              (\ x -> BatchArrayProperties' <$> (x .:? "Size"))

instance Hashable BatchArrayProperties where

instance NFData BatchArrayProperties where

instance ToJSON BatchArrayProperties where
        toJSON BatchArrayProperties'{..}
          = object (catMaybes [("Size" .=) <$> _bapSize])

-- | The custom parameters to be used when the target is an AWS Batch job.
--
--
--
-- /See:/ 'batchParameters' smart constructor.
data BatchParameters = BatchParameters'
  { _bpRetryStrategy   :: !(Maybe BatchRetryStrategy)
  , _bpArrayProperties :: !(Maybe BatchArrayProperties)
  , _bpJobDefinition   :: !Text
  , _bpJobName         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpRetryStrategy' - The retry strategy to use for failed jobs, if the target is an AWS Batch job. The retry strategy is the number of times to retry the failed job execution. Valid values are 1 to 10. When you specify a retry strategy here, it overrides the retry strategy defined in the job definition.
--
-- * 'bpArrayProperties' - The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. This parameter is used only if the target is an AWS Batch job.
--
-- * 'bpJobDefinition' - The ARN or name of the job definition to use if the event target is an AWS Batch job. This job definition must already exist.
--
-- * 'bpJobName' - The name to use for this execution of the job, if the target is an AWS Batch job.
batchParameters
    :: Text -- ^ 'bpJobDefinition'
    -> Text -- ^ 'bpJobName'
    -> BatchParameters
batchParameters pJobDefinition_ pJobName_ =
  BatchParameters'
    { _bpRetryStrategy = Nothing
    , _bpArrayProperties = Nothing
    , _bpJobDefinition = pJobDefinition_
    , _bpJobName = pJobName_
    }


-- | The retry strategy to use for failed jobs, if the target is an AWS Batch job. The retry strategy is the number of times to retry the failed job execution. Valid values are 1 to 10. When you specify a retry strategy here, it overrides the retry strategy defined in the job definition.
bpRetryStrategy :: Lens' BatchParameters (Maybe BatchRetryStrategy)
bpRetryStrategy = lens _bpRetryStrategy (\ s a -> s{_bpRetryStrategy = a})

-- | The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. This parameter is used only if the target is an AWS Batch job.
bpArrayProperties :: Lens' BatchParameters (Maybe BatchArrayProperties)
bpArrayProperties = lens _bpArrayProperties (\ s a -> s{_bpArrayProperties = a})

-- | The ARN or name of the job definition to use if the event target is an AWS Batch job. This job definition must already exist.
bpJobDefinition :: Lens' BatchParameters Text
bpJobDefinition = lens _bpJobDefinition (\ s a -> s{_bpJobDefinition = a})

-- | The name to use for this execution of the job, if the target is an AWS Batch job.
bpJobName :: Lens' BatchParameters Text
bpJobName = lens _bpJobName (\ s a -> s{_bpJobName = a})

instance FromJSON BatchParameters where
        parseJSON
          = withObject "BatchParameters"
              (\ x ->
                 BatchParameters' <$>
                   (x .:? "RetryStrategy") <*> (x .:? "ArrayProperties")
                     <*> (x .: "JobDefinition")
                     <*> (x .: "JobName"))

instance Hashable BatchParameters where

instance NFData BatchParameters where

instance ToJSON BatchParameters where
        toJSON BatchParameters'{..}
          = object
              (catMaybes
                 [("RetryStrategy" .=) <$> _bpRetryStrategy,
                  ("ArrayProperties" .=) <$> _bpArrayProperties,
                  Just ("JobDefinition" .= _bpJobDefinition),
                  Just ("JobName" .= _bpJobName)])

-- | The retry strategy to use for failed jobs, if the target is an AWS Batch job. If you specify a retry strategy here, it overrides the retry strategy defined in the job definition.
--
--
--
-- /See:/ 'batchRetryStrategy' smart constructor.
newtype BatchRetryStrategy = BatchRetryStrategy'
  { _brsAttempts :: Maybe Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchRetryStrategy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brsAttempts' - The number of times to attempt to retry, if the job fails. Valid values are 1 to 10.
batchRetryStrategy
    :: BatchRetryStrategy
batchRetryStrategy = BatchRetryStrategy' {_brsAttempts = Nothing}


-- | The number of times to attempt to retry, if the job fails. Valid values are 1 to 10.
brsAttempts :: Lens' BatchRetryStrategy (Maybe Int)
brsAttempts = lens _brsAttempts (\ s a -> s{_brsAttempts = a})

instance FromJSON BatchRetryStrategy where
        parseJSON
          = withObject "BatchRetryStrategy"
              (\ x -> BatchRetryStrategy' <$> (x .:? "Attempts"))

instance Hashable BatchRetryStrategy where

instance NFData BatchRetryStrategy where

instance ToJSON BatchRetryStrategy where
        toJSON BatchRetryStrategy'{..}
          = object
              (catMaybes [("Attempts" .=) <$> _brsAttempts])

-- | The custom parameters to be used when the target is an Amazon ECS cluster.
--
--
--
-- /See:/ 'ecsParameters' smart constructor.
data EcsParameters = EcsParameters'
  { _epTaskCount         :: !(Maybe Nat)
  , _epTaskDefinitionARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EcsParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epTaskCount' - The number of tasks to create based on the @TaskDefinition@ . The default is one.
--
-- * 'epTaskDefinitionARN' - The ARN of the task definition to use if the event target is an Amazon ECS cluster.
ecsParameters
    :: Text -- ^ 'epTaskDefinitionARN'
    -> EcsParameters
ecsParameters pTaskDefinitionARN_ =
  EcsParameters'
    {_epTaskCount = Nothing, _epTaskDefinitionARN = pTaskDefinitionARN_}


-- | The number of tasks to create based on the @TaskDefinition@ . The default is one.
epTaskCount :: Lens' EcsParameters (Maybe Natural)
epTaskCount = lens _epTaskCount (\ s a -> s{_epTaskCount = a}) . mapping _Nat

-- | The ARN of the task definition to use if the event target is an Amazon ECS cluster.
epTaskDefinitionARN :: Lens' EcsParameters Text
epTaskDefinitionARN = lens _epTaskDefinitionARN (\ s a -> s{_epTaskDefinitionARN = a})

instance FromJSON EcsParameters where
        parseJSON
          = withObject "EcsParameters"
              (\ x ->
                 EcsParameters' <$>
                   (x .:? "TaskCount") <*> (x .: "TaskDefinitionArn"))

instance Hashable EcsParameters where

instance NFData EcsParameters where

instance ToJSON EcsParameters where
        toJSON EcsParameters'{..}
          = object
              (catMaybes
                 [("TaskCount" .=) <$> _epTaskCount,
                  Just ("TaskDefinitionArn" .= _epTaskDefinitionARN)])

-- | Contains the parameters needed for you to provide custom input to a target based on one or more pieces of data extracted from the event.
--
--
--
-- /See:/ 'inputTransformer' smart constructor.
data InputTransformer = InputTransformer'
  { _itInputPathsMap :: !(Maybe (Map Text Text))
  , _itInputTemplate :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputTransformer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itInputPathsMap' - Map of JSON paths to be extracted from the event. These are key-value pairs, where each value is a JSON path. You must use JSON dot notation, not bracket notation.
--
-- * 'itInputTemplate' - Input template where you can use the values of the keys from @InputPathsMap@ to customize the data sent to the target.
inputTransformer
    :: Text -- ^ 'itInputTemplate'
    -> InputTransformer
inputTransformer pInputTemplate_ =
  InputTransformer'
    {_itInputPathsMap = Nothing, _itInputTemplate = pInputTemplate_}


-- | Map of JSON paths to be extracted from the event. These are key-value pairs, where each value is a JSON path. You must use JSON dot notation, not bracket notation.
itInputPathsMap :: Lens' InputTransformer (HashMap Text Text)
itInputPathsMap = lens _itInputPathsMap (\ s a -> s{_itInputPathsMap = a}) . _Default . _Map

-- | Input template where you can use the values of the keys from @InputPathsMap@ to customize the data sent to the target.
itInputTemplate :: Lens' InputTransformer Text
itInputTemplate = lens _itInputTemplate (\ s a -> s{_itInputTemplate = a})

instance FromJSON InputTransformer where
        parseJSON
          = withObject "InputTransformer"
              (\ x ->
                 InputTransformer' <$>
                   (x .:? "InputPathsMap" .!= mempty) <*>
                     (x .: "InputTemplate"))

instance Hashable InputTransformer where

instance NFData InputTransformer where

instance ToJSON InputTransformer where
        toJSON InputTransformer'{..}
          = object
              (catMaybes
                 [("InputPathsMap" .=) <$> _itInputPathsMap,
                  Just ("InputTemplate" .= _itInputTemplate)])

-- | This object enables you to specify a JSON path to extract from the event and use as the partition key for the Amazon Kinesis stream, so that you can control the shard to which the event goes. If you do not include this parameter, the default is to use the @eventId@ as the partition key.
--
--
--
-- /See:/ 'kinesisParameters' smart constructor.
newtype KinesisParameters = KinesisParameters'
  { _kpPartitionKeyPath :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpPartitionKeyPath' - The JSON path to be extracted from the event and used as the partition key. For more information, see <http://docs.aws.amazon.com/streams/latest/dev/key-concepts.html#partition-key Amazon Kinesis Streams Key Concepts> in the /Amazon Kinesis Streams Developer Guide/ .
kinesisParameters
    :: Text -- ^ 'kpPartitionKeyPath'
    -> KinesisParameters
kinesisParameters pPartitionKeyPath_ =
  KinesisParameters' {_kpPartitionKeyPath = pPartitionKeyPath_}


-- | The JSON path to be extracted from the event and used as the partition key. For more information, see <http://docs.aws.amazon.com/streams/latest/dev/key-concepts.html#partition-key Amazon Kinesis Streams Key Concepts> in the /Amazon Kinesis Streams Developer Guide/ .
kpPartitionKeyPath :: Lens' KinesisParameters Text
kpPartitionKeyPath = lens _kpPartitionKeyPath (\ s a -> s{_kpPartitionKeyPath = a})

instance FromJSON KinesisParameters where
        parseJSON
          = withObject "KinesisParameters"
              (\ x ->
                 KinesisParameters' <$> (x .: "PartitionKeyPath"))

instance Hashable KinesisParameters where

instance NFData KinesisParameters where

instance ToJSON KinesisParameters where
        toJSON KinesisParameters'{..}
          = object
              (catMaybes
                 [Just ("PartitionKeyPath" .= _kpPartitionKeyPath)])

-- | Represents an event to be submitted.
--
--
--
-- /See:/ 'putEventsRequestEntry' smart constructor.
data PutEventsRequestEntry = PutEventsRequestEntry'
  { _pereTime       :: !(Maybe POSIX)
  , _pereDetailType :: !(Maybe Text)
  , _pereResources  :: !(Maybe [Text])
  , _pereSource     :: !(Maybe Text)
  , _pereDetail     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutEventsRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pereTime' - The timestamp of the event, per <https://www.rfc-editor.org/rfc/rfc3339.txt RFC3339> . If no timestamp is provided, the timestamp of the 'PutEvents' call is used.
--
-- * 'pereDetailType' - Free-form string used to decide what fields to expect in the event detail.
--
-- * 'pereResources' - AWS resources, identified by Amazon Resource Name (ARN), which the event primarily concerns. Any number, including zero, may be present.
--
-- * 'pereSource' - The source of the event.
--
-- * 'pereDetail' - A valid JSON string. There is no other schema imposed. The JSON string may contain fields and nested subobjects.
putEventsRequestEntry
    :: PutEventsRequestEntry
putEventsRequestEntry =
  PutEventsRequestEntry'
    { _pereTime = Nothing
    , _pereDetailType = Nothing
    , _pereResources = Nothing
    , _pereSource = Nothing
    , _pereDetail = Nothing
    }


-- | The timestamp of the event, per <https://www.rfc-editor.org/rfc/rfc3339.txt RFC3339> . If no timestamp is provided, the timestamp of the 'PutEvents' call is used.
pereTime :: Lens' PutEventsRequestEntry (Maybe UTCTime)
pereTime = lens _pereTime (\ s a -> s{_pereTime = a}) . mapping _Time

-- | Free-form string used to decide what fields to expect in the event detail.
pereDetailType :: Lens' PutEventsRequestEntry (Maybe Text)
pereDetailType = lens _pereDetailType (\ s a -> s{_pereDetailType = a})

-- | AWS resources, identified by Amazon Resource Name (ARN), which the event primarily concerns. Any number, including zero, may be present.
pereResources :: Lens' PutEventsRequestEntry [Text]
pereResources = lens _pereResources (\ s a -> s{_pereResources = a}) . _Default . _Coerce

-- | The source of the event.
pereSource :: Lens' PutEventsRequestEntry (Maybe Text)
pereSource = lens _pereSource (\ s a -> s{_pereSource = a})

-- | A valid JSON string. There is no other schema imposed. The JSON string may contain fields and nested subobjects.
pereDetail :: Lens' PutEventsRequestEntry (Maybe Text)
pereDetail = lens _pereDetail (\ s a -> s{_pereDetail = a})

instance Hashable PutEventsRequestEntry where

instance NFData PutEventsRequestEntry where

instance ToJSON PutEventsRequestEntry where
        toJSON PutEventsRequestEntry'{..}
          = object
              (catMaybes
                 [("Time" .=) <$> _pereTime,
                  ("DetailType" .=) <$> _pereDetailType,
                  ("Resources" .=) <$> _pereResources,
                  ("Source" .=) <$> _pereSource,
                  ("Detail" .=) <$> _pereDetail])

-- | Represents an event that failed to be submitted.
--
--
--
-- /See:/ 'putEventsResultEntry' smart constructor.
data PutEventsResultEntry = PutEventsResultEntry'
  { _pereErrorCode    :: !(Maybe Text)
  , _pereErrorMessage :: !(Maybe Text)
  , _pereEventId      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutEventsResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pereErrorCode' - The error code that indicates why the event submission failed.
--
-- * 'pereErrorMessage' - The error message that explains why the event submission failed.
--
-- * 'pereEventId' - The ID of the event.
putEventsResultEntry
    :: PutEventsResultEntry
putEventsResultEntry =
  PutEventsResultEntry'
    { _pereErrorCode = Nothing
    , _pereErrorMessage = Nothing
    , _pereEventId = Nothing
    }


-- | The error code that indicates why the event submission failed.
pereErrorCode :: Lens' PutEventsResultEntry (Maybe Text)
pereErrorCode = lens _pereErrorCode (\ s a -> s{_pereErrorCode = a})

-- | The error message that explains why the event submission failed.
pereErrorMessage :: Lens' PutEventsResultEntry (Maybe Text)
pereErrorMessage = lens _pereErrorMessage (\ s a -> s{_pereErrorMessage = a})

-- | The ID of the event.
pereEventId :: Lens' PutEventsResultEntry (Maybe Text)
pereEventId = lens _pereEventId (\ s a -> s{_pereEventId = a})

instance FromJSON PutEventsResultEntry where
        parseJSON
          = withObject "PutEventsResultEntry"
              (\ x ->
                 PutEventsResultEntry' <$>
                   (x .:? "ErrorCode") <*> (x .:? "ErrorMessage") <*>
                     (x .:? "EventId"))

instance Hashable PutEventsResultEntry where

instance NFData PutEventsResultEntry where

-- | Represents a target that failed to be added to a rule.
--
--
--
-- /See:/ 'putTargetsResultEntry' smart constructor.
data PutTargetsResultEntry = PutTargetsResultEntry'
  { _ptreTargetId     :: !(Maybe Text)
  , _ptreErrorCode    :: !(Maybe Text)
  , _ptreErrorMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutTargetsResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptreTargetId' - The ID of the target.
--
-- * 'ptreErrorCode' - The error code that indicates why the target addition failed. If the value is @ConcurrentModificationException@ , too many requests were made at the same time.
--
-- * 'ptreErrorMessage' - The error message that explains why the target addition failed.
putTargetsResultEntry
    :: PutTargetsResultEntry
putTargetsResultEntry =
  PutTargetsResultEntry'
    { _ptreTargetId = Nothing
    , _ptreErrorCode = Nothing
    , _ptreErrorMessage = Nothing
    }


-- | The ID of the target.
ptreTargetId :: Lens' PutTargetsResultEntry (Maybe Text)
ptreTargetId = lens _ptreTargetId (\ s a -> s{_ptreTargetId = a})

-- | The error code that indicates why the target addition failed. If the value is @ConcurrentModificationException@ , too many requests were made at the same time.
ptreErrorCode :: Lens' PutTargetsResultEntry (Maybe Text)
ptreErrorCode = lens _ptreErrorCode (\ s a -> s{_ptreErrorCode = a})

-- | The error message that explains why the target addition failed.
ptreErrorMessage :: Lens' PutTargetsResultEntry (Maybe Text)
ptreErrorMessage = lens _ptreErrorMessage (\ s a -> s{_ptreErrorMessage = a})

instance FromJSON PutTargetsResultEntry where
        parseJSON
          = withObject "PutTargetsResultEntry"
              (\ x ->
                 PutTargetsResultEntry' <$>
                   (x .:? "TargetId") <*> (x .:? "ErrorCode") <*>
                     (x .:? "ErrorMessage"))

instance Hashable PutTargetsResultEntry where

instance NFData PutTargetsResultEntry where

-- | Represents a target that failed to be removed from a rule.
--
--
--
-- /See:/ 'removeTargetsResultEntry' smart constructor.
data RemoveTargetsResultEntry = RemoveTargetsResultEntry'
  { _rtreTargetId     :: !(Maybe Text)
  , _rtreErrorCode    :: !(Maybe Text)
  , _rtreErrorMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTargetsResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtreTargetId' - The ID of the target.
--
-- * 'rtreErrorCode' - The error code that indicates why the target removal failed. If the value is @ConcurrentModificationException@ , too many requests were made at the same time.
--
-- * 'rtreErrorMessage' - The error message that explains why the target removal failed.
removeTargetsResultEntry
    :: RemoveTargetsResultEntry
removeTargetsResultEntry =
  RemoveTargetsResultEntry'
    { _rtreTargetId = Nothing
    , _rtreErrorCode = Nothing
    , _rtreErrorMessage = Nothing
    }


-- | The ID of the target.
rtreTargetId :: Lens' RemoveTargetsResultEntry (Maybe Text)
rtreTargetId = lens _rtreTargetId (\ s a -> s{_rtreTargetId = a})

-- | The error code that indicates why the target removal failed. If the value is @ConcurrentModificationException@ , too many requests were made at the same time.
rtreErrorCode :: Lens' RemoveTargetsResultEntry (Maybe Text)
rtreErrorCode = lens _rtreErrorCode (\ s a -> s{_rtreErrorCode = a})

-- | The error message that explains why the target removal failed.
rtreErrorMessage :: Lens' RemoveTargetsResultEntry (Maybe Text)
rtreErrorMessage = lens _rtreErrorMessage (\ s a -> s{_rtreErrorMessage = a})

instance FromJSON RemoveTargetsResultEntry where
        parseJSON
          = withObject "RemoveTargetsResultEntry"
              (\ x ->
                 RemoveTargetsResultEntry' <$>
                   (x .:? "TargetId") <*> (x .:? "ErrorCode") <*>
                     (x .:? "ErrorMessage"))

instance Hashable RemoveTargetsResultEntry where

instance NFData RemoveTargetsResultEntry where

-- | Contains information about a rule in Amazon CloudWatch Events.
--
--
--
-- /See:/ 'rule' smart constructor.
data Rule = Rule'
  { _rEventPattern       :: !(Maybe Text)
  , _rState              :: !(Maybe RuleState)
  , _rARN                :: !(Maybe Text)
  , _rScheduleExpression :: !(Maybe Text)
  , _rName               :: !(Maybe Text)
  , _rDescription        :: !(Maybe Text)
  , _rRoleARN            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rEventPattern' - The event pattern of the rule. For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/events/CloudWatchEventsandEventPatterns.html Events and Event Patterns> in the /Amazon CloudWatch Events User Guide/ .
--
-- * 'rState' - The state of the rule.
--
-- * 'rARN' - The Amazon Resource Name (ARN) of the rule.
--
-- * 'rScheduleExpression' - The scheduling expression. For example, "cron(0 20 * * ? *)", "rate(5 minutes)".
--
-- * 'rName' - The name of the rule.
--
-- * 'rDescription' - The description of the rule.
--
-- * 'rRoleARN' - The Amazon Resource Name (ARN) of the role that is used for target invocation.
rule
    :: Rule
rule =
  Rule'
    { _rEventPattern = Nothing
    , _rState = Nothing
    , _rARN = Nothing
    , _rScheduleExpression = Nothing
    , _rName = Nothing
    , _rDescription = Nothing
    , _rRoleARN = Nothing
    }


-- | The event pattern of the rule. For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/events/CloudWatchEventsandEventPatterns.html Events and Event Patterns> in the /Amazon CloudWatch Events User Guide/ .
rEventPattern :: Lens' Rule (Maybe Text)
rEventPattern = lens _rEventPattern (\ s a -> s{_rEventPattern = a})

-- | The state of the rule.
rState :: Lens' Rule (Maybe RuleState)
rState = lens _rState (\ s a -> s{_rState = a})

-- | The Amazon Resource Name (ARN) of the rule.
rARN :: Lens' Rule (Maybe Text)
rARN = lens _rARN (\ s a -> s{_rARN = a})

-- | The scheduling expression. For example, "cron(0 20 * * ? *)", "rate(5 minutes)".
rScheduleExpression :: Lens' Rule (Maybe Text)
rScheduleExpression = lens _rScheduleExpression (\ s a -> s{_rScheduleExpression = a})

-- | The name of the rule.
rName :: Lens' Rule (Maybe Text)
rName = lens _rName (\ s a -> s{_rName = a})

-- | The description of the rule.
rDescription :: Lens' Rule (Maybe Text)
rDescription = lens _rDescription (\ s a -> s{_rDescription = a})

-- | The Amazon Resource Name (ARN) of the role that is used for target invocation.
rRoleARN :: Lens' Rule (Maybe Text)
rRoleARN = lens _rRoleARN (\ s a -> s{_rRoleARN = a})

instance FromJSON Rule where
        parseJSON
          = withObject "Rule"
              (\ x ->
                 Rule' <$>
                   (x .:? "EventPattern") <*> (x .:? "State") <*>
                     (x .:? "Arn")
                     <*> (x .:? "ScheduleExpression")
                     <*> (x .:? "Name")
                     <*> (x .:? "Description")
                     <*> (x .:? "RoleArn"))

instance Hashable Rule where

instance NFData Rule where

-- | This parameter contains the criteria (either InstanceIds or a tag) used to specify which EC2 instances are to be sent the command.
--
--
--
-- /See:/ 'runCommandParameters' smart constructor.
newtype RunCommandParameters = RunCommandParameters'
  { _rcpRunCommandTargets :: List1 RunCommandTarget
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RunCommandParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcpRunCommandTargets' - Currently, we support including only one RunCommandTarget block, which specifies either an array of InstanceIds or a tag.
runCommandParameters
    :: NonEmpty RunCommandTarget -- ^ 'rcpRunCommandTargets'
    -> RunCommandParameters
runCommandParameters pRunCommandTargets_ =
  RunCommandParameters' {_rcpRunCommandTargets = _List1 # pRunCommandTargets_}


-- | Currently, we support including only one RunCommandTarget block, which specifies either an array of InstanceIds or a tag.
rcpRunCommandTargets :: Lens' RunCommandParameters (NonEmpty RunCommandTarget)
rcpRunCommandTargets = lens _rcpRunCommandTargets (\ s a -> s{_rcpRunCommandTargets = a}) . _List1

instance FromJSON RunCommandParameters where
        parseJSON
          = withObject "RunCommandParameters"
              (\ x ->
                 RunCommandParameters' <$> (x .: "RunCommandTargets"))

instance Hashable RunCommandParameters where

instance NFData RunCommandParameters where

instance ToJSON RunCommandParameters where
        toJSON RunCommandParameters'{..}
          = object
              (catMaybes
                 [Just
                    ("RunCommandTargets" .= _rcpRunCommandTargets)])

-- | Information about the EC2 instances that are to be sent the command, specified as key-value pairs. Each @RunCommandTarget@ block can include only one key, but this key may specify multiple values.
--
--
--
-- /See:/ 'runCommandTarget' smart constructor.
data RunCommandTarget = RunCommandTarget'
  { _rctKey    :: !Text
  , _rctValues :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RunCommandTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rctKey' - Can be either @tag:@ /tag-key/ or @InstanceIds@ .
--
-- * 'rctValues' - If @Key@ is @tag:@ /tag-key/ , @Values@ is a list of tag values. If @Key@ is @InstanceIds@ , @Values@ is a list of Amazon EC2 instance IDs.
runCommandTarget
    :: Text -- ^ 'rctKey'
    -> NonEmpty Text -- ^ 'rctValues'
    -> RunCommandTarget
runCommandTarget pKey_ pValues_ =
  RunCommandTarget' {_rctKey = pKey_, _rctValues = _List1 # pValues_}


-- | Can be either @tag:@ /tag-key/ or @InstanceIds@ .
rctKey :: Lens' RunCommandTarget Text
rctKey = lens _rctKey (\ s a -> s{_rctKey = a})

-- | If @Key@ is @tag:@ /tag-key/ , @Values@ is a list of tag values. If @Key@ is @InstanceIds@ , @Values@ is a list of Amazon EC2 instance IDs.
rctValues :: Lens' RunCommandTarget (NonEmpty Text)
rctValues = lens _rctValues (\ s a -> s{_rctValues = a}) . _List1

instance FromJSON RunCommandTarget where
        parseJSON
          = withObject "RunCommandTarget"
              (\ x ->
                 RunCommandTarget' <$>
                   (x .: "Key") <*> (x .: "Values"))

instance Hashable RunCommandTarget where

instance NFData RunCommandTarget where

instance ToJSON RunCommandTarget where
        toJSON RunCommandTarget'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _rctKey),
                  Just ("Values" .= _rctValues)])

-- | This structure includes the custom parameter to be used when the target is an SQS FIFO queue.
--
--
--
-- /See:/ 'sqsParameters' smart constructor.
newtype SqsParameters = SqsParameters'
  { _spMessageGroupId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SqsParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spMessageGroupId' - The FIFO message group ID to use as the target.
sqsParameters
    :: SqsParameters
sqsParameters = SqsParameters' {_spMessageGroupId = Nothing}


-- | The FIFO message group ID to use as the target.
spMessageGroupId :: Lens' SqsParameters (Maybe Text)
spMessageGroupId = lens _spMessageGroupId (\ s a -> s{_spMessageGroupId = a})

instance FromJSON SqsParameters where
        parseJSON
          = withObject "SqsParameters"
              (\ x -> SqsParameters' <$> (x .:? "MessageGroupId"))

instance Hashable SqsParameters where

instance NFData SqsParameters where

instance ToJSON SqsParameters where
        toJSON SqsParameters'{..}
          = object
              (catMaybes
                 [("MessageGroupId" .=) <$> _spMessageGroupId])

-- | Targets are the resources to be invoked when a rule is triggered. Target types include EC2 instances, AWS Lambda functions, Amazon Kinesis streams, Amazon ECS tasks, AWS Step Functions state machines, Run Command, and built-in targets.
--
--
--
-- /See:/ 'target' smart constructor.
data Target = Target'
  { _tRunCommandParameters :: !(Maybe RunCommandParameters)
  , _tKinesisParameters    :: !(Maybe KinesisParameters)
  , _tInputTransformer     :: !(Maybe InputTransformer)
  , _tSqsParameters        :: !(Maybe SqsParameters)
  , _tInput                :: !(Maybe Text)
  , _tBatchParameters      :: !(Maybe BatchParameters)
  , _tEcsParameters        :: !(Maybe EcsParameters)
  , _tInputPath            :: !(Maybe Text)
  , _tRoleARN              :: !(Maybe Text)
  , _tId                   :: !Text
  , _tARN                  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Target' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tRunCommandParameters' - Parameters used when you are using the rule to invoke Amazon EC2 Run Command.
--
-- * 'tKinesisParameters' - The custom parameter you can use to control shard assignment, when the target is an Amazon Kinesis stream. If you do not include this parameter, the default is to use the @eventId@ as the partition key.
--
-- * 'tInputTransformer' - Settings to enable you to provide custom input to a target based on certain event data. You can extract one or more key-value pairs from the event and then use that data to send customized input to the target.
--
-- * 'tSqsParameters' - Contains the message group ID to use when the target is a FIFO queue.
--
-- * 'tInput' - Valid JSON text passed to the target. In this case, nothing from the event itself is passed to the target. For more information, see <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format> .
--
-- * 'tBatchParameters' - Contains the job definition, job name, and other parameters if the event target is an AWS Batch job. For more information about AWS Batch, see <http://docs.aws.amazon.com/batch/latest/userguide/jobs.html Jobs> in the /AWS Batch User Guide/ .
--
-- * 'tEcsParameters' - Contains the Amazon ECS task definition and task count to be used, if the event target is an Amazon ECS task. For more information about Amazon ECS tasks, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Task Definitions > in the /Amazon EC2 Container Service Developer Guide/ .
--
-- * 'tInputPath' - The value of the JSONPath that is used for extracting part of the matched event when passing it to the target. You must use JSON dot notation, not bracket notation. For more information about JSON paths, see <http://goessner.net/articles/JsonPath/ JSONPath> .
--
-- * 'tRoleARN' - The Amazon Resource Name (ARN) of the IAM role to be used for this target when the rule is triggered. If one rule triggers multiple targets, you can use a different IAM role for each target.
--
-- * 'tId' - The ID of the target.
--
-- * 'tARN' - The Amazon Resource Name (ARN) of the target.
target
    :: Text -- ^ 'tId'
    -> Text -- ^ 'tARN'
    -> Target
target pId_ pARN_ =
  Target'
    { _tRunCommandParameters = Nothing
    , _tKinesisParameters = Nothing
    , _tInputTransformer = Nothing
    , _tSqsParameters = Nothing
    , _tInput = Nothing
    , _tBatchParameters = Nothing
    , _tEcsParameters = Nothing
    , _tInputPath = Nothing
    , _tRoleARN = Nothing
    , _tId = pId_
    , _tARN = pARN_
    }


-- | Parameters used when you are using the rule to invoke Amazon EC2 Run Command.
tRunCommandParameters :: Lens' Target (Maybe RunCommandParameters)
tRunCommandParameters = lens _tRunCommandParameters (\ s a -> s{_tRunCommandParameters = a})

-- | The custom parameter you can use to control shard assignment, when the target is an Amazon Kinesis stream. If you do not include this parameter, the default is to use the @eventId@ as the partition key.
tKinesisParameters :: Lens' Target (Maybe KinesisParameters)
tKinesisParameters = lens _tKinesisParameters (\ s a -> s{_tKinesisParameters = a})

-- | Settings to enable you to provide custom input to a target based on certain event data. You can extract one or more key-value pairs from the event and then use that data to send customized input to the target.
tInputTransformer :: Lens' Target (Maybe InputTransformer)
tInputTransformer = lens _tInputTransformer (\ s a -> s{_tInputTransformer = a})

-- | Contains the message group ID to use when the target is a FIFO queue.
tSqsParameters :: Lens' Target (Maybe SqsParameters)
tSqsParameters = lens _tSqsParameters (\ s a -> s{_tSqsParameters = a})

-- | Valid JSON text passed to the target. In this case, nothing from the event itself is passed to the target. For more information, see <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format> .
tInput :: Lens' Target (Maybe Text)
tInput = lens _tInput (\ s a -> s{_tInput = a})

-- | Contains the job definition, job name, and other parameters if the event target is an AWS Batch job. For more information about AWS Batch, see <http://docs.aws.amazon.com/batch/latest/userguide/jobs.html Jobs> in the /AWS Batch User Guide/ .
tBatchParameters :: Lens' Target (Maybe BatchParameters)
tBatchParameters = lens _tBatchParameters (\ s a -> s{_tBatchParameters = a})

-- | Contains the Amazon ECS task definition and task count to be used, if the event target is an Amazon ECS task. For more information about Amazon ECS tasks, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Task Definitions > in the /Amazon EC2 Container Service Developer Guide/ .
tEcsParameters :: Lens' Target (Maybe EcsParameters)
tEcsParameters = lens _tEcsParameters (\ s a -> s{_tEcsParameters = a})

-- | The value of the JSONPath that is used for extracting part of the matched event when passing it to the target. You must use JSON dot notation, not bracket notation. For more information about JSON paths, see <http://goessner.net/articles/JsonPath/ JSONPath> .
tInputPath :: Lens' Target (Maybe Text)
tInputPath = lens _tInputPath (\ s a -> s{_tInputPath = a})

-- | The Amazon Resource Name (ARN) of the IAM role to be used for this target when the rule is triggered. If one rule triggers multiple targets, you can use a different IAM role for each target.
tRoleARN :: Lens' Target (Maybe Text)
tRoleARN = lens _tRoleARN (\ s a -> s{_tRoleARN = a})

-- | The ID of the target.
tId :: Lens' Target Text
tId = lens _tId (\ s a -> s{_tId = a})

-- | The Amazon Resource Name (ARN) of the target.
tARN :: Lens' Target Text
tARN = lens _tARN (\ s a -> s{_tARN = a})

instance FromJSON Target where
        parseJSON
          = withObject "Target"
              (\ x ->
                 Target' <$>
                   (x .:? "RunCommandParameters") <*>
                     (x .:? "KinesisParameters")
                     <*> (x .:? "InputTransformer")
                     <*> (x .:? "SqsParameters")
                     <*> (x .:? "Input")
                     <*> (x .:? "BatchParameters")
                     <*> (x .:? "EcsParameters")
                     <*> (x .:? "InputPath")
                     <*> (x .:? "RoleArn")
                     <*> (x .: "Id")
                     <*> (x .: "Arn"))

instance Hashable Target where

instance NFData Target where

instance ToJSON Target where
        toJSON Target'{..}
          = object
              (catMaybes
                 [("RunCommandParameters" .=) <$>
                    _tRunCommandParameters,
                  ("KinesisParameters" .=) <$> _tKinesisParameters,
                  ("InputTransformer" .=) <$> _tInputTransformer,
                  ("SqsParameters" .=) <$> _tSqsParameters,
                  ("Input" .=) <$> _tInput,
                  ("BatchParameters" .=) <$> _tBatchParameters,
                  ("EcsParameters" .=) <$> _tEcsParameters,
                  ("InputPath" .=) <$> _tInputPath,
                  ("RoleArn" .=) <$> _tRoleARN, Just ("Id" .= _tId),
                  Just ("Arn" .= _tARN)])
