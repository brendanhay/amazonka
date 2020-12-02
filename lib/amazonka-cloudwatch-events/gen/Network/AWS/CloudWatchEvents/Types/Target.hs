{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.Target
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.Target where

import Network.AWS.CloudWatchEvents.Types.BatchParameters
import Network.AWS.CloudWatchEvents.Types.DeadLetterConfig
import Network.AWS.CloudWatchEvents.Types.EcsParameters
import Network.AWS.CloudWatchEvents.Types.HTTPParameters
import Network.AWS.CloudWatchEvents.Types.InputTransformer
import Network.AWS.CloudWatchEvents.Types.KinesisParameters
import Network.AWS.CloudWatchEvents.Types.RedshiftDataParameters
import Network.AWS.CloudWatchEvents.Types.RetryPolicy
import Network.AWS.CloudWatchEvents.Types.RunCommandParameters
import Network.AWS.CloudWatchEvents.Types.SqsParameters
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Targets are the resources to be invoked when a rule is triggered. For a complete list of services and resources that can be set as a target, see 'PutTargets' .
--
--
-- If you are setting the event bus of another account as the target, and that account granted permission to your account through an organization instead of directly by the account ID, then you must specify a @RoleArn@ with proper permissions in the @Target@ structure. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-cross-account-event-delivery.html Sending and Receiving Events Between AWS Accounts> in the /Amazon EventBridge User Guide/ .
--
--
-- /See:/ 'target' smart constructor.
data Target = Target'
  { _tRunCommandParameters ::
      !(Maybe RunCommandParameters),
    _tHTTPParameters :: !(Maybe HTTPParameters),
    _tKinesisParameters :: !(Maybe KinesisParameters),
    _tInputTransformer :: !(Maybe InputTransformer),
    _tDeadLetterConfig :: !(Maybe DeadLetterConfig),
    _tSqsParameters :: !(Maybe SqsParameters),
    _tInput :: !(Maybe Text),
    _tBatchParameters :: !(Maybe BatchParameters),
    _tRedshiftDataParameters :: !(Maybe RedshiftDataParameters),
    _tEcsParameters :: !(Maybe EcsParameters),
    _tRetryPolicy :: !(Maybe RetryPolicy),
    _tInputPath :: !(Maybe Text),
    _tRoleARN :: !(Maybe Text),
    _tId :: !Text,
    _tARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Target' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tRunCommandParameters' - Parameters used when you are using the rule to invoke Amazon EC2 Run Command.
--
-- * 'tHTTPParameters' - Contains the HTTP parameters to use when the target is a API Gateway REST endpoint. If you specify an API Gateway REST API as a target, you can use this parameter to specify headers, path parameter, query string keys/values as part of your target invoking request.
--
-- * 'tKinesisParameters' - The custom parameter you can use to control the shard assignment, when the target is a Kinesis data stream. If you do not include this parameter, the default is to use the @eventId@ as the partition key.
--
-- * 'tInputTransformer' - Settings to enable you to provide custom input to a target based on certain event data. You can extract one or more key-value pairs from the event and then use that data to send customized input to the target.
--
-- * 'tDeadLetterConfig' - The @DeadLetterConfig@ that defines the target queue to send dead-letter queue events to.
--
-- * 'tSqsParameters' - Contains the message group ID to use when the target is a FIFO queue. If you specify an SQS FIFO queue as a target, the queue must have content-based deduplication enabled.
--
-- * 'tInput' - Valid JSON text passed to the target. In this case, nothing from the event itself is passed to the target. For more information, see <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format> .
--
-- * 'tBatchParameters' - If the event target is an AWS Batch job, this contains the job definition, job name, and other parameters. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/jobs.html Jobs> in the /AWS Batch User Guide/ .
--
-- * 'tRedshiftDataParameters' - Contains the Redshift Data API parameters to use when the target is a Redshift cluster. If you specify a Redshift Cluster as a Target, you can use this to specify parameters to invoke the Redshift Data API ExecuteStatement based on EventBridge events.
--
-- * 'tEcsParameters' - Contains the Amazon ECS task definition and task count to be used, if the event target is an Amazon ECS task. For more information about Amazon ECS tasks, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Task Definitions > in the /Amazon EC2 Container Service Developer Guide/ .
--
-- * 'tRetryPolicy' - The @RetryPolicy@ object that contains the retry policy configuration to use for the dead-letter queue.
--
-- * 'tInputPath' - The value of the JSONPath that is used for extracting part of the matched event when passing it to the target. You must use JSON dot notation, not bracket notation. For more information about JSON paths, see <http://goessner.net/articles/JsonPath/ JSONPath> .
--
-- * 'tRoleARN' - The Amazon Resource Name (ARN) of the IAM role to be used for this target when the rule is triggered. If one rule triggers multiple targets, you can use a different IAM role for each target.
--
-- * 'tId' - The ID of the target.
--
-- * 'tARN' - The Amazon Resource Name (ARN) of the target.
target ::
  -- | 'tId'
  Text ->
  -- | 'tARN'
  Text ->
  Target
target pId_ pARN_ =
  Target'
    { _tRunCommandParameters = Nothing,
      _tHTTPParameters = Nothing,
      _tKinesisParameters = Nothing,
      _tInputTransformer = Nothing,
      _tDeadLetterConfig = Nothing,
      _tSqsParameters = Nothing,
      _tInput = Nothing,
      _tBatchParameters = Nothing,
      _tRedshiftDataParameters = Nothing,
      _tEcsParameters = Nothing,
      _tRetryPolicy = Nothing,
      _tInputPath = Nothing,
      _tRoleARN = Nothing,
      _tId = pId_,
      _tARN = pARN_
    }

-- | Parameters used when you are using the rule to invoke Amazon EC2 Run Command.
tRunCommandParameters :: Lens' Target (Maybe RunCommandParameters)
tRunCommandParameters = lens _tRunCommandParameters (\s a -> s {_tRunCommandParameters = a})

-- | Contains the HTTP parameters to use when the target is a API Gateway REST endpoint. If you specify an API Gateway REST API as a target, you can use this parameter to specify headers, path parameter, query string keys/values as part of your target invoking request.
tHTTPParameters :: Lens' Target (Maybe HTTPParameters)
tHTTPParameters = lens _tHTTPParameters (\s a -> s {_tHTTPParameters = a})

-- | The custom parameter you can use to control the shard assignment, when the target is a Kinesis data stream. If you do not include this parameter, the default is to use the @eventId@ as the partition key.
tKinesisParameters :: Lens' Target (Maybe KinesisParameters)
tKinesisParameters = lens _tKinesisParameters (\s a -> s {_tKinesisParameters = a})

-- | Settings to enable you to provide custom input to a target based on certain event data. You can extract one or more key-value pairs from the event and then use that data to send customized input to the target.
tInputTransformer :: Lens' Target (Maybe InputTransformer)
tInputTransformer = lens _tInputTransformer (\s a -> s {_tInputTransformer = a})

-- | The @DeadLetterConfig@ that defines the target queue to send dead-letter queue events to.
tDeadLetterConfig :: Lens' Target (Maybe DeadLetterConfig)
tDeadLetterConfig = lens _tDeadLetterConfig (\s a -> s {_tDeadLetterConfig = a})

-- | Contains the message group ID to use when the target is a FIFO queue. If you specify an SQS FIFO queue as a target, the queue must have content-based deduplication enabled.
tSqsParameters :: Lens' Target (Maybe SqsParameters)
tSqsParameters = lens _tSqsParameters (\s a -> s {_tSqsParameters = a})

-- | Valid JSON text passed to the target. In this case, nothing from the event itself is passed to the target. For more information, see <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format> .
tInput :: Lens' Target (Maybe Text)
tInput = lens _tInput (\s a -> s {_tInput = a})

-- | If the event target is an AWS Batch job, this contains the job definition, job name, and other parameters. For more information, see <https://docs.aws.amazon.com/batch/latest/userguide/jobs.html Jobs> in the /AWS Batch User Guide/ .
tBatchParameters :: Lens' Target (Maybe BatchParameters)
tBatchParameters = lens _tBatchParameters (\s a -> s {_tBatchParameters = a})

-- | Contains the Redshift Data API parameters to use when the target is a Redshift cluster. If you specify a Redshift Cluster as a Target, you can use this to specify parameters to invoke the Redshift Data API ExecuteStatement based on EventBridge events.
tRedshiftDataParameters :: Lens' Target (Maybe RedshiftDataParameters)
tRedshiftDataParameters = lens _tRedshiftDataParameters (\s a -> s {_tRedshiftDataParameters = a})

-- | Contains the Amazon ECS task definition and task count to be used, if the event target is an Amazon ECS task. For more information about Amazon ECS tasks, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Task Definitions > in the /Amazon EC2 Container Service Developer Guide/ .
tEcsParameters :: Lens' Target (Maybe EcsParameters)
tEcsParameters = lens _tEcsParameters (\s a -> s {_tEcsParameters = a})

-- | The @RetryPolicy@ object that contains the retry policy configuration to use for the dead-letter queue.
tRetryPolicy :: Lens' Target (Maybe RetryPolicy)
tRetryPolicy = lens _tRetryPolicy (\s a -> s {_tRetryPolicy = a})

-- | The value of the JSONPath that is used for extracting part of the matched event when passing it to the target. You must use JSON dot notation, not bracket notation. For more information about JSON paths, see <http://goessner.net/articles/JsonPath/ JSONPath> .
tInputPath :: Lens' Target (Maybe Text)
tInputPath = lens _tInputPath (\s a -> s {_tInputPath = a})

-- | The Amazon Resource Name (ARN) of the IAM role to be used for this target when the rule is triggered. If one rule triggers multiple targets, you can use a different IAM role for each target.
tRoleARN :: Lens' Target (Maybe Text)
tRoleARN = lens _tRoleARN (\s a -> s {_tRoleARN = a})

-- | The ID of the target.
tId :: Lens' Target Text
tId = lens _tId (\s a -> s {_tId = a})

-- | The Amazon Resource Name (ARN) of the target.
tARN :: Lens' Target Text
tARN = lens _tARN (\s a -> s {_tARN = a})

instance FromJSON Target where
  parseJSON =
    withObject
      "Target"
      ( \x ->
          Target'
            <$> (x .:? "RunCommandParameters")
            <*> (x .:? "HttpParameters")
            <*> (x .:? "KinesisParameters")
            <*> (x .:? "InputTransformer")
            <*> (x .:? "DeadLetterConfig")
            <*> (x .:? "SqsParameters")
            <*> (x .:? "Input")
            <*> (x .:? "BatchParameters")
            <*> (x .:? "RedshiftDataParameters")
            <*> (x .:? "EcsParameters")
            <*> (x .:? "RetryPolicy")
            <*> (x .:? "InputPath")
            <*> (x .:? "RoleArn")
            <*> (x .: "Id")
            <*> (x .: "Arn")
      )

instance Hashable Target

instance NFData Target

instance ToJSON Target where
  toJSON Target' {..} =
    object
      ( catMaybes
          [ ("RunCommandParameters" .=) <$> _tRunCommandParameters,
            ("HttpParameters" .=) <$> _tHTTPParameters,
            ("KinesisParameters" .=) <$> _tKinesisParameters,
            ("InputTransformer" .=) <$> _tInputTransformer,
            ("DeadLetterConfig" .=) <$> _tDeadLetterConfig,
            ("SqsParameters" .=) <$> _tSqsParameters,
            ("Input" .=) <$> _tInput,
            ("BatchParameters" .=) <$> _tBatchParameters,
            ("RedshiftDataParameters" .=) <$> _tRedshiftDataParameters,
            ("EcsParameters" .=) <$> _tEcsParameters,
            ("RetryPolicy" .=) <$> _tRetryPolicy,
            ("InputPath" .=) <$> _tInputPath,
            ("RoleArn" .=) <$> _tRoleARN,
            Just ("Id" .= _tId),
            Just ("Arn" .= _tARN)
          ]
      )
