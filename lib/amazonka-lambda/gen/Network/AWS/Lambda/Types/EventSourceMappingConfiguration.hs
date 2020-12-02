{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.EventSourceMappingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.EventSourceMappingConfiguration where

import Network.AWS.Lambda.Types.DestinationConfig
import Network.AWS.Lambda.Types.EventSourcePosition
import Network.AWS.Lambda.Types.SourceAccessConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A mapping between an AWS resource and an AWS Lambda function. See 'CreateEventSourceMapping' for details.
--
--
--
-- /See:/ 'eventSourceMappingConfiguration' smart constructor.
data EventSourceMappingConfiguration = EventSourceMappingConfiguration'
  { _esmcEventSourceARN ::
      !(Maybe Text),
    _esmcState :: !(Maybe Text),
    _esmcStartingPositionTimestamp ::
      !(Maybe POSIX),
    _esmcFunctionARN ::
      !(Maybe Text),
    _esmcTopics ::
      !(Maybe (List1 Text)),
    _esmcQueues ::
      !(Maybe (List1 Text)),
    _esmcBisectBatchOnFunctionError ::
      !(Maybe Bool),
    _esmcUUId :: !(Maybe Text),
    _esmcParallelizationFactor ::
      !(Maybe Nat),
    _esmcLastProcessingResult ::
      !(Maybe Text),
    _esmcMaximumRetryAttempts ::
      !(Maybe Int),
    _esmcBatchSize ::
      !(Maybe Nat),
    _esmcStateTransitionReason ::
      !(Maybe Text),
    _esmcMaximumBatchingWindowInSeconds ::
      !(Maybe Nat),
    _esmcSourceAccessConfigurations ::
      !( Maybe
           ( List1
               SourceAccessConfiguration
           )
       ),
    _esmcMaximumRecordAgeInSeconds ::
      !(Maybe Int),
    _esmcLastModified ::
      !(Maybe POSIX),
    _esmcDestinationConfig ::
      !(Maybe DestinationConfig),
    _esmcStartingPosition ::
      !( Maybe
           EventSourcePosition
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventSourceMappingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esmcEventSourceARN' - The Amazon Resource Name (ARN) of the event source.
--
-- * 'esmcState' - The state of the event source mapping. It can be one of the following: @Creating@ , @Enabling@ , @Enabled@ , @Disabling@ , @Disabled@ , @Updating@ , or @Deleting@ .
--
-- * 'esmcStartingPositionTimestamp' - With @StartingPosition@ set to @AT_TIMESTAMP@ , the time from which to start reading.
--
-- * 'esmcFunctionARN' - The ARN of the Lambda function.
--
-- * 'esmcTopics' - (MSK) The name of the Kafka topic to consume.
--
-- * 'esmcQueues' - (MQ) The name of the Amazon MQ broker destination queue to consume.
--
-- * 'esmcBisectBatchOnFunctionError' - (Streams) If the function returns an error, split the batch in two and retry. The default value is false.
--
-- * 'esmcUUId' - The identifier of the event source mapping.
--
-- * 'esmcParallelizationFactor' - (Streams) The number of batches to process from each shard concurrently. The default value is 1.
--
-- * 'esmcLastProcessingResult' - The result of the last AWS Lambda invocation of your Lambda function.
--
-- * 'esmcMaximumRetryAttempts' - (Streams) Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records are retried until the record expires.
--
-- * 'esmcBatchSize' - The maximum number of items to retrieve in a single batch.
--
-- * 'esmcStateTransitionReason' - Indicates whether the last change to the event source mapping was made by a user, or by the Lambda service.
--
-- * 'esmcMaximumBatchingWindowInSeconds' - (Streams) The maximum amount of time to gather records before invoking the function, in seconds. The default value is zero.
--
-- * 'esmcSourceAccessConfigurations' - (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@  To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@  The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
--
-- * 'esmcMaximumRecordAgeInSeconds' - (Streams) Discard records older than the specified age. The default value is infinite (-1). When set to infinite (-1), failed records are retried until the record expires.
--
-- * 'esmcLastModified' - The date that the event source mapping was last updated, or its state changed.
--
-- * 'esmcDestinationConfig' - (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
--
-- * 'esmcStartingPosition' - The position in a stream from which to start reading. Required for Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources. @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
eventSourceMappingConfiguration ::
  EventSourceMappingConfiguration
eventSourceMappingConfiguration =
  EventSourceMappingConfiguration'
    { _esmcEventSourceARN = Nothing,
      _esmcState = Nothing,
      _esmcStartingPositionTimestamp = Nothing,
      _esmcFunctionARN = Nothing,
      _esmcTopics = Nothing,
      _esmcQueues = Nothing,
      _esmcBisectBatchOnFunctionError = Nothing,
      _esmcUUId = Nothing,
      _esmcParallelizationFactor = Nothing,
      _esmcLastProcessingResult = Nothing,
      _esmcMaximumRetryAttempts = Nothing,
      _esmcBatchSize = Nothing,
      _esmcStateTransitionReason = Nothing,
      _esmcMaximumBatchingWindowInSeconds = Nothing,
      _esmcSourceAccessConfigurations = Nothing,
      _esmcMaximumRecordAgeInSeconds = Nothing,
      _esmcLastModified = Nothing,
      _esmcDestinationConfig = Nothing,
      _esmcStartingPosition = Nothing
    }

-- | The Amazon Resource Name (ARN) of the event source.
esmcEventSourceARN :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcEventSourceARN = lens _esmcEventSourceARN (\s a -> s {_esmcEventSourceARN = a})

-- | The state of the event source mapping. It can be one of the following: @Creating@ , @Enabling@ , @Enabled@ , @Disabling@ , @Disabled@ , @Updating@ , or @Deleting@ .
esmcState :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcState = lens _esmcState (\s a -> s {_esmcState = a})

-- | With @StartingPosition@ set to @AT_TIMESTAMP@ , the time from which to start reading.
esmcStartingPositionTimestamp :: Lens' EventSourceMappingConfiguration (Maybe UTCTime)
esmcStartingPositionTimestamp = lens _esmcStartingPositionTimestamp (\s a -> s {_esmcStartingPositionTimestamp = a}) . mapping _Time

-- | The ARN of the Lambda function.
esmcFunctionARN :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcFunctionARN = lens _esmcFunctionARN (\s a -> s {_esmcFunctionARN = a})

-- | (MSK) The name of the Kafka topic to consume.
esmcTopics :: Lens' EventSourceMappingConfiguration (Maybe (NonEmpty Text))
esmcTopics = lens _esmcTopics (\s a -> s {_esmcTopics = a}) . mapping _List1

-- | (MQ) The name of the Amazon MQ broker destination queue to consume.
esmcQueues :: Lens' EventSourceMappingConfiguration (Maybe (NonEmpty Text))
esmcQueues = lens _esmcQueues (\s a -> s {_esmcQueues = a}) . mapping _List1

-- | (Streams) If the function returns an error, split the batch in two and retry. The default value is false.
esmcBisectBatchOnFunctionError :: Lens' EventSourceMappingConfiguration (Maybe Bool)
esmcBisectBatchOnFunctionError = lens _esmcBisectBatchOnFunctionError (\s a -> s {_esmcBisectBatchOnFunctionError = a})

-- | The identifier of the event source mapping.
esmcUUId :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcUUId = lens _esmcUUId (\s a -> s {_esmcUUId = a})

-- | (Streams) The number of batches to process from each shard concurrently. The default value is 1.
esmcParallelizationFactor :: Lens' EventSourceMappingConfiguration (Maybe Natural)
esmcParallelizationFactor = lens _esmcParallelizationFactor (\s a -> s {_esmcParallelizationFactor = a}) . mapping _Nat

-- | The result of the last AWS Lambda invocation of your Lambda function.
esmcLastProcessingResult :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcLastProcessingResult = lens _esmcLastProcessingResult (\s a -> s {_esmcLastProcessingResult = a})

-- | (Streams) Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records are retried until the record expires.
esmcMaximumRetryAttempts :: Lens' EventSourceMappingConfiguration (Maybe Int)
esmcMaximumRetryAttempts = lens _esmcMaximumRetryAttempts (\s a -> s {_esmcMaximumRetryAttempts = a})

-- | The maximum number of items to retrieve in a single batch.
esmcBatchSize :: Lens' EventSourceMappingConfiguration (Maybe Natural)
esmcBatchSize = lens _esmcBatchSize (\s a -> s {_esmcBatchSize = a}) . mapping _Nat

-- | Indicates whether the last change to the event source mapping was made by a user, or by the Lambda service.
esmcStateTransitionReason :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcStateTransitionReason = lens _esmcStateTransitionReason (\s a -> s {_esmcStateTransitionReason = a})

-- | (Streams) The maximum amount of time to gather records before invoking the function, in seconds. The default value is zero.
esmcMaximumBatchingWindowInSeconds :: Lens' EventSourceMappingConfiguration (Maybe Natural)
esmcMaximumBatchingWindowInSeconds = lens _esmcMaximumBatchingWindowInSeconds (\s a -> s {_esmcMaximumBatchingWindowInSeconds = a}) . mapping _Nat

-- | (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@  To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@  The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
esmcSourceAccessConfigurations :: Lens' EventSourceMappingConfiguration (Maybe (NonEmpty SourceAccessConfiguration))
esmcSourceAccessConfigurations = lens _esmcSourceAccessConfigurations (\s a -> s {_esmcSourceAccessConfigurations = a}) . mapping _List1

-- | (Streams) Discard records older than the specified age. The default value is infinite (-1). When set to infinite (-1), failed records are retried until the record expires.
esmcMaximumRecordAgeInSeconds :: Lens' EventSourceMappingConfiguration (Maybe Int)
esmcMaximumRecordAgeInSeconds = lens _esmcMaximumRecordAgeInSeconds (\s a -> s {_esmcMaximumRecordAgeInSeconds = a})

-- | The date that the event source mapping was last updated, or its state changed.
esmcLastModified :: Lens' EventSourceMappingConfiguration (Maybe UTCTime)
esmcLastModified = lens _esmcLastModified (\s a -> s {_esmcLastModified = a}) . mapping _Time

-- | (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
esmcDestinationConfig :: Lens' EventSourceMappingConfiguration (Maybe DestinationConfig)
esmcDestinationConfig = lens _esmcDestinationConfig (\s a -> s {_esmcDestinationConfig = a})

-- | The position in a stream from which to start reading. Required for Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources. @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
esmcStartingPosition :: Lens' EventSourceMappingConfiguration (Maybe EventSourcePosition)
esmcStartingPosition = lens _esmcStartingPosition (\s a -> s {_esmcStartingPosition = a})

instance FromJSON EventSourceMappingConfiguration where
  parseJSON =
    withObject
      "EventSourceMappingConfiguration"
      ( \x ->
          EventSourceMappingConfiguration'
            <$> (x .:? "EventSourceArn")
            <*> (x .:? "State")
            <*> (x .:? "StartingPositionTimestamp")
            <*> (x .:? "FunctionArn")
            <*> (x .:? "Topics")
            <*> (x .:? "Queues")
            <*> (x .:? "BisectBatchOnFunctionError")
            <*> (x .:? "UUID")
            <*> (x .:? "ParallelizationFactor")
            <*> (x .:? "LastProcessingResult")
            <*> (x .:? "MaximumRetryAttempts")
            <*> (x .:? "BatchSize")
            <*> (x .:? "StateTransitionReason")
            <*> (x .:? "MaximumBatchingWindowInSeconds")
            <*> (x .:? "SourceAccessConfigurations")
            <*> (x .:? "MaximumRecordAgeInSeconds")
            <*> (x .:? "LastModified")
            <*> (x .:? "DestinationConfig")
            <*> (x .:? "StartingPosition")
      )

instance Hashable EventSourceMappingConfiguration

instance NFData EventSourceMappingConfiguration
