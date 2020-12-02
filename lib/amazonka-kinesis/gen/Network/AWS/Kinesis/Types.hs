{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types
  ( -- * Service Configuration
    kinesis,

    -- * Errors
    _KMSInvalidStateException,
    _KMSThrottlingException,
    _KMSOptInRequired,
    _KMSNotFoundException,
    _KMSDisabledException,
    _InternalFailureException,
    _ResourceNotFoundException,
    _KMSAccessDeniedException,
    _ResourceInUseException,

    -- * ConsumerStatus
    ConsumerStatus (..),

    -- * EncryptionType
    EncryptionType (..),

    -- * MetricsName
    MetricsName (..),

    -- * ScalingType
    ScalingType (..),

    -- * ShardFilterType
    ShardFilterType (..),

    -- * ShardIteratorType
    ShardIteratorType (..),

    -- * StreamStatus
    StreamStatus (..),

    -- * ChildShard
    ChildShard,
    childShard,
    csShardId,
    csParentShards,
    csHashKeyRange,

    -- * Consumer
    Consumer,
    consumer,
    cConsumerName,
    cConsumerARN,
    cConsumerStatus,
    cConsumerCreationTimestamp,

    -- * ConsumerDescription
    ConsumerDescription,
    consumerDescription,
    cdConsumerName,
    cdConsumerARN,
    cdConsumerStatus,
    cdConsumerCreationTimestamp,
    cdStreamARN,

    -- * EnhancedMetrics
    EnhancedMetrics,
    enhancedMetrics,
    emShardLevelMetrics,

    -- * EnhancedMonitoringOutput
    EnhancedMonitoringOutput,
    enhancedMonitoringOutput,
    emoDesiredShardLevelMetrics,
    emoCurrentShardLevelMetrics,
    emoStreamName,

    -- * HashKeyRange
    HashKeyRange,
    hashKeyRange,
    hkrStartingHashKey,
    hkrEndingHashKey,

    -- * PutRecordsRequestEntry
    PutRecordsRequestEntry,
    putRecordsRequestEntry,
    prreExplicitHashKey,
    prreData,
    prrePartitionKey,

    -- * PutRecordsResultEntry
    PutRecordsResultEntry,
    putRecordsResultEntry,
    prreSequenceNumber,
    prreErrorCode,
    prreErrorMessage,
    prreShardId,

    -- * Record
    Record,
    record,
    rEncryptionType,
    rApproximateArrivalTimestamp,
    rSequenceNumber,
    rData,
    rPartitionKey,

    -- * SequenceNumberRange
    SequenceNumberRange,
    sequenceNumberRange,
    snrEndingSequenceNumber,
    snrStartingSequenceNumber,

    -- * Shard
    Shard,
    shard,
    sAdjacentParentShardId,
    sParentShardId,
    sShardId,
    sHashKeyRange,
    sSequenceNumberRange,

    -- * ShardFilter
    ShardFilter,
    shardFilter,
    sfTimestamp,
    sfShardId,
    sfType,

    -- * StartingPosition
    StartingPosition,
    startingPosition,
    spSequenceNumber,
    spTimestamp,
    spType,

    -- * StreamDescription
    StreamDescription,
    streamDescription,
    sdEncryptionType,
    sdKeyId,
    sdStreamName,
    sdStreamARN,
    sdStreamStatus,
    sdShards,
    sdHasMoreShards,
    sdRetentionPeriodHours,
    sdStreamCreationTimestamp,
    sdEnhancedMonitoring,

    -- * StreamDescriptionSummary
    StreamDescriptionSummary,
    streamDescriptionSummary,
    sdsEncryptionType,
    sdsKeyId,
    sdsConsumerCount,
    sdsStreamName,
    sdsStreamARN,
    sdsStreamStatus,
    sdsRetentionPeriodHours,
    sdsStreamCreationTimestamp,
    sdsEnhancedMonitoring,
    sdsOpenShardCount,

    -- * SubscribeToShardEvent
    SubscribeToShardEvent,
    subscribeToShardEvent,
    stseChildShards,
    stseRecords,
    stseContinuationSequenceNumber,
    stseMillisBehindLatest,

    -- * SubscribeToShardEventStream
    SubscribeToShardEventStream,
    subscribeToShardEventStream,
    stsesKMSInvalidStateException,
    stsesKMSThrottlingException,
    stsesKMSOptInRequired,
    stsesKMSNotFoundException,
    stsesKMSDisabledException,
    stsesInternalFailureException,
    stsesResourceNotFoundException,
    stsesKMSAccessDeniedException,
    stsesResourceInUseException,
    stsesSubscribeToShardEvent,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,
  )
where

import Network.AWS.Kinesis.Types.ChildShard
import Network.AWS.Kinesis.Types.Consumer
import Network.AWS.Kinesis.Types.ConsumerDescription
import Network.AWS.Kinesis.Types.ConsumerStatus
import Network.AWS.Kinesis.Types.EncryptionType
import Network.AWS.Kinesis.Types.EnhancedMetrics
import Network.AWS.Kinesis.Types.EnhancedMonitoringOutput
import Network.AWS.Kinesis.Types.HashKeyRange
import Network.AWS.Kinesis.Types.MetricsName
import Network.AWS.Kinesis.Types.PutRecordsRequestEntry
import Network.AWS.Kinesis.Types.PutRecordsResultEntry
import Network.AWS.Kinesis.Types.Record
import Network.AWS.Kinesis.Types.ScalingType
import Network.AWS.Kinesis.Types.SequenceNumberRange
import Network.AWS.Kinesis.Types.Shard
import Network.AWS.Kinesis.Types.ShardFilter
import Network.AWS.Kinesis.Types.ShardFilterType
import Network.AWS.Kinesis.Types.ShardIteratorType
import Network.AWS.Kinesis.Types.StartingPosition
import Network.AWS.Kinesis.Types.StreamDescription
import Network.AWS.Kinesis.Types.StreamDescriptionSummary
import Network.AWS.Kinesis.Types.StreamStatus
import Network.AWS.Kinesis.Types.SubscribeToShardEvent
import Network.AWS.Kinesis.Types.SubscribeToShardEventStream
import Network.AWS.Kinesis.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2013-12-02@ of the Amazon Kinesis SDK configuration.
kinesis :: Service
kinesis =
  Service
    { _svcAbbrev = "Kinesis",
      _svcSigner = v4,
      _svcPrefix = "kinesis",
      _svcVersion = "2013-12-02",
      _svcEndpoint = defaultEndpoint kinesis,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Kinesis",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasCode "LimitExceededException" . hasStatus 400) e =
        Just "request_limit_exceeded"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The request was rejected because the state of the specified resource isn't valid for this request. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
_KMSInvalidStateException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSInvalidStateException =
  _MatchServiceError kinesis "KMSInvalidStateException"

-- | The request was denied due to request throttling. For more information about throttling, see <https://docs.aws.amazon.com/kms/latest/developerguide/limits.html#requests-per-second Limits> in the /AWS Key Management Service Developer Guide/ .
_KMSThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSThrottlingException =
  _MatchServiceError kinesis "KMSThrottlingException"

-- | The AWS access key ID needs a subscription for the service.
_KMSOptInRequired :: AsError a => Getting (First ServiceError) a ServiceError
_KMSOptInRequired = _MatchServiceError kinesis "KMSOptInRequired"

-- | The request was rejected because the specified entity or resource can't be found.
_KMSNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSNotFoundException =
  _MatchServiceError kinesis "KMSNotFoundException"

-- | The request was rejected because the specified customer master key (CMK) isn't enabled.
_KMSDisabledException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSDisabledException =
  _MatchServiceError kinesis "KMSDisabledException"

-- | The processing of the request failed because of an unknown error, exception, or failure.
_InternalFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalFailureException =
  _MatchServiceError kinesis "InternalFailureException"

-- | The requested resource could not be found. The stream might not be specified correctly.
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError kinesis "ResourceNotFoundException"

-- | The ciphertext references a key that doesn't exist or that you don't have access to.
_KMSAccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSAccessDeniedException =
  _MatchServiceError kinesis "KMSAccessDeniedException"

-- | The resource is not available for this operation. For successful operation, the resource must be in the @ACTIVE@ state.
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException =
  _MatchServiceError kinesis "ResourceInUseException"
