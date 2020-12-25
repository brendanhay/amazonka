-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _KMSInvalidStateException,
    _KMSThrottlingException,
    _ExpiredIteratorException,
    _InvalidArgumentException,
    _KMSOptInRequired,
    _ProvisionedThroughputExceededException,
    _KMSNotFoundException,
    _ExpiredNextTokenException,
    _KMSDisabledException,
    _InternalFailureException,
    _ResourceNotFoundException,
    _KMSAccessDeniedException,
    _LimitExceededException,
    _ResourceInUseException,

    -- * ShardFilterType
    ShardFilterType (..),

    -- * SequenceNumber
    SequenceNumber (..),

    -- * Shard
    Shard (..),
    mkShard,
    sShardId,
    sHashKeyRange,
    sSequenceNumberRange,
    sAdjacentParentShardId,
    sParentShardId,

    -- * EncryptionType
    EncryptionType (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * ConsumerStatus
    ConsumerStatus (..),

    -- * StreamDescription
    StreamDescription (..),
    mkStreamDescription,
    sdStreamName,
    sdStreamARN,
    sdStreamStatus,
    sdShards,
    sdHasMoreShards,
    sdRetentionPeriodHours,
    sdStreamCreationTimestamp,
    sdEnhancedMonitoring,
    sdEncryptionType,
    sdKeyId,

    -- * PutRecordsResultEntry
    PutRecordsResultEntry (..),
    mkPutRecordsResultEntry,
    prreErrorCode,
    prreErrorMessage,
    prreSequenceNumber,
    prreShardId,

    -- * KeyId
    KeyId (..),

    -- * PutRecordsRequestEntry
    PutRecordsRequestEntry (..),
    mkPutRecordsRequestEntry,
    prreData,
    prrePartitionKey,
    prreExplicitHashKey,

    -- * PartitionKey
    PartitionKey (..),

    -- * ScalingType
    ScalingType (..),

    -- * StreamStatus
    StreamStatus (..),

    -- * ConsumerDescription
    ConsumerDescription (..),
    mkConsumerDescription,
    cdConsumerName,
    cdConsumerARN,
    cdConsumerStatus,
    cdConsumerCreationTimestamp,
    cdStreamARN,

    -- * ConsumerARN
    ConsumerARN (..),

    -- * ShardIterator
    ShardIterator (..),

    -- * TagValue
    TagValue (..),

    -- * StreamARN
    StreamARN (..),

    -- * ShardFilter
    ShardFilter (..),
    mkShardFilter,
    sfType,
    sfShardId,
    sfTimestamp,

    -- * NextToken
    NextToken (..),

    -- * SubscribeToShardEventStream
    SubscribeToShardEventStream (..),
    mkSubscribeToShardEventStream,
    stsesSubscribeToShardEvent,
    stsesInternalFailureException,
    stsesKMSAccessDeniedException,
    stsesKMSDisabledException,
    stsesKMSInvalidStateException,
    stsesKMSNotFoundException,
    stsesKMSOptInRequired,
    stsesKMSThrottlingException,
    stsesResourceInUseException,
    stsesResourceNotFoundException,

    -- * HashKeyRange
    HashKeyRange (..),
    mkHashKeyRange,
    hkrStartingHashKey,
    hkrEndingHashKey,

    -- * ErrorCode
    ErrorCode (..),

    -- * HashKey
    HashKey (..),

    -- * EnhancedMonitoringOutput
    EnhancedMonitoringOutput (..),
    mkEnhancedMonitoringOutput,
    emoCurrentShardLevelMetrics,
    emoDesiredShardLevelMetrics,
    emoStreamName,

    -- * TagKey
    TagKey (..),

    -- * Record
    Record (..),
    mkRecord,
    rSequenceNumber,
    rData,
    rPartitionKey,
    rApproximateArrivalTimestamp,
    rEncryptionType,

    -- * MetricsName
    MetricsName (..),

    -- * SequenceNumberRange
    SequenceNumberRange (..),
    mkSequenceNumberRange,
    snrStartingSequenceNumber,
    snrEndingSequenceNumber,

    -- * ErrorMessage
    ErrorMessage (..),

    -- * StreamName
    StreamName (..),

    -- * SubscribeToShardEvent
    SubscribeToShardEvent (..),
    mkSubscribeToShardEvent,
    stseRecords,
    stseContinuationSequenceNumber,
    stseMillisBehindLatest,
    stseChildShards,

    -- * ChildShard
    ChildShard (..),
    mkChildShard,
    csShardId,
    csParentShards,
    csHashKeyRange,

    -- * ShardIteratorType
    ShardIteratorType (..),

    -- * StreamDescriptionSummary
    StreamDescriptionSummary (..),
    mkStreamDescriptionSummary,
    sdsStreamName,
    sdsStreamARN,
    sdsStreamStatus,
    sdsRetentionPeriodHours,
    sdsStreamCreationTimestamp,
    sdsEnhancedMonitoring,
    sdsOpenShardCount,
    sdsConsumerCount,
    sdsEncryptionType,
    sdsKeyId,

    -- * Consumer
    Consumer (..),
    mkConsumer,
    cConsumerName,
    cConsumerARN,
    cConsumerStatus,
    cConsumerCreationTimestamp,

    -- * StartingPosition
    StartingPosition (..),
    mkStartingPosition,
    spType,
    spSequenceNumber,
    spTimestamp,

    -- * ConsumerName
    ConsumerName (..),

    -- * ShardId
    ShardId (..),

    -- * EnhancedMetrics
    EnhancedMetrics (..),
    mkEnhancedMetrics,
    emShardLevelMetrics,

    -- * Message
    Message (..),

    -- * AdjacentParentShardId
    AdjacentParentShardId (..),

    -- * ParentShardId
    ParentShardId (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * ExclusiveStartTagKey
    ExclusiveStartTagKey (..),

    -- * ExplicitHashKey
    ExplicitHashKey (..),

    -- * ShardToMerge
    ShardToMerge (..),

    -- * AdjacentShardToMerge
    AdjacentShardToMerge (..),

    -- * ExclusiveStartShardId
    ExclusiveStartShardId (..),

    -- * StartingHashKey
    StartingHashKey (..),

    -- * EndingHashKey
    EndingHashKey (..),

    -- * ShardToSplit
    ShardToSplit (..),

    -- * ExclusiveStartStreamName
    ExclusiveStartStreamName (..),
  )
where

import Network.AWS.Kinesis.Types.AdjacentParentShardId
import Network.AWS.Kinesis.Types.AdjacentShardToMerge
import Network.AWS.Kinesis.Types.ChildShard
import Network.AWS.Kinesis.Types.Consumer
import Network.AWS.Kinesis.Types.ConsumerARN
import Network.AWS.Kinesis.Types.ConsumerDescription
import Network.AWS.Kinesis.Types.ConsumerName
import Network.AWS.Kinesis.Types.ConsumerStatus
import Network.AWS.Kinesis.Types.EncryptionType
import Network.AWS.Kinesis.Types.EndingHashKey
import Network.AWS.Kinesis.Types.EnhancedMetrics
import Network.AWS.Kinesis.Types.EnhancedMonitoringOutput
import Network.AWS.Kinesis.Types.ErrorCode
import Network.AWS.Kinesis.Types.ErrorMessage
import Network.AWS.Kinesis.Types.ExclusiveStartShardId
import Network.AWS.Kinesis.Types.ExclusiveStartStreamName
import Network.AWS.Kinesis.Types.ExclusiveStartTagKey
import Network.AWS.Kinesis.Types.ExplicitHashKey
import Network.AWS.Kinesis.Types.HashKey
import Network.AWS.Kinesis.Types.HashKeyRange
import Network.AWS.Kinesis.Types.Key
import Network.AWS.Kinesis.Types.KeyId
import Network.AWS.Kinesis.Types.Message
import Network.AWS.Kinesis.Types.MetricsName
import Network.AWS.Kinesis.Types.NextToken
import Network.AWS.Kinesis.Types.ParentShardId
import Network.AWS.Kinesis.Types.PartitionKey
import Network.AWS.Kinesis.Types.PutRecordsRequestEntry
import Network.AWS.Kinesis.Types.PutRecordsResultEntry
import Network.AWS.Kinesis.Types.Record
import Network.AWS.Kinesis.Types.ScalingType
import Network.AWS.Kinesis.Types.SequenceNumber
import Network.AWS.Kinesis.Types.SequenceNumberRange
import Network.AWS.Kinesis.Types.Shard
import Network.AWS.Kinesis.Types.ShardFilter
import Network.AWS.Kinesis.Types.ShardFilterType
import Network.AWS.Kinesis.Types.ShardId
import Network.AWS.Kinesis.Types.ShardIterator
import Network.AWS.Kinesis.Types.ShardIteratorType
import Network.AWS.Kinesis.Types.ShardToMerge
import Network.AWS.Kinesis.Types.ShardToSplit
import Network.AWS.Kinesis.Types.StartingHashKey
import Network.AWS.Kinesis.Types.StartingPosition
import Network.AWS.Kinesis.Types.StreamARN
import Network.AWS.Kinesis.Types.StreamDescription
import Network.AWS.Kinesis.Types.StreamDescriptionSummary
import Network.AWS.Kinesis.Types.StreamName
import Network.AWS.Kinesis.Types.StreamStatus
import Network.AWS.Kinesis.Types.SubscribeToShardEvent
import Network.AWS.Kinesis.Types.SubscribeToShardEventStream
import Network.AWS.Kinesis.Types.Tag
import Network.AWS.Kinesis.Types.TagKey
import Network.AWS.Kinesis.Types.TagValue
import Network.AWS.Kinesis.Types.Value
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2013-12-02@ of the Amazon Kinesis SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Kinesis",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "kinesis",
      Core._svcVersion = "2013-12-02",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Kinesis",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Lens.has
          (Core.hasCode "LimitExceededException" Core.. Core.hasStatus 400)
          e =
        Core.Just "request_limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The request was rejected because the state of the specified resource isn't valid for this request. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
_KMSInvalidStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSInvalidStateException =
  Core._MatchServiceError
    mkServiceConfig
    "KMSInvalidStateException"
{-# DEPRECATED _KMSInvalidStateException "Use generic-lens or generic-optics instead." #-}

-- | The request was denied due to request throttling. For more information about throttling, see <https://docs.aws.amazon.com/kms/latest/developerguide/limits.html#requests-per-second Limits> in the /AWS Key Management Service Developer Guide/ .
_KMSThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSThrottlingException =
  Core._MatchServiceError mkServiceConfig "KMSThrottlingException"
{-# DEPRECATED _KMSThrottlingException "Use generic-lens or generic-optics instead." #-}

-- | The provided iterator exceeds the maximum age allowed.
_ExpiredIteratorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExpiredIteratorException =
  Core._MatchServiceError
    mkServiceConfig
    "ExpiredIteratorException"
{-# DEPRECATED _ExpiredIteratorException "Use generic-lens or generic-optics instead." #-}

-- | A specified parameter exceeds its restrictions, is not supported, or can't be used. For more information, see the returned message.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidArgumentException"
{-# DEPRECATED _InvalidArgumentException "Use generic-lens or generic-optics instead." #-}

-- | The AWS access key ID needs a subscription for the service.
_KMSOptInRequired :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSOptInRequired =
  Core._MatchServiceError mkServiceConfig "KMSOptInRequired"
{-# DEPRECATED _KMSOptInRequired "Use generic-lens or generic-optics instead." #-}

-- | The request rate for the stream is too high, or the requested data is too large for the available throughput. Reduce the frequency or size of your requests. For more information, see <https://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Streams Limits> in the /Amazon Kinesis Data Streams Developer Guide/ , and <https://docs.aws.amazon.com/general/latest/gr/api-retries.html Error Retries and Exponential Backoff in AWS> in the /AWS General Reference/ .
_ProvisionedThroughputExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ProvisionedThroughputExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "ProvisionedThroughputExceededException"
{-# DEPRECATED _ProvisionedThroughputExceededException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified entity or resource can't be found.
_KMSNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSNotFoundException =
  Core._MatchServiceError mkServiceConfig "KMSNotFoundException"
{-# DEPRECATED _KMSNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The pagination token passed to the operation is expired.
_ExpiredNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExpiredNextTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "ExpiredNextTokenException"
{-# DEPRECATED _ExpiredNextTokenException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified customer master key (CMK) isn't enabled.
_KMSDisabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSDisabledException =
  Core._MatchServiceError mkServiceConfig "KMSDisabledException"
{-# DEPRECATED _KMSDisabledException "Use generic-lens or generic-optics instead." #-}

-- | The processing of the request failed because of an unknown error, exception, or failure.
_InternalFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    mkServiceConfig
    "InternalFailureException"
{-# DEPRECATED _InternalFailureException "Use generic-lens or generic-optics instead." #-}

-- | The requested resource could not be found. The stream might not be specified correctly.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The ciphertext references a key that doesn't exist or that you don't have access to.
_KMSAccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSAccessDeniedException =
  Core._MatchServiceError
    mkServiceConfig
    "KMSAccessDeniedException"
{-# DEPRECATED _KMSAccessDeniedException "Use generic-lens or generic-optics instead." #-}

-- | The requested resource exceeds the maximum number allowed, or the number of concurrent stream requests exceeds the maximum number allowed.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The resource is not available for this operation. For successful operation, the resource must be in the @ACTIVE@ state.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError mkServiceConfig "ResourceInUseException"
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead." #-}
