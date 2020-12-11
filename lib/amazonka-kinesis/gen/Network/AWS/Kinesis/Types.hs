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
    kinesisService,

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
    ChildShard (..),
    mkChildShard,
    csShardId,
    csParentShards,
    csHashKeyRange,

    -- * Consumer
    Consumer (..),
    mkConsumer,
    cConsumerName,
    cConsumerARN,
    cConsumerStatus,
    cConsumerCreationTimestamp,

    -- * ConsumerDescription
    ConsumerDescription (..),
    mkConsumerDescription,
    cdConsumerName,
    cdConsumerARN,
    cdConsumerStatus,
    cdConsumerCreationTimestamp,
    cdStreamARN,

    -- * EnhancedMetrics
    EnhancedMetrics (..),
    mkEnhancedMetrics,
    emShardLevelMetrics,

    -- * EnhancedMonitoringOutput
    EnhancedMonitoringOutput (..),
    mkEnhancedMonitoringOutput,
    emoDesiredShardLevelMetrics,
    emoCurrentShardLevelMetrics,
    emoStreamName,

    -- * HashKeyRange
    HashKeyRange (..),
    mkHashKeyRange,
    hkrStartingHashKey,
    hkrEndingHashKey,

    -- * PutRecordsRequestEntry
    PutRecordsRequestEntry (..),
    mkPutRecordsRequestEntry,
    prreExplicitHashKey,
    prreData,
    prrePartitionKey,

    -- * PutRecordsResultEntry
    PutRecordsResultEntry (..),
    mkPutRecordsResultEntry,
    prreSequenceNumber,
    prreErrorCode,
    prreErrorMessage,
    prreShardId,

    -- * Record
    Record (..),
    mkRecord,
    rEncryptionType,
    rApproximateArrivalTimestamp,
    rSequenceNumber,
    rData,
    rPartitionKey,

    -- * SequenceNumberRange
    SequenceNumberRange (..),
    mkSequenceNumberRange,
    snrEndingSequenceNumber,
    snrStartingSequenceNumber,

    -- * Shard
    Shard (..),
    mkShard,
    sAdjacentParentShardId,
    sParentShardId,
    sShardId,
    sHashKeyRange,
    sSequenceNumberRange,

    -- * ShardFilter
    ShardFilter (..),
    mkShardFilter,
    sfTimestamp,
    sfShardId,
    sfType,

    -- * StartingPosition
    StartingPosition (..),
    mkStartingPosition,
    spSequenceNumber,
    spTimestamp,
    spType,

    -- * StreamDescription
    StreamDescription (..),
    mkStreamDescription,
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
    StreamDescriptionSummary (..),
    mkStreamDescriptionSummary,
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
    SubscribeToShardEvent (..),
    mkSubscribeToShardEvent,
    stseChildShards,
    stseRecords,
    stseContinuationSequenceNumber,
    stseMillisBehindLatest,

    -- * SubscribeToShardEventStream
    SubscribeToShardEventStream (..),
    mkSubscribeToShardEventStream,
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
    Tag (..),
    mkTag,
    tValue,
    tKey,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2013-12-02@ of the Amazon Kinesis SDK configuration.
kinesisService :: Lude.Service
kinesisService =
  Lude.Service
    { Lude._svcAbbrev = "Kinesis",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "kinesis",
      Lude._svcVersion = "2013-12-02",
      Lude._svcEndpoint = Lude.defaultEndpoint kinesisService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Kinesis",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has
          (Lude.hasCode "LimitExceededException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "request_limit_exceeded"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing

-- | The request was rejected because the state of the specified resource isn't valid for this request. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
_KMSInvalidStateException :: Lude.AsError a => Lens.Getting (Lude.First Lude.ServiceError) a Lude.ServiceError
_KMSInvalidStateException =
  Lude._MatchServiceError kinesisService "KMSInvalidStateException"
{-# DEPRECATED _KMSInvalidStateException "Use generic-lens or generic-optics instead." #-}

-- | The request was denied due to request throttling. For more information about throttling, see <https://docs.aws.amazon.com/kms/latest/developerguide/limits.html#requests-per-second Limits> in the /AWS Key Management Service Developer Guide/ .
_KMSThrottlingException :: Lude.AsError a => Lens.Getting (Lude.First Lude.ServiceError) a Lude.ServiceError
_KMSThrottlingException =
  Lude._MatchServiceError kinesisService "KMSThrottlingException"
{-# DEPRECATED _KMSThrottlingException "Use generic-lens or generic-optics instead." #-}

-- | The AWS access key ID needs a subscription for the service.
_KMSOptInRequired :: Lude.AsError a => Lens.Getting (Lude.First Lude.ServiceError) a Lude.ServiceError
_KMSOptInRequired =
  Lude._MatchServiceError kinesisService "KMSOptInRequired"
{-# DEPRECATED _KMSOptInRequired "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified entity or resource can't be found.
_KMSNotFoundException :: Lude.AsError a => Lens.Getting (Lude.First Lude.ServiceError) a Lude.ServiceError
_KMSNotFoundException =
  Lude._MatchServiceError kinesisService "KMSNotFoundException"
{-# DEPRECATED _KMSNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the specified customer master key (CMK) isn't enabled.
_KMSDisabledException :: Lude.AsError a => Lens.Getting (Lude.First Lude.ServiceError) a Lude.ServiceError
_KMSDisabledException =
  Lude._MatchServiceError kinesisService "KMSDisabledException"
{-# DEPRECATED _KMSDisabledException "Use generic-lens or generic-optics instead." #-}

-- | The processing of the request failed because of an unknown error, exception, or failure.
_InternalFailureException :: Lude.AsError a => Lens.Getting (Lude.First Lude.ServiceError) a Lude.ServiceError
_InternalFailureException =
  Lude._MatchServiceError kinesisService "InternalFailureException"
{-# DEPRECATED _InternalFailureException "Use generic-lens or generic-optics instead." #-}

-- | The requested resource could not be found. The stream might not be specified correctly.
_ResourceNotFoundException :: Lude.AsError a => Lens.Getting (Lude.First Lude.ServiceError) a Lude.ServiceError
_ResourceNotFoundException =
  Lude._MatchServiceError
    kinesisService
    "ResourceNotFoundException"
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The ciphertext references a key that doesn't exist or that you don't have access to.
_KMSAccessDeniedException :: Lude.AsError a => Lens.Getting (Lude.First Lude.ServiceError) a Lude.ServiceError
_KMSAccessDeniedException =
  Lude._MatchServiceError kinesisService "KMSAccessDeniedException"
{-# DEPRECATED _KMSAccessDeniedException "Use generic-lens or generic-optics instead." #-}

-- | The resource is not available for this operation. For successful operation, the resource must be in the @ACTIVE@ state.
_ResourceInUseException :: Lude.AsError a => Lens.Getting (Lude.First Lude.ServiceError) a Lude.ServiceError
_ResourceInUseException =
  Lude._MatchServiceError kinesisService "ResourceInUseException"
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead." #-}
