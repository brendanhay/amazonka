{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _KMSThrottlingException,
    _ExpiredNextTokenException,
    _KMSInvalidStateException,
    _KMSNotFoundException,
    _KMSOptInRequired,
    _ExpiredIteratorException,
    _ResourceInUseException,
    _LimitExceededException,
    _KMSAccessDeniedException,
    _ProvisionedThroughputExceededException,
    _ResourceNotFoundException,
    _InternalFailureException,
    _InvalidArgumentException,
    _KMSDisabledException,

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
    newChildShard,
    childShard_shardId,
    childShard_parentShards,
    childShard_hashKeyRange,

    -- * Consumer
    Consumer (..),
    newConsumer,
    consumer_consumerName,
    consumer_consumerARN,
    consumer_consumerStatus,
    consumer_consumerCreationTimestamp,

    -- * ConsumerDescription
    ConsumerDescription (..),
    newConsumerDescription,
    consumerDescription_consumerName,
    consumerDescription_consumerARN,
    consumerDescription_consumerStatus,
    consumerDescription_consumerCreationTimestamp,
    consumerDescription_streamARN,

    -- * EnhancedMetrics
    EnhancedMetrics (..),
    newEnhancedMetrics,
    enhancedMetrics_shardLevelMetrics,

    -- * EnhancedMonitoringOutput
    EnhancedMonitoringOutput (..),
    newEnhancedMonitoringOutput,
    enhancedMonitoringOutput_currentShardLevelMetrics,
    enhancedMonitoringOutput_streamName,
    enhancedMonitoringOutput_desiredShardLevelMetrics,

    -- * HashKeyRange
    HashKeyRange (..),
    newHashKeyRange,
    hashKeyRange_startingHashKey,
    hashKeyRange_endingHashKey,

    -- * PutRecordsRequestEntry
    PutRecordsRequestEntry (..),
    newPutRecordsRequestEntry,
    putRecordsRequestEntry_explicitHashKey,
    putRecordsRequestEntry_data,
    putRecordsRequestEntry_partitionKey,

    -- * PutRecordsResultEntry
    PutRecordsResultEntry (..),
    newPutRecordsResultEntry,
    putRecordsResultEntry_sequenceNumber,
    putRecordsResultEntry_shardId,
    putRecordsResultEntry_errorMessage,
    putRecordsResultEntry_errorCode,

    -- * Record
    Record (..),
    newRecord,
    record_encryptionType,
    record_approximateArrivalTimestamp,
    record_sequenceNumber,
    record_data,
    record_partitionKey,

    -- * SequenceNumberRange
    SequenceNumberRange (..),
    newSequenceNumberRange,
    sequenceNumberRange_endingSequenceNumber,
    sequenceNumberRange_startingSequenceNumber,

    -- * Shard
    Shard (..),
    newShard,
    shard_adjacentParentShardId,
    shard_parentShardId,
    shard_shardId,
    shard_hashKeyRange,
    shard_sequenceNumberRange,

    -- * ShardFilter
    ShardFilter (..),
    newShardFilter,
    shardFilter_shardId,
    shardFilter_timestamp,
    shardFilter_type,

    -- * StartingPosition
    StartingPosition (..),
    newStartingPosition,
    startingPosition_sequenceNumber,
    startingPosition_timestamp,
    startingPosition_type,

    -- * StreamDescription
    StreamDescription (..),
    newStreamDescription,
    streamDescription_encryptionType,
    streamDescription_keyId,
    streamDescription_streamName,
    streamDescription_streamARN,
    streamDescription_streamStatus,
    streamDescription_shards,
    streamDescription_hasMoreShards,
    streamDescription_retentionPeriodHours,
    streamDescription_streamCreationTimestamp,
    streamDescription_enhancedMonitoring,

    -- * StreamDescriptionSummary
    StreamDescriptionSummary (..),
    newStreamDescriptionSummary,
    streamDescriptionSummary_encryptionType,
    streamDescriptionSummary_consumerCount,
    streamDescriptionSummary_keyId,
    streamDescriptionSummary_streamName,
    streamDescriptionSummary_streamARN,
    streamDescriptionSummary_streamStatus,
    streamDescriptionSummary_retentionPeriodHours,
    streamDescriptionSummary_streamCreationTimestamp,
    streamDescriptionSummary_enhancedMonitoring,
    streamDescriptionSummary_openShardCount,

    -- * SubscribeToShardEvent
    SubscribeToShardEvent (..),
    newSubscribeToShardEvent,
    subscribeToShardEvent_childShards,
    subscribeToShardEvent_records,
    subscribeToShardEvent_continuationSequenceNumber,
    subscribeToShardEvent_millisBehindLatest,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,
  )
where

import qualified Network.AWS.Core as Core
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
import Network.AWS.Kinesis.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2013-12-02@ of the Amazon Kinesis SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Kinesis",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "kinesis",
      Core._serviceSigningName = "kinesis",
      Core._serviceVersion = "2013-12-02",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Kinesis",
      Core._serviceRetry = retry
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
          ( Core.hasCode "LimitExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_limit_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The request was denied due to request throttling. For more information
-- about throttling, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/limits.html#requests-per-second Limits>
-- in the /AWS Key Management Service Developer Guide/.
_KMSThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSThrottlingException =
  Core._MatchServiceError
    defaultService
    "KMSThrottlingException"

-- | The pagination token passed to the operation is expired.
_ExpiredNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExpiredNextTokenException =
  Core._MatchServiceError
    defaultService
    "ExpiredNextTokenException"

-- | The request was rejected because the state of the specified resource
-- isn\'t valid for this request. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
_KMSInvalidStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSInvalidStateException =
  Core._MatchServiceError
    defaultService
    "KMSInvalidStateException"

-- | The request was rejected because the specified entity or resource can\'t
-- be found.
_KMSNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSNotFoundException =
  Core._MatchServiceError
    defaultService
    "KMSNotFoundException"

-- | The AWS access key ID needs a subscription for the service.
_KMSOptInRequired :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSOptInRequired =
  Core._MatchServiceError
    defaultService
    "KMSOptInRequired"

-- | The provided iterator exceeds the maximum age allowed.
_ExpiredIteratorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExpiredIteratorException =
  Core._MatchServiceError
    defaultService
    "ExpiredIteratorException"

-- | The resource is not available for this operation. For successful
-- operation, the resource must be in the @ACTIVE@ state.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The requested resource exceeds the maximum number allowed, or the number
-- of concurrent stream requests exceeds the maximum number allowed.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The ciphertext references a key that doesn\'t exist or that you don\'t
-- have access to.
_KMSAccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSAccessDeniedException =
  Core._MatchServiceError
    defaultService
    "KMSAccessDeniedException"

-- | The request rate for the stream is too high, or the requested data is
-- too large for the available throughput. Reduce the frequency or size of
-- your requests. For more information, see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Streams Limits>
-- in the /Amazon Kinesis Data Streams Developer Guide/, and
-- <https://docs.aws.amazon.com/general/latest/gr/api-retries.html Error Retries and Exponential Backoff in AWS>
-- in the /AWS General Reference/.
_ProvisionedThroughputExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ProvisionedThroughputExceededException =
  Core._MatchServiceError
    defaultService
    "ProvisionedThroughputExceededException"

-- | The requested resource could not be found. The stream might not be
-- specified correctly.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The processing of the request failed because of an unknown error,
-- exception, or failure.
_InternalFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"

-- | A specified parameter exceeds its restrictions, is not supported, or
-- can\'t be used. For more information, see the returned message.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"

-- | The request was rejected because the specified customer master key (CMK)
-- isn\'t enabled.
_KMSDisabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSDisabledException =
  Core._MatchServiceError
    defaultService
    "KMSDisabledException"
