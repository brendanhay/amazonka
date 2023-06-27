{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Kinesis.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ExpiredIteratorException,
    _ExpiredNextTokenException,
    _InternalFailureException,
    _InvalidArgumentException,
    _KMSAccessDeniedException,
    _KMSDisabledException,
    _KMSInvalidStateException,
    _KMSNotFoundException,
    _KMSOptInRequired,
    _KMSThrottlingException,
    _LimitExceededException,
    _ProvisionedThroughputExceededException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _ValidationException,

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

    -- * StreamMode
    StreamMode (..),

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
    enhancedMonitoringOutput_desiredShardLevelMetrics,
    enhancedMonitoringOutput_streamARN,
    enhancedMonitoringOutput_streamName,

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
    putRecordsResultEntry_errorCode,
    putRecordsResultEntry_errorMessage,
    putRecordsResultEntry_sequenceNumber,
    putRecordsResultEntry_shardId,

    -- * Record
    Record (..),
    newRecord,
    record_approximateArrivalTimestamp,
    record_encryptionType,
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
    streamDescription_streamModeDetails,
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
    streamDescriptionSummary_consumerCount,
    streamDescriptionSummary_encryptionType,
    streamDescriptionSummary_keyId,
    streamDescriptionSummary_streamModeDetails,
    streamDescriptionSummary_streamName,
    streamDescriptionSummary_streamARN,
    streamDescriptionSummary_streamStatus,
    streamDescriptionSummary_retentionPeriodHours,
    streamDescriptionSummary_streamCreationTimestamp,
    streamDescriptionSummary_enhancedMonitoring,
    streamDescriptionSummary_openShardCount,

    -- * StreamModeDetails
    StreamModeDetails (..),
    newStreamModeDetails,
    streamModeDetails_streamMode,

    -- * StreamSummary
    StreamSummary (..),
    newStreamSummary,
    streamSummary_streamCreationTimestamp,
    streamSummary_streamModeDetails,
    streamSummary_streamName,
    streamSummary_streamARN,
    streamSummary_streamStatus,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kinesis.Types.ChildShard
import Amazonka.Kinesis.Types.Consumer
import Amazonka.Kinesis.Types.ConsumerDescription
import Amazonka.Kinesis.Types.ConsumerStatus
import Amazonka.Kinesis.Types.EncryptionType
import Amazonka.Kinesis.Types.EnhancedMetrics
import Amazonka.Kinesis.Types.EnhancedMonitoringOutput
import Amazonka.Kinesis.Types.HashKeyRange
import Amazonka.Kinesis.Types.MetricsName
import Amazonka.Kinesis.Types.PutRecordsRequestEntry
import Amazonka.Kinesis.Types.PutRecordsResultEntry
import Amazonka.Kinesis.Types.Record
import Amazonka.Kinesis.Types.ScalingType
import Amazonka.Kinesis.Types.SequenceNumberRange
import Amazonka.Kinesis.Types.Shard
import Amazonka.Kinesis.Types.ShardFilter
import Amazonka.Kinesis.Types.ShardFilterType
import Amazonka.Kinesis.Types.ShardIteratorType
import Amazonka.Kinesis.Types.StartingPosition
import Amazonka.Kinesis.Types.StreamDescription
import Amazonka.Kinesis.Types.StreamDescriptionSummary
import Amazonka.Kinesis.Types.StreamMode
import Amazonka.Kinesis.Types.StreamModeDetails
import Amazonka.Kinesis.Types.StreamStatus
import Amazonka.Kinesis.Types.StreamSummary
import Amazonka.Kinesis.Types.SubscribeToShardEvent
import Amazonka.Kinesis.Types.Tag
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2013-12-02@ of the Amazon Kinesis SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Kinesis",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "kinesis",
      Core.signingName = "kinesis",
      Core.version = "2013-12-02",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Kinesis",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "LimitExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Specifies that you do not have the permissions required to perform this
-- operation.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The provided iterator exceeds the maximum age allowed.
_ExpiredIteratorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ExpiredIteratorException =
  Core._MatchServiceError
    defaultService
    "ExpiredIteratorException"

-- | The pagination token passed to the operation is expired.
_ExpiredNextTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ExpiredNextTokenException =
  Core._MatchServiceError
    defaultService
    "ExpiredNextTokenException"

-- | The processing of the request failed because of an unknown error,
-- exception, or failure.
_InternalFailureException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"

-- | A specified parameter exceeds its restrictions, is not supported, or
-- can\'t be used. For more information, see the returned message.
_InvalidArgumentException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"

-- | The ciphertext references a key that doesn\'t exist or that you don\'t
-- have access to.
_KMSAccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSAccessDeniedException =
  Core._MatchServiceError
    defaultService
    "KMSAccessDeniedException"

-- | The request was rejected because the specified customer master key (CMK)
-- isn\'t enabled.
_KMSDisabledException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSDisabledException =
  Core._MatchServiceError
    defaultService
    "KMSDisabledException"

-- | The request was rejected because the state of the specified resource
-- isn\'t valid for this request. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /Amazon Web Services Key Management Service Developer Guide/.
_KMSInvalidStateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSInvalidStateException =
  Core._MatchServiceError
    defaultService
    "KMSInvalidStateException"

-- | The request was rejected because the specified entity or resource can\'t
-- be found.
_KMSNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSNotFoundException =
  Core._MatchServiceError
    defaultService
    "KMSNotFoundException"

-- | The Amazon Web Services access key ID needs a subscription for the
-- service.
_KMSOptInRequired :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSOptInRequired =
  Core._MatchServiceError
    defaultService
    "KMSOptInRequired"

-- | The request was denied due to request throttling. For more information
-- about throttling, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/limits.html#requests-per-second Limits>
-- in the /Amazon Web Services Key Management Service Developer Guide/.
_KMSThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSThrottlingException =
  Core._MatchServiceError
    defaultService
    "KMSThrottlingException"

-- | The requested resource exceeds the maximum number allowed, or the number
-- of concurrent stream requests exceeds the maximum number allowed.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The request rate for the stream is too high, or the requested data is
-- too large for the available throughput. Reduce the frequency or size of
-- your requests. For more information, see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Streams Limits>
-- in the /Amazon Kinesis Data Streams Developer Guide/, and
-- <https://docs.aws.amazon.com/general/latest/gr/api-retries.html Error Retries and Exponential Backoff in Amazon Web Services>
-- in the /Amazon Web Services General Reference/.
_ProvisionedThroughputExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ProvisionedThroughputExceededException =
  Core._MatchServiceError
    defaultService
    "ProvisionedThroughputExceededException"

-- | The resource is not available for this operation. For successful
-- operation, the resource must be in the @ACTIVE@ state.
_ResourceInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The requested resource could not be found. The stream might not be
-- specified correctly.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Specifies that you tried to invoke this API for a data stream with the
-- on-demand capacity mode. This API is only supported for data streams
-- with the provisioned capacity mode.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
