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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2013-12-02@ of the Amazon Kinesis SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "Kinesis",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "kinesis",
      Prelude._svcSigningName = "kinesis",
      Prelude._svcVersion = "2013-12-02",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseJSONError "Kinesis",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has
          ( Prelude.hasCode "LimitExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_limit_exceeded"
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The request was denied due to request throttling. For more information
-- about throttling, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/limits.html#requests-per-second Limits>
-- in the /AWS Key Management Service Developer Guide/.
_KMSThrottlingException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KMSThrottlingException =
  Prelude._MatchServiceError
    defaultService
    "KMSThrottlingException"

-- | The pagination token passed to the operation is expired.
_ExpiredNextTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ExpiredNextTokenException =
  Prelude._MatchServiceError
    defaultService
    "ExpiredNextTokenException"

-- | The request was rejected because the state of the specified resource
-- isn\'t valid for this request. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
_KMSInvalidStateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KMSInvalidStateException =
  Prelude._MatchServiceError
    defaultService
    "KMSInvalidStateException"

-- | The request was rejected because the specified entity or resource can\'t
-- be found.
_KMSNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KMSNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "KMSNotFoundException"

-- | The AWS access key ID needs a subscription for the service.
_KMSOptInRequired :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KMSOptInRequired =
  Prelude._MatchServiceError
    defaultService
    "KMSOptInRequired"

-- | The provided iterator exceeds the maximum age allowed.
_ExpiredIteratorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ExpiredIteratorException =
  Prelude._MatchServiceError
    defaultService
    "ExpiredIteratorException"

-- | The resource is not available for this operation. For successful
-- operation, the resource must be in the @ACTIVE@ state.
_ResourceInUseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceInUseException =
  Prelude._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The requested resource exceeds the maximum number allowed, or the number
-- of concurrent stream requests exceeds the maximum number allowed.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The ciphertext references a key that doesn\'t exist or that you don\'t
-- have access to.
_KMSAccessDeniedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KMSAccessDeniedException =
  Prelude._MatchServiceError
    defaultService
    "KMSAccessDeniedException"

-- | The request rate for the stream is too high, or the requested data is
-- too large for the available throughput. Reduce the frequency or size of
-- your requests. For more information, see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Streams Limits>
-- in the /Amazon Kinesis Data Streams Developer Guide/, and
-- <https://docs.aws.amazon.com/general/latest/gr/api-retries.html Error Retries and Exponential Backoff in AWS>
-- in the /AWS General Reference/.
_ProvisionedThroughputExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ProvisionedThroughputExceededException =
  Prelude._MatchServiceError
    defaultService
    "ProvisionedThroughputExceededException"

-- | The requested resource could not be found. The stream might not be
-- specified correctly.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The processing of the request failed because of an unknown error,
-- exception, or failure.
_InternalFailureException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalFailureException =
  Prelude._MatchServiceError
    defaultService
    "InternalFailureException"

-- | A specified parameter exceeds its restrictions, is not supported, or
-- can\'t be used. For more information, see the returned message.
_InvalidArgumentException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidArgumentException =
  Prelude._MatchServiceError
    defaultService
    "InvalidArgumentException"

-- | The request was rejected because the specified customer master key (CMK)
-- isn\'t enabled.
_KMSDisabledException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KMSDisabledException =
  Prelude._MatchServiceError
    defaultService
    "KMSDisabledException"
