{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _TrimmedDataAccessException,
    _ExpiredIteratorException,
    _InternalServerError,
    _LimitExceededException,
    _ResourceNotFoundException,

    -- * KeyType
    KeyType (..),

    -- * OperationType
    OperationType (..),

    -- * ShardIteratorType
    ShardIteratorType (..),

    -- * StreamStatus
    StreamStatus (..),

    -- * StreamViewType
    StreamViewType (..),

    -- * AttributeValue
    AttributeValue (..),
    newAttributeValue,
    attributeValue_bs,
    attributeValue_bool,
    attributeValue_n,
    attributeValue_s,
    attributeValue_null,
    attributeValue_m,
    attributeValue_b,
    attributeValue_l,
    attributeValue_ss,
    attributeValue_ns,

    -- * Identity
    Identity (..),
    newIdentity,
    identity_principalId,
    identity_type,

    -- * KeySchemaElement
    KeySchemaElement (..),
    newKeySchemaElement,
    keySchemaElement_attributeName,
    keySchemaElement_keyType,

    -- * Record
    Record (..),
    newRecord,
    record_userIdentity,
    record_eventID,
    record_eventSource,
    record_eventName,
    record_eventVersion,
    record_dynamodb,
    record_awsRegion,

    -- * SequenceNumberRange
    SequenceNumberRange (..),
    newSequenceNumberRange,
    sequenceNumberRange_startingSequenceNumber,
    sequenceNumberRange_endingSequenceNumber,

    -- * Shard
    Shard (..),
    newShard,
    shard_shardId,
    shard_sequenceNumberRange,
    shard_parentShardId,

    -- * Stream
    Stream (..),
    newStream,
    stream_tableName,
    stream_streamArn,
    stream_streamLabel,

    -- * StreamDescription
    StreamDescription (..),
    newStreamDescription,
    streamDescription_lastEvaluatedShardId,
    streamDescription_streamViewType,
    streamDescription_tableName,
    streamDescription_creationRequestDateTime,
    streamDescription_keySchema,
    streamDescription_streamStatus,
    streamDescription_shards,
    streamDescription_streamArn,
    streamDescription_streamLabel,

    -- * StreamRecord
    StreamRecord (..),
    newStreamRecord,
    streamRecord_sequenceNumber,
    streamRecord_streamViewType,
    streamRecord_keys,
    streamRecord_sizeBytes,
    streamRecord_newImage,
    streamRecord_oldImage,
    streamRecord_approximateCreationDateTime,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDBStreams.Types.AttributeValue
import Network.AWS.DynamoDBStreams.Types.Identity
import Network.AWS.DynamoDBStreams.Types.KeySchemaElement
import Network.AWS.DynamoDBStreams.Types.KeyType
import Network.AWS.DynamoDBStreams.Types.OperationType
import Network.AWS.DynamoDBStreams.Types.Record
import Network.AWS.DynamoDBStreams.Types.SequenceNumberRange
import Network.AWS.DynamoDBStreams.Types.Shard
import Network.AWS.DynamoDBStreams.Types.ShardIteratorType
import Network.AWS.DynamoDBStreams.Types.Stream
import Network.AWS.DynamoDBStreams.Types.StreamDescription
import Network.AWS.DynamoDBStreams.Types.StreamRecord
import Network.AWS.DynamoDBStreams.Types.StreamStatus
import Network.AWS.DynamoDBStreams.Types.StreamViewType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-08-10@ of the Amazon DynamoDB Streams SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "DynamoDBStreams",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "streams.dynamodb",
      Core._serviceSigningName = "dynamodb",
      Core._serviceVersion = "2012-08-10",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "DynamoDBStreams",
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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The operation attempted to read past the oldest stream record in a
-- shard.
--
-- In DynamoDB Streams, there is a 24 hour limit on data retention. Stream
-- records whose age exceeds this limit are subject to removal (trimming)
-- from the stream. You might receive a TrimmedDataAccessException if:
--
-- -   You request a shard iterator with a sequence number older than the
--     trim point (24 hours).
--
-- -   You obtain a shard iterator, but before you use the iterator in a
--     @GetRecords@ request, a stream record in the shard exceeds the 24
--     hour period and is trimmed. This causes the iterator to access a
--     record that no longer exists.
_TrimmedDataAccessException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TrimmedDataAccessException =
  Core._MatchServiceError
    defaultService
    "TrimmedDataAccessException"

-- | The shard iterator has expired and can no longer be used to retrieve
-- stream records. A shard iterator expires 15 minutes after it is
-- retrieved using the @GetShardIterator@ action.
_ExpiredIteratorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ExpiredIteratorException =
  Core._MatchServiceError
    defaultService
    "ExpiredIteratorException"

-- | An error occurred on the server side.
_InternalServerError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | There is no limit to the number of daily on-demand backups that can be
-- taken.
--
-- Up to 50 simultaneous table operations are allowed per account. These
-- operations include @CreateTable@, @UpdateTable@,
-- @DeleteTable@,@UpdateTimeToLive@, @RestoreTableFromBackup@, and
-- @RestoreTableToPointInTime@.
--
-- The only exception is when you are creating a table with one or more
-- secondary indexes. You can have up to 25 such requests running at a
-- time; however, if the table or index specifications are complex,
-- DynamoDB might temporarily reduce the number of concurrent operations.
--
-- There is a soft account quota of 256 tables.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The operation tried to access a nonexistent table or index. The resource
-- might not be specified correctly, or its status might not be @ACTIVE@.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
