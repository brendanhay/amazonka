{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DynamoDBStreams.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDBStreams.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ExpiredIteratorException,
    _InternalServerError,
    _LimitExceededException,
    _ResourceNotFoundException,
    _TrimmedDataAccessException,

    -- * Re-exported Types
    module Amazonka.DynamoDBStreams.Internal,

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
    record_awsRegion,
    record_dynamodb,
    record_eventID,
    record_eventName,
    record_eventSource,
    record_eventVersion,
    record_userIdentity,

    -- * SequenceNumberRange
    SequenceNumberRange (..),
    newSequenceNumberRange,
    sequenceNumberRange_endingSequenceNumber,
    sequenceNumberRange_startingSequenceNumber,

    -- * Shard
    Shard (..),
    newShard,
    shard_parentShardId,
    shard_sequenceNumberRange,
    shard_shardId,

    -- * Stream
    Stream (..),
    newStream,
    stream_streamArn,
    stream_streamLabel,
    stream_tableName,

    -- * StreamDescription
    StreamDescription (..),
    newStreamDescription,
    streamDescription_creationRequestDateTime,
    streamDescription_keySchema,
    streamDescription_lastEvaluatedShardId,
    streamDescription_shards,
    streamDescription_streamArn,
    streamDescription_streamLabel,
    streamDescription_streamStatus,
    streamDescription_streamViewType,
    streamDescription_tableName,

    -- * StreamRecord
    StreamRecord (..),
    newStreamRecord,
    streamRecord_approximateCreationDateTime,
    streamRecord_keys,
    streamRecord_newImage,
    streamRecord_oldImage,
    streamRecord_sequenceNumber,
    streamRecord_sizeBytes,
    streamRecord_streamViewType,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDBStreams.Internal
import Amazonka.DynamoDBStreams.Types.Identity
import Amazonka.DynamoDBStreams.Types.KeySchemaElement
import Amazonka.DynamoDBStreams.Types.KeyType
import Amazonka.DynamoDBStreams.Types.OperationType
import Amazonka.DynamoDBStreams.Types.Record
import Amazonka.DynamoDBStreams.Types.SequenceNumberRange
import Amazonka.DynamoDBStreams.Types.Shard
import Amazonka.DynamoDBStreams.Types.ShardIteratorType
import Amazonka.DynamoDBStreams.Types.Stream
import Amazonka.DynamoDBStreams.Types.StreamDescription
import Amazonka.DynamoDBStreams.Types.StreamRecord
import Amazonka.DynamoDBStreams.Types.StreamStatus
import Amazonka.DynamoDBStreams.Types.StreamViewType
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2012-08-10@ of the Amazon DynamoDB Streams SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "DynamoDBStreams",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "streams.dynamodb",
      Core.signingName = "dynamodb",
      Core.version = "2012-08-10",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "DynamoDBStreams",
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

-- | The shard iterator has expired and can no longer be used to retrieve
-- stream records. A shard iterator expires 15 minutes after it is
-- retrieved using the @GetShardIterator@ action.
_ExpiredIteratorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ExpiredIteratorException =
  Core._MatchServiceError
    defaultService
    "ExpiredIteratorException"

-- | An error occurred on the server side.
_InternalServerError :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | There is no limit to the number of daily on-demand backups that can be
-- taken.
--
-- For most purposes, up to 500 simultaneous table operations are allowed
-- per account. These operations include @CreateTable@, @UpdateTable@,
-- @DeleteTable@,@UpdateTimeToLive@, @RestoreTableFromBackup@, and
-- @RestoreTableToPointInTime@.
--
-- When you are creating a table with one or more secondary indexes, you
-- can have up to 250 such requests running at a time. However, if the
-- table or index specifications are complex, then DynamoDB might
-- temporarily reduce the number of concurrent operations.
--
-- When importing into DynamoDB, up to 50 simultaneous import table
-- operations are allowed per account.
--
-- There is a soft account quota of 2,500 tables.
--
-- GetRecords was called with a value of more than 1000 for the limit
-- request parameter.
--
-- More than 2 processes are reading from the same streams shard at the
-- same time. Exceeding this limit may result in request throttling.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The operation tried to access a nonexistent table or index. The resource
-- might not be specified correctly, or its status might not be @ACTIVE@.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

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
_TrimmedDataAccessException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TrimmedDataAccessException =
  Core._MatchServiceError
    defaultService
    "TrimmedDataAccessException"
