-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _ExpiredIteratorException,
    _InternalServerError,
    _TrimmedDataAccessException,
    _ResourceNotFoundException,
    _LimitExceededException,

    -- * SequenceNumber
    SequenceNumber (..),

    -- * Stream
    Stream (..),
    mkStream,
    sStreamArn,
    sStreamLabel,
    sTableName,

    -- * Shard
    Shard (..),
    mkShard,
    sParentShardId,
    sSequenceNumberRange,
    sShardId,

    -- * KeyType
    KeyType (..),

    -- * AttributeValue
    AttributeValue (..),
    mkAttributeValue,
    avB,
    avBOOL,
    avBS,
    avL,
    avM,
    avN,
    avNS,
    avNULL,
    avS,
    avSS,

    -- * StreamDescription
    StreamDescription (..),
    mkStreamDescription,
    sdCreationRequestDateTime,
    sdKeySchema,
    sdLastEvaluatedShardId,
    sdShards,
    sdStreamArn,
    sdStreamLabel,
    sdStreamStatus,
    sdStreamViewType,
    sdTableName,

    -- * StringAttributeValue
    StringAttributeValue (..),

    -- * String
    String (..),

    -- * KeySchemaAttributeName
    KeySchemaAttributeName (..),

    -- * StreamStatus
    StreamStatus (..),

    -- * ShardIterator
    ShardIterator (..),

    -- * StreamViewType
    StreamViewType (..),

    -- * StreamArn
    StreamArn (..),

    -- * NumberAttributeValue
    NumberAttributeValue (..),

    -- * OperationType
    OperationType (..),

    -- * Record
    Record (..),
    mkRecord,
    rAwsRegion,
    rDynamodb,
    rEventID,
    rEventName,
    rEventSource,
    rEventVersion,
    rUserIdentity,

    -- * KeySchemaElement
    KeySchemaElement (..),
    mkKeySchemaElement,
    kseAttributeName,
    kseKeyType,

    -- * SequenceNumberRange
    SequenceNumberRange (..),
    mkSequenceNumberRange,
    snrEndingSequenceNumber,
    snrStartingSequenceNumber,

    -- * AttributeName
    AttributeName (..),

    -- * Identity
    Identity (..),
    mkIdentity,
    iPrincipalId,
    iType,

    -- * ShardIteratorType
    ShardIteratorType (..),

    -- * TableName
    TableName (..),

    -- * StreamRecord
    StreamRecord (..),
    mkStreamRecord,
    srApproximateCreationDateTime,
    srKeys,
    srNewImage,
    srOldImage,
    srSequenceNumber,
    srSizeBytes,
    srStreamViewType,

    -- * ShardId
    ShardId (..),

    -- * StreamLabel
    StreamLabel (..),

    -- * ParentShardId
    ParentShardId (..),

    -- * N
    N (..),

    -- * S
    S (..),

    -- * LastEvaluatedShardId
    LastEvaluatedShardId (..),

    -- * LastEvaluatedStreamArn
    LastEvaluatedStreamArn (..),

    -- * ExclusiveStartShardId
    ExclusiveStartShardId (..),
  )
where

import Network.AWS.DynamoDBStreams.Types.AttributeName
import Network.AWS.DynamoDBStreams.Types.AttributeValue
import Network.AWS.DynamoDBStreams.Types.ExclusiveStartShardId
import Network.AWS.DynamoDBStreams.Types.Identity
import Network.AWS.DynamoDBStreams.Types.KeySchemaAttributeName
import Network.AWS.DynamoDBStreams.Types.KeySchemaElement
import Network.AWS.DynamoDBStreams.Types.KeyType
import Network.AWS.DynamoDBStreams.Types.LastEvaluatedShardId
import Network.AWS.DynamoDBStreams.Types.LastEvaluatedStreamArn
import Network.AWS.DynamoDBStreams.Types.N
import Network.AWS.DynamoDBStreams.Types.NumberAttributeValue
import Network.AWS.DynamoDBStreams.Types.OperationType
import Network.AWS.DynamoDBStreams.Types.ParentShardId
import Network.AWS.DynamoDBStreams.Types.Record
import Network.AWS.DynamoDBStreams.Types.S
import Network.AWS.DynamoDBStreams.Types.SequenceNumber
import Network.AWS.DynamoDBStreams.Types.SequenceNumberRange
import Network.AWS.DynamoDBStreams.Types.Shard
import Network.AWS.DynamoDBStreams.Types.ShardId
import Network.AWS.DynamoDBStreams.Types.ShardIterator
import Network.AWS.DynamoDBStreams.Types.ShardIteratorType
import Network.AWS.DynamoDBStreams.Types.Stream
import Network.AWS.DynamoDBStreams.Types.StreamArn
import Network.AWS.DynamoDBStreams.Types.StreamDescription
import Network.AWS.DynamoDBStreams.Types.StreamLabel
import Network.AWS.DynamoDBStreams.Types.StreamRecord
import Network.AWS.DynamoDBStreams.Types.StreamStatus
import Network.AWS.DynamoDBStreams.Types.StreamViewType
import Network.AWS.DynamoDBStreams.Types.String
import Network.AWS.DynamoDBStreams.Types.StringAttributeValue
import Network.AWS.DynamoDBStreams.Types.TableName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-08-10@ of the Amazon DynamoDB Streams SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "DynamoDBStreams",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "streams.dynamodb",
      Core._svcVersion = "2012-08-10",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "DynamoDBStreams",
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
      | Core.otherwise = Core.Nothing

-- | The shard iterator has expired and can no longer be used to retrieve stream records. A shard iterator expires 15 minutes after it is retrieved using the @GetShardIterator@ action.
_ExpiredIteratorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExpiredIteratorException =
  Core._MatchServiceError
    mkServiceConfig
    "ExpiredIteratorException"
{-# DEPRECATED _ExpiredIteratorException "Use generic-lens or generic-optics instead." #-}

-- | An error occurred on the server side.
_InternalServerError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError mkServiceConfig "InternalServerError"
{-# DEPRECATED _InternalServerError "Use generic-lens or generic-optics instead." #-}

-- | The operation attempted to read past the oldest stream record in a shard.
--
-- In DynamoDB Streams, there is a 24 hour limit on data retention. Stream records whose age exceeds this limit are subject to removal (trimming) from the stream. You might receive a TrimmedDataAccessException if:
--
--     * You request a shard iterator with a sequence number older than the trim point (24 hours).
--
--
--     * You obtain a shard iterator, but before you use the iterator in a @GetRecords@ request, a stream record in the shard exceeds the 24 hour period and is trimmed. This causes the iterator to access a record that no longer exists.
_TrimmedDataAccessException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TrimmedDataAccessException =
  Core._MatchServiceError
    mkServiceConfig
    "TrimmedDataAccessException"
{-# DEPRECATED _TrimmedDataAccessException "Use generic-lens or generic-optics instead." #-}

-- | The operation tried to access a nonexistent table or index. The resource might not be specified correctly, or its status might not be @ACTIVE@ .
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | There is no limit to the number of daily on-demand backups that can be taken.
--
-- Up to 50 simultaneous table operations are allowed per account. These operations include @CreateTable@ , @UpdateTable@ , @DeleteTable@ ,@UpdateTimeToLive@ , @RestoreTableFromBackup@ , and @RestoreTableToPointInTime@ .
-- The only exception is when you are creating a table with one or more secondary indexes. You can have up to 25 such requests running at a time; however, if the table or index specifications are complex, DynamoDB might temporarily reduce the number of concurrent operations.
-- There is a soft account quota of 256 tables.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}
