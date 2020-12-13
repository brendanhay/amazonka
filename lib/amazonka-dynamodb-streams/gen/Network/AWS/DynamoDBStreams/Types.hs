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
    dynamoDBStreamsService,

    -- * Errors

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
    mkAttributeValue,
    avL,
    avNS,
    avM,
    avNULL,
    avN,
    avBS,
    avB,
    avSS,
    avS,
    avBOOL,

    -- * Identity
    Identity (..),
    mkIdentity,
    iPrincipalId,
    iType,

    -- * KeySchemaElement
    KeySchemaElement (..),
    mkKeySchemaElement,
    kseKeyType,
    kseAttributeName,

    -- * Record
    Record (..),
    mkRecord,
    rUserIdentity,
    rEventVersion,
    rDynamodb,
    rAwsRegion,
    rEventName,
    rEventSource,
    rEventId,

    -- * SequenceNumberRange
    SequenceNumberRange (..),
    mkSequenceNumberRange,
    snrStartingSequenceNumber,
    snrEndingSequenceNumber,

    -- * Shard
    Shard (..),
    mkShard,
    sParentShardId,
    sSequenceNumberRange,
    sShardId,

    -- * Stream
    Stream (..),
    mkStream,
    sStreamLabel,
    sStreamARN,
    sTableName,

    -- * StreamDescription
    StreamDescription (..),
    mkStreamDescription,
    sdLastEvaluatedShardId,
    sdStreamLabel,
    sdStreamStatus,
    sdKeySchema,
    sdStreamViewType,
    sdStreamARN,
    sdShards,
    sdTableName,
    sdCreationRequestDateTime,

    -- * StreamRecord
    StreamRecord (..),
    mkStreamRecord,
    srSizeBytes,
    srSequenceNumber,
    srApproximateCreationDateTime,
    srStreamViewType,
    srKeys,
    srOldImage,
    srNewImage,
  )
where

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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-08-10@ of the Amazon DynamoDB Streams SDK configuration.
dynamoDBStreamsService :: Lude.Service
dynamoDBStreamsService =
  Lude.Service
    { Lude._svcAbbrev = "DynamoDBStreams",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "streams.dynamodb",
      Lude._svcVersion = "2012-08-10",
      Lude._svcEndpoint = Lude.defaultEndpoint dynamoDBStreamsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "DynamoDBStreams",
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
