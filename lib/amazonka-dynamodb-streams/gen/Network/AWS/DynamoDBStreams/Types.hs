{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types
  ( -- * Service Configuration
    dynamoDBStreams,

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
    AttributeValue,
    attributeValue,
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
    Identity,
    identity,
    iPrincipalId,
    iType,

    -- * KeySchemaElement
    KeySchemaElement,
    keySchemaElement,
    kseAttributeName,
    kseKeyType,

    -- * Record
    Record,
    record,
    rUserIdentity,
    rEventVersion,
    rDynamodb,
    rAwsRegion,
    rEventName,
    rEventSource,
    rEventId,

    -- * SequenceNumberRange
    SequenceNumberRange,
    sequenceNumberRange,
    snrStartingSequenceNumber,
    snrEndingSequenceNumber,

    -- * Shard
    Shard,
    shard,
    sParentShardId,
    sSequenceNumberRange,
    sShardId,

    -- * Stream
    Stream,
    stream,
    sStreamLabel,
    sStreamARN,
    sTableName,

    -- * StreamDescription
    StreamDescription,
    streamDescription,
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
    StreamRecord,
    streamRecord,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2012-08-10@ of the Amazon DynamoDB Streams SDK configuration.
dynamoDBStreams :: Service
dynamoDBStreams =
  Service
    { _svcAbbrev = "DynamoDBStreams",
      _svcSigner = v4,
      _svcPrefix = "streams.dynamodb",
      _svcVersion = "2012-08-10",
      _svcEndpoint = defaultEndpoint dynamoDBStreams,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "DynamoDBStreams",
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
