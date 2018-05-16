{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDBStreams.Types
    (
    -- * Service Configuration
      dynamoDBStreams

    -- * Errors
    , _ExpiredIteratorException
    , _InternalServerError
    , _TrimmedDataAccessException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * KeyType
    , KeyType (..)

    -- * OperationType
    , OperationType (..)

    -- * ShardIteratorType
    , ShardIteratorType (..)

    -- * StreamStatus
    , StreamStatus (..)

    -- * StreamViewType
    , StreamViewType (..)

    -- * AttributeValue
    , AttributeValue
    , attributeValue
    , avL
    , avNS
    , avM
    , avNULL
    , avN
    , avBS
    , avB
    , avSS
    , avS
    , avBOOL

    -- * Identity
    , Identity
    , identity
    , iPrincipalId
    , iType

    -- * KeySchemaElement
    , KeySchemaElement
    , keySchemaElement
    , kseAttributeName
    , kseKeyType

    -- * Record
    , Record
    , record
    , rUserIdentity
    , rEventVersion
    , rDynamodb
    , rAwsRegion
    , rEventName
    , rEventSource
    , rEventId

    -- * SequenceNumberRange
    , SequenceNumberRange
    , sequenceNumberRange
    , snrStartingSequenceNumber
    , snrEndingSequenceNumber

    -- * Shard
    , Shard
    , shard
    , sParentShardId
    , sSequenceNumberRange
    , sShardId

    -- * Stream
    , Stream
    , stream
    , sStreamLabel
    , sStreamARN
    , sTableName

    -- * StreamDescription
    , StreamDescription
    , streamDescription
    , sdLastEvaluatedShardId
    , sdStreamLabel
    , sdStreamStatus
    , sdKeySchema
    , sdStreamViewType
    , sdStreamARN
    , sdShards
    , sdTableName
    , sdCreationRequestDateTime

    -- * StreamRecord
    , StreamRecord
    , streamRecord
    , srSizeBytes
    , srSequenceNumber
    , srApproximateCreationDateTime
    , srStreamViewType
    , srKeys
    , srOldImage
    , srNewImage
    ) where

import Network.AWS.DynamoDBStreams.Types.Product
import Network.AWS.DynamoDBStreams.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2012-08-10@ of the Amazon DynamoDB Streams SDK configuration.
dynamoDBStreams :: Service
dynamoDBStreams =
  Service
    { _svcAbbrev = "DynamoDBStreams"
    , _svcSigner = v4
    , _svcPrefix = "streams.dynamodb"
    , _svcVersion = "2012-08-10"
    , _svcEndpoint = defaultEndpoint dynamoDBStreams
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "DynamoDBStreams"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The shard iterator has expired and can no longer be used to retrieve stream records. A shard iterator expires 15 minutes after it is retrieved using the @GetShardIterator@ action.
--
--
_ExpiredIteratorException :: AsError a => Getting (First ServiceError) a ServiceError
_ExpiredIteratorException =
  _MatchServiceError dynamoDBStreams "ExpiredIteratorException"


-- | An error occurred on the server side.
--
--
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _MatchServiceError dynamoDBStreams "InternalServerError"


-- | The operation attempted to read past the oldest stream record in a shard.
--
--
-- In DynamoDB Streams, there is a 24 hour limit on data retention. Stream records whose age exceeds this limit are subject to removal (trimming) from the stream. You might receive a TrimmedDataAccessException if:
--
--     * You request a shard iterator with a sequence number older than the trim point (24 hours).
--
--     * You obtain a shard iterator, but before you use the iterator in a @GetRecords@ request, a stream record in the shard exceeds the 24 hour period and is trimmed. This causes the iterator to access a record that no longer exists.
--
--
--
_TrimmedDataAccessException :: AsError a => Getting (First ServiceError) a ServiceError
_TrimmedDataAccessException =
  _MatchServiceError dynamoDBStreams "TrimmedDataAccessException"


-- | The operation tried to access a nonexistent stream.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError dynamoDBStreams "ResourceNotFoundException"


-- | Your request rate is too high. The AWS SDKs for DynamoDB automatically retry requests that receive this exception. Your request is eventually successful, unless your retry queue is too large to finish. Reduce the frequency of requests and use exponential backoff. For more information, go to <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html#APIRetries Error Retries and Exponential Backoff> in the /Amazon DynamoDB Developer Guide/ .
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError dynamoDBStreams "LimitExceededException"

