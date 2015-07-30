{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDBStreams.Types
    (
    -- * Service
      DynamoDBStreams

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
    , avM
    , avNS
    , avNULL
    , avN
    , avBS
    , avB
    , avSS
    , avS
    , avBOOL

    -- * KeySchemaElement
    , KeySchemaElement
    , keySchemaElement
    , kseAttributeName
    , kseKeyType

    -- * Record
    , Record
    , record
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
    , sdStreamARN
    , sdStreamViewType
    , sdShards
    , sdTableName
    , sdCreationRequestDateTime

    -- * StreamRecord
    , StreamRecord
    , streamRecord
    , srSequenceNumber
    , srSizeBytes
    , srStreamViewType
    , srKeys
    , srOldImage
    , srNewImage
    ) where

import           Network.AWS.DynamoDBStreams.Types.Product
import           Network.AWS.DynamoDBStreams.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2012-08-10@ of the Amazon DynamoDB Streams SDK.
data DynamoDBStreams

instance AWSService DynamoDBStreams where
    type Sg DynamoDBStreams = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "DynamoDBStreams"
            , _svcPrefix = "streams.dynamodb"
            , _svcVersion = "2012-08-10"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The shard iterator has expired and can no longer be used to retrieve
-- stream records. A shard iterator expires 15 minutes after it is
-- retrieved using the /GetShardIterator/ action.
_ExpiredIteratorException :: AWSError a => Getting (First ServiceError) a ServiceError
_ExpiredIteratorException = _ServiceError . hasCode "ExpiredIteratorException"

-- | An error occurred on the server side.
_InternalServerError :: AWSError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _ServiceError . hasCode "InternalServerError"

-- | The operation attempted to read past the oldest stream record in a
-- shard.
--
-- In DynamoDB Streams, there is a 24 hour limit on data retention. Stream
-- records whose age exceeds this limit are subject to removal (trimming)
-- from the stream. You might receive a TrimmedDataAccessException if:
--
-- -   You request a shard iterator with a sequence number older than the
--     trim point (24 hours).
-- -   You obtain a shard iterator, but before you use the iterator in a
--     /GetRecords/ request, a stream record in the shard exceeds the 24
--     hour period and is trimmed. This causes the iterator to access a
--     record that no longer exists.
_TrimmedDataAccessException :: AWSError a => Getting (First ServiceError) a ServiceError
_TrimmedDataAccessException =
    _ServiceError . hasCode "TrimmedDataAccessException"

-- | The operation tried to access a nonexistent stream.
_ResourceNotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasCode "ResourceNotFoundException"

-- | Your request rate is too high. The AWS SDKs for DynamoDB automatically
-- retry requests that receive this exception. Your request is eventually
-- successful, unless your retry queue is too large to finish. Reduce the
-- frequency of requests and use exponential backoff. For more information,
-- go to
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html#APIRetries Error Retries and Exponential Backoff>
-- in the /Amazon DynamoDB Developer Guide/.
_LimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"
