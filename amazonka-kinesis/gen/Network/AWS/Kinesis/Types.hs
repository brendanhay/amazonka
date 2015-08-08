{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types
    (
    -- * Service Decription
      Kinesis

    -- * Error Matchers
    , _ExpiredIteratorException
    , _InvalidArgumentException
    , _ProvisionedThroughputExceededException
    , _ResourceNotFoundException
    , _ResourceInUseException
    , _LimitExceededException

    -- * ShardIteratorType
    , ShardIteratorType (..)

    -- * StreamStatus
    , StreamStatus (..)

    -- * HashKeyRange
    , HashKeyRange
    , hashKeyRange
    , hkrStartingHashKey
    , hkrEndingHashKey

    -- * PutRecordsRequestEntry
    , PutRecordsRequestEntry
    , putRecordsRequestEntry
    , prreExplicitHashKey
    , prreData
    , prrePartitionKey

    -- * PutRecordsResultEntry
    , PutRecordsResultEntry
    , putRecordsResultEntry
    , prreSequenceNumber
    , prreErrorCode
    , prreErrorMessage
    , prreShardId

    -- * Record
    , Record
    , record
    , rSequenceNumber
    , rData
    , rPartitionKey

    -- * SequenceNumberRange
    , SequenceNumberRange
    , sequenceNumberRange
    , snrEndingSequenceNumber
    , snrStartingSequenceNumber

    -- * Shard
    , Shard
    , shard
    , sAdjacentParentShardId
    , sParentShardId
    , sShardId
    , sHashKeyRange
    , sSequenceNumberRange

    -- * StreamDescription
    , StreamDescription
    , streamDescription
    , sdStreamName
    , sdStreamARN
    , sdStreamStatus
    , sdShards
    , sdHasMoreShards

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import           Network.AWS.Kinesis.Types.Product
import           Network.AWS.Kinesis.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2013-12-02@ of the Amazon Kinesis SDK.
data Kinesis

instance AWSService Kinesis where
    type Sg Kinesis = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "Kinesis"
            , _svcPrefix = "kinesis"
            , _svcVersion = "2013-12-02"
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

-- | The provided iterator exceeds the maximum age allowed.
_ExpiredIteratorException :: AsError a => Getting (First ServiceError) a ServiceError
_ExpiredIteratorException = _ServiceError . hasCode "ExpiredIteratorException"

-- | A specified parameter exceeds its restrictions, is not supported, or
-- can\'t be used. For more information, see the returned message.
_InvalidArgumentException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidArgumentException = _ServiceError . hasCode "InvalidArgumentException"

-- | The request rate is too high, or the requested data is too large for the
-- available throughput. Reduce the frequency or size of your requests. For
-- more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/api-retries.html Error Retries and Exponential Backoff in AWS>
-- in the /AWS General Reference/.
_ProvisionedThroughputExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ProvisionedThroughputExceededException =
    _ServiceError . hasCode "ProvisionedThroughputExceededException"

-- | The requested resource could not be found. It might not be specified
-- correctly, or it might not be in the @ACTIVE@ state.
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasCode "ResourceNotFoundException"

-- | The resource is not available for this operation. For example, you
-- attempted to split a shard but the stream is not in the @ACTIVE@ state.
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException = _ServiceError . hasCode "ResourceInUseException"

-- | The requested resource exceeds the maximum number allowed, or the number
-- of concurrent stream requests exceeds the maximum number allowed (5).
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"
