{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types
    (
    -- * Service Configuration
      kinesis

    -- * Errors
    , _ExpiredIteratorException
    , _InvalidArgumentException
    , _ProvisionedThroughputExceededException
    , _ResourceNotFoundException
    , _LimitExceededException
    , _ResourceInUseException

    -- * MetricsName
    , MetricsName (..)

    -- * ScalingType
    , ScalingType (..)

    -- * ShardIteratorType
    , ShardIteratorType (..)

    -- * StreamStatus
    , StreamStatus (..)

    -- * EnhancedMetrics
    , EnhancedMetrics
    , enhancedMetrics
    , emShardLevelMetrics

    -- * EnhancedMonitoringOutput
    , EnhancedMonitoringOutput
    , enhancedMonitoringOutput
    , emoDesiredShardLevelMetrics
    , emoCurrentShardLevelMetrics
    , emoStreamName

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
    , rApproximateArrivalTimestamp
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
    , sdRetentionPeriodHours
    , sdStreamCreationTimestamp
    , sdEnhancedMonitoring

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import           Network.AWS.Kinesis.Types.Product
import           Network.AWS.Kinesis.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version @2013-12-02@ of the Amazon Kinesis SDK configuration.
kinesis :: Service
kinesis =
    Service
    { _svcAbbrev = "Kinesis"
    , _svcSigner = v4
    , _svcPrefix = "kinesis"
    , _svcVersion = "2013-12-02"
    , _svcEndpoint = defaultEndpoint kinesis
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Kinesis"
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
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The provided iterator exceeds the maximum age allowed.
--
--
_ExpiredIteratorException :: AsError a => Getting (First ServiceError) a ServiceError
_ExpiredIteratorException = _ServiceError . hasCode "ExpiredIteratorException"

-- | A specified parameter exceeds its restrictions, is not supported, or can't be used. For more information, see the returned message.
--
--
_InvalidArgumentException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidArgumentException = _ServiceError . hasCode "InvalidArgumentException"

-- | The request rate for the stream is too high, or the requested data is too large for the available throughput. Reduce the frequency or size of your requests. For more information, see <http://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Streams Limits> in the /Amazon Kinesis Streams Developer Guide/ , and <http://docs.aws.amazon.com/general/latest/gr/api-retries.html Error Retries and Exponential Backoff in AWS> in the /AWS General Reference/ .
--
--
_ProvisionedThroughputExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ProvisionedThroughputExceededException =
    _ServiceError . hasCode "ProvisionedThroughputExceededException"

-- | The requested resource could not be found. The stream might not be specified correctly.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasCode "ResourceNotFoundException"

-- | The requested resource exceeds the maximum number allowed, or the number of concurrent stream requests exceeds the maximum number allowed (5).
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"

-- | The resource is not available for this operation. For successful operation, the resource needs to be in the @ACTIVE@ state.
--
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException = _ServiceError . hasCode "ResourceInUseException"
