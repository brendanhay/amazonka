{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types
    (
    -- * Service
      CloudWatchLogs

    -- * Errors
    , _InvalidParameterException
    , _InvalidSequenceTokenException
    , _ResourceAlreadyExistsException
    , _OperationAbortedException
    , _DataAlreadyAcceptedException
    , _ServiceUnavailableException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * OrderBy
    , OrderBy (..)

    -- * FilteredLogEvent
    , FilteredLogEvent
    , filteredLogEvent
    , fleIngestionTime
    , fleLogStreamName
    , fleMessage
    , fleTimestamp
    , fleEventId

    -- * InputLogEvent
    , InputLogEvent
    , inputLogEvent
    , ileTimestamp
    , ileMessage

    -- * LogGroup
    , LogGroup
    , logGroup
    , lgCreationTime
    , lgMetricFilterCount
    , lgArn
    , lgLogGroupName
    , lgRetentionInDays
    , lgStoredBytes

    -- * LogStream
    , LogStream
    , logStream
    , lsCreationTime
    , lsUploadSequenceToken
    , lsArn
    , lsFirstEventTimestamp
    , lsLogStreamName
    , lsStoredBytes
    , lsLastIngestionTime
    , lsLastEventTimestamp

    -- * MetricFilter
    , MetricFilter
    , metricFilter
    , mfCreationTime
    , mfFilterName
    , mfFilterPattern
    , mfMetricTransformations

    -- * MetricFilterMatchRecord
    , MetricFilterMatchRecord
    , metricFilterMatchRecord
    , mfmrExtractedValues
    , mfmrEventMessage
    , mfmrEventNumber

    -- * MetricTransformation
    , MetricTransformation
    , metricTransformation
    , mtMetricName
    , mtMetricNamespace
    , mtMetricValue

    -- * OutputLogEvent
    , OutputLogEvent
    , outputLogEvent
    , oleIngestionTime
    , oleMessage
    , oleTimestamp

    -- * RejectedLogEventsInfo
    , RejectedLogEventsInfo
    , rejectedLogEventsInfo
    , rleiTooOldLogEventEndIndex
    , rleiTooNewLogEventStartIndex
    , rleiExpiredLogEventEndIndex

    -- * SearchedLogStream
    , SearchedLogStream
    , searchedLogStream
    , slsLogStreamName
    , slsSearchedCompletely

    -- * SubscriptionFilter
    , SubscriptionFilter
    , subscriptionFilter
    , sfCreationTime
    , sfFilterName
    , sfDestinationARN
    , sfLogGroupName
    , sfFilterPattern
    , sfRoleARN
    ) where

import           Network.AWS.CloudWatchLogs.Types.Product
import           Network.AWS.CloudWatchLogs.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2014-03-28@ of the Amazon CloudWatch Logs SDK.
data CloudWatchLogs

instance AWSService CloudWatchLogs where
    type Sg CloudWatchLogs = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "CloudWatchLogs"
            , _svcPrefix = "logs"
            , _svcVersion = "2014-03-28"
            , _svcEndpoint = defaultEndpoint svc
            , _svcPreflight = id
            , _svcTimeout = Just 70000000
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

-- | Returned if a parameter of the request is incorrectly specified.
_InvalidParameterException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
    _ServiceError . hasCode "InvalidParameterException"

-- | Prism for InvalidSequenceTokenException' errors.
_InvalidSequenceTokenException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidSequenceTokenException =
    _ServiceError . hasCode "InvalidSequenceTokenException"

-- | Returned if the specified resource already exists.
_ResourceAlreadyExistsException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException =
    _ServiceError . hasCode "ResourceAlreadyExistsException"

-- | Returned if multiple requests to update the same resource were in
-- conflict.
_OperationAbortedException :: AWSError a => Getting (First ServiceError) a ServiceError
_OperationAbortedException =
    _ServiceError . hasCode "OperationAbortedException"

-- | Prism for DataAlreadyAcceptedException' errors.
_DataAlreadyAcceptedException :: AWSError a => Getting (First ServiceError) a ServiceError
_DataAlreadyAcceptedException =
    _ServiceError . hasCode "DataAlreadyAcceptedException"

-- | Returned if the service cannot complete the request.
_ServiceUnavailableException :: AWSError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
    _ServiceError . hasCode "ServiceUnavailableException"

-- | Returned if the specified resource does not exist.
_ResourceNotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasCode "ResourceNotFoundException"

-- | Returned if you have reached the maximum number of resources that can be
-- created.
_LimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"
