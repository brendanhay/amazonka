{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types
    (
    -- * Service Configuration
      cloudWatchLogs

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

    -- * Destination
    , Destination
    , destination
    , dTargetARN
    , dCreationTime
    , dArn
    , dAccessPolicy
    , dDestinationName
    , dRoleARN

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

-- | API version '2014-03-28' of the Amazon CloudWatch Logs SDK configuration.
cloudWatchLogs :: Service
cloudWatchLogs =
    Service
    { _svcAbbrev = "CloudWatchLogs"
    , _svcSigner = v4
    , _svcPrefix = "logs"
    , _svcVersion = "2014-03-28"
    , _svcEndpoint = defaultEndpoint cloudWatchLogs
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError
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
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | Returned if a parameter of the request is incorrectly specified.
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
    _ServiceError . hasCode "InvalidParameterException"

-- | Prism for InvalidSequenceTokenException' errors.
_InvalidSequenceTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSequenceTokenException =
    _ServiceError . hasCode "InvalidSequenceTokenException"

-- | Returned if the specified resource already exists.
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException =
    _ServiceError . hasCode "ResourceAlreadyExistsException"

-- | Returned if multiple requests to update the same resource were in
-- conflict.
_OperationAbortedException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationAbortedException =
    _ServiceError . hasCode "OperationAbortedException"

-- | Prism for DataAlreadyAcceptedException' errors.
_DataAlreadyAcceptedException :: AsError a => Getting (First ServiceError) a ServiceError
_DataAlreadyAcceptedException =
    _ServiceError . hasCode "DataAlreadyAcceptedException"

-- | Returned if the service cannot complete the request.
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
    _ServiceError . hasCode "ServiceUnavailableException"

-- | Returned if the specified resource does not exist.
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasCode "ResourceNotFoundException"

-- | Returned if you have reached the maximum number of resources that can be
-- created.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"
