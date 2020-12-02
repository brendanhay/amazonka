{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types
    (
    -- * Service Configuration
      xRay

    -- * Errors
    , _InvalidRequestException
    , _ThrottledException

    -- * EncryptionStatus
    , EncryptionStatus (..)

    -- * EncryptionType
    , EncryptionType (..)

    -- * Alias
    , Alias
    , alias
    , aNames
    , aName
    , aType

    -- * AnnotationValue
    , AnnotationValue
    , annotationValue
    , avNumberValue
    , avStringValue
    , avBooleanValue

    -- * BackendConnectionErrors
    , BackendConnectionErrors
    , backendConnectionErrors
    , bceOtherCount
    , bceTimeoutCount
    , bceHTTPCode5XXCount
    , bceConnectionRefusedCount
    , bceHTTPCode4XXCount
    , bceUnknownHostCount

    -- * Edge
    , Edge
    , edge
    , eStartTime
    , eAliases
    , eResponseTimeHistogram
    , eReferenceId
    , eEndTime
    , eSummaryStatistics

    -- * EdgeStatistics
    , EdgeStatistics
    , edgeStatistics
    , esFaultStatistics
    , esOKCount
    , esTotalResponseTime
    , esErrorStatistics
    , esTotalCount

    -- * EncryptionConfig
    , EncryptionConfig
    , encryptionConfig
    , ecStatus
    , ecKeyId
    , ecType

    -- * ErrorStatistics
    , ErrorStatistics
    , errorStatistics
    , eOtherCount
    , eThrottleCount
    , eTotalCount

    -- * FaultStatistics
    , FaultStatistics
    , faultStatistics
    , fsOtherCount
    , fsTotalCount

    -- * HTTP
    , HTTP
    , hTTP
    , httpHTTPMethod
    , httpHTTPStatus
    , httpClientIP
    , httpUserAgent
    , httpHTTPURL

    -- * HistogramEntry
    , HistogramEntry
    , histogramEntry
    , heCount
    , heValue

    -- * Segment
    , Segment
    , segment
    , sDocument
    , sId

    -- * ServiceId
    , ServiceId
    , serviceId
    , siAccountId
    , siNames
    , siName
    , siType

    -- * ServiceInfo
    , ServiceInfo
    , serviceInfo
    , sState
    , sStartTime
    , sRoot
    , sResponseTimeHistogram
    , sDurationHistogram
    , sReferenceId
    , sAccountId
    , sNames
    , sName
    , sEndTime
    , sType
    , sEdges
    , sSummaryStatistics

    -- * ServiceStatistics
    , ServiceStatistics
    , serviceStatistics
    , ssFaultStatistics
    , ssOKCount
    , ssTotalResponseTime
    , ssErrorStatistics
    , ssTotalCount

    -- * TelemetryRecord
    , TelemetryRecord
    , telemetryRecord
    , trSegmentsReceivedCount
    , trSegmentsSentCount
    , trSegmentsSpilloverCount
    , trSegmentsRejectedCount
    , trBackendConnectionErrors
    , trTimestamp

    -- * Trace
    , Trace
    , trace
    , tId
    , tSegments
    , tDuration

    -- * TraceSummary
    , TraceSummary
    , traceSummary
    , tsAnnotations
    , tsHasThrottle
    , tsUsers
    , tsHasFault
    , tsServiceIds
    , tsIsPartial
    , tsHasError
    , tsId
    , tsHTTP
    , tsDuration
    , tsResponseTime

    -- * TraceUser
    , TraceUser
    , traceUser
    , tuServiceIds
    , tuUserName

    -- * UnprocessedTraceSegment
    , UnprocessedTraceSegment
    , unprocessedTraceSegment
    , utsErrorCode
    , utsId
    , utsMessage

    -- * ValueWithServiceIds
    , ValueWithServiceIds
    , valueWithServiceIds
    , vwsiServiceIds
    , vwsiAnnotationValue
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.XRay.Types.Product
import Network.AWS.XRay.Types.Sum

-- | API version @2016-04-12@ of the Amazon X-Ray SDK configuration.
xRay :: Service
xRay =
  Service
    { _svcAbbrev = "XRay"
    , _svcSigner = v4
    , _svcPrefix = "xray"
    , _svcVersion = "2016-04-12"
    , _svcEndpoint = defaultEndpoint xRay
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "XRay"
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


-- | The request is missing required parameters or has invalid parameters.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException = _MatchServiceError xRay "InvalidRequestException"


-- | The request exceeds the maximum number of requests per second.
--
--
_ThrottledException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottledException =
  _MatchServiceError xRay "ThrottledException" . hasStatus 429

