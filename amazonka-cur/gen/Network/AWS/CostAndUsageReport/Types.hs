{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostAndUsageReport.Types
    (
    -- * Service Configuration
      costAndUsageReport

    -- * Errors
    , _ValidationException
    , _InternalErrorException
    , _DuplicateReportNameException
    , _ReportLimitReachedException

    -- * AWSRegion
    , AWSRegion (..)

    -- * AdditionalArtifact
    , AdditionalArtifact (..)

    -- * CompressionFormat
    , CompressionFormat (..)

    -- * ReportFormat
    , ReportFormat (..)

    -- * SchemaElement
    , SchemaElement (..)

    -- * TimeUnit
    , TimeUnit (..)

    -- * ReportDefinition
    , ReportDefinition
    , reportDefinition
    , rdAdditionalArtifacts
    , rdReportName
    , rdTimeUnit
    , rdFormat
    , rdCompression
    , rdAdditionalSchemaElements
    , rdS3Bucket
    , rdS3Prefix
    , rdS3Region
    ) where

import Network.AWS.CostAndUsageReport.Types.Product
import Network.AWS.CostAndUsageReport.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-01-06@ of the Amazon Cost and Usage Report Service SDK configuration.
costAndUsageReport :: Service
costAndUsageReport =
  Service
    { _svcAbbrev = "CostAndUsageReport"
    , _svcSigner = v4
    , _svcPrefix = "cur"
    , _svcVersion = "2017-01-06"
    , _svcEndpoint = defaultEndpoint costAndUsageReport
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "CostAndUsageReport"
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


-- | This exception is thrown when providing an invalid input. eg. Put a report preference with an invalid report name, or Delete a report preference with an empty report name.
_ValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationException =
  _MatchServiceError costAndUsageReport "ValidationException"


-- | This exception is thrown on a known dependency failure.
_InternalErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalErrorException =
  _MatchServiceError costAndUsageReport "InternalErrorException"


-- | This exception is thrown when putting a report preference with a name that already exists.
_DuplicateReportNameException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateReportNameException =
  _MatchServiceError costAndUsageReport "DuplicateReportNameException"


-- | This exception is thrown when the number of report preference reaches max limit. The max number is 5.
_ReportLimitReachedException :: AsError a => Getting (First ServiceError) a ServiceError
_ReportLimitReachedException =
  _MatchServiceError costAndUsageReport "ReportLimitReachedException"

