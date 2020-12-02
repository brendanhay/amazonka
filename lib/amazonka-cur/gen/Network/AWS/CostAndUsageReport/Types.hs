{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types
  ( -- * Service Configuration
    costAndUsageReport,

    -- * Errors

    -- * AWSRegion
    AWSRegion (..),

    -- * AdditionalArtifact
    AdditionalArtifact (..),

    -- * CompressionFormat
    CompressionFormat (..),

    -- * ReportFormat
    ReportFormat (..),

    -- * ReportVersioning
    ReportVersioning (..),

    -- * SchemaElement
    SchemaElement (..),

    -- * TimeUnit
    TimeUnit (..),

    -- * ReportDefinition
    ReportDefinition,
    reportDefinition,
    rdReportVersioning,
    rdAdditionalArtifacts,
    rdRefreshClosedReports,
    rdReportName,
    rdTimeUnit,
    rdFormat,
    rdCompression,
    rdAdditionalSchemaElements,
    rdS3Bucket,
    rdS3Prefix,
    rdS3Region,
  )
where

import Network.AWS.CostAndUsageReport.Types.AWSRegion
import Network.AWS.CostAndUsageReport.Types.AdditionalArtifact
import Network.AWS.CostAndUsageReport.Types.CompressionFormat
import Network.AWS.CostAndUsageReport.Types.ReportDefinition
import Network.AWS.CostAndUsageReport.Types.ReportFormat
import Network.AWS.CostAndUsageReport.Types.ReportVersioning
import Network.AWS.CostAndUsageReport.Types.SchemaElement
import Network.AWS.CostAndUsageReport.Types.TimeUnit
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-01-06@ of the Amazon Cost and Usage Report Service SDK configuration.
costAndUsageReport :: Service
costAndUsageReport =
  Service
    { _svcAbbrev = "CostAndUsageReport",
      _svcSigner = v4,
      _svcPrefix = "cur",
      _svcVersion = "2017-01-06",
      _svcEndpoint = defaultEndpoint costAndUsageReport,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "CostAndUsageReport",
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
