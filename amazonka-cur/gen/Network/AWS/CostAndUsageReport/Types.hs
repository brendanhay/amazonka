{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DuplicateReportNameException,
    _InternalErrorException,
    _ReportLimitReachedException,
    _ValidationException,

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
    ReportDefinition (..),
    newReportDefinition,
    reportDefinition_additionalArtifacts,
    reportDefinition_reportVersioning,
    reportDefinition_refreshClosedReports,
    reportDefinition_reportName,
    reportDefinition_timeUnit,
    reportDefinition_format,
    reportDefinition_compression,
    reportDefinition_additionalSchemaElements,
    reportDefinition_s3Bucket,
    reportDefinition_s3Prefix,
    reportDefinition_s3Region,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostAndUsageReport.Types.AWSRegion
import Network.AWS.CostAndUsageReport.Types.AdditionalArtifact
import Network.AWS.CostAndUsageReport.Types.CompressionFormat
import Network.AWS.CostAndUsageReport.Types.ReportDefinition
import Network.AWS.CostAndUsageReport.Types.ReportFormat
import Network.AWS.CostAndUsageReport.Types.ReportVersioning
import Network.AWS.CostAndUsageReport.Types.SchemaElement
import Network.AWS.CostAndUsageReport.Types.TimeUnit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-01-06@ of the Amazon Cost and Usage Report Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "CostAndUsageReport",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "cur",
      Core._serviceSigningName = "cur",
      Core._serviceVersion = "2017-01-06",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CostAndUsageReport",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | A report with the specified name already exists in the account. Specify
-- a different report name.
_DuplicateReportNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateReportNameException =
  Core._MatchServiceError
    defaultService
    "DuplicateReportNameException"

-- | An error on the server occurred during the processing of your request.
-- Try again later.
_InternalErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError
    defaultService
    "InternalErrorException"

-- | This account already has five reports defined. To define a new report,
-- you must delete an existing report.
_ReportLimitReachedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReportLimitReachedException =
  Core._MatchServiceError
    defaultService
    "ReportLimitReachedException"

-- | The input fails to satisfy the constraints specified by an AWS service.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
