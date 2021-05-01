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

import Network.AWS.CostAndUsageReport.Types.AWSRegion
import Network.AWS.CostAndUsageReport.Types.AdditionalArtifact
import Network.AWS.CostAndUsageReport.Types.CompressionFormat
import Network.AWS.CostAndUsageReport.Types.ReportDefinition
import Network.AWS.CostAndUsageReport.Types.ReportFormat
import Network.AWS.CostAndUsageReport.Types.ReportVersioning
import Network.AWS.CostAndUsageReport.Types.SchemaElement
import Network.AWS.CostAndUsageReport.Types.TimeUnit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-01-06@ of the Amazon Cost and Usage Report Service SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "CostAndUsageReport",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "cur",
      Prelude._svcVersion = "2017-01-06",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "CostAndUsageReport",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | A report with the specified name already exists in the account. Specify
-- a different report name.
_DuplicateReportNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DuplicateReportNameException =
  Prelude._MatchServiceError
    defaultService
    "DuplicateReportNameException"

-- | An error on the server occurred during the processing of your request.
-- Try again later.
_InternalErrorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalErrorException =
  Prelude._MatchServiceError
    defaultService
    "InternalErrorException"

-- | This account already has five reports defined. To define a new report,
-- you must delete an existing report.
_ReportLimitReachedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReportLimitReachedException =
  Prelude._MatchServiceError
    defaultService
    "ReportLimitReachedException"

-- | The input fails to satisfy the constraints specified by an AWS service.
_ValidationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ValidationException =
  Prelude._MatchServiceError
    defaultService
    "ValidationException"
