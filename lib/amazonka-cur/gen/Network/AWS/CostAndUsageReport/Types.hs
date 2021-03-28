-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostAndUsageReport.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _ValidationException
    , _InternalErrorException
    , _DuplicateReportNameException
    , _ReportLimitReachedException

    -- * ReportVersioning
    , ReportVersioning (..)

    -- * TimeUnit
    , TimeUnit (..)

    -- * ReportName
    , ReportName (..)

    -- * GenericString
    , GenericString (..)

    -- * SchemaElement
    , SchemaElement (..)

    -- * AdditionalArtifact
    , AdditionalArtifact (..)

    -- * CompressionFormat
    , CompressionFormat (..)

    -- * AWSRegion
    , AWSRegion (..)

    -- * ReportFormat
    , ReportFormat (..)

    -- * S3Prefix
    , S3Prefix (..)

    -- * ReportDefinition
    , ReportDefinition (..)
    , mkReportDefinition
    , rdReportName
    , rdTimeUnit
    , rdFormat
    , rdCompression
    , rdAdditionalSchemaElements
    , rdS3Bucket
    , rdS3Prefix
    , rdS3Region
    , rdAdditionalArtifacts
    , rdRefreshClosedReports
    , rdReportVersioning

    -- * S3Bucket
    , S3Bucket (..)

    -- * NextToken
    , NextToken (..)

    -- * ResponseMessage
    , ResponseMessage (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.CostAndUsageReport.Types.ReportVersioning
  
  
  
import Network.AWS.CostAndUsageReport.Types.TimeUnit
  
  
import Network.AWS.CostAndUsageReport.Types.ReportName
  
import Network.AWS.CostAndUsageReport.Types.GenericString
  
import Network.AWS.CostAndUsageReport.Types.SchemaElement
  
import Network.AWS.CostAndUsageReport.Types.AdditionalArtifact
  
import Network.AWS.CostAndUsageReport.Types.CompressionFormat
  
import Network.AWS.CostAndUsageReport.Types.AWSRegion
  
import Network.AWS.CostAndUsageReport.Types.ReportFormat
  
import Network.AWS.CostAndUsageReport.Types.S3Prefix
  
  
import Network.AWS.CostAndUsageReport.Types.ReportDefinition
  
import Network.AWS.CostAndUsageReport.Types.S3Bucket
  
import Network.AWS.CostAndUsageReport.Types.NextToken
  
import Network.AWS.CostAndUsageReport.Types.ResponseMessage
  

-- | API version @2017-01-06@ of the Amazon Cost and Usage Report Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "CostAndUsageReport",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "cur",
                 Core._svcVersion = "2017-01-06", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "CostAndUsageReport",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The input fails to satisfy the constraints specified by an AWS service.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException
  = Core._MatchServiceError mkServiceConfig "ValidationException"
{-# INLINEABLE _ValidationException #-}
{-# DEPRECATED _ValidationException "Use generic-lens or generic-optics instead"  #-}

-- | An error on the server occurred during the processing of your request. Try again later.
_InternalErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalErrorException
  = Core._MatchServiceError mkServiceConfig "InternalErrorException"
{-# INLINEABLE _InternalErrorException #-}
{-# DEPRECATED _InternalErrorException "Use generic-lens or generic-optics instead"  #-}

-- | A report with the specified name already exists in the account. Specify a different report name.
_DuplicateReportNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateReportNameException
  = Core._MatchServiceError mkServiceConfig
      "DuplicateReportNameException"
{-# INLINEABLE _DuplicateReportNameException #-}
{-# DEPRECATED _DuplicateReportNameException "Use generic-lens or generic-optics instead"  #-}

-- | This account already has five reports defined. To define a new report, you must delete an existing report.
_ReportLimitReachedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReportLimitReachedException
  = Core._MatchServiceError mkServiceConfig
      "ReportLimitReachedException"
{-# INLINEABLE _ReportLimitReachedException #-}
{-# DEPRECATED _ReportLimitReachedException "Use generic-lens or generic-optics instead"  #-}
