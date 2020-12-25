-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceAnalytics.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceAnalytics.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _MarketplaceCommerceAnalyticsException,

    -- * SnsTopicArn
    SnsTopicArn (..),

    -- * SupportDataSetType
    SupportDataSetType (..),

    -- * OptionalValue
    OptionalValue (..),

    -- * RoleNameArn
    RoleNameArn (..),

    -- * DestinationS3Prefix
    DestinationS3Prefix (..),

    -- * DataSetType
    DataSetType (..),

    -- * DestinationS3BucketName
    DestinationS3BucketName (..),

    -- * DataSetRequestId
    DataSetRequestId (..),

    -- * OptionalKey
    OptionalKey (..),
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceAnalytics.Types.DataSetRequestId
import Network.AWS.MarketplaceAnalytics.Types.DataSetType
import Network.AWS.MarketplaceAnalytics.Types.DestinationS3BucketName
import Network.AWS.MarketplaceAnalytics.Types.DestinationS3Prefix
import Network.AWS.MarketplaceAnalytics.Types.OptionalKey
import Network.AWS.MarketplaceAnalytics.Types.OptionalValue
import Network.AWS.MarketplaceAnalytics.Types.RoleNameArn
import Network.AWS.MarketplaceAnalytics.Types.SnsTopicArn
import Network.AWS.MarketplaceAnalytics.Types.SupportDataSetType
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-07-01@ of the Amazon Marketplace Commerce Analytics SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "MarketplaceAnalytics",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "marketplacecommerceanalytics",
      Core._svcVersion = "2015-07-01",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "MarketplaceAnalytics",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | This exception is thrown when an internal service error occurs.
_MarketplaceCommerceAnalyticsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MarketplaceCommerceAnalyticsException =
  Core._MatchServiceError
    mkServiceConfig
    "MarketplaceCommerceAnalyticsException"
{-# DEPRECATED _MarketplaceCommerceAnalyticsException "Use generic-lens or generic-optics instead." #-}
