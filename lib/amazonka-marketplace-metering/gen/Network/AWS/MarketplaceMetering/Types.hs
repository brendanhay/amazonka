-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceMetering.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _InvalidTagException,
    _InvalidEndpointRegionException,
    _InvalidProductCodeException,
    _InvalidUsageDimensionException,
    _PlatformNotSupportedException,
    _CustomerNotEntitledException,
    _DuplicateRequestException,
    _DisabledApiException,
    _TimestampOutOfBoundsException,
    _ThrottlingException,
    _InvalidPublicKeyVersionException,
    _InternalServiceErrorException,
    _InvalidTokenException,
    _InvalidUsageAllocationsException,
    _ExpiredTokenException,
    _InvalidRegionException,
    _InvalidCustomerIdentifierException,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * UsageDimension
    UsageDimension (..),

    -- * String
    String (..),

    -- * UsageRecordResultStatus
    UsageRecordResultStatus (..),

    -- * UsageAllocation
    UsageAllocation (..),
    mkUsageAllocation,
    uaAllocatedUsageQuantity,
    uaTags,

    -- * UsageRecord
    UsageRecord (..),
    mkUsageRecord,
    urTimestamp,
    urCustomerIdentifier,
    urDimension,
    urQuantity,
    urUsageAllocations,

    -- * UsageRecordResult
    UsageRecordResult (..),
    mkUsageRecordResult,
    urrMeteringRecordId,
    urrStatus,
    urrUsageRecord,

    -- * NonEmptyString
    NonEmptyString (..),

    -- * CustomerIdentifier
    CustomerIdentifier (..),

    -- * ProductCode
    ProductCode (..),

    -- * Nonce
    Nonce (..),

    -- * MeteringRecordId
    MeteringRecordId (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * Signature
    Signature (..),
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceMetering.Types.CustomerIdentifier
import Network.AWS.MarketplaceMetering.Types.Key
import Network.AWS.MarketplaceMetering.Types.MeteringRecordId
import Network.AWS.MarketplaceMetering.Types.NonEmptyString
import Network.AWS.MarketplaceMetering.Types.Nonce
import Network.AWS.MarketplaceMetering.Types.ProductCode
import Network.AWS.MarketplaceMetering.Types.Signature
import Network.AWS.MarketplaceMetering.Types.String
import Network.AWS.MarketplaceMetering.Types.Tag
import Network.AWS.MarketplaceMetering.Types.UsageAllocation
import Network.AWS.MarketplaceMetering.Types.UsageDimension
import Network.AWS.MarketplaceMetering.Types.UsageRecord
import Network.AWS.MarketplaceMetering.Types.UsageRecordResult
import Network.AWS.MarketplaceMetering.Types.UsageRecordResultStatus
import Network.AWS.MarketplaceMetering.Types.Value
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-01-14@ of the Amazon Marketplace Metering SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "MarketplaceMetering",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "metering.marketplace",
      Core._svcVersion = "2016-01-14",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "MarketplaceMetering",
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

-- | The tag is invalid, or the number of tags is greater than 5.
_InvalidTagException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTagException =
  Core._MatchServiceError mkServiceConfig "InvalidTagException"
{-# DEPRECATED _InvalidTagException "Use generic-lens or generic-optics instead." #-}

-- | The endpoint being called is in a AWS Region different from your EC2 instance, ECS task, or EKS pod. The Region of the Metering Service endpoint and the AWS Region of the resource must match.
_InvalidEndpointRegionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidEndpointRegionException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidEndpointRegionException"
{-# DEPRECATED _InvalidEndpointRegionException "Use generic-lens or generic-optics instead." #-}

-- | The product code passed does not match the product code used for publishing the product.
_InvalidProductCodeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidProductCodeException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidProductCodeException"
{-# DEPRECATED _InvalidProductCodeException "Use generic-lens or generic-optics instead." #-}

-- | The usage dimension does not match one of the UsageDimensions associated with products.
_InvalidUsageDimensionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidUsageDimensionException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidUsageDimensionException"
{-# DEPRECATED _InvalidUsageDimensionException "Use generic-lens or generic-optics instead." #-}

-- | AWS Marketplace does not support metering usage from the underlying platform. Currently, Amazon ECS, Amazon EKS, and AWS Fargate are supported.
_PlatformNotSupportedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PlatformNotSupportedException =
  Core._MatchServiceError
    mkServiceConfig
    "PlatformNotSupportedException"
{-# DEPRECATED _PlatformNotSupportedException "Use generic-lens or generic-optics instead." #-}

-- | Exception thrown when the customer does not have a valid subscription for the product.
_CustomerNotEntitledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomerNotEntitledException =
  Core._MatchServiceError
    mkServiceConfig
    "CustomerNotEntitledException"
{-# DEPRECATED _CustomerNotEntitledException "Use generic-lens or generic-optics instead." #-}

-- | A metering record has already been emitted by the same EC2 instance, ECS task, or EKS pod for the given {usageDimension, timestamp} with a different usageQuantity.
_DuplicateRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateRequestException =
  Core._MatchServiceError
    mkServiceConfig
    "DuplicateRequestException"
{-# DEPRECATED _DuplicateRequestException "Use generic-lens or generic-optics instead." #-}

-- | The API is disabled in the Region.
_DisabledApiException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DisabledApiException =
  Core._MatchServiceError mkServiceConfig "DisabledApiException"
{-# DEPRECATED _DisabledApiException "Use generic-lens or generic-optics instead." #-}

-- | The timestamp value passed in the meterUsage() is out of allowed range.
_TimestampOutOfBoundsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TimestampOutOfBoundsException =
  Core._MatchServiceError
    mkServiceConfig
    "TimestampOutOfBoundsException"
{-# DEPRECATED _TimestampOutOfBoundsException "Use generic-lens or generic-optics instead." #-}

-- | The calls to the API are throttled.
_ThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError mkServiceConfig "ThrottlingException"
{-# DEPRECATED _ThrottlingException "Use generic-lens or generic-optics instead." #-}

-- | Public Key version is invalid.
_InvalidPublicKeyVersionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPublicKeyVersionException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidPublicKeyVersionException"
{-# DEPRECATED _InvalidPublicKeyVersionException "Use generic-lens or generic-optics instead." #-}

-- | An internal error has occurred. Retry your request. If the problem persists, post a message with details on the AWS forums.
_InternalServiceErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceErrorException =
  Core._MatchServiceError
    mkServiceConfig
    "InternalServiceErrorException"
{-# DEPRECATED _InternalServiceErrorException "Use generic-lens or generic-optics instead." #-}

-- | Registration token is invalid.
_InvalidTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTokenException =
  Core._MatchServiceError mkServiceConfig "InvalidTokenException"
{-# DEPRECATED _InvalidTokenException "Use generic-lens or generic-optics instead." #-}

-- | The usage allocation objects are invalid, or the number of allocations is greater than 500 for a single usage record.
_InvalidUsageAllocationsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidUsageAllocationsException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidUsageAllocationsException"
{-# DEPRECATED _InvalidUsageAllocationsException "Use generic-lens or generic-optics instead." #-}

-- | The submitted registration token has expired. This can happen if the buyer's browser takes too long to redirect to your page, the buyer has resubmitted the registration token, or your application has held on to the registration token for too long. Your SaaS registration website should redeem this token as soon as it is submitted by the buyer's browser.
_ExpiredTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExpiredTokenException =
  Core._MatchServiceError mkServiceConfig "ExpiredTokenException"
{-# DEPRECATED _ExpiredTokenException "Use generic-lens or generic-optics instead." #-}

-- | RegisterUsage must be called in the same AWS Region the ECS task was launched in. This prevents a container from hardcoding a Region (e.g. withRegion(“us-east-1”) when calling RegisterUsage.
_InvalidRegionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRegionException =
  Core._MatchServiceError mkServiceConfig "InvalidRegionException"
{-# DEPRECATED _InvalidRegionException "Use generic-lens or generic-optics instead." #-}

-- | You have metered usage for a CustomerIdentifier that does not exist.
_InvalidCustomerIdentifierException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCustomerIdentifierException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidCustomerIdentifierException"
{-# DEPRECATED _InvalidCustomerIdentifierException "Use generic-lens or generic-optics instead." #-}
