{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceMetering.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _TimestampOutOfBoundsException,
    _CustomerNotEntitledException,
    _InvalidRegionException,
    _InvalidUsageDimensionException,
    _ExpiredTokenException,
    _ThrottlingException,
    _DisabledApiException,
    _InvalidTagException,
    _DuplicateRequestException,
    _InvalidCustomerIdentifierException,
    _PlatformNotSupportedException,
    _InvalidProductCodeException,
    _InvalidUsageAllocationsException,
    _InternalServiceErrorException,
    _InvalidEndpointRegionException,
    _InvalidTokenException,
    _InvalidPublicKeyVersionException,

    -- * UsageRecordResultStatus
    UsageRecordResultStatus (..),

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * UsageAllocation
    UsageAllocation (..),
    newUsageAllocation,
    usageAllocation_tags,
    usageAllocation_allocatedUsageQuantity,

    -- * UsageRecord
    UsageRecord (..),
    newUsageRecord,
    usageRecord_usageAllocations,
    usageRecord_quantity,
    usageRecord_timestamp,
    usageRecord_customerIdentifier,
    usageRecord_dimension,

    -- * UsageRecordResult
    UsageRecordResult (..),
    newUsageRecordResult,
    usageRecordResult_status,
    usageRecordResult_meteringRecordId,
    usageRecordResult_usageRecord,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceMetering.Types.Tag
import Network.AWS.MarketplaceMetering.Types.UsageAllocation
import Network.AWS.MarketplaceMetering.Types.UsageRecord
import Network.AWS.MarketplaceMetering.Types.UsageRecordResult
import Network.AWS.MarketplaceMetering.Types.UsageRecordResultStatus
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-01-14@ of the Amazon Marketplace Metering SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "MarketplaceMetering",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "metering.marketplace",
      Core._serviceSigningName = "aws-marketplace",
      Core._serviceVersion = "2016-01-14",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "MarketplaceMetering",
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

-- | The timestamp value passed in the meterUsage() is out of allowed range.
_TimestampOutOfBoundsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TimestampOutOfBoundsException =
  Core._MatchServiceError
    defaultService
    "TimestampOutOfBoundsException"

-- | Exception thrown when the customer does not have a valid subscription
-- for the product.
_CustomerNotEntitledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomerNotEntitledException =
  Core._MatchServiceError
    defaultService
    "CustomerNotEntitledException"

-- | RegisterUsage must be called in the same AWS Region the ECS task was
-- launched in. This prevents a container from hardcoding a Region (e.g.
-- withRegion(“us-east-1”) when calling RegisterUsage.
_InvalidRegionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRegionException =
  Core._MatchServiceError
    defaultService
    "InvalidRegionException"

-- | The usage dimension does not match one of the UsageDimensions associated
-- with products.
_InvalidUsageDimensionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidUsageDimensionException =
  Core._MatchServiceError
    defaultService
    "InvalidUsageDimensionException"

-- | The submitted registration token has expired. This can happen if the
-- buyer\'s browser takes too long to redirect to your page, the buyer has
-- resubmitted the registration token, or your application has held on to
-- the registration token for too long. Your SaaS registration website
-- should redeem this token as soon as it is submitted by the buyer\'s
-- browser.
_ExpiredTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExpiredTokenException =
  Core._MatchServiceError
    defaultService
    "ExpiredTokenException"

-- | The calls to the API are throttled.
_ThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The API is disabled in the Region.
_DisabledApiException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DisabledApiException =
  Core._MatchServiceError
    defaultService
    "DisabledApiException"

-- | The tag is invalid, or the number of tags is greater than 5.
_InvalidTagException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTagException =
  Core._MatchServiceError
    defaultService
    "InvalidTagException"

-- | A metering record has already been emitted by the same EC2 instance, ECS
-- task, or EKS pod for the given {usageDimension, timestamp} with a
-- different usageQuantity.
_DuplicateRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateRequestException =
  Core._MatchServiceError
    defaultService
    "DuplicateRequestException"

-- | You have metered usage for a CustomerIdentifier that does not exist.
_InvalidCustomerIdentifierException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCustomerIdentifierException =
  Core._MatchServiceError
    defaultService
    "InvalidCustomerIdentifierException"

-- | AWS Marketplace does not support metering usage from the underlying
-- platform. Currently, Amazon ECS, Amazon EKS, and AWS Fargate are
-- supported.
_PlatformNotSupportedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PlatformNotSupportedException =
  Core._MatchServiceError
    defaultService
    "PlatformNotSupportedException"

-- | The product code passed does not match the product code used for
-- publishing the product.
_InvalidProductCodeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidProductCodeException =
  Core._MatchServiceError
    defaultService
    "InvalidProductCodeException"

-- | The usage allocation objects are invalid, or the number of allocations
-- is greater than 500 for a single usage record.
_InvalidUsageAllocationsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidUsageAllocationsException =
  Core._MatchServiceError
    defaultService
    "InvalidUsageAllocationsException"

-- | An internal error has occurred. Retry your request. If the problem
-- persists, post a message with details on the AWS forums.
_InternalServiceErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServiceErrorException"

-- | The endpoint being called is in a AWS Region different from your EC2
-- instance, ECS task, or EKS pod. The Region of the Metering Service
-- endpoint and the AWS Region of the resource must match.
_InvalidEndpointRegionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidEndpointRegionException =
  Core._MatchServiceError
    defaultService
    "InvalidEndpointRegionException"

-- | Registration token is invalid.
_InvalidTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidTokenException"

-- | Public Key version is invalid.
_InvalidPublicKeyVersionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPublicKeyVersionException =
  Core._MatchServiceError
    defaultService
    "InvalidPublicKeyVersionException"
