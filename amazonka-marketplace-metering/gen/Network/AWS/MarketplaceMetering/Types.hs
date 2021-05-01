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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceMetering.Types.Tag
import Network.AWS.MarketplaceMetering.Types.UsageAllocation
import Network.AWS.MarketplaceMetering.Types.UsageRecord
import Network.AWS.MarketplaceMetering.Types.UsageRecordResult
import Network.AWS.MarketplaceMetering.Types.UsageRecordResultStatus
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-01-14@ of the Amazon Marketplace Metering SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "MarketplaceMetering",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "metering.marketplace",
      Prelude._svcVersion = "2016-01-14",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "MarketplaceMetering",
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

-- | The timestamp value passed in the meterUsage() is out of allowed range.
_TimestampOutOfBoundsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TimestampOutOfBoundsException =
  Prelude._MatchServiceError
    defaultService
    "TimestampOutOfBoundsException"

-- | Exception thrown when the customer does not have a valid subscription
-- for the product.
_CustomerNotEntitledException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CustomerNotEntitledException =
  Prelude._MatchServiceError
    defaultService
    "CustomerNotEntitledException"

-- | RegisterUsage must be called in the same AWS Region the ECS task was
-- launched in. This prevents a container from hardcoding a Region (e.g.
-- withRegion(“us-east-1”) when calling RegisterUsage.
_InvalidRegionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRegionException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRegionException"

-- | The usage dimension does not match one of the UsageDimensions associated
-- with products.
_InvalidUsageDimensionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidUsageDimensionException =
  Prelude._MatchServiceError
    defaultService
    "InvalidUsageDimensionException"

-- | The submitted registration token has expired. This can happen if the
-- buyer\'s browser takes too long to redirect to your page, the buyer has
-- resubmitted the registration token, or your application has held on to
-- the registration token for too long. Your SaaS registration website
-- should redeem this token as soon as it is submitted by the buyer\'s
-- browser.
_ExpiredTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ExpiredTokenException =
  Prelude._MatchServiceError
    defaultService
    "ExpiredTokenException"

-- | The calls to the API are throttled.
_ThrottlingException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ThrottlingException =
  Prelude._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The API is disabled in the Region.
_DisabledApiException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DisabledApiException =
  Prelude._MatchServiceError
    defaultService
    "DisabledApiException"

-- | The tag is invalid, or the number of tags is greater than 5.
_InvalidTagException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTagException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTagException"

-- | A metering record has already been emitted by the same EC2 instance, ECS
-- task, or EKS pod for the given {usageDimension, timestamp} with a
-- different usageQuantity.
_DuplicateRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DuplicateRequestException =
  Prelude._MatchServiceError
    defaultService
    "DuplicateRequestException"

-- | You have metered usage for a CustomerIdentifier that does not exist.
_InvalidCustomerIdentifierException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidCustomerIdentifierException =
  Prelude._MatchServiceError
    defaultService
    "InvalidCustomerIdentifierException"

-- | AWS Marketplace does not support metering usage from the underlying
-- platform. Currently, Amazon ECS, Amazon EKS, and AWS Fargate are
-- supported.
_PlatformNotSupportedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PlatformNotSupportedException =
  Prelude._MatchServiceError
    defaultService
    "PlatformNotSupportedException"

-- | The product code passed does not match the product code used for
-- publishing the product.
_InvalidProductCodeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidProductCodeException =
  Prelude._MatchServiceError
    defaultService
    "InvalidProductCodeException"

-- | The usage allocation objects are invalid, or the number of allocations
-- is greater than 500 for a single usage record.
_InvalidUsageAllocationsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidUsageAllocationsException =
  Prelude._MatchServiceError
    defaultService
    "InvalidUsageAllocationsException"

-- | An internal error has occurred. Retry your request. If the problem
-- persists, post a message with details on the AWS forums.
_InternalServiceErrorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServiceErrorException =
  Prelude._MatchServiceError
    defaultService
    "InternalServiceErrorException"

-- | The endpoint being called is in a AWS Region different from your EC2
-- instance, ECS task, or EKS pod. The Region of the Metering Service
-- endpoint and the AWS Region of the resource must match.
_InvalidEndpointRegionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidEndpointRegionException =
  Prelude._MatchServiceError
    defaultService
    "InvalidEndpointRegionException"

-- | Registration token is invalid.
_InvalidTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTokenException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTokenException"

-- | Public Key version is invalid.
_InvalidPublicKeyVersionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidPublicKeyVersionException =
  Prelude._MatchServiceError
    defaultService
    "InvalidPublicKeyVersionException"
