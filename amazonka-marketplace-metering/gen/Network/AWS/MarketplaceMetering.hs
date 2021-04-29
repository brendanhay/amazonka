{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Marketplace Metering Service
--
-- This reference provides descriptions of the low-level AWS Marketplace
-- Metering Service API.
--
-- AWS Marketplace sellers can use this API to submit usage data for custom
-- usage dimensions.
--
-- For information on the permissions you need to use this API, see
-- <https://docs.aws.amazon.com/marketplace/latest/userguide/iam-user-policy-for-aws-marketplace-actions.html AWS Marketing metering and entitlement API permissions>
-- in the /AWS Marketplace Seller Guide./
--
-- __Submitting Metering Records__
--
-- -   /MeterUsage/- Submits the metering record for a Marketplace product.
--     MeterUsage is called from an EC2 instance or a container running on
--     EKS or ECS.
--
-- -   /BatchMeterUsage/- Submits the metering record for a set of
--     customers. BatchMeterUsage is called from a software-as-a-service
--     (SaaS) application.
--
-- __Accepting New Customers__
--
-- -   /ResolveCustomer/- Called by a SaaS application during the
--     registration process. When a buyer visits your website during the
--     registration process, the buyer submits a Registration Token through
--     the browser. The Registration Token is resolved through this API to
--     obtain a CustomerIdentifier and Product Code.
--
-- __Entitlement and Metering for Paid Container Products__
--
-- -   Paid container software products sold through AWS Marketplace must
--     integrate with the AWS Marketplace Metering Service and call the
--     RegisterUsage operation for software entitlement and metering. Free
--     and BYOL products for Amazon ECS or Amazon EKS aren\'t required to
--     call RegisterUsage, but you can do so if you want to receive usage
--     data in your seller reports. For more information on using the
--     RegisterUsage operation, see
--     <https://docs.aws.amazon.com/marketplace/latest/userguide/container-based-products.html Container-Based Products>.
--
-- BatchMeterUsage API calls are captured by AWS CloudTrail. You can use
-- Cloudtrail to verify that the SaaS metering records that you sent are
-- accurate by searching for records with the eventName of BatchMeterUsage.
-- You can also use CloudTrail to audit records over time. For more
-- information, see the
-- /<http://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-concepts.html AWS CloudTrail User Guide>/
-- .
module Network.AWS.MarketplaceMetering
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** TimestampOutOfBoundsException
    _TimestampOutOfBoundsException,

    -- ** CustomerNotEntitledException
    _CustomerNotEntitledException,

    -- ** InvalidRegionException
    _InvalidRegionException,

    -- ** InvalidUsageDimensionException
    _InvalidUsageDimensionException,

    -- ** ExpiredTokenException
    _ExpiredTokenException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** DisabledApiException
    _DisabledApiException,

    -- ** InvalidTagException
    _InvalidTagException,

    -- ** DuplicateRequestException
    _DuplicateRequestException,

    -- ** InvalidCustomerIdentifierException
    _InvalidCustomerIdentifierException,

    -- ** PlatformNotSupportedException
    _PlatformNotSupportedException,

    -- ** InvalidProductCodeException
    _InvalidProductCodeException,

    -- ** InvalidUsageAllocationsException
    _InvalidUsageAllocationsException,

    -- ** InternalServiceErrorException
    _InternalServiceErrorException,

    -- ** InvalidEndpointRegionException
    _InvalidEndpointRegionException,

    -- ** InvalidTokenException
    _InvalidTokenException,

    -- ** InvalidPublicKeyVersionException
    _InvalidPublicKeyVersionException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** MeterUsage
    MeterUsage (MeterUsage'),
    newMeterUsage,
    MeterUsageResponse (MeterUsageResponse'),
    newMeterUsageResponse,

    -- ** RegisterUsage
    RegisterUsage (RegisterUsage'),
    newRegisterUsage,
    RegisterUsageResponse (RegisterUsageResponse'),
    newRegisterUsageResponse,

    -- ** ResolveCustomer
    ResolveCustomer (ResolveCustomer'),
    newResolveCustomer,
    ResolveCustomerResponse (ResolveCustomerResponse'),
    newResolveCustomerResponse,

    -- ** BatchMeterUsage
    BatchMeterUsage (BatchMeterUsage'),
    newBatchMeterUsage,
    BatchMeterUsageResponse (BatchMeterUsageResponse'),
    newBatchMeterUsageResponse,

    -- * Types

    -- ** UsageRecordResultStatus
    UsageRecordResultStatus (..),

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** UsageAllocation
    UsageAllocation (UsageAllocation'),
    newUsageAllocation,

    -- ** UsageRecord
    UsageRecord (UsageRecord'),
    newUsageRecord,

    -- ** UsageRecordResult
    UsageRecordResult (UsageRecordResult'),
    newUsageRecordResult,
  )
where

import Network.AWS.MarketplaceMetering.BatchMeterUsage
import Network.AWS.MarketplaceMetering.Lens
import Network.AWS.MarketplaceMetering.MeterUsage
import Network.AWS.MarketplaceMetering.RegisterUsage
import Network.AWS.MarketplaceMetering.ResolveCustomer
import Network.AWS.MarketplaceMetering.Types
import Network.AWS.MarketplaceMetering.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MarketplaceMetering'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
