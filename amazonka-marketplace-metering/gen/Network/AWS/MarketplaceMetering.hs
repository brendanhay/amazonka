{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Marketplace Metering Service__
--
-- This reference provides descriptions of the low-level AWS Marketplace Metering Service API.
--
-- AWS Marketplace sellers can use this API to submit usage data for custom usage dimensions.
--
-- __Submitting Metering Records__
--
--     * /MeterUsage/ - Submits the metering record for a Marketplace product. MeterUsage is called from an EC2 instance.
--
--     * /BatchMeterUsage/ - Submits the metering record for a set of customers. BatchMeterUsage is called from a software-as-a-service (SaaS) application.
--
--
--
-- __Accepting New Customers__
--
--     * /ResolveCustomer/ - Called by a SaaS application during the registration process. When a buyer visits your website during the registration process, the buyer submits a Registration Token through the browser. The Registration Token is resolved through this API to obtain a CustomerIdentifier and Product Code.
--
--
--
module Network.AWS.MarketplaceMetering
    (
    -- * Service Configuration
      marketplaceMetering

    -- * Errors
    -- $errors

    -- ** InvalidEndpointRegionException
    , _InvalidEndpointRegionException

    -- ** InvalidProductCodeException
    , _InvalidProductCodeException

    -- ** InvalidUsageDimensionException
    , _InvalidUsageDimensionException

    -- ** DuplicateRequestException
    , _DuplicateRequestException

    -- ** TimestampOutOfBoundsException
    , _TimestampOutOfBoundsException

    -- ** ThrottlingException
    , _ThrottlingException

    -- ** InternalServiceErrorException
    , _InternalServiceErrorException

    -- ** InvalidTokenException
    , _InvalidTokenException

    -- ** ExpiredTokenException
    , _ExpiredTokenException

    -- ** InvalidCustomerIdentifierException
    , _InvalidCustomerIdentifierException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchMeterUsage
    , module Network.AWS.MarketplaceMetering.BatchMeterUsage

    -- ** ResolveCustomer
    , module Network.AWS.MarketplaceMetering.ResolveCustomer

    -- ** MeterUsage
    , module Network.AWS.MarketplaceMetering.MeterUsage

    -- * Types

    -- ** UsageRecordResultStatus
    , UsageRecordResultStatus (..)

    -- ** UsageRecord
    , UsageRecord
    , usageRecord
    , urTimestamp
    , urCustomerIdentifier
    , urDimension
    , urQuantity

    -- ** UsageRecordResult
    , UsageRecordResult
    , usageRecordResult
    , urrStatus
    , urrUsageRecord
    , urrMeteringRecordId
    ) where

import Network.AWS.MarketplaceMetering.BatchMeterUsage
import Network.AWS.MarketplaceMetering.MeterUsage
import Network.AWS.MarketplaceMetering.ResolveCustomer
import Network.AWS.MarketplaceMetering.Types
import Network.AWS.MarketplaceMetering.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'MarketplaceMetering'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
