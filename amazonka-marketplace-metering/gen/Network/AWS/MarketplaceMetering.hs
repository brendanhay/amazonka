{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
-- __Submitting Metering Records__
--
-- -   /MeterUsage/- Submits the metering record for a Marketplace product.
--
--
-- /See:/ <http://docs.aws.amazon.com/apigateway/api-reference/ AWS API Reference>
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

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** MeterUsage
    , module Network.AWS.MarketplaceMetering.MeterUsage

    -- * Types
    ) where

import           Network.AWS.MarketplaceMetering.MeterUsage
import           Network.AWS.MarketplaceMetering.Types
import           Network.AWS.MarketplaceMetering.Waiters

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
