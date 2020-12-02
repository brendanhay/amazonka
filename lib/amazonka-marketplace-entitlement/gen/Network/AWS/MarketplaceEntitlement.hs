{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceEntitlement
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Marketplace Entitlement Service__
--
-- This reference provides descriptions of the AWS Marketplace Entitlement Service API.
--
-- AWS Marketplace Entitlement Service is used to determine the entitlement of a customer to a given product. An entitlement represents capacity in a product owned by the customer. For example, a customer might own some number of users or seats in an SaaS application or some amount of data capacity in a multi-tenant database.
--
-- __Getting Entitlement Records__
--
--     * /GetEntitlements/ - Gets the entitlements for a Marketplace product.
--
--
--
module Network.AWS.MarketplaceEntitlement
    (
    -- * Service Configuration
      marketplaceEntitlement

    -- * Errors
    -- $errors

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** ThrottlingException
    , _ThrottlingException

    -- ** InternalServiceErrorException
    , _InternalServiceErrorException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetEntitlements
    , module Network.AWS.MarketplaceEntitlement.GetEntitlements

    -- * Types

    -- ** GetEntitlementFilterName
    , GetEntitlementFilterName (..)

    -- ** Entitlement
    , Entitlement
    , entitlement
    , eDimension
    , eValue
    , eExpirationDate
    , eCustomerIdentifier
    , eProductCode

    -- ** EntitlementValue
    , EntitlementValue
    , entitlementValue
    , evIntegerValue
    , evDoubleValue
    , evStringValue
    , evBooleanValue
    ) where

import Network.AWS.MarketplaceEntitlement.GetEntitlements
import Network.AWS.MarketplaceEntitlement.Types
import Network.AWS.MarketplaceEntitlement.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'MarketplaceEntitlement'.
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
