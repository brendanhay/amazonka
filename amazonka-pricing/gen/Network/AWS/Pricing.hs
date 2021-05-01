{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Price List Service API (AWS Price List Service) is a centralized and
-- convenient way to programmatically query Amazon Web Services for
-- services, products, and pricing information. The AWS Price List Service
-- uses standardized product attributes such as @Location@,
-- @Storage Class@, and @Operating System@, and provides prices at the SKU
-- level. You can use the AWS Price List Service to build cost control and
-- scenario planning tools, reconcile billing data, forecast future spend
-- for budgeting purposes, and provide cost benefit analysis that compare
-- your internal workloads with AWS.
--
-- Use @GetServices@ without a service code to retrieve the service codes
-- for all AWS services, then @GetServices@ with a service code to retreive
-- the attribute names for that service. After you have the service code
-- and attribute names, you can use @GetAttributeValues@ to see what values
-- are available for an attribute. With the service code and an attribute
-- name and value, you can use @GetProducts@ to find specific products that
-- you\'re interested in, such as an @AmazonEC2@ instance, with a
-- @Provisioned IOPS@ @volumeType@.
--
-- Service Endpoint
--
-- AWS Price List Service API provides the following two endpoints:
--
-- -   https:\/\/api.pricing.us-east-1.amazonaws.com
--
-- -   https:\/\/api.pricing.ap-south-1.amazonaws.com
module Network.AWS.Pricing
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotFoundException
    _NotFoundException,

    -- ** ExpiredNextTokenException
    _ExpiredNextTokenException,

    -- ** InternalErrorException
    _InternalErrorException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetProducts (Paginated)
    GetProducts (GetProducts'),
    newGetProducts,
    GetProductsResponse (GetProductsResponse'),
    newGetProductsResponse,

    -- ** DescribeServices (Paginated)
    DescribeServices (DescribeServices'),
    newDescribeServices,
    DescribeServicesResponse (DescribeServicesResponse'),
    newDescribeServicesResponse,

    -- ** GetAttributeValues (Paginated)
    GetAttributeValues (GetAttributeValues'),
    newGetAttributeValues,
    GetAttributeValuesResponse (GetAttributeValuesResponse'),
    newGetAttributeValuesResponse,

    -- * Types

    -- ** FilterType
    FilterType (..),

    -- ** AttributeValue
    AttributeValue (AttributeValue'),
    newAttributeValue,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** PricingService
    PricingService (PricingService'),
    newPricingService,
  )
where

import Network.AWS.Pricing.DescribeServices
import Network.AWS.Pricing.GetAttributeValues
import Network.AWS.Pricing.GetProducts
import Network.AWS.Pricing.Lens
import Network.AWS.Pricing.Types
import Network.AWS.Pricing.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Pricing'.

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
