{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SavingsPlans
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-06-28@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Savings Plans are a pricing model that offer significant savings on AWS
-- usage (for example, on Amazon EC2 instances). You commit to a consistent
-- amount of usage, in USD per hour, for a term of 1 or 3 years, and
-- receive a lower price for that usage. For more information, see the
-- <https://docs.aws.amazon.com/savingsplans/latest/userguide/ AWS Savings Plans User Guide>.
module Amazonka.SavingsPlans
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateSavingsPlan
    CreateSavingsPlan (CreateSavingsPlan'),
    newCreateSavingsPlan,
    CreateSavingsPlanResponse (CreateSavingsPlanResponse'),
    newCreateSavingsPlanResponse,

    -- ** DeleteQueuedSavingsPlan
    DeleteQueuedSavingsPlan (DeleteQueuedSavingsPlan'),
    newDeleteQueuedSavingsPlan,
    DeleteQueuedSavingsPlanResponse (DeleteQueuedSavingsPlanResponse'),
    newDeleteQueuedSavingsPlanResponse,

    -- ** DescribeSavingsPlanRates
    DescribeSavingsPlanRates (DescribeSavingsPlanRates'),
    newDescribeSavingsPlanRates,
    DescribeSavingsPlanRatesResponse (DescribeSavingsPlanRatesResponse'),
    newDescribeSavingsPlanRatesResponse,

    -- ** DescribeSavingsPlans
    DescribeSavingsPlans (DescribeSavingsPlans'),
    newDescribeSavingsPlans,
    DescribeSavingsPlansResponse (DescribeSavingsPlansResponse'),
    newDescribeSavingsPlansResponse,

    -- ** DescribeSavingsPlansOfferingRates
    DescribeSavingsPlansOfferingRates (DescribeSavingsPlansOfferingRates'),
    newDescribeSavingsPlansOfferingRates,
    DescribeSavingsPlansOfferingRatesResponse (DescribeSavingsPlansOfferingRatesResponse'),
    newDescribeSavingsPlansOfferingRatesResponse,

    -- ** DescribeSavingsPlansOfferings
    DescribeSavingsPlansOfferings (DescribeSavingsPlansOfferings'),
    newDescribeSavingsPlansOfferings,
    DescribeSavingsPlansOfferingsResponse (DescribeSavingsPlansOfferingsResponse'),
    newDescribeSavingsPlansOfferingsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- * Types

    -- ** CurrencyCode
    CurrencyCode (..),

    -- ** SavingsPlanOfferingFilterAttribute
    SavingsPlanOfferingFilterAttribute (..),

    -- ** SavingsPlanOfferingPropertyKey
    SavingsPlanOfferingPropertyKey (..),

    -- ** SavingsPlanPaymentOption
    SavingsPlanPaymentOption (..),

    -- ** SavingsPlanProductType
    SavingsPlanProductType (..),

    -- ** SavingsPlanRateFilterAttribute
    SavingsPlanRateFilterAttribute (..),

    -- ** SavingsPlanRateFilterName
    SavingsPlanRateFilterName (..),

    -- ** SavingsPlanRatePropertyKey
    SavingsPlanRatePropertyKey (..),

    -- ** SavingsPlanRateServiceCode
    SavingsPlanRateServiceCode (..),

    -- ** SavingsPlanRateUnit
    SavingsPlanRateUnit (..),

    -- ** SavingsPlanState
    SavingsPlanState (..),

    -- ** SavingsPlanType
    SavingsPlanType (..),

    -- ** SavingsPlansFilterName
    SavingsPlansFilterName (..),

    -- ** ParentSavingsPlanOffering
    ParentSavingsPlanOffering (ParentSavingsPlanOffering'),
    newParentSavingsPlanOffering,

    -- ** SavingsPlan
    SavingsPlan (SavingsPlan'),
    newSavingsPlan,

    -- ** SavingsPlanFilter
    SavingsPlanFilter (SavingsPlanFilter'),
    newSavingsPlanFilter,

    -- ** SavingsPlanOffering
    SavingsPlanOffering (SavingsPlanOffering'),
    newSavingsPlanOffering,

    -- ** SavingsPlanOfferingFilterElement
    SavingsPlanOfferingFilterElement (SavingsPlanOfferingFilterElement'),
    newSavingsPlanOfferingFilterElement,

    -- ** SavingsPlanOfferingProperty
    SavingsPlanOfferingProperty (SavingsPlanOfferingProperty'),
    newSavingsPlanOfferingProperty,

    -- ** SavingsPlanOfferingRate
    SavingsPlanOfferingRate (SavingsPlanOfferingRate'),
    newSavingsPlanOfferingRate,

    -- ** SavingsPlanOfferingRateFilterElement
    SavingsPlanOfferingRateFilterElement (SavingsPlanOfferingRateFilterElement'),
    newSavingsPlanOfferingRateFilterElement,

    -- ** SavingsPlanOfferingRateProperty
    SavingsPlanOfferingRateProperty (SavingsPlanOfferingRateProperty'),
    newSavingsPlanOfferingRateProperty,

    -- ** SavingsPlanRate
    SavingsPlanRate (SavingsPlanRate'),
    newSavingsPlanRate,

    -- ** SavingsPlanRateFilter
    SavingsPlanRateFilter (SavingsPlanRateFilter'),
    newSavingsPlanRateFilter,

    -- ** SavingsPlanRateProperty
    SavingsPlanRateProperty (SavingsPlanRateProperty'),
    newSavingsPlanRateProperty,
  )
where

import Amazonka.SavingsPlans.CreateSavingsPlan
import Amazonka.SavingsPlans.DeleteQueuedSavingsPlan
import Amazonka.SavingsPlans.DescribeSavingsPlanRates
import Amazonka.SavingsPlans.DescribeSavingsPlans
import Amazonka.SavingsPlans.DescribeSavingsPlansOfferingRates
import Amazonka.SavingsPlans.DescribeSavingsPlansOfferings
import Amazonka.SavingsPlans.Lens
import Amazonka.SavingsPlans.ListTagsForResource
import Amazonka.SavingsPlans.TagResource
import Amazonka.SavingsPlans.Types
import Amazonka.SavingsPlans.UntagResource
import Amazonka.SavingsPlans.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SavingsPlans'.

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
