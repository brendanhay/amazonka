{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SavingsPlans.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SavingsPlans.Lens
  ( -- * Operations

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribeSavingsPlanRates
    describeSavingsPlanRates_filters,
    describeSavingsPlanRates_nextToken,
    describeSavingsPlanRates_maxResults,
    describeSavingsPlanRates_savingsPlanId,
    describeSavingsPlanRatesResponse_searchResults,
    describeSavingsPlanRatesResponse_savingsPlanId,
    describeSavingsPlanRatesResponse_nextToken,
    describeSavingsPlanRatesResponse_httpStatus,

    -- ** DeleteQueuedSavingsPlan
    deleteQueuedSavingsPlan_savingsPlanId,
    deleteQueuedSavingsPlanResponse_httpStatus,

    -- ** CreateSavingsPlan
    createSavingsPlan_clientToken,
    createSavingsPlan_purchaseTime,
    createSavingsPlan_upfrontPaymentAmount,
    createSavingsPlan_tags,
    createSavingsPlan_savingsPlanOfferingId,
    createSavingsPlan_commitment,
    createSavingsPlanResponse_savingsPlanId,
    createSavingsPlanResponse_httpStatus,

    -- ** DescribeSavingsPlansOfferings
    describeSavingsPlansOfferings_serviceCodes,
    describeSavingsPlansOfferings_productType,
    describeSavingsPlansOfferings_filters,
    describeSavingsPlansOfferings_offeringIds,
    describeSavingsPlansOfferings_currencies,
    describeSavingsPlansOfferings_nextToken,
    describeSavingsPlansOfferings_paymentOptions,
    describeSavingsPlansOfferings_descriptions,
    describeSavingsPlansOfferings_durations,
    describeSavingsPlansOfferings_planTypes,
    describeSavingsPlansOfferings_usageTypes,
    describeSavingsPlansOfferings_operations,
    describeSavingsPlansOfferings_maxResults,
    describeSavingsPlansOfferingsResponse_searchResults,
    describeSavingsPlansOfferingsResponse_nextToken,
    describeSavingsPlansOfferingsResponse_httpStatus,

    -- ** DescribeSavingsPlans
    describeSavingsPlans_states,
    describeSavingsPlans_savingsPlanIds,
    describeSavingsPlans_filters,
    describeSavingsPlans_nextToken,
    describeSavingsPlans_savingsPlanArns,
    describeSavingsPlans_maxResults,
    describeSavingsPlansResponse_savingsPlans,
    describeSavingsPlansResponse_nextToken,
    describeSavingsPlansResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DescribeSavingsPlansOfferingRates
    describeSavingsPlansOfferingRates_savingsPlanOfferingIds,
    describeSavingsPlansOfferingRates_serviceCodes,
    describeSavingsPlansOfferingRates_filters,
    describeSavingsPlansOfferingRates_nextToken,
    describeSavingsPlansOfferingRates_savingsPlanTypes,
    describeSavingsPlansOfferingRates_products,
    describeSavingsPlansOfferingRates_savingsPlanPaymentOptions,
    describeSavingsPlansOfferingRates_usageTypes,
    describeSavingsPlansOfferingRates_operations,
    describeSavingsPlansOfferingRates_maxResults,
    describeSavingsPlansOfferingRatesResponse_searchResults,
    describeSavingsPlansOfferingRatesResponse_nextToken,
    describeSavingsPlansOfferingRatesResponse_httpStatus,

    -- * Types

    -- ** ParentSavingsPlanOffering
    parentSavingsPlanOffering_currency,
    parentSavingsPlanOffering_durationSeconds,
    parentSavingsPlanOffering_offeringId,
    parentSavingsPlanOffering_planDescription,
    parentSavingsPlanOffering_planType,
    parentSavingsPlanOffering_paymentOption,

    -- ** SavingsPlan
    savingsPlan_termDurationInSeconds,
    savingsPlan_savingsPlanType,
    savingsPlan_state,
    savingsPlan_productTypes,
    savingsPlan_start,
    savingsPlan_ec2InstanceFamily,
    savingsPlan_savingsPlanId,
    savingsPlan_recurringPaymentAmount,
    savingsPlan_currency,
    savingsPlan_end,
    savingsPlan_upfrontPaymentAmount,
    savingsPlan_offeringId,
    savingsPlan_region,
    savingsPlan_commitment,
    savingsPlan_description,
    savingsPlan_paymentOption,
    savingsPlan_savingsPlanArn,
    savingsPlan_tags,

    -- ** SavingsPlanFilter
    savingsPlanFilter_values,
    savingsPlanFilter_name,

    -- ** SavingsPlanOffering
    savingsPlanOffering_operation,
    savingsPlanOffering_usageType,
    savingsPlanOffering_productTypes,
    savingsPlanOffering_currency,
    savingsPlanOffering_durationSeconds,
    savingsPlanOffering_offeringId,
    savingsPlanOffering_serviceCode,
    savingsPlanOffering_planType,
    savingsPlanOffering_description,
    savingsPlanOffering_paymentOption,
    savingsPlanOffering_properties,

    -- ** SavingsPlanOfferingFilterElement
    savingsPlanOfferingFilterElement_values,
    savingsPlanOfferingFilterElement_name,

    -- ** SavingsPlanOfferingProperty
    savingsPlanOfferingProperty_value,
    savingsPlanOfferingProperty_name,

    -- ** SavingsPlanOfferingRate
    savingsPlanOfferingRate_operation,
    savingsPlanOfferingRate_usageType,
    savingsPlanOfferingRate_productType,
    savingsPlanOfferingRate_savingsPlanOffering,
    savingsPlanOfferingRate_rate,
    savingsPlanOfferingRate_serviceCode,
    savingsPlanOfferingRate_unit,
    savingsPlanOfferingRate_properties,

    -- ** SavingsPlanOfferingRateFilterElement
    savingsPlanOfferingRateFilterElement_values,
    savingsPlanOfferingRateFilterElement_name,

    -- ** SavingsPlanOfferingRateProperty
    savingsPlanOfferingRateProperty_value,
    savingsPlanOfferingRateProperty_name,

    -- ** SavingsPlanRate
    savingsPlanRate_operation,
    savingsPlanRate_usageType,
    savingsPlanRate_productType,
    savingsPlanRate_currency,
    savingsPlanRate_rate,
    savingsPlanRate_serviceCode,
    savingsPlanRate_unit,
    savingsPlanRate_properties,

    -- ** SavingsPlanRateFilter
    savingsPlanRateFilter_values,
    savingsPlanRateFilter_name,

    -- ** SavingsPlanRateProperty
    savingsPlanRateProperty_value,
    savingsPlanRateProperty_name,
  )
where

import Network.AWS.SavingsPlans.CreateSavingsPlan
import Network.AWS.SavingsPlans.DeleteQueuedSavingsPlan
import Network.AWS.SavingsPlans.DescribeSavingsPlanRates
import Network.AWS.SavingsPlans.DescribeSavingsPlans
import Network.AWS.SavingsPlans.DescribeSavingsPlansOfferingRates
import Network.AWS.SavingsPlans.DescribeSavingsPlansOfferings
import Network.AWS.SavingsPlans.ListTagsForResource
import Network.AWS.SavingsPlans.TagResource
import Network.AWS.SavingsPlans.Types.ParentSavingsPlanOffering
import Network.AWS.SavingsPlans.Types.SavingsPlan
import Network.AWS.SavingsPlans.Types.SavingsPlanFilter
import Network.AWS.SavingsPlans.Types.SavingsPlanOffering
import Network.AWS.SavingsPlans.Types.SavingsPlanOfferingFilterElement
import Network.AWS.SavingsPlans.Types.SavingsPlanOfferingProperty
import Network.AWS.SavingsPlans.Types.SavingsPlanOfferingRate
import Network.AWS.SavingsPlans.Types.SavingsPlanOfferingRateFilterElement
import Network.AWS.SavingsPlans.Types.SavingsPlanOfferingRateProperty
import Network.AWS.SavingsPlans.Types.SavingsPlanRate
import Network.AWS.SavingsPlans.Types.SavingsPlanRateFilter
import Network.AWS.SavingsPlans.Types.SavingsPlanRateProperty
import Network.AWS.SavingsPlans.UntagResource
