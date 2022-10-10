{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SavingsPlans.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Lens
  ( -- * Operations

    -- ** CreateSavingsPlan
    createSavingsPlan_tags,
    createSavingsPlan_upfrontPaymentAmount,
    createSavingsPlan_clientToken,
    createSavingsPlan_purchaseTime,
    createSavingsPlan_savingsPlanOfferingId,
    createSavingsPlan_commitment,
    createSavingsPlanResponse_savingsPlanId,
    createSavingsPlanResponse_httpStatus,

    -- ** DeleteQueuedSavingsPlan
    deleteQueuedSavingsPlan_savingsPlanId,
    deleteQueuedSavingsPlanResponse_httpStatus,

    -- ** DescribeSavingsPlanRates
    describeSavingsPlanRates_nextToken,
    describeSavingsPlanRates_filters,
    describeSavingsPlanRates_maxResults,
    describeSavingsPlanRates_savingsPlanId,
    describeSavingsPlanRatesResponse_nextToken,
    describeSavingsPlanRatesResponse_savingsPlanId,
    describeSavingsPlanRatesResponse_searchResults,
    describeSavingsPlanRatesResponse_httpStatus,

    -- ** DescribeSavingsPlans
    describeSavingsPlans_nextToken,
    describeSavingsPlans_savingsPlanArns,
    describeSavingsPlans_filters,
    describeSavingsPlans_maxResults,
    describeSavingsPlans_savingsPlanIds,
    describeSavingsPlans_states,
    describeSavingsPlansResponse_nextToken,
    describeSavingsPlansResponse_savingsPlans,
    describeSavingsPlansResponse_httpStatus,

    -- ** DescribeSavingsPlansOfferingRates
    describeSavingsPlansOfferingRates_nextToken,
    describeSavingsPlansOfferingRates_usageTypes,
    describeSavingsPlansOfferingRates_operations,
    describeSavingsPlansOfferingRates_filters,
    describeSavingsPlansOfferingRates_savingsPlanOfferingIds,
    describeSavingsPlansOfferingRates_savingsPlanPaymentOptions,
    describeSavingsPlansOfferingRates_products,
    describeSavingsPlansOfferingRates_maxResults,
    describeSavingsPlansOfferingRates_savingsPlanTypes,
    describeSavingsPlansOfferingRates_serviceCodes,
    describeSavingsPlansOfferingRatesResponse_nextToken,
    describeSavingsPlansOfferingRatesResponse_searchResults,
    describeSavingsPlansOfferingRatesResponse_httpStatus,

    -- ** DescribeSavingsPlansOfferings
    describeSavingsPlansOfferings_nextToken,
    describeSavingsPlansOfferings_usageTypes,
    describeSavingsPlansOfferings_operations,
    describeSavingsPlansOfferings_productType,
    describeSavingsPlansOfferings_filters,
    describeSavingsPlansOfferings_descriptions,
    describeSavingsPlansOfferings_currencies,
    describeSavingsPlansOfferings_maxResults,
    describeSavingsPlansOfferings_durations,
    describeSavingsPlansOfferings_offeringIds,
    describeSavingsPlansOfferings_paymentOptions,
    describeSavingsPlansOfferings_planTypes,
    describeSavingsPlansOfferings_serviceCodes,
    describeSavingsPlansOfferingsResponse_nextToken,
    describeSavingsPlansOfferingsResponse_searchResults,
    describeSavingsPlansOfferingsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** ParentSavingsPlanOffering
    parentSavingsPlanOffering_planType,
    parentSavingsPlanOffering_durationSeconds,
    parentSavingsPlanOffering_currency,
    parentSavingsPlanOffering_offeringId,
    parentSavingsPlanOffering_paymentOption,
    parentSavingsPlanOffering_planDescription,

    -- ** SavingsPlan
    savingsPlan_tags,
    savingsPlan_upfrontPaymentAmount,
    savingsPlan_productTypes,
    savingsPlan_recurringPaymentAmount,
    savingsPlan_savingsPlanId,
    savingsPlan_start,
    savingsPlan_state,
    savingsPlan_commitment,
    savingsPlan_description,
    savingsPlan_end,
    savingsPlan_region,
    savingsPlan_ec2InstanceFamily,
    savingsPlan_savingsPlanType,
    savingsPlan_currency,
    savingsPlan_termDurationInSeconds,
    savingsPlan_offeringId,
    savingsPlan_savingsPlanArn,
    savingsPlan_paymentOption,

    -- ** SavingsPlanFilter
    savingsPlanFilter_name,
    savingsPlanFilter_values,

    -- ** SavingsPlanOffering
    savingsPlanOffering_productTypes,
    savingsPlanOffering_planType,
    savingsPlanOffering_properties,
    savingsPlanOffering_serviceCode,
    savingsPlanOffering_usageType,
    savingsPlanOffering_description,
    savingsPlanOffering_durationSeconds,
    savingsPlanOffering_currency,
    savingsPlanOffering_offeringId,
    savingsPlanOffering_paymentOption,
    savingsPlanOffering_operation,

    -- ** SavingsPlanOfferingFilterElement
    savingsPlanOfferingFilterElement_name,
    savingsPlanOfferingFilterElement_values,

    -- ** SavingsPlanOfferingProperty
    savingsPlanOfferingProperty_name,
    savingsPlanOfferingProperty_value,

    -- ** SavingsPlanOfferingRate
    savingsPlanOfferingRate_rate,
    savingsPlanOfferingRate_productType,
    savingsPlanOfferingRate_properties,
    savingsPlanOfferingRate_serviceCode,
    savingsPlanOfferingRate_usageType,
    savingsPlanOfferingRate_savingsPlanOffering,
    savingsPlanOfferingRate_unit,
    savingsPlanOfferingRate_operation,

    -- ** SavingsPlanOfferingRateFilterElement
    savingsPlanOfferingRateFilterElement_name,
    savingsPlanOfferingRateFilterElement_values,

    -- ** SavingsPlanOfferingRateProperty
    savingsPlanOfferingRateProperty_name,
    savingsPlanOfferingRateProperty_value,

    -- ** SavingsPlanRate
    savingsPlanRate_rate,
    savingsPlanRate_productType,
    savingsPlanRate_properties,
    savingsPlanRate_serviceCode,
    savingsPlanRate_usageType,
    savingsPlanRate_currency,
    savingsPlanRate_unit,
    savingsPlanRate_operation,

    -- ** SavingsPlanRateFilter
    savingsPlanRateFilter_name,
    savingsPlanRateFilter_values,

    -- ** SavingsPlanRateProperty
    savingsPlanRateProperty_name,
    savingsPlanRateProperty_value,
  )
where

import Amazonka.SavingsPlans.CreateSavingsPlan
import Amazonka.SavingsPlans.DeleteQueuedSavingsPlan
import Amazonka.SavingsPlans.DescribeSavingsPlanRates
import Amazonka.SavingsPlans.DescribeSavingsPlans
import Amazonka.SavingsPlans.DescribeSavingsPlansOfferingRates
import Amazonka.SavingsPlans.DescribeSavingsPlansOfferings
import Amazonka.SavingsPlans.ListTagsForResource
import Amazonka.SavingsPlans.TagResource
import Amazonka.SavingsPlans.Types.ParentSavingsPlanOffering
import Amazonka.SavingsPlans.Types.SavingsPlan
import Amazonka.SavingsPlans.Types.SavingsPlanFilter
import Amazonka.SavingsPlans.Types.SavingsPlanOffering
import Amazonka.SavingsPlans.Types.SavingsPlanOfferingFilterElement
import Amazonka.SavingsPlans.Types.SavingsPlanOfferingProperty
import Amazonka.SavingsPlans.Types.SavingsPlanOfferingRate
import Amazonka.SavingsPlans.Types.SavingsPlanOfferingRateFilterElement
import Amazonka.SavingsPlans.Types.SavingsPlanOfferingRateProperty
import Amazonka.SavingsPlans.Types.SavingsPlanRate
import Amazonka.SavingsPlans.Types.SavingsPlanRateFilter
import Amazonka.SavingsPlans.Types.SavingsPlanRateProperty
import Amazonka.SavingsPlans.UntagResource
