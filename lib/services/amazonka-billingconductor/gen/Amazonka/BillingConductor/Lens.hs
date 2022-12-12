{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.BillingConductor.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Lens
  ( -- * Operations

    -- ** AssociateAccounts
    associateAccounts_arn,
    associateAccounts_accountIds,
    associateAccountsResponse_arn,
    associateAccountsResponse_httpStatus,

    -- ** AssociatePricingRules
    associatePricingRules_arn,
    associatePricingRules_pricingRuleArns,
    associatePricingRulesResponse_arn,
    associatePricingRulesResponse_httpStatus,

    -- ** BatchAssociateResourcesToCustomLineItem
    batchAssociateResourcesToCustomLineItem_billingPeriodRange,
    batchAssociateResourcesToCustomLineItem_targetArn,
    batchAssociateResourcesToCustomLineItem_resourceArns,
    batchAssociateResourcesToCustomLineItemResponse_failedAssociatedResources,
    batchAssociateResourcesToCustomLineItemResponse_successfullyAssociatedResources,
    batchAssociateResourcesToCustomLineItemResponse_httpStatus,

    -- ** BatchDisassociateResourcesFromCustomLineItem
    batchDisassociateResourcesFromCustomLineItem_billingPeriodRange,
    batchDisassociateResourcesFromCustomLineItem_targetArn,
    batchDisassociateResourcesFromCustomLineItem_resourceArns,
    batchDisassociateResourcesFromCustomLineItemResponse_failedDisassociatedResources,
    batchDisassociateResourcesFromCustomLineItemResponse_successfullyDisassociatedResources,
    batchDisassociateResourcesFromCustomLineItemResponse_httpStatus,

    -- ** CreateBillingGroup
    createBillingGroup_clientToken,
    createBillingGroup_description,
    createBillingGroup_primaryAccountId,
    createBillingGroup_tags,
    createBillingGroup_name,
    createBillingGroup_accountGrouping,
    createBillingGroup_computationPreference,
    createBillingGroupResponse_arn,
    createBillingGroupResponse_httpStatus,

    -- ** CreateCustomLineItem
    createCustomLineItem_billingPeriodRange,
    createCustomLineItem_clientToken,
    createCustomLineItem_tags,
    createCustomLineItem_name,
    createCustomLineItem_description,
    createCustomLineItem_billingGroupArn,
    createCustomLineItem_chargeDetails,
    createCustomLineItemResponse_arn,
    createCustomLineItemResponse_httpStatus,

    -- ** CreatePricingPlan
    createPricingPlan_clientToken,
    createPricingPlan_description,
    createPricingPlan_pricingRuleArns,
    createPricingPlan_tags,
    createPricingPlan_name,
    createPricingPlanResponse_arn,
    createPricingPlanResponse_httpStatus,

    -- ** CreatePricingRule
    createPricingRule_billingEntity,
    createPricingRule_clientToken,
    createPricingRule_description,
    createPricingRule_modifierPercentage,
    createPricingRule_service,
    createPricingRule_tags,
    createPricingRule_tiering,
    createPricingRule_name,
    createPricingRule_scope,
    createPricingRule_type,
    createPricingRuleResponse_arn,
    createPricingRuleResponse_httpStatus,

    -- ** DeleteBillingGroup
    deleteBillingGroup_arn,
    deleteBillingGroupResponse_arn,
    deleteBillingGroupResponse_httpStatus,

    -- ** DeleteCustomLineItem
    deleteCustomLineItem_billingPeriodRange,
    deleteCustomLineItem_arn,
    deleteCustomLineItemResponse_arn,
    deleteCustomLineItemResponse_httpStatus,

    -- ** DeletePricingPlan
    deletePricingPlan_arn,
    deletePricingPlanResponse_arn,
    deletePricingPlanResponse_httpStatus,

    -- ** DeletePricingRule
    deletePricingRule_arn,
    deletePricingRuleResponse_arn,
    deletePricingRuleResponse_httpStatus,

    -- ** DisassociateAccounts
    disassociateAccounts_arn,
    disassociateAccounts_accountIds,
    disassociateAccountsResponse_arn,
    disassociateAccountsResponse_httpStatus,

    -- ** DisassociatePricingRules
    disassociatePricingRules_arn,
    disassociatePricingRules_pricingRuleArns,
    disassociatePricingRulesResponse_arn,
    disassociatePricingRulesResponse_httpStatus,

    -- ** ListAccountAssociations
    listAccountAssociations_billingPeriod,
    listAccountAssociations_filters,
    listAccountAssociations_nextToken,
    listAccountAssociationsResponse_linkedAccounts,
    listAccountAssociationsResponse_nextToken,
    listAccountAssociationsResponse_httpStatus,

    -- ** ListBillingGroupCostReports
    listBillingGroupCostReports_billingPeriod,
    listBillingGroupCostReports_filters,
    listBillingGroupCostReports_maxResults,
    listBillingGroupCostReports_nextToken,
    listBillingGroupCostReportsResponse_billingGroupCostReports,
    listBillingGroupCostReportsResponse_nextToken,
    listBillingGroupCostReportsResponse_httpStatus,

    -- ** ListBillingGroups
    listBillingGroups_billingPeriod,
    listBillingGroups_filters,
    listBillingGroups_maxResults,
    listBillingGroups_nextToken,
    listBillingGroupsResponse_billingGroups,
    listBillingGroupsResponse_nextToken,
    listBillingGroupsResponse_httpStatus,

    -- ** ListCustomLineItemVersions
    listCustomLineItemVersions_filters,
    listCustomLineItemVersions_maxResults,
    listCustomLineItemVersions_nextToken,
    listCustomLineItemVersions_arn,
    listCustomLineItemVersionsResponse_customLineItemVersions,
    listCustomLineItemVersionsResponse_nextToken,
    listCustomLineItemVersionsResponse_httpStatus,

    -- ** ListCustomLineItems
    listCustomLineItems_billingPeriod,
    listCustomLineItems_filters,
    listCustomLineItems_maxResults,
    listCustomLineItems_nextToken,
    listCustomLineItemsResponse_customLineItems,
    listCustomLineItemsResponse_nextToken,
    listCustomLineItemsResponse_httpStatus,

    -- ** ListPricingPlans
    listPricingPlans_billingPeriod,
    listPricingPlans_filters,
    listPricingPlans_maxResults,
    listPricingPlans_nextToken,
    listPricingPlansResponse_billingPeriod,
    listPricingPlansResponse_nextToken,
    listPricingPlansResponse_pricingPlans,
    listPricingPlansResponse_httpStatus,

    -- ** ListPricingPlansAssociatedWithPricingRule
    listPricingPlansAssociatedWithPricingRule_billingPeriod,
    listPricingPlansAssociatedWithPricingRule_maxResults,
    listPricingPlansAssociatedWithPricingRule_nextToken,
    listPricingPlansAssociatedWithPricingRule_pricingRuleArn,
    listPricingPlansAssociatedWithPricingRuleResponse_billingPeriod,
    listPricingPlansAssociatedWithPricingRuleResponse_nextToken,
    listPricingPlansAssociatedWithPricingRuleResponse_pricingPlanArns,
    listPricingPlansAssociatedWithPricingRuleResponse_pricingRuleArn,
    listPricingPlansAssociatedWithPricingRuleResponse_httpStatus,

    -- ** ListPricingRules
    listPricingRules_billingPeriod,
    listPricingRules_filters,
    listPricingRules_maxResults,
    listPricingRules_nextToken,
    listPricingRulesResponse_billingPeriod,
    listPricingRulesResponse_nextToken,
    listPricingRulesResponse_pricingRules,
    listPricingRulesResponse_httpStatus,

    -- ** ListPricingRulesAssociatedToPricingPlan
    listPricingRulesAssociatedToPricingPlan_billingPeriod,
    listPricingRulesAssociatedToPricingPlan_maxResults,
    listPricingRulesAssociatedToPricingPlan_nextToken,
    listPricingRulesAssociatedToPricingPlan_pricingPlanArn,
    listPricingRulesAssociatedToPricingPlanResponse_billingPeriod,
    listPricingRulesAssociatedToPricingPlanResponse_nextToken,
    listPricingRulesAssociatedToPricingPlanResponse_pricingPlanArn,
    listPricingRulesAssociatedToPricingPlanResponse_pricingRuleArns,
    listPricingRulesAssociatedToPricingPlanResponse_httpStatus,

    -- ** ListResourcesAssociatedToCustomLineItem
    listResourcesAssociatedToCustomLineItem_billingPeriod,
    listResourcesAssociatedToCustomLineItem_filters,
    listResourcesAssociatedToCustomLineItem_maxResults,
    listResourcesAssociatedToCustomLineItem_nextToken,
    listResourcesAssociatedToCustomLineItem_arn,
    listResourcesAssociatedToCustomLineItemResponse_arn,
    listResourcesAssociatedToCustomLineItemResponse_associatedResources,
    listResourcesAssociatedToCustomLineItemResponse_nextToken,
    listResourcesAssociatedToCustomLineItemResponse_httpStatus,

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

    -- ** UpdateBillingGroup
    updateBillingGroup_computationPreference,
    updateBillingGroup_description,
    updateBillingGroup_name,
    updateBillingGroup_status,
    updateBillingGroup_arn,
    updateBillingGroupResponse_arn,
    updateBillingGroupResponse_description,
    updateBillingGroupResponse_lastModifiedTime,
    updateBillingGroupResponse_name,
    updateBillingGroupResponse_pricingPlanArn,
    updateBillingGroupResponse_primaryAccountId,
    updateBillingGroupResponse_size,
    updateBillingGroupResponse_status,
    updateBillingGroupResponse_statusReason,
    updateBillingGroupResponse_httpStatus,

    -- ** UpdateCustomLineItem
    updateCustomLineItem_billingPeriodRange,
    updateCustomLineItem_chargeDetails,
    updateCustomLineItem_description,
    updateCustomLineItem_name,
    updateCustomLineItem_arn,
    updateCustomLineItemResponse_arn,
    updateCustomLineItemResponse_associationSize,
    updateCustomLineItemResponse_billingGroupArn,
    updateCustomLineItemResponse_chargeDetails,
    updateCustomLineItemResponse_description,
    updateCustomLineItemResponse_lastModifiedTime,
    updateCustomLineItemResponse_name,
    updateCustomLineItemResponse_httpStatus,

    -- ** UpdatePricingPlan
    updatePricingPlan_description,
    updatePricingPlan_name,
    updatePricingPlan_arn,
    updatePricingPlanResponse_arn,
    updatePricingPlanResponse_description,
    updatePricingPlanResponse_lastModifiedTime,
    updatePricingPlanResponse_name,
    updatePricingPlanResponse_size,
    updatePricingPlanResponse_httpStatus,

    -- ** UpdatePricingRule
    updatePricingRule_description,
    updatePricingRule_modifierPercentage,
    updatePricingRule_name,
    updatePricingRule_tiering,
    updatePricingRule_type,
    updatePricingRule_arn,
    updatePricingRuleResponse_arn,
    updatePricingRuleResponse_associatedPricingPlanCount,
    updatePricingRuleResponse_billingEntity,
    updatePricingRuleResponse_description,
    updatePricingRuleResponse_lastModifiedTime,
    updatePricingRuleResponse_modifierPercentage,
    updatePricingRuleResponse_name,
    updatePricingRuleResponse_scope,
    updatePricingRuleResponse_service,
    updatePricingRuleResponse_tiering,
    updatePricingRuleResponse_type,
    updatePricingRuleResponse_httpStatus,

    -- * Types

    -- ** AccountAssociationsListElement
    accountAssociationsListElement_accountEmail,
    accountAssociationsListElement_accountId,
    accountAssociationsListElement_accountName,
    accountAssociationsListElement_billingGroupArn,

    -- ** AccountGrouping
    accountGrouping_linkedAccountIds,

    -- ** AssociateResourceError
    associateResourceError_message,
    associateResourceError_reason,

    -- ** AssociateResourceResponseElement
    associateResourceResponseElement_arn,
    associateResourceResponseElement_error,

    -- ** BillingGroupCostReportElement
    billingGroupCostReportElement_aWSCost,
    billingGroupCostReportElement_arn,
    billingGroupCostReportElement_currency,
    billingGroupCostReportElement_margin,
    billingGroupCostReportElement_marginPercentage,
    billingGroupCostReportElement_proformaCost,

    -- ** BillingGroupListElement
    billingGroupListElement_arn,
    billingGroupListElement_computationPreference,
    billingGroupListElement_creationTime,
    billingGroupListElement_description,
    billingGroupListElement_lastModifiedTime,
    billingGroupListElement_name,
    billingGroupListElement_primaryAccountId,
    billingGroupListElement_size,
    billingGroupListElement_status,
    billingGroupListElement_statusReason,

    -- ** ComputationPreference
    computationPreference_pricingPlanArn,

    -- ** CreateFreeTierConfig
    createFreeTierConfig_activated,

    -- ** CreateTieringInput
    createTieringInput_freeTier,

    -- ** CustomLineItemBillingPeriodRange
    customLineItemBillingPeriodRange_exclusiveEndBillingPeriod,
    customLineItemBillingPeriodRange_inclusiveStartBillingPeriod,

    -- ** CustomLineItemChargeDetails
    customLineItemChargeDetails_flat,
    customLineItemChargeDetails_percentage,
    customLineItemChargeDetails_type,

    -- ** CustomLineItemFlatChargeDetails
    customLineItemFlatChargeDetails_chargeValue,

    -- ** CustomLineItemListElement
    customLineItemListElement_arn,
    customLineItemListElement_associationSize,
    customLineItemListElement_billingGroupArn,
    customLineItemListElement_chargeDetails,
    customLineItemListElement_creationTime,
    customLineItemListElement_currencyCode,
    customLineItemListElement_description,
    customLineItemListElement_lastModifiedTime,
    customLineItemListElement_name,
    customLineItemListElement_productCode,

    -- ** CustomLineItemPercentageChargeDetails
    customLineItemPercentageChargeDetails_associatedValues,
    customLineItemPercentageChargeDetails_percentageValue,

    -- ** CustomLineItemVersionListElement
    customLineItemVersionListElement_associationSize,
    customLineItemVersionListElement_billingGroupArn,
    customLineItemVersionListElement_chargeDetails,
    customLineItemVersionListElement_creationTime,
    customLineItemVersionListElement_currencyCode,
    customLineItemVersionListElement_description,
    customLineItemVersionListElement_endBillingPeriod,
    customLineItemVersionListElement_lastModifiedTime,
    customLineItemVersionListElement_name,
    customLineItemVersionListElement_productCode,
    customLineItemVersionListElement_startBillingPeriod,

    -- ** DisassociateResourceResponseElement
    disassociateResourceResponseElement_arn,
    disassociateResourceResponseElement_error,

    -- ** FreeTierConfig
    freeTierConfig_activated,

    -- ** ListAccountAssociationsFilter
    listAccountAssociationsFilter_accountId,
    listAccountAssociationsFilter_association,

    -- ** ListBillingGroupCostReportsFilter
    listBillingGroupCostReportsFilter_billingGroupArns,

    -- ** ListBillingGroupsFilter
    listBillingGroupsFilter_arns,
    listBillingGroupsFilter_pricingPlan,

    -- ** ListCustomLineItemChargeDetails
    listCustomLineItemChargeDetails_flat,
    listCustomLineItemChargeDetails_percentage,
    listCustomLineItemChargeDetails_type,

    -- ** ListCustomLineItemFlatChargeDetails
    listCustomLineItemFlatChargeDetails_chargeValue,

    -- ** ListCustomLineItemPercentageChargeDetails
    listCustomLineItemPercentageChargeDetails_percentageValue,

    -- ** ListCustomLineItemVersionsBillingPeriodRangeFilter
    listCustomLineItemVersionsBillingPeriodRangeFilter_endBillingPeriod,
    listCustomLineItemVersionsBillingPeriodRangeFilter_startBillingPeriod,

    -- ** ListCustomLineItemVersionsFilter
    listCustomLineItemVersionsFilter_billingPeriodRange,

    -- ** ListCustomLineItemsFilter
    listCustomLineItemsFilter_arns,
    listCustomLineItemsFilter_billingGroups,
    listCustomLineItemsFilter_names,

    -- ** ListPricingPlansFilter
    listPricingPlansFilter_arns,

    -- ** ListPricingRulesFilter
    listPricingRulesFilter_arns,

    -- ** ListResourcesAssociatedToCustomLineItemFilter
    listResourcesAssociatedToCustomLineItemFilter_relationship,

    -- ** ListResourcesAssociatedToCustomLineItemResponseElement
    listResourcesAssociatedToCustomLineItemResponseElement_arn,
    listResourcesAssociatedToCustomLineItemResponseElement_endBillingPeriod,
    listResourcesAssociatedToCustomLineItemResponseElement_relationship,

    -- ** PricingPlanListElement
    pricingPlanListElement_arn,
    pricingPlanListElement_creationTime,
    pricingPlanListElement_description,
    pricingPlanListElement_lastModifiedTime,
    pricingPlanListElement_name,
    pricingPlanListElement_size,

    -- ** PricingRuleListElement
    pricingRuleListElement_arn,
    pricingRuleListElement_associatedPricingPlanCount,
    pricingRuleListElement_billingEntity,
    pricingRuleListElement_creationTime,
    pricingRuleListElement_description,
    pricingRuleListElement_lastModifiedTime,
    pricingRuleListElement_modifierPercentage,
    pricingRuleListElement_name,
    pricingRuleListElement_scope,
    pricingRuleListElement_service,
    pricingRuleListElement_tiering,
    pricingRuleListElement_type,

    -- ** Tiering
    tiering_freeTier,

    -- ** UpdateCustomLineItemChargeDetails
    updateCustomLineItemChargeDetails_flat,
    updateCustomLineItemChargeDetails_percentage,

    -- ** UpdateCustomLineItemFlatChargeDetails
    updateCustomLineItemFlatChargeDetails_chargeValue,

    -- ** UpdateCustomLineItemPercentageChargeDetails
    updateCustomLineItemPercentageChargeDetails_percentageValue,

    -- ** UpdateFreeTierConfig
    updateFreeTierConfig_activated,

    -- ** UpdateTieringInput
    updateTieringInput_freeTier,
  )
where

import Amazonka.BillingConductor.AssociateAccounts
import Amazonka.BillingConductor.AssociatePricingRules
import Amazonka.BillingConductor.BatchAssociateResourcesToCustomLineItem
import Amazonka.BillingConductor.BatchDisassociateResourcesFromCustomLineItem
import Amazonka.BillingConductor.CreateBillingGroup
import Amazonka.BillingConductor.CreateCustomLineItem
import Amazonka.BillingConductor.CreatePricingPlan
import Amazonka.BillingConductor.CreatePricingRule
import Amazonka.BillingConductor.DeleteBillingGroup
import Amazonka.BillingConductor.DeleteCustomLineItem
import Amazonka.BillingConductor.DeletePricingPlan
import Amazonka.BillingConductor.DeletePricingRule
import Amazonka.BillingConductor.DisassociateAccounts
import Amazonka.BillingConductor.DisassociatePricingRules
import Amazonka.BillingConductor.ListAccountAssociations
import Amazonka.BillingConductor.ListBillingGroupCostReports
import Amazonka.BillingConductor.ListBillingGroups
import Amazonka.BillingConductor.ListCustomLineItemVersions
import Amazonka.BillingConductor.ListCustomLineItems
import Amazonka.BillingConductor.ListPricingPlans
import Amazonka.BillingConductor.ListPricingPlansAssociatedWithPricingRule
import Amazonka.BillingConductor.ListPricingRules
import Amazonka.BillingConductor.ListPricingRulesAssociatedToPricingPlan
import Amazonka.BillingConductor.ListResourcesAssociatedToCustomLineItem
import Amazonka.BillingConductor.ListTagsForResource
import Amazonka.BillingConductor.TagResource
import Amazonka.BillingConductor.Types.AccountAssociationsListElement
import Amazonka.BillingConductor.Types.AccountGrouping
import Amazonka.BillingConductor.Types.AssociateResourceError
import Amazonka.BillingConductor.Types.AssociateResourceResponseElement
import Amazonka.BillingConductor.Types.BillingGroupCostReportElement
import Amazonka.BillingConductor.Types.BillingGroupListElement
import Amazonka.BillingConductor.Types.ComputationPreference
import Amazonka.BillingConductor.Types.CreateFreeTierConfig
import Amazonka.BillingConductor.Types.CreateTieringInput
import Amazonka.BillingConductor.Types.CustomLineItemBillingPeriodRange
import Amazonka.BillingConductor.Types.CustomLineItemChargeDetails
import Amazonka.BillingConductor.Types.CustomLineItemFlatChargeDetails
import Amazonka.BillingConductor.Types.CustomLineItemListElement
import Amazonka.BillingConductor.Types.CustomLineItemPercentageChargeDetails
import Amazonka.BillingConductor.Types.CustomLineItemVersionListElement
import Amazonka.BillingConductor.Types.DisassociateResourceResponseElement
import Amazonka.BillingConductor.Types.FreeTierConfig
import Amazonka.BillingConductor.Types.ListAccountAssociationsFilter
import Amazonka.BillingConductor.Types.ListBillingGroupCostReportsFilter
import Amazonka.BillingConductor.Types.ListBillingGroupsFilter
import Amazonka.BillingConductor.Types.ListCustomLineItemChargeDetails
import Amazonka.BillingConductor.Types.ListCustomLineItemFlatChargeDetails
import Amazonka.BillingConductor.Types.ListCustomLineItemPercentageChargeDetails
import Amazonka.BillingConductor.Types.ListCustomLineItemVersionsBillingPeriodRangeFilter
import Amazonka.BillingConductor.Types.ListCustomLineItemVersionsFilter
import Amazonka.BillingConductor.Types.ListCustomLineItemsFilter
import Amazonka.BillingConductor.Types.ListPricingPlansFilter
import Amazonka.BillingConductor.Types.ListPricingRulesFilter
import Amazonka.BillingConductor.Types.ListResourcesAssociatedToCustomLineItemFilter
import Amazonka.BillingConductor.Types.ListResourcesAssociatedToCustomLineItemResponseElement
import Amazonka.BillingConductor.Types.PricingPlanListElement
import Amazonka.BillingConductor.Types.PricingRuleListElement
import Amazonka.BillingConductor.Types.Tiering
import Amazonka.BillingConductor.Types.UpdateCustomLineItemChargeDetails
import Amazonka.BillingConductor.Types.UpdateCustomLineItemFlatChargeDetails
import Amazonka.BillingConductor.Types.UpdateCustomLineItemPercentageChargeDetails
import Amazonka.BillingConductor.Types.UpdateFreeTierConfig
import Amazonka.BillingConductor.Types.UpdateTieringInput
import Amazonka.BillingConductor.UntagResource
import Amazonka.BillingConductor.UpdateBillingGroup
import Amazonka.BillingConductor.UpdateCustomLineItem
import Amazonka.BillingConductor.UpdatePricingPlan
import Amazonka.BillingConductor.UpdatePricingRule
