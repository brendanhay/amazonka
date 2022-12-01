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
    batchAssociateResourcesToCustomLineItemResponse_successfullyAssociatedResources,
    batchAssociateResourcesToCustomLineItemResponse_failedAssociatedResources,
    batchAssociateResourcesToCustomLineItemResponse_httpStatus,

    -- ** BatchDisassociateResourcesFromCustomLineItem
    batchDisassociateResourcesFromCustomLineItem_billingPeriodRange,
    batchDisassociateResourcesFromCustomLineItem_targetArn,
    batchDisassociateResourcesFromCustomLineItem_resourceArns,
    batchDisassociateResourcesFromCustomLineItemResponse_successfullyDisassociatedResources,
    batchDisassociateResourcesFromCustomLineItemResponse_failedDisassociatedResources,
    batchDisassociateResourcesFromCustomLineItemResponse_httpStatus,

    -- ** CreateBillingGroup
    createBillingGroup_tags,
    createBillingGroup_clientToken,
    createBillingGroup_description,
    createBillingGroup_primaryAccountId,
    createBillingGroup_name,
    createBillingGroup_accountGrouping,
    createBillingGroup_computationPreference,
    createBillingGroupResponse_arn,
    createBillingGroupResponse_httpStatus,

    -- ** CreateCustomLineItem
    createCustomLineItem_tags,
    createCustomLineItem_clientToken,
    createCustomLineItem_billingPeriodRange,
    createCustomLineItem_name,
    createCustomLineItem_description,
    createCustomLineItem_billingGroupArn,
    createCustomLineItem_chargeDetails,
    createCustomLineItemResponse_arn,
    createCustomLineItemResponse_httpStatus,

    -- ** CreatePricingPlan
    createPricingPlan_tags,
    createPricingPlan_clientToken,
    createPricingPlan_description,
    createPricingPlan_pricingRuleArns,
    createPricingPlan_name,
    createPricingPlanResponse_arn,
    createPricingPlanResponse_httpStatus,

    -- ** CreatePricingRule
    createPricingRule_tags,
    createPricingRule_clientToken,
    createPricingRule_billingEntity,
    createPricingRule_description,
    createPricingRule_service,
    createPricingRule_name,
    createPricingRule_scope,
    createPricingRule_type,
    createPricingRule_modifierPercentage,
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
    listAccountAssociations_nextToken,
    listAccountAssociations_billingPeriod,
    listAccountAssociations_filters,
    listAccountAssociationsResponse_nextToken,
    listAccountAssociationsResponse_linkedAccounts,
    listAccountAssociationsResponse_httpStatus,

    -- ** ListBillingGroupCostReports
    listBillingGroupCostReports_nextToken,
    listBillingGroupCostReports_billingPeriod,
    listBillingGroupCostReports_filters,
    listBillingGroupCostReports_maxResults,
    listBillingGroupCostReportsResponse_nextToken,
    listBillingGroupCostReportsResponse_billingGroupCostReports,
    listBillingGroupCostReportsResponse_httpStatus,

    -- ** ListBillingGroups
    listBillingGroups_nextToken,
    listBillingGroups_billingPeriod,
    listBillingGroups_filters,
    listBillingGroups_maxResults,
    listBillingGroupsResponse_nextToken,
    listBillingGroupsResponse_billingGroups,
    listBillingGroupsResponse_httpStatus,

    -- ** ListCustomLineItemVersions
    listCustomLineItemVersions_nextToken,
    listCustomLineItemVersions_filters,
    listCustomLineItemVersions_maxResults,
    listCustomLineItemVersions_arn,
    listCustomLineItemVersionsResponse_nextToken,
    listCustomLineItemVersionsResponse_customLineItemVersions,
    listCustomLineItemVersionsResponse_httpStatus,

    -- ** ListCustomLineItems
    listCustomLineItems_nextToken,
    listCustomLineItems_billingPeriod,
    listCustomLineItems_filters,
    listCustomLineItems_maxResults,
    listCustomLineItemsResponse_nextToken,
    listCustomLineItemsResponse_customLineItems,
    listCustomLineItemsResponse_httpStatus,

    -- ** ListPricingPlans
    listPricingPlans_nextToken,
    listPricingPlans_billingPeriod,
    listPricingPlans_filters,
    listPricingPlans_maxResults,
    listPricingPlansResponse_nextToken,
    listPricingPlansResponse_billingPeriod,
    listPricingPlansResponse_pricingPlans,
    listPricingPlansResponse_httpStatus,

    -- ** ListPricingPlansAssociatedWithPricingRule
    listPricingPlansAssociatedWithPricingRule_nextToken,
    listPricingPlansAssociatedWithPricingRule_billingPeriod,
    listPricingPlansAssociatedWithPricingRule_maxResults,
    listPricingPlansAssociatedWithPricingRule_pricingRuleArn,
    listPricingPlansAssociatedWithPricingRuleResponse_nextToken,
    listPricingPlansAssociatedWithPricingRuleResponse_pricingPlanArns,
    listPricingPlansAssociatedWithPricingRuleResponse_billingPeriod,
    listPricingPlansAssociatedWithPricingRuleResponse_pricingRuleArn,
    listPricingPlansAssociatedWithPricingRuleResponse_httpStatus,

    -- ** ListPricingRules
    listPricingRules_nextToken,
    listPricingRules_billingPeriod,
    listPricingRules_filters,
    listPricingRules_maxResults,
    listPricingRulesResponse_nextToken,
    listPricingRulesResponse_pricingRules,
    listPricingRulesResponse_billingPeriod,
    listPricingRulesResponse_httpStatus,

    -- ** ListPricingRulesAssociatedToPricingPlan
    listPricingRulesAssociatedToPricingPlan_nextToken,
    listPricingRulesAssociatedToPricingPlan_billingPeriod,
    listPricingRulesAssociatedToPricingPlan_maxResults,
    listPricingRulesAssociatedToPricingPlan_pricingPlanArn,
    listPricingRulesAssociatedToPricingPlanResponse_nextToken,
    listPricingRulesAssociatedToPricingPlanResponse_billingPeriod,
    listPricingRulesAssociatedToPricingPlanResponse_pricingPlanArn,
    listPricingRulesAssociatedToPricingPlanResponse_pricingRuleArns,
    listPricingRulesAssociatedToPricingPlanResponse_httpStatus,

    -- ** ListResourcesAssociatedToCustomLineItem
    listResourcesAssociatedToCustomLineItem_nextToken,
    listResourcesAssociatedToCustomLineItem_billingPeriod,
    listResourcesAssociatedToCustomLineItem_filters,
    listResourcesAssociatedToCustomLineItem_maxResults,
    listResourcesAssociatedToCustomLineItem_arn,
    listResourcesAssociatedToCustomLineItemResponse_nextToken,
    listResourcesAssociatedToCustomLineItemResponse_associatedResources,
    listResourcesAssociatedToCustomLineItemResponse_arn,
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
    updateBillingGroup_name,
    updateBillingGroup_status,
    updateBillingGroup_description,
    updateBillingGroup_computationPreference,
    updateBillingGroup_arn,
    updateBillingGroupResponse_name,
    updateBillingGroupResponse_arn,
    updateBillingGroupResponse_statusReason,
    updateBillingGroupResponse_size,
    updateBillingGroupResponse_status,
    updateBillingGroupResponse_description,
    updateBillingGroupResponse_pricingPlanArn,
    updateBillingGroupResponse_lastModifiedTime,
    updateBillingGroupResponse_primaryAccountId,
    updateBillingGroupResponse_httpStatus,

    -- ** UpdateCustomLineItem
    updateCustomLineItem_name,
    updateCustomLineItem_chargeDetails,
    updateCustomLineItem_billingPeriodRange,
    updateCustomLineItem_description,
    updateCustomLineItem_arn,
    updateCustomLineItemResponse_name,
    updateCustomLineItemResponse_chargeDetails,
    updateCustomLineItemResponse_billingGroupArn,
    updateCustomLineItemResponse_arn,
    updateCustomLineItemResponse_associationSize,
    updateCustomLineItemResponse_description,
    updateCustomLineItemResponse_lastModifiedTime,
    updateCustomLineItemResponse_httpStatus,

    -- ** UpdatePricingPlan
    updatePricingPlan_name,
    updatePricingPlan_description,
    updatePricingPlan_arn,
    updatePricingPlanResponse_name,
    updatePricingPlanResponse_arn,
    updatePricingPlanResponse_size,
    updatePricingPlanResponse_description,
    updatePricingPlanResponse_lastModifiedTime,
    updatePricingPlanResponse_httpStatus,

    -- ** UpdatePricingRule
    updatePricingRule_modifierPercentage,
    updatePricingRule_name,
    updatePricingRule_type,
    updatePricingRule_description,
    updatePricingRule_arn,
    updatePricingRuleResponse_modifierPercentage,
    updatePricingRuleResponse_name,
    updatePricingRuleResponse_billingEntity,
    updatePricingRuleResponse_type,
    updatePricingRuleResponse_arn,
    updatePricingRuleResponse_description,
    updatePricingRuleResponse_service,
    updatePricingRuleResponse_lastModifiedTime,
    updatePricingRuleResponse_scope,
    updatePricingRuleResponse_associatedPricingPlanCount,
    updatePricingRuleResponse_httpStatus,

    -- * Types

    -- ** AccountAssociationsListElement
    accountAssociationsListElement_billingGroupArn,
    accountAssociationsListElement_accountId,
    accountAssociationsListElement_accountName,
    accountAssociationsListElement_accountEmail,

    -- ** AccountGrouping
    accountGrouping_linkedAccountIds,

    -- ** AssociateResourceError
    associateResourceError_message,
    associateResourceError_reason,

    -- ** AssociateResourceResponseElement
    associateResourceResponseElement_arn,
    associateResourceResponseElement_error,

    -- ** BillingGroupCostReportElement
    billingGroupCostReportElement_proformaCost,
    billingGroupCostReportElement_marginPercentage,
    billingGroupCostReportElement_aWSCost,
    billingGroupCostReportElement_arn,
    billingGroupCostReportElement_currency,
    billingGroupCostReportElement_margin,

    -- ** BillingGroupListElement
    billingGroupListElement_name,
    billingGroupListElement_arn,
    billingGroupListElement_statusReason,
    billingGroupListElement_size,
    billingGroupListElement_status,
    billingGroupListElement_description,
    billingGroupListElement_lastModifiedTime,
    billingGroupListElement_creationTime,
    billingGroupListElement_computationPreference,
    billingGroupListElement_primaryAccountId,

    -- ** ComputationPreference
    computationPreference_pricingPlanArn,

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
    customLineItemListElement_name,
    customLineItemListElement_chargeDetails,
    customLineItemListElement_billingGroupArn,
    customLineItemListElement_arn,
    customLineItemListElement_associationSize,
    customLineItemListElement_productCode,
    customLineItemListElement_description,
    customLineItemListElement_currencyCode,
    customLineItemListElement_lastModifiedTime,
    customLineItemListElement_creationTime,

    -- ** CustomLineItemPercentageChargeDetails
    customLineItemPercentageChargeDetails_associatedValues,
    customLineItemPercentageChargeDetails_percentageValue,

    -- ** CustomLineItemVersionListElement
    customLineItemVersionListElement_name,
    customLineItemVersionListElement_chargeDetails,
    customLineItemVersionListElement_billingGroupArn,
    customLineItemVersionListElement_associationSize,
    customLineItemVersionListElement_productCode,
    customLineItemVersionListElement_description,
    customLineItemVersionListElement_currencyCode,
    customLineItemVersionListElement_lastModifiedTime,
    customLineItemVersionListElement_startBillingPeriod,
    customLineItemVersionListElement_endBillingPeriod,
    customLineItemVersionListElement_creationTime,

    -- ** DisassociateResourceResponseElement
    disassociateResourceResponseElement_arn,
    disassociateResourceResponseElement_error,

    -- ** ListAccountAssociationsFilter
    listAccountAssociationsFilter_association,
    listAccountAssociationsFilter_accountId,

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
    listCustomLineItemVersionsBillingPeriodRangeFilter_startBillingPeriod,
    listCustomLineItemVersionsBillingPeriodRangeFilter_endBillingPeriod,

    -- ** ListCustomLineItemVersionsFilter
    listCustomLineItemVersionsFilter_billingPeriodRange,

    -- ** ListCustomLineItemsFilter
    listCustomLineItemsFilter_arns,
    listCustomLineItemsFilter_names,
    listCustomLineItemsFilter_billingGroups,

    -- ** ListPricingPlansFilter
    listPricingPlansFilter_arns,

    -- ** ListPricingRulesFilter
    listPricingRulesFilter_arns,

    -- ** ListResourcesAssociatedToCustomLineItemFilter
    listResourcesAssociatedToCustomLineItemFilter_relationship,

    -- ** ListResourcesAssociatedToCustomLineItemResponseElement
    listResourcesAssociatedToCustomLineItemResponseElement_relationship,
    listResourcesAssociatedToCustomLineItemResponseElement_arn,
    listResourcesAssociatedToCustomLineItemResponseElement_endBillingPeriod,

    -- ** PricingPlanListElement
    pricingPlanListElement_name,
    pricingPlanListElement_arn,
    pricingPlanListElement_size,
    pricingPlanListElement_description,
    pricingPlanListElement_lastModifiedTime,
    pricingPlanListElement_creationTime,

    -- ** PricingRuleListElement
    pricingRuleListElement_modifierPercentage,
    pricingRuleListElement_name,
    pricingRuleListElement_billingEntity,
    pricingRuleListElement_type,
    pricingRuleListElement_arn,
    pricingRuleListElement_description,
    pricingRuleListElement_service,
    pricingRuleListElement_lastModifiedTime,
    pricingRuleListElement_scope,
    pricingRuleListElement_associatedPricingPlanCount,
    pricingRuleListElement_creationTime,

    -- ** UpdateCustomLineItemChargeDetails
    updateCustomLineItemChargeDetails_flat,
    updateCustomLineItemChargeDetails_percentage,

    -- ** UpdateCustomLineItemFlatChargeDetails
    updateCustomLineItemFlatChargeDetails_chargeValue,

    -- ** UpdateCustomLineItemPercentageChargeDetails
    updateCustomLineItemPercentageChargeDetails_percentageValue,
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
import Amazonka.BillingConductor.Types.CustomLineItemBillingPeriodRange
import Amazonka.BillingConductor.Types.CustomLineItemChargeDetails
import Amazonka.BillingConductor.Types.CustomLineItemFlatChargeDetails
import Amazonka.BillingConductor.Types.CustomLineItemListElement
import Amazonka.BillingConductor.Types.CustomLineItemPercentageChargeDetails
import Amazonka.BillingConductor.Types.CustomLineItemVersionListElement
import Amazonka.BillingConductor.Types.DisassociateResourceResponseElement
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
import Amazonka.BillingConductor.Types.UpdateCustomLineItemChargeDetails
import Amazonka.BillingConductor.Types.UpdateCustomLineItemFlatChargeDetails
import Amazonka.BillingConductor.Types.UpdateCustomLineItemPercentageChargeDetails
import Amazonka.BillingConductor.UntagResource
import Amazonka.BillingConductor.UpdateBillingGroup
import Amazonka.BillingConductor.UpdateCustomLineItem
import Amazonka.BillingConductor.UpdatePricingPlan
import Amazonka.BillingConductor.UpdatePricingRule
