{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.BillingConductor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-07-30@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services Billing Conductor is a fully managed service that
-- you can use to customize a
-- <https://docs.aws.amazon.com/billingconductor/latest/userguide/understanding-eb.html#eb-other-definitions pro forma>
-- version of your billing data each month, to accurately show or
-- chargeback your end customers. Amazon Web Services Billing Conductor
-- doesn\'t change the way you\'re billed by Amazon Web Services each month
-- by design. Instead, it provides you with a mechanism to configure,
-- generate, and display rates to certain customers over a given billing
-- period. You can also analyze the difference between the rates you apply
-- to your accounting groupings relative to your actual rates from Amazon
-- Web Services. As a result of your Amazon Web Services Billing Conductor
-- configuration, the payer account can also see the custom rate applied on
-- the billing details page of the
-- <https://console.aws.amazon.com/billing Amazon Web Services Billing console>,
-- or configure a cost and usage report per billing group.
--
-- This documentation shows how you can configure Amazon Web Services
-- Billing Conductor using its API. For more information about using the
-- <https://console.aws.amazon.com/billingconductor/ Amazon Web Services Billing Conductor>
-- user interface, see the
-- <https://docs.aws.amazon.com/billingconductor/latest/userguide/what-is-billingconductor.html Amazon Web Services Billing Conductor User Guide>.
module Amazonka.BillingConductor
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceLimitExceededException
    _ServiceLimitExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateAccounts
    AssociateAccounts (AssociateAccounts'),
    newAssociateAccounts,
    AssociateAccountsResponse (AssociateAccountsResponse'),
    newAssociateAccountsResponse,

    -- ** AssociatePricingRules
    AssociatePricingRules (AssociatePricingRules'),
    newAssociatePricingRules,
    AssociatePricingRulesResponse (AssociatePricingRulesResponse'),
    newAssociatePricingRulesResponse,

    -- ** BatchAssociateResourcesToCustomLineItem
    BatchAssociateResourcesToCustomLineItem (BatchAssociateResourcesToCustomLineItem'),
    newBatchAssociateResourcesToCustomLineItem,
    BatchAssociateResourcesToCustomLineItemResponse (BatchAssociateResourcesToCustomLineItemResponse'),
    newBatchAssociateResourcesToCustomLineItemResponse,

    -- ** BatchDisassociateResourcesFromCustomLineItem
    BatchDisassociateResourcesFromCustomLineItem (BatchDisassociateResourcesFromCustomLineItem'),
    newBatchDisassociateResourcesFromCustomLineItem,
    BatchDisassociateResourcesFromCustomLineItemResponse (BatchDisassociateResourcesFromCustomLineItemResponse'),
    newBatchDisassociateResourcesFromCustomLineItemResponse,

    -- ** CreateBillingGroup
    CreateBillingGroup (CreateBillingGroup'),
    newCreateBillingGroup,
    CreateBillingGroupResponse (CreateBillingGroupResponse'),
    newCreateBillingGroupResponse,

    -- ** CreateCustomLineItem
    CreateCustomLineItem (CreateCustomLineItem'),
    newCreateCustomLineItem,
    CreateCustomLineItemResponse (CreateCustomLineItemResponse'),
    newCreateCustomLineItemResponse,

    -- ** CreatePricingPlan
    CreatePricingPlan (CreatePricingPlan'),
    newCreatePricingPlan,
    CreatePricingPlanResponse (CreatePricingPlanResponse'),
    newCreatePricingPlanResponse,

    -- ** CreatePricingRule
    CreatePricingRule (CreatePricingRule'),
    newCreatePricingRule,
    CreatePricingRuleResponse (CreatePricingRuleResponse'),
    newCreatePricingRuleResponse,

    -- ** DeleteBillingGroup
    DeleteBillingGroup (DeleteBillingGroup'),
    newDeleteBillingGroup,
    DeleteBillingGroupResponse (DeleteBillingGroupResponse'),
    newDeleteBillingGroupResponse,

    -- ** DeleteCustomLineItem
    DeleteCustomLineItem (DeleteCustomLineItem'),
    newDeleteCustomLineItem,
    DeleteCustomLineItemResponse (DeleteCustomLineItemResponse'),
    newDeleteCustomLineItemResponse,

    -- ** DeletePricingPlan
    DeletePricingPlan (DeletePricingPlan'),
    newDeletePricingPlan,
    DeletePricingPlanResponse (DeletePricingPlanResponse'),
    newDeletePricingPlanResponse,

    -- ** DeletePricingRule
    DeletePricingRule (DeletePricingRule'),
    newDeletePricingRule,
    DeletePricingRuleResponse (DeletePricingRuleResponse'),
    newDeletePricingRuleResponse,

    -- ** DisassociateAccounts
    DisassociateAccounts (DisassociateAccounts'),
    newDisassociateAccounts,
    DisassociateAccountsResponse (DisassociateAccountsResponse'),
    newDisassociateAccountsResponse,

    -- ** DisassociatePricingRules
    DisassociatePricingRules (DisassociatePricingRules'),
    newDisassociatePricingRules,
    DisassociatePricingRulesResponse (DisassociatePricingRulesResponse'),
    newDisassociatePricingRulesResponse,

    -- ** ListAccountAssociations (Paginated)
    ListAccountAssociations (ListAccountAssociations'),
    newListAccountAssociations,
    ListAccountAssociationsResponse (ListAccountAssociationsResponse'),
    newListAccountAssociationsResponse,

    -- ** ListBillingGroupCostReports (Paginated)
    ListBillingGroupCostReports (ListBillingGroupCostReports'),
    newListBillingGroupCostReports,
    ListBillingGroupCostReportsResponse (ListBillingGroupCostReportsResponse'),
    newListBillingGroupCostReportsResponse,

    -- ** ListBillingGroups (Paginated)
    ListBillingGroups (ListBillingGroups'),
    newListBillingGroups,
    ListBillingGroupsResponse (ListBillingGroupsResponse'),
    newListBillingGroupsResponse,

    -- ** ListCustomLineItemVersions (Paginated)
    ListCustomLineItemVersions (ListCustomLineItemVersions'),
    newListCustomLineItemVersions,
    ListCustomLineItemVersionsResponse (ListCustomLineItemVersionsResponse'),
    newListCustomLineItemVersionsResponse,

    -- ** ListCustomLineItems (Paginated)
    ListCustomLineItems (ListCustomLineItems'),
    newListCustomLineItems,
    ListCustomLineItemsResponse (ListCustomLineItemsResponse'),
    newListCustomLineItemsResponse,

    -- ** ListPricingPlans (Paginated)
    ListPricingPlans (ListPricingPlans'),
    newListPricingPlans,
    ListPricingPlansResponse (ListPricingPlansResponse'),
    newListPricingPlansResponse,

    -- ** ListPricingPlansAssociatedWithPricingRule (Paginated)
    ListPricingPlansAssociatedWithPricingRule (ListPricingPlansAssociatedWithPricingRule'),
    newListPricingPlansAssociatedWithPricingRule,
    ListPricingPlansAssociatedWithPricingRuleResponse (ListPricingPlansAssociatedWithPricingRuleResponse'),
    newListPricingPlansAssociatedWithPricingRuleResponse,

    -- ** ListPricingRules (Paginated)
    ListPricingRules (ListPricingRules'),
    newListPricingRules,
    ListPricingRulesResponse (ListPricingRulesResponse'),
    newListPricingRulesResponse,

    -- ** ListPricingRulesAssociatedToPricingPlan (Paginated)
    ListPricingRulesAssociatedToPricingPlan (ListPricingRulesAssociatedToPricingPlan'),
    newListPricingRulesAssociatedToPricingPlan,
    ListPricingRulesAssociatedToPricingPlanResponse (ListPricingRulesAssociatedToPricingPlanResponse'),
    newListPricingRulesAssociatedToPricingPlanResponse,

    -- ** ListResourcesAssociatedToCustomLineItem (Paginated)
    ListResourcesAssociatedToCustomLineItem (ListResourcesAssociatedToCustomLineItem'),
    newListResourcesAssociatedToCustomLineItem,
    ListResourcesAssociatedToCustomLineItemResponse (ListResourcesAssociatedToCustomLineItemResponse'),
    newListResourcesAssociatedToCustomLineItemResponse,

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

    -- ** UpdateBillingGroup
    UpdateBillingGroup (UpdateBillingGroup'),
    newUpdateBillingGroup,
    UpdateBillingGroupResponse (UpdateBillingGroupResponse'),
    newUpdateBillingGroupResponse,

    -- ** UpdateCustomLineItem
    UpdateCustomLineItem (UpdateCustomLineItem'),
    newUpdateCustomLineItem,
    UpdateCustomLineItemResponse (UpdateCustomLineItemResponse'),
    newUpdateCustomLineItemResponse,

    -- ** UpdatePricingPlan
    UpdatePricingPlan (UpdatePricingPlan'),
    newUpdatePricingPlan,
    UpdatePricingPlanResponse (UpdatePricingPlanResponse'),
    newUpdatePricingPlanResponse,

    -- ** UpdatePricingRule
    UpdatePricingRule (UpdatePricingRule'),
    newUpdatePricingRule,
    UpdatePricingRuleResponse (UpdatePricingRuleResponse'),
    newUpdatePricingRuleResponse,

    -- * Types

    -- ** AssociateResourceErrorReason
    AssociateResourceErrorReason (..),

    -- ** BillingGroupStatus
    BillingGroupStatus (..),

    -- ** CurrencyCode
    CurrencyCode (..),

    -- ** CustomLineItemRelationship
    CustomLineItemRelationship (..),

    -- ** CustomLineItemType
    CustomLineItemType (..),

    -- ** PricingRuleScope
    PricingRuleScope (..),

    -- ** PricingRuleType
    PricingRuleType (..),

    -- ** AccountAssociationsListElement
    AccountAssociationsListElement (AccountAssociationsListElement'),
    newAccountAssociationsListElement,

    -- ** AccountGrouping
    AccountGrouping (AccountGrouping'),
    newAccountGrouping,

    -- ** AssociateResourceError
    AssociateResourceError (AssociateResourceError'),
    newAssociateResourceError,

    -- ** AssociateResourceResponseElement
    AssociateResourceResponseElement (AssociateResourceResponseElement'),
    newAssociateResourceResponseElement,

    -- ** BillingGroupCostReportElement
    BillingGroupCostReportElement (BillingGroupCostReportElement'),
    newBillingGroupCostReportElement,

    -- ** BillingGroupListElement
    BillingGroupListElement (BillingGroupListElement'),
    newBillingGroupListElement,

    -- ** ComputationPreference
    ComputationPreference (ComputationPreference'),
    newComputationPreference,

    -- ** CreateFreeTierConfig
    CreateFreeTierConfig (CreateFreeTierConfig'),
    newCreateFreeTierConfig,

    -- ** CreateTieringInput
    CreateTieringInput (CreateTieringInput'),
    newCreateTieringInput,

    -- ** CustomLineItemBillingPeriodRange
    CustomLineItemBillingPeriodRange (CustomLineItemBillingPeriodRange'),
    newCustomLineItemBillingPeriodRange,

    -- ** CustomLineItemChargeDetails
    CustomLineItemChargeDetails (CustomLineItemChargeDetails'),
    newCustomLineItemChargeDetails,

    -- ** CustomLineItemFlatChargeDetails
    CustomLineItemFlatChargeDetails (CustomLineItemFlatChargeDetails'),
    newCustomLineItemFlatChargeDetails,

    -- ** CustomLineItemListElement
    CustomLineItemListElement (CustomLineItemListElement'),
    newCustomLineItemListElement,

    -- ** CustomLineItemPercentageChargeDetails
    CustomLineItemPercentageChargeDetails (CustomLineItemPercentageChargeDetails'),
    newCustomLineItemPercentageChargeDetails,

    -- ** CustomLineItemVersionListElement
    CustomLineItemVersionListElement (CustomLineItemVersionListElement'),
    newCustomLineItemVersionListElement,

    -- ** DisassociateResourceResponseElement
    DisassociateResourceResponseElement (DisassociateResourceResponseElement'),
    newDisassociateResourceResponseElement,

    -- ** FreeTierConfig
    FreeTierConfig (FreeTierConfig'),
    newFreeTierConfig,

    -- ** ListAccountAssociationsFilter
    ListAccountAssociationsFilter (ListAccountAssociationsFilter'),
    newListAccountAssociationsFilter,

    -- ** ListBillingGroupCostReportsFilter
    ListBillingGroupCostReportsFilter (ListBillingGroupCostReportsFilter'),
    newListBillingGroupCostReportsFilter,

    -- ** ListBillingGroupsFilter
    ListBillingGroupsFilter (ListBillingGroupsFilter'),
    newListBillingGroupsFilter,

    -- ** ListCustomLineItemChargeDetails
    ListCustomLineItemChargeDetails (ListCustomLineItemChargeDetails'),
    newListCustomLineItemChargeDetails,

    -- ** ListCustomLineItemFlatChargeDetails
    ListCustomLineItemFlatChargeDetails (ListCustomLineItemFlatChargeDetails'),
    newListCustomLineItemFlatChargeDetails,

    -- ** ListCustomLineItemPercentageChargeDetails
    ListCustomLineItemPercentageChargeDetails (ListCustomLineItemPercentageChargeDetails'),
    newListCustomLineItemPercentageChargeDetails,

    -- ** ListCustomLineItemVersionsBillingPeriodRangeFilter
    ListCustomLineItemVersionsBillingPeriodRangeFilter (ListCustomLineItemVersionsBillingPeriodRangeFilter'),
    newListCustomLineItemVersionsBillingPeriodRangeFilter,

    -- ** ListCustomLineItemVersionsFilter
    ListCustomLineItemVersionsFilter (ListCustomLineItemVersionsFilter'),
    newListCustomLineItemVersionsFilter,

    -- ** ListCustomLineItemsFilter
    ListCustomLineItemsFilter (ListCustomLineItemsFilter'),
    newListCustomLineItemsFilter,

    -- ** ListPricingPlansFilter
    ListPricingPlansFilter (ListPricingPlansFilter'),
    newListPricingPlansFilter,

    -- ** ListPricingRulesFilter
    ListPricingRulesFilter (ListPricingRulesFilter'),
    newListPricingRulesFilter,

    -- ** ListResourcesAssociatedToCustomLineItemFilter
    ListResourcesAssociatedToCustomLineItemFilter (ListResourcesAssociatedToCustomLineItemFilter'),
    newListResourcesAssociatedToCustomLineItemFilter,

    -- ** ListResourcesAssociatedToCustomLineItemResponseElement
    ListResourcesAssociatedToCustomLineItemResponseElement (ListResourcesAssociatedToCustomLineItemResponseElement'),
    newListResourcesAssociatedToCustomLineItemResponseElement,

    -- ** PricingPlanListElement
    PricingPlanListElement (PricingPlanListElement'),
    newPricingPlanListElement,

    -- ** PricingRuleListElement
    PricingRuleListElement (PricingRuleListElement'),
    newPricingRuleListElement,

    -- ** Tiering
    Tiering (Tiering'),
    newTiering,

    -- ** UpdateCustomLineItemChargeDetails
    UpdateCustomLineItemChargeDetails (UpdateCustomLineItemChargeDetails'),
    newUpdateCustomLineItemChargeDetails,

    -- ** UpdateCustomLineItemFlatChargeDetails
    UpdateCustomLineItemFlatChargeDetails (UpdateCustomLineItemFlatChargeDetails'),
    newUpdateCustomLineItemFlatChargeDetails,

    -- ** UpdateCustomLineItemPercentageChargeDetails
    UpdateCustomLineItemPercentageChargeDetails (UpdateCustomLineItemPercentageChargeDetails'),
    newUpdateCustomLineItemPercentageChargeDetails,

    -- ** UpdateFreeTierConfig
    UpdateFreeTierConfig (UpdateFreeTierConfig'),
    newUpdateFreeTierConfig,

    -- ** UpdateTieringInput
    UpdateTieringInput (UpdateTieringInput'),
    newUpdateTieringInput,
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
import Amazonka.BillingConductor.Lens
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
import Amazonka.BillingConductor.Types
import Amazonka.BillingConductor.UntagResource
import Amazonka.BillingConductor.UpdateBillingGroup
import Amazonka.BillingConductor.UpdateCustomLineItem
import Amazonka.BillingConductor.UpdatePricingPlan
import Amazonka.BillingConductor.UpdatePricingRule
import Amazonka.BillingConductor.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'BillingConductor'.

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
