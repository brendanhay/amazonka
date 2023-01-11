{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.BillingConductor.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceLimitExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * AssociateResourceErrorReason
    AssociateResourceErrorReason (..),

    -- * BillingGroupStatus
    BillingGroupStatus (..),

    -- * CurrencyCode
    CurrencyCode (..),

    -- * CustomLineItemRelationship
    CustomLineItemRelationship (..),

    -- * CustomLineItemType
    CustomLineItemType (..),

    -- * PricingRuleScope
    PricingRuleScope (..),

    -- * PricingRuleType
    PricingRuleType (..),

    -- * AccountAssociationsListElement
    AccountAssociationsListElement (..),
    newAccountAssociationsListElement,
    accountAssociationsListElement_accountEmail,
    accountAssociationsListElement_accountId,
    accountAssociationsListElement_accountName,
    accountAssociationsListElement_billingGroupArn,

    -- * AccountGrouping
    AccountGrouping (..),
    newAccountGrouping,
    accountGrouping_linkedAccountIds,

    -- * AssociateResourceError
    AssociateResourceError (..),
    newAssociateResourceError,
    associateResourceError_message,
    associateResourceError_reason,

    -- * AssociateResourceResponseElement
    AssociateResourceResponseElement (..),
    newAssociateResourceResponseElement,
    associateResourceResponseElement_arn,
    associateResourceResponseElement_error,

    -- * BillingGroupCostReportElement
    BillingGroupCostReportElement (..),
    newBillingGroupCostReportElement,
    billingGroupCostReportElement_aWSCost,
    billingGroupCostReportElement_arn,
    billingGroupCostReportElement_currency,
    billingGroupCostReportElement_margin,
    billingGroupCostReportElement_marginPercentage,
    billingGroupCostReportElement_proformaCost,

    -- * BillingGroupListElement
    BillingGroupListElement (..),
    newBillingGroupListElement,
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

    -- * ComputationPreference
    ComputationPreference (..),
    newComputationPreference,
    computationPreference_pricingPlanArn,

    -- * CreateFreeTierConfig
    CreateFreeTierConfig (..),
    newCreateFreeTierConfig,
    createFreeTierConfig_activated,

    -- * CreateTieringInput
    CreateTieringInput (..),
    newCreateTieringInput,
    createTieringInput_freeTier,

    -- * CustomLineItemBillingPeriodRange
    CustomLineItemBillingPeriodRange (..),
    newCustomLineItemBillingPeriodRange,
    customLineItemBillingPeriodRange_exclusiveEndBillingPeriod,
    customLineItemBillingPeriodRange_inclusiveStartBillingPeriod,

    -- * CustomLineItemChargeDetails
    CustomLineItemChargeDetails (..),
    newCustomLineItemChargeDetails,
    customLineItemChargeDetails_flat,
    customLineItemChargeDetails_percentage,
    customLineItemChargeDetails_type,

    -- * CustomLineItemFlatChargeDetails
    CustomLineItemFlatChargeDetails (..),
    newCustomLineItemFlatChargeDetails,
    customLineItemFlatChargeDetails_chargeValue,

    -- * CustomLineItemListElement
    CustomLineItemListElement (..),
    newCustomLineItemListElement,
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

    -- * CustomLineItemPercentageChargeDetails
    CustomLineItemPercentageChargeDetails (..),
    newCustomLineItemPercentageChargeDetails,
    customLineItemPercentageChargeDetails_associatedValues,
    customLineItemPercentageChargeDetails_percentageValue,

    -- * CustomLineItemVersionListElement
    CustomLineItemVersionListElement (..),
    newCustomLineItemVersionListElement,
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

    -- * DisassociateResourceResponseElement
    DisassociateResourceResponseElement (..),
    newDisassociateResourceResponseElement,
    disassociateResourceResponseElement_arn,
    disassociateResourceResponseElement_error,

    -- * FreeTierConfig
    FreeTierConfig (..),
    newFreeTierConfig,
    freeTierConfig_activated,

    -- * ListAccountAssociationsFilter
    ListAccountAssociationsFilter (..),
    newListAccountAssociationsFilter,
    listAccountAssociationsFilter_accountId,
    listAccountAssociationsFilter_association,

    -- * ListBillingGroupCostReportsFilter
    ListBillingGroupCostReportsFilter (..),
    newListBillingGroupCostReportsFilter,
    listBillingGroupCostReportsFilter_billingGroupArns,

    -- * ListBillingGroupsFilter
    ListBillingGroupsFilter (..),
    newListBillingGroupsFilter,
    listBillingGroupsFilter_arns,
    listBillingGroupsFilter_pricingPlan,

    -- * ListCustomLineItemChargeDetails
    ListCustomLineItemChargeDetails (..),
    newListCustomLineItemChargeDetails,
    listCustomLineItemChargeDetails_flat,
    listCustomLineItemChargeDetails_percentage,
    listCustomLineItemChargeDetails_type,

    -- * ListCustomLineItemFlatChargeDetails
    ListCustomLineItemFlatChargeDetails (..),
    newListCustomLineItemFlatChargeDetails,
    listCustomLineItemFlatChargeDetails_chargeValue,

    -- * ListCustomLineItemPercentageChargeDetails
    ListCustomLineItemPercentageChargeDetails (..),
    newListCustomLineItemPercentageChargeDetails,
    listCustomLineItemPercentageChargeDetails_percentageValue,

    -- * ListCustomLineItemVersionsBillingPeriodRangeFilter
    ListCustomLineItemVersionsBillingPeriodRangeFilter (..),
    newListCustomLineItemVersionsBillingPeriodRangeFilter,
    listCustomLineItemVersionsBillingPeriodRangeFilter_endBillingPeriod,
    listCustomLineItemVersionsBillingPeriodRangeFilter_startBillingPeriod,

    -- * ListCustomLineItemVersionsFilter
    ListCustomLineItemVersionsFilter (..),
    newListCustomLineItemVersionsFilter,
    listCustomLineItemVersionsFilter_billingPeriodRange,

    -- * ListCustomLineItemsFilter
    ListCustomLineItemsFilter (..),
    newListCustomLineItemsFilter,
    listCustomLineItemsFilter_arns,
    listCustomLineItemsFilter_billingGroups,
    listCustomLineItemsFilter_names,

    -- * ListPricingPlansFilter
    ListPricingPlansFilter (..),
    newListPricingPlansFilter,
    listPricingPlansFilter_arns,

    -- * ListPricingRulesFilter
    ListPricingRulesFilter (..),
    newListPricingRulesFilter,
    listPricingRulesFilter_arns,

    -- * ListResourcesAssociatedToCustomLineItemFilter
    ListResourcesAssociatedToCustomLineItemFilter (..),
    newListResourcesAssociatedToCustomLineItemFilter,
    listResourcesAssociatedToCustomLineItemFilter_relationship,

    -- * ListResourcesAssociatedToCustomLineItemResponseElement
    ListResourcesAssociatedToCustomLineItemResponseElement (..),
    newListResourcesAssociatedToCustomLineItemResponseElement,
    listResourcesAssociatedToCustomLineItemResponseElement_arn,
    listResourcesAssociatedToCustomLineItemResponseElement_endBillingPeriod,
    listResourcesAssociatedToCustomLineItemResponseElement_relationship,

    -- * PricingPlanListElement
    PricingPlanListElement (..),
    newPricingPlanListElement,
    pricingPlanListElement_arn,
    pricingPlanListElement_creationTime,
    pricingPlanListElement_description,
    pricingPlanListElement_lastModifiedTime,
    pricingPlanListElement_name,
    pricingPlanListElement_size,

    -- * PricingRuleListElement
    PricingRuleListElement (..),
    newPricingRuleListElement,
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

    -- * Tiering
    Tiering (..),
    newTiering,
    tiering_freeTier,

    -- * UpdateCustomLineItemChargeDetails
    UpdateCustomLineItemChargeDetails (..),
    newUpdateCustomLineItemChargeDetails,
    updateCustomLineItemChargeDetails_flat,
    updateCustomLineItemChargeDetails_percentage,

    -- * UpdateCustomLineItemFlatChargeDetails
    UpdateCustomLineItemFlatChargeDetails (..),
    newUpdateCustomLineItemFlatChargeDetails,
    updateCustomLineItemFlatChargeDetails_chargeValue,

    -- * UpdateCustomLineItemPercentageChargeDetails
    UpdateCustomLineItemPercentageChargeDetails (..),
    newUpdateCustomLineItemPercentageChargeDetails,
    updateCustomLineItemPercentageChargeDetails_percentageValue,

    -- * UpdateFreeTierConfig
    UpdateFreeTierConfig (..),
    newUpdateFreeTierConfig,
    updateFreeTierConfig_activated,

    -- * UpdateTieringInput
    UpdateTieringInput (..),
    newUpdateTieringInput,
    updateTieringInput_freeTier,
  )
where

import Amazonka.BillingConductor.Types.AccountAssociationsListElement
import Amazonka.BillingConductor.Types.AccountGrouping
import Amazonka.BillingConductor.Types.AssociateResourceError
import Amazonka.BillingConductor.Types.AssociateResourceErrorReason
import Amazonka.BillingConductor.Types.AssociateResourceResponseElement
import Amazonka.BillingConductor.Types.BillingGroupCostReportElement
import Amazonka.BillingConductor.Types.BillingGroupListElement
import Amazonka.BillingConductor.Types.BillingGroupStatus
import Amazonka.BillingConductor.Types.ComputationPreference
import Amazonka.BillingConductor.Types.CreateFreeTierConfig
import Amazonka.BillingConductor.Types.CreateTieringInput
import Amazonka.BillingConductor.Types.CurrencyCode
import Amazonka.BillingConductor.Types.CustomLineItemBillingPeriodRange
import Amazonka.BillingConductor.Types.CustomLineItemChargeDetails
import Amazonka.BillingConductor.Types.CustomLineItemFlatChargeDetails
import Amazonka.BillingConductor.Types.CustomLineItemListElement
import Amazonka.BillingConductor.Types.CustomLineItemPercentageChargeDetails
import Amazonka.BillingConductor.Types.CustomLineItemRelationship
import Amazonka.BillingConductor.Types.CustomLineItemType
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
import Amazonka.BillingConductor.Types.PricingRuleScope
import Amazonka.BillingConductor.Types.PricingRuleType
import Amazonka.BillingConductor.Types.Tiering
import Amazonka.BillingConductor.Types.UpdateCustomLineItemChargeDetails
import Amazonka.BillingConductor.Types.UpdateCustomLineItemFlatChargeDetails
import Amazonka.BillingConductor.Types.UpdateCustomLineItemPercentageChargeDetails
import Amazonka.BillingConductor.Types.UpdateFreeTierConfig
import Amazonka.BillingConductor.Types.UpdateTieringInput
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-07-30@ of the Amazon BillingConductor SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "BillingConductor",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "billingconductor",
      Core.signingName = "billingconductor",
      Core.version = "2021-07-30",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "BillingConductor",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | You can cause an inconsistent state by updating or deleting a resource.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | An unexpected error occurred while processing a request.
_InternalServerException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The request references a resource that doesn\'t exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request would cause a service limit to exceed.
_ServiceLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceLimitExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input doesn\'t match with the constraints specified by Amazon Web
-- Services services.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
