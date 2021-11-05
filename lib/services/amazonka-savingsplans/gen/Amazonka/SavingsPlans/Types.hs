{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SavingsPlans.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _ServiceQuotaExceededException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * CurrencyCode
    CurrencyCode (..),

    -- * SavingsPlanOfferingFilterAttribute
    SavingsPlanOfferingFilterAttribute (..),

    -- * SavingsPlanOfferingPropertyKey
    SavingsPlanOfferingPropertyKey (..),

    -- * SavingsPlanPaymentOption
    SavingsPlanPaymentOption (..),

    -- * SavingsPlanProductType
    SavingsPlanProductType (..),

    -- * SavingsPlanRateFilterAttribute
    SavingsPlanRateFilterAttribute (..),

    -- * SavingsPlanRateFilterName
    SavingsPlanRateFilterName (..),

    -- * SavingsPlanRatePropertyKey
    SavingsPlanRatePropertyKey (..),

    -- * SavingsPlanRateServiceCode
    SavingsPlanRateServiceCode (..),

    -- * SavingsPlanRateUnit
    SavingsPlanRateUnit (..),

    -- * SavingsPlanState
    SavingsPlanState (..),

    -- * SavingsPlanType
    SavingsPlanType (..),

    -- * SavingsPlansFilterName
    SavingsPlansFilterName (..),

    -- * ParentSavingsPlanOffering
    ParentSavingsPlanOffering (..),
    newParentSavingsPlanOffering,
    parentSavingsPlanOffering_currency,
    parentSavingsPlanOffering_durationSeconds,
    parentSavingsPlanOffering_offeringId,
    parentSavingsPlanOffering_planDescription,
    parentSavingsPlanOffering_planType,
    parentSavingsPlanOffering_paymentOption,

    -- * SavingsPlan
    SavingsPlan (..),
    newSavingsPlan,
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

    -- * SavingsPlanFilter
    SavingsPlanFilter (..),
    newSavingsPlanFilter,
    savingsPlanFilter_values,
    savingsPlanFilter_name,

    -- * SavingsPlanOffering
    SavingsPlanOffering (..),
    newSavingsPlanOffering,
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

    -- * SavingsPlanOfferingFilterElement
    SavingsPlanOfferingFilterElement (..),
    newSavingsPlanOfferingFilterElement,
    savingsPlanOfferingFilterElement_values,
    savingsPlanOfferingFilterElement_name,

    -- * SavingsPlanOfferingProperty
    SavingsPlanOfferingProperty (..),
    newSavingsPlanOfferingProperty,
    savingsPlanOfferingProperty_value,
    savingsPlanOfferingProperty_name,

    -- * SavingsPlanOfferingRate
    SavingsPlanOfferingRate (..),
    newSavingsPlanOfferingRate,
    savingsPlanOfferingRate_operation,
    savingsPlanOfferingRate_usageType,
    savingsPlanOfferingRate_productType,
    savingsPlanOfferingRate_savingsPlanOffering,
    savingsPlanOfferingRate_rate,
    savingsPlanOfferingRate_serviceCode,
    savingsPlanOfferingRate_unit,
    savingsPlanOfferingRate_properties,

    -- * SavingsPlanOfferingRateFilterElement
    SavingsPlanOfferingRateFilterElement (..),
    newSavingsPlanOfferingRateFilterElement,
    savingsPlanOfferingRateFilterElement_values,
    savingsPlanOfferingRateFilterElement_name,

    -- * SavingsPlanOfferingRateProperty
    SavingsPlanOfferingRateProperty (..),
    newSavingsPlanOfferingRateProperty,
    savingsPlanOfferingRateProperty_value,
    savingsPlanOfferingRateProperty_name,

    -- * SavingsPlanRate
    SavingsPlanRate (..),
    newSavingsPlanRate,
    savingsPlanRate_operation,
    savingsPlanRate_usageType,
    savingsPlanRate_productType,
    savingsPlanRate_currency,
    savingsPlanRate_rate,
    savingsPlanRate_serviceCode,
    savingsPlanRate_unit,
    savingsPlanRate_properties,

    -- * SavingsPlanRateFilter
    SavingsPlanRateFilter (..),
    newSavingsPlanRateFilter,
    savingsPlanRateFilter_values,
    savingsPlanRateFilter_name,

    -- * SavingsPlanRateProperty
    SavingsPlanRateProperty (..),
    newSavingsPlanRateProperty,
    savingsPlanRateProperty_value,
    savingsPlanRateProperty_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SavingsPlans.Types.CurrencyCode
import Amazonka.SavingsPlans.Types.ParentSavingsPlanOffering
import Amazonka.SavingsPlans.Types.SavingsPlan
import Amazonka.SavingsPlans.Types.SavingsPlanFilter
import Amazonka.SavingsPlans.Types.SavingsPlanOffering
import Amazonka.SavingsPlans.Types.SavingsPlanOfferingFilterAttribute
import Amazonka.SavingsPlans.Types.SavingsPlanOfferingFilterElement
import Amazonka.SavingsPlans.Types.SavingsPlanOfferingProperty
import Amazonka.SavingsPlans.Types.SavingsPlanOfferingPropertyKey
import Amazonka.SavingsPlans.Types.SavingsPlanOfferingRate
import Amazonka.SavingsPlans.Types.SavingsPlanOfferingRateFilterElement
import Amazonka.SavingsPlans.Types.SavingsPlanOfferingRateProperty
import Amazonka.SavingsPlans.Types.SavingsPlanPaymentOption
import Amazonka.SavingsPlans.Types.SavingsPlanProductType
import Amazonka.SavingsPlans.Types.SavingsPlanRate
import Amazonka.SavingsPlans.Types.SavingsPlanRateFilter
import Amazonka.SavingsPlans.Types.SavingsPlanRateFilterAttribute
import Amazonka.SavingsPlans.Types.SavingsPlanRateFilterName
import Amazonka.SavingsPlans.Types.SavingsPlanRateProperty
import Amazonka.SavingsPlans.Types.SavingsPlanRatePropertyKey
import Amazonka.SavingsPlans.Types.SavingsPlanRateServiceCode
import Amazonka.SavingsPlans.Types.SavingsPlanRateUnit
import Amazonka.SavingsPlans.Types.SavingsPlanState
import Amazonka.SavingsPlans.Types.SavingsPlanType
import Amazonka.SavingsPlans.Types.SavingsPlansFilterName
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-06-28@ of the Amazon Savings Plans SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "SavingsPlans",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "savingsplans",
      Core._serviceSigningName = "savingsplans",
      Core._serviceVersion = "2019-06-28",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "SavingsPlans",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | One of the input parameters is not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | A service quota has been exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | An unexpected error occurred.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
