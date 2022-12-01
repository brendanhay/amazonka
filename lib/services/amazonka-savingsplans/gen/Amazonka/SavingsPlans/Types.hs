{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SavingsPlans.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _ValidationException,

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
    parentSavingsPlanOffering_planType,
    parentSavingsPlanOffering_durationSeconds,
    parentSavingsPlanOffering_currency,
    parentSavingsPlanOffering_offeringId,
    parentSavingsPlanOffering_paymentOption,
    parentSavingsPlanOffering_planDescription,

    -- * SavingsPlan
    SavingsPlan (..),
    newSavingsPlan,
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

    -- * SavingsPlanFilter
    SavingsPlanFilter (..),
    newSavingsPlanFilter,
    savingsPlanFilter_name,
    savingsPlanFilter_values,

    -- * SavingsPlanOffering
    SavingsPlanOffering (..),
    newSavingsPlanOffering,
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

    -- * SavingsPlanOfferingFilterElement
    SavingsPlanOfferingFilterElement (..),
    newSavingsPlanOfferingFilterElement,
    savingsPlanOfferingFilterElement_name,
    savingsPlanOfferingFilterElement_values,

    -- * SavingsPlanOfferingProperty
    SavingsPlanOfferingProperty (..),
    newSavingsPlanOfferingProperty,
    savingsPlanOfferingProperty_name,
    savingsPlanOfferingProperty_value,

    -- * SavingsPlanOfferingRate
    SavingsPlanOfferingRate (..),
    newSavingsPlanOfferingRate,
    savingsPlanOfferingRate_rate,
    savingsPlanOfferingRate_productType,
    savingsPlanOfferingRate_properties,
    savingsPlanOfferingRate_serviceCode,
    savingsPlanOfferingRate_usageType,
    savingsPlanOfferingRate_savingsPlanOffering,
    savingsPlanOfferingRate_unit,
    savingsPlanOfferingRate_operation,

    -- * SavingsPlanOfferingRateFilterElement
    SavingsPlanOfferingRateFilterElement (..),
    newSavingsPlanOfferingRateFilterElement,
    savingsPlanOfferingRateFilterElement_name,
    savingsPlanOfferingRateFilterElement_values,

    -- * SavingsPlanOfferingRateProperty
    SavingsPlanOfferingRateProperty (..),
    newSavingsPlanOfferingRateProperty,
    savingsPlanOfferingRateProperty_name,
    savingsPlanOfferingRateProperty_value,

    -- * SavingsPlanRate
    SavingsPlanRate (..),
    newSavingsPlanRate,
    savingsPlanRate_rate,
    savingsPlanRate_productType,
    savingsPlanRate_properties,
    savingsPlanRate_serviceCode,
    savingsPlanRate_usageType,
    savingsPlanRate_currency,
    savingsPlanRate_unit,
    savingsPlanRate_operation,

    -- * SavingsPlanRateFilter
    SavingsPlanRateFilter (..),
    newSavingsPlanRateFilter,
    savingsPlanRateFilter_name,
    savingsPlanRateFilter_values,

    -- * SavingsPlanRateProperty
    SavingsPlanRateProperty (..),
    newSavingsPlanRateProperty,
    savingsPlanRateProperty_name,
    savingsPlanRateProperty_value,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
    { Core.abbrev = "SavingsPlans",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "savingsplans",
      Core.signingName = "savingsplans",
      Core.version = "2019-06-28",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "SavingsPlans",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | An unexpected error occurred.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | A service quota has been exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | One of the input parameters is not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
