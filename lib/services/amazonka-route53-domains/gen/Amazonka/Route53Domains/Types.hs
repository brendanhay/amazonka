{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53Domains.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidInput,
    _OperationLimitExceeded,
    _DomainLimitExceeded,
    _UnsupportedTLD,
    _TLDRulesViolation,
    _DuplicateRequest,

    -- * ContactType
    ContactType (..),

    -- * CountryCode
    CountryCode (..),

    -- * DomainAvailability
    DomainAvailability (..),

    -- * ExtraParamName
    ExtraParamName (..),

    -- * OperationStatus
    OperationStatus (..),

    -- * OperationType
    OperationType (..),

    -- * ReachabilityStatus
    ReachabilityStatus (..),

    -- * Transferable
    Transferable (..),

    -- * BillingRecord
    BillingRecord (..),
    newBillingRecord,
    billingRecord_operation,
    billingRecord_invoiceId,
    billingRecord_domainName,
    billingRecord_billDate,
    billingRecord_price,

    -- * ContactDetail
    ContactDetail (..),
    newContactDetail,
    contactDetail_organizationName,
    contactDetail_email,
    contactDetail_state,
    contactDetail_fax,
    contactDetail_lastName,
    contactDetail_extraParams,
    contactDetail_zipCode,
    contactDetail_addressLine1,
    contactDetail_city,
    contactDetail_phoneNumber,
    contactDetail_addressLine2,
    contactDetail_firstName,
    contactDetail_countryCode,
    contactDetail_contactType,

    -- * DomainSuggestion
    DomainSuggestion (..),
    newDomainSuggestion,
    domainSuggestion_availability,
    domainSuggestion_domainName,

    -- * DomainSummary
    DomainSummary (..),
    newDomainSummary,
    domainSummary_expiry,
    domainSummary_transferLock,
    domainSummary_autoRenew,
    domainSummary_domainName,

    -- * DomainTransferability
    DomainTransferability (..),
    newDomainTransferability,
    domainTransferability_transferable,

    -- * ExtraParam
    ExtraParam (..),
    newExtraParam,
    extraParam_name,
    extraParam_value,

    -- * Nameserver
    Nameserver (..),
    newNameserver,
    nameserver_glueIps,
    nameserver_name,

    -- * OperationSummary
    OperationSummary (..),
    newOperationSummary,
    operationSummary_operationId,
    operationSummary_status,
    operationSummary_type,
    operationSummary_submittedDate,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Domains.Types.BillingRecord
import Amazonka.Route53Domains.Types.ContactDetail
import Amazonka.Route53Domains.Types.ContactType
import Amazonka.Route53Domains.Types.CountryCode
import Amazonka.Route53Domains.Types.DomainAvailability
import Amazonka.Route53Domains.Types.DomainSuggestion
import Amazonka.Route53Domains.Types.DomainSummary
import Amazonka.Route53Domains.Types.DomainTransferability
import Amazonka.Route53Domains.Types.ExtraParam
import Amazonka.Route53Domains.Types.ExtraParamName
import Amazonka.Route53Domains.Types.Nameserver
import Amazonka.Route53Domains.Types.OperationStatus
import Amazonka.Route53Domains.Types.OperationSummary
import Amazonka.Route53Domains.Types.OperationType
import Amazonka.Route53Domains.Types.ReachabilityStatus
import Amazonka.Route53Domains.Types.Tag
import Amazonka.Route53Domains.Types.Transferable
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2014-05-15@ of the Amazon Route 53 Domains SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "Route53Domains",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "route53domains",
      Core._serviceSigningName = "route53domains",
      Core._serviceVersion = "2014-05-15",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "Route53Domains",
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

-- | The requested item is not acceptable. For example, for APIs that accept
-- a domain name, the request might specify a domain name that doesn\'t
-- belong to the account that submitted the request. For
-- @AcceptDomainTransferFromAnotherAwsAccount@, the password might be
-- invalid.
_InvalidInput :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInput =
  Core._MatchServiceError
    defaultService
    "InvalidInput"

-- | The number of operations or jobs running exceeded the allowed threshold
-- for the account.
_OperationLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationLimitExceeded =
  Core._MatchServiceError
    defaultService
    "OperationLimitExceeded"

-- | The number of domains has exceeded the allowed threshold for the
-- account.
_DomainLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DomainLimitExceeded =
  Core._MatchServiceError
    defaultService
    "DomainLimitExceeded"

-- | Amazon Route 53 does not support this top-level domain (TLD).
_UnsupportedTLD :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedTLD =
  Core._MatchServiceError
    defaultService
    "UnsupportedTLD"

-- | The top-level domain does not support this operation.
_TLDRulesViolation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TLDRulesViolation =
  Core._MatchServiceError
    defaultService
    "TLDRulesViolation"

-- | The request is already in progress for the domain.
_DuplicateRequest :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateRequest =
  Core._MatchServiceError
    defaultService
    "DuplicateRequest"
