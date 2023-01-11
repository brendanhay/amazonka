{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53Domains.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DnssecLimitExceeded,
    _DomainLimitExceeded,
    _DuplicateRequest,
    _InvalidInput,
    _OperationLimitExceeded,
    _TLDRulesViolation,
    _UnsupportedTLD,

    -- * ContactType
    ContactType (..),

    -- * CountryCode
    CountryCode (..),

    -- * DomainAvailability
    DomainAvailability (..),

    -- * ExtraParamName
    ExtraParamName (..),

    -- * ListDomainsAttributeName
    ListDomainsAttributeName (..),

    -- * ListOperationsSortAttributeName
    ListOperationsSortAttributeName (..),

    -- * OperationStatus
    OperationStatus (..),

    -- * OperationType
    OperationType (..),

    -- * Operator
    Operator (..),

    -- * ReachabilityStatus
    ReachabilityStatus (..),

    -- * SortOrder
    SortOrder (..),

    -- * StatusFlag
    StatusFlag (..),

    -- * Transferable
    Transferable (..),

    -- * BillingRecord
    BillingRecord (..),
    newBillingRecord,
    billingRecord_billDate,
    billingRecord_domainName,
    billingRecord_invoiceId,
    billingRecord_operation,
    billingRecord_price,

    -- * Consent
    Consent (..),
    newConsent,
    consent_maxPrice,
    consent_currency,

    -- * ContactDetail
    ContactDetail (..),
    newContactDetail,
    contactDetail_addressLine1,
    contactDetail_addressLine2,
    contactDetail_city,
    contactDetail_contactType,
    contactDetail_countryCode,
    contactDetail_email,
    contactDetail_extraParams,
    contactDetail_fax,
    contactDetail_firstName,
    contactDetail_lastName,
    contactDetail_organizationName,
    contactDetail_phoneNumber,
    contactDetail_state,
    contactDetail_zipCode,

    -- * DnssecKey
    DnssecKey (..),
    newDnssecKey,
    dnssecKey_algorithm,
    dnssecKey_digest,
    dnssecKey_digestType,
    dnssecKey_flags,
    dnssecKey_id,
    dnssecKey_keyTag,
    dnssecKey_publicKey,

    -- * DnssecSigningAttributes
    DnssecSigningAttributes (..),
    newDnssecSigningAttributes,
    dnssecSigningAttributes_algorithm,
    dnssecSigningAttributes_flags,
    dnssecSigningAttributes_publicKey,

    -- * DomainPrice
    DomainPrice (..),
    newDomainPrice,
    domainPrice_changeOwnershipPrice,
    domainPrice_name,
    domainPrice_registrationPrice,
    domainPrice_renewalPrice,
    domainPrice_restorationPrice,
    domainPrice_transferPrice,

    -- * DomainSuggestion
    DomainSuggestion (..),
    newDomainSuggestion,
    domainSuggestion_availability,
    domainSuggestion_domainName,

    -- * DomainSummary
    DomainSummary (..),
    newDomainSummary,
    domainSummary_autoRenew,
    domainSummary_domainName,
    domainSummary_expiry,
    domainSummary_transferLock,

    -- * DomainTransferability
    DomainTransferability (..),
    newDomainTransferability,
    domainTransferability_transferable,

    -- * ExtraParam
    ExtraParam (..),
    newExtraParam,
    extraParam_name,
    extraParam_value,

    -- * FilterCondition
    FilterCondition (..),
    newFilterCondition,
    filterCondition_name,
    filterCondition_operator,
    filterCondition_values,

    -- * Nameserver
    Nameserver (..),
    newNameserver,
    nameserver_glueIps,
    nameserver_name,

    -- * OperationSummary
    OperationSummary (..),
    newOperationSummary,
    operationSummary_domainName,
    operationSummary_lastUpdatedDate,
    operationSummary_message,
    operationSummary_operationId,
    operationSummary_status,
    operationSummary_statusFlag,
    operationSummary_submittedDate,
    operationSummary_type,

    -- * PriceWithCurrency
    PriceWithCurrency (..),
    newPriceWithCurrency,
    priceWithCurrency_price,
    priceWithCurrency_currency,

    -- * SortCondition
    SortCondition (..),
    newSortCondition,
    sortCondition_name,
    sortCondition_sortOrder,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Domains.Types.BillingRecord
import Amazonka.Route53Domains.Types.Consent
import Amazonka.Route53Domains.Types.ContactDetail
import Amazonka.Route53Domains.Types.ContactType
import Amazonka.Route53Domains.Types.CountryCode
import Amazonka.Route53Domains.Types.DnssecKey
import Amazonka.Route53Domains.Types.DnssecSigningAttributes
import Amazonka.Route53Domains.Types.DomainAvailability
import Amazonka.Route53Domains.Types.DomainPrice
import Amazonka.Route53Domains.Types.DomainSuggestion
import Amazonka.Route53Domains.Types.DomainSummary
import Amazonka.Route53Domains.Types.DomainTransferability
import Amazonka.Route53Domains.Types.ExtraParam
import Amazonka.Route53Domains.Types.ExtraParamName
import Amazonka.Route53Domains.Types.FilterCondition
import Amazonka.Route53Domains.Types.ListDomainsAttributeName
import Amazonka.Route53Domains.Types.ListOperationsSortAttributeName
import Amazonka.Route53Domains.Types.Nameserver
import Amazonka.Route53Domains.Types.OperationStatus
import Amazonka.Route53Domains.Types.OperationSummary
import Amazonka.Route53Domains.Types.OperationType
import Amazonka.Route53Domains.Types.Operator
import Amazonka.Route53Domains.Types.PriceWithCurrency
import Amazonka.Route53Domains.Types.ReachabilityStatus
import Amazonka.Route53Domains.Types.SortCondition
import Amazonka.Route53Domains.Types.SortOrder
import Amazonka.Route53Domains.Types.StatusFlag
import Amazonka.Route53Domains.Types.Tag
import Amazonka.Route53Domains.Types.Transferable
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2014-05-15@ of the Amazon Route 53 Domains SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Route53Domains",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "route53domains",
      Core.signingName = "route53domains",
      Core.version = "2014-05-15",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Route53Domains",
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

-- | This error is returned if you call @AssociateDelegationSignerToDomain@
-- when the specified domain has reached the maximum number of DS records.
-- You can\'t add any additional DS records unless you delete an existing
-- one first.
_DnssecLimitExceeded :: Core.AsError a => Lens.Fold a Core.ServiceError
_DnssecLimitExceeded =
  Core._MatchServiceError
    defaultService
    "DnssecLimitExceeded"

-- | The number of domains has exceeded the allowed threshold for the
-- account.
_DomainLimitExceeded :: Core.AsError a => Lens.Fold a Core.ServiceError
_DomainLimitExceeded =
  Core._MatchServiceError
    defaultService
    "DomainLimitExceeded"

-- | The request is already in progress for the domain.
_DuplicateRequest :: Core.AsError a => Lens.Fold a Core.ServiceError
_DuplicateRequest =
  Core._MatchServiceError
    defaultService
    "DuplicateRequest"

-- | The requested item is not acceptable. For example, for APIs that accept
-- a domain name, the request might specify a domain name that doesn\'t
-- belong to the account that submitted the request. For
-- @AcceptDomainTransferFromAnotherAwsAccount@, the password might be
-- invalid.
_InvalidInput :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidInput =
  Core._MatchServiceError
    defaultService
    "InvalidInput"

-- | The number of operations or jobs running exceeded the allowed threshold
-- for the account.
_OperationLimitExceeded :: Core.AsError a => Lens.Fold a Core.ServiceError
_OperationLimitExceeded =
  Core._MatchServiceError
    defaultService
    "OperationLimitExceeded"

-- | The top-level domain does not support this operation.
_TLDRulesViolation :: Core.AsError a => Lens.Fold a Core.ServiceError
_TLDRulesViolation =
  Core._MatchServiceError
    defaultService
    "TLDRulesViolation"

-- | Amazon Route 53 does not support this top-level domain (TLD).
_UnsupportedTLD :: Core.AsError a => Lens.Fold a Core.ServiceError
_UnsupportedTLD =
  Core._MatchServiceError
    defaultService
    "UnsupportedTLD"
