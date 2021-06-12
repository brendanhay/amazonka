{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidInput,
    _UnsupportedTLD,
    _DuplicateRequest,
    _DomainLimitExceeded,
    _OperationLimitExceeded,
    _TLDRulesViolation,

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
    billingRecord_invoiceId,
    billingRecord_operation,
    billingRecord_domainName,
    billingRecord_billDate,
    billingRecord_price,

    -- * ContactDetail
    ContactDetail (..),
    newContactDetail,
    contactDetail_phoneNumber,
    contactDetail_organizationName,
    contactDetail_addressLine1,
    contactDetail_extraParams,
    contactDetail_zipCode,
    contactDetail_contactType,
    contactDetail_city,
    contactDetail_state,
    contactDetail_fax,
    contactDetail_email,
    contactDetail_countryCode,
    contactDetail_firstName,
    contactDetail_lastName,
    contactDetail_addressLine2,

    -- * DomainSuggestion
    DomainSuggestion (..),
    newDomainSuggestion,
    domainSuggestion_availability,
    domainSuggestion_domainName,

    -- * DomainSummary
    DomainSummary (..),
    newDomainSummary,
    domainSummary_expiry,
    domainSummary_autoRenew,
    domainSummary_transferLock,
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
    tag_key,
    tag_value,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Route53Domains.Types.BillingRecord
import Network.AWS.Route53Domains.Types.ContactDetail
import Network.AWS.Route53Domains.Types.ContactType
import Network.AWS.Route53Domains.Types.CountryCode
import Network.AWS.Route53Domains.Types.DomainAvailability
import Network.AWS.Route53Domains.Types.DomainSuggestion
import Network.AWS.Route53Domains.Types.DomainSummary
import Network.AWS.Route53Domains.Types.DomainTransferability
import Network.AWS.Route53Domains.Types.ExtraParam
import Network.AWS.Route53Domains.Types.ExtraParamName
import Network.AWS.Route53Domains.Types.Nameserver
import Network.AWS.Route53Domains.Types.OperationStatus
import Network.AWS.Route53Domains.Types.OperationSummary
import Network.AWS.Route53Domains.Types.OperationType
import Network.AWS.Route53Domains.Types.ReachabilityStatus
import Network.AWS.Route53Domains.Types.Tag
import Network.AWS.Route53Domains.Types.Transferable
import qualified Network.AWS.Sign.V4 as Sign

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
      Core._serviceTimeout = Core.Just 70,
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The requested item is not acceptable. For example, for APIs that accept
-- a domain name, the request might specify a domain name that doesn\'t
-- belong to the account that submitted the request. For
-- @AcceptDomainTransferFromAnotherAwsAccount@, the password might be
-- invalid.
_InvalidInput :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInput =
  Core._MatchServiceError
    defaultService
    "InvalidInput"

-- | Amazon Route 53 does not support this top-level domain (TLD).
_UnsupportedTLD :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedTLD =
  Core._MatchServiceError
    defaultService
    "UnsupportedTLD"

-- | The request is already in progress for the domain.
_DuplicateRequest :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateRequest =
  Core._MatchServiceError
    defaultService
    "DuplicateRequest"

-- | The number of domains has exceeded the allowed threshold for the
-- account.
_DomainLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DomainLimitExceeded =
  Core._MatchServiceError
    defaultService
    "DomainLimitExceeded"

-- | The number of operations or jobs running exceeded the allowed threshold
-- for the account.
_OperationLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationLimitExceeded =
  Core._MatchServiceError
    defaultService
    "OperationLimitExceeded"

-- | The top-level domain does not support this operation.
_TLDRulesViolation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TLDRulesViolation =
  Core._MatchServiceError
    defaultService
    "TLDRulesViolation"
