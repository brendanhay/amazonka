{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types
    (
    -- * Service Configuration
      route53Domains

    -- * Errors
    , _InvalidInput
    , _OperationLimitExceeded
    , _DomainLimitExceeded
    , _UnsupportedTLD
    , _TLDRulesViolation
    , _DuplicateRequest

    -- * ContactType
    , ContactType (..)

    -- * CountryCode
    , CountryCode (..)

    -- * DomainAvailability
    , DomainAvailability (..)

    -- * ExtraParamName
    , ExtraParamName (..)

    -- * OperationStatus
    , OperationStatus (..)

    -- * OperationType
    , OperationType (..)

    -- * ReachabilityStatus
    , ReachabilityStatus (..)

    -- * Transferable
    , Transferable (..)

    -- * BillingRecord
    , BillingRecord
    , billingRecord
    , brOperation
    , brInvoiceId
    , brDomainName
    , brBillDate
    , brPrice

    -- * ContactDetail
    , ContactDetail
    , contactDetail
    , cdOrganizationName
    , cdEmail
    , cdState
    , cdFax
    , cdLastName
    , cdExtraParams
    , cdZipCode
    , cdAddressLine1
    , cdCity
    , cdPhoneNumber
    , cdAddressLine2
    , cdFirstName
    , cdCountryCode
    , cdContactType

    -- * DomainSuggestion
    , DomainSuggestion
    , domainSuggestion
    , dAvailability
    , dDomainName

    -- * DomainSummary
    , DomainSummary
    , domainSummary
    , dsExpiry
    , dsTransferLock
    , dsAutoRenew
    , dsDomainName

    -- * DomainTransferability
    , DomainTransferability
    , domainTransferability
    , dtTransferable

    -- * ExtraParam
    , ExtraParam
    , extraParam
    , epName
    , epValue

    -- * Nameserver
    , Nameserver
    , nameserver
    , nGlueIPs
    , nName

    -- * OperationSummary
    , OperationSummary
    , operationSummary
    , osOperationId
    , osStatus
    , osType
    , osSubmittedDate

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53Domains.Types.Product
import Network.AWS.Route53Domains.Types.Sum
import Network.AWS.Sign.V4

-- | API version @2014-05-15@ of the Amazon Route 53 Domains SDK configuration.
route53Domains :: Service
route53Domains =
  Service
    { _svcAbbrev = "Route53Domains"
    , _svcSigner = v4
    , _svcPrefix = "route53domains"
    , _svcVersion = "2014-05-15"
    , _svcEndpoint = defaultEndpoint route53Domains
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Route53Domains"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The requested item is not acceptable. For example, for an OperationId it might refer to the ID of an operation that is already completed. For a domain name, it might not be a valid domain name or belong to the requester account.
--
--
_InvalidInput :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInput = _MatchServiceError route53Domains "InvalidInput"


-- | The number of operations or jobs running exceeded the allowed threshold for the account.
--
--
_OperationLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_OperationLimitExceeded =
  _MatchServiceError route53Domains "OperationLimitExceeded"


-- | The number of domains has exceeded the allowed threshold for the account.
--
--
_DomainLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_DomainLimitExceeded = _MatchServiceError route53Domains "DomainLimitExceeded"


-- | Amazon Route 53 does not support this top-level domain (TLD).
--
--
_UnsupportedTLD :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedTLD = _MatchServiceError route53Domains "UnsupportedTLD"


-- | The top-level domain does not support this operation.
--
--
_TLDRulesViolation :: AsError a => Getting (First ServiceError) a ServiceError
_TLDRulesViolation = _MatchServiceError route53Domains "TLDRulesViolation"


-- | The request is already in progress for the domain.
--
--
_DuplicateRequest :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateRequest = _MatchServiceError route53Domains "DuplicateRequest"

