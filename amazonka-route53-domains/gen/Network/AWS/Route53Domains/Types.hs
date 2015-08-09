{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types
    (
    -- * Service
      Route53Domains

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

    -- * ContactDetail
    , ContactDetail
    , contactDetail
    , cdOrganizationName
    , cdEmail
    , cdFax
    , cdState
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

    -- * DomainSummary
    , DomainSummary
    , domainSummary
    , dsExpiry
    , dsTransferLock
    , dsAutoRenew
    , dsDomainName

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

import           Network.AWS.Prelude
import           Network.AWS.Route53Domains.Types.Product
import           Network.AWS.Route53Domains.Types.Sum
import           Network.AWS.Sign.V4

-- | Version @2014-05-15@ of the Amazon Route 53 Domains SDK.
data Route53Domains

instance AWSService Route53Domains where
    type Sg Route53Domains = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "Route53Domains"
            , _svcPrefix = "route53domains"
            , _svcVersion = "2014-05-15"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The requested item is not acceptable. For example, for an OperationId it
-- may refer to the ID of an operation that is already completed. For a
-- domain name, it may not be a valid domain name or belong to the
-- requester account.
_InvalidInput :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInput = _ServiceError . hasStatus 400 . hasCode "InvalidInput"

-- | The number of operations or jobs running exceeded the allowed threshold
-- for the account.
_OperationLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_OperationLimitExceeded =
    _ServiceError . hasStatus 400 . hasCode "OperationLimitExceeded"

-- | The number of domains has exceeded the allowed threshold for the
-- account.
_DomainLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_DomainLimitExceeded =
    _ServiceError . hasStatus 400 . hasCode "DomainLimitExceeded"

-- | Amazon Route 53 does not support this top-level domain.
_UnsupportedTLD :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedTLD = _ServiceError . hasStatus 400 . hasCode "UnsupportedTLD"

-- | The top-level domain does not support this operation.
_TLDRulesViolation :: AsError a => Getting (First ServiceError) a ServiceError
_TLDRulesViolation =
    _ServiceError . hasStatus 400 . hasCode "TLDRulesViolation"

-- | The request is already in progress for the domain.
_DuplicateRequest :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateRequest = _ServiceError . hasStatus 400 . hasCode "DuplicateRequest"
