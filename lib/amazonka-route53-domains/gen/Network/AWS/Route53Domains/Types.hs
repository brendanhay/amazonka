-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _InvalidInput
    , _OperationLimitExceeded
    , _DomainLimitExceeded
    , _UnsupportedTLD
    , _TLDRulesViolation
    , _DuplicateRequest

    -- * DomainSummary
    , DomainSummary (..)
    , mkDomainSummary
    , dsDomainName
    , dsAutoRenew
    , dsExpiry
    , dsTransferLock

    -- * DomainStatus
    , DomainStatus (..)

    -- * DNSSec
    , DNSSec (..)

    -- * DomainSuggestion
    , DomainSuggestion (..)
    , mkDomainSuggestion
    , dAvailability
    , dDomainName

    -- * Email
    , Email (..)

    -- * State
    , State (..)

    -- * HostName
    , HostName (..)

    -- * GlueIp
    , GlueIp (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * ExtraParamName
    , ExtraParamName (..)

    -- * Nameserver
    , Nameserver (..)
    , mkNameserver
    , nName
    , nGlueIps

    -- * AddressLine
    , AddressLine (..)

    -- * RegistryDomainId
    , RegistryDomainId (..)

    -- * ReachabilityStatus
    , ReachabilityStatus (..)

    -- * ZipCode
    , ZipCode (..)

    -- * OperationStatus
    , OperationStatus (..)

    -- * LangCode
    , LangCode (..)

    -- * DomainAvailability
    , DomainAvailability (..)

    -- * Transferable
    , Transferable (..)

    -- * AccountId
    , AccountId (..)

    -- * RegistrarUrl
    , RegistrarUrl (..)

    -- * InvoiceId
    , InvoiceId (..)

    -- * City
    , City (..)

    -- * DomainName
    , DomainName (..)

    -- * ContactName
    , ContactName (..)

    -- * OperationType
    , OperationType (..)

    -- * CountryCode
    , CountryCode (..)

    -- * DomainAuthCode
    , DomainAuthCode (..)

    -- * TagKey
    , TagKey (..)

    -- * ExtraParam
    , ExtraParam (..)
    , mkExtraParam
    , epName
    , epValue

    -- * ContactType
    , ContactType (..)

    -- * OperationId
    , OperationId (..)

    -- * FIAuthKey
    , FIAuthKey (..)

    -- * RegistrarName
    , RegistrarName (..)

    -- * ContactNumber
    , ContactNumber (..)

    -- * Reseller
    , Reseller (..)

    -- * BillingRecord
    , BillingRecord (..)
    , mkBillingRecord
    , brBillDate
    , brDomainName
    , brInvoiceId
    , brOperation
    , brPrice

    -- * ContactDetail
    , ContactDetail (..)
    , mkContactDetail
    , cdAddressLine1
    , cdAddressLine2
    , cdCity
    , cdContactType
    , cdCountryCode
    , cdEmail
    , cdExtraParams
    , cdFax
    , cdFirstName
    , cdLastName
    , cdOrganizationName
    , cdPhoneNumber
    , cdState
    , cdZipCode

    -- * OperationSummary
    , OperationSummary (..)
    , mkOperationSummary
    , osOperationId
    , osStatus
    , osType
    , osSubmittedDate

    -- * DomainTransferability
    , DomainTransferability (..)
    , mkDomainTransferability
    , dtTransferable

    -- * AbuseContactPhone
    , AbuseContactPhone (..)

    -- * WhoIsServer
    , WhoIsServer (..)

    -- * NextPageMarker
    , NextPageMarker (..)

    -- * Marker
    , Marker (..)

    -- * Message
    , Message (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * IdnLangCode
    , IdnLangCode (..)

    -- * AuthCode
    , AuthCode (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Route53Domains.Types.DomainSummary
  
import Network.AWS.Route53Domains.Types.DomainStatus
  
import Network.AWS.Route53Domains.Types.DNSSec
  
import Network.AWS.Route53Domains.Types.DomainSuggestion
  
import Network.AWS.Route53Domains.Types.Email
  
import Network.AWS.Route53Domains.Types.State
  
  
import Network.AWS.Route53Domains.Types.HostName
  
import Network.AWS.Route53Domains.Types.GlueIp
  
import Network.AWS.Route53Domains.Types.Tag
  
import Network.AWS.Route53Domains.Types.ExtraParamName
  
import Network.AWS.Route53Domains.Types.Nameserver
  
import Network.AWS.Route53Domains.Types.AddressLine
  
import Network.AWS.Route53Domains.Types.RegistryDomainId
  
import Network.AWS.Route53Domains.Types.ReachabilityStatus
  
  
  
import Network.AWS.Route53Domains.Types.ZipCode
  
import Network.AWS.Route53Domains.Types.OperationStatus
  
import Network.AWS.Route53Domains.Types.LangCode
  
import Network.AWS.Route53Domains.Types.DomainAvailability
  
  
import Network.AWS.Route53Domains.Types.Transferable
  
import Network.AWS.Route53Domains.Types.AccountId
  
import Network.AWS.Route53Domains.Types.RegistrarUrl
  
import Network.AWS.Route53Domains.Types.InvoiceId
  
import Network.AWS.Route53Domains.Types.City
  
import Network.AWS.Route53Domains.Types.DomainName
  
import Network.AWS.Route53Domains.Types.ContactName
  
import Network.AWS.Route53Domains.Types.OperationType
  
import Network.AWS.Route53Domains.Types.CountryCode
  
import Network.AWS.Route53Domains.Types.DomainAuthCode
  
import Network.AWS.Route53Domains.Types.TagKey
  
import Network.AWS.Route53Domains.Types.ExtraParam
  
import Network.AWS.Route53Domains.Types.ContactType
  
import Network.AWS.Route53Domains.Types.OperationId
  
import Network.AWS.Route53Domains.Types.FIAuthKey
  
  
import Network.AWS.Route53Domains.Types.RegistrarName
  
import Network.AWS.Route53Domains.Types.ContactNumber
  
import Network.AWS.Route53Domains.Types.Reseller
  
  
import Network.AWS.Route53Domains.Types.BillingRecord
  
import Network.AWS.Route53Domains.Types.ContactDetail
  
import Network.AWS.Route53Domains.Types.OperationSummary
  
import Network.AWS.Route53Domains.Types.DomainTransferability
  
import Network.AWS.Route53Domains.Types.AbuseContactPhone
  
import Network.AWS.Route53Domains.Types.WhoIsServer
  
import Network.AWS.Route53Domains.Types.NextPageMarker
  
import Network.AWS.Route53Domains.Types.Marker
  
import Network.AWS.Route53Domains.Types.Message
  
import Network.AWS.Route53Domains.Types.Key
  
import Network.AWS.Route53Domains.Types.Value
  
import Network.AWS.Route53Domains.Types.IdnLangCode
  
import Network.AWS.Route53Domains.Types.AuthCode
  

-- | API version @2014-05-15@ of the Amazon Route 53 Domains SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "Route53Domains",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "route53domains",
                 Core._svcVersion = "2014-05-15", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "Route53Domains",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The requested item is not acceptable. For example, for APIs that accept a domain name, the request might specify a domain name that doesn't belong to the account that submitted the request. For @AcceptDomainTransferFromAnotherAwsAccount@ , the password might be invalid.
_InvalidInput :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInput
  = Core._MatchServiceError mkServiceConfig "InvalidInput"
{-# INLINEABLE _InvalidInput #-}
{-# DEPRECATED _InvalidInput "Use generic-lens or generic-optics instead"  #-}

-- | The number of operations or jobs running exceeded the allowed threshold for the account.
_OperationLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationLimitExceeded
  = Core._MatchServiceError mkServiceConfig "OperationLimitExceeded"
{-# INLINEABLE _OperationLimitExceeded #-}
{-# DEPRECATED _OperationLimitExceeded "Use generic-lens or generic-optics instead"  #-}

-- | The number of domains has exceeded the allowed threshold for the account.
_DomainLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DomainLimitExceeded
  = Core._MatchServiceError mkServiceConfig "DomainLimitExceeded"
{-# INLINEABLE _DomainLimitExceeded #-}
{-# DEPRECATED _DomainLimitExceeded "Use generic-lens or generic-optics instead"  #-}

-- | Amazon Route 53 does not support this top-level domain (TLD).
_UnsupportedTLD :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedTLD
  = Core._MatchServiceError mkServiceConfig "UnsupportedTLD"
{-# INLINEABLE _UnsupportedTLD #-}
{-# DEPRECATED _UnsupportedTLD "Use generic-lens or generic-optics instead"  #-}

-- | The top-level domain does not support this operation.
_TLDRulesViolation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TLDRulesViolation
  = Core._MatchServiceError mkServiceConfig "TLDRulesViolation"
{-# INLINEABLE _TLDRulesViolation #-}
{-# DEPRECATED _TLDRulesViolation "Use generic-lens or generic-optics instead"  #-}

-- | The request is already in progress for the domain.
_DuplicateRequest :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateRequest
  = Core._MatchServiceError mkServiceConfig "DuplicateRequest"
{-# INLINEABLE _DuplicateRequest #-}
{-# DEPRECATED _DuplicateRequest "Use generic-lens or generic-optics instead"  #-}
