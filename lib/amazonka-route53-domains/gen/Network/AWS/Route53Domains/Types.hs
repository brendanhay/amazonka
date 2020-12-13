-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types
  ( -- * Service configuration
    route53DomainsService,

    -- * Errors

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
    mkBillingRecord,
    brOperation,
    brInvoiceId,
    brDomainName,
    brBillDate,
    brPrice,

    -- * ContactDetail
    ContactDetail (..),
    mkContactDetail,
    cdOrganizationName,
    cdEmail,
    cdState,
    cdFax,
    cdLastName,
    cdExtraParams,
    cdZipCode,
    cdAddressLine1,
    cdCity,
    cdPhoneNumber,
    cdAddressLine2,
    cdFirstName,
    cdCountryCode,
    cdContactType,

    -- * DomainSuggestion
    DomainSuggestion (..),
    mkDomainSuggestion,
    dAvailability,
    dDomainName,

    -- * DomainSummary
    DomainSummary (..),
    mkDomainSummary,
    dsExpiry,
    dsTransferLock,
    dsAutoRenew,
    dsDomainName,

    -- * DomainTransferability
    DomainTransferability (..),
    mkDomainTransferability,
    dtTransferable,

    -- * ExtraParam
    ExtraParam (..),
    mkExtraParam,
    epValue,
    epName,

    -- * Nameserver
    Nameserver (..),
    mkNameserver,
    nName,
    nGlueIPs,

    -- * OperationSummary
    OperationSummary (..),
    mkOperationSummary,
    osStatus,
    osSubmittedDate,
    osOperationId,
    osType,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
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
route53DomainsService :: Lude.Service
route53DomainsService =
  Lude.Service
    { Lude._svcAbbrev = "Route53Domains",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "route53domains",
      Lude._svcVersion = "2014-05-15",
      Lude._svcEndpoint = Lude.defaultEndpoint route53DomainsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Route53Domains",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
