{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Pending
module Network.AWS.Route53Domains
  ( -- * Service configuration
    route53DomainsService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListOperations (Paginated)
    module Network.AWS.Route53Domains.ListOperations,

    -- ** GetDomainDetail
    module Network.AWS.Route53Domains.GetDomainDetail,

    -- ** CheckDomainTransferability
    module Network.AWS.Route53Domains.CheckDomainTransferability,

    -- ** UpdateDomainContactPrivacy
    module Network.AWS.Route53Domains.UpdateDomainContactPrivacy,

    -- ** GetOperationDetail
    module Network.AWS.Route53Domains.GetOperationDetail,

    -- ** RejectDomainTransferFromAnotherAWSAccount
    module Network.AWS.Route53Domains.RejectDomainTransferFromAnotherAWSAccount,

    -- ** EnableDomainAutoRenew
    module Network.AWS.Route53Domains.EnableDomainAutoRenew,

    -- ** ResendContactReachabilityEmail
    module Network.AWS.Route53Domains.ResendContactReachabilityEmail,

    -- ** DisableDomainAutoRenew
    module Network.AWS.Route53Domains.DisableDomainAutoRenew,

    -- ** RenewDomain
    module Network.AWS.Route53Domains.RenewDomain,

    -- ** ViewBilling (Paginated)
    module Network.AWS.Route53Domains.ViewBilling,

    -- ** UpdateDomainContact
    module Network.AWS.Route53Domains.UpdateDomainContact,

    -- ** EnableDomainTransferLock
    module Network.AWS.Route53Domains.EnableDomainTransferLock,

    -- ** RegisterDomain
    module Network.AWS.Route53Domains.RegisterDomain,

    -- ** GetDomainSuggestions
    module Network.AWS.Route53Domains.GetDomainSuggestions,

    -- ** DisableDomainTransferLock
    module Network.AWS.Route53Domains.DisableDomainTransferLock,

    -- ** CheckDomainAvailability
    module Network.AWS.Route53Domains.CheckDomainAvailability,

    -- ** TransferDomainToAnotherAWSAccount
    module Network.AWS.Route53Domains.TransferDomainToAnotherAWSAccount,

    -- ** AcceptDomainTransferFromAnotherAWSAccount
    module Network.AWS.Route53Domains.AcceptDomainTransferFromAnotherAWSAccount,

    -- ** GetContactReachabilityStatus
    module Network.AWS.Route53Domains.GetContactReachabilityStatus,

    -- ** ListTagsForDomain
    module Network.AWS.Route53Domains.ListTagsForDomain,

    -- ** UpdateDomainNameservers
    module Network.AWS.Route53Domains.UpdateDomainNameservers,

    -- ** DeleteTagsForDomain
    module Network.AWS.Route53Domains.DeleteTagsForDomain,

    -- ** UpdateTagsForDomain
    module Network.AWS.Route53Domains.UpdateTagsForDomain,

    -- ** RetrieveDomainAuthCode
    module Network.AWS.Route53Domains.RetrieveDomainAuthCode,

    -- ** TransferDomain
    module Network.AWS.Route53Domains.TransferDomain,

    -- ** ListDomains (Paginated)
    module Network.AWS.Route53Domains.ListDomains,

    -- ** CancelDomainTransferToAnotherAWSAccount
    module Network.AWS.Route53Domains.CancelDomainTransferToAnotherAWSAccount,

    -- * Types

    -- ** ContactType
    ContactType (..),

    -- ** CountryCode
    CountryCode (..),

    -- ** DomainAvailability
    DomainAvailability (..),

    -- ** ExtraParamName
    ExtraParamName (..),

    -- ** OperationStatus
    OperationStatus (..),

    -- ** OperationType
    OperationType (..),

    -- ** ReachabilityStatus
    ReachabilityStatus (..),

    -- ** Transferable
    Transferable (..),

    -- ** BillingRecord
    BillingRecord (..),
    mkBillingRecord,
    brOperation,
    brInvoiceId,
    brDomainName,
    brBillDate,
    brPrice,

    -- ** ContactDetail
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

    -- ** DomainSuggestion
    DomainSuggestion (..),
    mkDomainSuggestion,
    dAvailability,
    dDomainName,

    -- ** DomainSummary
    DomainSummary (..),
    mkDomainSummary,
    dsExpiry,
    dsTransferLock,
    dsAutoRenew,
    dsDomainName,

    -- ** DomainTransferability
    DomainTransferability (..),
    mkDomainTransferability,
    dtTransferable,

    -- ** ExtraParam
    ExtraParam (..),
    mkExtraParam,
    epValue,
    epName,

    -- ** Nameserver
    Nameserver (..),
    mkNameserver,
    nName,
    nGlueIPs,

    -- ** OperationSummary
    OperationSummary (..),
    mkOperationSummary,
    osStatus,
    osSubmittedDate,
    osOperationId,
    osType,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53Domains.AcceptDomainTransferFromAnotherAWSAccount
import Network.AWS.Route53Domains.CancelDomainTransferToAnotherAWSAccount
import Network.AWS.Route53Domains.CheckDomainAvailability
import Network.AWS.Route53Domains.CheckDomainTransferability
import Network.AWS.Route53Domains.DeleteTagsForDomain
import Network.AWS.Route53Domains.DisableDomainAutoRenew
import Network.AWS.Route53Domains.DisableDomainTransferLock
import Network.AWS.Route53Domains.EnableDomainAutoRenew
import Network.AWS.Route53Domains.EnableDomainTransferLock
import Network.AWS.Route53Domains.GetContactReachabilityStatus
import Network.AWS.Route53Domains.GetDomainDetail
import Network.AWS.Route53Domains.GetDomainSuggestions
import Network.AWS.Route53Domains.GetOperationDetail
import Network.AWS.Route53Domains.ListDomains
import Network.AWS.Route53Domains.ListOperations
import Network.AWS.Route53Domains.ListTagsForDomain
import Network.AWS.Route53Domains.RegisterDomain
import Network.AWS.Route53Domains.RejectDomainTransferFromAnotherAWSAccount
import Network.AWS.Route53Domains.RenewDomain
import Network.AWS.Route53Domains.ResendContactReachabilityEmail
import Network.AWS.Route53Domains.RetrieveDomainAuthCode
import Network.AWS.Route53Domains.TransferDomain
import Network.AWS.Route53Domains.TransferDomainToAnotherAWSAccount
import Network.AWS.Route53Domains.Types
import Network.AWS.Route53Domains.UpdateDomainContact
import Network.AWS.Route53Domains.UpdateDomainContactPrivacy
import Network.AWS.Route53Domains.UpdateDomainNameservers
import Network.AWS.Route53Domains.UpdateTagsForDomain
import Network.AWS.Route53Domains.ViewBilling
import Network.AWS.Route53Domains.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Route53Domains'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
