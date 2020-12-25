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
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** InvalidInput
    _InvalidInput,

    -- ** OperationLimitExceeded
    _OperationLimitExceeded,

    -- ** DomainLimitExceeded
    _DomainLimitExceeded,

    -- ** UnsupportedTLD
    _UnsupportedTLD,

    -- ** TLDRulesViolation
    _TLDRulesViolation,

    -- ** DuplicateRequest
    _DuplicateRequest,

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

    -- ** RejectDomainTransferFromAnotherAwsAccount
    module Network.AWS.Route53Domains.RejectDomainTransferFromAnotherAwsAccount,

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

    -- ** TransferDomainToAnotherAwsAccount
    module Network.AWS.Route53Domains.TransferDomainToAnotherAwsAccount,

    -- ** AcceptDomainTransferFromAnotherAwsAccount
    module Network.AWS.Route53Domains.AcceptDomainTransferFromAnotherAwsAccount,

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

    -- ** CancelDomainTransferToAnotherAwsAccount
    module Network.AWS.Route53Domains.CancelDomainTransferToAnotherAwsAccount,

    -- * Types

    -- ** DomainSummary
    DomainSummary (..),
    mkDomainSummary,
    dsDomainName,
    dsAutoRenew,
    dsExpiry,
    dsTransferLock,

    -- ** DomainStatus
    DomainStatus (..),

    -- ** DNSSec
    DNSSec (..),

    -- ** DomainSuggestion
    DomainSuggestion (..),
    mkDomainSuggestion,
    dAvailability,
    dDomainName,

    -- ** Email
    Email (..),

    -- ** State
    State (..),

    -- ** HostName
    HostName (..),

    -- ** GlueIp
    GlueIp (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** ExtraParamName
    ExtraParamName (..),

    -- ** Nameserver
    Nameserver (..),
    mkNameserver,
    nName,
    nGlueIps,

    -- ** String
    String (..),

    -- ** AddressLine
    AddressLine (..),

    -- ** RegistryDomainId
    RegistryDomainId (..),

    -- ** ReachabilityStatus
    ReachabilityStatus (..),

    -- ** ZipCode
    ZipCode (..),

    -- ** OperationStatus
    OperationStatus (..),

    -- ** LangCode
    LangCode (..),

    -- ** DomainAvailability
    DomainAvailability (..),

    -- ** Transferable
    Transferable (..),

    -- ** AccountId
    AccountId (..),

    -- ** RegistrarUrl
    RegistrarUrl (..),

    -- ** InvoiceId
    InvoiceId (..),

    -- ** City
    City (..),

    -- ** DomainName
    DomainName (..),

    -- ** ContactName
    ContactName (..),

    -- ** OperationType
    OperationType (..),

    -- ** CountryCode
    CountryCode (..),

    -- ** DomainAuthCode
    DomainAuthCode (..),

    -- ** TagKey
    TagKey (..),

    -- ** ExtraParam
    ExtraParam (..),
    mkExtraParam,
    epName,
    epValue,

    -- ** ContactType
    ContactType (..),

    -- ** OperationId
    OperationId (..),

    -- ** FIAuthKey
    FIAuthKey (..),

    -- ** RegistrarName
    RegistrarName (..),

    -- ** ContactNumber
    ContactNumber (..),

    -- ** Reseller
    Reseller (..),

    -- ** BillingRecord
    BillingRecord (..),
    mkBillingRecord,
    brBillDate,
    brDomainName,
    brInvoiceId,
    brOperation,
    brPrice,

    -- ** ContactDetail
    ContactDetail (..),
    mkContactDetail,
    cdAddressLine1,
    cdAddressLine2,
    cdCity,
    cdContactType,
    cdCountryCode,
    cdEmail,
    cdExtraParams,
    cdFax,
    cdFirstName,
    cdLastName,
    cdOrganizationName,
    cdPhoneNumber,
    cdState,
    cdZipCode,

    -- ** OperationSummary
    OperationSummary (..),
    mkOperationSummary,
    osOperationId,
    osStatus,
    osType,
    osSubmittedDate,

    -- ** DomainTransferability
    DomainTransferability (..),
    mkDomainTransferability,
    dtTransferable,

    -- ** Availability
    Availability (..),

    -- ** AbuseContactPhone
    AbuseContactPhone (..),

    -- ** WhoIsServer
    WhoIsServer (..),

    -- ** NextPageMarker
    NextPageMarker (..),

    -- ** Marker
    Marker (..),

    -- ** Message
    Message (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** IdnLangCode
    IdnLangCode (..),

    -- ** AuthCode
    AuthCode (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53Domains.AcceptDomainTransferFromAnotherAwsAccount
import Network.AWS.Route53Domains.CancelDomainTransferToAnotherAwsAccount
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
import Network.AWS.Route53Domains.RejectDomainTransferFromAnotherAwsAccount
import Network.AWS.Route53Domains.RenewDomain
import Network.AWS.Route53Domains.ResendContactReachabilityEmail
import Network.AWS.Route53Domains.RetrieveDomainAuthCode
import Network.AWS.Route53Domains.TransferDomain
import Network.AWS.Route53Domains.TransferDomainToAnotherAwsAccount
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
