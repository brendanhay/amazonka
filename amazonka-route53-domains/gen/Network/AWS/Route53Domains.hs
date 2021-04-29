{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Pending
module Network.AWS.Route53Domains
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidInput
    _InvalidInput,

    -- ** UnsupportedTLD
    _UnsupportedTLD,

    -- ** DuplicateRequest
    _DuplicateRequest,

    -- ** DomainLimitExceeded
    _DomainLimitExceeded,

    -- ** OperationLimitExceeded
    _OperationLimitExceeded,

    -- ** TLDRulesViolation
    _TLDRulesViolation,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** TransferDomainToAnotherAwsAccount
    TransferDomainToAnotherAwsAccount (TransferDomainToAnotherAwsAccount'),
    newTransferDomainToAnotherAwsAccount,
    TransferDomainToAnotherAwsAccountResponse (TransferDomainToAnotherAwsAccountResponse'),
    newTransferDomainToAnotherAwsAccountResponse,

    -- ** UpdateDomainContactPrivacy
    UpdateDomainContactPrivacy (UpdateDomainContactPrivacy'),
    newUpdateDomainContactPrivacy,
    UpdateDomainContactPrivacyResponse (UpdateDomainContactPrivacyResponse'),
    newUpdateDomainContactPrivacyResponse,

    -- ** CheckDomainAvailability
    CheckDomainAvailability (CheckDomainAvailability'),
    newCheckDomainAvailability,
    CheckDomainAvailabilityResponse (CheckDomainAvailabilityResponse'),
    newCheckDomainAvailabilityResponse,

    -- ** CheckDomainTransferability
    CheckDomainTransferability (CheckDomainTransferability'),
    newCheckDomainTransferability,
    CheckDomainTransferabilityResponse (CheckDomainTransferabilityResponse'),
    newCheckDomainTransferabilityResponse,

    -- ** ListOperations (Paginated)
    ListOperations (ListOperations'),
    newListOperations,
    ListOperationsResponse (ListOperationsResponse'),
    newListOperationsResponse,

    -- ** DisableDomainTransferLock
    DisableDomainTransferLock (DisableDomainTransferLock'),
    newDisableDomainTransferLock,
    DisableDomainTransferLockResponse (DisableDomainTransferLockResponse'),
    newDisableDomainTransferLockResponse,

    -- ** RegisterDomain
    RegisterDomain (RegisterDomain'),
    newRegisterDomain,
    RegisterDomainResponse (RegisterDomainResponse'),
    newRegisterDomainResponse,

    -- ** GetDomainSuggestions
    GetDomainSuggestions (GetDomainSuggestions'),
    newGetDomainSuggestions,
    GetDomainSuggestionsResponse (GetDomainSuggestionsResponse'),
    newGetDomainSuggestionsResponse,

    -- ** ListDomains (Paginated)
    ListDomains (ListDomains'),
    newListDomains,
    ListDomainsResponse (ListDomainsResponse'),
    newListDomainsResponse,

    -- ** CancelDomainTransferToAnotherAwsAccount
    CancelDomainTransferToAnotherAwsAccount (CancelDomainTransferToAnotherAwsAccount'),
    newCancelDomainTransferToAnotherAwsAccount,
    CancelDomainTransferToAnotherAwsAccountResponse (CancelDomainTransferToAnotherAwsAccountResponse'),
    newCancelDomainTransferToAnotherAwsAccountResponse,

    -- ** EnableDomainTransferLock
    EnableDomainTransferLock (EnableDomainTransferLock'),
    newEnableDomainTransferLock,
    EnableDomainTransferLockResponse (EnableDomainTransferLockResponse'),
    newEnableDomainTransferLockResponse,

    -- ** ViewBilling (Paginated)
    ViewBilling (ViewBilling'),
    newViewBilling,
    ViewBillingResponse (ViewBillingResponse'),
    newViewBillingResponse,

    -- ** DeleteTagsForDomain
    DeleteTagsForDomain (DeleteTagsForDomain'),
    newDeleteTagsForDomain,
    DeleteTagsForDomainResponse (DeleteTagsForDomainResponse'),
    newDeleteTagsForDomainResponse,

    -- ** UpdateTagsForDomain
    UpdateTagsForDomain (UpdateTagsForDomain'),
    newUpdateTagsForDomain,
    UpdateTagsForDomainResponse (UpdateTagsForDomainResponse'),
    newUpdateTagsForDomainResponse,

    -- ** ListTagsForDomain
    ListTagsForDomain (ListTagsForDomain'),
    newListTagsForDomain,
    ListTagsForDomainResponse (ListTagsForDomainResponse'),
    newListTagsForDomainResponse,

    -- ** ResendContactReachabilityEmail
    ResendContactReachabilityEmail (ResendContactReachabilityEmail'),
    newResendContactReachabilityEmail,
    ResendContactReachabilityEmailResponse (ResendContactReachabilityEmailResponse'),
    newResendContactReachabilityEmailResponse,

    -- ** DisableDomainAutoRenew
    DisableDomainAutoRenew (DisableDomainAutoRenew'),
    newDisableDomainAutoRenew,
    DisableDomainAutoRenewResponse (DisableDomainAutoRenewResponse'),
    newDisableDomainAutoRenewResponse,

    -- ** UpdateDomainNameservers
    UpdateDomainNameservers (UpdateDomainNameservers'),
    newUpdateDomainNameservers,
    UpdateDomainNameserversResponse (UpdateDomainNameserversResponse'),
    newUpdateDomainNameserversResponse,

    -- ** EnableDomainAutoRenew
    EnableDomainAutoRenew (EnableDomainAutoRenew'),
    newEnableDomainAutoRenew,
    EnableDomainAutoRenewResponse (EnableDomainAutoRenewResponse'),
    newEnableDomainAutoRenewResponse,

    -- ** GetContactReachabilityStatus
    GetContactReachabilityStatus (GetContactReachabilityStatus'),
    newGetContactReachabilityStatus,
    GetContactReachabilityStatusResponse (GetContactReachabilityStatusResponse'),
    newGetContactReachabilityStatusResponse,

    -- ** RejectDomainTransferFromAnotherAwsAccount
    RejectDomainTransferFromAnotherAwsAccount (RejectDomainTransferFromAnotherAwsAccount'),
    newRejectDomainTransferFromAnotherAwsAccount,
    RejectDomainTransferFromAnotherAwsAccountResponse (RejectDomainTransferFromAnotherAwsAccountResponse'),
    newRejectDomainTransferFromAnotherAwsAccountResponse,

    -- ** AcceptDomainTransferFromAnotherAwsAccount
    AcceptDomainTransferFromAnotherAwsAccount (AcceptDomainTransferFromAnotherAwsAccount'),
    newAcceptDomainTransferFromAnotherAwsAccount,
    AcceptDomainTransferFromAnotherAwsAccountResponse (AcceptDomainTransferFromAnotherAwsAccountResponse'),
    newAcceptDomainTransferFromAnotherAwsAccountResponse,

    -- ** GetOperationDetail
    GetOperationDetail (GetOperationDetail'),
    newGetOperationDetail,
    GetOperationDetailResponse (GetOperationDetailResponse'),
    newGetOperationDetailResponse,

    -- ** GetDomainDetail
    GetDomainDetail (GetDomainDetail'),
    newGetDomainDetail,
    GetDomainDetailResponse (GetDomainDetailResponse'),
    newGetDomainDetailResponse,

    -- ** UpdateDomainContact
    UpdateDomainContact (UpdateDomainContact'),
    newUpdateDomainContact,
    UpdateDomainContactResponse (UpdateDomainContactResponse'),
    newUpdateDomainContactResponse,

    -- ** TransferDomain
    TransferDomain (TransferDomain'),
    newTransferDomain,
    TransferDomainResponse (TransferDomainResponse'),
    newTransferDomainResponse,

    -- ** RenewDomain
    RenewDomain (RenewDomain'),
    newRenewDomain,
    RenewDomainResponse (RenewDomainResponse'),
    newRenewDomainResponse,

    -- ** RetrieveDomainAuthCode
    RetrieveDomainAuthCode (RetrieveDomainAuthCode'),
    newRetrieveDomainAuthCode,
    RetrieveDomainAuthCodeResponse (RetrieveDomainAuthCodeResponse'),
    newRetrieveDomainAuthCodeResponse,

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
    BillingRecord (BillingRecord'),
    newBillingRecord,

    -- ** ContactDetail
    ContactDetail (ContactDetail'),
    newContactDetail,

    -- ** DomainSuggestion
    DomainSuggestion (DomainSuggestion'),
    newDomainSuggestion,

    -- ** DomainSummary
    DomainSummary (DomainSummary'),
    newDomainSummary,

    -- ** DomainTransferability
    DomainTransferability (DomainTransferability'),
    newDomainTransferability,

    -- ** ExtraParam
    ExtraParam (ExtraParam'),
    newExtraParam,

    -- ** Nameserver
    Nameserver (Nameserver'),
    newNameserver,

    -- ** OperationSummary
    OperationSummary (OperationSummary'),
    newOperationSummary,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

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
import Network.AWS.Route53Domains.Lens
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
