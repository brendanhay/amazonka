{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Route53Domains
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2014-05-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Pending
module Amazonka.Route53Domains
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** DnssecLimitExceeded
    _DnssecLimitExceeded,

    -- ** DomainLimitExceeded
    _DomainLimitExceeded,

    -- ** DuplicateRequest
    _DuplicateRequest,

    -- ** InvalidInput
    _InvalidInput,

    -- ** OperationLimitExceeded
    _OperationLimitExceeded,

    -- ** TLDRulesViolation
    _TLDRulesViolation,

    -- ** UnsupportedTLD
    _UnsupportedTLD,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AcceptDomainTransferFromAnotherAwsAccount
    AcceptDomainTransferFromAnotherAwsAccount (AcceptDomainTransferFromAnotherAwsAccount'),
    newAcceptDomainTransferFromAnotherAwsAccount,
    AcceptDomainTransferFromAnotherAwsAccountResponse (AcceptDomainTransferFromAnotherAwsAccountResponse'),
    newAcceptDomainTransferFromAnotherAwsAccountResponse,

    -- ** AssociateDelegationSignerToDomain
    AssociateDelegationSignerToDomain (AssociateDelegationSignerToDomain'),
    newAssociateDelegationSignerToDomain,
    AssociateDelegationSignerToDomainResponse (AssociateDelegationSignerToDomainResponse'),
    newAssociateDelegationSignerToDomainResponse,

    -- ** CancelDomainTransferToAnotherAwsAccount
    CancelDomainTransferToAnotherAwsAccount (CancelDomainTransferToAnotherAwsAccount'),
    newCancelDomainTransferToAnotherAwsAccount,
    CancelDomainTransferToAnotherAwsAccountResponse (CancelDomainTransferToAnotherAwsAccountResponse'),
    newCancelDomainTransferToAnotherAwsAccountResponse,

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

    -- ** DeleteDomain
    DeleteDomain (DeleteDomain'),
    newDeleteDomain,
    DeleteDomainResponse (DeleteDomainResponse'),
    newDeleteDomainResponse,

    -- ** DeleteTagsForDomain
    DeleteTagsForDomain (DeleteTagsForDomain'),
    newDeleteTagsForDomain,
    DeleteTagsForDomainResponse (DeleteTagsForDomainResponse'),
    newDeleteTagsForDomainResponse,

    -- ** DisableDomainAutoRenew
    DisableDomainAutoRenew (DisableDomainAutoRenew'),
    newDisableDomainAutoRenew,
    DisableDomainAutoRenewResponse (DisableDomainAutoRenewResponse'),
    newDisableDomainAutoRenewResponse,

    -- ** DisableDomainTransferLock
    DisableDomainTransferLock (DisableDomainTransferLock'),
    newDisableDomainTransferLock,
    DisableDomainTransferLockResponse (DisableDomainTransferLockResponse'),
    newDisableDomainTransferLockResponse,

    -- ** DisassociateDelegationSignerFromDomain
    DisassociateDelegationSignerFromDomain (DisassociateDelegationSignerFromDomain'),
    newDisassociateDelegationSignerFromDomain,
    DisassociateDelegationSignerFromDomainResponse (DisassociateDelegationSignerFromDomainResponse'),
    newDisassociateDelegationSignerFromDomainResponse,

    -- ** EnableDomainAutoRenew
    EnableDomainAutoRenew (EnableDomainAutoRenew'),
    newEnableDomainAutoRenew,
    EnableDomainAutoRenewResponse (EnableDomainAutoRenewResponse'),
    newEnableDomainAutoRenewResponse,

    -- ** EnableDomainTransferLock
    EnableDomainTransferLock (EnableDomainTransferLock'),
    newEnableDomainTransferLock,
    EnableDomainTransferLockResponse (EnableDomainTransferLockResponse'),
    newEnableDomainTransferLockResponse,

    -- ** GetContactReachabilityStatus
    GetContactReachabilityStatus (GetContactReachabilityStatus'),
    newGetContactReachabilityStatus,
    GetContactReachabilityStatusResponse (GetContactReachabilityStatusResponse'),
    newGetContactReachabilityStatusResponse,

    -- ** GetDomainDetail
    GetDomainDetail (GetDomainDetail'),
    newGetDomainDetail,
    GetDomainDetailResponse (GetDomainDetailResponse'),
    newGetDomainDetailResponse,

    -- ** GetDomainSuggestions
    GetDomainSuggestions (GetDomainSuggestions'),
    newGetDomainSuggestions,
    GetDomainSuggestionsResponse (GetDomainSuggestionsResponse'),
    newGetDomainSuggestionsResponse,

    -- ** GetOperationDetail
    GetOperationDetail (GetOperationDetail'),
    newGetOperationDetail,
    GetOperationDetailResponse (GetOperationDetailResponse'),
    newGetOperationDetailResponse,

    -- ** ListDomains (Paginated)
    ListDomains (ListDomains'),
    newListDomains,
    ListDomainsResponse (ListDomainsResponse'),
    newListDomainsResponse,

    -- ** ListOperations (Paginated)
    ListOperations (ListOperations'),
    newListOperations,
    ListOperationsResponse (ListOperationsResponse'),
    newListOperationsResponse,

    -- ** ListPrices (Paginated)
    ListPrices (ListPrices'),
    newListPrices,
    ListPricesResponse (ListPricesResponse'),
    newListPricesResponse,

    -- ** ListTagsForDomain
    ListTagsForDomain (ListTagsForDomain'),
    newListTagsForDomain,
    ListTagsForDomainResponse (ListTagsForDomainResponse'),
    newListTagsForDomainResponse,

    -- ** PushDomain
    PushDomain (PushDomain'),
    newPushDomain,
    PushDomainResponse (PushDomainResponse'),
    newPushDomainResponse,

    -- ** RegisterDomain
    RegisterDomain (RegisterDomain'),
    newRegisterDomain,
    RegisterDomainResponse (RegisterDomainResponse'),
    newRegisterDomainResponse,

    -- ** RejectDomainTransferFromAnotherAwsAccount
    RejectDomainTransferFromAnotherAwsAccount (RejectDomainTransferFromAnotherAwsAccount'),
    newRejectDomainTransferFromAnotherAwsAccount,
    RejectDomainTransferFromAnotherAwsAccountResponse (RejectDomainTransferFromAnotherAwsAccountResponse'),
    newRejectDomainTransferFromAnotherAwsAccountResponse,

    -- ** RenewDomain
    RenewDomain (RenewDomain'),
    newRenewDomain,
    RenewDomainResponse (RenewDomainResponse'),
    newRenewDomainResponse,

    -- ** ResendContactReachabilityEmail
    ResendContactReachabilityEmail (ResendContactReachabilityEmail'),
    newResendContactReachabilityEmail,
    ResendContactReachabilityEmailResponse (ResendContactReachabilityEmailResponse'),
    newResendContactReachabilityEmailResponse,

    -- ** ResendOperationAuthorization
    ResendOperationAuthorization (ResendOperationAuthorization'),
    newResendOperationAuthorization,
    ResendOperationAuthorizationResponse (ResendOperationAuthorizationResponse'),
    newResendOperationAuthorizationResponse,

    -- ** RetrieveDomainAuthCode
    RetrieveDomainAuthCode (RetrieveDomainAuthCode'),
    newRetrieveDomainAuthCode,
    RetrieveDomainAuthCodeResponse (RetrieveDomainAuthCodeResponse'),
    newRetrieveDomainAuthCodeResponse,

    -- ** TransferDomain
    TransferDomain (TransferDomain'),
    newTransferDomain,
    TransferDomainResponse (TransferDomainResponse'),
    newTransferDomainResponse,

    -- ** TransferDomainToAnotherAwsAccount
    TransferDomainToAnotherAwsAccount (TransferDomainToAnotherAwsAccount'),
    newTransferDomainToAnotherAwsAccount,
    TransferDomainToAnotherAwsAccountResponse (TransferDomainToAnotherAwsAccountResponse'),
    newTransferDomainToAnotherAwsAccountResponse,

    -- ** UpdateDomainContact
    UpdateDomainContact (UpdateDomainContact'),
    newUpdateDomainContact,
    UpdateDomainContactResponse (UpdateDomainContactResponse'),
    newUpdateDomainContactResponse,

    -- ** UpdateDomainContactPrivacy
    UpdateDomainContactPrivacy (UpdateDomainContactPrivacy'),
    newUpdateDomainContactPrivacy,
    UpdateDomainContactPrivacyResponse (UpdateDomainContactPrivacyResponse'),
    newUpdateDomainContactPrivacyResponse,

    -- ** UpdateDomainNameservers
    UpdateDomainNameservers (UpdateDomainNameservers'),
    newUpdateDomainNameservers,
    UpdateDomainNameserversResponse (UpdateDomainNameserversResponse'),
    newUpdateDomainNameserversResponse,

    -- ** UpdateTagsForDomain
    UpdateTagsForDomain (UpdateTagsForDomain'),
    newUpdateTagsForDomain,
    UpdateTagsForDomainResponse (UpdateTagsForDomainResponse'),
    newUpdateTagsForDomainResponse,

    -- ** ViewBilling (Paginated)
    ViewBilling (ViewBilling'),
    newViewBilling,
    ViewBillingResponse (ViewBillingResponse'),
    newViewBillingResponse,

    -- * Types

    -- ** ContactType
    ContactType (..),

    -- ** CountryCode
    CountryCode (..),

    -- ** DomainAvailability
    DomainAvailability (..),

    -- ** ExtraParamName
    ExtraParamName (..),

    -- ** ListDomainsAttributeName
    ListDomainsAttributeName (..),

    -- ** ListOperationsSortAttributeName
    ListOperationsSortAttributeName (..),

    -- ** OperationStatus
    OperationStatus (..),

    -- ** OperationType
    OperationType (..),

    -- ** Operator
    Operator (..),

    -- ** ReachabilityStatus
    ReachabilityStatus (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** StatusFlag
    StatusFlag (..),

    -- ** Transferable
    Transferable (..),

    -- ** BillingRecord
    BillingRecord (BillingRecord'),
    newBillingRecord,

    -- ** Consent
    Consent (Consent'),
    newConsent,

    -- ** ContactDetail
    ContactDetail (ContactDetail'),
    newContactDetail,

    -- ** DnssecKey
    DnssecKey (DnssecKey'),
    newDnssecKey,

    -- ** DnssecSigningAttributes
    DnssecSigningAttributes (DnssecSigningAttributes'),
    newDnssecSigningAttributes,

    -- ** DomainPrice
    DomainPrice (DomainPrice'),
    newDomainPrice,

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

    -- ** FilterCondition
    FilterCondition (FilterCondition'),
    newFilterCondition,

    -- ** Nameserver
    Nameserver (Nameserver'),
    newNameserver,

    -- ** OperationSummary
    OperationSummary (OperationSummary'),
    newOperationSummary,

    -- ** PriceWithCurrency
    PriceWithCurrency (PriceWithCurrency'),
    newPriceWithCurrency,

    -- ** SortCondition
    SortCondition (SortCondition'),
    newSortCondition,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Amazonka.Route53Domains.AcceptDomainTransferFromAnotherAwsAccount
import Amazonka.Route53Domains.AssociateDelegationSignerToDomain
import Amazonka.Route53Domains.CancelDomainTransferToAnotherAwsAccount
import Amazonka.Route53Domains.CheckDomainAvailability
import Amazonka.Route53Domains.CheckDomainTransferability
import Amazonka.Route53Domains.DeleteDomain
import Amazonka.Route53Domains.DeleteTagsForDomain
import Amazonka.Route53Domains.DisableDomainAutoRenew
import Amazonka.Route53Domains.DisableDomainTransferLock
import Amazonka.Route53Domains.DisassociateDelegationSignerFromDomain
import Amazonka.Route53Domains.EnableDomainAutoRenew
import Amazonka.Route53Domains.EnableDomainTransferLock
import Amazonka.Route53Domains.GetContactReachabilityStatus
import Amazonka.Route53Domains.GetDomainDetail
import Amazonka.Route53Domains.GetDomainSuggestions
import Amazonka.Route53Domains.GetOperationDetail
import Amazonka.Route53Domains.Lens
import Amazonka.Route53Domains.ListDomains
import Amazonka.Route53Domains.ListOperations
import Amazonka.Route53Domains.ListPrices
import Amazonka.Route53Domains.ListTagsForDomain
import Amazonka.Route53Domains.PushDomain
import Amazonka.Route53Domains.RegisterDomain
import Amazonka.Route53Domains.RejectDomainTransferFromAnotherAwsAccount
import Amazonka.Route53Domains.RenewDomain
import Amazonka.Route53Domains.ResendContactReachabilityEmail
import Amazonka.Route53Domains.ResendOperationAuthorization
import Amazonka.Route53Domains.RetrieveDomainAuthCode
import Amazonka.Route53Domains.TransferDomain
import Amazonka.Route53Domains.TransferDomainToAnotherAwsAccount
import Amazonka.Route53Domains.Types
import Amazonka.Route53Domains.UpdateDomainContact
import Amazonka.Route53Domains.UpdateDomainContactPrivacy
import Amazonka.Route53Domains.UpdateDomainNameservers
import Amazonka.Route53Domains.UpdateTagsForDomain
import Amazonka.Route53Domains.ViewBilling
import Amazonka.Route53Domains.Waiters

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
