{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Lens
  ( -- * Operations

    -- ** TransferDomainToAnotherAwsAccount
    transferDomainToAnotherAwsAccount_domainName,
    transferDomainToAnotherAwsAccount_accountId,
    transferDomainToAnotherAwsAccountResponse_operationId,
    transferDomainToAnotherAwsAccountResponse_password,
    transferDomainToAnotherAwsAccountResponse_httpStatus,

    -- ** UpdateDomainContactPrivacy
    updateDomainContactPrivacy_adminPrivacy,
    updateDomainContactPrivacy_techPrivacy,
    updateDomainContactPrivacy_registrantPrivacy,
    updateDomainContactPrivacy_domainName,
    updateDomainContactPrivacyResponse_httpStatus,
    updateDomainContactPrivacyResponse_operationId,

    -- ** CheckDomainAvailability
    checkDomainAvailability_idnLangCode,
    checkDomainAvailability_domainName,
    checkDomainAvailabilityResponse_httpStatus,
    checkDomainAvailabilityResponse_availability,

    -- ** CheckDomainTransferability
    checkDomainTransferability_authCode,
    checkDomainTransferability_domainName,
    checkDomainTransferabilityResponse_httpStatus,
    checkDomainTransferabilityResponse_transferability,

    -- ** ListOperations
    listOperations_submittedSince,
    listOperations_maxItems,
    listOperations_marker,
    listOperationsResponse_nextPageMarker,
    listOperationsResponse_httpStatus,
    listOperationsResponse_operations,

    -- ** DisableDomainTransferLock
    disableDomainTransferLock_domainName,
    disableDomainTransferLockResponse_httpStatus,
    disableDomainTransferLockResponse_operationId,

    -- ** RegisterDomain
    registerDomain_autoRenew,
    registerDomain_idnLangCode,
    registerDomain_privacyProtectTechContact,
    registerDomain_privacyProtectRegistrantContact,
    registerDomain_privacyProtectAdminContact,
    registerDomain_domainName,
    registerDomain_durationInYears,
    registerDomain_adminContact,
    registerDomain_registrantContact,
    registerDomain_techContact,
    registerDomainResponse_httpStatus,
    registerDomainResponse_operationId,

    -- ** GetDomainSuggestions
    getDomainSuggestions_domainName,
    getDomainSuggestions_suggestionCount,
    getDomainSuggestions_onlyAvailable,
    getDomainSuggestionsResponse_suggestionsList,
    getDomainSuggestionsResponse_httpStatus,

    -- ** ListDomains
    listDomains_maxItems,
    listDomains_marker,
    listDomainsResponse_nextPageMarker,
    listDomainsResponse_httpStatus,
    listDomainsResponse_domains,

    -- ** CancelDomainTransferToAnotherAwsAccount
    cancelDomainTransferToAnotherAwsAccount_domainName,
    cancelDomainTransferToAnotherAwsAccountResponse_operationId,
    cancelDomainTransferToAnotherAwsAccountResponse_httpStatus,

    -- ** EnableDomainTransferLock
    enableDomainTransferLock_domainName,
    enableDomainTransferLockResponse_httpStatus,
    enableDomainTransferLockResponse_operationId,

    -- ** ViewBilling
    viewBilling_end,
    viewBilling_start,
    viewBilling_maxItems,
    viewBilling_marker,
    viewBillingResponse_billingRecords,
    viewBillingResponse_nextPageMarker,
    viewBillingResponse_httpStatus,

    -- ** DeleteTagsForDomain
    deleteTagsForDomain_domainName,
    deleteTagsForDomain_tagsToDelete,
    deleteTagsForDomainResponse_httpStatus,

    -- ** UpdateTagsForDomain
    updateTagsForDomain_tagsToUpdate,
    updateTagsForDomain_domainName,
    updateTagsForDomainResponse_httpStatus,

    -- ** ListTagsForDomain
    listTagsForDomain_domainName,
    listTagsForDomainResponse_httpStatus,
    listTagsForDomainResponse_tagList,

    -- ** ResendContactReachabilityEmail
    resendContactReachabilityEmail_domainName,
    resendContactReachabilityEmailResponse_isAlreadyVerified,
    resendContactReachabilityEmailResponse_domainName,
    resendContactReachabilityEmailResponse_emailAddress,
    resendContactReachabilityEmailResponse_httpStatus,

    -- ** DisableDomainAutoRenew
    disableDomainAutoRenew_domainName,
    disableDomainAutoRenewResponse_httpStatus,

    -- ** UpdateDomainNameservers
    updateDomainNameservers_fIAuthKey,
    updateDomainNameservers_domainName,
    updateDomainNameservers_nameservers,
    updateDomainNameserversResponse_httpStatus,
    updateDomainNameserversResponse_operationId,

    -- ** EnableDomainAutoRenew
    enableDomainAutoRenew_domainName,
    enableDomainAutoRenewResponse_httpStatus,

    -- ** GetContactReachabilityStatus
    getContactReachabilityStatus_domainName,
    getContactReachabilityStatusResponse_status,
    getContactReachabilityStatusResponse_domainName,
    getContactReachabilityStatusResponse_httpStatus,

    -- ** RejectDomainTransferFromAnotherAwsAccount
    rejectDomainTransferFromAnotherAwsAccount_domainName,
    rejectDomainTransferFromAnotherAwsAccountResponse_operationId,
    rejectDomainTransferFromAnotherAwsAccountResponse_httpStatus,

    -- ** AcceptDomainTransferFromAnotherAwsAccount
    acceptDomainTransferFromAnotherAwsAccount_domainName,
    acceptDomainTransferFromAnotherAwsAccount_password,
    acceptDomainTransferFromAnotherAwsAccountResponse_operationId,
    acceptDomainTransferFromAnotherAwsAccountResponse_httpStatus,

    -- ** GetOperationDetail
    getOperationDetail_operationId,
    getOperationDetailResponse_status,
    getOperationDetailResponse_message,
    getOperationDetailResponse_operationId,
    getOperationDetailResponse_submittedDate,
    getOperationDetailResponse_domainName,
    getOperationDetailResponse_type,
    getOperationDetailResponse_httpStatus,

    -- ** GetDomainDetail
    getDomainDetail_domainName,
    getDomainDetailResponse_dnsSec,
    getDomainDetailResponse_abuseContactPhone,
    getDomainDetailResponse_autoRenew,
    getDomainDetailResponse_abuseContactEmail,
    getDomainDetailResponse_adminPrivacy,
    getDomainDetailResponse_statusList,
    getDomainDetailResponse_reseller,
    getDomainDetailResponse_registrarName,
    getDomainDetailResponse_registryDomainId,
    getDomainDetailResponse_creationDate,
    getDomainDetailResponse_expirationDate,
    getDomainDetailResponse_whoIsServer,
    getDomainDetailResponse_registrarUrl,
    getDomainDetailResponse_techPrivacy,
    getDomainDetailResponse_registrantPrivacy,
    getDomainDetailResponse_updatedDate,
    getDomainDetailResponse_httpStatus,
    getDomainDetailResponse_domainName,
    getDomainDetailResponse_nameservers,
    getDomainDetailResponse_adminContact,
    getDomainDetailResponse_registrantContact,
    getDomainDetailResponse_techContact,

    -- ** UpdateDomainContact
    updateDomainContact_registrantContact,
    updateDomainContact_techContact,
    updateDomainContact_adminContact,
    updateDomainContact_domainName,
    updateDomainContactResponse_httpStatus,
    updateDomainContactResponse_operationId,

    -- ** TransferDomain
    transferDomain_autoRenew,
    transferDomain_nameservers,
    transferDomain_authCode,
    transferDomain_idnLangCode,
    transferDomain_privacyProtectTechContact,
    transferDomain_privacyProtectRegistrantContact,
    transferDomain_privacyProtectAdminContact,
    transferDomain_domainName,
    transferDomain_durationInYears,
    transferDomain_adminContact,
    transferDomain_registrantContact,
    transferDomain_techContact,
    transferDomainResponse_httpStatus,
    transferDomainResponse_operationId,

    -- ** RenewDomain
    renewDomain_durationInYears,
    renewDomain_domainName,
    renewDomain_currentExpiryYear,
    renewDomainResponse_httpStatus,
    renewDomainResponse_operationId,

    -- ** RetrieveDomainAuthCode
    retrieveDomainAuthCode_domainName,
    retrieveDomainAuthCodeResponse_httpStatus,
    retrieveDomainAuthCodeResponse_authCode,

    -- * Types

    -- ** BillingRecord
    billingRecord_invoiceId,
    billingRecord_operation,
    billingRecord_domainName,
    billingRecord_billDate,
    billingRecord_price,

    -- ** ContactDetail
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

    -- ** DomainSuggestion
    domainSuggestion_availability,
    domainSuggestion_domainName,

    -- ** DomainSummary
    domainSummary_expiry,
    domainSummary_autoRenew,
    domainSummary_transferLock,
    domainSummary_domainName,

    -- ** DomainTransferability
    domainTransferability_transferable,

    -- ** ExtraParam
    extraParam_name,
    extraParam_value,

    -- ** Nameserver
    nameserver_glueIps,
    nameserver_name,

    -- ** OperationSummary
    operationSummary_operationId,
    operationSummary_status,
    operationSummary_type,
    operationSummary_submittedDate,

    -- ** Tag
    tag_key,
    tag_value,
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
import Network.AWS.Route53Domains.Types.BillingRecord
import Network.AWS.Route53Domains.Types.ContactDetail
import Network.AWS.Route53Domains.Types.DomainSuggestion
import Network.AWS.Route53Domains.Types.DomainSummary
import Network.AWS.Route53Domains.Types.DomainTransferability
import Network.AWS.Route53Domains.Types.ExtraParam
import Network.AWS.Route53Domains.Types.Nameserver
import Network.AWS.Route53Domains.Types.OperationSummary
import Network.AWS.Route53Domains.Types.Tag
import Network.AWS.Route53Domains.UpdateDomainContact
import Network.AWS.Route53Domains.UpdateDomainContactPrivacy
import Network.AWS.Route53Domains.UpdateDomainNameservers
import Network.AWS.Route53Domains.UpdateTagsForDomain
import Network.AWS.Route53Domains.ViewBilling
