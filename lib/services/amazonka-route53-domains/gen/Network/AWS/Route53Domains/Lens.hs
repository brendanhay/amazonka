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

    -- ** ListOperations
    listOperations_marker,
    listOperations_maxItems,
    listOperations_submittedSince,
    listOperationsResponse_nextPageMarker,
    listOperationsResponse_httpStatus,
    listOperationsResponse_operations,

    -- ** GetDomainDetail
    getDomainDetail_domainName,
    getDomainDetailResponse_techPrivacy,
    getDomainDetailResponse_dnsSec,
    getDomainDetailResponse_whoIsServer,
    getDomainDetailResponse_registryDomainId,
    getDomainDetailResponse_registrantPrivacy,
    getDomainDetailResponse_updatedDate,
    getDomainDetailResponse_adminPrivacy,
    getDomainDetailResponse_autoRenew,
    getDomainDetailResponse_abuseContactPhone,
    getDomainDetailResponse_registrarUrl,
    getDomainDetailResponse_abuseContactEmail,
    getDomainDetailResponse_expirationDate,
    getDomainDetailResponse_creationDate,
    getDomainDetailResponse_registrarName,
    getDomainDetailResponse_reseller,
    getDomainDetailResponse_statusList,
    getDomainDetailResponse_httpStatus,
    getDomainDetailResponse_domainName,
    getDomainDetailResponse_nameservers,
    getDomainDetailResponse_adminContact,
    getDomainDetailResponse_registrantContact,
    getDomainDetailResponse_techContact,

    -- ** CheckDomainTransferability
    checkDomainTransferability_authCode,
    checkDomainTransferability_domainName,
    checkDomainTransferabilityResponse_httpStatus,
    checkDomainTransferabilityResponse_transferability,

    -- ** UpdateDomainContactPrivacy
    updateDomainContactPrivacy_techPrivacy,
    updateDomainContactPrivacy_registrantPrivacy,
    updateDomainContactPrivacy_adminPrivacy,
    updateDomainContactPrivacy_domainName,
    updateDomainContactPrivacyResponse_httpStatus,
    updateDomainContactPrivacyResponse_operationId,

    -- ** GetOperationDetail
    getOperationDetail_operationId,
    getOperationDetailResponse_status,
    getOperationDetailResponse_submittedDate,
    getOperationDetailResponse_domainName,
    getOperationDetailResponse_operationId,
    getOperationDetailResponse_type,
    getOperationDetailResponse_message,
    getOperationDetailResponse_httpStatus,

    -- ** RejectDomainTransferFromAnotherAwsAccount
    rejectDomainTransferFromAnotherAwsAccount_domainName,
    rejectDomainTransferFromAnotherAwsAccountResponse_operationId,
    rejectDomainTransferFromAnotherAwsAccountResponse_httpStatus,

    -- ** EnableDomainAutoRenew
    enableDomainAutoRenew_domainName,
    enableDomainAutoRenewResponse_httpStatus,

    -- ** ResendContactReachabilityEmail
    resendContactReachabilityEmail_domainName,
    resendContactReachabilityEmailResponse_domainName,
    resendContactReachabilityEmailResponse_emailAddress,
    resendContactReachabilityEmailResponse_isAlreadyVerified,
    resendContactReachabilityEmailResponse_httpStatus,

    -- ** DisableDomainAutoRenew
    disableDomainAutoRenew_domainName,
    disableDomainAutoRenewResponse_httpStatus,

    -- ** RenewDomain
    renewDomain_durationInYears,
    renewDomain_domainName,
    renewDomain_currentExpiryYear,
    renewDomainResponse_httpStatus,
    renewDomainResponse_operationId,

    -- ** ViewBilling
    viewBilling_start,
    viewBilling_end,
    viewBilling_marker,
    viewBilling_maxItems,
    viewBillingResponse_nextPageMarker,
    viewBillingResponse_billingRecords,
    viewBillingResponse_httpStatus,

    -- ** UpdateDomainContact
    updateDomainContact_registrantContact,
    updateDomainContact_adminContact,
    updateDomainContact_techContact,
    updateDomainContact_domainName,
    updateDomainContactResponse_httpStatus,
    updateDomainContactResponse_operationId,

    -- ** EnableDomainTransferLock
    enableDomainTransferLock_domainName,
    enableDomainTransferLockResponse_httpStatus,
    enableDomainTransferLockResponse_operationId,

    -- ** RegisterDomain
    registerDomain_privacyProtectTechContact,
    registerDomain_privacyProtectRegistrantContact,
    registerDomain_autoRenew,
    registerDomain_privacyProtectAdminContact,
    registerDomain_idnLangCode,
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

    -- ** DisableDomainTransferLock
    disableDomainTransferLock_domainName,
    disableDomainTransferLockResponse_httpStatus,
    disableDomainTransferLockResponse_operationId,

    -- ** CheckDomainAvailability
    checkDomainAvailability_idnLangCode,
    checkDomainAvailability_domainName,
    checkDomainAvailabilityResponse_httpStatus,
    checkDomainAvailabilityResponse_availability,

    -- ** TransferDomainToAnotherAwsAccount
    transferDomainToAnotherAwsAccount_domainName,
    transferDomainToAnotherAwsAccount_accountId,
    transferDomainToAnotherAwsAccountResponse_password,
    transferDomainToAnotherAwsAccountResponse_operationId,
    transferDomainToAnotherAwsAccountResponse_httpStatus,

    -- ** AcceptDomainTransferFromAnotherAwsAccount
    acceptDomainTransferFromAnotherAwsAccount_domainName,
    acceptDomainTransferFromAnotherAwsAccount_password,
    acceptDomainTransferFromAnotherAwsAccountResponse_operationId,
    acceptDomainTransferFromAnotherAwsAccountResponse_httpStatus,

    -- ** GetContactReachabilityStatus
    getContactReachabilityStatus_domainName,
    getContactReachabilityStatusResponse_status,
    getContactReachabilityStatusResponse_domainName,
    getContactReachabilityStatusResponse_httpStatus,

    -- ** ListTagsForDomain
    listTagsForDomain_domainName,
    listTagsForDomainResponse_httpStatus,
    listTagsForDomainResponse_tagList,

    -- ** UpdateDomainNameservers
    updateDomainNameservers_fIAuthKey,
    updateDomainNameservers_domainName,
    updateDomainNameservers_nameservers,
    updateDomainNameserversResponse_httpStatus,
    updateDomainNameserversResponse_operationId,

    -- ** DeleteTagsForDomain
    deleteTagsForDomain_domainName,
    deleteTagsForDomain_tagsToDelete,
    deleteTagsForDomainResponse_httpStatus,

    -- ** UpdateTagsForDomain
    updateTagsForDomain_tagsToUpdate,
    updateTagsForDomain_domainName,
    updateTagsForDomainResponse_httpStatus,

    -- ** RetrieveDomainAuthCode
    retrieveDomainAuthCode_domainName,
    retrieveDomainAuthCodeResponse_httpStatus,
    retrieveDomainAuthCodeResponse_authCode,

    -- ** TransferDomain
    transferDomain_privacyProtectTechContact,
    transferDomain_privacyProtectRegistrantContact,
    transferDomain_autoRenew,
    transferDomain_privacyProtectAdminContact,
    transferDomain_idnLangCode,
    transferDomain_authCode,
    transferDomain_nameservers,
    transferDomain_domainName,
    transferDomain_durationInYears,
    transferDomain_adminContact,
    transferDomain_registrantContact,
    transferDomain_techContact,
    transferDomainResponse_httpStatus,
    transferDomainResponse_operationId,

    -- ** ListDomains
    listDomains_marker,
    listDomains_maxItems,
    listDomainsResponse_nextPageMarker,
    listDomainsResponse_httpStatus,
    listDomainsResponse_domains,

    -- ** CancelDomainTransferToAnotherAwsAccount
    cancelDomainTransferToAnotherAwsAccount_domainName,
    cancelDomainTransferToAnotherAwsAccountResponse_operationId,
    cancelDomainTransferToAnotherAwsAccountResponse_httpStatus,

    -- * Types

    -- ** BillingRecord
    billingRecord_operation,
    billingRecord_invoiceId,
    billingRecord_domainName,
    billingRecord_billDate,
    billingRecord_price,

    -- ** ContactDetail
    contactDetail_organizationName,
    contactDetail_email,
    contactDetail_state,
    contactDetail_fax,
    contactDetail_lastName,
    contactDetail_extraParams,
    contactDetail_zipCode,
    contactDetail_addressLine1,
    contactDetail_city,
    contactDetail_phoneNumber,
    contactDetail_addressLine2,
    contactDetail_firstName,
    contactDetail_countryCode,
    contactDetail_contactType,

    -- ** DomainSuggestion
    domainSuggestion_availability,
    domainSuggestion_domainName,

    -- ** DomainSummary
    domainSummary_expiry,
    domainSummary_transferLock,
    domainSummary_autoRenew,
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
    tag_value,
    tag_key,
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
