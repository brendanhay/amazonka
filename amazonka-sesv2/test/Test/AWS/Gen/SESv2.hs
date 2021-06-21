{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SESv2
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.SESv2 where

import Data.Proxy
import Network.AWS.SESv2
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SESv2.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListDedicatedIpPools $
--             newListDedicatedIpPools
--
--         , requestPutEmailIdentityDkimAttributes $
--             newPutEmailIdentityDkimAttributes
--
--         , requestDeleteDedicatedIpPool $
--             newDeleteDedicatedIpPool
--
--         , requestPutConfigurationSetDeliveryOptions $
--             newPutConfigurationSetDeliveryOptions
--
--         , requestCreateContact $
--             newCreateContact
--
--         , requestPutConfigurationSetTrackingOptions $
--             newPutConfigurationSetTrackingOptions
--
--         , requestGetImportJob $
--             newGetImportJob
--
--         , requestPutAccountDedicatedIpWarmupAttributes $
--             newPutAccountDedicatedIpWarmupAttributes
--
--         , requestGetDeliverabilityTestReport $
--             newGetDeliverabilityTestReport
--
--         , requestPutEmailIdentityConfigurationSetAttributes $
--             newPutEmailIdentityConfigurationSetAttributes
--
--         , requestCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestination
--
--         , requestListEmailIdentities $
--             newListEmailIdentities
--
--         , requestCreateEmailIdentity $
--             newCreateEmailIdentity
--
--         , requestGetBlacklistReports $
--             newGetBlacklistReports
--
--         , requestCreateConfigurationSet $
--             newCreateConfigurationSet
--
--         , requestCreateContactList $
--             newCreateContactList
--
--         , requestDeleteConfigurationSet $
--             newDeleteConfigurationSet
--
--         , requestListImportJobs $
--             newListImportJobs
--
--         , requestUpdateEmailIdentityPolicy $
--             newUpdateEmailIdentityPolicy
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteEmailIdentityPolicy $
--             newDeleteEmailIdentityPolicy
--
--         , requestUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestination
--
--         , requestDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestination
--
--         , requestTestRenderEmailTemplate $
--             newTestRenderEmailTemplate
--
--         , requestSendEmail $
--             newSendEmail
--
--         , requestPutConfigurationSetReputationOptions $
--             newPutConfigurationSetReputationOptions
--
--         , requestPutDedicatedIpInPool $
--             newPutDedicatedIpInPool
--
--         , requestSendBulkEmail $
--             newSendBulkEmail
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListDomainDeliverabilityCampaigns $
--             newListDomainDeliverabilityCampaigns
--
--         , requestPutSuppressedDestination $
--             newPutSuppressedDestination
--
--         , requestPutAccountDetails $
--             newPutAccountDetails
--
--         , requestCreateCustomVerificationEmailTemplate $
--             newCreateCustomVerificationEmailTemplate
--
--         , requestPutConfigurationSetSuppressionOptions $
--             newPutConfigurationSetSuppressionOptions
--
--         , requestGetSuppressedDestination $
--             newGetSuppressedDestination
--
--         , requestGetEmailTemplate $
--             newGetEmailTemplate
--
--         , requestListCustomVerificationEmailTemplates $
--             newListCustomVerificationEmailTemplates
--
--         , requestGetDedicatedIps $
--             newGetDedicatedIps
--
--         , requestDeleteCustomVerificationEmailTemplate $
--             newDeleteCustomVerificationEmailTemplate
--
--         , requestGetConfigurationSetEventDestinations $
--             newGetConfigurationSetEventDestinations
--
--         , requestPutConfigurationSetSendingOptions $
--             newPutConfigurationSetSendingOptions
--
--         , requestGetAccount $
--             newGetAccount
--
--         , requestUpdateCustomVerificationEmailTemplate $
--             newUpdateCustomVerificationEmailTemplate
--
--         , requestGetDomainDeliverabilityCampaign $
--             newGetDomainDeliverabilityCampaign
--
--         , requestGetDedicatedIp $
--             newGetDedicatedIp
--
--         , requestPutEmailIdentityDkimSigningAttributes $
--             newPutEmailIdentityDkimSigningAttributes
--
--         , requestGetConfigurationSet $
--             newGetConfigurationSet
--
--         , requestGetContactList $
--             newGetContactList
--
--         , requestGetEmailIdentity $
--             newGetEmailIdentity
--
--         , requestUpdateContact $
--             newUpdateContact
--
--         , requestListContacts $
--             newListContacts
--
--         , requestDeleteContact $
--             newDeleteContact
--
--         , requestPutDeliverabilityDashboardOption $
--             newPutDeliverabilityDashboardOption
--
--         , requestListDeliverabilityTestReports $
--             newListDeliverabilityTestReports
--
--         , requestCreateEmailIdentityPolicy $
--             newCreateEmailIdentityPolicy
--
--         , requestPutAccountSuppressionAttributes $
--             newPutAccountSuppressionAttributes
--
--         , requestCreateImportJob $
--             newCreateImportJob
--
--         , requestGetContact $
--             newGetContact
--
--         , requestListConfigurationSets $
--             newListConfigurationSets
--
--         , requestUpdateContactList $
--             newUpdateContactList
--
--         , requestPutAccountSendingAttributes $
--             newPutAccountSendingAttributes
--
--         , requestPutEmailIdentityMailFromAttributes $
--             newPutEmailIdentityMailFromAttributes
--
--         , requestCreateDeliverabilityTestReport $
--             newCreateDeliverabilityTestReport
--
--         , requestDeleteEmailIdentity $
--             newDeleteEmailIdentity
--
--         , requestListContactLists $
--             newListContactLists
--
--         , requestDeleteContactList $
--             newDeleteContactList
--
--         , requestPutDedicatedIpWarmupAttributes $
--             newPutDedicatedIpWarmupAttributes
--
--         , requestGetCustomVerificationEmailTemplate $
--             newGetCustomVerificationEmailTemplate
--
--         , requestCreateEmailTemplate $
--             newCreateEmailTemplate
--
--         , requestListEmailTemplates $
--             newListEmailTemplates
--
--         , requestUpdateEmailTemplate $
--             newUpdateEmailTemplate
--
--         , requestGetDomainStatisticsReport $
--             newGetDomainStatisticsReport
--
--         , requestDeleteEmailTemplate $
--             newDeleteEmailTemplate
--
--         , requestPutEmailIdentityFeedbackAttributes $
--             newPutEmailIdentityFeedbackAttributes
--
--         , requestListSuppressedDestinations $
--             newListSuppressedDestinations
--
--         , requestDeleteSuppressedDestination $
--             newDeleteSuppressedDestination
--
--         , requestGetDeliverabilityDashboardOptions $
--             newGetDeliverabilityDashboardOptions
--
--         , requestCreateDedicatedIpPool $
--             newCreateDedicatedIpPool
--
--         , requestSendCustomVerificationEmail $
--             newSendCustomVerificationEmail
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetEmailIdentityPolicies $
--             newGetEmailIdentityPolicies
--
--           ]

--     , testGroup "response"
--         [ responseListDedicatedIpPools $
--             newListDedicatedIpPoolsResponse
--
--         , responsePutEmailIdentityDkimAttributes $
--             newPutEmailIdentityDkimAttributesResponse
--
--         , responseDeleteDedicatedIpPool $
--             newDeleteDedicatedIpPoolResponse
--
--         , responsePutConfigurationSetDeliveryOptions $
--             newPutConfigurationSetDeliveryOptionsResponse
--
--         , responseCreateContact $
--             newCreateContactResponse
--
--         , responsePutConfigurationSetTrackingOptions $
--             newPutConfigurationSetTrackingOptionsResponse
--
--         , responseGetImportJob $
--             newGetImportJobResponse
--
--         , responsePutAccountDedicatedIpWarmupAttributes $
--             newPutAccountDedicatedIpWarmupAttributesResponse
--
--         , responseGetDeliverabilityTestReport $
--             newGetDeliverabilityTestReportResponse
--
--         , responsePutEmailIdentityConfigurationSetAttributes $
--             newPutEmailIdentityConfigurationSetAttributesResponse
--
--         , responseCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestinationResponse
--
--         , responseListEmailIdentities $
--             newListEmailIdentitiesResponse
--
--         , responseCreateEmailIdentity $
--             newCreateEmailIdentityResponse
--
--         , responseGetBlacklistReports $
--             newGetBlacklistReportsResponse
--
--         , responseCreateConfigurationSet $
--             newCreateConfigurationSetResponse
--
--         , responseCreateContactList $
--             newCreateContactListResponse
--
--         , responseDeleteConfigurationSet $
--             newDeleteConfigurationSetResponse
--
--         , responseListImportJobs $
--             newListImportJobsResponse
--
--         , responseUpdateEmailIdentityPolicy $
--             newUpdateEmailIdentityPolicyResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteEmailIdentityPolicy $
--             newDeleteEmailIdentityPolicyResponse
--
--         , responseUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestinationResponse
--
--         , responseDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestinationResponse
--
--         , responseTestRenderEmailTemplate $
--             newTestRenderEmailTemplateResponse
--
--         , responseSendEmail $
--             newSendEmailResponse
--
--         , responsePutConfigurationSetReputationOptions $
--             newPutConfigurationSetReputationOptionsResponse
--
--         , responsePutDedicatedIpInPool $
--             newPutDedicatedIpInPoolResponse
--
--         , responseSendBulkEmail $
--             newSendBulkEmailResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListDomainDeliverabilityCampaigns $
--             newListDomainDeliverabilityCampaignsResponse
--
--         , responsePutSuppressedDestination $
--             newPutSuppressedDestinationResponse
--
--         , responsePutAccountDetails $
--             newPutAccountDetailsResponse
--
--         , responseCreateCustomVerificationEmailTemplate $
--             newCreateCustomVerificationEmailTemplateResponse
--
--         , responsePutConfigurationSetSuppressionOptions $
--             newPutConfigurationSetSuppressionOptionsResponse
--
--         , responseGetSuppressedDestination $
--             newGetSuppressedDestinationResponse
--
--         , responseGetEmailTemplate $
--             newGetEmailTemplateResponse
--
--         , responseListCustomVerificationEmailTemplates $
--             newListCustomVerificationEmailTemplatesResponse
--
--         , responseGetDedicatedIps $
--             newGetDedicatedIpsResponse
--
--         , responseDeleteCustomVerificationEmailTemplate $
--             newDeleteCustomVerificationEmailTemplateResponse
--
--         , responseGetConfigurationSetEventDestinations $
--             newGetConfigurationSetEventDestinationsResponse
--
--         , responsePutConfigurationSetSendingOptions $
--             newPutConfigurationSetSendingOptionsResponse
--
--         , responseGetAccount $
--             newGetAccountResponse
--
--         , responseUpdateCustomVerificationEmailTemplate $
--             newUpdateCustomVerificationEmailTemplateResponse
--
--         , responseGetDomainDeliverabilityCampaign $
--             newGetDomainDeliverabilityCampaignResponse
--
--         , responseGetDedicatedIp $
--             newGetDedicatedIpResponse
--
--         , responsePutEmailIdentityDkimSigningAttributes $
--             newPutEmailIdentityDkimSigningAttributesResponse
--
--         , responseGetConfigurationSet $
--             newGetConfigurationSetResponse
--
--         , responseGetContactList $
--             newGetContactListResponse
--
--         , responseGetEmailIdentity $
--             newGetEmailIdentityResponse
--
--         , responseUpdateContact $
--             newUpdateContactResponse
--
--         , responseListContacts $
--             newListContactsResponse
--
--         , responseDeleteContact $
--             newDeleteContactResponse
--
--         , responsePutDeliverabilityDashboardOption $
--             newPutDeliverabilityDashboardOptionResponse
--
--         , responseListDeliverabilityTestReports $
--             newListDeliverabilityTestReportsResponse
--
--         , responseCreateEmailIdentityPolicy $
--             newCreateEmailIdentityPolicyResponse
--
--         , responsePutAccountSuppressionAttributes $
--             newPutAccountSuppressionAttributesResponse
--
--         , responseCreateImportJob $
--             newCreateImportJobResponse
--
--         , responseGetContact $
--             newGetContactResponse
--
--         , responseListConfigurationSets $
--             newListConfigurationSetsResponse
--
--         , responseUpdateContactList $
--             newUpdateContactListResponse
--
--         , responsePutAccountSendingAttributes $
--             newPutAccountSendingAttributesResponse
--
--         , responsePutEmailIdentityMailFromAttributes $
--             newPutEmailIdentityMailFromAttributesResponse
--
--         , responseCreateDeliverabilityTestReport $
--             newCreateDeliverabilityTestReportResponse
--
--         , responseDeleteEmailIdentity $
--             newDeleteEmailIdentityResponse
--
--         , responseListContactLists $
--             newListContactListsResponse
--
--         , responseDeleteContactList $
--             newDeleteContactListResponse
--
--         , responsePutDedicatedIpWarmupAttributes $
--             newPutDedicatedIpWarmupAttributesResponse
--
--         , responseGetCustomVerificationEmailTemplate $
--             newGetCustomVerificationEmailTemplateResponse
--
--         , responseCreateEmailTemplate $
--             newCreateEmailTemplateResponse
--
--         , responseListEmailTemplates $
--             newListEmailTemplatesResponse
--
--         , responseUpdateEmailTemplate $
--             newUpdateEmailTemplateResponse
--
--         , responseGetDomainStatisticsReport $
--             newGetDomainStatisticsReportResponse
--
--         , responseDeleteEmailTemplate $
--             newDeleteEmailTemplateResponse
--
--         , responsePutEmailIdentityFeedbackAttributes $
--             newPutEmailIdentityFeedbackAttributesResponse
--
--         , responseListSuppressedDestinations $
--             newListSuppressedDestinationsResponse
--
--         , responseDeleteSuppressedDestination $
--             newDeleteSuppressedDestinationResponse
--
--         , responseGetDeliverabilityDashboardOptions $
--             newGetDeliverabilityDashboardOptionsResponse
--
--         , responseCreateDedicatedIpPool $
--             newCreateDedicatedIpPoolResponse
--
--         , responseSendCustomVerificationEmail $
--             newSendCustomVerificationEmailResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetEmailIdentityPolicies $
--             newGetEmailIdentityPoliciesResponse
--
--           ]
--     ]

-- Requests

requestListDedicatedIpPools :: ListDedicatedIpPools -> TestTree
requestListDedicatedIpPools =
  req
    "ListDedicatedIpPools"
    "fixture/ListDedicatedIpPools.yaml"

requestPutEmailIdentityDkimAttributes :: PutEmailIdentityDkimAttributes -> TestTree
requestPutEmailIdentityDkimAttributes =
  req
    "PutEmailIdentityDkimAttributes"
    "fixture/PutEmailIdentityDkimAttributes.yaml"

requestDeleteDedicatedIpPool :: DeleteDedicatedIpPool -> TestTree
requestDeleteDedicatedIpPool =
  req
    "DeleteDedicatedIpPool"
    "fixture/DeleteDedicatedIpPool.yaml"

requestPutConfigurationSetDeliveryOptions :: PutConfigurationSetDeliveryOptions -> TestTree
requestPutConfigurationSetDeliveryOptions =
  req
    "PutConfigurationSetDeliveryOptions"
    "fixture/PutConfigurationSetDeliveryOptions.yaml"

requestCreateContact :: CreateContact -> TestTree
requestCreateContact =
  req
    "CreateContact"
    "fixture/CreateContact.yaml"

requestPutConfigurationSetTrackingOptions :: PutConfigurationSetTrackingOptions -> TestTree
requestPutConfigurationSetTrackingOptions =
  req
    "PutConfigurationSetTrackingOptions"
    "fixture/PutConfigurationSetTrackingOptions.yaml"

requestGetImportJob :: GetImportJob -> TestTree
requestGetImportJob =
  req
    "GetImportJob"
    "fixture/GetImportJob.yaml"

requestPutAccountDedicatedIpWarmupAttributes :: PutAccountDedicatedIpWarmupAttributes -> TestTree
requestPutAccountDedicatedIpWarmupAttributes =
  req
    "PutAccountDedicatedIpWarmupAttributes"
    "fixture/PutAccountDedicatedIpWarmupAttributes.yaml"

requestGetDeliverabilityTestReport :: GetDeliverabilityTestReport -> TestTree
requestGetDeliverabilityTestReport =
  req
    "GetDeliverabilityTestReport"
    "fixture/GetDeliverabilityTestReport.yaml"

requestPutEmailIdentityConfigurationSetAttributes :: PutEmailIdentityConfigurationSetAttributes -> TestTree
requestPutEmailIdentityConfigurationSetAttributes =
  req
    "PutEmailIdentityConfigurationSetAttributes"
    "fixture/PutEmailIdentityConfigurationSetAttributes.yaml"

requestCreateConfigurationSetEventDestination :: CreateConfigurationSetEventDestination -> TestTree
requestCreateConfigurationSetEventDestination =
  req
    "CreateConfigurationSetEventDestination"
    "fixture/CreateConfigurationSetEventDestination.yaml"

requestListEmailIdentities :: ListEmailIdentities -> TestTree
requestListEmailIdentities =
  req
    "ListEmailIdentities"
    "fixture/ListEmailIdentities.yaml"

requestCreateEmailIdentity :: CreateEmailIdentity -> TestTree
requestCreateEmailIdentity =
  req
    "CreateEmailIdentity"
    "fixture/CreateEmailIdentity.yaml"

requestGetBlacklistReports :: GetBlacklistReports -> TestTree
requestGetBlacklistReports =
  req
    "GetBlacklistReports"
    "fixture/GetBlacklistReports.yaml"

requestCreateConfigurationSet :: CreateConfigurationSet -> TestTree
requestCreateConfigurationSet =
  req
    "CreateConfigurationSet"
    "fixture/CreateConfigurationSet.yaml"

requestCreateContactList :: CreateContactList -> TestTree
requestCreateContactList =
  req
    "CreateContactList"
    "fixture/CreateContactList.yaml"

requestDeleteConfigurationSet :: DeleteConfigurationSet -> TestTree
requestDeleteConfigurationSet =
  req
    "DeleteConfigurationSet"
    "fixture/DeleteConfigurationSet.yaml"

requestListImportJobs :: ListImportJobs -> TestTree
requestListImportJobs =
  req
    "ListImportJobs"
    "fixture/ListImportJobs.yaml"

requestUpdateEmailIdentityPolicy :: UpdateEmailIdentityPolicy -> TestTree
requestUpdateEmailIdentityPolicy =
  req
    "UpdateEmailIdentityPolicy"
    "fixture/UpdateEmailIdentityPolicy.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteEmailIdentityPolicy :: DeleteEmailIdentityPolicy -> TestTree
requestDeleteEmailIdentityPolicy =
  req
    "DeleteEmailIdentityPolicy"
    "fixture/DeleteEmailIdentityPolicy.yaml"

requestUpdateConfigurationSetEventDestination :: UpdateConfigurationSetEventDestination -> TestTree
requestUpdateConfigurationSetEventDestination =
  req
    "UpdateConfigurationSetEventDestination"
    "fixture/UpdateConfigurationSetEventDestination.yaml"

requestDeleteConfigurationSetEventDestination :: DeleteConfigurationSetEventDestination -> TestTree
requestDeleteConfigurationSetEventDestination =
  req
    "DeleteConfigurationSetEventDestination"
    "fixture/DeleteConfigurationSetEventDestination.yaml"

requestTestRenderEmailTemplate :: TestRenderEmailTemplate -> TestTree
requestTestRenderEmailTemplate =
  req
    "TestRenderEmailTemplate"
    "fixture/TestRenderEmailTemplate.yaml"

requestSendEmail :: SendEmail -> TestTree
requestSendEmail =
  req
    "SendEmail"
    "fixture/SendEmail.yaml"

requestPutConfigurationSetReputationOptions :: PutConfigurationSetReputationOptions -> TestTree
requestPutConfigurationSetReputationOptions =
  req
    "PutConfigurationSetReputationOptions"
    "fixture/PutConfigurationSetReputationOptions.yaml"

requestPutDedicatedIpInPool :: PutDedicatedIpInPool -> TestTree
requestPutDedicatedIpInPool =
  req
    "PutDedicatedIpInPool"
    "fixture/PutDedicatedIpInPool.yaml"

requestSendBulkEmail :: SendBulkEmail -> TestTree
requestSendBulkEmail =
  req
    "SendBulkEmail"
    "fixture/SendBulkEmail.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListDomainDeliverabilityCampaigns :: ListDomainDeliverabilityCampaigns -> TestTree
requestListDomainDeliverabilityCampaigns =
  req
    "ListDomainDeliverabilityCampaigns"
    "fixture/ListDomainDeliverabilityCampaigns.yaml"

requestPutSuppressedDestination :: PutSuppressedDestination -> TestTree
requestPutSuppressedDestination =
  req
    "PutSuppressedDestination"
    "fixture/PutSuppressedDestination.yaml"

requestPutAccountDetails :: PutAccountDetails -> TestTree
requestPutAccountDetails =
  req
    "PutAccountDetails"
    "fixture/PutAccountDetails.yaml"

requestCreateCustomVerificationEmailTemplate :: CreateCustomVerificationEmailTemplate -> TestTree
requestCreateCustomVerificationEmailTemplate =
  req
    "CreateCustomVerificationEmailTemplate"
    "fixture/CreateCustomVerificationEmailTemplate.yaml"

requestPutConfigurationSetSuppressionOptions :: PutConfigurationSetSuppressionOptions -> TestTree
requestPutConfigurationSetSuppressionOptions =
  req
    "PutConfigurationSetSuppressionOptions"
    "fixture/PutConfigurationSetSuppressionOptions.yaml"

requestGetSuppressedDestination :: GetSuppressedDestination -> TestTree
requestGetSuppressedDestination =
  req
    "GetSuppressedDestination"
    "fixture/GetSuppressedDestination.yaml"

requestGetEmailTemplate :: GetEmailTemplate -> TestTree
requestGetEmailTemplate =
  req
    "GetEmailTemplate"
    "fixture/GetEmailTemplate.yaml"

requestListCustomVerificationEmailTemplates :: ListCustomVerificationEmailTemplates -> TestTree
requestListCustomVerificationEmailTemplates =
  req
    "ListCustomVerificationEmailTemplates"
    "fixture/ListCustomVerificationEmailTemplates.yaml"

requestGetDedicatedIps :: GetDedicatedIps -> TestTree
requestGetDedicatedIps =
  req
    "GetDedicatedIps"
    "fixture/GetDedicatedIps.yaml"

requestDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplate -> TestTree
requestDeleteCustomVerificationEmailTemplate =
  req
    "DeleteCustomVerificationEmailTemplate"
    "fixture/DeleteCustomVerificationEmailTemplate.yaml"

requestGetConfigurationSetEventDestinations :: GetConfigurationSetEventDestinations -> TestTree
requestGetConfigurationSetEventDestinations =
  req
    "GetConfigurationSetEventDestinations"
    "fixture/GetConfigurationSetEventDestinations.yaml"

requestPutConfigurationSetSendingOptions :: PutConfigurationSetSendingOptions -> TestTree
requestPutConfigurationSetSendingOptions =
  req
    "PutConfigurationSetSendingOptions"
    "fixture/PutConfigurationSetSendingOptions.yaml"

requestGetAccount :: GetAccount -> TestTree
requestGetAccount =
  req
    "GetAccount"
    "fixture/GetAccount.yaml"

requestUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplate -> TestTree
requestUpdateCustomVerificationEmailTemplate =
  req
    "UpdateCustomVerificationEmailTemplate"
    "fixture/UpdateCustomVerificationEmailTemplate.yaml"

requestGetDomainDeliverabilityCampaign :: GetDomainDeliverabilityCampaign -> TestTree
requestGetDomainDeliverabilityCampaign =
  req
    "GetDomainDeliverabilityCampaign"
    "fixture/GetDomainDeliverabilityCampaign.yaml"

requestGetDedicatedIp :: GetDedicatedIp -> TestTree
requestGetDedicatedIp =
  req
    "GetDedicatedIp"
    "fixture/GetDedicatedIp.yaml"

requestPutEmailIdentityDkimSigningAttributes :: PutEmailIdentityDkimSigningAttributes -> TestTree
requestPutEmailIdentityDkimSigningAttributes =
  req
    "PutEmailIdentityDkimSigningAttributes"
    "fixture/PutEmailIdentityDkimSigningAttributes.yaml"

requestGetConfigurationSet :: GetConfigurationSet -> TestTree
requestGetConfigurationSet =
  req
    "GetConfigurationSet"
    "fixture/GetConfigurationSet.yaml"

requestGetContactList :: GetContactList -> TestTree
requestGetContactList =
  req
    "GetContactList"
    "fixture/GetContactList.yaml"

requestGetEmailIdentity :: GetEmailIdentity -> TestTree
requestGetEmailIdentity =
  req
    "GetEmailIdentity"
    "fixture/GetEmailIdentity.yaml"

requestUpdateContact :: UpdateContact -> TestTree
requestUpdateContact =
  req
    "UpdateContact"
    "fixture/UpdateContact.yaml"

requestListContacts :: ListContacts -> TestTree
requestListContacts =
  req
    "ListContacts"
    "fixture/ListContacts.yaml"

requestDeleteContact :: DeleteContact -> TestTree
requestDeleteContact =
  req
    "DeleteContact"
    "fixture/DeleteContact.yaml"

requestPutDeliverabilityDashboardOption :: PutDeliverabilityDashboardOption -> TestTree
requestPutDeliverabilityDashboardOption =
  req
    "PutDeliverabilityDashboardOption"
    "fixture/PutDeliverabilityDashboardOption.yaml"

requestListDeliverabilityTestReports :: ListDeliverabilityTestReports -> TestTree
requestListDeliverabilityTestReports =
  req
    "ListDeliverabilityTestReports"
    "fixture/ListDeliverabilityTestReports.yaml"

requestCreateEmailIdentityPolicy :: CreateEmailIdentityPolicy -> TestTree
requestCreateEmailIdentityPolicy =
  req
    "CreateEmailIdentityPolicy"
    "fixture/CreateEmailIdentityPolicy.yaml"

requestPutAccountSuppressionAttributes :: PutAccountSuppressionAttributes -> TestTree
requestPutAccountSuppressionAttributes =
  req
    "PutAccountSuppressionAttributes"
    "fixture/PutAccountSuppressionAttributes.yaml"

requestCreateImportJob :: CreateImportJob -> TestTree
requestCreateImportJob =
  req
    "CreateImportJob"
    "fixture/CreateImportJob.yaml"

requestGetContact :: GetContact -> TestTree
requestGetContact =
  req
    "GetContact"
    "fixture/GetContact.yaml"

requestListConfigurationSets :: ListConfigurationSets -> TestTree
requestListConfigurationSets =
  req
    "ListConfigurationSets"
    "fixture/ListConfigurationSets.yaml"

requestUpdateContactList :: UpdateContactList -> TestTree
requestUpdateContactList =
  req
    "UpdateContactList"
    "fixture/UpdateContactList.yaml"

requestPutAccountSendingAttributes :: PutAccountSendingAttributes -> TestTree
requestPutAccountSendingAttributes =
  req
    "PutAccountSendingAttributes"
    "fixture/PutAccountSendingAttributes.yaml"

requestPutEmailIdentityMailFromAttributes :: PutEmailIdentityMailFromAttributes -> TestTree
requestPutEmailIdentityMailFromAttributes =
  req
    "PutEmailIdentityMailFromAttributes"
    "fixture/PutEmailIdentityMailFromAttributes.yaml"

requestCreateDeliverabilityTestReport :: CreateDeliverabilityTestReport -> TestTree
requestCreateDeliverabilityTestReport =
  req
    "CreateDeliverabilityTestReport"
    "fixture/CreateDeliverabilityTestReport.yaml"

requestDeleteEmailIdentity :: DeleteEmailIdentity -> TestTree
requestDeleteEmailIdentity =
  req
    "DeleteEmailIdentity"
    "fixture/DeleteEmailIdentity.yaml"

requestListContactLists :: ListContactLists -> TestTree
requestListContactLists =
  req
    "ListContactLists"
    "fixture/ListContactLists.yaml"

requestDeleteContactList :: DeleteContactList -> TestTree
requestDeleteContactList =
  req
    "DeleteContactList"
    "fixture/DeleteContactList.yaml"

requestPutDedicatedIpWarmupAttributes :: PutDedicatedIpWarmupAttributes -> TestTree
requestPutDedicatedIpWarmupAttributes =
  req
    "PutDedicatedIpWarmupAttributes"
    "fixture/PutDedicatedIpWarmupAttributes.yaml"

requestGetCustomVerificationEmailTemplate :: GetCustomVerificationEmailTemplate -> TestTree
requestGetCustomVerificationEmailTemplate =
  req
    "GetCustomVerificationEmailTemplate"
    "fixture/GetCustomVerificationEmailTemplate.yaml"

requestCreateEmailTemplate :: CreateEmailTemplate -> TestTree
requestCreateEmailTemplate =
  req
    "CreateEmailTemplate"
    "fixture/CreateEmailTemplate.yaml"

requestListEmailTemplates :: ListEmailTemplates -> TestTree
requestListEmailTemplates =
  req
    "ListEmailTemplates"
    "fixture/ListEmailTemplates.yaml"

requestUpdateEmailTemplate :: UpdateEmailTemplate -> TestTree
requestUpdateEmailTemplate =
  req
    "UpdateEmailTemplate"
    "fixture/UpdateEmailTemplate.yaml"

requestGetDomainStatisticsReport :: GetDomainStatisticsReport -> TestTree
requestGetDomainStatisticsReport =
  req
    "GetDomainStatisticsReport"
    "fixture/GetDomainStatisticsReport.yaml"

requestDeleteEmailTemplate :: DeleteEmailTemplate -> TestTree
requestDeleteEmailTemplate =
  req
    "DeleteEmailTemplate"
    "fixture/DeleteEmailTemplate.yaml"

requestPutEmailIdentityFeedbackAttributes :: PutEmailIdentityFeedbackAttributes -> TestTree
requestPutEmailIdentityFeedbackAttributes =
  req
    "PutEmailIdentityFeedbackAttributes"
    "fixture/PutEmailIdentityFeedbackAttributes.yaml"

requestListSuppressedDestinations :: ListSuppressedDestinations -> TestTree
requestListSuppressedDestinations =
  req
    "ListSuppressedDestinations"
    "fixture/ListSuppressedDestinations.yaml"

requestDeleteSuppressedDestination :: DeleteSuppressedDestination -> TestTree
requestDeleteSuppressedDestination =
  req
    "DeleteSuppressedDestination"
    "fixture/DeleteSuppressedDestination.yaml"

requestGetDeliverabilityDashboardOptions :: GetDeliverabilityDashboardOptions -> TestTree
requestGetDeliverabilityDashboardOptions =
  req
    "GetDeliverabilityDashboardOptions"
    "fixture/GetDeliverabilityDashboardOptions.yaml"

requestCreateDedicatedIpPool :: CreateDedicatedIpPool -> TestTree
requestCreateDedicatedIpPool =
  req
    "CreateDedicatedIpPool"
    "fixture/CreateDedicatedIpPool.yaml"

requestSendCustomVerificationEmail :: SendCustomVerificationEmail -> TestTree
requestSendCustomVerificationEmail =
  req
    "SendCustomVerificationEmail"
    "fixture/SendCustomVerificationEmail.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetEmailIdentityPolicies :: GetEmailIdentityPolicies -> TestTree
requestGetEmailIdentityPolicies =
  req
    "GetEmailIdentityPolicies"
    "fixture/GetEmailIdentityPolicies.yaml"

-- Responses

responseListDedicatedIpPools :: ListDedicatedIpPoolsResponse -> TestTree
responseListDedicatedIpPools =
  res
    "ListDedicatedIpPoolsResponse"
    "fixture/ListDedicatedIpPoolsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDedicatedIpPools)

responsePutEmailIdentityDkimAttributes :: PutEmailIdentityDkimAttributesResponse -> TestTree
responsePutEmailIdentityDkimAttributes =
  res
    "PutEmailIdentityDkimAttributesResponse"
    "fixture/PutEmailIdentityDkimAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutEmailIdentityDkimAttributes)

responseDeleteDedicatedIpPool :: DeleteDedicatedIpPoolResponse -> TestTree
responseDeleteDedicatedIpPool =
  res
    "DeleteDedicatedIpPoolResponse"
    "fixture/DeleteDedicatedIpPoolResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDedicatedIpPool)

responsePutConfigurationSetDeliveryOptions :: PutConfigurationSetDeliveryOptionsResponse -> TestTree
responsePutConfigurationSetDeliveryOptions =
  res
    "PutConfigurationSetDeliveryOptionsResponse"
    "fixture/PutConfigurationSetDeliveryOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutConfigurationSetDeliveryOptions)

responseCreateContact :: CreateContactResponse -> TestTree
responseCreateContact =
  res
    "CreateContactResponse"
    "fixture/CreateContactResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContact)

responsePutConfigurationSetTrackingOptions :: PutConfigurationSetTrackingOptionsResponse -> TestTree
responsePutConfigurationSetTrackingOptions =
  res
    "PutConfigurationSetTrackingOptionsResponse"
    "fixture/PutConfigurationSetTrackingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutConfigurationSetTrackingOptions)

responseGetImportJob :: GetImportJobResponse -> TestTree
responseGetImportJob =
  res
    "GetImportJobResponse"
    "fixture/GetImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy GetImportJob)

responsePutAccountDedicatedIpWarmupAttributes :: PutAccountDedicatedIpWarmupAttributesResponse -> TestTree
responsePutAccountDedicatedIpWarmupAttributes =
  res
    "PutAccountDedicatedIpWarmupAttributesResponse"
    "fixture/PutAccountDedicatedIpWarmupAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutAccountDedicatedIpWarmupAttributes)

responseGetDeliverabilityTestReport :: GetDeliverabilityTestReportResponse -> TestTree
responseGetDeliverabilityTestReport =
  res
    "GetDeliverabilityTestReportResponse"
    "fixture/GetDeliverabilityTestReportResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeliverabilityTestReport)

responsePutEmailIdentityConfigurationSetAttributes :: PutEmailIdentityConfigurationSetAttributesResponse -> TestTree
responsePutEmailIdentityConfigurationSetAttributes =
  res
    "PutEmailIdentityConfigurationSetAttributesResponse"
    "fixture/PutEmailIdentityConfigurationSetAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutEmailIdentityConfigurationSetAttributes)

responseCreateConfigurationSetEventDestination :: CreateConfigurationSetEventDestinationResponse -> TestTree
responseCreateConfigurationSetEventDestination =
  res
    "CreateConfigurationSetEventDestinationResponse"
    "fixture/CreateConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConfigurationSetEventDestination)

responseListEmailIdentities :: ListEmailIdentitiesResponse -> TestTree
responseListEmailIdentities =
  res
    "ListEmailIdentitiesResponse"
    "fixture/ListEmailIdentitiesResponse.proto"
    defaultService
    (Proxy :: Proxy ListEmailIdentities)

responseCreateEmailIdentity :: CreateEmailIdentityResponse -> TestTree
responseCreateEmailIdentity =
  res
    "CreateEmailIdentityResponse"
    "fixture/CreateEmailIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEmailIdentity)

responseGetBlacklistReports :: GetBlacklistReportsResponse -> TestTree
responseGetBlacklistReports =
  res
    "GetBlacklistReportsResponse"
    "fixture/GetBlacklistReportsResponse.proto"
    defaultService
    (Proxy :: Proxy GetBlacklistReports)

responseCreateConfigurationSet :: CreateConfigurationSetResponse -> TestTree
responseCreateConfigurationSet =
  res
    "CreateConfigurationSetResponse"
    "fixture/CreateConfigurationSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConfigurationSet)

responseCreateContactList :: CreateContactListResponse -> TestTree
responseCreateContactList =
  res
    "CreateContactListResponse"
    "fixture/CreateContactListResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContactList)

responseDeleteConfigurationSet :: DeleteConfigurationSetResponse -> TestTree
responseDeleteConfigurationSet =
  res
    "DeleteConfigurationSetResponse"
    "fixture/DeleteConfigurationSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfigurationSet)

responseListImportJobs :: ListImportJobsResponse -> TestTree
responseListImportJobs =
  res
    "ListImportJobsResponse"
    "fixture/ListImportJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListImportJobs)

responseUpdateEmailIdentityPolicy :: UpdateEmailIdentityPolicyResponse -> TestTree
responseUpdateEmailIdentityPolicy =
  res
    "UpdateEmailIdentityPolicyResponse"
    "fixture/UpdateEmailIdentityPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEmailIdentityPolicy)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDeleteEmailIdentityPolicy :: DeleteEmailIdentityPolicyResponse -> TestTree
responseDeleteEmailIdentityPolicy =
  res
    "DeleteEmailIdentityPolicyResponse"
    "fixture/DeleteEmailIdentityPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEmailIdentityPolicy)

responseUpdateConfigurationSetEventDestination :: UpdateConfigurationSetEventDestinationResponse -> TestTree
responseUpdateConfigurationSetEventDestination =
  res
    "UpdateConfigurationSetEventDestinationResponse"
    "fixture/UpdateConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConfigurationSetEventDestination)

responseDeleteConfigurationSetEventDestination :: DeleteConfigurationSetEventDestinationResponse -> TestTree
responseDeleteConfigurationSetEventDestination =
  res
    "DeleteConfigurationSetEventDestinationResponse"
    "fixture/DeleteConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfigurationSetEventDestination)

responseTestRenderEmailTemplate :: TestRenderEmailTemplateResponse -> TestTree
responseTestRenderEmailTemplate =
  res
    "TestRenderEmailTemplateResponse"
    "fixture/TestRenderEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy TestRenderEmailTemplate)

responseSendEmail :: SendEmailResponse -> TestTree
responseSendEmail =
  res
    "SendEmailResponse"
    "fixture/SendEmailResponse.proto"
    defaultService
    (Proxy :: Proxy SendEmail)

responsePutConfigurationSetReputationOptions :: PutConfigurationSetReputationOptionsResponse -> TestTree
responsePutConfigurationSetReputationOptions =
  res
    "PutConfigurationSetReputationOptionsResponse"
    "fixture/PutConfigurationSetReputationOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutConfigurationSetReputationOptions)

responsePutDedicatedIpInPool :: PutDedicatedIpInPoolResponse -> TestTree
responsePutDedicatedIpInPool =
  res
    "PutDedicatedIpInPoolResponse"
    "fixture/PutDedicatedIpInPoolResponse.proto"
    defaultService
    (Proxy :: Proxy PutDedicatedIpInPool)

responseSendBulkEmail :: SendBulkEmailResponse -> TestTree
responseSendBulkEmail =
  res
    "SendBulkEmailResponse"
    "fixture/SendBulkEmailResponse.proto"
    defaultService
    (Proxy :: Proxy SendBulkEmail)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListDomainDeliverabilityCampaigns :: ListDomainDeliverabilityCampaignsResponse -> TestTree
responseListDomainDeliverabilityCampaigns =
  res
    "ListDomainDeliverabilityCampaignsResponse"
    "fixture/ListDomainDeliverabilityCampaignsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDomainDeliverabilityCampaigns)

responsePutSuppressedDestination :: PutSuppressedDestinationResponse -> TestTree
responsePutSuppressedDestination =
  res
    "PutSuppressedDestinationResponse"
    "fixture/PutSuppressedDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy PutSuppressedDestination)

responsePutAccountDetails :: PutAccountDetailsResponse -> TestTree
responsePutAccountDetails =
  res
    "PutAccountDetailsResponse"
    "fixture/PutAccountDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy PutAccountDetails)

responseCreateCustomVerificationEmailTemplate :: CreateCustomVerificationEmailTemplateResponse -> TestTree
responseCreateCustomVerificationEmailTemplate =
  res
    "CreateCustomVerificationEmailTemplateResponse"
    "fixture/CreateCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCustomVerificationEmailTemplate)

responsePutConfigurationSetSuppressionOptions :: PutConfigurationSetSuppressionOptionsResponse -> TestTree
responsePutConfigurationSetSuppressionOptions =
  res
    "PutConfigurationSetSuppressionOptionsResponse"
    "fixture/PutConfigurationSetSuppressionOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutConfigurationSetSuppressionOptions)

responseGetSuppressedDestination :: GetSuppressedDestinationResponse -> TestTree
responseGetSuppressedDestination =
  res
    "GetSuppressedDestinationResponse"
    "fixture/GetSuppressedDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy GetSuppressedDestination)

responseGetEmailTemplate :: GetEmailTemplateResponse -> TestTree
responseGetEmailTemplate =
  res
    "GetEmailTemplateResponse"
    "fixture/GetEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetEmailTemplate)

responseListCustomVerificationEmailTemplates :: ListCustomVerificationEmailTemplatesResponse -> TestTree
responseListCustomVerificationEmailTemplates =
  res
    "ListCustomVerificationEmailTemplatesResponse"
    "fixture/ListCustomVerificationEmailTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListCustomVerificationEmailTemplates)

responseGetDedicatedIps :: GetDedicatedIpsResponse -> TestTree
responseGetDedicatedIps =
  res
    "GetDedicatedIpsResponse"
    "fixture/GetDedicatedIpsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDedicatedIps)

responseDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplateResponse -> TestTree
responseDeleteCustomVerificationEmailTemplate =
  res
    "DeleteCustomVerificationEmailTemplateResponse"
    "fixture/DeleteCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCustomVerificationEmailTemplate)

responseGetConfigurationSetEventDestinations :: GetConfigurationSetEventDestinationsResponse -> TestTree
responseGetConfigurationSetEventDestinations =
  res
    "GetConfigurationSetEventDestinationsResponse"
    "fixture/GetConfigurationSetEventDestinationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetConfigurationSetEventDestinations)

responsePutConfigurationSetSendingOptions :: PutConfigurationSetSendingOptionsResponse -> TestTree
responsePutConfigurationSetSendingOptions =
  res
    "PutConfigurationSetSendingOptionsResponse"
    "fixture/PutConfigurationSetSendingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutConfigurationSetSendingOptions)

responseGetAccount :: GetAccountResponse -> TestTree
responseGetAccount =
  res
    "GetAccountResponse"
    "fixture/GetAccountResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccount)

responseUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplateResponse -> TestTree
responseUpdateCustomVerificationEmailTemplate =
  res
    "UpdateCustomVerificationEmailTemplateResponse"
    "fixture/UpdateCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCustomVerificationEmailTemplate)

responseGetDomainDeliverabilityCampaign :: GetDomainDeliverabilityCampaignResponse -> TestTree
responseGetDomainDeliverabilityCampaign =
  res
    "GetDomainDeliverabilityCampaignResponse"
    "fixture/GetDomainDeliverabilityCampaignResponse.proto"
    defaultService
    (Proxy :: Proxy GetDomainDeliverabilityCampaign)

responseGetDedicatedIp :: GetDedicatedIpResponse -> TestTree
responseGetDedicatedIp =
  res
    "GetDedicatedIpResponse"
    "fixture/GetDedicatedIpResponse.proto"
    defaultService
    (Proxy :: Proxy GetDedicatedIp)

responsePutEmailIdentityDkimSigningAttributes :: PutEmailIdentityDkimSigningAttributesResponse -> TestTree
responsePutEmailIdentityDkimSigningAttributes =
  res
    "PutEmailIdentityDkimSigningAttributesResponse"
    "fixture/PutEmailIdentityDkimSigningAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutEmailIdentityDkimSigningAttributes)

responseGetConfigurationSet :: GetConfigurationSetResponse -> TestTree
responseGetConfigurationSet =
  res
    "GetConfigurationSetResponse"
    "fixture/GetConfigurationSetResponse.proto"
    defaultService
    (Proxy :: Proxy GetConfigurationSet)

responseGetContactList :: GetContactListResponse -> TestTree
responseGetContactList =
  res
    "GetContactListResponse"
    "fixture/GetContactListResponse.proto"
    defaultService
    (Proxy :: Proxy GetContactList)

responseGetEmailIdentity :: GetEmailIdentityResponse -> TestTree
responseGetEmailIdentity =
  res
    "GetEmailIdentityResponse"
    "fixture/GetEmailIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy GetEmailIdentity)

responseUpdateContact :: UpdateContactResponse -> TestTree
responseUpdateContact =
  res
    "UpdateContactResponse"
    "fixture/UpdateContactResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContact)

responseListContacts :: ListContactsResponse -> TestTree
responseListContacts =
  res
    "ListContactsResponse"
    "fixture/ListContactsResponse.proto"
    defaultService
    (Proxy :: Proxy ListContacts)

responseDeleteContact :: DeleteContactResponse -> TestTree
responseDeleteContact =
  res
    "DeleteContactResponse"
    "fixture/DeleteContactResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteContact)

responsePutDeliverabilityDashboardOption :: PutDeliverabilityDashboardOptionResponse -> TestTree
responsePutDeliverabilityDashboardOption =
  res
    "PutDeliverabilityDashboardOptionResponse"
    "fixture/PutDeliverabilityDashboardOptionResponse.proto"
    defaultService
    (Proxy :: Proxy PutDeliverabilityDashboardOption)

responseListDeliverabilityTestReports :: ListDeliverabilityTestReportsResponse -> TestTree
responseListDeliverabilityTestReports =
  res
    "ListDeliverabilityTestReportsResponse"
    "fixture/ListDeliverabilityTestReportsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeliverabilityTestReports)

responseCreateEmailIdentityPolicy :: CreateEmailIdentityPolicyResponse -> TestTree
responseCreateEmailIdentityPolicy =
  res
    "CreateEmailIdentityPolicyResponse"
    "fixture/CreateEmailIdentityPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEmailIdentityPolicy)

responsePutAccountSuppressionAttributes :: PutAccountSuppressionAttributesResponse -> TestTree
responsePutAccountSuppressionAttributes =
  res
    "PutAccountSuppressionAttributesResponse"
    "fixture/PutAccountSuppressionAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutAccountSuppressionAttributes)

responseCreateImportJob :: CreateImportJobResponse -> TestTree
responseCreateImportJob =
  res
    "CreateImportJobResponse"
    "fixture/CreateImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImportJob)

responseGetContact :: GetContactResponse -> TestTree
responseGetContact =
  res
    "GetContactResponse"
    "fixture/GetContactResponse.proto"
    defaultService
    (Proxy :: Proxy GetContact)

responseListConfigurationSets :: ListConfigurationSetsResponse -> TestTree
responseListConfigurationSets =
  res
    "ListConfigurationSetsResponse"
    "fixture/ListConfigurationSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListConfigurationSets)

responseUpdateContactList :: UpdateContactListResponse -> TestTree
responseUpdateContactList =
  res
    "UpdateContactListResponse"
    "fixture/UpdateContactListResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContactList)

responsePutAccountSendingAttributes :: PutAccountSendingAttributesResponse -> TestTree
responsePutAccountSendingAttributes =
  res
    "PutAccountSendingAttributesResponse"
    "fixture/PutAccountSendingAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutAccountSendingAttributes)

responsePutEmailIdentityMailFromAttributes :: PutEmailIdentityMailFromAttributesResponse -> TestTree
responsePutEmailIdentityMailFromAttributes =
  res
    "PutEmailIdentityMailFromAttributesResponse"
    "fixture/PutEmailIdentityMailFromAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutEmailIdentityMailFromAttributes)

responseCreateDeliverabilityTestReport :: CreateDeliverabilityTestReportResponse -> TestTree
responseCreateDeliverabilityTestReport =
  res
    "CreateDeliverabilityTestReportResponse"
    "fixture/CreateDeliverabilityTestReportResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeliverabilityTestReport)

responseDeleteEmailIdentity :: DeleteEmailIdentityResponse -> TestTree
responseDeleteEmailIdentity =
  res
    "DeleteEmailIdentityResponse"
    "fixture/DeleteEmailIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEmailIdentity)

responseListContactLists :: ListContactListsResponse -> TestTree
responseListContactLists =
  res
    "ListContactListsResponse"
    "fixture/ListContactListsResponse.proto"
    defaultService
    (Proxy :: Proxy ListContactLists)

responseDeleteContactList :: DeleteContactListResponse -> TestTree
responseDeleteContactList =
  res
    "DeleteContactListResponse"
    "fixture/DeleteContactListResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteContactList)

responsePutDedicatedIpWarmupAttributes :: PutDedicatedIpWarmupAttributesResponse -> TestTree
responsePutDedicatedIpWarmupAttributes =
  res
    "PutDedicatedIpWarmupAttributesResponse"
    "fixture/PutDedicatedIpWarmupAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutDedicatedIpWarmupAttributes)

responseGetCustomVerificationEmailTemplate :: GetCustomVerificationEmailTemplateResponse -> TestTree
responseGetCustomVerificationEmailTemplate =
  res
    "GetCustomVerificationEmailTemplateResponse"
    "fixture/GetCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetCustomVerificationEmailTemplate)

responseCreateEmailTemplate :: CreateEmailTemplateResponse -> TestTree
responseCreateEmailTemplate =
  res
    "CreateEmailTemplateResponse"
    "fixture/CreateEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEmailTemplate)

responseListEmailTemplates :: ListEmailTemplatesResponse -> TestTree
responseListEmailTemplates =
  res
    "ListEmailTemplatesResponse"
    "fixture/ListEmailTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListEmailTemplates)

responseUpdateEmailTemplate :: UpdateEmailTemplateResponse -> TestTree
responseUpdateEmailTemplate =
  res
    "UpdateEmailTemplateResponse"
    "fixture/UpdateEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEmailTemplate)

responseGetDomainStatisticsReport :: GetDomainStatisticsReportResponse -> TestTree
responseGetDomainStatisticsReport =
  res
    "GetDomainStatisticsReportResponse"
    "fixture/GetDomainStatisticsReportResponse.proto"
    defaultService
    (Proxy :: Proxy GetDomainStatisticsReport)

responseDeleteEmailTemplate :: DeleteEmailTemplateResponse -> TestTree
responseDeleteEmailTemplate =
  res
    "DeleteEmailTemplateResponse"
    "fixture/DeleteEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEmailTemplate)

responsePutEmailIdentityFeedbackAttributes :: PutEmailIdentityFeedbackAttributesResponse -> TestTree
responsePutEmailIdentityFeedbackAttributes =
  res
    "PutEmailIdentityFeedbackAttributesResponse"
    "fixture/PutEmailIdentityFeedbackAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutEmailIdentityFeedbackAttributes)

responseListSuppressedDestinations :: ListSuppressedDestinationsResponse -> TestTree
responseListSuppressedDestinations =
  res
    "ListSuppressedDestinationsResponse"
    "fixture/ListSuppressedDestinationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSuppressedDestinations)

responseDeleteSuppressedDestination :: DeleteSuppressedDestinationResponse -> TestTree
responseDeleteSuppressedDestination =
  res
    "DeleteSuppressedDestinationResponse"
    "fixture/DeleteSuppressedDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSuppressedDestination)

responseGetDeliverabilityDashboardOptions :: GetDeliverabilityDashboardOptionsResponse -> TestTree
responseGetDeliverabilityDashboardOptions =
  res
    "GetDeliverabilityDashboardOptionsResponse"
    "fixture/GetDeliverabilityDashboardOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeliverabilityDashboardOptions)

responseCreateDedicatedIpPool :: CreateDedicatedIpPoolResponse -> TestTree
responseCreateDedicatedIpPool =
  res
    "CreateDedicatedIpPoolResponse"
    "fixture/CreateDedicatedIpPoolResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDedicatedIpPool)

responseSendCustomVerificationEmail :: SendCustomVerificationEmailResponse -> TestTree
responseSendCustomVerificationEmail =
  res
    "SendCustomVerificationEmailResponse"
    "fixture/SendCustomVerificationEmailResponse.proto"
    defaultService
    (Proxy :: Proxy SendCustomVerificationEmail)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseGetEmailIdentityPolicies :: GetEmailIdentityPoliciesResponse -> TestTree
responseGetEmailIdentityPolicies =
  res
    "GetEmailIdentityPoliciesResponse"
    "fixture/GetEmailIdentityPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy GetEmailIdentityPolicies)
