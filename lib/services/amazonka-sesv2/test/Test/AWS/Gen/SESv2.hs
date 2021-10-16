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
--         [ requestPutEmailIdentityDkimAttributes $
--             newPutEmailIdentityDkimAttributes
--
--         , requestPutConfigurationSetDeliveryOptions $
--             newPutConfigurationSetDeliveryOptions
--
--         , requestListDedicatedIpPools $
--             newListDedicatedIpPools
--
--         , requestDeleteDedicatedIpPool $
--             newDeleteDedicatedIpPool
--
--         , requestGetImportJob $
--             newGetImportJob
--
--         , requestCreateContact $
--             newCreateContact
--
--         , requestPutConfigurationSetTrackingOptions $
--             newPutConfigurationSetTrackingOptions
--
--         , requestGetDeliverabilityTestReport $
--             newGetDeliverabilityTestReport
--
--         , requestPutAccountDedicatedIpWarmupAttributes $
--             newPutAccountDedicatedIpWarmupAttributes
--
--         , requestCreateEmailIdentity $
--             newCreateEmailIdentity
--
--         , requestGetBlacklistReports $
--             newGetBlacklistReports
--
--         , requestCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestination
--
--         , requestListEmailIdentities $
--             newListEmailIdentities
--
--         , requestCreateContactList $
--             newCreateContactList
--
--         , requestCreateConfigurationSet $
--             newCreateConfigurationSet
--
--         , requestPutEmailIdentityConfigurationSetAttributes $
--             newPutEmailIdentityConfigurationSetAttributes
--
--         , requestDeleteConfigurationSet $
--             newDeleteConfigurationSet
--
--         , requestDeleteEmailIdentityPolicy $
--             newDeleteEmailIdentityPolicy
--
--         , requestUpdateEmailIdentityPolicy $
--             newUpdateEmailIdentityPolicy
--
--         , requestUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestination
--
--         , requestListImportJobs $
--             newListImportJobs
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestination
--
--         , requestSendEmail $
--             newSendEmail
--
--         , requestPutConfigurationSetReputationOptions $
--             newPutConfigurationSetReputationOptions
--
--         , requestSendBulkEmail $
--             newSendBulkEmail
--
--         , requestTestRenderEmailTemplate $
--             newTestRenderEmailTemplate
--
--         , requestTagResource $
--             newTagResource
--
--         , requestPutDedicatedIpInPool $
--             newPutDedicatedIpInPool
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
--         , requestGetSuppressedDestination $
--             newGetSuppressedDestination
--
--         , requestGetEmailTemplate $
--             newGetEmailTemplate
--
--         , requestPutConfigurationSetSuppressionOptions $
--             newPutConfigurationSetSuppressionOptions
--
--         , requestDeleteCustomVerificationEmailTemplate $
--             newDeleteCustomVerificationEmailTemplate
--
--         , requestGetDomainDeliverabilityCampaign $
--             newGetDomainDeliverabilityCampaign
--
--         , requestUpdateCustomVerificationEmailTemplate $
--             newUpdateCustomVerificationEmailTemplate
--
--         , requestGetConfigurationSetEventDestinations $
--             newGetConfigurationSetEventDestinations
--
--         , requestListCustomVerificationEmailTemplates $
--             newListCustomVerificationEmailTemplates
--
--         , requestGetDedicatedIps $
--             newGetDedicatedIps
--
--         , requestGetAccount $
--             newGetAccount
--
--         , requestPutConfigurationSetSendingOptions $
--             newPutConfigurationSetSendingOptions
--
--         , requestGetConfigurationSet $
--             newGetConfigurationSet
--
--         , requestGetDedicatedIp $
--             newGetDedicatedIp
--
--         , requestGetEmailIdentity $
--             newGetEmailIdentity
--
--         , requestPutEmailIdentityDkimSigningAttributes $
--             newPutEmailIdentityDkimSigningAttributes
--
--         , requestGetContactList $
--             newGetContactList
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
--         , requestCreateImportJob $
--             newCreateImportJob
--
--         , requestGetContact $
--             newGetContact
--
--         , requestPutAccountSuppressionAttributes $
--             newPutAccountSuppressionAttributes
--
--         , requestCreateEmailIdentityPolicy $
--             newCreateEmailIdentityPolicy
--
--         , requestPutAccountSendingAttributes $
--             newPutAccountSendingAttributes
--
--         , requestUpdateContactList $
--             newUpdateContactList
--
--         , requestListConfigurationSets $
--             newListConfigurationSets
--
--         , requestListContactLists $
--             newListContactLists
--
--         , requestDeleteContactList $
--             newDeleteContactList
--
--         , requestCreateDeliverabilityTestReport $
--             newCreateDeliverabilityTestReport
--
--         , requestDeleteEmailIdentity $
--             newDeleteEmailIdentity
--
--         , requestPutEmailIdentityMailFromAttributes $
--             newPutEmailIdentityMailFromAttributes
--
--         , requestGetCustomVerificationEmailTemplate $
--             newGetCustomVerificationEmailTemplate
--
--         , requestPutDedicatedIpWarmupAttributes $
--             newPutDedicatedIpWarmupAttributes
--
--         , requestCreateEmailTemplate $
--             newCreateEmailTemplate
--
--         , requestGetDomainStatisticsReport $
--             newGetDomainStatisticsReport
--
--         , requestPutEmailIdentityFeedbackAttributes $
--             newPutEmailIdentityFeedbackAttributes
--
--         , requestGetDeliverabilityDashboardOptions $
--             newGetDeliverabilityDashboardOptions
--
--         , requestListEmailTemplates $
--             newListEmailTemplates
--
--         , requestDeleteEmailTemplate $
--             newDeleteEmailTemplate
--
--         , requestListSuppressedDestinations $
--             newListSuppressedDestinations
--
--         , requestUpdateEmailTemplate $
--             newUpdateEmailTemplate
--
--         , requestDeleteSuppressedDestination $
--             newDeleteSuppressedDestination
--
--         , requestGetEmailIdentityPolicies $
--             newGetEmailIdentityPolicies
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestSendCustomVerificationEmail $
--             newSendCustomVerificationEmail
--
--         , requestCreateDedicatedIpPool $
--             newCreateDedicatedIpPool
--
--           ]

--     , testGroup "response"
--         [ responsePutEmailIdentityDkimAttributes $
--             newPutEmailIdentityDkimAttributesResponse
--
--         , responsePutConfigurationSetDeliveryOptions $
--             newPutConfigurationSetDeliveryOptionsResponse
--
--         , responseListDedicatedIpPools $
--             newListDedicatedIpPoolsResponse
--
--         , responseDeleteDedicatedIpPool $
--             newDeleteDedicatedIpPoolResponse
--
--         , responseGetImportJob $
--             newGetImportJobResponse
--
--         , responseCreateContact $
--             newCreateContactResponse
--
--         , responsePutConfigurationSetTrackingOptions $
--             newPutConfigurationSetTrackingOptionsResponse
--
--         , responseGetDeliverabilityTestReport $
--             newGetDeliverabilityTestReportResponse
--
--         , responsePutAccountDedicatedIpWarmupAttributes $
--             newPutAccountDedicatedIpWarmupAttributesResponse
--
--         , responseCreateEmailIdentity $
--             newCreateEmailIdentityResponse
--
--         , responseGetBlacklistReports $
--             newGetBlacklistReportsResponse
--
--         , responseCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestinationResponse
--
--         , responseListEmailIdentities $
--             newListEmailIdentitiesResponse
--
--         , responseCreateContactList $
--             newCreateContactListResponse
--
--         , responseCreateConfigurationSet $
--             newCreateConfigurationSetResponse
--
--         , responsePutEmailIdentityConfigurationSetAttributes $
--             newPutEmailIdentityConfigurationSetAttributesResponse
--
--         , responseDeleteConfigurationSet $
--             newDeleteConfigurationSetResponse
--
--         , responseDeleteEmailIdentityPolicy $
--             newDeleteEmailIdentityPolicyResponse
--
--         , responseUpdateEmailIdentityPolicy $
--             newUpdateEmailIdentityPolicyResponse
--
--         , responseUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestinationResponse
--
--         , responseListImportJobs $
--             newListImportJobsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestinationResponse
--
--         , responseSendEmail $
--             newSendEmailResponse
--
--         , responsePutConfigurationSetReputationOptions $
--             newPutConfigurationSetReputationOptionsResponse
--
--         , responseSendBulkEmail $
--             newSendBulkEmailResponse
--
--         , responseTestRenderEmailTemplate $
--             newTestRenderEmailTemplateResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responsePutDedicatedIpInPool $
--             newPutDedicatedIpInPoolResponse
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
--         , responseGetSuppressedDestination $
--             newGetSuppressedDestinationResponse
--
--         , responseGetEmailTemplate $
--             newGetEmailTemplateResponse
--
--         , responsePutConfigurationSetSuppressionOptions $
--             newPutConfigurationSetSuppressionOptionsResponse
--
--         , responseDeleteCustomVerificationEmailTemplate $
--             newDeleteCustomVerificationEmailTemplateResponse
--
--         , responseGetDomainDeliverabilityCampaign $
--             newGetDomainDeliverabilityCampaignResponse
--
--         , responseUpdateCustomVerificationEmailTemplate $
--             newUpdateCustomVerificationEmailTemplateResponse
--
--         , responseGetConfigurationSetEventDestinations $
--             newGetConfigurationSetEventDestinationsResponse
--
--         , responseListCustomVerificationEmailTemplates $
--             newListCustomVerificationEmailTemplatesResponse
--
--         , responseGetDedicatedIps $
--             newGetDedicatedIpsResponse
--
--         , responseGetAccount $
--             newGetAccountResponse
--
--         , responsePutConfigurationSetSendingOptions $
--             newPutConfigurationSetSendingOptionsResponse
--
--         , responseGetConfigurationSet $
--             newGetConfigurationSetResponse
--
--         , responseGetDedicatedIp $
--             newGetDedicatedIpResponse
--
--         , responseGetEmailIdentity $
--             newGetEmailIdentityResponse
--
--         , responsePutEmailIdentityDkimSigningAttributes $
--             newPutEmailIdentityDkimSigningAttributesResponse
--
--         , responseGetContactList $
--             newGetContactListResponse
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
--         , responseCreateImportJob $
--             newCreateImportJobResponse
--
--         , responseGetContact $
--             newGetContactResponse
--
--         , responsePutAccountSuppressionAttributes $
--             newPutAccountSuppressionAttributesResponse
--
--         , responseCreateEmailIdentityPolicy $
--             newCreateEmailIdentityPolicyResponse
--
--         , responsePutAccountSendingAttributes $
--             newPutAccountSendingAttributesResponse
--
--         , responseUpdateContactList $
--             newUpdateContactListResponse
--
--         , responseListConfigurationSets $
--             newListConfigurationSetsResponse
--
--         , responseListContactLists $
--             newListContactListsResponse
--
--         , responseDeleteContactList $
--             newDeleteContactListResponse
--
--         , responseCreateDeliverabilityTestReport $
--             newCreateDeliverabilityTestReportResponse
--
--         , responseDeleteEmailIdentity $
--             newDeleteEmailIdentityResponse
--
--         , responsePutEmailIdentityMailFromAttributes $
--             newPutEmailIdentityMailFromAttributesResponse
--
--         , responseGetCustomVerificationEmailTemplate $
--             newGetCustomVerificationEmailTemplateResponse
--
--         , responsePutDedicatedIpWarmupAttributes $
--             newPutDedicatedIpWarmupAttributesResponse
--
--         , responseCreateEmailTemplate $
--             newCreateEmailTemplateResponse
--
--         , responseGetDomainStatisticsReport $
--             newGetDomainStatisticsReportResponse
--
--         , responsePutEmailIdentityFeedbackAttributes $
--             newPutEmailIdentityFeedbackAttributesResponse
--
--         , responseGetDeliverabilityDashboardOptions $
--             newGetDeliverabilityDashboardOptionsResponse
--
--         , responseListEmailTemplates $
--             newListEmailTemplatesResponse
--
--         , responseDeleteEmailTemplate $
--             newDeleteEmailTemplateResponse
--
--         , responseListSuppressedDestinations $
--             newListSuppressedDestinationsResponse
--
--         , responseUpdateEmailTemplate $
--             newUpdateEmailTemplateResponse
--
--         , responseDeleteSuppressedDestination $
--             newDeleteSuppressedDestinationResponse
--
--         , responseGetEmailIdentityPolicies $
--             newGetEmailIdentityPoliciesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseSendCustomVerificationEmail $
--             newSendCustomVerificationEmailResponse
--
--         , responseCreateDedicatedIpPool $
--             newCreateDedicatedIpPoolResponse
--
--           ]
--     ]

-- Requests

requestPutEmailIdentityDkimAttributes :: PutEmailIdentityDkimAttributes -> TestTree
requestPutEmailIdentityDkimAttributes =
  req
    "PutEmailIdentityDkimAttributes"
    "fixture/PutEmailIdentityDkimAttributes.yaml"

requestPutConfigurationSetDeliveryOptions :: PutConfigurationSetDeliveryOptions -> TestTree
requestPutConfigurationSetDeliveryOptions =
  req
    "PutConfigurationSetDeliveryOptions"
    "fixture/PutConfigurationSetDeliveryOptions.yaml"

requestListDedicatedIpPools :: ListDedicatedIpPools -> TestTree
requestListDedicatedIpPools =
  req
    "ListDedicatedIpPools"
    "fixture/ListDedicatedIpPools.yaml"

requestDeleteDedicatedIpPool :: DeleteDedicatedIpPool -> TestTree
requestDeleteDedicatedIpPool =
  req
    "DeleteDedicatedIpPool"
    "fixture/DeleteDedicatedIpPool.yaml"

requestGetImportJob :: GetImportJob -> TestTree
requestGetImportJob =
  req
    "GetImportJob"
    "fixture/GetImportJob.yaml"

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

requestGetDeliverabilityTestReport :: GetDeliverabilityTestReport -> TestTree
requestGetDeliverabilityTestReport =
  req
    "GetDeliverabilityTestReport"
    "fixture/GetDeliverabilityTestReport.yaml"

requestPutAccountDedicatedIpWarmupAttributes :: PutAccountDedicatedIpWarmupAttributes -> TestTree
requestPutAccountDedicatedIpWarmupAttributes =
  req
    "PutAccountDedicatedIpWarmupAttributes"
    "fixture/PutAccountDedicatedIpWarmupAttributes.yaml"

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

requestCreateContactList :: CreateContactList -> TestTree
requestCreateContactList =
  req
    "CreateContactList"
    "fixture/CreateContactList.yaml"

requestCreateConfigurationSet :: CreateConfigurationSet -> TestTree
requestCreateConfigurationSet =
  req
    "CreateConfigurationSet"
    "fixture/CreateConfigurationSet.yaml"

requestPutEmailIdentityConfigurationSetAttributes :: PutEmailIdentityConfigurationSetAttributes -> TestTree
requestPutEmailIdentityConfigurationSetAttributes =
  req
    "PutEmailIdentityConfigurationSetAttributes"
    "fixture/PutEmailIdentityConfigurationSetAttributes.yaml"

requestDeleteConfigurationSet :: DeleteConfigurationSet -> TestTree
requestDeleteConfigurationSet =
  req
    "DeleteConfigurationSet"
    "fixture/DeleteConfigurationSet.yaml"

requestDeleteEmailIdentityPolicy :: DeleteEmailIdentityPolicy -> TestTree
requestDeleteEmailIdentityPolicy =
  req
    "DeleteEmailIdentityPolicy"
    "fixture/DeleteEmailIdentityPolicy.yaml"

requestUpdateEmailIdentityPolicy :: UpdateEmailIdentityPolicy -> TestTree
requestUpdateEmailIdentityPolicy =
  req
    "UpdateEmailIdentityPolicy"
    "fixture/UpdateEmailIdentityPolicy.yaml"

requestUpdateConfigurationSetEventDestination :: UpdateConfigurationSetEventDestination -> TestTree
requestUpdateConfigurationSetEventDestination =
  req
    "UpdateConfigurationSetEventDestination"
    "fixture/UpdateConfigurationSetEventDestination.yaml"

requestListImportJobs :: ListImportJobs -> TestTree
requestListImportJobs =
  req
    "ListImportJobs"
    "fixture/ListImportJobs.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteConfigurationSetEventDestination :: DeleteConfigurationSetEventDestination -> TestTree
requestDeleteConfigurationSetEventDestination =
  req
    "DeleteConfigurationSetEventDestination"
    "fixture/DeleteConfigurationSetEventDestination.yaml"

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

requestSendBulkEmail :: SendBulkEmail -> TestTree
requestSendBulkEmail =
  req
    "SendBulkEmail"
    "fixture/SendBulkEmail.yaml"

requestTestRenderEmailTemplate :: TestRenderEmailTemplate -> TestTree
requestTestRenderEmailTemplate =
  req
    "TestRenderEmailTemplate"
    "fixture/TestRenderEmailTemplate.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestPutDedicatedIpInPool :: PutDedicatedIpInPool -> TestTree
requestPutDedicatedIpInPool =
  req
    "PutDedicatedIpInPool"
    "fixture/PutDedicatedIpInPool.yaml"

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

requestPutConfigurationSetSuppressionOptions :: PutConfigurationSetSuppressionOptions -> TestTree
requestPutConfigurationSetSuppressionOptions =
  req
    "PutConfigurationSetSuppressionOptions"
    "fixture/PutConfigurationSetSuppressionOptions.yaml"

requestDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplate -> TestTree
requestDeleteCustomVerificationEmailTemplate =
  req
    "DeleteCustomVerificationEmailTemplate"
    "fixture/DeleteCustomVerificationEmailTemplate.yaml"

requestGetDomainDeliverabilityCampaign :: GetDomainDeliverabilityCampaign -> TestTree
requestGetDomainDeliverabilityCampaign =
  req
    "GetDomainDeliverabilityCampaign"
    "fixture/GetDomainDeliverabilityCampaign.yaml"

requestUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplate -> TestTree
requestUpdateCustomVerificationEmailTemplate =
  req
    "UpdateCustomVerificationEmailTemplate"
    "fixture/UpdateCustomVerificationEmailTemplate.yaml"

requestGetConfigurationSetEventDestinations :: GetConfigurationSetEventDestinations -> TestTree
requestGetConfigurationSetEventDestinations =
  req
    "GetConfigurationSetEventDestinations"
    "fixture/GetConfigurationSetEventDestinations.yaml"

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

requestGetAccount :: GetAccount -> TestTree
requestGetAccount =
  req
    "GetAccount"
    "fixture/GetAccount.yaml"

requestPutConfigurationSetSendingOptions :: PutConfigurationSetSendingOptions -> TestTree
requestPutConfigurationSetSendingOptions =
  req
    "PutConfigurationSetSendingOptions"
    "fixture/PutConfigurationSetSendingOptions.yaml"

requestGetConfigurationSet :: GetConfigurationSet -> TestTree
requestGetConfigurationSet =
  req
    "GetConfigurationSet"
    "fixture/GetConfigurationSet.yaml"

requestGetDedicatedIp :: GetDedicatedIp -> TestTree
requestGetDedicatedIp =
  req
    "GetDedicatedIp"
    "fixture/GetDedicatedIp.yaml"

requestGetEmailIdentity :: GetEmailIdentity -> TestTree
requestGetEmailIdentity =
  req
    "GetEmailIdentity"
    "fixture/GetEmailIdentity.yaml"

requestPutEmailIdentityDkimSigningAttributes :: PutEmailIdentityDkimSigningAttributes -> TestTree
requestPutEmailIdentityDkimSigningAttributes =
  req
    "PutEmailIdentityDkimSigningAttributes"
    "fixture/PutEmailIdentityDkimSigningAttributes.yaml"

requestGetContactList :: GetContactList -> TestTree
requestGetContactList =
  req
    "GetContactList"
    "fixture/GetContactList.yaml"

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

requestPutAccountSuppressionAttributes :: PutAccountSuppressionAttributes -> TestTree
requestPutAccountSuppressionAttributes =
  req
    "PutAccountSuppressionAttributes"
    "fixture/PutAccountSuppressionAttributes.yaml"

requestCreateEmailIdentityPolicy :: CreateEmailIdentityPolicy -> TestTree
requestCreateEmailIdentityPolicy =
  req
    "CreateEmailIdentityPolicy"
    "fixture/CreateEmailIdentityPolicy.yaml"

requestPutAccountSendingAttributes :: PutAccountSendingAttributes -> TestTree
requestPutAccountSendingAttributes =
  req
    "PutAccountSendingAttributes"
    "fixture/PutAccountSendingAttributes.yaml"

requestUpdateContactList :: UpdateContactList -> TestTree
requestUpdateContactList =
  req
    "UpdateContactList"
    "fixture/UpdateContactList.yaml"

requestListConfigurationSets :: ListConfigurationSets -> TestTree
requestListConfigurationSets =
  req
    "ListConfigurationSets"
    "fixture/ListConfigurationSets.yaml"

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

requestPutEmailIdentityMailFromAttributes :: PutEmailIdentityMailFromAttributes -> TestTree
requestPutEmailIdentityMailFromAttributes =
  req
    "PutEmailIdentityMailFromAttributes"
    "fixture/PutEmailIdentityMailFromAttributes.yaml"

requestGetCustomVerificationEmailTemplate :: GetCustomVerificationEmailTemplate -> TestTree
requestGetCustomVerificationEmailTemplate =
  req
    "GetCustomVerificationEmailTemplate"
    "fixture/GetCustomVerificationEmailTemplate.yaml"

requestPutDedicatedIpWarmupAttributes :: PutDedicatedIpWarmupAttributes -> TestTree
requestPutDedicatedIpWarmupAttributes =
  req
    "PutDedicatedIpWarmupAttributes"
    "fixture/PutDedicatedIpWarmupAttributes.yaml"

requestCreateEmailTemplate :: CreateEmailTemplate -> TestTree
requestCreateEmailTemplate =
  req
    "CreateEmailTemplate"
    "fixture/CreateEmailTemplate.yaml"

requestGetDomainStatisticsReport :: GetDomainStatisticsReport -> TestTree
requestGetDomainStatisticsReport =
  req
    "GetDomainStatisticsReport"
    "fixture/GetDomainStatisticsReport.yaml"

requestPutEmailIdentityFeedbackAttributes :: PutEmailIdentityFeedbackAttributes -> TestTree
requestPutEmailIdentityFeedbackAttributes =
  req
    "PutEmailIdentityFeedbackAttributes"
    "fixture/PutEmailIdentityFeedbackAttributes.yaml"

requestGetDeliverabilityDashboardOptions :: GetDeliverabilityDashboardOptions -> TestTree
requestGetDeliverabilityDashboardOptions =
  req
    "GetDeliverabilityDashboardOptions"
    "fixture/GetDeliverabilityDashboardOptions.yaml"

requestListEmailTemplates :: ListEmailTemplates -> TestTree
requestListEmailTemplates =
  req
    "ListEmailTemplates"
    "fixture/ListEmailTemplates.yaml"

requestDeleteEmailTemplate :: DeleteEmailTemplate -> TestTree
requestDeleteEmailTemplate =
  req
    "DeleteEmailTemplate"
    "fixture/DeleteEmailTemplate.yaml"

requestListSuppressedDestinations :: ListSuppressedDestinations -> TestTree
requestListSuppressedDestinations =
  req
    "ListSuppressedDestinations"
    "fixture/ListSuppressedDestinations.yaml"

requestUpdateEmailTemplate :: UpdateEmailTemplate -> TestTree
requestUpdateEmailTemplate =
  req
    "UpdateEmailTemplate"
    "fixture/UpdateEmailTemplate.yaml"

requestDeleteSuppressedDestination :: DeleteSuppressedDestination -> TestTree
requestDeleteSuppressedDestination =
  req
    "DeleteSuppressedDestination"
    "fixture/DeleteSuppressedDestination.yaml"

requestGetEmailIdentityPolicies :: GetEmailIdentityPolicies -> TestTree
requestGetEmailIdentityPolicies =
  req
    "GetEmailIdentityPolicies"
    "fixture/GetEmailIdentityPolicies.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestSendCustomVerificationEmail :: SendCustomVerificationEmail -> TestTree
requestSendCustomVerificationEmail =
  req
    "SendCustomVerificationEmail"
    "fixture/SendCustomVerificationEmail.yaml"

requestCreateDedicatedIpPool :: CreateDedicatedIpPool -> TestTree
requestCreateDedicatedIpPool =
  req
    "CreateDedicatedIpPool"
    "fixture/CreateDedicatedIpPool.yaml"

-- Responses

responsePutEmailIdentityDkimAttributes :: PutEmailIdentityDkimAttributesResponse -> TestTree
responsePutEmailIdentityDkimAttributes =
  res
    "PutEmailIdentityDkimAttributesResponse"
    "fixture/PutEmailIdentityDkimAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutEmailIdentityDkimAttributes)

responsePutConfigurationSetDeliveryOptions :: PutConfigurationSetDeliveryOptionsResponse -> TestTree
responsePutConfigurationSetDeliveryOptions =
  res
    "PutConfigurationSetDeliveryOptionsResponse"
    "fixture/PutConfigurationSetDeliveryOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutConfigurationSetDeliveryOptions)

responseListDedicatedIpPools :: ListDedicatedIpPoolsResponse -> TestTree
responseListDedicatedIpPools =
  res
    "ListDedicatedIpPoolsResponse"
    "fixture/ListDedicatedIpPoolsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDedicatedIpPools)

responseDeleteDedicatedIpPool :: DeleteDedicatedIpPoolResponse -> TestTree
responseDeleteDedicatedIpPool =
  res
    "DeleteDedicatedIpPoolResponse"
    "fixture/DeleteDedicatedIpPoolResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDedicatedIpPool)

responseGetImportJob :: GetImportJobResponse -> TestTree
responseGetImportJob =
  res
    "GetImportJobResponse"
    "fixture/GetImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy GetImportJob)

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

responseGetDeliverabilityTestReport :: GetDeliverabilityTestReportResponse -> TestTree
responseGetDeliverabilityTestReport =
  res
    "GetDeliverabilityTestReportResponse"
    "fixture/GetDeliverabilityTestReportResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeliverabilityTestReport)

responsePutAccountDedicatedIpWarmupAttributes :: PutAccountDedicatedIpWarmupAttributesResponse -> TestTree
responsePutAccountDedicatedIpWarmupAttributes =
  res
    "PutAccountDedicatedIpWarmupAttributesResponse"
    "fixture/PutAccountDedicatedIpWarmupAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutAccountDedicatedIpWarmupAttributes)

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

responseCreateContactList :: CreateContactListResponse -> TestTree
responseCreateContactList =
  res
    "CreateContactListResponse"
    "fixture/CreateContactListResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContactList)

responseCreateConfigurationSet :: CreateConfigurationSetResponse -> TestTree
responseCreateConfigurationSet =
  res
    "CreateConfigurationSetResponse"
    "fixture/CreateConfigurationSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConfigurationSet)

responsePutEmailIdentityConfigurationSetAttributes :: PutEmailIdentityConfigurationSetAttributesResponse -> TestTree
responsePutEmailIdentityConfigurationSetAttributes =
  res
    "PutEmailIdentityConfigurationSetAttributesResponse"
    "fixture/PutEmailIdentityConfigurationSetAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutEmailIdentityConfigurationSetAttributes)

responseDeleteConfigurationSet :: DeleteConfigurationSetResponse -> TestTree
responseDeleteConfigurationSet =
  res
    "DeleteConfigurationSetResponse"
    "fixture/DeleteConfigurationSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfigurationSet)

responseDeleteEmailIdentityPolicy :: DeleteEmailIdentityPolicyResponse -> TestTree
responseDeleteEmailIdentityPolicy =
  res
    "DeleteEmailIdentityPolicyResponse"
    "fixture/DeleteEmailIdentityPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEmailIdentityPolicy)

responseUpdateEmailIdentityPolicy :: UpdateEmailIdentityPolicyResponse -> TestTree
responseUpdateEmailIdentityPolicy =
  res
    "UpdateEmailIdentityPolicyResponse"
    "fixture/UpdateEmailIdentityPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEmailIdentityPolicy)

responseUpdateConfigurationSetEventDestination :: UpdateConfigurationSetEventDestinationResponse -> TestTree
responseUpdateConfigurationSetEventDestination =
  res
    "UpdateConfigurationSetEventDestinationResponse"
    "fixture/UpdateConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConfigurationSetEventDestination)

responseListImportJobs :: ListImportJobsResponse -> TestTree
responseListImportJobs =
  res
    "ListImportJobsResponse"
    "fixture/ListImportJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListImportJobs)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDeleteConfigurationSetEventDestination :: DeleteConfigurationSetEventDestinationResponse -> TestTree
responseDeleteConfigurationSetEventDestination =
  res
    "DeleteConfigurationSetEventDestinationResponse"
    "fixture/DeleteConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfigurationSetEventDestination)

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

responseSendBulkEmail :: SendBulkEmailResponse -> TestTree
responseSendBulkEmail =
  res
    "SendBulkEmailResponse"
    "fixture/SendBulkEmailResponse.proto"
    defaultService
    (Proxy :: Proxy SendBulkEmail)

responseTestRenderEmailTemplate :: TestRenderEmailTemplateResponse -> TestTree
responseTestRenderEmailTemplate =
  res
    "TestRenderEmailTemplateResponse"
    "fixture/TestRenderEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy TestRenderEmailTemplate)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responsePutDedicatedIpInPool :: PutDedicatedIpInPoolResponse -> TestTree
responsePutDedicatedIpInPool =
  res
    "PutDedicatedIpInPoolResponse"
    "fixture/PutDedicatedIpInPoolResponse.proto"
    defaultService
    (Proxy :: Proxy PutDedicatedIpInPool)

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

responsePutConfigurationSetSuppressionOptions :: PutConfigurationSetSuppressionOptionsResponse -> TestTree
responsePutConfigurationSetSuppressionOptions =
  res
    "PutConfigurationSetSuppressionOptionsResponse"
    "fixture/PutConfigurationSetSuppressionOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutConfigurationSetSuppressionOptions)

responseDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplateResponse -> TestTree
responseDeleteCustomVerificationEmailTemplate =
  res
    "DeleteCustomVerificationEmailTemplateResponse"
    "fixture/DeleteCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCustomVerificationEmailTemplate)

responseGetDomainDeliverabilityCampaign :: GetDomainDeliverabilityCampaignResponse -> TestTree
responseGetDomainDeliverabilityCampaign =
  res
    "GetDomainDeliverabilityCampaignResponse"
    "fixture/GetDomainDeliverabilityCampaignResponse.proto"
    defaultService
    (Proxy :: Proxy GetDomainDeliverabilityCampaign)

responseUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplateResponse -> TestTree
responseUpdateCustomVerificationEmailTemplate =
  res
    "UpdateCustomVerificationEmailTemplateResponse"
    "fixture/UpdateCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCustomVerificationEmailTemplate)

responseGetConfigurationSetEventDestinations :: GetConfigurationSetEventDestinationsResponse -> TestTree
responseGetConfigurationSetEventDestinations =
  res
    "GetConfigurationSetEventDestinationsResponse"
    "fixture/GetConfigurationSetEventDestinationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetConfigurationSetEventDestinations)

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

responseGetAccount :: GetAccountResponse -> TestTree
responseGetAccount =
  res
    "GetAccountResponse"
    "fixture/GetAccountResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccount)

responsePutConfigurationSetSendingOptions :: PutConfigurationSetSendingOptionsResponse -> TestTree
responsePutConfigurationSetSendingOptions =
  res
    "PutConfigurationSetSendingOptionsResponse"
    "fixture/PutConfigurationSetSendingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutConfigurationSetSendingOptions)

responseGetConfigurationSet :: GetConfigurationSetResponse -> TestTree
responseGetConfigurationSet =
  res
    "GetConfigurationSetResponse"
    "fixture/GetConfigurationSetResponse.proto"
    defaultService
    (Proxy :: Proxy GetConfigurationSet)

responseGetDedicatedIp :: GetDedicatedIpResponse -> TestTree
responseGetDedicatedIp =
  res
    "GetDedicatedIpResponse"
    "fixture/GetDedicatedIpResponse.proto"
    defaultService
    (Proxy :: Proxy GetDedicatedIp)

responseGetEmailIdentity :: GetEmailIdentityResponse -> TestTree
responseGetEmailIdentity =
  res
    "GetEmailIdentityResponse"
    "fixture/GetEmailIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy GetEmailIdentity)

responsePutEmailIdentityDkimSigningAttributes :: PutEmailIdentityDkimSigningAttributesResponse -> TestTree
responsePutEmailIdentityDkimSigningAttributes =
  res
    "PutEmailIdentityDkimSigningAttributesResponse"
    "fixture/PutEmailIdentityDkimSigningAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutEmailIdentityDkimSigningAttributes)

responseGetContactList :: GetContactListResponse -> TestTree
responseGetContactList =
  res
    "GetContactListResponse"
    "fixture/GetContactListResponse.proto"
    defaultService
    (Proxy :: Proxy GetContactList)

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

responsePutAccountSuppressionAttributes :: PutAccountSuppressionAttributesResponse -> TestTree
responsePutAccountSuppressionAttributes =
  res
    "PutAccountSuppressionAttributesResponse"
    "fixture/PutAccountSuppressionAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutAccountSuppressionAttributes)

responseCreateEmailIdentityPolicy :: CreateEmailIdentityPolicyResponse -> TestTree
responseCreateEmailIdentityPolicy =
  res
    "CreateEmailIdentityPolicyResponse"
    "fixture/CreateEmailIdentityPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEmailIdentityPolicy)

responsePutAccountSendingAttributes :: PutAccountSendingAttributesResponse -> TestTree
responsePutAccountSendingAttributes =
  res
    "PutAccountSendingAttributesResponse"
    "fixture/PutAccountSendingAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutAccountSendingAttributes)

responseUpdateContactList :: UpdateContactListResponse -> TestTree
responseUpdateContactList =
  res
    "UpdateContactListResponse"
    "fixture/UpdateContactListResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContactList)

responseListConfigurationSets :: ListConfigurationSetsResponse -> TestTree
responseListConfigurationSets =
  res
    "ListConfigurationSetsResponse"
    "fixture/ListConfigurationSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListConfigurationSets)

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

responsePutEmailIdentityMailFromAttributes :: PutEmailIdentityMailFromAttributesResponse -> TestTree
responsePutEmailIdentityMailFromAttributes =
  res
    "PutEmailIdentityMailFromAttributesResponse"
    "fixture/PutEmailIdentityMailFromAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutEmailIdentityMailFromAttributes)

responseGetCustomVerificationEmailTemplate :: GetCustomVerificationEmailTemplateResponse -> TestTree
responseGetCustomVerificationEmailTemplate =
  res
    "GetCustomVerificationEmailTemplateResponse"
    "fixture/GetCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetCustomVerificationEmailTemplate)

responsePutDedicatedIpWarmupAttributes :: PutDedicatedIpWarmupAttributesResponse -> TestTree
responsePutDedicatedIpWarmupAttributes =
  res
    "PutDedicatedIpWarmupAttributesResponse"
    "fixture/PutDedicatedIpWarmupAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutDedicatedIpWarmupAttributes)

responseCreateEmailTemplate :: CreateEmailTemplateResponse -> TestTree
responseCreateEmailTemplate =
  res
    "CreateEmailTemplateResponse"
    "fixture/CreateEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEmailTemplate)

responseGetDomainStatisticsReport :: GetDomainStatisticsReportResponse -> TestTree
responseGetDomainStatisticsReport =
  res
    "GetDomainStatisticsReportResponse"
    "fixture/GetDomainStatisticsReportResponse.proto"
    defaultService
    (Proxy :: Proxy GetDomainStatisticsReport)

responsePutEmailIdentityFeedbackAttributes :: PutEmailIdentityFeedbackAttributesResponse -> TestTree
responsePutEmailIdentityFeedbackAttributes =
  res
    "PutEmailIdentityFeedbackAttributesResponse"
    "fixture/PutEmailIdentityFeedbackAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutEmailIdentityFeedbackAttributes)

responseGetDeliverabilityDashboardOptions :: GetDeliverabilityDashboardOptionsResponse -> TestTree
responseGetDeliverabilityDashboardOptions =
  res
    "GetDeliverabilityDashboardOptionsResponse"
    "fixture/GetDeliverabilityDashboardOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeliverabilityDashboardOptions)

responseListEmailTemplates :: ListEmailTemplatesResponse -> TestTree
responseListEmailTemplates =
  res
    "ListEmailTemplatesResponse"
    "fixture/ListEmailTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListEmailTemplates)

responseDeleteEmailTemplate :: DeleteEmailTemplateResponse -> TestTree
responseDeleteEmailTemplate =
  res
    "DeleteEmailTemplateResponse"
    "fixture/DeleteEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEmailTemplate)

responseListSuppressedDestinations :: ListSuppressedDestinationsResponse -> TestTree
responseListSuppressedDestinations =
  res
    "ListSuppressedDestinationsResponse"
    "fixture/ListSuppressedDestinationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSuppressedDestinations)

responseUpdateEmailTemplate :: UpdateEmailTemplateResponse -> TestTree
responseUpdateEmailTemplate =
  res
    "UpdateEmailTemplateResponse"
    "fixture/UpdateEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEmailTemplate)

responseDeleteSuppressedDestination :: DeleteSuppressedDestinationResponse -> TestTree
responseDeleteSuppressedDestination =
  res
    "DeleteSuppressedDestinationResponse"
    "fixture/DeleteSuppressedDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSuppressedDestination)

responseGetEmailIdentityPolicies :: GetEmailIdentityPoliciesResponse -> TestTree
responseGetEmailIdentityPolicies =
  res
    "GetEmailIdentityPoliciesResponse"
    "fixture/GetEmailIdentityPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy GetEmailIdentityPolicies)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseSendCustomVerificationEmail :: SendCustomVerificationEmailResponse -> TestTree
responseSendCustomVerificationEmail =
  res
    "SendCustomVerificationEmailResponse"
    "fixture/SendCustomVerificationEmailResponse.proto"
    defaultService
    (Proxy :: Proxy SendCustomVerificationEmail)

responseCreateDedicatedIpPool :: CreateDedicatedIpPoolResponse -> TestTree
responseCreateDedicatedIpPool =
  res
    "CreateDedicatedIpPoolResponse"
    "fixture/CreateDedicatedIpPoolResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDedicatedIpPool)
