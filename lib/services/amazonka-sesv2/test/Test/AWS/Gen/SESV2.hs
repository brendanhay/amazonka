{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SESV2
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.SESV2 where

import Data.Proxy
import Network.AWS.SESV2
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SESV2.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetConfigurationSet $
--             newGetConfigurationSet
--
--         , requestGetImportJob $
--             newGetImportJob
--
--         , requestPutConfigurationSetTrackingOptions $
--             newPutConfigurationSetTrackingOptions
--
--         , requestPutEmailIdentityDkimSigningAttributes $
--             newPutEmailIdentityDkimSigningAttributes
--
--         , requestPutEmailIdentityDkimAttributes $
--             newPutEmailIdentityDkimAttributes
--
--         , requestPutConfigurationSetDeliveryOptions $
--             newPutConfigurationSetDeliveryOptions
--
--         , requestListDedicatedIpPools $
--             newListDedicatedIpPools
--
--         , requestGetDomainDeliverabilityCampaign $
--             newGetDomainDeliverabilityCampaign
--
--         , requestGetDedicatedIps $
--             newGetDedicatedIps
--
--         , requestPutConfigurationSetSendingOptions $
--             newPutConfigurationSetSendingOptions
--
--         , requestDeleteCustomVerificationEmailTemplate $
--             newDeleteCustomVerificationEmailTemplate
--
--         , requestUpdateCustomVerificationEmailTemplate $
--             newUpdateCustomVerificationEmailTemplate
--
--         , requestCreateDedicatedIpPool $
--             newCreateDedicatedIpPool
--
--         , requestSendCustomVerificationEmail $
--             newSendCustomVerificationEmail
--
--         , requestGetSuppressedDestination $
--             newGetSuppressedDestination
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetEmailTemplate $
--             newGetEmailTemplate
--
--         , requestListSuppressedDestinations $
--             newListSuppressedDestinations
--
--         , requestPutEmailIdentityFeedbackAttributes $
--             newPutEmailIdentityFeedbackAttributes
--
--         , requestListEmailTemplates $
--             newListEmailTemplates
--
--         , requestPutConfigurationSetReputationOptions $
--             newPutConfigurationSetReputationOptions
--
--         , requestPutDedicatedIpInPool $
--             newPutDedicatedIpInPool
--
--         , requestCreateEmailTemplate $
--             newCreateEmailTemplate
--
--         , requestPutAccountSendingAttributes $
--             newPutAccountSendingAttributes
--
--         , requestUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestination
--
--         , requestDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestination
--
--         , requestListConfigurationSets $
--             newListConfigurationSets
--
--         , requestDeleteEmailIdentity $
--             newDeleteEmailIdentity
--
--         , requestDeleteContactList $
--             newDeleteContactList
--
--         , requestUpdateContactList $
--             newUpdateContactList
--
--         , requestListImportJobs $
--             newListImportJobs
--
--         , requestDeleteConfigurationSet $
--             newDeleteConfigurationSet
--
--         , requestCreateEmailIdentity $
--             newCreateEmailIdentity
--
--         , requestGetBlacklistReports $
--             newGetBlacklistReports
--
--         , requestCreateContactList $
--             newCreateContactList
--
--         , requestListEmailIdentities $
--             newListEmailIdentities
--
--         , requestGetContact $
--             newGetContact
--
--         , requestDeleteContact $
--             newDeleteContact
--
--         , requestUpdateContact $
--             newUpdateContact
--
--         , requestGetContactList $
--             newGetContactList
--
--         , requestGetDedicatedIp $
--             newGetDedicatedIp
--
--         , requestCreateContact $
--             newCreateContact
--
--         , requestGetEmailIdentity $
--             newGetEmailIdentity
--
--         , requestGetConfigurationSetEventDestinations $
--             newGetConfigurationSetEventDestinations
--
--         , requestListCustomVerificationEmailTemplates $
--             newListCustomVerificationEmailTemplates
--
--         , requestGetAccount $
--             newGetAccount
--
--         , requestDeleteDedicatedIpPool $
--             newDeleteDedicatedIpPool
--
--         , requestGetEmailIdentityPolicies $
--             newGetEmailIdentityPolicies
--
--         , requestPutConfigurationSetSuppressionOptions $
--             newPutConfigurationSetSuppressionOptions
--
--         , requestCreateCustomVerificationEmailTemplate $
--             newCreateCustomVerificationEmailTemplate
--
--         , requestPutAccountDetails $
--             newPutAccountDetails
--
--         , requestDeleteSuppressedDestination $
--             newDeleteSuppressedDestination
--
--         , requestPutSuppressedDestination $
--             newPutSuppressedDestination
--
--         , requestGetDomainStatisticsReport $
--             newGetDomainStatisticsReport
--
--         , requestDeleteEmailTemplate $
--             newDeleteEmailTemplate
--
--         , requestUpdateEmailTemplate $
--             newUpdateEmailTemplate
--
--         , requestGetDeliverabilityDashboardOptions $
--             newGetDeliverabilityDashboardOptions
--
--         , requestGetCustomVerificationEmailTemplate $
--             newGetCustomVerificationEmailTemplate
--
--         , requestListDomainDeliverabilityCampaigns $
--             newListDomainDeliverabilityCampaigns
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
--         , requestSendEmail $
--             newSendEmail
--
--         , requestPutDedicatedIpWarmupAttributes $
--             newPutDedicatedIpWarmupAttributes
--
--         , requestDeleteEmailIdentityPolicy $
--             newDeleteEmailIdentityPolicy
--
--         , requestUpdateEmailIdentityPolicy $
--             newUpdateEmailIdentityPolicy
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateDeliverabilityTestReport $
--             newCreateDeliverabilityTestReport
--
--         , requestPutEmailIdentityMailFromAttributes $
--             newPutEmailIdentityMailFromAttributes
--
--         , requestListContactLists $
--             newListContactLists
--
--         , requestCreateEmailIdentityPolicy $
--             newCreateEmailIdentityPolicy
--
--         , requestCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestination
--
--         , requestPutEmailIdentityConfigurationSetAttributes $
--             newPutEmailIdentityConfigurationSetAttributes
--
--         , requestPutAccountSuppressionAttributes $
--             newPutAccountSuppressionAttributes
--
--         , requestCreateImportJob $
--             newCreateImportJob
--
--         , requestListDeliverabilityTestReports $
--             newListDeliverabilityTestReports
--
--         , requestCreateConfigurationSet $
--             newCreateConfigurationSet
--
--         , requestGetDeliverabilityTestReport $
--             newGetDeliverabilityTestReport
--
--         , requestPutDeliverabilityDashboardOption $
--             newPutDeliverabilityDashboardOption
--
--         , requestPutAccountDedicatedIpWarmupAttributes $
--             newPutAccountDedicatedIpWarmupAttributes
--
--         , requestListContacts $
--             newListContacts
--
--           ]

--     , testGroup "response"
--         [ responseGetConfigurationSet $
--             newGetConfigurationSetResponse
--
--         , responseGetImportJob $
--             newGetImportJobResponse
--
--         , responsePutConfigurationSetTrackingOptions $
--             newPutConfigurationSetTrackingOptionsResponse
--
--         , responsePutEmailIdentityDkimSigningAttributes $
--             newPutEmailIdentityDkimSigningAttributesResponse
--
--         , responsePutEmailIdentityDkimAttributes $
--             newPutEmailIdentityDkimAttributesResponse
--
--         , responsePutConfigurationSetDeliveryOptions $
--             newPutConfigurationSetDeliveryOptionsResponse
--
--         , responseListDedicatedIpPools $
--             newListDedicatedIpPoolsResponse
--
--         , responseGetDomainDeliverabilityCampaign $
--             newGetDomainDeliverabilityCampaignResponse
--
--         , responseGetDedicatedIps $
--             newGetDedicatedIpsResponse
--
--         , responsePutConfigurationSetSendingOptions $
--             newPutConfigurationSetSendingOptionsResponse
--
--         , responseDeleteCustomVerificationEmailTemplate $
--             newDeleteCustomVerificationEmailTemplateResponse
--
--         , responseUpdateCustomVerificationEmailTemplate $
--             newUpdateCustomVerificationEmailTemplateResponse
--
--         , responseCreateDedicatedIpPool $
--             newCreateDedicatedIpPoolResponse
--
--         , responseSendCustomVerificationEmail $
--             newSendCustomVerificationEmailResponse
--
--         , responseGetSuppressedDestination $
--             newGetSuppressedDestinationResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetEmailTemplate $
--             newGetEmailTemplateResponse
--
--         , responseListSuppressedDestinations $
--             newListSuppressedDestinationsResponse
--
--         , responsePutEmailIdentityFeedbackAttributes $
--             newPutEmailIdentityFeedbackAttributesResponse
--
--         , responseListEmailTemplates $
--             newListEmailTemplatesResponse
--
--         , responsePutConfigurationSetReputationOptions $
--             newPutConfigurationSetReputationOptionsResponse
--
--         , responsePutDedicatedIpInPool $
--             newPutDedicatedIpInPoolResponse
--
--         , responseCreateEmailTemplate $
--             newCreateEmailTemplateResponse
--
--         , responsePutAccountSendingAttributes $
--             newPutAccountSendingAttributesResponse
--
--         , responseUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestinationResponse
--
--         , responseDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestinationResponse
--
--         , responseListConfigurationSets $
--             newListConfigurationSetsResponse
--
--         , responseDeleteEmailIdentity $
--             newDeleteEmailIdentityResponse
--
--         , responseDeleteContactList $
--             newDeleteContactListResponse
--
--         , responseUpdateContactList $
--             newUpdateContactListResponse
--
--         , responseListImportJobs $
--             newListImportJobsResponse
--
--         , responseDeleteConfigurationSet $
--             newDeleteConfigurationSetResponse
--
--         , responseCreateEmailIdentity $
--             newCreateEmailIdentityResponse
--
--         , responseGetBlacklistReports $
--             newGetBlacklistReportsResponse
--
--         , responseCreateContactList $
--             newCreateContactListResponse
--
--         , responseListEmailIdentities $
--             newListEmailIdentitiesResponse
--
--         , responseGetContact $
--             newGetContactResponse
--
--         , responseDeleteContact $
--             newDeleteContactResponse
--
--         , responseUpdateContact $
--             newUpdateContactResponse
--
--         , responseGetContactList $
--             newGetContactListResponse
--
--         , responseGetDedicatedIp $
--             newGetDedicatedIpResponse
--
--         , responseCreateContact $
--             newCreateContactResponse
--
--         , responseGetEmailIdentity $
--             newGetEmailIdentityResponse
--
--         , responseGetConfigurationSetEventDestinations $
--             newGetConfigurationSetEventDestinationsResponse
--
--         , responseListCustomVerificationEmailTemplates $
--             newListCustomVerificationEmailTemplatesResponse
--
--         , responseGetAccount $
--             newGetAccountResponse
--
--         , responseDeleteDedicatedIpPool $
--             newDeleteDedicatedIpPoolResponse
--
--         , responseGetEmailIdentityPolicies $
--             newGetEmailIdentityPoliciesResponse
--
--         , responsePutConfigurationSetSuppressionOptions $
--             newPutConfigurationSetSuppressionOptionsResponse
--
--         , responseCreateCustomVerificationEmailTemplate $
--             newCreateCustomVerificationEmailTemplateResponse
--
--         , responsePutAccountDetails $
--             newPutAccountDetailsResponse
--
--         , responseDeleteSuppressedDestination $
--             newDeleteSuppressedDestinationResponse
--
--         , responsePutSuppressedDestination $
--             newPutSuppressedDestinationResponse
--
--         , responseGetDomainStatisticsReport $
--             newGetDomainStatisticsReportResponse
--
--         , responseDeleteEmailTemplate $
--             newDeleteEmailTemplateResponse
--
--         , responseUpdateEmailTemplate $
--             newUpdateEmailTemplateResponse
--
--         , responseGetDeliverabilityDashboardOptions $
--             newGetDeliverabilityDashboardOptionsResponse
--
--         , responseGetCustomVerificationEmailTemplate $
--             newGetCustomVerificationEmailTemplateResponse
--
--         , responseListDomainDeliverabilityCampaigns $
--             newListDomainDeliverabilityCampaignsResponse
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
--         , responseSendEmail $
--             newSendEmailResponse
--
--         , responsePutDedicatedIpWarmupAttributes $
--             newPutDedicatedIpWarmupAttributesResponse
--
--         , responseDeleteEmailIdentityPolicy $
--             newDeleteEmailIdentityPolicyResponse
--
--         , responseUpdateEmailIdentityPolicy $
--             newUpdateEmailIdentityPolicyResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateDeliverabilityTestReport $
--             newCreateDeliverabilityTestReportResponse
--
--         , responsePutEmailIdentityMailFromAttributes $
--             newPutEmailIdentityMailFromAttributesResponse
--
--         , responseListContactLists $
--             newListContactListsResponse
--
--         , responseCreateEmailIdentityPolicy $
--             newCreateEmailIdentityPolicyResponse
--
--         , responseCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestinationResponse
--
--         , responsePutEmailIdentityConfigurationSetAttributes $
--             newPutEmailIdentityConfigurationSetAttributesResponse
--
--         , responsePutAccountSuppressionAttributes $
--             newPutAccountSuppressionAttributesResponse
--
--         , responseCreateImportJob $
--             newCreateImportJobResponse
--
--         , responseListDeliverabilityTestReports $
--             newListDeliverabilityTestReportsResponse
--
--         , responseCreateConfigurationSet $
--             newCreateConfigurationSetResponse
--
--         , responseGetDeliverabilityTestReport $
--             newGetDeliverabilityTestReportResponse
--
--         , responsePutDeliverabilityDashboardOption $
--             newPutDeliverabilityDashboardOptionResponse
--
--         , responsePutAccountDedicatedIpWarmupAttributes $
--             newPutAccountDedicatedIpWarmupAttributesResponse
--
--         , responseListContacts $
--             newListContactsResponse
--
--           ]
--     ]

-- Requests

requestGetConfigurationSet :: GetConfigurationSet -> TestTree
requestGetConfigurationSet =
  req
    "GetConfigurationSet"
    "fixture/GetConfigurationSet.yaml"

requestGetImportJob :: GetImportJob -> TestTree
requestGetImportJob =
  req
    "GetImportJob"
    "fixture/GetImportJob.yaml"

requestPutConfigurationSetTrackingOptions :: PutConfigurationSetTrackingOptions -> TestTree
requestPutConfigurationSetTrackingOptions =
  req
    "PutConfigurationSetTrackingOptions"
    "fixture/PutConfigurationSetTrackingOptions.yaml"

requestPutEmailIdentityDkimSigningAttributes :: PutEmailIdentityDkimSigningAttributes -> TestTree
requestPutEmailIdentityDkimSigningAttributes =
  req
    "PutEmailIdentityDkimSigningAttributes"
    "fixture/PutEmailIdentityDkimSigningAttributes.yaml"

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

requestGetDomainDeliverabilityCampaign :: GetDomainDeliverabilityCampaign -> TestTree
requestGetDomainDeliverabilityCampaign =
  req
    "GetDomainDeliverabilityCampaign"
    "fixture/GetDomainDeliverabilityCampaign.yaml"

requestGetDedicatedIps :: GetDedicatedIps -> TestTree
requestGetDedicatedIps =
  req
    "GetDedicatedIps"
    "fixture/GetDedicatedIps.yaml"

requestPutConfigurationSetSendingOptions :: PutConfigurationSetSendingOptions -> TestTree
requestPutConfigurationSetSendingOptions =
  req
    "PutConfigurationSetSendingOptions"
    "fixture/PutConfigurationSetSendingOptions.yaml"

requestDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplate -> TestTree
requestDeleteCustomVerificationEmailTemplate =
  req
    "DeleteCustomVerificationEmailTemplate"
    "fixture/DeleteCustomVerificationEmailTemplate.yaml"

requestUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplate -> TestTree
requestUpdateCustomVerificationEmailTemplate =
  req
    "UpdateCustomVerificationEmailTemplate"
    "fixture/UpdateCustomVerificationEmailTemplate.yaml"

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

requestGetSuppressedDestination :: GetSuppressedDestination -> TestTree
requestGetSuppressedDestination =
  req
    "GetSuppressedDestination"
    "fixture/GetSuppressedDestination.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetEmailTemplate :: GetEmailTemplate -> TestTree
requestGetEmailTemplate =
  req
    "GetEmailTemplate"
    "fixture/GetEmailTemplate.yaml"

requestListSuppressedDestinations :: ListSuppressedDestinations -> TestTree
requestListSuppressedDestinations =
  req
    "ListSuppressedDestinations"
    "fixture/ListSuppressedDestinations.yaml"

requestPutEmailIdentityFeedbackAttributes :: PutEmailIdentityFeedbackAttributes -> TestTree
requestPutEmailIdentityFeedbackAttributes =
  req
    "PutEmailIdentityFeedbackAttributes"
    "fixture/PutEmailIdentityFeedbackAttributes.yaml"

requestListEmailTemplates :: ListEmailTemplates -> TestTree
requestListEmailTemplates =
  req
    "ListEmailTemplates"
    "fixture/ListEmailTemplates.yaml"

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

requestCreateEmailTemplate :: CreateEmailTemplate -> TestTree
requestCreateEmailTemplate =
  req
    "CreateEmailTemplate"
    "fixture/CreateEmailTemplate.yaml"

requestPutAccountSendingAttributes :: PutAccountSendingAttributes -> TestTree
requestPutAccountSendingAttributes =
  req
    "PutAccountSendingAttributes"
    "fixture/PutAccountSendingAttributes.yaml"

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

requestListConfigurationSets :: ListConfigurationSets -> TestTree
requestListConfigurationSets =
  req
    "ListConfigurationSets"
    "fixture/ListConfigurationSets.yaml"

requestDeleteEmailIdentity :: DeleteEmailIdentity -> TestTree
requestDeleteEmailIdentity =
  req
    "DeleteEmailIdentity"
    "fixture/DeleteEmailIdentity.yaml"

requestDeleteContactList :: DeleteContactList -> TestTree
requestDeleteContactList =
  req
    "DeleteContactList"
    "fixture/DeleteContactList.yaml"

requestUpdateContactList :: UpdateContactList -> TestTree
requestUpdateContactList =
  req
    "UpdateContactList"
    "fixture/UpdateContactList.yaml"

requestListImportJobs :: ListImportJobs -> TestTree
requestListImportJobs =
  req
    "ListImportJobs"
    "fixture/ListImportJobs.yaml"

requestDeleteConfigurationSet :: DeleteConfigurationSet -> TestTree
requestDeleteConfigurationSet =
  req
    "DeleteConfigurationSet"
    "fixture/DeleteConfigurationSet.yaml"

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

requestCreateContactList :: CreateContactList -> TestTree
requestCreateContactList =
  req
    "CreateContactList"
    "fixture/CreateContactList.yaml"

requestListEmailIdentities :: ListEmailIdentities -> TestTree
requestListEmailIdentities =
  req
    "ListEmailIdentities"
    "fixture/ListEmailIdentities.yaml"

requestGetContact :: GetContact -> TestTree
requestGetContact =
  req
    "GetContact"
    "fixture/GetContact.yaml"

requestDeleteContact :: DeleteContact -> TestTree
requestDeleteContact =
  req
    "DeleteContact"
    "fixture/DeleteContact.yaml"

requestUpdateContact :: UpdateContact -> TestTree
requestUpdateContact =
  req
    "UpdateContact"
    "fixture/UpdateContact.yaml"

requestGetContactList :: GetContactList -> TestTree
requestGetContactList =
  req
    "GetContactList"
    "fixture/GetContactList.yaml"

requestGetDedicatedIp :: GetDedicatedIp -> TestTree
requestGetDedicatedIp =
  req
    "GetDedicatedIp"
    "fixture/GetDedicatedIp.yaml"

requestCreateContact :: CreateContact -> TestTree
requestCreateContact =
  req
    "CreateContact"
    "fixture/CreateContact.yaml"

requestGetEmailIdentity :: GetEmailIdentity -> TestTree
requestGetEmailIdentity =
  req
    "GetEmailIdentity"
    "fixture/GetEmailIdentity.yaml"

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

requestGetAccount :: GetAccount -> TestTree
requestGetAccount =
  req
    "GetAccount"
    "fixture/GetAccount.yaml"

requestDeleteDedicatedIpPool :: DeleteDedicatedIpPool -> TestTree
requestDeleteDedicatedIpPool =
  req
    "DeleteDedicatedIpPool"
    "fixture/DeleteDedicatedIpPool.yaml"

requestGetEmailIdentityPolicies :: GetEmailIdentityPolicies -> TestTree
requestGetEmailIdentityPolicies =
  req
    "GetEmailIdentityPolicies"
    "fixture/GetEmailIdentityPolicies.yaml"

requestPutConfigurationSetSuppressionOptions :: PutConfigurationSetSuppressionOptions -> TestTree
requestPutConfigurationSetSuppressionOptions =
  req
    "PutConfigurationSetSuppressionOptions"
    "fixture/PutConfigurationSetSuppressionOptions.yaml"

requestCreateCustomVerificationEmailTemplate :: CreateCustomVerificationEmailTemplate -> TestTree
requestCreateCustomVerificationEmailTemplate =
  req
    "CreateCustomVerificationEmailTemplate"
    "fixture/CreateCustomVerificationEmailTemplate.yaml"

requestPutAccountDetails :: PutAccountDetails -> TestTree
requestPutAccountDetails =
  req
    "PutAccountDetails"
    "fixture/PutAccountDetails.yaml"

requestDeleteSuppressedDestination :: DeleteSuppressedDestination -> TestTree
requestDeleteSuppressedDestination =
  req
    "DeleteSuppressedDestination"
    "fixture/DeleteSuppressedDestination.yaml"

requestPutSuppressedDestination :: PutSuppressedDestination -> TestTree
requestPutSuppressedDestination =
  req
    "PutSuppressedDestination"
    "fixture/PutSuppressedDestination.yaml"

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

requestUpdateEmailTemplate :: UpdateEmailTemplate -> TestTree
requestUpdateEmailTemplate =
  req
    "UpdateEmailTemplate"
    "fixture/UpdateEmailTemplate.yaml"

requestGetDeliverabilityDashboardOptions :: GetDeliverabilityDashboardOptions -> TestTree
requestGetDeliverabilityDashboardOptions =
  req
    "GetDeliverabilityDashboardOptions"
    "fixture/GetDeliverabilityDashboardOptions.yaml"

requestGetCustomVerificationEmailTemplate :: GetCustomVerificationEmailTemplate -> TestTree
requestGetCustomVerificationEmailTemplate =
  req
    "GetCustomVerificationEmailTemplate"
    "fixture/GetCustomVerificationEmailTemplate.yaml"

requestListDomainDeliverabilityCampaigns :: ListDomainDeliverabilityCampaigns -> TestTree
requestListDomainDeliverabilityCampaigns =
  req
    "ListDomainDeliverabilityCampaigns"
    "fixture/ListDomainDeliverabilityCampaigns.yaml"

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

requestSendEmail :: SendEmail -> TestTree
requestSendEmail =
  req
    "SendEmail"
    "fixture/SendEmail.yaml"

requestPutDedicatedIpWarmupAttributes :: PutDedicatedIpWarmupAttributes -> TestTree
requestPutDedicatedIpWarmupAttributes =
  req
    "PutDedicatedIpWarmupAttributes"
    "fixture/PutDedicatedIpWarmupAttributes.yaml"

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

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateDeliverabilityTestReport :: CreateDeliverabilityTestReport -> TestTree
requestCreateDeliverabilityTestReport =
  req
    "CreateDeliverabilityTestReport"
    "fixture/CreateDeliverabilityTestReport.yaml"

requestPutEmailIdentityMailFromAttributes :: PutEmailIdentityMailFromAttributes -> TestTree
requestPutEmailIdentityMailFromAttributes =
  req
    "PutEmailIdentityMailFromAttributes"
    "fixture/PutEmailIdentityMailFromAttributes.yaml"

requestListContactLists :: ListContactLists -> TestTree
requestListContactLists =
  req
    "ListContactLists"
    "fixture/ListContactLists.yaml"

requestCreateEmailIdentityPolicy :: CreateEmailIdentityPolicy -> TestTree
requestCreateEmailIdentityPolicy =
  req
    "CreateEmailIdentityPolicy"
    "fixture/CreateEmailIdentityPolicy.yaml"

requestCreateConfigurationSetEventDestination :: CreateConfigurationSetEventDestination -> TestTree
requestCreateConfigurationSetEventDestination =
  req
    "CreateConfigurationSetEventDestination"
    "fixture/CreateConfigurationSetEventDestination.yaml"

requestPutEmailIdentityConfigurationSetAttributes :: PutEmailIdentityConfigurationSetAttributes -> TestTree
requestPutEmailIdentityConfigurationSetAttributes =
  req
    "PutEmailIdentityConfigurationSetAttributes"
    "fixture/PutEmailIdentityConfigurationSetAttributes.yaml"

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

requestListDeliverabilityTestReports :: ListDeliverabilityTestReports -> TestTree
requestListDeliverabilityTestReports =
  req
    "ListDeliverabilityTestReports"
    "fixture/ListDeliverabilityTestReports.yaml"

requestCreateConfigurationSet :: CreateConfigurationSet -> TestTree
requestCreateConfigurationSet =
  req
    "CreateConfigurationSet"
    "fixture/CreateConfigurationSet.yaml"

requestGetDeliverabilityTestReport :: GetDeliverabilityTestReport -> TestTree
requestGetDeliverabilityTestReport =
  req
    "GetDeliverabilityTestReport"
    "fixture/GetDeliverabilityTestReport.yaml"

requestPutDeliverabilityDashboardOption :: PutDeliverabilityDashboardOption -> TestTree
requestPutDeliverabilityDashboardOption =
  req
    "PutDeliverabilityDashboardOption"
    "fixture/PutDeliverabilityDashboardOption.yaml"

requestPutAccountDedicatedIpWarmupAttributes :: PutAccountDedicatedIpWarmupAttributes -> TestTree
requestPutAccountDedicatedIpWarmupAttributes =
  req
    "PutAccountDedicatedIpWarmupAttributes"
    "fixture/PutAccountDedicatedIpWarmupAttributes.yaml"

requestListContacts :: ListContacts -> TestTree
requestListContacts =
  req
    "ListContacts"
    "fixture/ListContacts.yaml"

-- Responses

responseGetConfigurationSet :: GetConfigurationSetResponse -> TestTree
responseGetConfigurationSet =
  res
    "GetConfigurationSetResponse"
    "fixture/GetConfigurationSetResponse.proto"
    defaultService
    (Proxy :: Proxy GetConfigurationSet)

responseGetImportJob :: GetImportJobResponse -> TestTree
responseGetImportJob =
  res
    "GetImportJobResponse"
    "fixture/GetImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy GetImportJob)

responsePutConfigurationSetTrackingOptions :: PutConfigurationSetTrackingOptionsResponse -> TestTree
responsePutConfigurationSetTrackingOptions =
  res
    "PutConfigurationSetTrackingOptionsResponse"
    "fixture/PutConfigurationSetTrackingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutConfigurationSetTrackingOptions)

responsePutEmailIdentityDkimSigningAttributes :: PutEmailIdentityDkimSigningAttributesResponse -> TestTree
responsePutEmailIdentityDkimSigningAttributes =
  res
    "PutEmailIdentityDkimSigningAttributesResponse"
    "fixture/PutEmailIdentityDkimSigningAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutEmailIdentityDkimSigningAttributes)

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

responseGetDomainDeliverabilityCampaign :: GetDomainDeliverabilityCampaignResponse -> TestTree
responseGetDomainDeliverabilityCampaign =
  res
    "GetDomainDeliverabilityCampaignResponse"
    "fixture/GetDomainDeliverabilityCampaignResponse.proto"
    defaultService
    (Proxy :: Proxy GetDomainDeliverabilityCampaign)

responseGetDedicatedIps :: GetDedicatedIpsResponse -> TestTree
responseGetDedicatedIps =
  res
    "GetDedicatedIpsResponse"
    "fixture/GetDedicatedIpsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDedicatedIps)

responsePutConfigurationSetSendingOptions :: PutConfigurationSetSendingOptionsResponse -> TestTree
responsePutConfigurationSetSendingOptions =
  res
    "PutConfigurationSetSendingOptionsResponse"
    "fixture/PutConfigurationSetSendingOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutConfigurationSetSendingOptions)

responseDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplateResponse -> TestTree
responseDeleteCustomVerificationEmailTemplate =
  res
    "DeleteCustomVerificationEmailTemplateResponse"
    "fixture/DeleteCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCustomVerificationEmailTemplate)

responseUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplateResponse -> TestTree
responseUpdateCustomVerificationEmailTemplate =
  res
    "UpdateCustomVerificationEmailTemplateResponse"
    "fixture/UpdateCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCustomVerificationEmailTemplate)

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

responseGetSuppressedDestination :: GetSuppressedDestinationResponse -> TestTree
responseGetSuppressedDestination =
  res
    "GetSuppressedDestinationResponse"
    "fixture/GetSuppressedDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy GetSuppressedDestination)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseGetEmailTemplate :: GetEmailTemplateResponse -> TestTree
responseGetEmailTemplate =
  res
    "GetEmailTemplateResponse"
    "fixture/GetEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetEmailTemplate)

responseListSuppressedDestinations :: ListSuppressedDestinationsResponse -> TestTree
responseListSuppressedDestinations =
  res
    "ListSuppressedDestinationsResponse"
    "fixture/ListSuppressedDestinationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSuppressedDestinations)

responsePutEmailIdentityFeedbackAttributes :: PutEmailIdentityFeedbackAttributesResponse -> TestTree
responsePutEmailIdentityFeedbackAttributes =
  res
    "PutEmailIdentityFeedbackAttributesResponse"
    "fixture/PutEmailIdentityFeedbackAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutEmailIdentityFeedbackAttributes)

responseListEmailTemplates :: ListEmailTemplatesResponse -> TestTree
responseListEmailTemplates =
  res
    "ListEmailTemplatesResponse"
    "fixture/ListEmailTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListEmailTemplates)

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

responseCreateEmailTemplate :: CreateEmailTemplateResponse -> TestTree
responseCreateEmailTemplate =
  res
    "CreateEmailTemplateResponse"
    "fixture/CreateEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEmailTemplate)

responsePutAccountSendingAttributes :: PutAccountSendingAttributesResponse -> TestTree
responsePutAccountSendingAttributes =
  res
    "PutAccountSendingAttributesResponse"
    "fixture/PutAccountSendingAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutAccountSendingAttributes)

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

responseListConfigurationSets :: ListConfigurationSetsResponse -> TestTree
responseListConfigurationSets =
  res
    "ListConfigurationSetsResponse"
    "fixture/ListConfigurationSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListConfigurationSets)

responseDeleteEmailIdentity :: DeleteEmailIdentityResponse -> TestTree
responseDeleteEmailIdentity =
  res
    "DeleteEmailIdentityResponse"
    "fixture/DeleteEmailIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEmailIdentity)

responseDeleteContactList :: DeleteContactListResponse -> TestTree
responseDeleteContactList =
  res
    "DeleteContactListResponse"
    "fixture/DeleteContactListResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteContactList)

responseUpdateContactList :: UpdateContactListResponse -> TestTree
responseUpdateContactList =
  res
    "UpdateContactListResponse"
    "fixture/UpdateContactListResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContactList)

responseListImportJobs :: ListImportJobsResponse -> TestTree
responseListImportJobs =
  res
    "ListImportJobsResponse"
    "fixture/ListImportJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListImportJobs)

responseDeleteConfigurationSet :: DeleteConfigurationSetResponse -> TestTree
responseDeleteConfigurationSet =
  res
    "DeleteConfigurationSetResponse"
    "fixture/DeleteConfigurationSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfigurationSet)

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

responseCreateContactList :: CreateContactListResponse -> TestTree
responseCreateContactList =
  res
    "CreateContactListResponse"
    "fixture/CreateContactListResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContactList)

responseListEmailIdentities :: ListEmailIdentitiesResponse -> TestTree
responseListEmailIdentities =
  res
    "ListEmailIdentitiesResponse"
    "fixture/ListEmailIdentitiesResponse.proto"
    defaultService
    (Proxy :: Proxy ListEmailIdentities)

responseGetContact :: GetContactResponse -> TestTree
responseGetContact =
  res
    "GetContactResponse"
    "fixture/GetContactResponse.proto"
    defaultService
    (Proxy :: Proxy GetContact)

responseDeleteContact :: DeleteContactResponse -> TestTree
responseDeleteContact =
  res
    "DeleteContactResponse"
    "fixture/DeleteContactResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteContact)

responseUpdateContact :: UpdateContactResponse -> TestTree
responseUpdateContact =
  res
    "UpdateContactResponse"
    "fixture/UpdateContactResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContact)

responseGetContactList :: GetContactListResponse -> TestTree
responseGetContactList =
  res
    "GetContactListResponse"
    "fixture/GetContactListResponse.proto"
    defaultService
    (Proxy :: Proxy GetContactList)

responseGetDedicatedIp :: GetDedicatedIpResponse -> TestTree
responseGetDedicatedIp =
  res
    "GetDedicatedIpResponse"
    "fixture/GetDedicatedIpResponse.proto"
    defaultService
    (Proxy :: Proxy GetDedicatedIp)

responseCreateContact :: CreateContactResponse -> TestTree
responseCreateContact =
  res
    "CreateContactResponse"
    "fixture/CreateContactResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContact)

responseGetEmailIdentity :: GetEmailIdentityResponse -> TestTree
responseGetEmailIdentity =
  res
    "GetEmailIdentityResponse"
    "fixture/GetEmailIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy GetEmailIdentity)

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

responseGetAccount :: GetAccountResponse -> TestTree
responseGetAccount =
  res
    "GetAccountResponse"
    "fixture/GetAccountResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccount)

responseDeleteDedicatedIpPool :: DeleteDedicatedIpPoolResponse -> TestTree
responseDeleteDedicatedIpPool =
  res
    "DeleteDedicatedIpPoolResponse"
    "fixture/DeleteDedicatedIpPoolResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDedicatedIpPool)

responseGetEmailIdentityPolicies :: GetEmailIdentityPoliciesResponse -> TestTree
responseGetEmailIdentityPolicies =
  res
    "GetEmailIdentityPoliciesResponse"
    "fixture/GetEmailIdentityPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy GetEmailIdentityPolicies)

responsePutConfigurationSetSuppressionOptions :: PutConfigurationSetSuppressionOptionsResponse -> TestTree
responsePutConfigurationSetSuppressionOptions =
  res
    "PutConfigurationSetSuppressionOptionsResponse"
    "fixture/PutConfigurationSetSuppressionOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutConfigurationSetSuppressionOptions)

responseCreateCustomVerificationEmailTemplate :: CreateCustomVerificationEmailTemplateResponse -> TestTree
responseCreateCustomVerificationEmailTemplate =
  res
    "CreateCustomVerificationEmailTemplateResponse"
    "fixture/CreateCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCustomVerificationEmailTemplate)

responsePutAccountDetails :: PutAccountDetailsResponse -> TestTree
responsePutAccountDetails =
  res
    "PutAccountDetailsResponse"
    "fixture/PutAccountDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy PutAccountDetails)

responseDeleteSuppressedDestination :: DeleteSuppressedDestinationResponse -> TestTree
responseDeleteSuppressedDestination =
  res
    "DeleteSuppressedDestinationResponse"
    "fixture/DeleteSuppressedDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSuppressedDestination)

responsePutSuppressedDestination :: PutSuppressedDestinationResponse -> TestTree
responsePutSuppressedDestination =
  res
    "PutSuppressedDestinationResponse"
    "fixture/PutSuppressedDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy PutSuppressedDestination)

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

responseUpdateEmailTemplate :: UpdateEmailTemplateResponse -> TestTree
responseUpdateEmailTemplate =
  res
    "UpdateEmailTemplateResponse"
    "fixture/UpdateEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEmailTemplate)

responseGetDeliverabilityDashboardOptions :: GetDeliverabilityDashboardOptionsResponse -> TestTree
responseGetDeliverabilityDashboardOptions =
  res
    "GetDeliverabilityDashboardOptionsResponse"
    "fixture/GetDeliverabilityDashboardOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeliverabilityDashboardOptions)

responseGetCustomVerificationEmailTemplate :: GetCustomVerificationEmailTemplateResponse -> TestTree
responseGetCustomVerificationEmailTemplate =
  res
    "GetCustomVerificationEmailTemplateResponse"
    "fixture/GetCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetCustomVerificationEmailTemplate)

responseListDomainDeliverabilityCampaigns :: ListDomainDeliverabilityCampaignsResponse -> TestTree
responseListDomainDeliverabilityCampaigns =
  res
    "ListDomainDeliverabilityCampaignsResponse"
    "fixture/ListDomainDeliverabilityCampaignsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDomainDeliverabilityCampaigns)

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

responseSendEmail :: SendEmailResponse -> TestTree
responseSendEmail =
  res
    "SendEmailResponse"
    "fixture/SendEmailResponse.proto"
    defaultService
    (Proxy :: Proxy SendEmail)

responsePutDedicatedIpWarmupAttributes :: PutDedicatedIpWarmupAttributesResponse -> TestTree
responsePutDedicatedIpWarmupAttributes =
  res
    "PutDedicatedIpWarmupAttributesResponse"
    "fixture/PutDedicatedIpWarmupAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutDedicatedIpWarmupAttributes)

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

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseCreateDeliverabilityTestReport :: CreateDeliverabilityTestReportResponse -> TestTree
responseCreateDeliverabilityTestReport =
  res
    "CreateDeliverabilityTestReportResponse"
    "fixture/CreateDeliverabilityTestReportResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeliverabilityTestReport)

responsePutEmailIdentityMailFromAttributes :: PutEmailIdentityMailFromAttributesResponse -> TestTree
responsePutEmailIdentityMailFromAttributes =
  res
    "PutEmailIdentityMailFromAttributesResponse"
    "fixture/PutEmailIdentityMailFromAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutEmailIdentityMailFromAttributes)

responseListContactLists :: ListContactListsResponse -> TestTree
responseListContactLists =
  res
    "ListContactListsResponse"
    "fixture/ListContactListsResponse.proto"
    defaultService
    (Proxy :: Proxy ListContactLists)

responseCreateEmailIdentityPolicy :: CreateEmailIdentityPolicyResponse -> TestTree
responseCreateEmailIdentityPolicy =
  res
    "CreateEmailIdentityPolicyResponse"
    "fixture/CreateEmailIdentityPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEmailIdentityPolicy)

responseCreateConfigurationSetEventDestination :: CreateConfigurationSetEventDestinationResponse -> TestTree
responseCreateConfigurationSetEventDestination =
  res
    "CreateConfigurationSetEventDestinationResponse"
    "fixture/CreateConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConfigurationSetEventDestination)

responsePutEmailIdentityConfigurationSetAttributes :: PutEmailIdentityConfigurationSetAttributesResponse -> TestTree
responsePutEmailIdentityConfigurationSetAttributes =
  res
    "PutEmailIdentityConfigurationSetAttributesResponse"
    "fixture/PutEmailIdentityConfigurationSetAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutEmailIdentityConfigurationSetAttributes)

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

responseListDeliverabilityTestReports :: ListDeliverabilityTestReportsResponse -> TestTree
responseListDeliverabilityTestReports =
  res
    "ListDeliverabilityTestReportsResponse"
    "fixture/ListDeliverabilityTestReportsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeliverabilityTestReports)

responseCreateConfigurationSet :: CreateConfigurationSetResponse -> TestTree
responseCreateConfigurationSet =
  res
    "CreateConfigurationSetResponse"
    "fixture/CreateConfigurationSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConfigurationSet)

responseGetDeliverabilityTestReport :: GetDeliverabilityTestReportResponse -> TestTree
responseGetDeliverabilityTestReport =
  res
    "GetDeliverabilityTestReportResponse"
    "fixture/GetDeliverabilityTestReportResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeliverabilityTestReport)

responsePutDeliverabilityDashboardOption :: PutDeliverabilityDashboardOptionResponse -> TestTree
responsePutDeliverabilityDashboardOption =
  res
    "PutDeliverabilityDashboardOptionResponse"
    "fixture/PutDeliverabilityDashboardOptionResponse.proto"
    defaultService
    (Proxy :: Proxy PutDeliverabilityDashboardOption)

responsePutAccountDedicatedIpWarmupAttributes :: PutAccountDedicatedIpWarmupAttributesResponse -> TestTree
responsePutAccountDedicatedIpWarmupAttributes =
  res
    "PutAccountDedicatedIpWarmupAttributesResponse"
    "fixture/PutAccountDedicatedIpWarmupAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutAccountDedicatedIpWarmupAttributes)

responseListContacts :: ListContactsResponse -> TestTree
responseListContacts =
  res
    "ListContactsResponse"
    "fixture/ListContactsResponse.proto"
    defaultService
    (Proxy :: Proxy ListContacts)
