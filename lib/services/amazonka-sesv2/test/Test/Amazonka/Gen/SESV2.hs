{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SESV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SESV2 where

import Amazonka.SESV2
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SESV2.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchGetMetricData $
--             newBatchGetMetricData
--
--         , requestCreateConfigurationSet $
--             newCreateConfigurationSet
--
--         , requestCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestination
--
--         , requestCreateContact $
--             newCreateContact
--
--         , requestCreateContactList $
--             newCreateContactList
--
--         , requestCreateCustomVerificationEmailTemplate $
--             newCreateCustomVerificationEmailTemplate
--
--         , requestCreateDedicatedIpPool $
--             newCreateDedicatedIpPool
--
--         , requestCreateDeliverabilityTestReport $
--             newCreateDeliverabilityTestReport
--
--         , requestCreateEmailIdentity $
--             newCreateEmailIdentity
--
--         , requestCreateEmailIdentityPolicy $
--             newCreateEmailIdentityPolicy
--
--         , requestCreateEmailTemplate $
--             newCreateEmailTemplate
--
--         , requestCreateImportJob $
--             newCreateImportJob
--
--         , requestDeleteConfigurationSet $
--             newDeleteConfigurationSet
--
--         , requestDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestination
--
--         , requestDeleteContact $
--             newDeleteContact
--
--         , requestDeleteContactList $
--             newDeleteContactList
--
--         , requestDeleteCustomVerificationEmailTemplate $
--             newDeleteCustomVerificationEmailTemplate
--
--         , requestDeleteDedicatedIpPool $
--             newDeleteDedicatedIpPool
--
--         , requestDeleteEmailIdentity $
--             newDeleteEmailIdentity
--
--         , requestDeleteEmailIdentityPolicy $
--             newDeleteEmailIdentityPolicy
--
--         , requestDeleteEmailTemplate $
--             newDeleteEmailTemplate
--
--         , requestDeleteSuppressedDestination $
--             newDeleteSuppressedDestination
--
--         , requestGetAccount $
--             newGetAccount
--
--         , requestGetBlacklistReports $
--             newGetBlacklistReports
--
--         , requestGetConfigurationSet $
--             newGetConfigurationSet
--
--         , requestGetConfigurationSetEventDestinations $
--             newGetConfigurationSetEventDestinations
--
--         , requestGetContact $
--             newGetContact
--
--         , requestGetContactList $
--             newGetContactList
--
--         , requestGetCustomVerificationEmailTemplate $
--             newGetCustomVerificationEmailTemplate
--
--         , requestGetDedicatedIp $
--             newGetDedicatedIp
--
--         , requestGetDedicatedIpPool $
--             newGetDedicatedIpPool
--
--         , requestGetDedicatedIps $
--             newGetDedicatedIps
--
--         , requestGetDeliverabilityDashboardOptions $
--             newGetDeliverabilityDashboardOptions
--
--         , requestGetDeliverabilityTestReport $
--             newGetDeliverabilityTestReport
--
--         , requestGetDomainDeliverabilityCampaign $
--             newGetDomainDeliverabilityCampaign
--
--         , requestGetDomainStatisticsReport $
--             newGetDomainStatisticsReport
--
--         , requestGetEmailIdentity $
--             newGetEmailIdentity
--
--         , requestGetEmailIdentityPolicies $
--             newGetEmailIdentityPolicies
--
--         , requestGetEmailTemplate $
--             newGetEmailTemplate
--
--         , requestGetImportJob $
--             newGetImportJob
--
--         , requestGetSuppressedDestination $
--             newGetSuppressedDestination
--
--         , requestListConfigurationSets $
--             newListConfigurationSets
--
--         , requestListContactLists $
--             newListContactLists
--
--         , requestListContacts $
--             newListContacts
--
--         , requestListCustomVerificationEmailTemplates $
--             newListCustomVerificationEmailTemplates
--
--         , requestListDedicatedIpPools $
--             newListDedicatedIpPools
--
--         , requestListDeliverabilityTestReports $
--             newListDeliverabilityTestReports
--
--         , requestListDomainDeliverabilityCampaigns $
--             newListDomainDeliverabilityCampaigns
--
--         , requestListEmailIdentities $
--             newListEmailIdentities
--
--         , requestListEmailTemplates $
--             newListEmailTemplates
--
--         , requestListImportJobs $
--             newListImportJobs
--
--         , requestListRecommendations $
--             newListRecommendations
--
--         , requestListSuppressedDestinations $
--             newListSuppressedDestinations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutAccountDedicatedIpWarmupAttributes $
--             newPutAccountDedicatedIpWarmupAttributes
--
--         , requestPutAccountDetails $
--             newPutAccountDetails
--
--         , requestPutAccountSendingAttributes $
--             newPutAccountSendingAttributes
--
--         , requestPutAccountSuppressionAttributes $
--             newPutAccountSuppressionAttributes
--
--         , requestPutAccountVdmAttributes $
--             newPutAccountVdmAttributes
--
--         , requestPutConfigurationSetDeliveryOptions $
--             newPutConfigurationSetDeliveryOptions
--
--         , requestPutConfigurationSetReputationOptions $
--             newPutConfigurationSetReputationOptions
--
--         , requestPutConfigurationSetSendingOptions $
--             newPutConfigurationSetSendingOptions
--
--         , requestPutConfigurationSetSuppressionOptions $
--             newPutConfigurationSetSuppressionOptions
--
--         , requestPutConfigurationSetTrackingOptions $
--             newPutConfigurationSetTrackingOptions
--
--         , requestPutConfigurationSetVdmOptions $
--             newPutConfigurationSetVdmOptions
--
--         , requestPutDedicatedIpInPool $
--             newPutDedicatedIpInPool
--
--         , requestPutDedicatedIpWarmupAttributes $
--             newPutDedicatedIpWarmupAttributes
--
--         , requestPutDeliverabilityDashboardOption $
--             newPutDeliverabilityDashboardOption
--
--         , requestPutEmailIdentityConfigurationSetAttributes $
--             newPutEmailIdentityConfigurationSetAttributes
--
--         , requestPutEmailIdentityDkimAttributes $
--             newPutEmailIdentityDkimAttributes
--
--         , requestPutEmailIdentityDkimSigningAttributes $
--             newPutEmailIdentityDkimSigningAttributes
--
--         , requestPutEmailIdentityFeedbackAttributes $
--             newPutEmailIdentityFeedbackAttributes
--
--         , requestPutEmailIdentityMailFromAttributes $
--             newPutEmailIdentityMailFromAttributes
--
--         , requestPutSuppressedDestination $
--             newPutSuppressedDestination
--
--         , requestSendBulkEmail $
--             newSendBulkEmail
--
--         , requestSendCustomVerificationEmail $
--             newSendCustomVerificationEmail
--
--         , requestSendEmail $
--             newSendEmail
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTestRenderEmailTemplate $
--             newTestRenderEmailTemplate
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestination
--
--         , requestUpdateContact $
--             newUpdateContact
--
--         , requestUpdateContactList $
--             newUpdateContactList
--
--         , requestUpdateCustomVerificationEmailTemplate $
--             newUpdateCustomVerificationEmailTemplate
--
--         , requestUpdateEmailIdentityPolicy $
--             newUpdateEmailIdentityPolicy
--
--         , requestUpdateEmailTemplate $
--             newUpdateEmailTemplate
--
--           ]

--     , testGroup "response"
--         [ responseBatchGetMetricData $
--             newBatchGetMetricDataResponse
--
--         , responseCreateConfigurationSet $
--             newCreateConfigurationSetResponse
--
--         , responseCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestinationResponse
--
--         , responseCreateContact $
--             newCreateContactResponse
--
--         , responseCreateContactList $
--             newCreateContactListResponse
--
--         , responseCreateCustomVerificationEmailTemplate $
--             newCreateCustomVerificationEmailTemplateResponse
--
--         , responseCreateDedicatedIpPool $
--             newCreateDedicatedIpPoolResponse
--
--         , responseCreateDeliverabilityTestReport $
--             newCreateDeliverabilityTestReportResponse
--
--         , responseCreateEmailIdentity $
--             newCreateEmailIdentityResponse
--
--         , responseCreateEmailIdentityPolicy $
--             newCreateEmailIdentityPolicyResponse
--
--         , responseCreateEmailTemplate $
--             newCreateEmailTemplateResponse
--
--         , responseCreateImportJob $
--             newCreateImportJobResponse
--
--         , responseDeleteConfigurationSet $
--             newDeleteConfigurationSetResponse
--
--         , responseDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestinationResponse
--
--         , responseDeleteContact $
--             newDeleteContactResponse
--
--         , responseDeleteContactList $
--             newDeleteContactListResponse
--
--         , responseDeleteCustomVerificationEmailTemplate $
--             newDeleteCustomVerificationEmailTemplateResponse
--
--         , responseDeleteDedicatedIpPool $
--             newDeleteDedicatedIpPoolResponse
--
--         , responseDeleteEmailIdentity $
--             newDeleteEmailIdentityResponse
--
--         , responseDeleteEmailIdentityPolicy $
--             newDeleteEmailIdentityPolicyResponse
--
--         , responseDeleteEmailTemplate $
--             newDeleteEmailTemplateResponse
--
--         , responseDeleteSuppressedDestination $
--             newDeleteSuppressedDestinationResponse
--
--         , responseGetAccount $
--             newGetAccountResponse
--
--         , responseGetBlacklistReports $
--             newGetBlacklistReportsResponse
--
--         , responseGetConfigurationSet $
--             newGetConfigurationSetResponse
--
--         , responseGetConfigurationSetEventDestinations $
--             newGetConfigurationSetEventDestinationsResponse
--
--         , responseGetContact $
--             newGetContactResponse
--
--         , responseGetContactList $
--             newGetContactListResponse
--
--         , responseGetCustomVerificationEmailTemplate $
--             newGetCustomVerificationEmailTemplateResponse
--
--         , responseGetDedicatedIp $
--             newGetDedicatedIpResponse
--
--         , responseGetDedicatedIpPool $
--             newGetDedicatedIpPoolResponse
--
--         , responseGetDedicatedIps $
--             newGetDedicatedIpsResponse
--
--         , responseGetDeliverabilityDashboardOptions $
--             newGetDeliverabilityDashboardOptionsResponse
--
--         , responseGetDeliverabilityTestReport $
--             newGetDeliverabilityTestReportResponse
--
--         , responseGetDomainDeliverabilityCampaign $
--             newGetDomainDeliverabilityCampaignResponse
--
--         , responseGetDomainStatisticsReport $
--             newGetDomainStatisticsReportResponse
--
--         , responseGetEmailIdentity $
--             newGetEmailIdentityResponse
--
--         , responseGetEmailIdentityPolicies $
--             newGetEmailIdentityPoliciesResponse
--
--         , responseGetEmailTemplate $
--             newGetEmailTemplateResponse
--
--         , responseGetImportJob $
--             newGetImportJobResponse
--
--         , responseGetSuppressedDestination $
--             newGetSuppressedDestinationResponse
--
--         , responseListConfigurationSets $
--             newListConfigurationSetsResponse
--
--         , responseListContactLists $
--             newListContactListsResponse
--
--         , responseListContacts $
--             newListContactsResponse
--
--         , responseListCustomVerificationEmailTemplates $
--             newListCustomVerificationEmailTemplatesResponse
--
--         , responseListDedicatedIpPools $
--             newListDedicatedIpPoolsResponse
--
--         , responseListDeliverabilityTestReports $
--             newListDeliverabilityTestReportsResponse
--
--         , responseListDomainDeliverabilityCampaigns $
--             newListDomainDeliverabilityCampaignsResponse
--
--         , responseListEmailIdentities $
--             newListEmailIdentitiesResponse
--
--         , responseListEmailTemplates $
--             newListEmailTemplatesResponse
--
--         , responseListImportJobs $
--             newListImportJobsResponse
--
--         , responseListRecommendations $
--             newListRecommendationsResponse
--
--         , responseListSuppressedDestinations $
--             newListSuppressedDestinationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutAccountDedicatedIpWarmupAttributes $
--             newPutAccountDedicatedIpWarmupAttributesResponse
--
--         , responsePutAccountDetails $
--             newPutAccountDetailsResponse
--
--         , responsePutAccountSendingAttributes $
--             newPutAccountSendingAttributesResponse
--
--         , responsePutAccountSuppressionAttributes $
--             newPutAccountSuppressionAttributesResponse
--
--         , responsePutAccountVdmAttributes $
--             newPutAccountVdmAttributesResponse
--
--         , responsePutConfigurationSetDeliveryOptions $
--             newPutConfigurationSetDeliveryOptionsResponse
--
--         , responsePutConfigurationSetReputationOptions $
--             newPutConfigurationSetReputationOptionsResponse
--
--         , responsePutConfigurationSetSendingOptions $
--             newPutConfigurationSetSendingOptionsResponse
--
--         , responsePutConfigurationSetSuppressionOptions $
--             newPutConfigurationSetSuppressionOptionsResponse
--
--         , responsePutConfigurationSetTrackingOptions $
--             newPutConfigurationSetTrackingOptionsResponse
--
--         , responsePutConfigurationSetVdmOptions $
--             newPutConfigurationSetVdmOptionsResponse
--
--         , responsePutDedicatedIpInPool $
--             newPutDedicatedIpInPoolResponse
--
--         , responsePutDedicatedIpWarmupAttributes $
--             newPutDedicatedIpWarmupAttributesResponse
--
--         , responsePutDeliverabilityDashboardOption $
--             newPutDeliverabilityDashboardOptionResponse
--
--         , responsePutEmailIdentityConfigurationSetAttributes $
--             newPutEmailIdentityConfigurationSetAttributesResponse
--
--         , responsePutEmailIdentityDkimAttributes $
--             newPutEmailIdentityDkimAttributesResponse
--
--         , responsePutEmailIdentityDkimSigningAttributes $
--             newPutEmailIdentityDkimSigningAttributesResponse
--
--         , responsePutEmailIdentityFeedbackAttributes $
--             newPutEmailIdentityFeedbackAttributesResponse
--
--         , responsePutEmailIdentityMailFromAttributes $
--             newPutEmailIdentityMailFromAttributesResponse
--
--         , responsePutSuppressedDestination $
--             newPutSuppressedDestinationResponse
--
--         , responseSendBulkEmail $
--             newSendBulkEmailResponse
--
--         , responseSendCustomVerificationEmail $
--             newSendCustomVerificationEmailResponse
--
--         , responseSendEmail $
--             newSendEmailResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTestRenderEmailTemplate $
--             newTestRenderEmailTemplateResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestinationResponse
--
--         , responseUpdateContact $
--             newUpdateContactResponse
--
--         , responseUpdateContactList $
--             newUpdateContactListResponse
--
--         , responseUpdateCustomVerificationEmailTemplate $
--             newUpdateCustomVerificationEmailTemplateResponse
--
--         , responseUpdateEmailIdentityPolicy $
--             newUpdateEmailIdentityPolicyResponse
--
--         , responseUpdateEmailTemplate $
--             newUpdateEmailTemplateResponse
--
--           ]
--     ]

-- Requests

requestBatchGetMetricData :: BatchGetMetricData -> TestTree
requestBatchGetMetricData =
  req
    "BatchGetMetricData"
    "fixture/BatchGetMetricData.yaml"

requestCreateConfigurationSet :: CreateConfigurationSet -> TestTree
requestCreateConfigurationSet =
  req
    "CreateConfigurationSet"
    "fixture/CreateConfigurationSet.yaml"

requestCreateConfigurationSetEventDestination :: CreateConfigurationSetEventDestination -> TestTree
requestCreateConfigurationSetEventDestination =
  req
    "CreateConfigurationSetEventDestination"
    "fixture/CreateConfigurationSetEventDestination.yaml"

requestCreateContact :: CreateContact -> TestTree
requestCreateContact =
  req
    "CreateContact"
    "fixture/CreateContact.yaml"

requestCreateContactList :: CreateContactList -> TestTree
requestCreateContactList =
  req
    "CreateContactList"
    "fixture/CreateContactList.yaml"

requestCreateCustomVerificationEmailTemplate :: CreateCustomVerificationEmailTemplate -> TestTree
requestCreateCustomVerificationEmailTemplate =
  req
    "CreateCustomVerificationEmailTemplate"
    "fixture/CreateCustomVerificationEmailTemplate.yaml"

requestCreateDedicatedIpPool :: CreateDedicatedIpPool -> TestTree
requestCreateDedicatedIpPool =
  req
    "CreateDedicatedIpPool"
    "fixture/CreateDedicatedIpPool.yaml"

requestCreateDeliverabilityTestReport :: CreateDeliverabilityTestReport -> TestTree
requestCreateDeliverabilityTestReport =
  req
    "CreateDeliverabilityTestReport"
    "fixture/CreateDeliverabilityTestReport.yaml"

requestCreateEmailIdentity :: CreateEmailIdentity -> TestTree
requestCreateEmailIdentity =
  req
    "CreateEmailIdentity"
    "fixture/CreateEmailIdentity.yaml"

requestCreateEmailIdentityPolicy :: CreateEmailIdentityPolicy -> TestTree
requestCreateEmailIdentityPolicy =
  req
    "CreateEmailIdentityPolicy"
    "fixture/CreateEmailIdentityPolicy.yaml"

requestCreateEmailTemplate :: CreateEmailTemplate -> TestTree
requestCreateEmailTemplate =
  req
    "CreateEmailTemplate"
    "fixture/CreateEmailTemplate.yaml"

requestCreateImportJob :: CreateImportJob -> TestTree
requestCreateImportJob =
  req
    "CreateImportJob"
    "fixture/CreateImportJob.yaml"

requestDeleteConfigurationSet :: DeleteConfigurationSet -> TestTree
requestDeleteConfigurationSet =
  req
    "DeleteConfigurationSet"
    "fixture/DeleteConfigurationSet.yaml"

requestDeleteConfigurationSetEventDestination :: DeleteConfigurationSetEventDestination -> TestTree
requestDeleteConfigurationSetEventDestination =
  req
    "DeleteConfigurationSetEventDestination"
    "fixture/DeleteConfigurationSetEventDestination.yaml"

requestDeleteContact :: DeleteContact -> TestTree
requestDeleteContact =
  req
    "DeleteContact"
    "fixture/DeleteContact.yaml"

requestDeleteContactList :: DeleteContactList -> TestTree
requestDeleteContactList =
  req
    "DeleteContactList"
    "fixture/DeleteContactList.yaml"

requestDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplate -> TestTree
requestDeleteCustomVerificationEmailTemplate =
  req
    "DeleteCustomVerificationEmailTemplate"
    "fixture/DeleteCustomVerificationEmailTemplate.yaml"

requestDeleteDedicatedIpPool :: DeleteDedicatedIpPool -> TestTree
requestDeleteDedicatedIpPool =
  req
    "DeleteDedicatedIpPool"
    "fixture/DeleteDedicatedIpPool.yaml"

requestDeleteEmailIdentity :: DeleteEmailIdentity -> TestTree
requestDeleteEmailIdentity =
  req
    "DeleteEmailIdentity"
    "fixture/DeleteEmailIdentity.yaml"

requestDeleteEmailIdentityPolicy :: DeleteEmailIdentityPolicy -> TestTree
requestDeleteEmailIdentityPolicy =
  req
    "DeleteEmailIdentityPolicy"
    "fixture/DeleteEmailIdentityPolicy.yaml"

requestDeleteEmailTemplate :: DeleteEmailTemplate -> TestTree
requestDeleteEmailTemplate =
  req
    "DeleteEmailTemplate"
    "fixture/DeleteEmailTemplate.yaml"

requestDeleteSuppressedDestination :: DeleteSuppressedDestination -> TestTree
requestDeleteSuppressedDestination =
  req
    "DeleteSuppressedDestination"
    "fixture/DeleteSuppressedDestination.yaml"

requestGetAccount :: GetAccount -> TestTree
requestGetAccount =
  req
    "GetAccount"
    "fixture/GetAccount.yaml"

requestGetBlacklistReports :: GetBlacklistReports -> TestTree
requestGetBlacklistReports =
  req
    "GetBlacklistReports"
    "fixture/GetBlacklistReports.yaml"

requestGetConfigurationSet :: GetConfigurationSet -> TestTree
requestGetConfigurationSet =
  req
    "GetConfigurationSet"
    "fixture/GetConfigurationSet.yaml"

requestGetConfigurationSetEventDestinations :: GetConfigurationSetEventDestinations -> TestTree
requestGetConfigurationSetEventDestinations =
  req
    "GetConfigurationSetEventDestinations"
    "fixture/GetConfigurationSetEventDestinations.yaml"

requestGetContact :: GetContact -> TestTree
requestGetContact =
  req
    "GetContact"
    "fixture/GetContact.yaml"

requestGetContactList :: GetContactList -> TestTree
requestGetContactList =
  req
    "GetContactList"
    "fixture/GetContactList.yaml"

requestGetCustomVerificationEmailTemplate :: GetCustomVerificationEmailTemplate -> TestTree
requestGetCustomVerificationEmailTemplate =
  req
    "GetCustomVerificationEmailTemplate"
    "fixture/GetCustomVerificationEmailTemplate.yaml"

requestGetDedicatedIp :: GetDedicatedIp -> TestTree
requestGetDedicatedIp =
  req
    "GetDedicatedIp"
    "fixture/GetDedicatedIp.yaml"

requestGetDedicatedIpPool :: GetDedicatedIpPool -> TestTree
requestGetDedicatedIpPool =
  req
    "GetDedicatedIpPool"
    "fixture/GetDedicatedIpPool.yaml"

requestGetDedicatedIps :: GetDedicatedIps -> TestTree
requestGetDedicatedIps =
  req
    "GetDedicatedIps"
    "fixture/GetDedicatedIps.yaml"

requestGetDeliverabilityDashboardOptions :: GetDeliverabilityDashboardOptions -> TestTree
requestGetDeliverabilityDashboardOptions =
  req
    "GetDeliverabilityDashboardOptions"
    "fixture/GetDeliverabilityDashboardOptions.yaml"

requestGetDeliverabilityTestReport :: GetDeliverabilityTestReport -> TestTree
requestGetDeliverabilityTestReport =
  req
    "GetDeliverabilityTestReport"
    "fixture/GetDeliverabilityTestReport.yaml"

requestGetDomainDeliverabilityCampaign :: GetDomainDeliverabilityCampaign -> TestTree
requestGetDomainDeliverabilityCampaign =
  req
    "GetDomainDeliverabilityCampaign"
    "fixture/GetDomainDeliverabilityCampaign.yaml"

requestGetDomainStatisticsReport :: GetDomainStatisticsReport -> TestTree
requestGetDomainStatisticsReport =
  req
    "GetDomainStatisticsReport"
    "fixture/GetDomainStatisticsReport.yaml"

requestGetEmailIdentity :: GetEmailIdentity -> TestTree
requestGetEmailIdentity =
  req
    "GetEmailIdentity"
    "fixture/GetEmailIdentity.yaml"

requestGetEmailIdentityPolicies :: GetEmailIdentityPolicies -> TestTree
requestGetEmailIdentityPolicies =
  req
    "GetEmailIdentityPolicies"
    "fixture/GetEmailIdentityPolicies.yaml"

requestGetEmailTemplate :: GetEmailTemplate -> TestTree
requestGetEmailTemplate =
  req
    "GetEmailTemplate"
    "fixture/GetEmailTemplate.yaml"

requestGetImportJob :: GetImportJob -> TestTree
requestGetImportJob =
  req
    "GetImportJob"
    "fixture/GetImportJob.yaml"

requestGetSuppressedDestination :: GetSuppressedDestination -> TestTree
requestGetSuppressedDestination =
  req
    "GetSuppressedDestination"
    "fixture/GetSuppressedDestination.yaml"

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

requestListContacts :: ListContacts -> TestTree
requestListContacts =
  req
    "ListContacts"
    "fixture/ListContacts.yaml"

requestListCustomVerificationEmailTemplates :: ListCustomVerificationEmailTemplates -> TestTree
requestListCustomVerificationEmailTemplates =
  req
    "ListCustomVerificationEmailTemplates"
    "fixture/ListCustomVerificationEmailTemplates.yaml"

requestListDedicatedIpPools :: ListDedicatedIpPools -> TestTree
requestListDedicatedIpPools =
  req
    "ListDedicatedIpPools"
    "fixture/ListDedicatedIpPools.yaml"

requestListDeliverabilityTestReports :: ListDeliverabilityTestReports -> TestTree
requestListDeliverabilityTestReports =
  req
    "ListDeliverabilityTestReports"
    "fixture/ListDeliverabilityTestReports.yaml"

requestListDomainDeliverabilityCampaigns :: ListDomainDeliverabilityCampaigns -> TestTree
requestListDomainDeliverabilityCampaigns =
  req
    "ListDomainDeliverabilityCampaigns"
    "fixture/ListDomainDeliverabilityCampaigns.yaml"

requestListEmailIdentities :: ListEmailIdentities -> TestTree
requestListEmailIdentities =
  req
    "ListEmailIdentities"
    "fixture/ListEmailIdentities.yaml"

requestListEmailTemplates :: ListEmailTemplates -> TestTree
requestListEmailTemplates =
  req
    "ListEmailTemplates"
    "fixture/ListEmailTemplates.yaml"

requestListImportJobs :: ListImportJobs -> TestTree
requestListImportJobs =
  req
    "ListImportJobs"
    "fixture/ListImportJobs.yaml"

requestListRecommendations :: ListRecommendations -> TestTree
requestListRecommendations =
  req
    "ListRecommendations"
    "fixture/ListRecommendations.yaml"

requestListSuppressedDestinations :: ListSuppressedDestinations -> TestTree
requestListSuppressedDestinations =
  req
    "ListSuppressedDestinations"
    "fixture/ListSuppressedDestinations.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutAccountDedicatedIpWarmupAttributes :: PutAccountDedicatedIpWarmupAttributes -> TestTree
requestPutAccountDedicatedIpWarmupAttributes =
  req
    "PutAccountDedicatedIpWarmupAttributes"
    "fixture/PutAccountDedicatedIpWarmupAttributes.yaml"

requestPutAccountDetails :: PutAccountDetails -> TestTree
requestPutAccountDetails =
  req
    "PutAccountDetails"
    "fixture/PutAccountDetails.yaml"

requestPutAccountSendingAttributes :: PutAccountSendingAttributes -> TestTree
requestPutAccountSendingAttributes =
  req
    "PutAccountSendingAttributes"
    "fixture/PutAccountSendingAttributes.yaml"

requestPutAccountSuppressionAttributes :: PutAccountSuppressionAttributes -> TestTree
requestPutAccountSuppressionAttributes =
  req
    "PutAccountSuppressionAttributes"
    "fixture/PutAccountSuppressionAttributes.yaml"

requestPutAccountVdmAttributes :: PutAccountVdmAttributes -> TestTree
requestPutAccountVdmAttributes =
  req
    "PutAccountVdmAttributes"
    "fixture/PutAccountVdmAttributes.yaml"

requestPutConfigurationSetDeliveryOptions :: PutConfigurationSetDeliveryOptions -> TestTree
requestPutConfigurationSetDeliveryOptions =
  req
    "PutConfigurationSetDeliveryOptions"
    "fixture/PutConfigurationSetDeliveryOptions.yaml"

requestPutConfigurationSetReputationOptions :: PutConfigurationSetReputationOptions -> TestTree
requestPutConfigurationSetReputationOptions =
  req
    "PutConfigurationSetReputationOptions"
    "fixture/PutConfigurationSetReputationOptions.yaml"

requestPutConfigurationSetSendingOptions :: PutConfigurationSetSendingOptions -> TestTree
requestPutConfigurationSetSendingOptions =
  req
    "PutConfigurationSetSendingOptions"
    "fixture/PutConfigurationSetSendingOptions.yaml"

requestPutConfigurationSetSuppressionOptions :: PutConfigurationSetSuppressionOptions -> TestTree
requestPutConfigurationSetSuppressionOptions =
  req
    "PutConfigurationSetSuppressionOptions"
    "fixture/PutConfigurationSetSuppressionOptions.yaml"

requestPutConfigurationSetTrackingOptions :: PutConfigurationSetTrackingOptions -> TestTree
requestPutConfigurationSetTrackingOptions =
  req
    "PutConfigurationSetTrackingOptions"
    "fixture/PutConfigurationSetTrackingOptions.yaml"

requestPutConfigurationSetVdmOptions :: PutConfigurationSetVdmOptions -> TestTree
requestPutConfigurationSetVdmOptions =
  req
    "PutConfigurationSetVdmOptions"
    "fixture/PutConfigurationSetVdmOptions.yaml"

requestPutDedicatedIpInPool :: PutDedicatedIpInPool -> TestTree
requestPutDedicatedIpInPool =
  req
    "PutDedicatedIpInPool"
    "fixture/PutDedicatedIpInPool.yaml"

requestPutDedicatedIpWarmupAttributes :: PutDedicatedIpWarmupAttributes -> TestTree
requestPutDedicatedIpWarmupAttributes =
  req
    "PutDedicatedIpWarmupAttributes"
    "fixture/PutDedicatedIpWarmupAttributes.yaml"

requestPutDeliverabilityDashboardOption :: PutDeliverabilityDashboardOption -> TestTree
requestPutDeliverabilityDashboardOption =
  req
    "PutDeliverabilityDashboardOption"
    "fixture/PutDeliverabilityDashboardOption.yaml"

requestPutEmailIdentityConfigurationSetAttributes :: PutEmailIdentityConfigurationSetAttributes -> TestTree
requestPutEmailIdentityConfigurationSetAttributes =
  req
    "PutEmailIdentityConfigurationSetAttributes"
    "fixture/PutEmailIdentityConfigurationSetAttributes.yaml"

requestPutEmailIdentityDkimAttributes :: PutEmailIdentityDkimAttributes -> TestTree
requestPutEmailIdentityDkimAttributes =
  req
    "PutEmailIdentityDkimAttributes"
    "fixture/PutEmailIdentityDkimAttributes.yaml"

requestPutEmailIdentityDkimSigningAttributes :: PutEmailIdentityDkimSigningAttributes -> TestTree
requestPutEmailIdentityDkimSigningAttributes =
  req
    "PutEmailIdentityDkimSigningAttributes"
    "fixture/PutEmailIdentityDkimSigningAttributes.yaml"

requestPutEmailIdentityFeedbackAttributes :: PutEmailIdentityFeedbackAttributes -> TestTree
requestPutEmailIdentityFeedbackAttributes =
  req
    "PutEmailIdentityFeedbackAttributes"
    "fixture/PutEmailIdentityFeedbackAttributes.yaml"

requestPutEmailIdentityMailFromAttributes :: PutEmailIdentityMailFromAttributes -> TestTree
requestPutEmailIdentityMailFromAttributes =
  req
    "PutEmailIdentityMailFromAttributes"
    "fixture/PutEmailIdentityMailFromAttributes.yaml"

requestPutSuppressedDestination :: PutSuppressedDestination -> TestTree
requestPutSuppressedDestination =
  req
    "PutSuppressedDestination"
    "fixture/PutSuppressedDestination.yaml"

requestSendBulkEmail :: SendBulkEmail -> TestTree
requestSendBulkEmail =
  req
    "SendBulkEmail"
    "fixture/SendBulkEmail.yaml"

requestSendCustomVerificationEmail :: SendCustomVerificationEmail -> TestTree
requestSendCustomVerificationEmail =
  req
    "SendCustomVerificationEmail"
    "fixture/SendCustomVerificationEmail.yaml"

requestSendEmail :: SendEmail -> TestTree
requestSendEmail =
  req
    "SendEmail"
    "fixture/SendEmail.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTestRenderEmailTemplate :: TestRenderEmailTemplate -> TestTree
requestTestRenderEmailTemplate =
  req
    "TestRenderEmailTemplate"
    "fixture/TestRenderEmailTemplate.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateConfigurationSetEventDestination :: UpdateConfigurationSetEventDestination -> TestTree
requestUpdateConfigurationSetEventDestination =
  req
    "UpdateConfigurationSetEventDestination"
    "fixture/UpdateConfigurationSetEventDestination.yaml"

requestUpdateContact :: UpdateContact -> TestTree
requestUpdateContact =
  req
    "UpdateContact"
    "fixture/UpdateContact.yaml"

requestUpdateContactList :: UpdateContactList -> TestTree
requestUpdateContactList =
  req
    "UpdateContactList"
    "fixture/UpdateContactList.yaml"

requestUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplate -> TestTree
requestUpdateCustomVerificationEmailTemplate =
  req
    "UpdateCustomVerificationEmailTemplate"
    "fixture/UpdateCustomVerificationEmailTemplate.yaml"

requestUpdateEmailIdentityPolicy :: UpdateEmailIdentityPolicy -> TestTree
requestUpdateEmailIdentityPolicy =
  req
    "UpdateEmailIdentityPolicy"
    "fixture/UpdateEmailIdentityPolicy.yaml"

requestUpdateEmailTemplate :: UpdateEmailTemplate -> TestTree
requestUpdateEmailTemplate =
  req
    "UpdateEmailTemplate"
    "fixture/UpdateEmailTemplate.yaml"

-- Responses

responseBatchGetMetricData :: BatchGetMetricDataResponse -> TestTree
responseBatchGetMetricData =
  res
    "BatchGetMetricDataResponse"
    "fixture/BatchGetMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetMetricData)

responseCreateConfigurationSet :: CreateConfigurationSetResponse -> TestTree
responseCreateConfigurationSet =
  res
    "CreateConfigurationSetResponse"
    "fixture/CreateConfigurationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConfigurationSet)

responseCreateConfigurationSetEventDestination :: CreateConfigurationSetEventDestinationResponse -> TestTree
responseCreateConfigurationSetEventDestination =
  res
    "CreateConfigurationSetEventDestinationResponse"
    "fixture/CreateConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConfigurationSetEventDestination)

responseCreateContact :: CreateContactResponse -> TestTree
responseCreateContact =
  res
    "CreateContactResponse"
    "fixture/CreateContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContact)

responseCreateContactList :: CreateContactListResponse -> TestTree
responseCreateContactList =
  res
    "CreateContactListResponse"
    "fixture/CreateContactListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContactList)

responseCreateCustomVerificationEmailTemplate :: CreateCustomVerificationEmailTemplateResponse -> TestTree
responseCreateCustomVerificationEmailTemplate =
  res
    "CreateCustomVerificationEmailTemplateResponse"
    "fixture/CreateCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomVerificationEmailTemplate)

responseCreateDedicatedIpPool :: CreateDedicatedIpPoolResponse -> TestTree
responseCreateDedicatedIpPool =
  res
    "CreateDedicatedIpPoolResponse"
    "fixture/CreateDedicatedIpPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDedicatedIpPool)

responseCreateDeliverabilityTestReport :: CreateDeliverabilityTestReportResponse -> TestTree
responseCreateDeliverabilityTestReport =
  res
    "CreateDeliverabilityTestReportResponse"
    "fixture/CreateDeliverabilityTestReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeliverabilityTestReport)

responseCreateEmailIdentity :: CreateEmailIdentityResponse -> TestTree
responseCreateEmailIdentity =
  res
    "CreateEmailIdentityResponse"
    "fixture/CreateEmailIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEmailIdentity)

responseCreateEmailIdentityPolicy :: CreateEmailIdentityPolicyResponse -> TestTree
responseCreateEmailIdentityPolicy =
  res
    "CreateEmailIdentityPolicyResponse"
    "fixture/CreateEmailIdentityPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEmailIdentityPolicy)

responseCreateEmailTemplate :: CreateEmailTemplateResponse -> TestTree
responseCreateEmailTemplate =
  res
    "CreateEmailTemplateResponse"
    "fixture/CreateEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEmailTemplate)

responseCreateImportJob :: CreateImportJobResponse -> TestTree
responseCreateImportJob =
  res
    "CreateImportJobResponse"
    "fixture/CreateImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateImportJob)

responseDeleteConfigurationSet :: DeleteConfigurationSetResponse -> TestTree
responseDeleteConfigurationSet =
  res
    "DeleteConfigurationSetResponse"
    "fixture/DeleteConfigurationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfigurationSet)

responseDeleteConfigurationSetEventDestination :: DeleteConfigurationSetEventDestinationResponse -> TestTree
responseDeleteConfigurationSetEventDestination =
  res
    "DeleteConfigurationSetEventDestinationResponse"
    "fixture/DeleteConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfigurationSetEventDestination)

responseDeleteContact :: DeleteContactResponse -> TestTree
responseDeleteContact =
  res
    "DeleteContactResponse"
    "fixture/DeleteContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContact)

responseDeleteContactList :: DeleteContactListResponse -> TestTree
responseDeleteContactList =
  res
    "DeleteContactListResponse"
    "fixture/DeleteContactListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContactList)

responseDeleteCustomVerificationEmailTemplate :: DeleteCustomVerificationEmailTemplateResponse -> TestTree
responseDeleteCustomVerificationEmailTemplate =
  res
    "DeleteCustomVerificationEmailTemplateResponse"
    "fixture/DeleteCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomVerificationEmailTemplate)

responseDeleteDedicatedIpPool :: DeleteDedicatedIpPoolResponse -> TestTree
responseDeleteDedicatedIpPool =
  res
    "DeleteDedicatedIpPoolResponse"
    "fixture/DeleteDedicatedIpPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDedicatedIpPool)

responseDeleteEmailIdentity :: DeleteEmailIdentityResponse -> TestTree
responseDeleteEmailIdentity =
  res
    "DeleteEmailIdentityResponse"
    "fixture/DeleteEmailIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEmailIdentity)

responseDeleteEmailIdentityPolicy :: DeleteEmailIdentityPolicyResponse -> TestTree
responseDeleteEmailIdentityPolicy =
  res
    "DeleteEmailIdentityPolicyResponse"
    "fixture/DeleteEmailIdentityPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEmailIdentityPolicy)

responseDeleteEmailTemplate :: DeleteEmailTemplateResponse -> TestTree
responseDeleteEmailTemplate =
  res
    "DeleteEmailTemplateResponse"
    "fixture/DeleteEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEmailTemplate)

responseDeleteSuppressedDestination :: DeleteSuppressedDestinationResponse -> TestTree
responseDeleteSuppressedDestination =
  res
    "DeleteSuppressedDestinationResponse"
    "fixture/DeleteSuppressedDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSuppressedDestination)

responseGetAccount :: GetAccountResponse -> TestTree
responseGetAccount =
  res
    "GetAccountResponse"
    "fixture/GetAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccount)

responseGetBlacklistReports :: GetBlacklistReportsResponse -> TestTree
responseGetBlacklistReports =
  res
    "GetBlacklistReportsResponse"
    "fixture/GetBlacklistReportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBlacklistReports)

responseGetConfigurationSet :: GetConfigurationSetResponse -> TestTree
responseGetConfigurationSet =
  res
    "GetConfigurationSetResponse"
    "fixture/GetConfigurationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConfigurationSet)

responseGetConfigurationSetEventDestinations :: GetConfigurationSetEventDestinationsResponse -> TestTree
responseGetConfigurationSetEventDestinations =
  res
    "GetConfigurationSetEventDestinationsResponse"
    "fixture/GetConfigurationSetEventDestinationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConfigurationSetEventDestinations)

responseGetContact :: GetContactResponse -> TestTree
responseGetContact =
  res
    "GetContactResponse"
    "fixture/GetContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContact)

responseGetContactList :: GetContactListResponse -> TestTree
responseGetContactList =
  res
    "GetContactListResponse"
    "fixture/GetContactListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContactList)

responseGetCustomVerificationEmailTemplate :: GetCustomVerificationEmailTemplateResponse -> TestTree
responseGetCustomVerificationEmailTemplate =
  res
    "GetCustomVerificationEmailTemplateResponse"
    "fixture/GetCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCustomVerificationEmailTemplate)

responseGetDedicatedIp :: GetDedicatedIpResponse -> TestTree
responseGetDedicatedIp =
  res
    "GetDedicatedIpResponse"
    "fixture/GetDedicatedIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDedicatedIp)

responseGetDedicatedIpPool :: GetDedicatedIpPoolResponse -> TestTree
responseGetDedicatedIpPool =
  res
    "GetDedicatedIpPoolResponse"
    "fixture/GetDedicatedIpPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDedicatedIpPool)

responseGetDedicatedIps :: GetDedicatedIpsResponse -> TestTree
responseGetDedicatedIps =
  res
    "GetDedicatedIpsResponse"
    "fixture/GetDedicatedIpsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDedicatedIps)

responseGetDeliverabilityDashboardOptions :: GetDeliverabilityDashboardOptionsResponse -> TestTree
responseGetDeliverabilityDashboardOptions =
  res
    "GetDeliverabilityDashboardOptionsResponse"
    "fixture/GetDeliverabilityDashboardOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeliverabilityDashboardOptions)

responseGetDeliverabilityTestReport :: GetDeliverabilityTestReportResponse -> TestTree
responseGetDeliverabilityTestReport =
  res
    "GetDeliverabilityTestReportResponse"
    "fixture/GetDeliverabilityTestReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeliverabilityTestReport)

responseGetDomainDeliverabilityCampaign :: GetDomainDeliverabilityCampaignResponse -> TestTree
responseGetDomainDeliverabilityCampaign =
  res
    "GetDomainDeliverabilityCampaignResponse"
    "fixture/GetDomainDeliverabilityCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomainDeliverabilityCampaign)

responseGetDomainStatisticsReport :: GetDomainStatisticsReportResponse -> TestTree
responseGetDomainStatisticsReport =
  res
    "GetDomainStatisticsReportResponse"
    "fixture/GetDomainStatisticsReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomainStatisticsReport)

responseGetEmailIdentity :: GetEmailIdentityResponse -> TestTree
responseGetEmailIdentity =
  res
    "GetEmailIdentityResponse"
    "fixture/GetEmailIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEmailIdentity)

responseGetEmailIdentityPolicies :: GetEmailIdentityPoliciesResponse -> TestTree
responseGetEmailIdentityPolicies =
  res
    "GetEmailIdentityPoliciesResponse"
    "fixture/GetEmailIdentityPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEmailIdentityPolicies)

responseGetEmailTemplate :: GetEmailTemplateResponse -> TestTree
responseGetEmailTemplate =
  res
    "GetEmailTemplateResponse"
    "fixture/GetEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEmailTemplate)

responseGetImportJob :: GetImportJobResponse -> TestTree
responseGetImportJob =
  res
    "GetImportJobResponse"
    "fixture/GetImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetImportJob)

responseGetSuppressedDestination :: GetSuppressedDestinationResponse -> TestTree
responseGetSuppressedDestination =
  res
    "GetSuppressedDestinationResponse"
    "fixture/GetSuppressedDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSuppressedDestination)

responseListConfigurationSets :: ListConfigurationSetsResponse -> TestTree
responseListConfigurationSets =
  res
    "ListConfigurationSetsResponse"
    "fixture/ListConfigurationSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConfigurationSets)

responseListContactLists :: ListContactListsResponse -> TestTree
responseListContactLists =
  res
    "ListContactListsResponse"
    "fixture/ListContactListsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContactLists)

responseListContacts :: ListContactsResponse -> TestTree
responseListContacts =
  res
    "ListContactsResponse"
    "fixture/ListContactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContacts)

responseListCustomVerificationEmailTemplates :: ListCustomVerificationEmailTemplatesResponse -> TestTree
responseListCustomVerificationEmailTemplates =
  res
    "ListCustomVerificationEmailTemplatesResponse"
    "fixture/ListCustomVerificationEmailTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomVerificationEmailTemplates)

responseListDedicatedIpPools :: ListDedicatedIpPoolsResponse -> TestTree
responseListDedicatedIpPools =
  res
    "ListDedicatedIpPoolsResponse"
    "fixture/ListDedicatedIpPoolsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDedicatedIpPools)

responseListDeliverabilityTestReports :: ListDeliverabilityTestReportsResponse -> TestTree
responseListDeliverabilityTestReports =
  res
    "ListDeliverabilityTestReportsResponse"
    "fixture/ListDeliverabilityTestReportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeliverabilityTestReports)

responseListDomainDeliverabilityCampaigns :: ListDomainDeliverabilityCampaignsResponse -> TestTree
responseListDomainDeliverabilityCampaigns =
  res
    "ListDomainDeliverabilityCampaignsResponse"
    "fixture/ListDomainDeliverabilityCampaignsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomainDeliverabilityCampaigns)

responseListEmailIdentities :: ListEmailIdentitiesResponse -> TestTree
responseListEmailIdentities =
  res
    "ListEmailIdentitiesResponse"
    "fixture/ListEmailIdentitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEmailIdentities)

responseListEmailTemplates :: ListEmailTemplatesResponse -> TestTree
responseListEmailTemplates =
  res
    "ListEmailTemplatesResponse"
    "fixture/ListEmailTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEmailTemplates)

responseListImportJobs :: ListImportJobsResponse -> TestTree
responseListImportJobs =
  res
    "ListImportJobsResponse"
    "fixture/ListImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImportJobs)

responseListRecommendations :: ListRecommendationsResponse -> TestTree
responseListRecommendations =
  res
    "ListRecommendationsResponse"
    "fixture/ListRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecommendations)

responseListSuppressedDestinations :: ListSuppressedDestinationsResponse -> TestTree
responseListSuppressedDestinations =
  res
    "ListSuppressedDestinationsResponse"
    "fixture/ListSuppressedDestinationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSuppressedDestinations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutAccountDedicatedIpWarmupAttributes :: PutAccountDedicatedIpWarmupAttributesResponse -> TestTree
responsePutAccountDedicatedIpWarmupAttributes =
  res
    "PutAccountDedicatedIpWarmupAttributesResponse"
    "fixture/PutAccountDedicatedIpWarmupAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAccountDedicatedIpWarmupAttributes)

responsePutAccountDetails :: PutAccountDetailsResponse -> TestTree
responsePutAccountDetails =
  res
    "PutAccountDetailsResponse"
    "fixture/PutAccountDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAccountDetails)

responsePutAccountSendingAttributes :: PutAccountSendingAttributesResponse -> TestTree
responsePutAccountSendingAttributes =
  res
    "PutAccountSendingAttributesResponse"
    "fixture/PutAccountSendingAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAccountSendingAttributes)

responsePutAccountSuppressionAttributes :: PutAccountSuppressionAttributesResponse -> TestTree
responsePutAccountSuppressionAttributes =
  res
    "PutAccountSuppressionAttributesResponse"
    "fixture/PutAccountSuppressionAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAccountSuppressionAttributes)

responsePutAccountVdmAttributes :: PutAccountVdmAttributesResponse -> TestTree
responsePutAccountVdmAttributes =
  res
    "PutAccountVdmAttributesResponse"
    "fixture/PutAccountVdmAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAccountVdmAttributes)

responsePutConfigurationSetDeliveryOptions :: PutConfigurationSetDeliveryOptionsResponse -> TestTree
responsePutConfigurationSetDeliveryOptions =
  res
    "PutConfigurationSetDeliveryOptionsResponse"
    "fixture/PutConfigurationSetDeliveryOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConfigurationSetDeliveryOptions)

responsePutConfigurationSetReputationOptions :: PutConfigurationSetReputationOptionsResponse -> TestTree
responsePutConfigurationSetReputationOptions =
  res
    "PutConfigurationSetReputationOptionsResponse"
    "fixture/PutConfigurationSetReputationOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConfigurationSetReputationOptions)

responsePutConfigurationSetSendingOptions :: PutConfigurationSetSendingOptionsResponse -> TestTree
responsePutConfigurationSetSendingOptions =
  res
    "PutConfigurationSetSendingOptionsResponse"
    "fixture/PutConfigurationSetSendingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConfigurationSetSendingOptions)

responsePutConfigurationSetSuppressionOptions :: PutConfigurationSetSuppressionOptionsResponse -> TestTree
responsePutConfigurationSetSuppressionOptions =
  res
    "PutConfigurationSetSuppressionOptionsResponse"
    "fixture/PutConfigurationSetSuppressionOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConfigurationSetSuppressionOptions)

responsePutConfigurationSetTrackingOptions :: PutConfigurationSetTrackingOptionsResponse -> TestTree
responsePutConfigurationSetTrackingOptions =
  res
    "PutConfigurationSetTrackingOptionsResponse"
    "fixture/PutConfigurationSetTrackingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConfigurationSetTrackingOptions)

responsePutConfigurationSetVdmOptions :: PutConfigurationSetVdmOptionsResponse -> TestTree
responsePutConfigurationSetVdmOptions =
  res
    "PutConfigurationSetVdmOptionsResponse"
    "fixture/PutConfigurationSetVdmOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConfigurationSetVdmOptions)

responsePutDedicatedIpInPool :: PutDedicatedIpInPoolResponse -> TestTree
responsePutDedicatedIpInPool =
  res
    "PutDedicatedIpInPoolResponse"
    "fixture/PutDedicatedIpInPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDedicatedIpInPool)

responsePutDedicatedIpWarmupAttributes :: PutDedicatedIpWarmupAttributesResponse -> TestTree
responsePutDedicatedIpWarmupAttributes =
  res
    "PutDedicatedIpWarmupAttributesResponse"
    "fixture/PutDedicatedIpWarmupAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDedicatedIpWarmupAttributes)

responsePutDeliverabilityDashboardOption :: PutDeliverabilityDashboardOptionResponse -> TestTree
responsePutDeliverabilityDashboardOption =
  res
    "PutDeliverabilityDashboardOptionResponse"
    "fixture/PutDeliverabilityDashboardOptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDeliverabilityDashboardOption)

responsePutEmailIdentityConfigurationSetAttributes :: PutEmailIdentityConfigurationSetAttributesResponse -> TestTree
responsePutEmailIdentityConfigurationSetAttributes =
  res
    "PutEmailIdentityConfigurationSetAttributesResponse"
    "fixture/PutEmailIdentityConfigurationSetAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEmailIdentityConfigurationSetAttributes)

responsePutEmailIdentityDkimAttributes :: PutEmailIdentityDkimAttributesResponse -> TestTree
responsePutEmailIdentityDkimAttributes =
  res
    "PutEmailIdentityDkimAttributesResponse"
    "fixture/PutEmailIdentityDkimAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEmailIdentityDkimAttributes)

responsePutEmailIdentityDkimSigningAttributes :: PutEmailIdentityDkimSigningAttributesResponse -> TestTree
responsePutEmailIdentityDkimSigningAttributes =
  res
    "PutEmailIdentityDkimSigningAttributesResponse"
    "fixture/PutEmailIdentityDkimSigningAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEmailIdentityDkimSigningAttributes)

responsePutEmailIdentityFeedbackAttributes :: PutEmailIdentityFeedbackAttributesResponse -> TestTree
responsePutEmailIdentityFeedbackAttributes =
  res
    "PutEmailIdentityFeedbackAttributesResponse"
    "fixture/PutEmailIdentityFeedbackAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEmailIdentityFeedbackAttributes)

responsePutEmailIdentityMailFromAttributes :: PutEmailIdentityMailFromAttributesResponse -> TestTree
responsePutEmailIdentityMailFromAttributes =
  res
    "PutEmailIdentityMailFromAttributesResponse"
    "fixture/PutEmailIdentityMailFromAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEmailIdentityMailFromAttributes)

responsePutSuppressedDestination :: PutSuppressedDestinationResponse -> TestTree
responsePutSuppressedDestination =
  res
    "PutSuppressedDestinationResponse"
    "fixture/PutSuppressedDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSuppressedDestination)

responseSendBulkEmail :: SendBulkEmailResponse -> TestTree
responseSendBulkEmail =
  res
    "SendBulkEmailResponse"
    "fixture/SendBulkEmailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendBulkEmail)

responseSendCustomVerificationEmail :: SendCustomVerificationEmailResponse -> TestTree
responseSendCustomVerificationEmail =
  res
    "SendCustomVerificationEmailResponse"
    "fixture/SendCustomVerificationEmailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendCustomVerificationEmail)

responseSendEmail :: SendEmailResponse -> TestTree
responseSendEmail =
  res
    "SendEmailResponse"
    "fixture/SendEmailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendEmail)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTestRenderEmailTemplate :: TestRenderEmailTemplateResponse -> TestTree
responseTestRenderEmailTemplate =
  res
    "TestRenderEmailTemplateResponse"
    "fixture/TestRenderEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestRenderEmailTemplate)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateConfigurationSetEventDestination :: UpdateConfigurationSetEventDestinationResponse -> TestTree
responseUpdateConfigurationSetEventDestination =
  res
    "UpdateConfigurationSetEventDestinationResponse"
    "fixture/UpdateConfigurationSetEventDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConfigurationSetEventDestination)

responseUpdateContact :: UpdateContactResponse -> TestTree
responseUpdateContact =
  res
    "UpdateContactResponse"
    "fixture/UpdateContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContact)

responseUpdateContactList :: UpdateContactListResponse -> TestTree
responseUpdateContactList =
  res
    "UpdateContactListResponse"
    "fixture/UpdateContactListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContactList)

responseUpdateCustomVerificationEmailTemplate :: UpdateCustomVerificationEmailTemplateResponse -> TestTree
responseUpdateCustomVerificationEmailTemplate =
  res
    "UpdateCustomVerificationEmailTemplateResponse"
    "fixture/UpdateCustomVerificationEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCustomVerificationEmailTemplate)

responseUpdateEmailIdentityPolicy :: UpdateEmailIdentityPolicyResponse -> TestTree
responseUpdateEmailIdentityPolicy =
  res
    "UpdateEmailIdentityPolicyResponse"
    "fixture/UpdateEmailIdentityPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEmailIdentityPolicy)

responseUpdateEmailTemplate :: UpdateEmailTemplateResponse -> TestTree
responseUpdateEmailTemplate =
  res
    "UpdateEmailTemplateResponse"
    "fixture/UpdateEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEmailTemplate)
