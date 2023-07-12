{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.PinpointEmail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.PinpointEmail where

import Amazonka.PinpointEmail
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.PinpointEmail.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateConfigurationSet $
--             newCreateConfigurationSet
--
--         , requestCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestination
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
--         , requestDeleteConfigurationSet $
--             newDeleteConfigurationSet
--
--         , requestDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestination
--
--         , requestDeleteDedicatedIpPool $
--             newDeleteDedicatedIpPool
--
--         , requestDeleteEmailIdentity $
--             newDeleteEmailIdentity
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
--         , requestGetDedicatedIp $
--             newGetDedicatedIp
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
--         , requestListConfigurationSets $
--             newListConfigurationSets
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
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutAccountDedicatedIpWarmupAttributes $
--             newPutAccountDedicatedIpWarmupAttributes
--
--         , requestPutAccountSendingAttributes $
--             newPutAccountSendingAttributes
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
--         , requestPutConfigurationSetTrackingOptions $
--             newPutConfigurationSetTrackingOptions
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
--         , requestPutEmailIdentityDkimAttributes $
--             newPutEmailIdentityDkimAttributes
--
--         , requestPutEmailIdentityFeedbackAttributes $
--             newPutEmailIdentityFeedbackAttributes
--
--         , requestPutEmailIdentityMailFromAttributes $
--             newPutEmailIdentityMailFromAttributes
--
--         , requestSendEmail $
--             newSendEmail
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestination
--
--           ]

--     , testGroup "response"
--         [ responseCreateConfigurationSet $
--             newCreateConfigurationSetResponse
--
--         , responseCreateConfigurationSetEventDestination $
--             newCreateConfigurationSetEventDestinationResponse
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
--         , responseDeleteConfigurationSet $
--             newDeleteConfigurationSetResponse
--
--         , responseDeleteConfigurationSetEventDestination $
--             newDeleteConfigurationSetEventDestinationResponse
--
--         , responseDeleteDedicatedIpPool $
--             newDeleteDedicatedIpPoolResponse
--
--         , responseDeleteEmailIdentity $
--             newDeleteEmailIdentityResponse
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
--         , responseGetDedicatedIp $
--             newGetDedicatedIpResponse
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
--         , responseListConfigurationSets $
--             newListConfigurationSetsResponse
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
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutAccountDedicatedIpWarmupAttributes $
--             newPutAccountDedicatedIpWarmupAttributesResponse
--
--         , responsePutAccountSendingAttributes $
--             newPutAccountSendingAttributesResponse
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
--         , responsePutConfigurationSetTrackingOptions $
--             newPutConfigurationSetTrackingOptionsResponse
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
--         , responsePutEmailIdentityDkimAttributes $
--             newPutEmailIdentityDkimAttributesResponse
--
--         , responsePutEmailIdentityFeedbackAttributes $
--             newPutEmailIdentityFeedbackAttributesResponse
--
--         , responsePutEmailIdentityMailFromAttributes $
--             newPutEmailIdentityMailFromAttributesResponse
--
--         , responseSendEmail $
--             newSendEmailResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateConfigurationSetEventDestination $
--             newUpdateConfigurationSetEventDestinationResponse
--
--           ]
--     ]

-- Requests

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

requestGetDedicatedIp :: GetDedicatedIp -> TestTree
requestGetDedicatedIp =
  req
    "GetDedicatedIp"
    "fixture/GetDedicatedIp.yaml"

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

requestListConfigurationSets :: ListConfigurationSets -> TestTree
requestListConfigurationSets =
  req
    "ListConfigurationSets"
    "fixture/ListConfigurationSets.yaml"

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

requestPutAccountSendingAttributes :: PutAccountSendingAttributes -> TestTree
requestPutAccountSendingAttributes =
  req
    "PutAccountSendingAttributes"
    "fixture/PutAccountSendingAttributes.yaml"

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

requestPutConfigurationSetTrackingOptions :: PutConfigurationSetTrackingOptions -> TestTree
requestPutConfigurationSetTrackingOptions =
  req
    "PutConfigurationSetTrackingOptions"
    "fixture/PutConfigurationSetTrackingOptions.yaml"

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

requestPutEmailIdentityDkimAttributes :: PutEmailIdentityDkimAttributes -> TestTree
requestPutEmailIdentityDkimAttributes =
  req
    "PutEmailIdentityDkimAttributes"
    "fixture/PutEmailIdentityDkimAttributes.yaml"

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

-- Responses

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

responseGetDedicatedIp :: GetDedicatedIpResponse -> TestTree
responseGetDedicatedIp =
  res
    "GetDedicatedIpResponse"
    "fixture/GetDedicatedIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDedicatedIp)

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

responseListConfigurationSets :: ListConfigurationSetsResponse -> TestTree
responseListConfigurationSets =
  res
    "ListConfigurationSetsResponse"
    "fixture/ListConfigurationSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConfigurationSets)

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

responsePutAccountSendingAttributes :: PutAccountSendingAttributesResponse -> TestTree
responsePutAccountSendingAttributes =
  res
    "PutAccountSendingAttributesResponse"
    "fixture/PutAccountSendingAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAccountSendingAttributes)

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

responsePutConfigurationSetTrackingOptions :: PutConfigurationSetTrackingOptionsResponse -> TestTree
responsePutConfigurationSetTrackingOptions =
  res
    "PutConfigurationSetTrackingOptionsResponse"
    "fixture/PutConfigurationSetTrackingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConfigurationSetTrackingOptions)

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

responsePutEmailIdentityDkimAttributes :: PutEmailIdentityDkimAttributesResponse -> TestTree
responsePutEmailIdentityDkimAttributes =
  res
    "PutEmailIdentityDkimAttributesResponse"
    "fixture/PutEmailIdentityDkimAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEmailIdentityDkimAttributes)

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
