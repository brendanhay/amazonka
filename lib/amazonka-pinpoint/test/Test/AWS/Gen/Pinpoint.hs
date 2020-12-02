{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Pinpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Pinpoint where

import Data.Proxy
import Network.AWS.Pinpoint
import Test.AWS.Fixture
import Test.AWS.Pinpoint.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetGCMChannel $
--             getGCMChannel
--
--         , requestGetSegmentImportJobs $
--             getSegmentImportJobs
--
--         , requestSendMessages $
--             sendMessages
--
--         , requestGetImportJob $
--             getImportJob
--
--         , requestDeleteSmsTemplate $
--             deleteSmsTemplate
--
--         , requestUpdateSmsTemplate $
--             updateSmsTemplate
--
--         , requestGetAPNSVoipSandboxChannel $
--             getAPNSVoipSandboxChannel
--
--         , requestGetSegmentVersions $
--             getSegmentVersions
--
--         , requestDeleteCampaign $
--             deleteCampaign
--
--         , requestUpdateCampaign $
--             updateCampaign
--
--         , requestGetSegmentVersion $
--             getSegmentVersion
--
--         , requestDeletePushTemplate $
--             deletePushTemplate
--
--         , requestUpdatePushTemplate $
--             updatePushTemplate
--
--         , requestCreateExportJob $
--             createExportJob
--
--         , requestCreateSegment $
--             createSegment
--
--         , requestCreateRecommenderConfiguration $
--             createRecommenderConfiguration
--
--         , requestCreateVoiceTemplate $
--             createVoiceTemplate
--
--         , requestUpdateADMChannel $
--             updateADMChannel
--
--         , requestDeleteADMChannel $
--             deleteADMChannel
--
--         , requestDeleteRecommenderConfiguration $
--             deleteRecommenderConfiguration
--
--         , requestUpdateRecommenderConfiguration $
--             updateRecommenderConfiguration
--
--         , requestCreatePushTemplate $
--             createPushTemplate
--
--         , requestDeleteEndpoint $
--             deleteEndpoint
--
--         , requestUpdateEndpoint $
--             updateEndpoint
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestCreateCampaign $
--             createCampaign
--
--         , requestGetEmailTemplate $
--             getEmailTemplate
--
--         , requestGetExportJob $
--             getExportJob
--
--         , requestGetEndpoint $
--             getEndpoint
--
--         , requestGetSegment $
--             getSegment
--
--         , requestGetRecommenderConfiguration $
--             getRecommenderConfiguration
--
--         , requestUpdateEndpointsBatch $
--             updateEndpointsBatch
--
--         , requestGetADMChannel $
--             getADMChannel
--
--         , requestGetCampaign $
--             getCampaign
--
--         , requestGetVoiceTemplate $
--             getVoiceTemplate
--
--         , requestGetPushTemplate $
--             getPushTemplate
--
--         , requestDeleteUserEndpoints $
--             deleteUserEndpoints
--
--         , requestCreateEmailTemplate $
--             createEmailTemplate
--
--         , requestDeleteApp $
--             deleteApp
--
--         , requestUpdateAPNSVoipSandboxChannel $
--             updateAPNSVoipSandboxChannel
--
--         , requestDeleteAPNSVoipSandboxChannel $
--             deleteAPNSVoipSandboxChannel
--
--         , requestUpdateGCMChannel $
--             updateGCMChannel
--
--         , requestDeleteGCMChannel $
--             deleteGCMChannel
--
--         , requestGetCampaignActivities $
--             getCampaignActivities
--
--         , requestGetJourneyExecutionMetrics $
--             getJourneyExecutionMetrics
--
--         , requestUpdateJourneyState $
--             updateJourneyState
--
--         , requestGetEventStream $
--             getEventStream
--
--         , requestGetChannels $
--             getChannels
--
--         , requestGetJourney $
--             getJourney
--
--         , requestDeleteEmailChannel $
--             deleteEmailChannel
--
--         , requestUpdateEmailChannel $
--             updateEmailChannel
--
--         , requestGetBaiduChannel $
--             getBaiduChannel
--
--         , requestDeleteAPNSChannel $
--             deleteAPNSChannel
--
--         , requestUpdateAPNSChannel $
--             updateAPNSChannel
--
--         , requestRemoveAttributes $
--             removeAttributes
--
--         , requestListTemplates $
--             listTemplates
--
--         , requestPutEventStream $
--             putEventStream
--
--         , requestDeleteEventStream $
--             deleteEventStream
--
--         , requestGetCampaignVersions $
--             getCampaignVersions
--
--         , requestDeleteJourney $
--             deleteJourney
--
--         , requestUpdateJourney $
--             updateJourney
--
--         , requestGetCampaignDateRangeKpi $
--             getCampaignDateRangeKpi
--
--         , requestGetAPNSChannel $
--             getAPNSChannel
--
--         , requestUpdateVoiceChannel $
--             updateVoiceChannel
--
--         , requestDeleteVoiceChannel $
--             deleteVoiceChannel
--
--         , requestGetApps $
--             getApps
--
--         , requestGetAPNSSandboxChannel $
--             getAPNSSandboxChannel
--
--         , requestCreateJourney $
--             createJourney
--
--         , requestGetUserEndpoints $
--             getUserEndpoints
--
--         , requestDeleteVoiceTemplate $
--             deleteVoiceTemplate
--
--         , requestUpdateVoiceTemplate $
--             updateVoiceTemplate
--
--         , requestGetImportJobs $
--             getImportJobs
--
--         , requestGetJourneyDateRangeKpi $
--             getJourneyDateRangeKpi
--
--         , requestUpdateTemplateActiveVersion $
--             updateTemplateActiveVersion
--
--         , requestDeleteSmsChannel $
--             deleteSmsChannel
--
--         , requestUpdateSmsChannel $
--             updateSmsChannel
--
--         , requestGetApp $
--             getApp
--
--         , requestGetCampaignVersion $
--             getCampaignVersion
--
--         , requestDeleteSegment $
--             deleteSegment
--
--         , requestUpdateSegment $
--             updateSegment
--
--         , requestGetApplicationDateRangeKpi $
--             getApplicationDateRangeKpi
--
--         , requestCreateApp $
--             createApp
--
--         , requestGetSegmentExportJobs $
--             getSegmentExportJobs
--
--         , requestDeleteEmailTemplate $
--             deleteEmailTemplate
--
--         , requestUpdateEmailTemplate $
--             updateEmailTemplate
--
--         , requestGetSmsChannel $
--             getSmsChannel
--
--         , requestTagResource $
--             tagResource
--
--         , requestDeleteAPNSSandboxChannel $
--             deleteAPNSSandboxChannel
--
--         , requestUpdateAPNSSandboxChannel $
--             updateAPNSSandboxChannel
--
--         , requestGetCampaigns $
--             getCampaigns
--
--         , requestGetVoiceChannel $
--             getVoiceChannel
--
--         , requestUntagResource $
--             untagResource
--
--         , requestListTemplateVersions $
--             listTemplateVersions
--
--         , requestGetSmsTemplate $
--             getSmsTemplate
--
--         , requestPutEvents $
--             putEvents
--
--         , requestUpdateApplicationSettings $
--             updateApplicationSettings
--
--         , requestGetJourneyExecutionActivityMetrics $
--             getJourneyExecutionActivityMetrics
--
--         , requestGetSegments $
--             getSegments
--
--         , requestGetExportJobs $
--             getExportJobs
--
--         , requestCreateImportJob $
--             createImportJob
--
--         , requestGetRecommenderConfigurations $
--             getRecommenderConfigurations
--
--         , requestDeleteAPNSVoipChannel $
--             deleteAPNSVoipChannel
--
--         , requestUpdateAPNSVoipChannel $
--             updateAPNSVoipChannel
--
--         , requestSendUsersMessages $
--             sendUsersMessages
--
--         , requestGetApplicationSettings $
--             getApplicationSettings
--
--         , requestDeleteBaiduChannel $
--             deleteBaiduChannel
--
--         , requestUpdateBaiduChannel $
--             updateBaiduChannel
--
--         , requestCreateSmsTemplate $
--             createSmsTemplate
--
--         , requestPhoneNumberValidate $
--             phoneNumberValidate
--
--         , requestListJourneys $
--             listJourneys
--
--         , requestGetAPNSVoipChannel $
--             getAPNSVoipChannel
--
--         , requestGetEmailChannel $
--             getEmailChannel
--
--           ]

--     , testGroup "response"
--         [ responseGetGCMChannel $
--             getGCMChannelResponse
--
--         , responseGetSegmentImportJobs $
--             getSegmentImportJobsResponse
--
--         , responseSendMessages $
--             sendMessagesResponse
--
--         , responseGetImportJob $
--             getImportJobResponse
--
--         , responseDeleteSmsTemplate $
--             deleteSmsTemplateResponse
--
--         , responseUpdateSmsTemplate $
--             updateSmsTemplateResponse
--
--         , responseGetAPNSVoipSandboxChannel $
--             getAPNSVoipSandboxChannelResponse
--
--         , responseGetSegmentVersions $
--             getSegmentVersionsResponse
--
--         , responseDeleteCampaign $
--             deleteCampaignResponse
--
--         , responseUpdateCampaign $
--             updateCampaignResponse
--
--         , responseGetSegmentVersion $
--             getSegmentVersionResponse
--
--         , responseDeletePushTemplate $
--             deletePushTemplateResponse
--
--         , responseUpdatePushTemplate $
--             updatePushTemplateResponse
--
--         , responseCreateExportJob $
--             createExportJobResponse
--
--         , responseCreateSegment $
--             createSegmentResponse
--
--         , responseCreateRecommenderConfiguration $
--             createRecommenderConfigurationResponse
--
--         , responseCreateVoiceTemplate $
--             createVoiceTemplateResponse
--
--         , responseUpdateADMChannel $
--             updateADMChannelResponse
--
--         , responseDeleteADMChannel $
--             deleteADMChannelResponse
--
--         , responseDeleteRecommenderConfiguration $
--             deleteRecommenderConfigurationResponse
--
--         , responseUpdateRecommenderConfiguration $
--             updateRecommenderConfigurationResponse
--
--         , responseCreatePushTemplate $
--             createPushTemplateResponse
--
--         , responseDeleteEndpoint $
--             deleteEndpointResponse
--
--         , responseUpdateEndpoint $
--             updateEndpointResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseCreateCampaign $
--             createCampaignResponse
--
--         , responseGetEmailTemplate $
--             getEmailTemplateResponse
--
--         , responseGetExportJob $
--             getExportJobResponse
--
--         , responseGetEndpoint $
--             getEndpointResponse
--
--         , responseGetSegment $
--             getSegmentResponse
--
--         , responseGetRecommenderConfiguration $
--             getRecommenderConfigurationResponse
--
--         , responseUpdateEndpointsBatch $
--             updateEndpointsBatchResponse
--
--         , responseGetADMChannel $
--             getADMChannelResponse
--
--         , responseGetCampaign $
--             getCampaignResponse
--
--         , responseGetVoiceTemplate $
--             getVoiceTemplateResponse
--
--         , responseGetPushTemplate $
--             getPushTemplateResponse
--
--         , responseDeleteUserEndpoints $
--             deleteUserEndpointsResponse
--
--         , responseCreateEmailTemplate $
--             createEmailTemplateResponse
--
--         , responseDeleteApp $
--             deleteAppResponse
--
--         , responseUpdateAPNSVoipSandboxChannel $
--             updateAPNSVoipSandboxChannelResponse
--
--         , responseDeleteAPNSVoipSandboxChannel $
--             deleteAPNSVoipSandboxChannelResponse
--
--         , responseUpdateGCMChannel $
--             updateGCMChannelResponse
--
--         , responseDeleteGCMChannel $
--             deleteGCMChannelResponse
--
--         , responseGetCampaignActivities $
--             getCampaignActivitiesResponse
--
--         , responseGetJourneyExecutionMetrics $
--             getJourneyExecutionMetricsResponse
--
--         , responseUpdateJourneyState $
--             updateJourneyStateResponse
--
--         , responseGetEventStream $
--             getEventStreamResponse
--
--         , responseGetChannels $
--             getChannelsResponse
--
--         , responseGetJourney $
--             getJourneyResponse
--
--         , responseDeleteEmailChannel $
--             deleteEmailChannelResponse
--
--         , responseUpdateEmailChannel $
--             updateEmailChannelResponse
--
--         , responseGetBaiduChannel $
--             getBaiduChannelResponse
--
--         , responseDeleteAPNSChannel $
--             deleteAPNSChannelResponse
--
--         , responseUpdateAPNSChannel $
--             updateAPNSChannelResponse
--
--         , responseRemoveAttributes $
--             removeAttributesResponse
--
--         , responseListTemplates $
--             listTemplatesResponse
--
--         , responsePutEventStream $
--             putEventStreamResponse
--
--         , responseDeleteEventStream $
--             deleteEventStreamResponse
--
--         , responseGetCampaignVersions $
--             getCampaignVersionsResponse
--
--         , responseDeleteJourney $
--             deleteJourneyResponse
--
--         , responseUpdateJourney $
--             updateJourneyResponse
--
--         , responseGetCampaignDateRangeKpi $
--             getCampaignDateRangeKpiResponse
--
--         , responseGetAPNSChannel $
--             getAPNSChannelResponse
--
--         , responseUpdateVoiceChannel $
--             updateVoiceChannelResponse
--
--         , responseDeleteVoiceChannel $
--             deleteVoiceChannelResponse
--
--         , responseGetApps $
--             getAppsResponse
--
--         , responseGetAPNSSandboxChannel $
--             getAPNSSandboxChannelResponse
--
--         , responseCreateJourney $
--             createJourneyResponse
--
--         , responseGetUserEndpoints $
--             getUserEndpointsResponse
--
--         , responseDeleteVoiceTemplate $
--             deleteVoiceTemplateResponse
--
--         , responseUpdateVoiceTemplate $
--             updateVoiceTemplateResponse
--
--         , responseGetImportJobs $
--             getImportJobsResponse
--
--         , responseGetJourneyDateRangeKpi $
--             getJourneyDateRangeKpiResponse
--
--         , responseUpdateTemplateActiveVersion $
--             updateTemplateActiveVersionResponse
--
--         , responseDeleteSmsChannel $
--             deleteSmsChannelResponse
--
--         , responseUpdateSmsChannel $
--             updateSmsChannelResponse
--
--         , responseGetApp $
--             getAppResponse
--
--         , responseGetCampaignVersion $
--             getCampaignVersionResponse
--
--         , responseDeleteSegment $
--             deleteSegmentResponse
--
--         , responseUpdateSegment $
--             updateSegmentResponse
--
--         , responseGetApplicationDateRangeKpi $
--             getApplicationDateRangeKpiResponse
--
--         , responseCreateApp $
--             createAppResponse
--
--         , responseGetSegmentExportJobs $
--             getSegmentExportJobsResponse
--
--         , responseDeleteEmailTemplate $
--             deleteEmailTemplateResponse
--
--         , responseUpdateEmailTemplate $
--             updateEmailTemplateResponse
--
--         , responseGetSmsChannel $
--             getSmsChannelResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseDeleteAPNSSandboxChannel $
--             deleteAPNSSandboxChannelResponse
--
--         , responseUpdateAPNSSandboxChannel $
--             updateAPNSSandboxChannelResponse
--
--         , responseGetCampaigns $
--             getCampaignsResponse
--
--         , responseGetVoiceChannel $
--             getVoiceChannelResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseListTemplateVersions $
--             listTemplateVersionsResponse
--
--         , responseGetSmsTemplate $
--             getSmsTemplateResponse
--
--         , responsePutEvents $
--             putEventsResponse
--
--         , responseUpdateApplicationSettings $
--             updateApplicationSettingsResponse
--
--         , responseGetJourneyExecutionActivityMetrics $
--             getJourneyExecutionActivityMetricsResponse
--
--         , responseGetSegments $
--             getSegmentsResponse
--
--         , responseGetExportJobs $
--             getExportJobsResponse
--
--         , responseCreateImportJob $
--             createImportJobResponse
--
--         , responseGetRecommenderConfigurations $
--             getRecommenderConfigurationsResponse
--
--         , responseDeleteAPNSVoipChannel $
--             deleteAPNSVoipChannelResponse
--
--         , responseUpdateAPNSVoipChannel $
--             updateAPNSVoipChannelResponse
--
--         , responseSendUsersMessages $
--             sendUsersMessagesResponse
--
--         , responseGetApplicationSettings $
--             getApplicationSettingsResponse
--
--         , responseDeleteBaiduChannel $
--             deleteBaiduChannelResponse
--
--         , responseUpdateBaiduChannel $
--             updateBaiduChannelResponse
--
--         , responseCreateSmsTemplate $
--             createSmsTemplateResponse
--
--         , responsePhoneNumberValidate $
--             phoneNumberValidateResponse
--
--         , responseListJourneys $
--             listJourneysResponse
--
--         , responseGetAPNSVoipChannel $
--             getAPNSVoipChannelResponse
--
--         , responseGetEmailChannel $
--             getEmailChannelResponse
--
--           ]
--     ]

-- Requests

requestGetGCMChannel :: GetGCMChannel -> TestTree
requestGetGCMChannel =
  req
    "GetGCMChannel"
    "fixture/GetGCMChannel.yaml"

requestGetSegmentImportJobs :: GetSegmentImportJobs -> TestTree
requestGetSegmentImportJobs =
  req
    "GetSegmentImportJobs"
    "fixture/GetSegmentImportJobs.yaml"

requestSendMessages :: SendMessages -> TestTree
requestSendMessages =
  req
    "SendMessages"
    "fixture/SendMessages.yaml"

requestGetImportJob :: GetImportJob -> TestTree
requestGetImportJob =
  req
    "GetImportJob"
    "fixture/GetImportJob.yaml"

requestDeleteSmsTemplate :: DeleteSmsTemplate -> TestTree
requestDeleteSmsTemplate =
  req
    "DeleteSmsTemplate"
    "fixture/DeleteSmsTemplate.yaml"

requestUpdateSmsTemplate :: UpdateSmsTemplate -> TestTree
requestUpdateSmsTemplate =
  req
    "UpdateSmsTemplate"
    "fixture/UpdateSmsTemplate.yaml"

requestGetAPNSVoipSandboxChannel :: GetAPNSVoipSandboxChannel -> TestTree
requestGetAPNSVoipSandboxChannel =
  req
    "GetAPNSVoipSandboxChannel"
    "fixture/GetAPNSVoipSandboxChannel.yaml"

requestGetSegmentVersions :: GetSegmentVersions -> TestTree
requestGetSegmentVersions =
  req
    "GetSegmentVersions"
    "fixture/GetSegmentVersions.yaml"

requestDeleteCampaign :: DeleteCampaign -> TestTree
requestDeleteCampaign =
  req
    "DeleteCampaign"
    "fixture/DeleteCampaign.yaml"

requestUpdateCampaign :: UpdateCampaign -> TestTree
requestUpdateCampaign =
  req
    "UpdateCampaign"
    "fixture/UpdateCampaign.yaml"

requestGetSegmentVersion :: GetSegmentVersion -> TestTree
requestGetSegmentVersion =
  req
    "GetSegmentVersion"
    "fixture/GetSegmentVersion.yaml"

requestDeletePushTemplate :: DeletePushTemplate -> TestTree
requestDeletePushTemplate =
  req
    "DeletePushTemplate"
    "fixture/DeletePushTemplate.yaml"

requestUpdatePushTemplate :: UpdatePushTemplate -> TestTree
requestUpdatePushTemplate =
  req
    "UpdatePushTemplate"
    "fixture/UpdatePushTemplate.yaml"

requestCreateExportJob :: CreateExportJob -> TestTree
requestCreateExportJob =
  req
    "CreateExportJob"
    "fixture/CreateExportJob.yaml"

requestCreateSegment :: CreateSegment -> TestTree
requestCreateSegment =
  req
    "CreateSegment"
    "fixture/CreateSegment.yaml"

requestCreateRecommenderConfiguration :: CreateRecommenderConfiguration -> TestTree
requestCreateRecommenderConfiguration =
  req
    "CreateRecommenderConfiguration"
    "fixture/CreateRecommenderConfiguration.yaml"

requestCreateVoiceTemplate :: CreateVoiceTemplate -> TestTree
requestCreateVoiceTemplate =
  req
    "CreateVoiceTemplate"
    "fixture/CreateVoiceTemplate.yaml"

requestUpdateADMChannel :: UpdateADMChannel -> TestTree
requestUpdateADMChannel =
  req
    "UpdateADMChannel"
    "fixture/UpdateADMChannel.yaml"

requestDeleteADMChannel :: DeleteADMChannel -> TestTree
requestDeleteADMChannel =
  req
    "DeleteADMChannel"
    "fixture/DeleteADMChannel.yaml"

requestDeleteRecommenderConfiguration :: DeleteRecommenderConfiguration -> TestTree
requestDeleteRecommenderConfiguration =
  req
    "DeleteRecommenderConfiguration"
    "fixture/DeleteRecommenderConfiguration.yaml"

requestUpdateRecommenderConfiguration :: UpdateRecommenderConfiguration -> TestTree
requestUpdateRecommenderConfiguration =
  req
    "UpdateRecommenderConfiguration"
    "fixture/UpdateRecommenderConfiguration.yaml"

requestCreatePushTemplate :: CreatePushTemplate -> TestTree
requestCreatePushTemplate =
  req
    "CreatePushTemplate"
    "fixture/CreatePushTemplate.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint =
  req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestUpdateEndpoint :: UpdateEndpoint -> TestTree
requestUpdateEndpoint =
  req
    "UpdateEndpoint"
    "fixture/UpdateEndpoint.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateCampaign :: CreateCampaign -> TestTree
requestCreateCampaign =
  req
    "CreateCampaign"
    "fixture/CreateCampaign.yaml"

requestGetEmailTemplate :: GetEmailTemplate -> TestTree
requestGetEmailTemplate =
  req
    "GetEmailTemplate"
    "fixture/GetEmailTemplate.yaml"

requestGetExportJob :: GetExportJob -> TestTree
requestGetExportJob =
  req
    "GetExportJob"
    "fixture/GetExportJob.yaml"

requestGetEndpoint :: GetEndpoint -> TestTree
requestGetEndpoint =
  req
    "GetEndpoint"
    "fixture/GetEndpoint.yaml"

requestGetSegment :: GetSegment -> TestTree
requestGetSegment =
  req
    "GetSegment"
    "fixture/GetSegment.yaml"

requestGetRecommenderConfiguration :: GetRecommenderConfiguration -> TestTree
requestGetRecommenderConfiguration =
  req
    "GetRecommenderConfiguration"
    "fixture/GetRecommenderConfiguration.yaml"

requestUpdateEndpointsBatch :: UpdateEndpointsBatch -> TestTree
requestUpdateEndpointsBatch =
  req
    "UpdateEndpointsBatch"
    "fixture/UpdateEndpointsBatch.yaml"

requestGetADMChannel :: GetADMChannel -> TestTree
requestGetADMChannel =
  req
    "GetADMChannel"
    "fixture/GetADMChannel.yaml"

requestGetCampaign :: GetCampaign -> TestTree
requestGetCampaign =
  req
    "GetCampaign"
    "fixture/GetCampaign.yaml"

requestGetVoiceTemplate :: GetVoiceTemplate -> TestTree
requestGetVoiceTemplate =
  req
    "GetVoiceTemplate"
    "fixture/GetVoiceTemplate.yaml"

requestGetPushTemplate :: GetPushTemplate -> TestTree
requestGetPushTemplate =
  req
    "GetPushTemplate"
    "fixture/GetPushTemplate.yaml"

requestDeleteUserEndpoints :: DeleteUserEndpoints -> TestTree
requestDeleteUserEndpoints =
  req
    "DeleteUserEndpoints"
    "fixture/DeleteUserEndpoints.yaml"

requestCreateEmailTemplate :: CreateEmailTemplate -> TestTree
requestCreateEmailTemplate =
  req
    "CreateEmailTemplate"
    "fixture/CreateEmailTemplate.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestUpdateAPNSVoipSandboxChannel :: UpdateAPNSVoipSandboxChannel -> TestTree
requestUpdateAPNSVoipSandboxChannel =
  req
    "UpdateAPNSVoipSandboxChannel"
    "fixture/UpdateAPNSVoipSandboxChannel.yaml"

requestDeleteAPNSVoipSandboxChannel :: DeleteAPNSVoipSandboxChannel -> TestTree
requestDeleteAPNSVoipSandboxChannel =
  req
    "DeleteAPNSVoipSandboxChannel"
    "fixture/DeleteAPNSVoipSandboxChannel.yaml"

requestUpdateGCMChannel :: UpdateGCMChannel -> TestTree
requestUpdateGCMChannel =
  req
    "UpdateGCMChannel"
    "fixture/UpdateGCMChannel.yaml"

requestDeleteGCMChannel :: DeleteGCMChannel -> TestTree
requestDeleteGCMChannel =
  req
    "DeleteGCMChannel"
    "fixture/DeleteGCMChannel.yaml"

requestGetCampaignActivities :: GetCampaignActivities -> TestTree
requestGetCampaignActivities =
  req
    "GetCampaignActivities"
    "fixture/GetCampaignActivities.yaml"

requestGetJourneyExecutionMetrics :: GetJourneyExecutionMetrics -> TestTree
requestGetJourneyExecutionMetrics =
  req
    "GetJourneyExecutionMetrics"
    "fixture/GetJourneyExecutionMetrics.yaml"

requestUpdateJourneyState :: UpdateJourneyState -> TestTree
requestUpdateJourneyState =
  req
    "UpdateJourneyState"
    "fixture/UpdateJourneyState.yaml"

requestGetEventStream :: GetEventStream -> TestTree
requestGetEventStream =
  req
    "GetEventStream"
    "fixture/GetEventStream.yaml"

requestGetChannels :: GetChannels -> TestTree
requestGetChannels =
  req
    "GetChannels"
    "fixture/GetChannels.yaml"

requestGetJourney :: GetJourney -> TestTree
requestGetJourney =
  req
    "GetJourney"
    "fixture/GetJourney.yaml"

requestDeleteEmailChannel :: DeleteEmailChannel -> TestTree
requestDeleteEmailChannel =
  req
    "DeleteEmailChannel"
    "fixture/DeleteEmailChannel.yaml"

requestUpdateEmailChannel :: UpdateEmailChannel -> TestTree
requestUpdateEmailChannel =
  req
    "UpdateEmailChannel"
    "fixture/UpdateEmailChannel.yaml"

requestGetBaiduChannel :: GetBaiduChannel -> TestTree
requestGetBaiduChannel =
  req
    "GetBaiduChannel"
    "fixture/GetBaiduChannel.yaml"

requestDeleteAPNSChannel :: DeleteAPNSChannel -> TestTree
requestDeleteAPNSChannel =
  req
    "DeleteAPNSChannel"
    "fixture/DeleteAPNSChannel.yaml"

requestUpdateAPNSChannel :: UpdateAPNSChannel -> TestTree
requestUpdateAPNSChannel =
  req
    "UpdateAPNSChannel"
    "fixture/UpdateAPNSChannel.yaml"

requestRemoveAttributes :: RemoveAttributes -> TestTree
requestRemoveAttributes =
  req
    "RemoveAttributes"
    "fixture/RemoveAttributes.yaml"

requestListTemplates :: ListTemplates -> TestTree
requestListTemplates =
  req
    "ListTemplates"
    "fixture/ListTemplates.yaml"

requestPutEventStream :: PutEventStream -> TestTree
requestPutEventStream =
  req
    "PutEventStream"
    "fixture/PutEventStream.yaml"

requestDeleteEventStream :: DeleteEventStream -> TestTree
requestDeleteEventStream =
  req
    "DeleteEventStream"
    "fixture/DeleteEventStream.yaml"

requestGetCampaignVersions :: GetCampaignVersions -> TestTree
requestGetCampaignVersions =
  req
    "GetCampaignVersions"
    "fixture/GetCampaignVersions.yaml"

requestDeleteJourney :: DeleteJourney -> TestTree
requestDeleteJourney =
  req
    "DeleteJourney"
    "fixture/DeleteJourney.yaml"

requestUpdateJourney :: UpdateJourney -> TestTree
requestUpdateJourney =
  req
    "UpdateJourney"
    "fixture/UpdateJourney.yaml"

requestGetCampaignDateRangeKpi :: GetCampaignDateRangeKpi -> TestTree
requestGetCampaignDateRangeKpi =
  req
    "GetCampaignDateRangeKpi"
    "fixture/GetCampaignDateRangeKpi.yaml"

requestGetAPNSChannel :: GetAPNSChannel -> TestTree
requestGetAPNSChannel =
  req
    "GetAPNSChannel"
    "fixture/GetAPNSChannel.yaml"

requestUpdateVoiceChannel :: UpdateVoiceChannel -> TestTree
requestUpdateVoiceChannel =
  req
    "UpdateVoiceChannel"
    "fixture/UpdateVoiceChannel.yaml"

requestDeleteVoiceChannel :: DeleteVoiceChannel -> TestTree
requestDeleteVoiceChannel =
  req
    "DeleteVoiceChannel"
    "fixture/DeleteVoiceChannel.yaml"

requestGetApps :: GetApps -> TestTree
requestGetApps =
  req
    "GetApps"
    "fixture/GetApps.yaml"

requestGetAPNSSandboxChannel :: GetAPNSSandboxChannel -> TestTree
requestGetAPNSSandboxChannel =
  req
    "GetAPNSSandboxChannel"
    "fixture/GetAPNSSandboxChannel.yaml"

requestCreateJourney :: CreateJourney -> TestTree
requestCreateJourney =
  req
    "CreateJourney"
    "fixture/CreateJourney.yaml"

requestGetUserEndpoints :: GetUserEndpoints -> TestTree
requestGetUserEndpoints =
  req
    "GetUserEndpoints"
    "fixture/GetUserEndpoints.yaml"

requestDeleteVoiceTemplate :: DeleteVoiceTemplate -> TestTree
requestDeleteVoiceTemplate =
  req
    "DeleteVoiceTemplate"
    "fixture/DeleteVoiceTemplate.yaml"

requestUpdateVoiceTemplate :: UpdateVoiceTemplate -> TestTree
requestUpdateVoiceTemplate =
  req
    "UpdateVoiceTemplate"
    "fixture/UpdateVoiceTemplate.yaml"

requestGetImportJobs :: GetImportJobs -> TestTree
requestGetImportJobs =
  req
    "GetImportJobs"
    "fixture/GetImportJobs.yaml"

requestGetJourneyDateRangeKpi :: GetJourneyDateRangeKpi -> TestTree
requestGetJourneyDateRangeKpi =
  req
    "GetJourneyDateRangeKpi"
    "fixture/GetJourneyDateRangeKpi.yaml"

requestUpdateTemplateActiveVersion :: UpdateTemplateActiveVersion -> TestTree
requestUpdateTemplateActiveVersion =
  req
    "UpdateTemplateActiveVersion"
    "fixture/UpdateTemplateActiveVersion.yaml"

requestDeleteSmsChannel :: DeleteSmsChannel -> TestTree
requestDeleteSmsChannel =
  req
    "DeleteSmsChannel"
    "fixture/DeleteSmsChannel.yaml"

requestUpdateSmsChannel :: UpdateSmsChannel -> TestTree
requestUpdateSmsChannel =
  req
    "UpdateSmsChannel"
    "fixture/UpdateSmsChannel.yaml"

requestGetApp :: GetApp -> TestTree
requestGetApp =
  req
    "GetApp"
    "fixture/GetApp.yaml"

requestGetCampaignVersion :: GetCampaignVersion -> TestTree
requestGetCampaignVersion =
  req
    "GetCampaignVersion"
    "fixture/GetCampaignVersion.yaml"

requestDeleteSegment :: DeleteSegment -> TestTree
requestDeleteSegment =
  req
    "DeleteSegment"
    "fixture/DeleteSegment.yaml"

requestUpdateSegment :: UpdateSegment -> TestTree
requestUpdateSegment =
  req
    "UpdateSegment"
    "fixture/UpdateSegment.yaml"

requestGetApplicationDateRangeKpi :: GetApplicationDateRangeKpi -> TestTree
requestGetApplicationDateRangeKpi =
  req
    "GetApplicationDateRangeKpi"
    "fixture/GetApplicationDateRangeKpi.yaml"

requestCreateApp :: CreateApp -> TestTree
requestCreateApp =
  req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestGetSegmentExportJobs :: GetSegmentExportJobs -> TestTree
requestGetSegmentExportJobs =
  req
    "GetSegmentExportJobs"
    "fixture/GetSegmentExportJobs.yaml"

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

requestGetSmsChannel :: GetSmsChannel -> TestTree
requestGetSmsChannel =
  req
    "GetSmsChannel"
    "fixture/GetSmsChannel.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDeleteAPNSSandboxChannel :: DeleteAPNSSandboxChannel -> TestTree
requestDeleteAPNSSandboxChannel =
  req
    "DeleteAPNSSandboxChannel"
    "fixture/DeleteAPNSSandboxChannel.yaml"

requestUpdateAPNSSandboxChannel :: UpdateAPNSSandboxChannel -> TestTree
requestUpdateAPNSSandboxChannel =
  req
    "UpdateAPNSSandboxChannel"
    "fixture/UpdateAPNSSandboxChannel.yaml"

requestGetCampaigns :: GetCampaigns -> TestTree
requestGetCampaigns =
  req
    "GetCampaigns"
    "fixture/GetCampaigns.yaml"

requestGetVoiceChannel :: GetVoiceChannel -> TestTree
requestGetVoiceChannel =
  req
    "GetVoiceChannel"
    "fixture/GetVoiceChannel.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListTemplateVersions :: ListTemplateVersions -> TestTree
requestListTemplateVersions =
  req
    "ListTemplateVersions"
    "fixture/ListTemplateVersions.yaml"

requestGetSmsTemplate :: GetSmsTemplate -> TestTree
requestGetSmsTemplate =
  req
    "GetSmsTemplate"
    "fixture/GetSmsTemplate.yaml"

requestPutEvents :: PutEvents -> TestTree
requestPutEvents =
  req
    "PutEvents"
    "fixture/PutEvents.yaml"

requestUpdateApplicationSettings :: UpdateApplicationSettings -> TestTree
requestUpdateApplicationSettings =
  req
    "UpdateApplicationSettings"
    "fixture/UpdateApplicationSettings.yaml"

requestGetJourneyExecutionActivityMetrics :: GetJourneyExecutionActivityMetrics -> TestTree
requestGetJourneyExecutionActivityMetrics =
  req
    "GetJourneyExecutionActivityMetrics"
    "fixture/GetJourneyExecutionActivityMetrics.yaml"

requestGetSegments :: GetSegments -> TestTree
requestGetSegments =
  req
    "GetSegments"
    "fixture/GetSegments.yaml"

requestGetExportJobs :: GetExportJobs -> TestTree
requestGetExportJobs =
  req
    "GetExportJobs"
    "fixture/GetExportJobs.yaml"

requestCreateImportJob :: CreateImportJob -> TestTree
requestCreateImportJob =
  req
    "CreateImportJob"
    "fixture/CreateImportJob.yaml"

requestGetRecommenderConfigurations :: GetRecommenderConfigurations -> TestTree
requestGetRecommenderConfigurations =
  req
    "GetRecommenderConfigurations"
    "fixture/GetRecommenderConfigurations.yaml"

requestDeleteAPNSVoipChannel :: DeleteAPNSVoipChannel -> TestTree
requestDeleteAPNSVoipChannel =
  req
    "DeleteAPNSVoipChannel"
    "fixture/DeleteAPNSVoipChannel.yaml"

requestUpdateAPNSVoipChannel :: UpdateAPNSVoipChannel -> TestTree
requestUpdateAPNSVoipChannel =
  req
    "UpdateAPNSVoipChannel"
    "fixture/UpdateAPNSVoipChannel.yaml"

requestSendUsersMessages :: SendUsersMessages -> TestTree
requestSendUsersMessages =
  req
    "SendUsersMessages"
    "fixture/SendUsersMessages.yaml"

requestGetApplicationSettings :: GetApplicationSettings -> TestTree
requestGetApplicationSettings =
  req
    "GetApplicationSettings"
    "fixture/GetApplicationSettings.yaml"

requestDeleteBaiduChannel :: DeleteBaiduChannel -> TestTree
requestDeleteBaiduChannel =
  req
    "DeleteBaiduChannel"
    "fixture/DeleteBaiduChannel.yaml"

requestUpdateBaiduChannel :: UpdateBaiduChannel -> TestTree
requestUpdateBaiduChannel =
  req
    "UpdateBaiduChannel"
    "fixture/UpdateBaiduChannel.yaml"

requestCreateSmsTemplate :: CreateSmsTemplate -> TestTree
requestCreateSmsTemplate =
  req
    "CreateSmsTemplate"
    "fixture/CreateSmsTemplate.yaml"

requestPhoneNumberValidate :: PhoneNumberValidate -> TestTree
requestPhoneNumberValidate =
  req
    "PhoneNumberValidate"
    "fixture/PhoneNumberValidate.yaml"

requestListJourneys :: ListJourneys -> TestTree
requestListJourneys =
  req
    "ListJourneys"
    "fixture/ListJourneys.yaml"

requestGetAPNSVoipChannel :: GetAPNSVoipChannel -> TestTree
requestGetAPNSVoipChannel =
  req
    "GetAPNSVoipChannel"
    "fixture/GetAPNSVoipChannel.yaml"

requestGetEmailChannel :: GetEmailChannel -> TestTree
requestGetEmailChannel =
  req
    "GetEmailChannel"
    "fixture/GetEmailChannel.yaml"

-- Responses

responseGetGCMChannel :: GetGCMChannelResponse -> TestTree
responseGetGCMChannel =
  res
    "GetGCMChannelResponse"
    "fixture/GetGCMChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetGCMChannel)

responseGetSegmentImportJobs :: GetSegmentImportJobsResponse -> TestTree
responseGetSegmentImportJobs =
  res
    "GetSegmentImportJobsResponse"
    "fixture/GetSegmentImportJobsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetSegmentImportJobs)

responseSendMessages :: SendMessagesResponse -> TestTree
responseSendMessages =
  res
    "SendMessagesResponse"
    "fixture/SendMessagesResponse.proto"
    pinpoint
    (Proxy :: Proxy SendMessages)

responseGetImportJob :: GetImportJobResponse -> TestTree
responseGetImportJob =
  res
    "GetImportJobResponse"
    "fixture/GetImportJobResponse.proto"
    pinpoint
    (Proxy :: Proxy GetImportJob)

responseDeleteSmsTemplate :: DeleteSmsTemplateResponse -> TestTree
responseDeleteSmsTemplate =
  res
    "DeleteSmsTemplateResponse"
    "fixture/DeleteSmsTemplateResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteSmsTemplate)

responseUpdateSmsTemplate :: UpdateSmsTemplateResponse -> TestTree
responseUpdateSmsTemplate =
  res
    "UpdateSmsTemplateResponse"
    "fixture/UpdateSmsTemplateResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateSmsTemplate)

responseGetAPNSVoipSandboxChannel :: GetAPNSVoipSandboxChannelResponse -> TestTree
responseGetAPNSVoipSandboxChannel =
  res
    "GetAPNSVoipSandboxChannelResponse"
    "fixture/GetAPNSVoipSandboxChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetAPNSVoipSandboxChannel)

responseGetSegmentVersions :: GetSegmentVersionsResponse -> TestTree
responseGetSegmentVersions =
  res
    "GetSegmentVersionsResponse"
    "fixture/GetSegmentVersionsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetSegmentVersions)

responseDeleteCampaign :: DeleteCampaignResponse -> TestTree
responseDeleteCampaign =
  res
    "DeleteCampaignResponse"
    "fixture/DeleteCampaignResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteCampaign)

responseUpdateCampaign :: UpdateCampaignResponse -> TestTree
responseUpdateCampaign =
  res
    "UpdateCampaignResponse"
    "fixture/UpdateCampaignResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateCampaign)

responseGetSegmentVersion :: GetSegmentVersionResponse -> TestTree
responseGetSegmentVersion =
  res
    "GetSegmentVersionResponse"
    "fixture/GetSegmentVersionResponse.proto"
    pinpoint
    (Proxy :: Proxy GetSegmentVersion)

responseDeletePushTemplate :: DeletePushTemplateResponse -> TestTree
responseDeletePushTemplate =
  res
    "DeletePushTemplateResponse"
    "fixture/DeletePushTemplateResponse.proto"
    pinpoint
    (Proxy :: Proxy DeletePushTemplate)

responseUpdatePushTemplate :: UpdatePushTemplateResponse -> TestTree
responseUpdatePushTemplate =
  res
    "UpdatePushTemplateResponse"
    "fixture/UpdatePushTemplateResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdatePushTemplate)

responseCreateExportJob :: CreateExportJobResponse -> TestTree
responseCreateExportJob =
  res
    "CreateExportJobResponse"
    "fixture/CreateExportJobResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateExportJob)

responseCreateSegment :: CreateSegmentResponse -> TestTree
responseCreateSegment =
  res
    "CreateSegmentResponse"
    "fixture/CreateSegmentResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateSegment)

responseCreateRecommenderConfiguration :: CreateRecommenderConfigurationResponse -> TestTree
responseCreateRecommenderConfiguration =
  res
    "CreateRecommenderConfigurationResponse"
    "fixture/CreateRecommenderConfigurationResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateRecommenderConfiguration)

responseCreateVoiceTemplate :: CreateVoiceTemplateResponse -> TestTree
responseCreateVoiceTemplate =
  res
    "CreateVoiceTemplateResponse"
    "fixture/CreateVoiceTemplateResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateVoiceTemplate)

responseUpdateADMChannel :: UpdateADMChannelResponse -> TestTree
responseUpdateADMChannel =
  res
    "UpdateADMChannelResponse"
    "fixture/UpdateADMChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateADMChannel)

responseDeleteADMChannel :: DeleteADMChannelResponse -> TestTree
responseDeleteADMChannel =
  res
    "DeleteADMChannelResponse"
    "fixture/DeleteADMChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteADMChannel)

responseDeleteRecommenderConfiguration :: DeleteRecommenderConfigurationResponse -> TestTree
responseDeleteRecommenderConfiguration =
  res
    "DeleteRecommenderConfigurationResponse"
    "fixture/DeleteRecommenderConfigurationResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteRecommenderConfiguration)

responseUpdateRecommenderConfiguration :: UpdateRecommenderConfigurationResponse -> TestTree
responseUpdateRecommenderConfiguration =
  res
    "UpdateRecommenderConfigurationResponse"
    "fixture/UpdateRecommenderConfigurationResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateRecommenderConfiguration)

responseCreatePushTemplate :: CreatePushTemplateResponse -> TestTree
responseCreatePushTemplate =
  res
    "CreatePushTemplateResponse"
    "fixture/CreatePushTemplateResponse.proto"
    pinpoint
    (Proxy :: Proxy CreatePushTemplate)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteEndpoint)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateEndpoint)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    pinpoint
    (Proxy :: Proxy ListTagsForResource)

responseCreateCampaign :: CreateCampaignResponse -> TestTree
responseCreateCampaign =
  res
    "CreateCampaignResponse"
    "fixture/CreateCampaignResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateCampaign)

responseGetEmailTemplate :: GetEmailTemplateResponse -> TestTree
responseGetEmailTemplate =
  res
    "GetEmailTemplateResponse"
    "fixture/GetEmailTemplateResponse.proto"
    pinpoint
    (Proxy :: Proxy GetEmailTemplate)

responseGetExportJob :: GetExportJobResponse -> TestTree
responseGetExportJob =
  res
    "GetExportJobResponse"
    "fixture/GetExportJobResponse.proto"
    pinpoint
    (Proxy :: Proxy GetExportJob)

responseGetEndpoint :: GetEndpointResponse -> TestTree
responseGetEndpoint =
  res
    "GetEndpointResponse"
    "fixture/GetEndpointResponse.proto"
    pinpoint
    (Proxy :: Proxy GetEndpoint)

responseGetSegment :: GetSegmentResponse -> TestTree
responseGetSegment =
  res
    "GetSegmentResponse"
    "fixture/GetSegmentResponse.proto"
    pinpoint
    (Proxy :: Proxy GetSegment)

responseGetRecommenderConfiguration :: GetRecommenderConfigurationResponse -> TestTree
responseGetRecommenderConfiguration =
  res
    "GetRecommenderConfigurationResponse"
    "fixture/GetRecommenderConfigurationResponse.proto"
    pinpoint
    (Proxy :: Proxy GetRecommenderConfiguration)

responseUpdateEndpointsBatch :: UpdateEndpointsBatchResponse -> TestTree
responseUpdateEndpointsBatch =
  res
    "UpdateEndpointsBatchResponse"
    "fixture/UpdateEndpointsBatchResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateEndpointsBatch)

responseGetADMChannel :: GetADMChannelResponse -> TestTree
responseGetADMChannel =
  res
    "GetADMChannelResponse"
    "fixture/GetADMChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetADMChannel)

responseGetCampaign :: GetCampaignResponse -> TestTree
responseGetCampaign =
  res
    "GetCampaignResponse"
    "fixture/GetCampaignResponse.proto"
    pinpoint
    (Proxy :: Proxy GetCampaign)

responseGetVoiceTemplate :: GetVoiceTemplateResponse -> TestTree
responseGetVoiceTemplate =
  res
    "GetVoiceTemplateResponse"
    "fixture/GetVoiceTemplateResponse.proto"
    pinpoint
    (Proxy :: Proxy GetVoiceTemplate)

responseGetPushTemplate :: GetPushTemplateResponse -> TestTree
responseGetPushTemplate =
  res
    "GetPushTemplateResponse"
    "fixture/GetPushTemplateResponse.proto"
    pinpoint
    (Proxy :: Proxy GetPushTemplate)

responseDeleteUserEndpoints :: DeleteUserEndpointsResponse -> TestTree
responseDeleteUserEndpoints =
  res
    "DeleteUserEndpointsResponse"
    "fixture/DeleteUserEndpointsResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteUserEndpoints)

responseCreateEmailTemplate :: CreateEmailTemplateResponse -> TestTree
responseCreateEmailTemplate =
  res
    "CreateEmailTemplateResponse"
    "fixture/CreateEmailTemplateResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateEmailTemplate)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteApp)

responseUpdateAPNSVoipSandboxChannel :: UpdateAPNSVoipSandboxChannelResponse -> TestTree
responseUpdateAPNSVoipSandboxChannel =
  res
    "UpdateAPNSVoipSandboxChannelResponse"
    "fixture/UpdateAPNSVoipSandboxChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateAPNSVoipSandboxChannel)

responseDeleteAPNSVoipSandboxChannel :: DeleteAPNSVoipSandboxChannelResponse -> TestTree
responseDeleteAPNSVoipSandboxChannel =
  res
    "DeleteAPNSVoipSandboxChannelResponse"
    "fixture/DeleteAPNSVoipSandboxChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteAPNSVoipSandboxChannel)

responseUpdateGCMChannel :: UpdateGCMChannelResponse -> TestTree
responseUpdateGCMChannel =
  res
    "UpdateGCMChannelResponse"
    "fixture/UpdateGCMChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateGCMChannel)

responseDeleteGCMChannel :: DeleteGCMChannelResponse -> TestTree
responseDeleteGCMChannel =
  res
    "DeleteGCMChannelResponse"
    "fixture/DeleteGCMChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteGCMChannel)

responseGetCampaignActivities :: GetCampaignActivitiesResponse -> TestTree
responseGetCampaignActivities =
  res
    "GetCampaignActivitiesResponse"
    "fixture/GetCampaignActivitiesResponse.proto"
    pinpoint
    (Proxy :: Proxy GetCampaignActivities)

responseGetJourneyExecutionMetrics :: GetJourneyExecutionMetricsResponse -> TestTree
responseGetJourneyExecutionMetrics =
  res
    "GetJourneyExecutionMetricsResponse"
    "fixture/GetJourneyExecutionMetricsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetJourneyExecutionMetrics)

responseUpdateJourneyState :: UpdateJourneyStateResponse -> TestTree
responseUpdateJourneyState =
  res
    "UpdateJourneyStateResponse"
    "fixture/UpdateJourneyStateResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateJourneyState)

responseGetEventStream :: GetEventStreamResponse -> TestTree
responseGetEventStream =
  res
    "GetEventStreamResponse"
    "fixture/GetEventStreamResponse.proto"
    pinpoint
    (Proxy :: Proxy GetEventStream)

responseGetChannels :: GetChannelsResponse -> TestTree
responseGetChannels =
  res
    "GetChannelsResponse"
    "fixture/GetChannelsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetChannels)

responseGetJourney :: GetJourneyResponse -> TestTree
responseGetJourney =
  res
    "GetJourneyResponse"
    "fixture/GetJourneyResponse.proto"
    pinpoint
    (Proxy :: Proxy GetJourney)

responseDeleteEmailChannel :: DeleteEmailChannelResponse -> TestTree
responseDeleteEmailChannel =
  res
    "DeleteEmailChannelResponse"
    "fixture/DeleteEmailChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteEmailChannel)

responseUpdateEmailChannel :: UpdateEmailChannelResponse -> TestTree
responseUpdateEmailChannel =
  res
    "UpdateEmailChannelResponse"
    "fixture/UpdateEmailChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateEmailChannel)

responseGetBaiduChannel :: GetBaiduChannelResponse -> TestTree
responseGetBaiduChannel =
  res
    "GetBaiduChannelResponse"
    "fixture/GetBaiduChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetBaiduChannel)

responseDeleteAPNSChannel :: DeleteAPNSChannelResponse -> TestTree
responseDeleteAPNSChannel =
  res
    "DeleteAPNSChannelResponse"
    "fixture/DeleteAPNSChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteAPNSChannel)

responseUpdateAPNSChannel :: UpdateAPNSChannelResponse -> TestTree
responseUpdateAPNSChannel =
  res
    "UpdateAPNSChannelResponse"
    "fixture/UpdateAPNSChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateAPNSChannel)

responseRemoveAttributes :: RemoveAttributesResponse -> TestTree
responseRemoveAttributes =
  res
    "RemoveAttributesResponse"
    "fixture/RemoveAttributesResponse.proto"
    pinpoint
    (Proxy :: Proxy RemoveAttributes)

responseListTemplates :: ListTemplatesResponse -> TestTree
responseListTemplates =
  res
    "ListTemplatesResponse"
    "fixture/ListTemplatesResponse.proto"
    pinpoint
    (Proxy :: Proxy ListTemplates)

responsePutEventStream :: PutEventStreamResponse -> TestTree
responsePutEventStream =
  res
    "PutEventStreamResponse"
    "fixture/PutEventStreamResponse.proto"
    pinpoint
    (Proxy :: Proxy PutEventStream)

responseDeleteEventStream :: DeleteEventStreamResponse -> TestTree
responseDeleteEventStream =
  res
    "DeleteEventStreamResponse"
    "fixture/DeleteEventStreamResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteEventStream)

responseGetCampaignVersions :: GetCampaignVersionsResponse -> TestTree
responseGetCampaignVersions =
  res
    "GetCampaignVersionsResponse"
    "fixture/GetCampaignVersionsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetCampaignVersions)

responseDeleteJourney :: DeleteJourneyResponse -> TestTree
responseDeleteJourney =
  res
    "DeleteJourneyResponse"
    "fixture/DeleteJourneyResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteJourney)

responseUpdateJourney :: UpdateJourneyResponse -> TestTree
responseUpdateJourney =
  res
    "UpdateJourneyResponse"
    "fixture/UpdateJourneyResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateJourney)

responseGetCampaignDateRangeKpi :: GetCampaignDateRangeKpiResponse -> TestTree
responseGetCampaignDateRangeKpi =
  res
    "GetCampaignDateRangeKpiResponse"
    "fixture/GetCampaignDateRangeKpiResponse.proto"
    pinpoint
    (Proxy :: Proxy GetCampaignDateRangeKpi)

responseGetAPNSChannel :: GetAPNSChannelResponse -> TestTree
responseGetAPNSChannel =
  res
    "GetAPNSChannelResponse"
    "fixture/GetAPNSChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetAPNSChannel)

responseUpdateVoiceChannel :: UpdateVoiceChannelResponse -> TestTree
responseUpdateVoiceChannel =
  res
    "UpdateVoiceChannelResponse"
    "fixture/UpdateVoiceChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateVoiceChannel)

responseDeleteVoiceChannel :: DeleteVoiceChannelResponse -> TestTree
responseDeleteVoiceChannel =
  res
    "DeleteVoiceChannelResponse"
    "fixture/DeleteVoiceChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteVoiceChannel)

responseGetApps :: GetAppsResponse -> TestTree
responseGetApps =
  res
    "GetAppsResponse"
    "fixture/GetAppsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetApps)

responseGetAPNSSandboxChannel :: GetAPNSSandboxChannelResponse -> TestTree
responseGetAPNSSandboxChannel =
  res
    "GetAPNSSandboxChannelResponse"
    "fixture/GetAPNSSandboxChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetAPNSSandboxChannel)

responseCreateJourney :: CreateJourneyResponse -> TestTree
responseCreateJourney =
  res
    "CreateJourneyResponse"
    "fixture/CreateJourneyResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateJourney)

responseGetUserEndpoints :: GetUserEndpointsResponse -> TestTree
responseGetUserEndpoints =
  res
    "GetUserEndpointsResponse"
    "fixture/GetUserEndpointsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetUserEndpoints)

responseDeleteVoiceTemplate :: DeleteVoiceTemplateResponse -> TestTree
responseDeleteVoiceTemplate =
  res
    "DeleteVoiceTemplateResponse"
    "fixture/DeleteVoiceTemplateResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteVoiceTemplate)

responseUpdateVoiceTemplate :: UpdateVoiceTemplateResponse -> TestTree
responseUpdateVoiceTemplate =
  res
    "UpdateVoiceTemplateResponse"
    "fixture/UpdateVoiceTemplateResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateVoiceTemplate)

responseGetImportJobs :: GetImportJobsResponse -> TestTree
responseGetImportJobs =
  res
    "GetImportJobsResponse"
    "fixture/GetImportJobsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetImportJobs)

responseGetJourneyDateRangeKpi :: GetJourneyDateRangeKpiResponse -> TestTree
responseGetJourneyDateRangeKpi =
  res
    "GetJourneyDateRangeKpiResponse"
    "fixture/GetJourneyDateRangeKpiResponse.proto"
    pinpoint
    (Proxy :: Proxy GetJourneyDateRangeKpi)

responseUpdateTemplateActiveVersion :: UpdateTemplateActiveVersionResponse -> TestTree
responseUpdateTemplateActiveVersion =
  res
    "UpdateTemplateActiveVersionResponse"
    "fixture/UpdateTemplateActiveVersionResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateTemplateActiveVersion)

responseDeleteSmsChannel :: DeleteSmsChannelResponse -> TestTree
responseDeleteSmsChannel =
  res
    "DeleteSmsChannelResponse"
    "fixture/DeleteSmsChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteSmsChannel)

responseUpdateSmsChannel :: UpdateSmsChannelResponse -> TestTree
responseUpdateSmsChannel =
  res
    "UpdateSmsChannelResponse"
    "fixture/UpdateSmsChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateSmsChannel)

responseGetApp :: GetAppResponse -> TestTree
responseGetApp =
  res
    "GetAppResponse"
    "fixture/GetAppResponse.proto"
    pinpoint
    (Proxy :: Proxy GetApp)

responseGetCampaignVersion :: GetCampaignVersionResponse -> TestTree
responseGetCampaignVersion =
  res
    "GetCampaignVersionResponse"
    "fixture/GetCampaignVersionResponse.proto"
    pinpoint
    (Proxy :: Proxy GetCampaignVersion)

responseDeleteSegment :: DeleteSegmentResponse -> TestTree
responseDeleteSegment =
  res
    "DeleteSegmentResponse"
    "fixture/DeleteSegmentResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteSegment)

responseUpdateSegment :: UpdateSegmentResponse -> TestTree
responseUpdateSegment =
  res
    "UpdateSegmentResponse"
    "fixture/UpdateSegmentResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateSegment)

responseGetApplicationDateRangeKpi :: GetApplicationDateRangeKpiResponse -> TestTree
responseGetApplicationDateRangeKpi =
  res
    "GetApplicationDateRangeKpiResponse"
    "fixture/GetApplicationDateRangeKpiResponse.proto"
    pinpoint
    (Proxy :: Proxy GetApplicationDateRangeKpi)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateApp)

responseGetSegmentExportJobs :: GetSegmentExportJobsResponse -> TestTree
responseGetSegmentExportJobs =
  res
    "GetSegmentExportJobsResponse"
    "fixture/GetSegmentExportJobsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetSegmentExportJobs)

responseDeleteEmailTemplate :: DeleteEmailTemplateResponse -> TestTree
responseDeleteEmailTemplate =
  res
    "DeleteEmailTemplateResponse"
    "fixture/DeleteEmailTemplateResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteEmailTemplate)

responseUpdateEmailTemplate :: UpdateEmailTemplateResponse -> TestTree
responseUpdateEmailTemplate =
  res
    "UpdateEmailTemplateResponse"
    "fixture/UpdateEmailTemplateResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateEmailTemplate)

responseGetSmsChannel :: GetSmsChannelResponse -> TestTree
responseGetSmsChannel =
  res
    "GetSmsChannelResponse"
    "fixture/GetSmsChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetSmsChannel)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    pinpoint
    (Proxy :: Proxy TagResource)

responseDeleteAPNSSandboxChannel :: DeleteAPNSSandboxChannelResponse -> TestTree
responseDeleteAPNSSandboxChannel =
  res
    "DeleteAPNSSandboxChannelResponse"
    "fixture/DeleteAPNSSandboxChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteAPNSSandboxChannel)

responseUpdateAPNSSandboxChannel :: UpdateAPNSSandboxChannelResponse -> TestTree
responseUpdateAPNSSandboxChannel =
  res
    "UpdateAPNSSandboxChannelResponse"
    "fixture/UpdateAPNSSandboxChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateAPNSSandboxChannel)

responseGetCampaigns :: GetCampaignsResponse -> TestTree
responseGetCampaigns =
  res
    "GetCampaignsResponse"
    "fixture/GetCampaignsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetCampaigns)

responseGetVoiceChannel :: GetVoiceChannelResponse -> TestTree
responseGetVoiceChannel =
  res
    "GetVoiceChannelResponse"
    "fixture/GetVoiceChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetVoiceChannel)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    pinpoint
    (Proxy :: Proxy UntagResource)

responseListTemplateVersions :: ListTemplateVersionsResponse -> TestTree
responseListTemplateVersions =
  res
    "ListTemplateVersionsResponse"
    "fixture/ListTemplateVersionsResponse.proto"
    pinpoint
    (Proxy :: Proxy ListTemplateVersions)

responseGetSmsTemplate :: GetSmsTemplateResponse -> TestTree
responseGetSmsTemplate =
  res
    "GetSmsTemplateResponse"
    "fixture/GetSmsTemplateResponse.proto"
    pinpoint
    (Proxy :: Proxy GetSmsTemplate)

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents =
  res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    pinpoint
    (Proxy :: Proxy PutEvents)

responseUpdateApplicationSettings :: UpdateApplicationSettingsResponse -> TestTree
responseUpdateApplicationSettings =
  res
    "UpdateApplicationSettingsResponse"
    "fixture/UpdateApplicationSettingsResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateApplicationSettings)

responseGetJourneyExecutionActivityMetrics :: GetJourneyExecutionActivityMetricsResponse -> TestTree
responseGetJourneyExecutionActivityMetrics =
  res
    "GetJourneyExecutionActivityMetricsResponse"
    "fixture/GetJourneyExecutionActivityMetricsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetJourneyExecutionActivityMetrics)

responseGetSegments :: GetSegmentsResponse -> TestTree
responseGetSegments =
  res
    "GetSegmentsResponse"
    "fixture/GetSegmentsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetSegments)

responseGetExportJobs :: GetExportJobsResponse -> TestTree
responseGetExportJobs =
  res
    "GetExportJobsResponse"
    "fixture/GetExportJobsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetExportJobs)

responseCreateImportJob :: CreateImportJobResponse -> TestTree
responseCreateImportJob =
  res
    "CreateImportJobResponse"
    "fixture/CreateImportJobResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateImportJob)

responseGetRecommenderConfigurations :: GetRecommenderConfigurationsResponse -> TestTree
responseGetRecommenderConfigurations =
  res
    "GetRecommenderConfigurationsResponse"
    "fixture/GetRecommenderConfigurationsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetRecommenderConfigurations)

responseDeleteAPNSVoipChannel :: DeleteAPNSVoipChannelResponse -> TestTree
responseDeleteAPNSVoipChannel =
  res
    "DeleteAPNSVoipChannelResponse"
    "fixture/DeleteAPNSVoipChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteAPNSVoipChannel)

responseUpdateAPNSVoipChannel :: UpdateAPNSVoipChannelResponse -> TestTree
responseUpdateAPNSVoipChannel =
  res
    "UpdateAPNSVoipChannelResponse"
    "fixture/UpdateAPNSVoipChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateAPNSVoipChannel)

responseSendUsersMessages :: SendUsersMessagesResponse -> TestTree
responseSendUsersMessages =
  res
    "SendUsersMessagesResponse"
    "fixture/SendUsersMessagesResponse.proto"
    pinpoint
    (Proxy :: Proxy SendUsersMessages)

responseGetApplicationSettings :: GetApplicationSettingsResponse -> TestTree
responseGetApplicationSettings =
  res
    "GetApplicationSettingsResponse"
    "fixture/GetApplicationSettingsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetApplicationSettings)

responseDeleteBaiduChannel :: DeleteBaiduChannelResponse -> TestTree
responseDeleteBaiduChannel =
  res
    "DeleteBaiduChannelResponse"
    "fixture/DeleteBaiduChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteBaiduChannel)

responseUpdateBaiduChannel :: UpdateBaiduChannelResponse -> TestTree
responseUpdateBaiduChannel =
  res
    "UpdateBaiduChannelResponse"
    "fixture/UpdateBaiduChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateBaiduChannel)

responseCreateSmsTemplate :: CreateSmsTemplateResponse -> TestTree
responseCreateSmsTemplate =
  res
    "CreateSmsTemplateResponse"
    "fixture/CreateSmsTemplateResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateSmsTemplate)

responsePhoneNumberValidate :: PhoneNumberValidateResponse -> TestTree
responsePhoneNumberValidate =
  res
    "PhoneNumberValidateResponse"
    "fixture/PhoneNumberValidateResponse.proto"
    pinpoint
    (Proxy :: Proxy PhoneNumberValidate)

responseListJourneys :: ListJourneysResponse -> TestTree
responseListJourneys =
  res
    "ListJourneysResponse"
    "fixture/ListJourneysResponse.proto"
    pinpoint
    (Proxy :: Proxy ListJourneys)

responseGetAPNSVoipChannel :: GetAPNSVoipChannelResponse -> TestTree
responseGetAPNSVoipChannel =
  res
    "GetAPNSVoipChannelResponse"
    "fixture/GetAPNSVoipChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetAPNSVoipChannel)

responseGetEmailChannel :: GetEmailChannelResponse -> TestTree
responseGetEmailChannel =
  res
    "GetEmailChannelResponse"
    "fixture/GetEmailChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetEmailChannel)
