{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Pinpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Pinpoint where

import qualified Data.Proxy as Proxy
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
--         [ requestGetGcmChannel $
--             newGetGcmChannel
--
--         , requestGetSegmentImportJobs $
--             newGetSegmentImportJobs
--
--         , requestSendMessages $
--             newSendMessages
--
--         , requestGetImportJob $
--             newGetImportJob
--
--         , requestDeleteSmsTemplate $
--             newDeleteSmsTemplate
--
--         , requestUpdateSmsTemplate $
--             newUpdateSmsTemplate
--
--         , requestGetApnsVoipSandboxChannel $
--             newGetApnsVoipSandboxChannel
--
--         , requestGetSegmentVersions $
--             newGetSegmentVersions
--
--         , requestDeleteCampaign $
--             newDeleteCampaign
--
--         , requestUpdateCampaign $
--             newUpdateCampaign
--
--         , requestGetSegmentVersion $
--             newGetSegmentVersion
--
--         , requestDeletePushTemplate $
--             newDeletePushTemplate
--
--         , requestUpdatePushTemplate $
--             newUpdatePushTemplate
--
--         , requestCreateExportJob $
--             newCreateExportJob
--
--         , requestCreateSegment $
--             newCreateSegment
--
--         , requestCreateRecommenderConfiguration $
--             newCreateRecommenderConfiguration'
--
--         , requestCreateInAppTemplate $
--             newCreateInAppTemplate
--
--         , requestCreateVoiceTemplate $
--             newCreateVoiceTemplate
--
--         , requestUpdateAdmChannel $
--             newUpdateAdmChannel
--
--         , requestDeleteAdmChannel $
--             newDeleteAdmChannel
--
--         , requestDeleteRecommenderConfiguration $
--             newDeleteRecommenderConfiguration
--
--         , requestUpdateRecommenderConfiguration $
--             newUpdateRecommenderConfiguration'
--
--         , requestCreatePushTemplate $
--             newCreatePushTemplate
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestUpdateEndpoint $
--             newUpdateEndpoint
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateCampaign $
--             newCreateCampaign
--
--         , requestGetEmailTemplate $
--             newGetEmailTemplate
--
--         , requestGetExportJob $
--             newGetExportJob
--
--         , requestGetEndpoint $
--             newGetEndpoint
--
--         , requestGetSegment $
--             newGetSegment
--
--         , requestGetRecommenderConfiguration $
--             newGetRecommenderConfiguration
--
--         , requestUpdateEndpointsBatch $
--             newUpdateEndpointsBatch
--
--         , requestGetAdmChannel $
--             newGetAdmChannel
--
--         , requestGetCampaign $
--             newGetCampaign
--
--         , requestGetVoiceTemplate $
--             newGetVoiceTemplate
--
--         , requestGetInAppTemplate $
--             newGetInAppTemplate
--
--         , requestGetPushTemplate $
--             newGetPushTemplate
--
--         , requestDeleteUserEndpoints $
--             newDeleteUserEndpoints
--
--         , requestCreateEmailTemplate $
--             newCreateEmailTemplate
--
--         , requestGetInAppMessages $
--             newGetInAppMessages
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestUpdateApnsVoipSandboxChannel $
--             newUpdateApnsVoipSandboxChannel
--
--         , requestDeleteApnsVoipSandboxChannel $
--             newDeleteApnsVoipSandboxChannel
--
--         , requestUpdateGcmChannel $
--             newUpdateGcmChannel
--
--         , requestDeleteGcmChannel $
--             newDeleteGcmChannel
--
--         , requestGetCampaignActivities $
--             newGetCampaignActivities
--
--         , requestGetJourneyExecutionMetrics $
--             newGetJourneyExecutionMetrics
--
--         , requestUpdateJourneyState $
--             newUpdateJourneyState
--
--         , requestGetEventStream $
--             newGetEventStream
--
--         , requestGetChannels $
--             newGetChannels
--
--         , requestGetJourney $
--             newGetJourney
--
--         , requestDeleteEmailChannel $
--             newDeleteEmailChannel
--
--         , requestUpdateEmailChannel $
--             newUpdateEmailChannel
--
--         , requestGetBaiduChannel $
--             newGetBaiduChannel
--
--         , requestDeleteApnsChannel $
--             newDeleteApnsChannel
--
--         , requestUpdateApnsChannel $
--             newUpdateApnsChannel
--
--         , requestRemoveAttributes $
--             newRemoveAttributes
--
--         , requestListTemplates $
--             newListTemplates
--
--         , requestPutEventStream $
--             newPutEventStream
--
--         , requestDeleteEventStream $
--             newDeleteEventStream
--
--         , requestGetCampaignVersions $
--             newGetCampaignVersions
--
--         , requestDeleteJourney $
--             newDeleteJourney
--
--         , requestUpdateJourney $
--             newUpdateJourney
--
--         , requestGetCampaignDateRangeKpi $
--             newGetCampaignDateRangeKpi
--
--         , requestGetApnsChannel $
--             newGetApnsChannel
--
--         , requestUpdateVoiceChannel $
--             newUpdateVoiceChannel
--
--         , requestDeleteVoiceChannel $
--             newDeleteVoiceChannel
--
--         , requestGetApps $
--             newGetApps
--
--         , requestGetApnsSandboxChannel $
--             newGetApnsSandboxChannel
--
--         , requestCreateJourney $
--             newCreateJourney
--
--         , requestGetUserEndpoints $
--             newGetUserEndpoints
--
--         , requestDeleteVoiceTemplate $
--             newDeleteVoiceTemplate
--
--         , requestUpdateVoiceTemplate $
--             newUpdateVoiceTemplate
--
--         , requestDeleteInAppTemplate $
--             newDeleteInAppTemplate
--
--         , requestUpdateInAppTemplate $
--             newUpdateInAppTemplate
--
--         , requestGetImportJobs $
--             newGetImportJobs
--
--         , requestGetJourneyDateRangeKpi $
--             newGetJourneyDateRangeKpi
--
--         , requestUpdateTemplateActiveVersion $
--             newUpdateTemplateActiveVersion
--
--         , requestDeleteSmsChannel $
--             newDeleteSmsChannel
--
--         , requestUpdateSmsChannel $
--             newUpdateSmsChannel
--
--         , requestGetApp $
--             newGetApp
--
--         , requestGetCampaignVersion $
--             newGetCampaignVersion
--
--         , requestDeleteSegment $
--             newDeleteSegment
--
--         , requestUpdateSegment $
--             newUpdateSegment
--
--         , requestGetApplicationDateRangeKpi $
--             newGetApplicationDateRangeKpi
--
--         , requestCreateApp $
--             newCreateApp
--
--         , requestGetSegmentExportJobs $
--             newGetSegmentExportJobs
--
--         , requestDeleteEmailTemplate $
--             newDeleteEmailTemplate
--
--         , requestUpdateEmailTemplate $
--             newUpdateEmailTemplate
--
--         , requestGetSmsChannel $
--             newGetSmsChannel
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDeleteApnsSandboxChannel $
--             newDeleteApnsSandboxChannel
--
--         , requestUpdateApnsSandboxChannel $
--             newUpdateApnsSandboxChannel
--
--         , requestGetCampaigns $
--             newGetCampaigns
--
--         , requestGetVoiceChannel $
--             newGetVoiceChannel
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListTemplateVersions $
--             newListTemplateVersions
--
--         , requestGetSmsTemplate $
--             newGetSmsTemplate
--
--         , requestPutEvents $
--             newPutEvents
--
--         , requestUpdateApplicationSettings $
--             newUpdateApplicationSettings
--
--         , requestGetJourneyExecutionActivityMetrics $
--             newGetJourneyExecutionActivityMetrics
--
--         , requestGetSegments $
--             newGetSegments
--
--         , requestGetExportJobs $
--             newGetExportJobs
--
--         , requestCreateImportJob $
--             newCreateImportJob
--
--         , requestGetRecommenderConfigurations $
--             newGetRecommenderConfigurations
--
--         , requestDeleteApnsVoipChannel $
--             newDeleteApnsVoipChannel
--
--         , requestUpdateApnsVoipChannel $
--             newUpdateApnsVoipChannel
--
--         , requestSendUsersMessages $
--             newSendUsersMessages
--
--         , requestGetApplicationSettings $
--             newGetApplicationSettings
--
--         , requestDeleteBaiduChannel $
--             newDeleteBaiduChannel
--
--         , requestUpdateBaiduChannel $
--             newUpdateBaiduChannel
--
--         , requestCreateSmsTemplate $
--             newCreateSmsTemplate
--
--         , requestPhoneNumberValidate $
--             newPhoneNumberValidate
--
--         , requestListJourneys $
--             newListJourneys
--
--         , requestGetApnsVoipChannel $
--             newGetApnsVoipChannel
--
--         , requestGetEmailChannel $
--             newGetEmailChannel
--
--           ]

--     , testGroup "response"
--         [ responseGetGcmChannel $
--             newGetGcmChannelResponse
--
--         , responseGetSegmentImportJobs $
--             newGetSegmentImportJobsResponse
--
--         , responseSendMessages $
--             newSendMessagesResponse
--
--         , responseGetImportJob $
--             newGetImportJobResponse
--
--         , responseDeleteSmsTemplate $
--             newDeleteSmsTemplateResponse
--
--         , responseUpdateSmsTemplate $
--             newUpdateSmsTemplateResponse
--
--         , responseGetApnsVoipSandboxChannel $
--             newGetApnsVoipSandboxChannelResponse
--
--         , responseGetSegmentVersions $
--             newGetSegmentVersionsResponse
--
--         , responseDeleteCampaign $
--             newDeleteCampaignResponse
--
--         , responseUpdateCampaign $
--             newUpdateCampaignResponse
--
--         , responseGetSegmentVersion $
--             newGetSegmentVersionResponse
--
--         , responseDeletePushTemplate $
--             newDeletePushTemplateResponse
--
--         , responseUpdatePushTemplate $
--             newUpdatePushTemplateResponse
--
--         , responseCreateExportJob $
--             newCreateExportJobResponse
--
--         , responseCreateSegment $
--             newCreateSegmentResponse
--
--         , responseCreateRecommenderConfiguration $
--             newCreateRecommenderConfigurationResponse
--
--         , responseCreateInAppTemplate $
--             newCreateInAppTemplateResponse
--
--         , responseCreateVoiceTemplate $
--             newCreateVoiceTemplateResponse
--
--         , responseUpdateAdmChannel $
--             newUpdateAdmChannelResponse
--
--         , responseDeleteAdmChannel $
--             newDeleteAdmChannelResponse
--
--         , responseDeleteRecommenderConfiguration $
--             newDeleteRecommenderConfigurationResponse
--
--         , responseUpdateRecommenderConfiguration $
--             newUpdateRecommenderConfigurationResponse
--
--         , responseCreatePushTemplate $
--             newCreatePushTemplateResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseUpdateEndpoint $
--             newUpdateEndpointResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateCampaign $
--             newCreateCampaignResponse
--
--         , responseGetEmailTemplate $
--             newGetEmailTemplateResponse
--
--         , responseGetExportJob $
--             newGetExportJobResponse
--
--         , responseGetEndpoint $
--             newGetEndpointResponse
--
--         , responseGetSegment $
--             newGetSegmentResponse
--
--         , responseGetRecommenderConfiguration $
--             newGetRecommenderConfigurationResponse
--
--         , responseUpdateEndpointsBatch $
--             newUpdateEndpointsBatchResponse
--
--         , responseGetAdmChannel $
--             newGetAdmChannelResponse
--
--         , responseGetCampaign $
--             newGetCampaignResponse
--
--         , responseGetVoiceTemplate $
--             newGetVoiceTemplateResponse
--
--         , responseGetInAppTemplate $
--             newGetInAppTemplateResponse
--
--         , responseGetPushTemplate $
--             newGetPushTemplateResponse
--
--         , responseDeleteUserEndpoints $
--             newDeleteUserEndpointsResponse
--
--         , responseCreateEmailTemplate $
--             newCreateEmailTemplateResponse
--
--         , responseGetInAppMessages $
--             newGetInAppMessagesResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseUpdateApnsVoipSandboxChannel $
--             newUpdateApnsVoipSandboxChannelResponse
--
--         , responseDeleteApnsVoipSandboxChannel $
--             newDeleteApnsVoipSandboxChannelResponse
--
--         , responseUpdateGcmChannel $
--             newUpdateGcmChannelResponse
--
--         , responseDeleteGcmChannel $
--             newDeleteGcmChannelResponse
--
--         , responseGetCampaignActivities $
--             newGetCampaignActivitiesResponse
--
--         , responseGetJourneyExecutionMetrics $
--             newGetJourneyExecutionMetricsResponse
--
--         , responseUpdateJourneyState $
--             newUpdateJourneyStateResponse
--
--         , responseGetEventStream $
--             newGetEventStreamResponse
--
--         , responseGetChannels $
--             newGetChannelsResponse
--
--         , responseGetJourney $
--             newGetJourneyResponse
--
--         , responseDeleteEmailChannel $
--             newDeleteEmailChannelResponse
--
--         , responseUpdateEmailChannel $
--             newUpdateEmailChannelResponse
--
--         , responseGetBaiduChannel $
--             newGetBaiduChannelResponse
--
--         , responseDeleteApnsChannel $
--             newDeleteApnsChannelResponse
--
--         , responseUpdateApnsChannel $
--             newUpdateApnsChannelResponse
--
--         , responseRemoveAttributes $
--             newRemoveAttributesResponse
--
--         , responseListTemplates $
--             newListTemplatesResponse
--
--         , responsePutEventStream $
--             newPutEventStreamResponse
--
--         , responseDeleteEventStream $
--             newDeleteEventStreamResponse
--
--         , responseGetCampaignVersions $
--             newGetCampaignVersionsResponse
--
--         , responseDeleteJourney $
--             newDeleteJourneyResponse
--
--         , responseUpdateJourney $
--             newUpdateJourneyResponse
--
--         , responseGetCampaignDateRangeKpi $
--             newGetCampaignDateRangeKpiResponse
--
--         , responseGetApnsChannel $
--             newGetApnsChannelResponse
--
--         , responseUpdateVoiceChannel $
--             newUpdateVoiceChannelResponse
--
--         , responseDeleteVoiceChannel $
--             newDeleteVoiceChannelResponse
--
--         , responseGetApps $
--             newGetAppsResponse
--
--         , responseGetApnsSandboxChannel $
--             newGetApnsSandboxChannelResponse
--
--         , responseCreateJourney $
--             newCreateJourneyResponse
--
--         , responseGetUserEndpoints $
--             newGetUserEndpointsResponse
--
--         , responseDeleteVoiceTemplate $
--             newDeleteVoiceTemplateResponse
--
--         , responseUpdateVoiceTemplate $
--             newUpdateVoiceTemplateResponse
--
--         , responseDeleteInAppTemplate $
--             newDeleteInAppTemplateResponse
--
--         , responseUpdateInAppTemplate $
--             newUpdateInAppTemplateResponse
--
--         , responseGetImportJobs $
--             newGetImportJobsResponse
--
--         , responseGetJourneyDateRangeKpi $
--             newGetJourneyDateRangeKpiResponse
--
--         , responseUpdateTemplateActiveVersion $
--             newUpdateTemplateActiveVersionResponse
--
--         , responseDeleteSmsChannel $
--             newDeleteSmsChannelResponse
--
--         , responseUpdateSmsChannel $
--             newUpdateSmsChannelResponse
--
--         , responseGetApp $
--             newGetAppResponse
--
--         , responseGetCampaignVersion $
--             newGetCampaignVersionResponse
--
--         , responseDeleteSegment $
--             newDeleteSegmentResponse
--
--         , responseUpdateSegment $
--             newUpdateSegmentResponse
--
--         , responseGetApplicationDateRangeKpi $
--             newGetApplicationDateRangeKpiResponse
--
--         , responseCreateApp $
--             newCreateAppResponse
--
--         , responseGetSegmentExportJobs $
--             newGetSegmentExportJobsResponse
--
--         , responseDeleteEmailTemplate $
--             newDeleteEmailTemplateResponse
--
--         , responseUpdateEmailTemplate $
--             newUpdateEmailTemplateResponse
--
--         , responseGetSmsChannel $
--             newGetSmsChannelResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDeleteApnsSandboxChannel $
--             newDeleteApnsSandboxChannelResponse
--
--         , responseUpdateApnsSandboxChannel $
--             newUpdateApnsSandboxChannelResponse
--
--         , responseGetCampaigns $
--             newGetCampaignsResponse
--
--         , responseGetVoiceChannel $
--             newGetVoiceChannelResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListTemplateVersions $
--             newListTemplateVersionsResponse
--
--         , responseGetSmsTemplate $
--             newGetSmsTemplateResponse
--
--         , responsePutEvents $
--             newPutEventsResponse
--
--         , responseUpdateApplicationSettings $
--             newUpdateApplicationSettingsResponse
--
--         , responseGetJourneyExecutionActivityMetrics $
--             newGetJourneyExecutionActivityMetricsResponse
--
--         , responseGetSegments $
--             newGetSegmentsResponse
--
--         , responseGetExportJobs $
--             newGetExportJobsResponse
--
--         , responseCreateImportJob $
--             newCreateImportJobResponse
--
--         , responseGetRecommenderConfigurations $
--             newGetRecommenderConfigurationsResponse
--
--         , responseDeleteApnsVoipChannel $
--             newDeleteApnsVoipChannelResponse
--
--         , responseUpdateApnsVoipChannel $
--             newUpdateApnsVoipChannelResponse
--
--         , responseSendUsersMessages $
--             newSendUsersMessagesResponse
--
--         , responseGetApplicationSettings $
--             newGetApplicationSettingsResponse
--
--         , responseDeleteBaiduChannel $
--             newDeleteBaiduChannelResponse
--
--         , responseUpdateBaiduChannel $
--             newUpdateBaiduChannelResponse
--
--         , responseCreateSmsTemplate $
--             newCreateSmsTemplateResponse
--
--         , responsePhoneNumberValidate $
--             newPhoneNumberValidateResponse
--
--         , responseListJourneys $
--             newListJourneysResponse
--
--         , responseGetApnsVoipChannel $
--             newGetApnsVoipChannelResponse
--
--         , responseGetEmailChannel $
--             newGetEmailChannelResponse
--
--           ]
--     ]

-- Requests

requestGetGcmChannel :: GetGcmChannel -> TestTree
requestGetGcmChannel =
  req
    "GetGcmChannel"
    "fixture/GetGcmChannel.yaml"

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

requestGetApnsVoipSandboxChannel :: GetApnsVoipSandboxChannel -> TestTree
requestGetApnsVoipSandboxChannel =
  req
    "GetApnsVoipSandboxChannel"
    "fixture/GetApnsVoipSandboxChannel.yaml"

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

requestCreateRecommenderConfiguration :: CreateRecommenderConfiguration' -> TestTree
requestCreateRecommenderConfiguration =
  req
    "CreateRecommenderConfiguration"
    "fixture/CreateRecommenderConfiguration.yaml"

requestCreateInAppTemplate :: CreateInAppTemplate -> TestTree
requestCreateInAppTemplate =
  req
    "CreateInAppTemplate"
    "fixture/CreateInAppTemplate.yaml"

requestCreateVoiceTemplate :: CreateVoiceTemplate -> TestTree
requestCreateVoiceTemplate =
  req
    "CreateVoiceTemplate"
    "fixture/CreateVoiceTemplate.yaml"

requestUpdateAdmChannel :: UpdateAdmChannel -> TestTree
requestUpdateAdmChannel =
  req
    "UpdateAdmChannel"
    "fixture/UpdateAdmChannel.yaml"

requestDeleteAdmChannel :: DeleteAdmChannel -> TestTree
requestDeleteAdmChannel =
  req
    "DeleteAdmChannel"
    "fixture/DeleteAdmChannel.yaml"

requestDeleteRecommenderConfiguration :: DeleteRecommenderConfiguration -> TestTree
requestDeleteRecommenderConfiguration =
  req
    "DeleteRecommenderConfiguration"
    "fixture/DeleteRecommenderConfiguration.yaml"

requestUpdateRecommenderConfiguration :: UpdateRecommenderConfiguration' -> TestTree
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

requestGetAdmChannel :: GetAdmChannel -> TestTree
requestGetAdmChannel =
  req
    "GetAdmChannel"
    "fixture/GetAdmChannel.yaml"

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

requestGetInAppTemplate :: GetInAppTemplate -> TestTree
requestGetInAppTemplate =
  req
    "GetInAppTemplate"
    "fixture/GetInAppTemplate.yaml"

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

requestGetInAppMessages :: GetInAppMessages -> TestTree
requestGetInAppMessages =
  req
    "GetInAppMessages"
    "fixture/GetInAppMessages.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestUpdateApnsVoipSandboxChannel :: UpdateApnsVoipSandboxChannel -> TestTree
requestUpdateApnsVoipSandboxChannel =
  req
    "UpdateApnsVoipSandboxChannel"
    "fixture/UpdateApnsVoipSandboxChannel.yaml"

requestDeleteApnsVoipSandboxChannel :: DeleteApnsVoipSandboxChannel -> TestTree
requestDeleteApnsVoipSandboxChannel =
  req
    "DeleteApnsVoipSandboxChannel"
    "fixture/DeleteApnsVoipSandboxChannel.yaml"

requestUpdateGcmChannel :: UpdateGcmChannel -> TestTree
requestUpdateGcmChannel =
  req
    "UpdateGcmChannel"
    "fixture/UpdateGcmChannel.yaml"

requestDeleteGcmChannel :: DeleteGcmChannel -> TestTree
requestDeleteGcmChannel =
  req
    "DeleteGcmChannel"
    "fixture/DeleteGcmChannel.yaml"

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

requestDeleteApnsChannel :: DeleteApnsChannel -> TestTree
requestDeleteApnsChannel =
  req
    "DeleteApnsChannel"
    "fixture/DeleteApnsChannel.yaml"

requestUpdateApnsChannel :: UpdateApnsChannel -> TestTree
requestUpdateApnsChannel =
  req
    "UpdateApnsChannel"
    "fixture/UpdateApnsChannel.yaml"

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

requestGetApnsChannel :: GetApnsChannel -> TestTree
requestGetApnsChannel =
  req
    "GetApnsChannel"
    "fixture/GetApnsChannel.yaml"

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

requestGetApnsSandboxChannel :: GetApnsSandboxChannel -> TestTree
requestGetApnsSandboxChannel =
  req
    "GetApnsSandboxChannel"
    "fixture/GetApnsSandboxChannel.yaml"

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

requestDeleteInAppTemplate :: DeleteInAppTemplate -> TestTree
requestDeleteInAppTemplate =
  req
    "DeleteInAppTemplate"
    "fixture/DeleteInAppTemplate.yaml"

requestUpdateInAppTemplate :: UpdateInAppTemplate -> TestTree
requestUpdateInAppTemplate =
  req
    "UpdateInAppTemplate"
    "fixture/UpdateInAppTemplate.yaml"

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

requestDeleteApnsSandboxChannel :: DeleteApnsSandboxChannel -> TestTree
requestDeleteApnsSandboxChannel =
  req
    "DeleteApnsSandboxChannel"
    "fixture/DeleteApnsSandboxChannel.yaml"

requestUpdateApnsSandboxChannel :: UpdateApnsSandboxChannel -> TestTree
requestUpdateApnsSandboxChannel =
  req
    "UpdateApnsSandboxChannel"
    "fixture/UpdateApnsSandboxChannel.yaml"

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

requestDeleteApnsVoipChannel :: DeleteApnsVoipChannel -> TestTree
requestDeleteApnsVoipChannel =
  req
    "DeleteApnsVoipChannel"
    "fixture/DeleteApnsVoipChannel.yaml"

requestUpdateApnsVoipChannel :: UpdateApnsVoipChannel -> TestTree
requestUpdateApnsVoipChannel =
  req
    "UpdateApnsVoipChannel"
    "fixture/UpdateApnsVoipChannel.yaml"

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

requestGetApnsVoipChannel :: GetApnsVoipChannel -> TestTree
requestGetApnsVoipChannel =
  req
    "GetApnsVoipChannel"
    "fixture/GetApnsVoipChannel.yaml"

requestGetEmailChannel :: GetEmailChannel -> TestTree
requestGetEmailChannel =
  req
    "GetEmailChannel"
    "fixture/GetEmailChannel.yaml"

-- Responses

responseGetGcmChannel :: GetGcmChannelResponse -> TestTree
responseGetGcmChannel =
  res
    "GetGcmChannelResponse"
    "fixture/GetGcmChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGcmChannel)

responseGetSegmentImportJobs :: GetSegmentImportJobsResponse -> TestTree
responseGetSegmentImportJobs =
  res
    "GetSegmentImportJobsResponse"
    "fixture/GetSegmentImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSegmentImportJobs)

responseSendMessages :: SendMessagesResponse -> TestTree
responseSendMessages =
  res
    "SendMessagesResponse"
    "fixture/SendMessagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendMessages)

responseGetImportJob :: GetImportJobResponse -> TestTree
responseGetImportJob =
  res
    "GetImportJobResponse"
    "fixture/GetImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetImportJob)

responseDeleteSmsTemplate :: DeleteSmsTemplateResponse -> TestTree
responseDeleteSmsTemplate =
  res
    "DeleteSmsTemplateResponse"
    "fixture/DeleteSmsTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSmsTemplate)

responseUpdateSmsTemplate :: UpdateSmsTemplateResponse -> TestTree
responseUpdateSmsTemplate =
  res
    "UpdateSmsTemplateResponse"
    "fixture/UpdateSmsTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSmsTemplate)

responseGetApnsVoipSandboxChannel :: GetApnsVoipSandboxChannelResponse -> TestTree
responseGetApnsVoipSandboxChannel =
  res
    "GetApnsVoipSandboxChannelResponse"
    "fixture/GetApnsVoipSandboxChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApnsVoipSandboxChannel)

responseGetSegmentVersions :: GetSegmentVersionsResponse -> TestTree
responseGetSegmentVersions =
  res
    "GetSegmentVersionsResponse"
    "fixture/GetSegmentVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSegmentVersions)

responseDeleteCampaign :: DeleteCampaignResponse -> TestTree
responseDeleteCampaign =
  res
    "DeleteCampaignResponse"
    "fixture/DeleteCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCampaign)

responseUpdateCampaign :: UpdateCampaignResponse -> TestTree
responseUpdateCampaign =
  res
    "UpdateCampaignResponse"
    "fixture/UpdateCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCampaign)

responseGetSegmentVersion :: GetSegmentVersionResponse -> TestTree
responseGetSegmentVersion =
  res
    "GetSegmentVersionResponse"
    "fixture/GetSegmentVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSegmentVersion)

responseDeletePushTemplate :: DeletePushTemplateResponse -> TestTree
responseDeletePushTemplate =
  res
    "DeletePushTemplateResponse"
    "fixture/DeletePushTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePushTemplate)

responseUpdatePushTemplate :: UpdatePushTemplateResponse -> TestTree
responseUpdatePushTemplate =
  res
    "UpdatePushTemplateResponse"
    "fixture/UpdatePushTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePushTemplate)

responseCreateExportJob :: CreateExportJobResponse -> TestTree
responseCreateExportJob =
  res
    "CreateExportJobResponse"
    "fixture/CreateExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateExportJob)

responseCreateSegment :: CreateSegmentResponse -> TestTree
responseCreateSegment =
  res
    "CreateSegmentResponse"
    "fixture/CreateSegmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSegment)

responseCreateRecommenderConfiguration :: CreateRecommenderConfigurationResponse -> TestTree
responseCreateRecommenderConfiguration =
  res
    "CreateRecommenderConfigurationResponse"
    "fixture/CreateRecommenderConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRecommenderConfiguration')

responseCreateInAppTemplate :: CreateInAppTemplateResponse -> TestTree
responseCreateInAppTemplate =
  res
    "CreateInAppTemplateResponse"
    "fixture/CreateInAppTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInAppTemplate)

responseCreateVoiceTemplate :: CreateVoiceTemplateResponse -> TestTree
responseCreateVoiceTemplate =
  res
    "CreateVoiceTemplateResponse"
    "fixture/CreateVoiceTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVoiceTemplate)

responseUpdateAdmChannel :: UpdateAdmChannelResponse -> TestTree
responseUpdateAdmChannel =
  res
    "UpdateAdmChannelResponse"
    "fixture/UpdateAdmChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAdmChannel)

responseDeleteAdmChannel :: DeleteAdmChannelResponse -> TestTree
responseDeleteAdmChannel =
  res
    "DeleteAdmChannelResponse"
    "fixture/DeleteAdmChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAdmChannel)

responseDeleteRecommenderConfiguration :: DeleteRecommenderConfigurationResponse -> TestTree
responseDeleteRecommenderConfiguration =
  res
    "DeleteRecommenderConfigurationResponse"
    "fixture/DeleteRecommenderConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRecommenderConfiguration)

responseUpdateRecommenderConfiguration :: UpdateRecommenderConfigurationResponse -> TestTree
responseUpdateRecommenderConfiguration =
  res
    "UpdateRecommenderConfigurationResponse"
    "fixture/UpdateRecommenderConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRecommenderConfiguration')

responseCreatePushTemplate :: CreatePushTemplateResponse -> TestTree
responseCreatePushTemplate =
  res
    "CreatePushTemplateResponse"
    "fixture/CreatePushTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePushTemplate)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpoint)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEndpoint)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseCreateCampaign :: CreateCampaignResponse -> TestTree
responseCreateCampaign =
  res
    "CreateCampaignResponse"
    "fixture/CreateCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCampaign)

responseGetEmailTemplate :: GetEmailTemplateResponse -> TestTree
responseGetEmailTemplate =
  res
    "GetEmailTemplateResponse"
    "fixture/GetEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEmailTemplate)

responseGetExportJob :: GetExportJobResponse -> TestTree
responseGetExportJob =
  res
    "GetExportJobResponse"
    "fixture/GetExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExportJob)

responseGetEndpoint :: GetEndpointResponse -> TestTree
responseGetEndpoint =
  res
    "GetEndpointResponse"
    "fixture/GetEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEndpoint)

responseGetSegment :: GetSegmentResponse -> TestTree
responseGetSegment =
  res
    "GetSegmentResponse"
    "fixture/GetSegmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSegment)

responseGetRecommenderConfiguration :: GetRecommenderConfigurationResponse -> TestTree
responseGetRecommenderConfiguration =
  res
    "GetRecommenderConfigurationResponse"
    "fixture/GetRecommenderConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecommenderConfiguration)

responseUpdateEndpointsBatch :: UpdateEndpointsBatchResponse -> TestTree
responseUpdateEndpointsBatch =
  res
    "UpdateEndpointsBatchResponse"
    "fixture/UpdateEndpointsBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEndpointsBatch)

responseGetAdmChannel :: GetAdmChannelResponse -> TestTree
responseGetAdmChannel =
  res
    "GetAdmChannelResponse"
    "fixture/GetAdmChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAdmChannel)

responseGetCampaign :: GetCampaignResponse -> TestTree
responseGetCampaign =
  res
    "GetCampaignResponse"
    "fixture/GetCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCampaign)

responseGetVoiceTemplate :: GetVoiceTemplateResponse -> TestTree
responseGetVoiceTemplate =
  res
    "GetVoiceTemplateResponse"
    "fixture/GetVoiceTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceTemplate)

responseGetInAppTemplate :: GetInAppTemplateResponse -> TestTree
responseGetInAppTemplate =
  res
    "GetInAppTemplateResponse"
    "fixture/GetInAppTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInAppTemplate)

responseGetPushTemplate :: GetPushTemplateResponse -> TestTree
responseGetPushTemplate =
  res
    "GetPushTemplateResponse"
    "fixture/GetPushTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPushTemplate)

responseDeleteUserEndpoints :: DeleteUserEndpointsResponse -> TestTree
responseDeleteUserEndpoints =
  res
    "DeleteUserEndpointsResponse"
    "fixture/DeleteUserEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserEndpoints)

responseCreateEmailTemplate :: CreateEmailTemplateResponse -> TestTree
responseCreateEmailTemplate =
  res
    "CreateEmailTemplateResponse"
    "fixture/CreateEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEmailTemplate)

responseGetInAppMessages :: GetInAppMessagesResponse -> TestTree
responseGetInAppMessages =
  res
    "GetInAppMessagesResponse"
    "fixture/GetInAppMessagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInAppMessages)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApp)

responseUpdateApnsVoipSandboxChannel :: UpdateApnsVoipSandboxChannelResponse -> TestTree
responseUpdateApnsVoipSandboxChannel =
  res
    "UpdateApnsVoipSandboxChannelResponse"
    "fixture/UpdateApnsVoipSandboxChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApnsVoipSandboxChannel)

responseDeleteApnsVoipSandboxChannel :: DeleteApnsVoipSandboxChannelResponse -> TestTree
responseDeleteApnsVoipSandboxChannel =
  res
    "DeleteApnsVoipSandboxChannelResponse"
    "fixture/DeleteApnsVoipSandboxChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApnsVoipSandboxChannel)

responseUpdateGcmChannel :: UpdateGcmChannelResponse -> TestTree
responseUpdateGcmChannel =
  res
    "UpdateGcmChannelResponse"
    "fixture/UpdateGcmChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGcmChannel)

responseDeleteGcmChannel :: DeleteGcmChannelResponse -> TestTree
responseDeleteGcmChannel =
  res
    "DeleteGcmChannelResponse"
    "fixture/DeleteGcmChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGcmChannel)

responseGetCampaignActivities :: GetCampaignActivitiesResponse -> TestTree
responseGetCampaignActivities =
  res
    "GetCampaignActivitiesResponse"
    "fixture/GetCampaignActivitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCampaignActivities)

responseGetJourneyExecutionMetrics :: GetJourneyExecutionMetricsResponse -> TestTree
responseGetJourneyExecutionMetrics =
  res
    "GetJourneyExecutionMetricsResponse"
    "fixture/GetJourneyExecutionMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJourneyExecutionMetrics)

responseUpdateJourneyState :: UpdateJourneyStateResponse -> TestTree
responseUpdateJourneyState =
  res
    "UpdateJourneyStateResponse"
    "fixture/UpdateJourneyStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateJourneyState)

responseGetEventStream :: GetEventStreamResponse -> TestTree
responseGetEventStream =
  res
    "GetEventStreamResponse"
    "fixture/GetEventStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEventStream)

responseGetChannels :: GetChannelsResponse -> TestTree
responseGetChannels =
  res
    "GetChannelsResponse"
    "fixture/GetChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChannels)

responseGetJourney :: GetJourneyResponse -> TestTree
responseGetJourney =
  res
    "GetJourneyResponse"
    "fixture/GetJourneyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJourney)

responseDeleteEmailChannel :: DeleteEmailChannelResponse -> TestTree
responseDeleteEmailChannel =
  res
    "DeleteEmailChannelResponse"
    "fixture/DeleteEmailChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEmailChannel)

responseUpdateEmailChannel :: UpdateEmailChannelResponse -> TestTree
responseUpdateEmailChannel =
  res
    "UpdateEmailChannelResponse"
    "fixture/UpdateEmailChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEmailChannel)

responseGetBaiduChannel :: GetBaiduChannelResponse -> TestTree
responseGetBaiduChannel =
  res
    "GetBaiduChannelResponse"
    "fixture/GetBaiduChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBaiduChannel)

responseDeleteApnsChannel :: DeleteApnsChannelResponse -> TestTree
responseDeleteApnsChannel =
  res
    "DeleteApnsChannelResponse"
    "fixture/DeleteApnsChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApnsChannel)

responseUpdateApnsChannel :: UpdateApnsChannelResponse -> TestTree
responseUpdateApnsChannel =
  res
    "UpdateApnsChannelResponse"
    "fixture/UpdateApnsChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApnsChannel)

responseRemoveAttributes :: RemoveAttributesResponse -> TestTree
responseRemoveAttributes =
  res
    "RemoveAttributesResponse"
    "fixture/RemoveAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveAttributes)

responseListTemplates :: ListTemplatesResponse -> TestTree
responseListTemplates =
  res
    "ListTemplatesResponse"
    "fixture/ListTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTemplates)

responsePutEventStream :: PutEventStreamResponse -> TestTree
responsePutEventStream =
  res
    "PutEventStreamResponse"
    "fixture/PutEventStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEventStream)

responseDeleteEventStream :: DeleteEventStreamResponse -> TestTree
responseDeleteEventStream =
  res
    "DeleteEventStreamResponse"
    "fixture/DeleteEventStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventStream)

responseGetCampaignVersions :: GetCampaignVersionsResponse -> TestTree
responseGetCampaignVersions =
  res
    "GetCampaignVersionsResponse"
    "fixture/GetCampaignVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCampaignVersions)

responseDeleteJourney :: DeleteJourneyResponse -> TestTree
responseDeleteJourney =
  res
    "DeleteJourneyResponse"
    "fixture/DeleteJourneyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJourney)

responseUpdateJourney :: UpdateJourneyResponse -> TestTree
responseUpdateJourney =
  res
    "UpdateJourneyResponse"
    "fixture/UpdateJourneyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateJourney)

responseGetCampaignDateRangeKpi :: GetCampaignDateRangeKpiResponse -> TestTree
responseGetCampaignDateRangeKpi =
  res
    "GetCampaignDateRangeKpiResponse"
    "fixture/GetCampaignDateRangeKpiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCampaignDateRangeKpi)

responseGetApnsChannel :: GetApnsChannelResponse -> TestTree
responseGetApnsChannel =
  res
    "GetApnsChannelResponse"
    "fixture/GetApnsChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApnsChannel)

responseUpdateVoiceChannel :: UpdateVoiceChannelResponse -> TestTree
responseUpdateVoiceChannel =
  res
    "UpdateVoiceChannelResponse"
    "fixture/UpdateVoiceChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVoiceChannel)

responseDeleteVoiceChannel :: DeleteVoiceChannelResponse -> TestTree
responseDeleteVoiceChannel =
  res
    "DeleteVoiceChannelResponse"
    "fixture/DeleteVoiceChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceChannel)

responseGetApps :: GetAppsResponse -> TestTree
responseGetApps =
  res
    "GetAppsResponse"
    "fixture/GetAppsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApps)

responseGetApnsSandboxChannel :: GetApnsSandboxChannelResponse -> TestTree
responseGetApnsSandboxChannel =
  res
    "GetApnsSandboxChannelResponse"
    "fixture/GetApnsSandboxChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApnsSandboxChannel)

responseCreateJourney :: CreateJourneyResponse -> TestTree
responseCreateJourney =
  res
    "CreateJourneyResponse"
    "fixture/CreateJourneyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJourney)

responseGetUserEndpoints :: GetUserEndpointsResponse -> TestTree
responseGetUserEndpoints =
  res
    "GetUserEndpointsResponse"
    "fixture/GetUserEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUserEndpoints)

responseDeleteVoiceTemplate :: DeleteVoiceTemplateResponse -> TestTree
responseDeleteVoiceTemplate =
  res
    "DeleteVoiceTemplateResponse"
    "fixture/DeleteVoiceTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceTemplate)

responseUpdateVoiceTemplate :: UpdateVoiceTemplateResponse -> TestTree
responseUpdateVoiceTemplate =
  res
    "UpdateVoiceTemplateResponse"
    "fixture/UpdateVoiceTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVoiceTemplate)

responseDeleteInAppTemplate :: DeleteInAppTemplateResponse -> TestTree
responseDeleteInAppTemplate =
  res
    "DeleteInAppTemplateResponse"
    "fixture/DeleteInAppTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInAppTemplate)

responseUpdateInAppTemplate :: UpdateInAppTemplateResponse -> TestTree
responseUpdateInAppTemplate =
  res
    "UpdateInAppTemplateResponse"
    "fixture/UpdateInAppTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInAppTemplate)

responseGetImportJobs :: GetImportJobsResponse -> TestTree
responseGetImportJobs =
  res
    "GetImportJobsResponse"
    "fixture/GetImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetImportJobs)

responseGetJourneyDateRangeKpi :: GetJourneyDateRangeKpiResponse -> TestTree
responseGetJourneyDateRangeKpi =
  res
    "GetJourneyDateRangeKpiResponse"
    "fixture/GetJourneyDateRangeKpiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJourneyDateRangeKpi)

responseUpdateTemplateActiveVersion :: UpdateTemplateActiveVersionResponse -> TestTree
responseUpdateTemplateActiveVersion =
  res
    "UpdateTemplateActiveVersionResponse"
    "fixture/UpdateTemplateActiveVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTemplateActiveVersion)

responseDeleteSmsChannel :: DeleteSmsChannelResponse -> TestTree
responseDeleteSmsChannel =
  res
    "DeleteSmsChannelResponse"
    "fixture/DeleteSmsChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSmsChannel)

responseUpdateSmsChannel :: UpdateSmsChannelResponse -> TestTree
responseUpdateSmsChannel =
  res
    "UpdateSmsChannelResponse"
    "fixture/UpdateSmsChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSmsChannel)

responseGetApp :: GetAppResponse -> TestTree
responseGetApp =
  res
    "GetAppResponse"
    "fixture/GetAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApp)

responseGetCampaignVersion :: GetCampaignVersionResponse -> TestTree
responseGetCampaignVersion =
  res
    "GetCampaignVersionResponse"
    "fixture/GetCampaignVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCampaignVersion)

responseDeleteSegment :: DeleteSegmentResponse -> TestTree
responseDeleteSegment =
  res
    "DeleteSegmentResponse"
    "fixture/DeleteSegmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSegment)

responseUpdateSegment :: UpdateSegmentResponse -> TestTree
responseUpdateSegment =
  res
    "UpdateSegmentResponse"
    "fixture/UpdateSegmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSegment)

responseGetApplicationDateRangeKpi :: GetApplicationDateRangeKpiResponse -> TestTree
responseGetApplicationDateRangeKpi =
  res
    "GetApplicationDateRangeKpiResponse"
    "fixture/GetApplicationDateRangeKpiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplicationDateRangeKpi)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApp)

responseGetSegmentExportJobs :: GetSegmentExportJobsResponse -> TestTree
responseGetSegmentExportJobs =
  res
    "GetSegmentExportJobsResponse"
    "fixture/GetSegmentExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSegmentExportJobs)

responseDeleteEmailTemplate :: DeleteEmailTemplateResponse -> TestTree
responseDeleteEmailTemplate =
  res
    "DeleteEmailTemplateResponse"
    "fixture/DeleteEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEmailTemplate)

responseUpdateEmailTemplate :: UpdateEmailTemplateResponse -> TestTree
responseUpdateEmailTemplate =
  res
    "UpdateEmailTemplateResponse"
    "fixture/UpdateEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEmailTemplate)

responseGetSmsChannel :: GetSmsChannelResponse -> TestTree
responseGetSmsChannel =
  res
    "GetSmsChannelResponse"
    "fixture/GetSmsChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSmsChannel)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseDeleteApnsSandboxChannel :: DeleteApnsSandboxChannelResponse -> TestTree
responseDeleteApnsSandboxChannel =
  res
    "DeleteApnsSandboxChannelResponse"
    "fixture/DeleteApnsSandboxChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApnsSandboxChannel)

responseUpdateApnsSandboxChannel :: UpdateApnsSandboxChannelResponse -> TestTree
responseUpdateApnsSandboxChannel =
  res
    "UpdateApnsSandboxChannelResponse"
    "fixture/UpdateApnsSandboxChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApnsSandboxChannel)

responseGetCampaigns :: GetCampaignsResponse -> TestTree
responseGetCampaigns =
  res
    "GetCampaignsResponse"
    "fixture/GetCampaignsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCampaigns)

responseGetVoiceChannel :: GetVoiceChannelResponse -> TestTree
responseGetVoiceChannel =
  res
    "GetVoiceChannelResponse"
    "fixture/GetVoiceChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceChannel)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseListTemplateVersions :: ListTemplateVersionsResponse -> TestTree
responseListTemplateVersions =
  res
    "ListTemplateVersionsResponse"
    "fixture/ListTemplateVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTemplateVersions)

responseGetSmsTemplate :: GetSmsTemplateResponse -> TestTree
responseGetSmsTemplate =
  res
    "GetSmsTemplateResponse"
    "fixture/GetSmsTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSmsTemplate)

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents =
  res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEvents)

responseUpdateApplicationSettings :: UpdateApplicationSettingsResponse -> TestTree
responseUpdateApplicationSettings =
  res
    "UpdateApplicationSettingsResponse"
    "fixture/UpdateApplicationSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplicationSettings)

responseGetJourneyExecutionActivityMetrics :: GetJourneyExecutionActivityMetricsResponse -> TestTree
responseGetJourneyExecutionActivityMetrics =
  res
    "GetJourneyExecutionActivityMetricsResponse"
    "fixture/GetJourneyExecutionActivityMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJourneyExecutionActivityMetrics)

responseGetSegments :: GetSegmentsResponse -> TestTree
responseGetSegments =
  res
    "GetSegmentsResponse"
    "fixture/GetSegmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSegments)

responseGetExportJobs :: GetExportJobsResponse -> TestTree
responseGetExportJobs =
  res
    "GetExportJobsResponse"
    "fixture/GetExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExportJobs)

responseCreateImportJob :: CreateImportJobResponse -> TestTree
responseCreateImportJob =
  res
    "CreateImportJobResponse"
    "fixture/CreateImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateImportJob)

responseGetRecommenderConfigurations :: GetRecommenderConfigurationsResponse -> TestTree
responseGetRecommenderConfigurations =
  res
    "GetRecommenderConfigurationsResponse"
    "fixture/GetRecommenderConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecommenderConfigurations)

responseDeleteApnsVoipChannel :: DeleteApnsVoipChannelResponse -> TestTree
responseDeleteApnsVoipChannel =
  res
    "DeleteApnsVoipChannelResponse"
    "fixture/DeleteApnsVoipChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApnsVoipChannel)

responseUpdateApnsVoipChannel :: UpdateApnsVoipChannelResponse -> TestTree
responseUpdateApnsVoipChannel =
  res
    "UpdateApnsVoipChannelResponse"
    "fixture/UpdateApnsVoipChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApnsVoipChannel)

responseSendUsersMessages :: SendUsersMessagesResponse -> TestTree
responseSendUsersMessages =
  res
    "SendUsersMessagesResponse"
    "fixture/SendUsersMessagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendUsersMessages)

responseGetApplicationSettings :: GetApplicationSettingsResponse -> TestTree
responseGetApplicationSettings =
  res
    "GetApplicationSettingsResponse"
    "fixture/GetApplicationSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplicationSettings)

responseDeleteBaiduChannel :: DeleteBaiduChannelResponse -> TestTree
responseDeleteBaiduChannel =
  res
    "DeleteBaiduChannelResponse"
    "fixture/DeleteBaiduChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBaiduChannel)

responseUpdateBaiduChannel :: UpdateBaiduChannelResponse -> TestTree
responseUpdateBaiduChannel =
  res
    "UpdateBaiduChannelResponse"
    "fixture/UpdateBaiduChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBaiduChannel)

responseCreateSmsTemplate :: CreateSmsTemplateResponse -> TestTree
responseCreateSmsTemplate =
  res
    "CreateSmsTemplateResponse"
    "fixture/CreateSmsTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSmsTemplate)

responsePhoneNumberValidate :: PhoneNumberValidateResponse -> TestTree
responsePhoneNumberValidate =
  res
    "PhoneNumberValidateResponse"
    "fixture/PhoneNumberValidateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PhoneNumberValidate)

responseListJourneys :: ListJourneysResponse -> TestTree
responseListJourneys =
  res
    "ListJourneysResponse"
    "fixture/ListJourneysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJourneys)

responseGetApnsVoipChannel :: GetApnsVoipChannelResponse -> TestTree
responseGetApnsVoipChannel =
  res
    "GetApnsVoipChannelResponse"
    "fixture/GetApnsVoipChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApnsVoipChannel)

responseGetEmailChannel :: GetEmailChannelResponse -> TestTree
responseGetEmailChannel =
  res
    "GetEmailChannelResponse"
    "fixture/GetEmailChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEmailChannel)
