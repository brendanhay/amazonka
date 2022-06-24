{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Pinpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Pinpoint where

import Amazonka.Pinpoint
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Pinpoint.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateApp $
--             newCreateApp
--
--         , requestCreateCampaign $
--             newCreateCampaign
--
--         , requestCreateEmailTemplate $
--             newCreateEmailTemplate
--
--         , requestCreateExportJob $
--             newCreateExportJob
--
--         , requestCreateImportJob $
--             newCreateImportJob
--
--         , requestCreateInAppTemplate $
--             newCreateInAppTemplate
--
--         , requestCreateJourney $
--             newCreateJourney
--
--         , requestCreatePushTemplate $
--             newCreatePushTemplate
--
--         , requestCreateRecommenderConfiguration $
--             newCreateRecommenderConfiguration'
--
--         , requestCreateSegment $
--             newCreateSegment
--
--         , requestCreateSmsTemplate $
--             newCreateSmsTemplate
--
--         , requestCreateVoiceTemplate $
--             newCreateVoiceTemplate
--
--         , requestDeleteAdmChannel $
--             newDeleteAdmChannel
--
--         , requestDeleteApnsChannel $
--             newDeleteApnsChannel
--
--         , requestDeleteApnsSandboxChannel $
--             newDeleteApnsSandboxChannel
--
--         , requestDeleteApnsVoipChannel $
--             newDeleteApnsVoipChannel
--
--         , requestDeleteApnsVoipSandboxChannel $
--             newDeleteApnsVoipSandboxChannel
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestDeleteBaiduChannel $
--             newDeleteBaiduChannel
--
--         , requestDeleteCampaign $
--             newDeleteCampaign
--
--         , requestDeleteEmailChannel $
--             newDeleteEmailChannel
--
--         , requestDeleteEmailTemplate $
--             newDeleteEmailTemplate
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestDeleteEventStream $
--             newDeleteEventStream
--
--         , requestDeleteGcmChannel $
--             newDeleteGcmChannel
--
--         , requestDeleteInAppTemplate $
--             newDeleteInAppTemplate
--
--         , requestDeleteJourney $
--             newDeleteJourney
--
--         , requestDeletePushTemplate $
--             newDeletePushTemplate
--
--         , requestDeleteRecommenderConfiguration $
--             newDeleteRecommenderConfiguration
--
--         , requestDeleteSegment $
--             newDeleteSegment
--
--         , requestDeleteSmsChannel $
--             newDeleteSmsChannel
--
--         , requestDeleteSmsTemplate $
--             newDeleteSmsTemplate
--
--         , requestDeleteUserEndpoints $
--             newDeleteUserEndpoints
--
--         , requestDeleteVoiceChannel $
--             newDeleteVoiceChannel
--
--         , requestDeleteVoiceTemplate $
--             newDeleteVoiceTemplate
--
--         , requestGetAdmChannel $
--             newGetAdmChannel
--
--         , requestGetApnsChannel $
--             newGetApnsChannel
--
--         , requestGetApnsSandboxChannel $
--             newGetApnsSandboxChannel
--
--         , requestGetApnsVoipChannel $
--             newGetApnsVoipChannel
--
--         , requestGetApnsVoipSandboxChannel $
--             newGetApnsVoipSandboxChannel
--
--         , requestGetApp $
--             newGetApp
--
--         , requestGetApplicationDateRangeKpi $
--             newGetApplicationDateRangeKpi
--
--         , requestGetApplicationSettings $
--             newGetApplicationSettings
--
--         , requestGetApps $
--             newGetApps
--
--         , requestGetBaiduChannel $
--             newGetBaiduChannel
--
--         , requestGetCampaign $
--             newGetCampaign
--
--         , requestGetCampaignActivities $
--             newGetCampaignActivities
--
--         , requestGetCampaignDateRangeKpi $
--             newGetCampaignDateRangeKpi
--
--         , requestGetCampaignVersion $
--             newGetCampaignVersion
--
--         , requestGetCampaignVersions $
--             newGetCampaignVersions
--
--         , requestGetCampaigns $
--             newGetCampaigns
--
--         , requestGetChannels $
--             newGetChannels
--
--         , requestGetEmailChannel $
--             newGetEmailChannel
--
--         , requestGetEmailTemplate $
--             newGetEmailTemplate
--
--         , requestGetEndpoint $
--             newGetEndpoint
--
--         , requestGetEventStream $
--             newGetEventStream
--
--         , requestGetExportJob $
--             newGetExportJob
--
--         , requestGetExportJobs $
--             newGetExportJobs
--
--         , requestGetGcmChannel $
--             newGetGcmChannel
--
--         , requestGetImportJob $
--             newGetImportJob
--
--         , requestGetImportJobs $
--             newGetImportJobs
--
--         , requestGetInAppMessages $
--             newGetInAppMessages
--
--         , requestGetInAppTemplate $
--             newGetInAppTemplate
--
--         , requestGetJourney $
--             newGetJourney
--
--         , requestGetJourneyDateRangeKpi $
--             newGetJourneyDateRangeKpi
--
--         , requestGetJourneyExecutionActivityMetrics $
--             newGetJourneyExecutionActivityMetrics
--
--         , requestGetJourneyExecutionMetrics $
--             newGetJourneyExecutionMetrics
--
--         , requestGetPushTemplate $
--             newGetPushTemplate
--
--         , requestGetRecommenderConfiguration $
--             newGetRecommenderConfiguration
--
--         , requestGetRecommenderConfigurations $
--             newGetRecommenderConfigurations
--
--         , requestGetSegment $
--             newGetSegment
--
--         , requestGetSegmentExportJobs $
--             newGetSegmentExportJobs
--
--         , requestGetSegmentImportJobs $
--             newGetSegmentImportJobs
--
--         , requestGetSegmentVersion $
--             newGetSegmentVersion
--
--         , requestGetSegmentVersions $
--             newGetSegmentVersions
--
--         , requestGetSegments $
--             newGetSegments
--
--         , requestGetSmsChannel $
--             newGetSmsChannel
--
--         , requestGetSmsTemplate $
--             newGetSmsTemplate
--
--         , requestGetUserEndpoints $
--             newGetUserEndpoints
--
--         , requestGetVoiceChannel $
--             newGetVoiceChannel
--
--         , requestGetVoiceTemplate $
--             newGetVoiceTemplate
--
--         , requestListJourneys $
--             newListJourneys
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTemplateVersions $
--             newListTemplateVersions
--
--         , requestListTemplates $
--             newListTemplates
--
--         , requestPhoneNumberValidate $
--             newPhoneNumberValidate
--
--         , requestPutEventStream $
--             newPutEventStream
--
--         , requestPutEvents $
--             newPutEvents
--
--         , requestRemoveAttributes $
--             newRemoveAttributes
--
--         , requestSendMessages $
--             newSendMessages
--
--         , requestSendUsersMessages $
--             newSendUsersMessages
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAdmChannel $
--             newUpdateAdmChannel
--
--         , requestUpdateApnsChannel $
--             newUpdateApnsChannel
--
--         , requestUpdateApnsSandboxChannel $
--             newUpdateApnsSandboxChannel
--
--         , requestUpdateApnsVoipChannel $
--             newUpdateApnsVoipChannel
--
--         , requestUpdateApnsVoipSandboxChannel $
--             newUpdateApnsVoipSandboxChannel
--
--         , requestUpdateApplicationSettings $
--             newUpdateApplicationSettings
--
--         , requestUpdateBaiduChannel $
--             newUpdateBaiduChannel
--
--         , requestUpdateCampaign $
--             newUpdateCampaign
--
--         , requestUpdateEmailChannel $
--             newUpdateEmailChannel
--
--         , requestUpdateEmailTemplate $
--             newUpdateEmailTemplate
--
--         , requestUpdateEndpoint $
--             newUpdateEndpoint
--
--         , requestUpdateEndpointsBatch $
--             newUpdateEndpointsBatch
--
--         , requestUpdateGcmChannel $
--             newUpdateGcmChannel
--
--         , requestUpdateInAppTemplate $
--             newUpdateInAppTemplate
--
--         , requestUpdateJourney $
--             newUpdateJourney
--
--         , requestUpdateJourneyState $
--             newUpdateJourneyState
--
--         , requestUpdatePushTemplate $
--             newUpdatePushTemplate
--
--         , requestUpdateRecommenderConfiguration $
--             newUpdateRecommenderConfiguration'
--
--         , requestUpdateSegment $
--             newUpdateSegment
--
--         , requestUpdateSmsChannel $
--             newUpdateSmsChannel
--
--         , requestUpdateSmsTemplate $
--             newUpdateSmsTemplate
--
--         , requestUpdateTemplateActiveVersion $
--             newUpdateTemplateActiveVersion
--
--         , requestUpdateVoiceChannel $
--             newUpdateVoiceChannel
--
--         , requestUpdateVoiceTemplate $
--             newUpdateVoiceTemplate
--
--           ]

--     , testGroup "response"
--         [ responseCreateApp $
--             newCreateAppResponse
--
--         , responseCreateCampaign $
--             newCreateCampaignResponse
--
--         , responseCreateEmailTemplate $
--             newCreateEmailTemplateResponse
--
--         , responseCreateExportJob $
--             newCreateExportJobResponse
--
--         , responseCreateImportJob $
--             newCreateImportJobResponse
--
--         , responseCreateInAppTemplate $
--             newCreateInAppTemplateResponse
--
--         , responseCreateJourney $
--             newCreateJourneyResponse
--
--         , responseCreatePushTemplate $
--             newCreatePushTemplateResponse
--
--         , responseCreateRecommenderConfiguration $
--             newCreateRecommenderConfigurationResponse
--
--         , responseCreateSegment $
--             newCreateSegmentResponse
--
--         , responseCreateSmsTemplate $
--             newCreateSmsTemplateResponse
--
--         , responseCreateVoiceTemplate $
--             newCreateVoiceTemplateResponse
--
--         , responseDeleteAdmChannel $
--             newDeleteAdmChannelResponse
--
--         , responseDeleteApnsChannel $
--             newDeleteApnsChannelResponse
--
--         , responseDeleteApnsSandboxChannel $
--             newDeleteApnsSandboxChannelResponse
--
--         , responseDeleteApnsVoipChannel $
--             newDeleteApnsVoipChannelResponse
--
--         , responseDeleteApnsVoipSandboxChannel $
--             newDeleteApnsVoipSandboxChannelResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseDeleteBaiduChannel $
--             newDeleteBaiduChannelResponse
--
--         , responseDeleteCampaign $
--             newDeleteCampaignResponse
--
--         , responseDeleteEmailChannel $
--             newDeleteEmailChannelResponse
--
--         , responseDeleteEmailTemplate $
--             newDeleteEmailTemplateResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseDeleteEventStream $
--             newDeleteEventStreamResponse
--
--         , responseDeleteGcmChannel $
--             newDeleteGcmChannelResponse
--
--         , responseDeleteInAppTemplate $
--             newDeleteInAppTemplateResponse
--
--         , responseDeleteJourney $
--             newDeleteJourneyResponse
--
--         , responseDeletePushTemplate $
--             newDeletePushTemplateResponse
--
--         , responseDeleteRecommenderConfiguration $
--             newDeleteRecommenderConfigurationResponse
--
--         , responseDeleteSegment $
--             newDeleteSegmentResponse
--
--         , responseDeleteSmsChannel $
--             newDeleteSmsChannelResponse
--
--         , responseDeleteSmsTemplate $
--             newDeleteSmsTemplateResponse
--
--         , responseDeleteUserEndpoints $
--             newDeleteUserEndpointsResponse
--
--         , responseDeleteVoiceChannel $
--             newDeleteVoiceChannelResponse
--
--         , responseDeleteVoiceTemplate $
--             newDeleteVoiceTemplateResponse
--
--         , responseGetAdmChannel $
--             newGetAdmChannelResponse
--
--         , responseGetApnsChannel $
--             newGetApnsChannelResponse
--
--         , responseGetApnsSandboxChannel $
--             newGetApnsSandboxChannelResponse
--
--         , responseGetApnsVoipChannel $
--             newGetApnsVoipChannelResponse
--
--         , responseGetApnsVoipSandboxChannel $
--             newGetApnsVoipSandboxChannelResponse
--
--         , responseGetApp $
--             newGetAppResponse
--
--         , responseGetApplicationDateRangeKpi $
--             newGetApplicationDateRangeKpiResponse
--
--         , responseGetApplicationSettings $
--             newGetApplicationSettingsResponse
--
--         , responseGetApps $
--             newGetAppsResponse
--
--         , responseGetBaiduChannel $
--             newGetBaiduChannelResponse
--
--         , responseGetCampaign $
--             newGetCampaignResponse
--
--         , responseGetCampaignActivities $
--             newGetCampaignActivitiesResponse
--
--         , responseGetCampaignDateRangeKpi $
--             newGetCampaignDateRangeKpiResponse
--
--         , responseGetCampaignVersion $
--             newGetCampaignVersionResponse
--
--         , responseGetCampaignVersions $
--             newGetCampaignVersionsResponse
--
--         , responseGetCampaigns $
--             newGetCampaignsResponse
--
--         , responseGetChannels $
--             newGetChannelsResponse
--
--         , responseGetEmailChannel $
--             newGetEmailChannelResponse
--
--         , responseGetEmailTemplate $
--             newGetEmailTemplateResponse
--
--         , responseGetEndpoint $
--             newGetEndpointResponse
--
--         , responseGetEventStream $
--             newGetEventStreamResponse
--
--         , responseGetExportJob $
--             newGetExportJobResponse
--
--         , responseGetExportJobs $
--             newGetExportJobsResponse
--
--         , responseGetGcmChannel $
--             newGetGcmChannelResponse
--
--         , responseGetImportJob $
--             newGetImportJobResponse
--
--         , responseGetImportJobs $
--             newGetImportJobsResponse
--
--         , responseGetInAppMessages $
--             newGetInAppMessagesResponse
--
--         , responseGetInAppTemplate $
--             newGetInAppTemplateResponse
--
--         , responseGetJourney $
--             newGetJourneyResponse
--
--         , responseGetJourneyDateRangeKpi $
--             newGetJourneyDateRangeKpiResponse
--
--         , responseGetJourneyExecutionActivityMetrics $
--             newGetJourneyExecutionActivityMetricsResponse
--
--         , responseGetJourneyExecutionMetrics $
--             newGetJourneyExecutionMetricsResponse
--
--         , responseGetPushTemplate $
--             newGetPushTemplateResponse
--
--         , responseGetRecommenderConfiguration $
--             newGetRecommenderConfigurationResponse
--
--         , responseGetRecommenderConfigurations $
--             newGetRecommenderConfigurationsResponse
--
--         , responseGetSegment $
--             newGetSegmentResponse
--
--         , responseGetSegmentExportJobs $
--             newGetSegmentExportJobsResponse
--
--         , responseGetSegmentImportJobs $
--             newGetSegmentImportJobsResponse
--
--         , responseGetSegmentVersion $
--             newGetSegmentVersionResponse
--
--         , responseGetSegmentVersions $
--             newGetSegmentVersionsResponse
--
--         , responseGetSegments $
--             newGetSegmentsResponse
--
--         , responseGetSmsChannel $
--             newGetSmsChannelResponse
--
--         , responseGetSmsTemplate $
--             newGetSmsTemplateResponse
--
--         , responseGetUserEndpoints $
--             newGetUserEndpointsResponse
--
--         , responseGetVoiceChannel $
--             newGetVoiceChannelResponse
--
--         , responseGetVoiceTemplate $
--             newGetVoiceTemplateResponse
--
--         , responseListJourneys $
--             newListJourneysResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTemplateVersions $
--             newListTemplateVersionsResponse
--
--         , responseListTemplates $
--             newListTemplatesResponse
--
--         , responsePhoneNumberValidate $
--             newPhoneNumberValidateResponse
--
--         , responsePutEventStream $
--             newPutEventStreamResponse
--
--         , responsePutEvents $
--             newPutEventsResponse
--
--         , responseRemoveAttributes $
--             newRemoveAttributesResponse
--
--         , responseSendMessages $
--             newSendMessagesResponse
--
--         , responseSendUsersMessages $
--             newSendUsersMessagesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAdmChannel $
--             newUpdateAdmChannelResponse
--
--         , responseUpdateApnsChannel $
--             newUpdateApnsChannelResponse
--
--         , responseUpdateApnsSandboxChannel $
--             newUpdateApnsSandboxChannelResponse
--
--         , responseUpdateApnsVoipChannel $
--             newUpdateApnsVoipChannelResponse
--
--         , responseUpdateApnsVoipSandboxChannel $
--             newUpdateApnsVoipSandboxChannelResponse
--
--         , responseUpdateApplicationSettings $
--             newUpdateApplicationSettingsResponse
--
--         , responseUpdateBaiduChannel $
--             newUpdateBaiduChannelResponse
--
--         , responseUpdateCampaign $
--             newUpdateCampaignResponse
--
--         , responseUpdateEmailChannel $
--             newUpdateEmailChannelResponse
--
--         , responseUpdateEmailTemplate $
--             newUpdateEmailTemplateResponse
--
--         , responseUpdateEndpoint $
--             newUpdateEndpointResponse
--
--         , responseUpdateEndpointsBatch $
--             newUpdateEndpointsBatchResponse
--
--         , responseUpdateGcmChannel $
--             newUpdateGcmChannelResponse
--
--         , responseUpdateInAppTemplate $
--             newUpdateInAppTemplateResponse
--
--         , responseUpdateJourney $
--             newUpdateJourneyResponse
--
--         , responseUpdateJourneyState $
--             newUpdateJourneyStateResponse
--
--         , responseUpdatePushTemplate $
--             newUpdatePushTemplateResponse
--
--         , responseUpdateRecommenderConfiguration $
--             newUpdateRecommenderConfigurationResponse
--
--         , responseUpdateSegment $
--             newUpdateSegmentResponse
--
--         , responseUpdateSmsChannel $
--             newUpdateSmsChannelResponse
--
--         , responseUpdateSmsTemplate $
--             newUpdateSmsTemplateResponse
--
--         , responseUpdateTemplateActiveVersion $
--             newUpdateTemplateActiveVersionResponse
--
--         , responseUpdateVoiceChannel $
--             newUpdateVoiceChannelResponse
--
--         , responseUpdateVoiceTemplate $
--             newUpdateVoiceTemplateResponse
--
--           ]
--     ]

-- Requests

requestCreateApp :: CreateApp -> TestTree
requestCreateApp =
  req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestCreateCampaign :: CreateCampaign -> TestTree
requestCreateCampaign =
  req
    "CreateCampaign"
    "fixture/CreateCampaign.yaml"

requestCreateEmailTemplate :: CreateEmailTemplate -> TestTree
requestCreateEmailTemplate =
  req
    "CreateEmailTemplate"
    "fixture/CreateEmailTemplate.yaml"

requestCreateExportJob :: CreateExportJob -> TestTree
requestCreateExportJob =
  req
    "CreateExportJob"
    "fixture/CreateExportJob.yaml"

requestCreateImportJob :: CreateImportJob -> TestTree
requestCreateImportJob =
  req
    "CreateImportJob"
    "fixture/CreateImportJob.yaml"

requestCreateInAppTemplate :: CreateInAppTemplate -> TestTree
requestCreateInAppTemplate =
  req
    "CreateInAppTemplate"
    "fixture/CreateInAppTemplate.yaml"

requestCreateJourney :: CreateJourney -> TestTree
requestCreateJourney =
  req
    "CreateJourney"
    "fixture/CreateJourney.yaml"

requestCreatePushTemplate :: CreatePushTemplate -> TestTree
requestCreatePushTemplate =
  req
    "CreatePushTemplate"
    "fixture/CreatePushTemplate.yaml"

requestCreateRecommenderConfiguration :: CreateRecommenderConfiguration' -> TestTree
requestCreateRecommenderConfiguration =
  req
    "CreateRecommenderConfiguration"
    "fixture/CreateRecommenderConfiguration.yaml"

requestCreateSegment :: CreateSegment -> TestTree
requestCreateSegment =
  req
    "CreateSegment"
    "fixture/CreateSegment.yaml"

requestCreateSmsTemplate :: CreateSmsTemplate -> TestTree
requestCreateSmsTemplate =
  req
    "CreateSmsTemplate"
    "fixture/CreateSmsTemplate.yaml"

requestCreateVoiceTemplate :: CreateVoiceTemplate -> TestTree
requestCreateVoiceTemplate =
  req
    "CreateVoiceTemplate"
    "fixture/CreateVoiceTemplate.yaml"

requestDeleteAdmChannel :: DeleteAdmChannel -> TestTree
requestDeleteAdmChannel =
  req
    "DeleteAdmChannel"
    "fixture/DeleteAdmChannel.yaml"

requestDeleteApnsChannel :: DeleteApnsChannel -> TestTree
requestDeleteApnsChannel =
  req
    "DeleteApnsChannel"
    "fixture/DeleteApnsChannel.yaml"

requestDeleteApnsSandboxChannel :: DeleteApnsSandboxChannel -> TestTree
requestDeleteApnsSandboxChannel =
  req
    "DeleteApnsSandboxChannel"
    "fixture/DeleteApnsSandboxChannel.yaml"

requestDeleteApnsVoipChannel :: DeleteApnsVoipChannel -> TestTree
requestDeleteApnsVoipChannel =
  req
    "DeleteApnsVoipChannel"
    "fixture/DeleteApnsVoipChannel.yaml"

requestDeleteApnsVoipSandboxChannel :: DeleteApnsVoipSandboxChannel -> TestTree
requestDeleteApnsVoipSandboxChannel =
  req
    "DeleteApnsVoipSandboxChannel"
    "fixture/DeleteApnsVoipSandboxChannel.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestDeleteBaiduChannel :: DeleteBaiduChannel -> TestTree
requestDeleteBaiduChannel =
  req
    "DeleteBaiduChannel"
    "fixture/DeleteBaiduChannel.yaml"

requestDeleteCampaign :: DeleteCampaign -> TestTree
requestDeleteCampaign =
  req
    "DeleteCampaign"
    "fixture/DeleteCampaign.yaml"

requestDeleteEmailChannel :: DeleteEmailChannel -> TestTree
requestDeleteEmailChannel =
  req
    "DeleteEmailChannel"
    "fixture/DeleteEmailChannel.yaml"

requestDeleteEmailTemplate :: DeleteEmailTemplate -> TestTree
requestDeleteEmailTemplate =
  req
    "DeleteEmailTemplate"
    "fixture/DeleteEmailTemplate.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint =
  req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestDeleteEventStream :: DeleteEventStream -> TestTree
requestDeleteEventStream =
  req
    "DeleteEventStream"
    "fixture/DeleteEventStream.yaml"

requestDeleteGcmChannel :: DeleteGcmChannel -> TestTree
requestDeleteGcmChannel =
  req
    "DeleteGcmChannel"
    "fixture/DeleteGcmChannel.yaml"

requestDeleteInAppTemplate :: DeleteInAppTemplate -> TestTree
requestDeleteInAppTemplate =
  req
    "DeleteInAppTemplate"
    "fixture/DeleteInAppTemplate.yaml"

requestDeleteJourney :: DeleteJourney -> TestTree
requestDeleteJourney =
  req
    "DeleteJourney"
    "fixture/DeleteJourney.yaml"

requestDeletePushTemplate :: DeletePushTemplate -> TestTree
requestDeletePushTemplate =
  req
    "DeletePushTemplate"
    "fixture/DeletePushTemplate.yaml"

requestDeleteRecommenderConfiguration :: DeleteRecommenderConfiguration -> TestTree
requestDeleteRecommenderConfiguration =
  req
    "DeleteRecommenderConfiguration"
    "fixture/DeleteRecommenderConfiguration.yaml"

requestDeleteSegment :: DeleteSegment -> TestTree
requestDeleteSegment =
  req
    "DeleteSegment"
    "fixture/DeleteSegment.yaml"

requestDeleteSmsChannel :: DeleteSmsChannel -> TestTree
requestDeleteSmsChannel =
  req
    "DeleteSmsChannel"
    "fixture/DeleteSmsChannel.yaml"

requestDeleteSmsTemplate :: DeleteSmsTemplate -> TestTree
requestDeleteSmsTemplate =
  req
    "DeleteSmsTemplate"
    "fixture/DeleteSmsTemplate.yaml"

requestDeleteUserEndpoints :: DeleteUserEndpoints -> TestTree
requestDeleteUserEndpoints =
  req
    "DeleteUserEndpoints"
    "fixture/DeleteUserEndpoints.yaml"

requestDeleteVoiceChannel :: DeleteVoiceChannel -> TestTree
requestDeleteVoiceChannel =
  req
    "DeleteVoiceChannel"
    "fixture/DeleteVoiceChannel.yaml"

requestDeleteVoiceTemplate :: DeleteVoiceTemplate -> TestTree
requestDeleteVoiceTemplate =
  req
    "DeleteVoiceTemplate"
    "fixture/DeleteVoiceTemplate.yaml"

requestGetAdmChannel :: GetAdmChannel -> TestTree
requestGetAdmChannel =
  req
    "GetAdmChannel"
    "fixture/GetAdmChannel.yaml"

requestGetApnsChannel :: GetApnsChannel -> TestTree
requestGetApnsChannel =
  req
    "GetApnsChannel"
    "fixture/GetApnsChannel.yaml"

requestGetApnsSandboxChannel :: GetApnsSandboxChannel -> TestTree
requestGetApnsSandboxChannel =
  req
    "GetApnsSandboxChannel"
    "fixture/GetApnsSandboxChannel.yaml"

requestGetApnsVoipChannel :: GetApnsVoipChannel -> TestTree
requestGetApnsVoipChannel =
  req
    "GetApnsVoipChannel"
    "fixture/GetApnsVoipChannel.yaml"

requestGetApnsVoipSandboxChannel :: GetApnsVoipSandboxChannel -> TestTree
requestGetApnsVoipSandboxChannel =
  req
    "GetApnsVoipSandboxChannel"
    "fixture/GetApnsVoipSandboxChannel.yaml"

requestGetApp :: GetApp -> TestTree
requestGetApp =
  req
    "GetApp"
    "fixture/GetApp.yaml"

requestGetApplicationDateRangeKpi :: GetApplicationDateRangeKpi -> TestTree
requestGetApplicationDateRangeKpi =
  req
    "GetApplicationDateRangeKpi"
    "fixture/GetApplicationDateRangeKpi.yaml"

requestGetApplicationSettings :: GetApplicationSettings -> TestTree
requestGetApplicationSettings =
  req
    "GetApplicationSettings"
    "fixture/GetApplicationSettings.yaml"

requestGetApps :: GetApps -> TestTree
requestGetApps =
  req
    "GetApps"
    "fixture/GetApps.yaml"

requestGetBaiduChannel :: GetBaiduChannel -> TestTree
requestGetBaiduChannel =
  req
    "GetBaiduChannel"
    "fixture/GetBaiduChannel.yaml"

requestGetCampaign :: GetCampaign -> TestTree
requestGetCampaign =
  req
    "GetCampaign"
    "fixture/GetCampaign.yaml"

requestGetCampaignActivities :: GetCampaignActivities -> TestTree
requestGetCampaignActivities =
  req
    "GetCampaignActivities"
    "fixture/GetCampaignActivities.yaml"

requestGetCampaignDateRangeKpi :: GetCampaignDateRangeKpi -> TestTree
requestGetCampaignDateRangeKpi =
  req
    "GetCampaignDateRangeKpi"
    "fixture/GetCampaignDateRangeKpi.yaml"

requestGetCampaignVersion :: GetCampaignVersion -> TestTree
requestGetCampaignVersion =
  req
    "GetCampaignVersion"
    "fixture/GetCampaignVersion.yaml"

requestGetCampaignVersions :: GetCampaignVersions -> TestTree
requestGetCampaignVersions =
  req
    "GetCampaignVersions"
    "fixture/GetCampaignVersions.yaml"

requestGetCampaigns :: GetCampaigns -> TestTree
requestGetCampaigns =
  req
    "GetCampaigns"
    "fixture/GetCampaigns.yaml"

requestGetChannels :: GetChannels -> TestTree
requestGetChannels =
  req
    "GetChannels"
    "fixture/GetChannels.yaml"

requestGetEmailChannel :: GetEmailChannel -> TestTree
requestGetEmailChannel =
  req
    "GetEmailChannel"
    "fixture/GetEmailChannel.yaml"

requestGetEmailTemplate :: GetEmailTemplate -> TestTree
requestGetEmailTemplate =
  req
    "GetEmailTemplate"
    "fixture/GetEmailTemplate.yaml"

requestGetEndpoint :: GetEndpoint -> TestTree
requestGetEndpoint =
  req
    "GetEndpoint"
    "fixture/GetEndpoint.yaml"

requestGetEventStream :: GetEventStream -> TestTree
requestGetEventStream =
  req
    "GetEventStream"
    "fixture/GetEventStream.yaml"

requestGetExportJob :: GetExportJob -> TestTree
requestGetExportJob =
  req
    "GetExportJob"
    "fixture/GetExportJob.yaml"

requestGetExportJobs :: GetExportJobs -> TestTree
requestGetExportJobs =
  req
    "GetExportJobs"
    "fixture/GetExportJobs.yaml"

requestGetGcmChannel :: GetGcmChannel -> TestTree
requestGetGcmChannel =
  req
    "GetGcmChannel"
    "fixture/GetGcmChannel.yaml"

requestGetImportJob :: GetImportJob -> TestTree
requestGetImportJob =
  req
    "GetImportJob"
    "fixture/GetImportJob.yaml"

requestGetImportJobs :: GetImportJobs -> TestTree
requestGetImportJobs =
  req
    "GetImportJobs"
    "fixture/GetImportJobs.yaml"

requestGetInAppMessages :: GetInAppMessages -> TestTree
requestGetInAppMessages =
  req
    "GetInAppMessages"
    "fixture/GetInAppMessages.yaml"

requestGetInAppTemplate :: GetInAppTemplate -> TestTree
requestGetInAppTemplate =
  req
    "GetInAppTemplate"
    "fixture/GetInAppTemplate.yaml"

requestGetJourney :: GetJourney -> TestTree
requestGetJourney =
  req
    "GetJourney"
    "fixture/GetJourney.yaml"

requestGetJourneyDateRangeKpi :: GetJourneyDateRangeKpi -> TestTree
requestGetJourneyDateRangeKpi =
  req
    "GetJourneyDateRangeKpi"
    "fixture/GetJourneyDateRangeKpi.yaml"

requestGetJourneyExecutionActivityMetrics :: GetJourneyExecutionActivityMetrics -> TestTree
requestGetJourneyExecutionActivityMetrics =
  req
    "GetJourneyExecutionActivityMetrics"
    "fixture/GetJourneyExecutionActivityMetrics.yaml"

requestGetJourneyExecutionMetrics :: GetJourneyExecutionMetrics -> TestTree
requestGetJourneyExecutionMetrics =
  req
    "GetJourneyExecutionMetrics"
    "fixture/GetJourneyExecutionMetrics.yaml"

requestGetPushTemplate :: GetPushTemplate -> TestTree
requestGetPushTemplate =
  req
    "GetPushTemplate"
    "fixture/GetPushTemplate.yaml"

requestGetRecommenderConfiguration :: GetRecommenderConfiguration -> TestTree
requestGetRecommenderConfiguration =
  req
    "GetRecommenderConfiguration"
    "fixture/GetRecommenderConfiguration.yaml"

requestGetRecommenderConfigurations :: GetRecommenderConfigurations -> TestTree
requestGetRecommenderConfigurations =
  req
    "GetRecommenderConfigurations"
    "fixture/GetRecommenderConfigurations.yaml"

requestGetSegment :: GetSegment -> TestTree
requestGetSegment =
  req
    "GetSegment"
    "fixture/GetSegment.yaml"

requestGetSegmentExportJobs :: GetSegmentExportJobs -> TestTree
requestGetSegmentExportJobs =
  req
    "GetSegmentExportJobs"
    "fixture/GetSegmentExportJobs.yaml"

requestGetSegmentImportJobs :: GetSegmentImportJobs -> TestTree
requestGetSegmentImportJobs =
  req
    "GetSegmentImportJobs"
    "fixture/GetSegmentImportJobs.yaml"

requestGetSegmentVersion :: GetSegmentVersion -> TestTree
requestGetSegmentVersion =
  req
    "GetSegmentVersion"
    "fixture/GetSegmentVersion.yaml"

requestGetSegmentVersions :: GetSegmentVersions -> TestTree
requestGetSegmentVersions =
  req
    "GetSegmentVersions"
    "fixture/GetSegmentVersions.yaml"

requestGetSegments :: GetSegments -> TestTree
requestGetSegments =
  req
    "GetSegments"
    "fixture/GetSegments.yaml"

requestGetSmsChannel :: GetSmsChannel -> TestTree
requestGetSmsChannel =
  req
    "GetSmsChannel"
    "fixture/GetSmsChannel.yaml"

requestGetSmsTemplate :: GetSmsTemplate -> TestTree
requestGetSmsTemplate =
  req
    "GetSmsTemplate"
    "fixture/GetSmsTemplate.yaml"

requestGetUserEndpoints :: GetUserEndpoints -> TestTree
requestGetUserEndpoints =
  req
    "GetUserEndpoints"
    "fixture/GetUserEndpoints.yaml"

requestGetVoiceChannel :: GetVoiceChannel -> TestTree
requestGetVoiceChannel =
  req
    "GetVoiceChannel"
    "fixture/GetVoiceChannel.yaml"

requestGetVoiceTemplate :: GetVoiceTemplate -> TestTree
requestGetVoiceTemplate =
  req
    "GetVoiceTemplate"
    "fixture/GetVoiceTemplate.yaml"

requestListJourneys :: ListJourneys -> TestTree
requestListJourneys =
  req
    "ListJourneys"
    "fixture/ListJourneys.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTemplateVersions :: ListTemplateVersions -> TestTree
requestListTemplateVersions =
  req
    "ListTemplateVersions"
    "fixture/ListTemplateVersions.yaml"

requestListTemplates :: ListTemplates -> TestTree
requestListTemplates =
  req
    "ListTemplates"
    "fixture/ListTemplates.yaml"

requestPhoneNumberValidate :: PhoneNumberValidate -> TestTree
requestPhoneNumberValidate =
  req
    "PhoneNumberValidate"
    "fixture/PhoneNumberValidate.yaml"

requestPutEventStream :: PutEventStream -> TestTree
requestPutEventStream =
  req
    "PutEventStream"
    "fixture/PutEventStream.yaml"

requestPutEvents :: PutEvents -> TestTree
requestPutEvents =
  req
    "PutEvents"
    "fixture/PutEvents.yaml"

requestRemoveAttributes :: RemoveAttributes -> TestTree
requestRemoveAttributes =
  req
    "RemoveAttributes"
    "fixture/RemoveAttributes.yaml"

requestSendMessages :: SendMessages -> TestTree
requestSendMessages =
  req
    "SendMessages"
    "fixture/SendMessages.yaml"

requestSendUsersMessages :: SendUsersMessages -> TestTree
requestSendUsersMessages =
  req
    "SendUsersMessages"
    "fixture/SendUsersMessages.yaml"

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

requestUpdateAdmChannel :: UpdateAdmChannel -> TestTree
requestUpdateAdmChannel =
  req
    "UpdateAdmChannel"
    "fixture/UpdateAdmChannel.yaml"

requestUpdateApnsChannel :: UpdateApnsChannel -> TestTree
requestUpdateApnsChannel =
  req
    "UpdateApnsChannel"
    "fixture/UpdateApnsChannel.yaml"

requestUpdateApnsSandboxChannel :: UpdateApnsSandboxChannel -> TestTree
requestUpdateApnsSandboxChannel =
  req
    "UpdateApnsSandboxChannel"
    "fixture/UpdateApnsSandboxChannel.yaml"

requestUpdateApnsVoipChannel :: UpdateApnsVoipChannel -> TestTree
requestUpdateApnsVoipChannel =
  req
    "UpdateApnsVoipChannel"
    "fixture/UpdateApnsVoipChannel.yaml"

requestUpdateApnsVoipSandboxChannel :: UpdateApnsVoipSandboxChannel -> TestTree
requestUpdateApnsVoipSandboxChannel =
  req
    "UpdateApnsVoipSandboxChannel"
    "fixture/UpdateApnsVoipSandboxChannel.yaml"

requestUpdateApplicationSettings :: UpdateApplicationSettings -> TestTree
requestUpdateApplicationSettings =
  req
    "UpdateApplicationSettings"
    "fixture/UpdateApplicationSettings.yaml"

requestUpdateBaiduChannel :: UpdateBaiduChannel -> TestTree
requestUpdateBaiduChannel =
  req
    "UpdateBaiduChannel"
    "fixture/UpdateBaiduChannel.yaml"

requestUpdateCampaign :: UpdateCampaign -> TestTree
requestUpdateCampaign =
  req
    "UpdateCampaign"
    "fixture/UpdateCampaign.yaml"

requestUpdateEmailChannel :: UpdateEmailChannel -> TestTree
requestUpdateEmailChannel =
  req
    "UpdateEmailChannel"
    "fixture/UpdateEmailChannel.yaml"

requestUpdateEmailTemplate :: UpdateEmailTemplate -> TestTree
requestUpdateEmailTemplate =
  req
    "UpdateEmailTemplate"
    "fixture/UpdateEmailTemplate.yaml"

requestUpdateEndpoint :: UpdateEndpoint -> TestTree
requestUpdateEndpoint =
  req
    "UpdateEndpoint"
    "fixture/UpdateEndpoint.yaml"

requestUpdateEndpointsBatch :: UpdateEndpointsBatch -> TestTree
requestUpdateEndpointsBatch =
  req
    "UpdateEndpointsBatch"
    "fixture/UpdateEndpointsBatch.yaml"

requestUpdateGcmChannel :: UpdateGcmChannel -> TestTree
requestUpdateGcmChannel =
  req
    "UpdateGcmChannel"
    "fixture/UpdateGcmChannel.yaml"

requestUpdateInAppTemplate :: UpdateInAppTemplate -> TestTree
requestUpdateInAppTemplate =
  req
    "UpdateInAppTemplate"
    "fixture/UpdateInAppTemplate.yaml"

requestUpdateJourney :: UpdateJourney -> TestTree
requestUpdateJourney =
  req
    "UpdateJourney"
    "fixture/UpdateJourney.yaml"

requestUpdateJourneyState :: UpdateJourneyState -> TestTree
requestUpdateJourneyState =
  req
    "UpdateJourneyState"
    "fixture/UpdateJourneyState.yaml"

requestUpdatePushTemplate :: UpdatePushTemplate -> TestTree
requestUpdatePushTemplate =
  req
    "UpdatePushTemplate"
    "fixture/UpdatePushTemplate.yaml"

requestUpdateRecommenderConfiguration :: UpdateRecommenderConfiguration' -> TestTree
requestUpdateRecommenderConfiguration =
  req
    "UpdateRecommenderConfiguration"
    "fixture/UpdateRecommenderConfiguration.yaml"

requestUpdateSegment :: UpdateSegment -> TestTree
requestUpdateSegment =
  req
    "UpdateSegment"
    "fixture/UpdateSegment.yaml"

requestUpdateSmsChannel :: UpdateSmsChannel -> TestTree
requestUpdateSmsChannel =
  req
    "UpdateSmsChannel"
    "fixture/UpdateSmsChannel.yaml"

requestUpdateSmsTemplate :: UpdateSmsTemplate -> TestTree
requestUpdateSmsTemplate =
  req
    "UpdateSmsTemplate"
    "fixture/UpdateSmsTemplate.yaml"

requestUpdateTemplateActiveVersion :: UpdateTemplateActiveVersion -> TestTree
requestUpdateTemplateActiveVersion =
  req
    "UpdateTemplateActiveVersion"
    "fixture/UpdateTemplateActiveVersion.yaml"

requestUpdateVoiceChannel :: UpdateVoiceChannel -> TestTree
requestUpdateVoiceChannel =
  req
    "UpdateVoiceChannel"
    "fixture/UpdateVoiceChannel.yaml"

requestUpdateVoiceTemplate :: UpdateVoiceTemplate -> TestTree
requestUpdateVoiceTemplate =
  req
    "UpdateVoiceTemplate"
    "fixture/UpdateVoiceTemplate.yaml"

-- Responses

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApp)

responseCreateCampaign :: CreateCampaignResponse -> TestTree
responseCreateCampaign =
  res
    "CreateCampaignResponse"
    "fixture/CreateCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCampaign)

responseCreateEmailTemplate :: CreateEmailTemplateResponse -> TestTree
responseCreateEmailTemplate =
  res
    "CreateEmailTemplateResponse"
    "fixture/CreateEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEmailTemplate)

responseCreateExportJob :: CreateExportJobResponse -> TestTree
responseCreateExportJob =
  res
    "CreateExportJobResponse"
    "fixture/CreateExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateExportJob)

responseCreateImportJob :: CreateImportJobResponse -> TestTree
responseCreateImportJob =
  res
    "CreateImportJobResponse"
    "fixture/CreateImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateImportJob)

responseCreateInAppTemplate :: CreateInAppTemplateResponse -> TestTree
responseCreateInAppTemplate =
  res
    "CreateInAppTemplateResponse"
    "fixture/CreateInAppTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInAppTemplate)

responseCreateJourney :: CreateJourneyResponse -> TestTree
responseCreateJourney =
  res
    "CreateJourneyResponse"
    "fixture/CreateJourneyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJourney)

responseCreatePushTemplate :: CreatePushTemplateResponse -> TestTree
responseCreatePushTemplate =
  res
    "CreatePushTemplateResponse"
    "fixture/CreatePushTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePushTemplate)

responseCreateRecommenderConfiguration :: CreateRecommenderConfigurationResponse -> TestTree
responseCreateRecommenderConfiguration =
  res
    "CreateRecommenderConfigurationResponse"
    "fixture/CreateRecommenderConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRecommenderConfiguration')

responseCreateSegment :: CreateSegmentResponse -> TestTree
responseCreateSegment =
  res
    "CreateSegmentResponse"
    "fixture/CreateSegmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSegment)

responseCreateSmsTemplate :: CreateSmsTemplateResponse -> TestTree
responseCreateSmsTemplate =
  res
    "CreateSmsTemplateResponse"
    "fixture/CreateSmsTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSmsTemplate)

responseCreateVoiceTemplate :: CreateVoiceTemplateResponse -> TestTree
responseCreateVoiceTemplate =
  res
    "CreateVoiceTemplateResponse"
    "fixture/CreateVoiceTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVoiceTemplate)

responseDeleteAdmChannel :: DeleteAdmChannelResponse -> TestTree
responseDeleteAdmChannel =
  res
    "DeleteAdmChannelResponse"
    "fixture/DeleteAdmChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAdmChannel)

responseDeleteApnsChannel :: DeleteApnsChannelResponse -> TestTree
responseDeleteApnsChannel =
  res
    "DeleteApnsChannelResponse"
    "fixture/DeleteApnsChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApnsChannel)

responseDeleteApnsSandboxChannel :: DeleteApnsSandboxChannelResponse -> TestTree
responseDeleteApnsSandboxChannel =
  res
    "DeleteApnsSandboxChannelResponse"
    "fixture/DeleteApnsSandboxChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApnsSandboxChannel)

responseDeleteApnsVoipChannel :: DeleteApnsVoipChannelResponse -> TestTree
responseDeleteApnsVoipChannel =
  res
    "DeleteApnsVoipChannelResponse"
    "fixture/DeleteApnsVoipChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApnsVoipChannel)

responseDeleteApnsVoipSandboxChannel :: DeleteApnsVoipSandboxChannelResponse -> TestTree
responseDeleteApnsVoipSandboxChannel =
  res
    "DeleteApnsVoipSandboxChannelResponse"
    "fixture/DeleteApnsVoipSandboxChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApnsVoipSandboxChannel)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApp)

responseDeleteBaiduChannel :: DeleteBaiduChannelResponse -> TestTree
responseDeleteBaiduChannel =
  res
    "DeleteBaiduChannelResponse"
    "fixture/DeleteBaiduChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBaiduChannel)

responseDeleteCampaign :: DeleteCampaignResponse -> TestTree
responseDeleteCampaign =
  res
    "DeleteCampaignResponse"
    "fixture/DeleteCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCampaign)

responseDeleteEmailChannel :: DeleteEmailChannelResponse -> TestTree
responseDeleteEmailChannel =
  res
    "DeleteEmailChannelResponse"
    "fixture/DeleteEmailChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEmailChannel)

responseDeleteEmailTemplate :: DeleteEmailTemplateResponse -> TestTree
responseDeleteEmailTemplate =
  res
    "DeleteEmailTemplateResponse"
    "fixture/DeleteEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEmailTemplate)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpoint)

responseDeleteEventStream :: DeleteEventStreamResponse -> TestTree
responseDeleteEventStream =
  res
    "DeleteEventStreamResponse"
    "fixture/DeleteEventStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventStream)

responseDeleteGcmChannel :: DeleteGcmChannelResponse -> TestTree
responseDeleteGcmChannel =
  res
    "DeleteGcmChannelResponse"
    "fixture/DeleteGcmChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGcmChannel)

responseDeleteInAppTemplate :: DeleteInAppTemplateResponse -> TestTree
responseDeleteInAppTemplate =
  res
    "DeleteInAppTemplateResponse"
    "fixture/DeleteInAppTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInAppTemplate)

responseDeleteJourney :: DeleteJourneyResponse -> TestTree
responseDeleteJourney =
  res
    "DeleteJourneyResponse"
    "fixture/DeleteJourneyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJourney)

responseDeletePushTemplate :: DeletePushTemplateResponse -> TestTree
responseDeletePushTemplate =
  res
    "DeletePushTemplateResponse"
    "fixture/DeletePushTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePushTemplate)

responseDeleteRecommenderConfiguration :: DeleteRecommenderConfigurationResponse -> TestTree
responseDeleteRecommenderConfiguration =
  res
    "DeleteRecommenderConfigurationResponse"
    "fixture/DeleteRecommenderConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRecommenderConfiguration)

responseDeleteSegment :: DeleteSegmentResponse -> TestTree
responseDeleteSegment =
  res
    "DeleteSegmentResponse"
    "fixture/DeleteSegmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSegment)

responseDeleteSmsChannel :: DeleteSmsChannelResponse -> TestTree
responseDeleteSmsChannel =
  res
    "DeleteSmsChannelResponse"
    "fixture/DeleteSmsChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSmsChannel)

responseDeleteSmsTemplate :: DeleteSmsTemplateResponse -> TestTree
responseDeleteSmsTemplate =
  res
    "DeleteSmsTemplateResponse"
    "fixture/DeleteSmsTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSmsTemplate)

responseDeleteUserEndpoints :: DeleteUserEndpointsResponse -> TestTree
responseDeleteUserEndpoints =
  res
    "DeleteUserEndpointsResponse"
    "fixture/DeleteUserEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserEndpoints)

responseDeleteVoiceChannel :: DeleteVoiceChannelResponse -> TestTree
responseDeleteVoiceChannel =
  res
    "DeleteVoiceChannelResponse"
    "fixture/DeleteVoiceChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceChannel)

responseDeleteVoiceTemplate :: DeleteVoiceTemplateResponse -> TestTree
responseDeleteVoiceTemplate =
  res
    "DeleteVoiceTemplateResponse"
    "fixture/DeleteVoiceTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceTemplate)

responseGetAdmChannel :: GetAdmChannelResponse -> TestTree
responseGetAdmChannel =
  res
    "GetAdmChannelResponse"
    "fixture/GetAdmChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAdmChannel)

responseGetApnsChannel :: GetApnsChannelResponse -> TestTree
responseGetApnsChannel =
  res
    "GetApnsChannelResponse"
    "fixture/GetApnsChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApnsChannel)

responseGetApnsSandboxChannel :: GetApnsSandboxChannelResponse -> TestTree
responseGetApnsSandboxChannel =
  res
    "GetApnsSandboxChannelResponse"
    "fixture/GetApnsSandboxChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApnsSandboxChannel)

responseGetApnsVoipChannel :: GetApnsVoipChannelResponse -> TestTree
responseGetApnsVoipChannel =
  res
    "GetApnsVoipChannelResponse"
    "fixture/GetApnsVoipChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApnsVoipChannel)

responseGetApnsVoipSandboxChannel :: GetApnsVoipSandboxChannelResponse -> TestTree
responseGetApnsVoipSandboxChannel =
  res
    "GetApnsVoipSandboxChannelResponse"
    "fixture/GetApnsVoipSandboxChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApnsVoipSandboxChannel)

responseGetApp :: GetAppResponse -> TestTree
responseGetApp =
  res
    "GetAppResponse"
    "fixture/GetAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApp)

responseGetApplicationDateRangeKpi :: GetApplicationDateRangeKpiResponse -> TestTree
responseGetApplicationDateRangeKpi =
  res
    "GetApplicationDateRangeKpiResponse"
    "fixture/GetApplicationDateRangeKpiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplicationDateRangeKpi)

responseGetApplicationSettings :: GetApplicationSettingsResponse -> TestTree
responseGetApplicationSettings =
  res
    "GetApplicationSettingsResponse"
    "fixture/GetApplicationSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplicationSettings)

responseGetApps :: GetAppsResponse -> TestTree
responseGetApps =
  res
    "GetAppsResponse"
    "fixture/GetAppsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApps)

responseGetBaiduChannel :: GetBaiduChannelResponse -> TestTree
responseGetBaiduChannel =
  res
    "GetBaiduChannelResponse"
    "fixture/GetBaiduChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBaiduChannel)

responseGetCampaign :: GetCampaignResponse -> TestTree
responseGetCampaign =
  res
    "GetCampaignResponse"
    "fixture/GetCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCampaign)

responseGetCampaignActivities :: GetCampaignActivitiesResponse -> TestTree
responseGetCampaignActivities =
  res
    "GetCampaignActivitiesResponse"
    "fixture/GetCampaignActivitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCampaignActivities)

responseGetCampaignDateRangeKpi :: GetCampaignDateRangeKpiResponse -> TestTree
responseGetCampaignDateRangeKpi =
  res
    "GetCampaignDateRangeKpiResponse"
    "fixture/GetCampaignDateRangeKpiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCampaignDateRangeKpi)

responseGetCampaignVersion :: GetCampaignVersionResponse -> TestTree
responseGetCampaignVersion =
  res
    "GetCampaignVersionResponse"
    "fixture/GetCampaignVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCampaignVersion)

responseGetCampaignVersions :: GetCampaignVersionsResponse -> TestTree
responseGetCampaignVersions =
  res
    "GetCampaignVersionsResponse"
    "fixture/GetCampaignVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCampaignVersions)

responseGetCampaigns :: GetCampaignsResponse -> TestTree
responseGetCampaigns =
  res
    "GetCampaignsResponse"
    "fixture/GetCampaignsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCampaigns)

responseGetChannels :: GetChannelsResponse -> TestTree
responseGetChannels =
  res
    "GetChannelsResponse"
    "fixture/GetChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChannels)

responseGetEmailChannel :: GetEmailChannelResponse -> TestTree
responseGetEmailChannel =
  res
    "GetEmailChannelResponse"
    "fixture/GetEmailChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEmailChannel)

responseGetEmailTemplate :: GetEmailTemplateResponse -> TestTree
responseGetEmailTemplate =
  res
    "GetEmailTemplateResponse"
    "fixture/GetEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEmailTemplate)

responseGetEndpoint :: GetEndpointResponse -> TestTree
responseGetEndpoint =
  res
    "GetEndpointResponse"
    "fixture/GetEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEndpoint)

responseGetEventStream :: GetEventStreamResponse -> TestTree
responseGetEventStream =
  res
    "GetEventStreamResponse"
    "fixture/GetEventStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEventStream)

responseGetExportJob :: GetExportJobResponse -> TestTree
responseGetExportJob =
  res
    "GetExportJobResponse"
    "fixture/GetExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExportJob)

responseGetExportJobs :: GetExportJobsResponse -> TestTree
responseGetExportJobs =
  res
    "GetExportJobsResponse"
    "fixture/GetExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExportJobs)

responseGetGcmChannel :: GetGcmChannelResponse -> TestTree
responseGetGcmChannel =
  res
    "GetGcmChannelResponse"
    "fixture/GetGcmChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGcmChannel)

responseGetImportJob :: GetImportJobResponse -> TestTree
responseGetImportJob =
  res
    "GetImportJobResponse"
    "fixture/GetImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetImportJob)

responseGetImportJobs :: GetImportJobsResponse -> TestTree
responseGetImportJobs =
  res
    "GetImportJobsResponse"
    "fixture/GetImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetImportJobs)

responseGetInAppMessages :: GetInAppMessagesResponse -> TestTree
responseGetInAppMessages =
  res
    "GetInAppMessagesResponse"
    "fixture/GetInAppMessagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInAppMessages)

responseGetInAppTemplate :: GetInAppTemplateResponse -> TestTree
responseGetInAppTemplate =
  res
    "GetInAppTemplateResponse"
    "fixture/GetInAppTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInAppTemplate)

responseGetJourney :: GetJourneyResponse -> TestTree
responseGetJourney =
  res
    "GetJourneyResponse"
    "fixture/GetJourneyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJourney)

responseGetJourneyDateRangeKpi :: GetJourneyDateRangeKpiResponse -> TestTree
responseGetJourneyDateRangeKpi =
  res
    "GetJourneyDateRangeKpiResponse"
    "fixture/GetJourneyDateRangeKpiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJourneyDateRangeKpi)

responseGetJourneyExecutionActivityMetrics :: GetJourneyExecutionActivityMetricsResponse -> TestTree
responseGetJourneyExecutionActivityMetrics =
  res
    "GetJourneyExecutionActivityMetricsResponse"
    "fixture/GetJourneyExecutionActivityMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJourneyExecutionActivityMetrics)

responseGetJourneyExecutionMetrics :: GetJourneyExecutionMetricsResponse -> TestTree
responseGetJourneyExecutionMetrics =
  res
    "GetJourneyExecutionMetricsResponse"
    "fixture/GetJourneyExecutionMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJourneyExecutionMetrics)

responseGetPushTemplate :: GetPushTemplateResponse -> TestTree
responseGetPushTemplate =
  res
    "GetPushTemplateResponse"
    "fixture/GetPushTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPushTemplate)

responseGetRecommenderConfiguration :: GetRecommenderConfigurationResponse -> TestTree
responseGetRecommenderConfiguration =
  res
    "GetRecommenderConfigurationResponse"
    "fixture/GetRecommenderConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecommenderConfiguration)

responseGetRecommenderConfigurations :: GetRecommenderConfigurationsResponse -> TestTree
responseGetRecommenderConfigurations =
  res
    "GetRecommenderConfigurationsResponse"
    "fixture/GetRecommenderConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecommenderConfigurations)

responseGetSegment :: GetSegmentResponse -> TestTree
responseGetSegment =
  res
    "GetSegmentResponse"
    "fixture/GetSegmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSegment)

responseGetSegmentExportJobs :: GetSegmentExportJobsResponse -> TestTree
responseGetSegmentExportJobs =
  res
    "GetSegmentExportJobsResponse"
    "fixture/GetSegmentExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSegmentExportJobs)

responseGetSegmentImportJobs :: GetSegmentImportJobsResponse -> TestTree
responseGetSegmentImportJobs =
  res
    "GetSegmentImportJobsResponse"
    "fixture/GetSegmentImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSegmentImportJobs)

responseGetSegmentVersion :: GetSegmentVersionResponse -> TestTree
responseGetSegmentVersion =
  res
    "GetSegmentVersionResponse"
    "fixture/GetSegmentVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSegmentVersion)

responseGetSegmentVersions :: GetSegmentVersionsResponse -> TestTree
responseGetSegmentVersions =
  res
    "GetSegmentVersionsResponse"
    "fixture/GetSegmentVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSegmentVersions)

responseGetSegments :: GetSegmentsResponse -> TestTree
responseGetSegments =
  res
    "GetSegmentsResponse"
    "fixture/GetSegmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSegments)

responseGetSmsChannel :: GetSmsChannelResponse -> TestTree
responseGetSmsChannel =
  res
    "GetSmsChannelResponse"
    "fixture/GetSmsChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSmsChannel)

responseGetSmsTemplate :: GetSmsTemplateResponse -> TestTree
responseGetSmsTemplate =
  res
    "GetSmsTemplateResponse"
    "fixture/GetSmsTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSmsTemplate)

responseGetUserEndpoints :: GetUserEndpointsResponse -> TestTree
responseGetUserEndpoints =
  res
    "GetUserEndpointsResponse"
    "fixture/GetUserEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUserEndpoints)

responseGetVoiceChannel :: GetVoiceChannelResponse -> TestTree
responseGetVoiceChannel =
  res
    "GetVoiceChannelResponse"
    "fixture/GetVoiceChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceChannel)

responseGetVoiceTemplate :: GetVoiceTemplateResponse -> TestTree
responseGetVoiceTemplate =
  res
    "GetVoiceTemplateResponse"
    "fixture/GetVoiceTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceTemplate)

responseListJourneys :: ListJourneysResponse -> TestTree
responseListJourneys =
  res
    "ListJourneysResponse"
    "fixture/ListJourneysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJourneys)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTemplateVersions :: ListTemplateVersionsResponse -> TestTree
responseListTemplateVersions =
  res
    "ListTemplateVersionsResponse"
    "fixture/ListTemplateVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTemplateVersions)

responseListTemplates :: ListTemplatesResponse -> TestTree
responseListTemplates =
  res
    "ListTemplatesResponse"
    "fixture/ListTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTemplates)

responsePhoneNumberValidate :: PhoneNumberValidateResponse -> TestTree
responsePhoneNumberValidate =
  res
    "PhoneNumberValidateResponse"
    "fixture/PhoneNumberValidateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PhoneNumberValidate)

responsePutEventStream :: PutEventStreamResponse -> TestTree
responsePutEventStream =
  res
    "PutEventStreamResponse"
    "fixture/PutEventStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEventStream)

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents =
  res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEvents)

responseRemoveAttributes :: RemoveAttributesResponse -> TestTree
responseRemoveAttributes =
  res
    "RemoveAttributesResponse"
    "fixture/RemoveAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveAttributes)

responseSendMessages :: SendMessagesResponse -> TestTree
responseSendMessages =
  res
    "SendMessagesResponse"
    "fixture/SendMessagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendMessages)

responseSendUsersMessages :: SendUsersMessagesResponse -> TestTree
responseSendUsersMessages =
  res
    "SendUsersMessagesResponse"
    "fixture/SendUsersMessagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendUsersMessages)

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

responseUpdateAdmChannel :: UpdateAdmChannelResponse -> TestTree
responseUpdateAdmChannel =
  res
    "UpdateAdmChannelResponse"
    "fixture/UpdateAdmChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAdmChannel)

responseUpdateApnsChannel :: UpdateApnsChannelResponse -> TestTree
responseUpdateApnsChannel =
  res
    "UpdateApnsChannelResponse"
    "fixture/UpdateApnsChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApnsChannel)

responseUpdateApnsSandboxChannel :: UpdateApnsSandboxChannelResponse -> TestTree
responseUpdateApnsSandboxChannel =
  res
    "UpdateApnsSandboxChannelResponse"
    "fixture/UpdateApnsSandboxChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApnsSandboxChannel)

responseUpdateApnsVoipChannel :: UpdateApnsVoipChannelResponse -> TestTree
responseUpdateApnsVoipChannel =
  res
    "UpdateApnsVoipChannelResponse"
    "fixture/UpdateApnsVoipChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApnsVoipChannel)

responseUpdateApnsVoipSandboxChannel :: UpdateApnsVoipSandboxChannelResponse -> TestTree
responseUpdateApnsVoipSandboxChannel =
  res
    "UpdateApnsVoipSandboxChannelResponse"
    "fixture/UpdateApnsVoipSandboxChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApnsVoipSandboxChannel)

responseUpdateApplicationSettings :: UpdateApplicationSettingsResponse -> TestTree
responseUpdateApplicationSettings =
  res
    "UpdateApplicationSettingsResponse"
    "fixture/UpdateApplicationSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplicationSettings)

responseUpdateBaiduChannel :: UpdateBaiduChannelResponse -> TestTree
responseUpdateBaiduChannel =
  res
    "UpdateBaiduChannelResponse"
    "fixture/UpdateBaiduChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBaiduChannel)

responseUpdateCampaign :: UpdateCampaignResponse -> TestTree
responseUpdateCampaign =
  res
    "UpdateCampaignResponse"
    "fixture/UpdateCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCampaign)

responseUpdateEmailChannel :: UpdateEmailChannelResponse -> TestTree
responseUpdateEmailChannel =
  res
    "UpdateEmailChannelResponse"
    "fixture/UpdateEmailChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEmailChannel)

responseUpdateEmailTemplate :: UpdateEmailTemplateResponse -> TestTree
responseUpdateEmailTemplate =
  res
    "UpdateEmailTemplateResponse"
    "fixture/UpdateEmailTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEmailTemplate)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEndpoint)

responseUpdateEndpointsBatch :: UpdateEndpointsBatchResponse -> TestTree
responseUpdateEndpointsBatch =
  res
    "UpdateEndpointsBatchResponse"
    "fixture/UpdateEndpointsBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEndpointsBatch)

responseUpdateGcmChannel :: UpdateGcmChannelResponse -> TestTree
responseUpdateGcmChannel =
  res
    "UpdateGcmChannelResponse"
    "fixture/UpdateGcmChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGcmChannel)

responseUpdateInAppTemplate :: UpdateInAppTemplateResponse -> TestTree
responseUpdateInAppTemplate =
  res
    "UpdateInAppTemplateResponse"
    "fixture/UpdateInAppTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInAppTemplate)

responseUpdateJourney :: UpdateJourneyResponse -> TestTree
responseUpdateJourney =
  res
    "UpdateJourneyResponse"
    "fixture/UpdateJourneyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateJourney)

responseUpdateJourneyState :: UpdateJourneyStateResponse -> TestTree
responseUpdateJourneyState =
  res
    "UpdateJourneyStateResponse"
    "fixture/UpdateJourneyStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateJourneyState)

responseUpdatePushTemplate :: UpdatePushTemplateResponse -> TestTree
responseUpdatePushTemplate =
  res
    "UpdatePushTemplateResponse"
    "fixture/UpdatePushTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePushTemplate)

responseUpdateRecommenderConfiguration :: UpdateRecommenderConfigurationResponse -> TestTree
responseUpdateRecommenderConfiguration =
  res
    "UpdateRecommenderConfigurationResponse"
    "fixture/UpdateRecommenderConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRecommenderConfiguration')

responseUpdateSegment :: UpdateSegmentResponse -> TestTree
responseUpdateSegment =
  res
    "UpdateSegmentResponse"
    "fixture/UpdateSegmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSegment)

responseUpdateSmsChannel :: UpdateSmsChannelResponse -> TestTree
responseUpdateSmsChannel =
  res
    "UpdateSmsChannelResponse"
    "fixture/UpdateSmsChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSmsChannel)

responseUpdateSmsTemplate :: UpdateSmsTemplateResponse -> TestTree
responseUpdateSmsTemplate =
  res
    "UpdateSmsTemplateResponse"
    "fixture/UpdateSmsTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSmsTemplate)

responseUpdateTemplateActiveVersion :: UpdateTemplateActiveVersionResponse -> TestTree
responseUpdateTemplateActiveVersion =
  res
    "UpdateTemplateActiveVersionResponse"
    "fixture/UpdateTemplateActiveVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTemplateActiveVersion)

responseUpdateVoiceChannel :: UpdateVoiceChannelResponse -> TestTree
responseUpdateVoiceChannel =
  res
    "UpdateVoiceChannelResponse"
    "fixture/UpdateVoiceChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVoiceChannel)

responseUpdateVoiceTemplate :: UpdateVoiceTemplateResponse -> TestTree
responseUpdateVoiceTemplate =
  res
    "UpdateVoiceTemplateResponse"
    "fixture/UpdateVoiceTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVoiceTemplate)
