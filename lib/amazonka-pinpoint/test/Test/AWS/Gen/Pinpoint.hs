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
--         [ requestGetGcmChannel $
--             mkGetGcmChannel
--
--         , requestGetSegmentImportJobs $
--             mkGetSegmentImportJobs
--
--         , requestSendMessages $
--             mkSendMessages
--
--         , requestGetImportJob $
--             mkGetImportJob
--
--         , requestDeleteSmsTemplate $
--             mkDeleteSmsTemplate
--
--         , requestUpdateSmsTemplate $
--             mkUpdateSmsTemplate
--
--         , requestGetApnsVoipSandboxChannel $
--             mkGetApnsVoipSandboxChannel
--
--         , requestGetSegmentVersions $
--             mkGetSegmentVersions
--
--         , requestDeleteCampaign $
--             mkDeleteCampaign
--
--         , requestUpdateCampaign $
--             mkUpdateCampaign
--
--         , requestGetSegmentVersion $
--             mkGetSegmentVersion
--
--         , requestDeletePushTemplate $
--             mkDeletePushTemplate
--
--         , requestUpdatePushTemplate $
--             mkUpdatePushTemplate
--
--         , requestCreateExportJob $
--             mkCreateExportJob
--
--         , requestCreateSegment $
--             mkCreateSegment
--
--         , requestCreateRecommenderConfiguration $
--             mkCreateRecommenderConfiguration
--
--         , requestCreateVoiceTemplate $
--             mkCreateVoiceTemplate
--
--         , requestUpdateAdmChannel $
--             mkUpdateAdmChannel
--
--         , requestDeleteAdmChannel $
--             mkDeleteAdmChannel
--
--         , requestDeleteRecommenderConfiguration $
--             mkDeleteRecommenderConfiguration
--
--         , requestUpdateRecommenderConfiguration $
--             mkUpdateRecommenderConfiguration
--
--         , requestCreatePushTemplate $
--             mkCreatePushTemplate
--
--         , requestDeleteEndpoint $
--             mkDeleteEndpoint
--
--         , requestUpdateEndpoint $
--             mkUpdateEndpoint
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestCreateCampaign $
--             mkCreateCampaign
--
--         , requestGetEmailTemplate $
--             mkGetEmailTemplate
--
--         , requestGetExportJob $
--             mkGetExportJob
--
--         , requestGetEndpoint $
--             mkGetEndpoint
--
--         , requestGetSegment $
--             mkGetSegment
--
--         , requestGetRecommenderConfiguration $
--             mkGetRecommenderConfiguration
--
--         , requestUpdateEndpointsBatch $
--             mkUpdateEndpointsBatch
--
--         , requestGetAdmChannel $
--             mkGetAdmChannel
--
--         , requestGetCampaign $
--             mkGetCampaign
--
--         , requestGetVoiceTemplate $
--             mkGetVoiceTemplate
--
--         , requestGetPushTemplate $
--             mkGetPushTemplate
--
--         , requestDeleteUserEndpoints $
--             mkDeleteUserEndpoints
--
--         , requestCreateEmailTemplate $
--             mkCreateEmailTemplate
--
--         , requestDeleteApp $
--             mkDeleteApp
--
--         , requestUpdateApnsVoipSandboxChannel $
--             mkUpdateApnsVoipSandboxChannel
--
--         , requestDeleteApnsVoipSandboxChannel $
--             mkDeleteApnsVoipSandboxChannel
--
--         , requestUpdateGcmChannel $
--             mkUpdateGcmChannel
--
--         , requestDeleteGcmChannel $
--             mkDeleteGcmChannel
--
--         , requestGetCampaignActivities $
--             mkGetCampaignActivities
--
--         , requestGetJourneyExecutionMetrics $
--             mkGetJourneyExecutionMetrics
--
--         , requestUpdateJourneyState $
--             mkUpdateJourneyState
--
--         , requestGetEventStream $
--             mkGetEventStream
--
--         , requestGetChannels $
--             mkGetChannels
--
--         , requestGetJourney $
--             mkGetJourney
--
--         , requestDeleteEmailChannel $
--             mkDeleteEmailChannel
--
--         , requestUpdateEmailChannel $
--             mkUpdateEmailChannel
--
--         , requestGetBaiduChannel $
--             mkGetBaiduChannel
--
--         , requestDeleteApnsChannel $
--             mkDeleteApnsChannel
--
--         , requestUpdateApnsChannel $
--             mkUpdateApnsChannel
--
--         , requestRemoveAttributes $
--             mkRemoveAttributes
--
--         , requestListTemplates $
--             mkListTemplates
--
--         , requestPutEventStream $
--             mkPutEventStream
--
--         , requestDeleteEventStream $
--             mkDeleteEventStream
--
--         , requestGetCampaignVersions $
--             mkGetCampaignVersions
--
--         , requestDeleteJourney $
--             mkDeleteJourney
--
--         , requestUpdateJourney $
--             mkUpdateJourney
--
--         , requestGetCampaignDateRangeKpi $
--             mkGetCampaignDateRangeKpi
--
--         , requestGetApnsChannel $
--             mkGetApnsChannel
--
--         , requestUpdateVoiceChannel $
--             mkUpdateVoiceChannel
--
--         , requestDeleteVoiceChannel $
--             mkDeleteVoiceChannel
--
--         , requestGetApps $
--             mkGetApps
--
--         , requestGetApnsSandboxChannel $
--             mkGetApnsSandboxChannel
--
--         , requestCreateJourney $
--             mkCreateJourney
--
--         , requestGetUserEndpoints $
--             mkGetUserEndpoints
--
--         , requestDeleteVoiceTemplate $
--             mkDeleteVoiceTemplate
--
--         , requestUpdateVoiceTemplate $
--             mkUpdateVoiceTemplate
--
--         , requestGetImportJobs $
--             mkGetImportJobs
--
--         , requestGetJourneyDateRangeKpi $
--             mkGetJourneyDateRangeKpi
--
--         , requestUpdateTemplateActiveVersion $
--             mkUpdateTemplateActiveVersion
--
--         , requestDeleteSmsChannel $
--             mkDeleteSmsChannel
--
--         , requestUpdateSmsChannel $
--             mkUpdateSmsChannel
--
--         , requestGetApp $
--             mkGetApp
--
--         , requestGetCampaignVersion $
--             mkGetCampaignVersion
--
--         , requestDeleteSegment $
--             mkDeleteSegment
--
--         , requestUpdateSegment $
--             mkUpdateSegment
--
--         , requestGetApplicationDateRangeKpi $
--             mkGetApplicationDateRangeKpi
--
--         , requestCreateApp $
--             mkCreateApp
--
--         , requestGetSegmentExportJobs $
--             mkGetSegmentExportJobs
--
--         , requestDeleteEmailTemplate $
--             mkDeleteEmailTemplate
--
--         , requestUpdateEmailTemplate $
--             mkUpdateEmailTemplate
--
--         , requestGetSmsChannel $
--             mkGetSmsChannel
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestDeleteApnsSandboxChannel $
--             mkDeleteApnsSandboxChannel
--
--         , requestUpdateApnsSandboxChannel $
--             mkUpdateApnsSandboxChannel
--
--         , requestGetCampaigns $
--             mkGetCampaigns
--
--         , requestGetVoiceChannel $
--             mkGetVoiceChannel
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestListTemplateVersions $
--             mkListTemplateVersions
--
--         , requestGetSmsTemplate $
--             mkGetSmsTemplate
--
--         , requestPutEvents $
--             mkPutEvents
--
--         , requestUpdateApplicationSettings $
--             mkUpdateApplicationSettings
--
--         , requestGetJourneyExecutionActivityMetrics $
--             mkGetJourneyExecutionActivityMetrics
--
--         , requestGetSegments $
--             mkGetSegments
--
--         , requestGetExportJobs $
--             mkGetExportJobs
--
--         , requestCreateImportJob $
--             mkCreateImportJob
--
--         , requestGetRecommenderConfigurations $
--             mkGetRecommenderConfigurations
--
--         , requestDeleteApnsVoipChannel $
--             mkDeleteApnsVoipChannel
--
--         , requestUpdateApnsVoipChannel $
--             mkUpdateApnsVoipChannel
--
--         , requestSendUsersMessages $
--             mkSendUsersMessages
--
--         , requestGetApplicationSettings $
--             mkGetApplicationSettings
--
--         , requestDeleteBaiduChannel $
--             mkDeleteBaiduChannel
--
--         , requestUpdateBaiduChannel $
--             mkUpdateBaiduChannel
--
--         , requestCreateSmsTemplate $
--             mkCreateSmsTemplate
--
--         , requestPhoneNumberValidate $
--             mkPhoneNumberValidate
--
--         , requestListJourneys $
--             mkListJourneys
--
--         , requestGetApnsVoipChannel $
--             mkGetApnsVoipChannel
--
--         , requestGetEmailChannel $
--             mkGetEmailChannel
--
--           ]

--     , testGroup "response"
--         [ responseGetGcmChannel $
--             mkGetGcmChannelResponse
--
--         , responseGetSegmentImportJobs $
--             mkGetSegmentImportJobsResponse
--
--         , responseSendMessages $
--             mkSendMessagesResponse
--
--         , responseGetImportJob $
--             mkGetImportJobResponse
--
--         , responseDeleteSmsTemplate $
--             mkDeleteSmsTemplateResponse
--
--         , responseUpdateSmsTemplate $
--             mkUpdateSmsTemplateResponse
--
--         , responseGetApnsVoipSandboxChannel $
--             mkGetApnsVoipSandboxChannelResponse
--
--         , responseGetSegmentVersions $
--             mkGetSegmentVersionsResponse
--
--         , responseDeleteCampaign $
--             mkDeleteCampaignResponse
--
--         , responseUpdateCampaign $
--             mkUpdateCampaignResponse
--
--         , responseGetSegmentVersion $
--             mkGetSegmentVersionResponse
--
--         , responseDeletePushTemplate $
--             mkDeletePushTemplateResponse
--
--         , responseUpdatePushTemplate $
--             mkUpdatePushTemplateResponse
--
--         , responseCreateExportJob $
--             mkCreateExportJobResponse
--
--         , responseCreateSegment $
--             mkCreateSegmentResponse
--
--         , responseCreateRecommenderConfiguration $
--             mkCreateRecommenderConfigurationResponse
--
--         , responseCreateVoiceTemplate $
--             mkCreateVoiceTemplateResponse
--
--         , responseUpdateAdmChannel $
--             mkUpdateAdmChannelResponse
--
--         , responseDeleteAdmChannel $
--             mkDeleteAdmChannelResponse
--
--         , responseDeleteRecommenderConfiguration $
--             mkDeleteRecommenderConfigurationResponse
--
--         , responseUpdateRecommenderConfiguration $
--             mkUpdateRecommenderConfigurationResponse
--
--         , responseCreatePushTemplate $
--             mkCreatePushTemplateResponse
--
--         , responseDeleteEndpoint $
--             mkDeleteEndpointResponse
--
--         , responseUpdateEndpoint $
--             mkUpdateEndpointResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseCreateCampaign $
--             mkCreateCampaignResponse
--
--         , responseGetEmailTemplate $
--             mkGetEmailTemplateResponse
--
--         , responseGetExportJob $
--             mkGetExportJobResponse
--
--         , responseGetEndpoint $
--             mkGetEndpointResponse
--
--         , responseGetSegment $
--             mkGetSegmentResponse
--
--         , responseGetRecommenderConfiguration $
--             mkGetRecommenderConfigurationResponse
--
--         , responseUpdateEndpointsBatch $
--             mkUpdateEndpointsBatchResponse
--
--         , responseGetAdmChannel $
--             mkGetAdmChannelResponse
--
--         , responseGetCampaign $
--             mkGetCampaignResponse
--
--         , responseGetVoiceTemplate $
--             mkGetVoiceTemplateResponse
--
--         , responseGetPushTemplate $
--             mkGetPushTemplateResponse
--
--         , responseDeleteUserEndpoints $
--             mkDeleteUserEndpointsResponse
--
--         , responseCreateEmailTemplate $
--             mkCreateEmailTemplateResponse
--
--         , responseDeleteApp $
--             mkDeleteAppResponse
--
--         , responseUpdateApnsVoipSandboxChannel $
--             mkUpdateApnsVoipSandboxChannelResponse
--
--         , responseDeleteApnsVoipSandboxChannel $
--             mkDeleteApnsVoipSandboxChannelResponse
--
--         , responseUpdateGcmChannel $
--             mkUpdateGcmChannelResponse
--
--         , responseDeleteGcmChannel $
--             mkDeleteGcmChannelResponse
--
--         , responseGetCampaignActivities $
--             mkGetCampaignActivitiesResponse
--
--         , responseGetJourneyExecutionMetrics $
--             mkGetJourneyExecutionMetricsResponse
--
--         , responseUpdateJourneyState $
--             mkUpdateJourneyStateResponse
--
--         , responseGetEventStream $
--             mkGetEventStreamResponse
--
--         , responseGetChannels $
--             mkGetChannelsResponse
--
--         , responseGetJourney $
--             mkGetJourneyResponse
--
--         , responseDeleteEmailChannel $
--             mkDeleteEmailChannelResponse
--
--         , responseUpdateEmailChannel $
--             mkUpdateEmailChannelResponse
--
--         , responseGetBaiduChannel $
--             mkGetBaiduChannelResponse
--
--         , responseDeleteApnsChannel $
--             mkDeleteApnsChannelResponse
--
--         , responseUpdateApnsChannel $
--             mkUpdateApnsChannelResponse
--
--         , responseRemoveAttributes $
--             mkRemoveAttributesResponse
--
--         , responseListTemplates $
--             mkListTemplatesResponse
--
--         , responsePutEventStream $
--             mkPutEventStreamResponse
--
--         , responseDeleteEventStream $
--             mkDeleteEventStreamResponse
--
--         , responseGetCampaignVersions $
--             mkGetCampaignVersionsResponse
--
--         , responseDeleteJourney $
--             mkDeleteJourneyResponse
--
--         , responseUpdateJourney $
--             mkUpdateJourneyResponse
--
--         , responseGetCampaignDateRangeKpi $
--             mkGetCampaignDateRangeKpiResponse
--
--         , responseGetApnsChannel $
--             mkGetApnsChannelResponse
--
--         , responseUpdateVoiceChannel $
--             mkUpdateVoiceChannelResponse
--
--         , responseDeleteVoiceChannel $
--             mkDeleteVoiceChannelResponse
--
--         , responseGetApps $
--             mkGetAppsResponse
--
--         , responseGetApnsSandboxChannel $
--             mkGetApnsSandboxChannelResponse
--
--         , responseCreateJourney $
--             mkCreateJourneyResponse
--
--         , responseGetUserEndpoints $
--             mkGetUserEndpointsResponse
--
--         , responseDeleteVoiceTemplate $
--             mkDeleteVoiceTemplateResponse
--
--         , responseUpdateVoiceTemplate $
--             mkUpdateVoiceTemplateResponse
--
--         , responseGetImportJobs $
--             mkGetImportJobsResponse
--
--         , responseGetJourneyDateRangeKpi $
--             mkGetJourneyDateRangeKpiResponse
--
--         , responseUpdateTemplateActiveVersion $
--             mkUpdateTemplateActiveVersionResponse
--
--         , responseDeleteSmsChannel $
--             mkDeleteSmsChannelResponse
--
--         , responseUpdateSmsChannel $
--             mkUpdateSmsChannelResponse
--
--         , responseGetApp $
--             mkGetAppResponse
--
--         , responseGetCampaignVersion $
--             mkGetCampaignVersionResponse
--
--         , responseDeleteSegment $
--             mkDeleteSegmentResponse
--
--         , responseUpdateSegment $
--             mkUpdateSegmentResponse
--
--         , responseGetApplicationDateRangeKpi $
--             mkGetApplicationDateRangeKpiResponse
--
--         , responseCreateApp $
--             mkCreateAppResponse
--
--         , responseGetSegmentExportJobs $
--             mkGetSegmentExportJobsResponse
--
--         , responseDeleteEmailTemplate $
--             mkDeleteEmailTemplateResponse
--
--         , responseUpdateEmailTemplate $
--             mkUpdateEmailTemplateResponse
--
--         , responseGetSmsChannel $
--             mkGetSmsChannelResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseDeleteApnsSandboxChannel $
--             mkDeleteApnsSandboxChannelResponse
--
--         , responseUpdateApnsSandboxChannel $
--             mkUpdateApnsSandboxChannelResponse
--
--         , responseGetCampaigns $
--             mkGetCampaignsResponse
--
--         , responseGetVoiceChannel $
--             mkGetVoiceChannelResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseListTemplateVersions $
--             mkListTemplateVersionsResponse
--
--         , responseGetSmsTemplate $
--             mkGetSmsTemplateResponse
--
--         , responsePutEvents $
--             mkPutEventsResponse
--
--         , responseUpdateApplicationSettings $
--             mkUpdateApplicationSettingsResponse
--
--         , responseGetJourneyExecutionActivityMetrics $
--             mkGetJourneyExecutionActivityMetricsResponse
--
--         , responseGetSegments $
--             mkGetSegmentsResponse
--
--         , responseGetExportJobs $
--             mkGetExportJobsResponse
--
--         , responseCreateImportJob $
--             mkCreateImportJobResponse
--
--         , responseGetRecommenderConfigurations $
--             mkGetRecommenderConfigurationsResponse
--
--         , responseDeleteApnsVoipChannel $
--             mkDeleteApnsVoipChannelResponse
--
--         , responseUpdateApnsVoipChannel $
--             mkUpdateApnsVoipChannelResponse
--
--         , responseSendUsersMessages $
--             mkSendUsersMessagesResponse
--
--         , responseGetApplicationSettings $
--             mkGetApplicationSettingsResponse
--
--         , responseDeleteBaiduChannel $
--             mkDeleteBaiduChannelResponse
--
--         , responseUpdateBaiduChannel $
--             mkUpdateBaiduChannelResponse
--
--         , responseCreateSmsTemplate $
--             mkCreateSmsTemplateResponse
--
--         , responsePhoneNumberValidate $
--             mkPhoneNumberValidateResponse
--
--         , responseListJourneys $
--             mkListJourneysResponse
--
--         , responseGetApnsVoipChannel $
--             mkGetApnsVoipChannelResponse
--
--         , responseGetEmailChannel $
--             mkGetEmailChannelResponse
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
    mkServiceConfig
    (Proxy :: Proxy GetGcmChannel)

responseGetSegmentImportJobs :: GetSegmentImportJobsResponse -> TestTree
responseGetSegmentImportJobs =
  res
    "GetSegmentImportJobsResponse"
    "fixture/GetSegmentImportJobsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSegmentImportJobs)

responseSendMessages :: SendMessagesResponse -> TestTree
responseSendMessages =
  res
    "SendMessagesResponse"
    "fixture/SendMessagesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SendMessages)

responseGetImportJob :: GetImportJobResponse -> TestTree
responseGetImportJob =
  res
    "GetImportJobResponse"
    "fixture/GetImportJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetImportJob)

responseDeleteSmsTemplate :: DeleteSmsTemplateResponse -> TestTree
responseDeleteSmsTemplate =
  res
    "DeleteSmsTemplateResponse"
    "fixture/DeleteSmsTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSmsTemplate)

responseUpdateSmsTemplate :: UpdateSmsTemplateResponse -> TestTree
responseUpdateSmsTemplate =
  res
    "UpdateSmsTemplateResponse"
    "fixture/UpdateSmsTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSmsTemplate)

responseGetApnsVoipSandboxChannel :: GetApnsVoipSandboxChannelResponse -> TestTree
responseGetApnsVoipSandboxChannel =
  res
    "GetApnsVoipSandboxChannelResponse"
    "fixture/GetApnsVoipSandboxChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetApnsVoipSandboxChannel)

responseGetSegmentVersions :: GetSegmentVersionsResponse -> TestTree
responseGetSegmentVersions =
  res
    "GetSegmentVersionsResponse"
    "fixture/GetSegmentVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSegmentVersions)

responseDeleteCampaign :: DeleteCampaignResponse -> TestTree
responseDeleteCampaign =
  res
    "DeleteCampaignResponse"
    "fixture/DeleteCampaignResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCampaign)

responseUpdateCampaign :: UpdateCampaignResponse -> TestTree
responseUpdateCampaign =
  res
    "UpdateCampaignResponse"
    "fixture/UpdateCampaignResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateCampaign)

responseGetSegmentVersion :: GetSegmentVersionResponse -> TestTree
responseGetSegmentVersion =
  res
    "GetSegmentVersionResponse"
    "fixture/GetSegmentVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSegmentVersion)

responseDeletePushTemplate :: DeletePushTemplateResponse -> TestTree
responseDeletePushTemplate =
  res
    "DeletePushTemplateResponse"
    "fixture/DeletePushTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeletePushTemplate)

responseUpdatePushTemplate :: UpdatePushTemplateResponse -> TestTree
responseUpdatePushTemplate =
  res
    "UpdatePushTemplateResponse"
    "fixture/UpdatePushTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdatePushTemplate)

responseCreateExportJob :: CreateExportJobResponse -> TestTree
responseCreateExportJob =
  res
    "CreateExportJobResponse"
    "fixture/CreateExportJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateExportJob)

responseCreateSegment :: CreateSegmentResponse -> TestTree
responseCreateSegment =
  res
    "CreateSegmentResponse"
    "fixture/CreateSegmentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSegment)

responseCreateRecommenderConfiguration :: CreateRecommenderConfigurationResponse -> TestTree
responseCreateRecommenderConfiguration =
  res
    "CreateRecommenderConfigurationResponse"
    "fixture/CreateRecommenderConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRecommenderConfiguration)

responseCreateVoiceTemplate :: CreateVoiceTemplateResponse -> TestTree
responseCreateVoiceTemplate =
  res
    "CreateVoiceTemplateResponse"
    "fixture/CreateVoiceTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateVoiceTemplate)

responseUpdateAdmChannel :: UpdateAdmChannelResponse -> TestTree
responseUpdateAdmChannel =
  res
    "UpdateAdmChannelResponse"
    "fixture/UpdateAdmChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAdmChannel)

responseDeleteAdmChannel :: DeleteAdmChannelResponse -> TestTree
responseDeleteAdmChannel =
  res
    "DeleteAdmChannelResponse"
    "fixture/DeleteAdmChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAdmChannel)

responseDeleteRecommenderConfiguration :: DeleteRecommenderConfigurationResponse -> TestTree
responseDeleteRecommenderConfiguration =
  res
    "DeleteRecommenderConfigurationResponse"
    "fixture/DeleteRecommenderConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRecommenderConfiguration)

responseUpdateRecommenderConfiguration :: UpdateRecommenderConfigurationResponse -> TestTree
responseUpdateRecommenderConfiguration =
  res
    "UpdateRecommenderConfigurationResponse"
    "fixture/UpdateRecommenderConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRecommenderConfiguration)

responseCreatePushTemplate :: CreatePushTemplateResponse -> TestTree
responseCreatePushTemplate =
  res
    "CreatePushTemplateResponse"
    "fixture/CreatePushTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreatePushTemplate)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteEndpoint)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateEndpoint)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseCreateCampaign :: CreateCampaignResponse -> TestTree
responseCreateCampaign =
  res
    "CreateCampaignResponse"
    "fixture/CreateCampaignResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCampaign)

responseGetEmailTemplate :: GetEmailTemplateResponse -> TestTree
responseGetEmailTemplate =
  res
    "GetEmailTemplateResponse"
    "fixture/GetEmailTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetEmailTemplate)

responseGetExportJob :: GetExportJobResponse -> TestTree
responseGetExportJob =
  res
    "GetExportJobResponse"
    "fixture/GetExportJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetExportJob)

responseGetEndpoint :: GetEndpointResponse -> TestTree
responseGetEndpoint =
  res
    "GetEndpointResponse"
    "fixture/GetEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetEndpoint)

responseGetSegment :: GetSegmentResponse -> TestTree
responseGetSegment =
  res
    "GetSegmentResponse"
    "fixture/GetSegmentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSegment)

responseGetRecommenderConfiguration :: GetRecommenderConfigurationResponse -> TestTree
responseGetRecommenderConfiguration =
  res
    "GetRecommenderConfigurationResponse"
    "fixture/GetRecommenderConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRecommenderConfiguration)

responseUpdateEndpointsBatch :: UpdateEndpointsBatchResponse -> TestTree
responseUpdateEndpointsBatch =
  res
    "UpdateEndpointsBatchResponse"
    "fixture/UpdateEndpointsBatchResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateEndpointsBatch)

responseGetAdmChannel :: GetAdmChannelResponse -> TestTree
responseGetAdmChannel =
  res
    "GetAdmChannelResponse"
    "fixture/GetAdmChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAdmChannel)

responseGetCampaign :: GetCampaignResponse -> TestTree
responseGetCampaign =
  res
    "GetCampaignResponse"
    "fixture/GetCampaignResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCampaign)

responseGetVoiceTemplate :: GetVoiceTemplateResponse -> TestTree
responseGetVoiceTemplate =
  res
    "GetVoiceTemplateResponse"
    "fixture/GetVoiceTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetVoiceTemplate)

responseGetPushTemplate :: GetPushTemplateResponse -> TestTree
responseGetPushTemplate =
  res
    "GetPushTemplateResponse"
    "fixture/GetPushTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPushTemplate)

responseDeleteUserEndpoints :: DeleteUserEndpointsResponse -> TestTree
responseDeleteUserEndpoints =
  res
    "DeleteUserEndpointsResponse"
    "fixture/DeleteUserEndpointsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteUserEndpoints)

responseCreateEmailTemplate :: CreateEmailTemplateResponse -> TestTree
responseCreateEmailTemplate =
  res
    "CreateEmailTemplateResponse"
    "fixture/CreateEmailTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateEmailTemplate)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteApp)

responseUpdateApnsVoipSandboxChannel :: UpdateApnsVoipSandboxChannelResponse -> TestTree
responseUpdateApnsVoipSandboxChannel =
  res
    "UpdateApnsVoipSandboxChannelResponse"
    "fixture/UpdateApnsVoipSandboxChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateApnsVoipSandboxChannel)

responseDeleteApnsVoipSandboxChannel :: DeleteApnsVoipSandboxChannelResponse -> TestTree
responseDeleteApnsVoipSandboxChannel =
  res
    "DeleteApnsVoipSandboxChannelResponse"
    "fixture/DeleteApnsVoipSandboxChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteApnsVoipSandboxChannel)

responseUpdateGcmChannel :: UpdateGcmChannelResponse -> TestTree
responseUpdateGcmChannel =
  res
    "UpdateGcmChannelResponse"
    "fixture/UpdateGcmChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateGcmChannel)

responseDeleteGcmChannel :: DeleteGcmChannelResponse -> TestTree
responseDeleteGcmChannel =
  res
    "DeleteGcmChannelResponse"
    "fixture/DeleteGcmChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteGcmChannel)

responseGetCampaignActivities :: GetCampaignActivitiesResponse -> TestTree
responseGetCampaignActivities =
  res
    "GetCampaignActivitiesResponse"
    "fixture/GetCampaignActivitiesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCampaignActivities)

responseGetJourneyExecutionMetrics :: GetJourneyExecutionMetricsResponse -> TestTree
responseGetJourneyExecutionMetrics =
  res
    "GetJourneyExecutionMetricsResponse"
    "fixture/GetJourneyExecutionMetricsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetJourneyExecutionMetrics)

responseUpdateJourneyState :: UpdateJourneyStateResponse -> TestTree
responseUpdateJourneyState =
  res
    "UpdateJourneyStateResponse"
    "fixture/UpdateJourneyStateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateJourneyState)

responseGetEventStream :: GetEventStreamResponse -> TestTree
responseGetEventStream =
  res
    "GetEventStreamResponse"
    "fixture/GetEventStreamResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetEventStream)

responseGetChannels :: GetChannelsResponse -> TestTree
responseGetChannels =
  res
    "GetChannelsResponse"
    "fixture/GetChannelsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetChannels)

responseGetJourney :: GetJourneyResponse -> TestTree
responseGetJourney =
  res
    "GetJourneyResponse"
    "fixture/GetJourneyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetJourney)

responseDeleteEmailChannel :: DeleteEmailChannelResponse -> TestTree
responseDeleteEmailChannel =
  res
    "DeleteEmailChannelResponse"
    "fixture/DeleteEmailChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteEmailChannel)

responseUpdateEmailChannel :: UpdateEmailChannelResponse -> TestTree
responseUpdateEmailChannel =
  res
    "UpdateEmailChannelResponse"
    "fixture/UpdateEmailChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateEmailChannel)

responseGetBaiduChannel :: GetBaiduChannelResponse -> TestTree
responseGetBaiduChannel =
  res
    "GetBaiduChannelResponse"
    "fixture/GetBaiduChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBaiduChannel)

responseDeleteApnsChannel :: DeleteApnsChannelResponse -> TestTree
responseDeleteApnsChannel =
  res
    "DeleteApnsChannelResponse"
    "fixture/DeleteApnsChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteApnsChannel)

responseUpdateApnsChannel :: UpdateApnsChannelResponse -> TestTree
responseUpdateApnsChannel =
  res
    "UpdateApnsChannelResponse"
    "fixture/UpdateApnsChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateApnsChannel)

responseRemoveAttributes :: RemoveAttributesResponse -> TestTree
responseRemoveAttributes =
  res
    "RemoveAttributesResponse"
    "fixture/RemoveAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveAttributes)

responseListTemplates :: ListTemplatesResponse -> TestTree
responseListTemplates =
  res
    "ListTemplatesResponse"
    "fixture/ListTemplatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTemplates)

responsePutEventStream :: PutEventStreamResponse -> TestTree
responsePutEventStream =
  res
    "PutEventStreamResponse"
    "fixture/PutEventStreamResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutEventStream)

responseDeleteEventStream :: DeleteEventStreamResponse -> TestTree
responseDeleteEventStream =
  res
    "DeleteEventStreamResponse"
    "fixture/DeleteEventStreamResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteEventStream)

responseGetCampaignVersions :: GetCampaignVersionsResponse -> TestTree
responseGetCampaignVersions =
  res
    "GetCampaignVersionsResponse"
    "fixture/GetCampaignVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCampaignVersions)

responseDeleteJourney :: DeleteJourneyResponse -> TestTree
responseDeleteJourney =
  res
    "DeleteJourneyResponse"
    "fixture/DeleteJourneyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteJourney)

responseUpdateJourney :: UpdateJourneyResponse -> TestTree
responseUpdateJourney =
  res
    "UpdateJourneyResponse"
    "fixture/UpdateJourneyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateJourney)

responseGetCampaignDateRangeKpi :: GetCampaignDateRangeKpiResponse -> TestTree
responseGetCampaignDateRangeKpi =
  res
    "GetCampaignDateRangeKpiResponse"
    "fixture/GetCampaignDateRangeKpiResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCampaignDateRangeKpi)

responseGetApnsChannel :: GetApnsChannelResponse -> TestTree
responseGetApnsChannel =
  res
    "GetApnsChannelResponse"
    "fixture/GetApnsChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetApnsChannel)

responseUpdateVoiceChannel :: UpdateVoiceChannelResponse -> TestTree
responseUpdateVoiceChannel =
  res
    "UpdateVoiceChannelResponse"
    "fixture/UpdateVoiceChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateVoiceChannel)

responseDeleteVoiceChannel :: DeleteVoiceChannelResponse -> TestTree
responseDeleteVoiceChannel =
  res
    "DeleteVoiceChannelResponse"
    "fixture/DeleteVoiceChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVoiceChannel)

responseGetApps :: GetAppsResponse -> TestTree
responseGetApps =
  res
    "GetAppsResponse"
    "fixture/GetAppsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetApps)

responseGetApnsSandboxChannel :: GetApnsSandboxChannelResponse -> TestTree
responseGetApnsSandboxChannel =
  res
    "GetApnsSandboxChannelResponse"
    "fixture/GetApnsSandboxChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetApnsSandboxChannel)

responseCreateJourney :: CreateJourneyResponse -> TestTree
responseCreateJourney =
  res
    "CreateJourneyResponse"
    "fixture/CreateJourneyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateJourney)

responseGetUserEndpoints :: GetUserEndpointsResponse -> TestTree
responseGetUserEndpoints =
  res
    "GetUserEndpointsResponse"
    "fixture/GetUserEndpointsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetUserEndpoints)

responseDeleteVoiceTemplate :: DeleteVoiceTemplateResponse -> TestTree
responseDeleteVoiceTemplate =
  res
    "DeleteVoiceTemplateResponse"
    "fixture/DeleteVoiceTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVoiceTemplate)

responseUpdateVoiceTemplate :: UpdateVoiceTemplateResponse -> TestTree
responseUpdateVoiceTemplate =
  res
    "UpdateVoiceTemplateResponse"
    "fixture/UpdateVoiceTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateVoiceTemplate)

responseGetImportJobs :: GetImportJobsResponse -> TestTree
responseGetImportJobs =
  res
    "GetImportJobsResponse"
    "fixture/GetImportJobsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetImportJobs)

responseGetJourneyDateRangeKpi :: GetJourneyDateRangeKpiResponse -> TestTree
responseGetJourneyDateRangeKpi =
  res
    "GetJourneyDateRangeKpiResponse"
    "fixture/GetJourneyDateRangeKpiResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetJourneyDateRangeKpi)

responseUpdateTemplateActiveVersion :: UpdateTemplateActiveVersionResponse -> TestTree
responseUpdateTemplateActiveVersion =
  res
    "UpdateTemplateActiveVersionResponse"
    "fixture/UpdateTemplateActiveVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateTemplateActiveVersion)

responseDeleteSmsChannel :: DeleteSmsChannelResponse -> TestTree
responseDeleteSmsChannel =
  res
    "DeleteSmsChannelResponse"
    "fixture/DeleteSmsChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSmsChannel)

responseUpdateSmsChannel :: UpdateSmsChannelResponse -> TestTree
responseUpdateSmsChannel =
  res
    "UpdateSmsChannelResponse"
    "fixture/UpdateSmsChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSmsChannel)

responseGetApp :: GetAppResponse -> TestTree
responseGetApp =
  res
    "GetAppResponse"
    "fixture/GetAppResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetApp)

responseGetCampaignVersion :: GetCampaignVersionResponse -> TestTree
responseGetCampaignVersion =
  res
    "GetCampaignVersionResponse"
    "fixture/GetCampaignVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCampaignVersion)

responseDeleteSegment :: DeleteSegmentResponse -> TestTree
responseDeleteSegment =
  res
    "DeleteSegmentResponse"
    "fixture/DeleteSegmentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSegment)

responseUpdateSegment :: UpdateSegmentResponse -> TestTree
responseUpdateSegment =
  res
    "UpdateSegmentResponse"
    "fixture/UpdateSegmentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSegment)

responseGetApplicationDateRangeKpi :: GetApplicationDateRangeKpiResponse -> TestTree
responseGetApplicationDateRangeKpi =
  res
    "GetApplicationDateRangeKpiResponse"
    "fixture/GetApplicationDateRangeKpiResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetApplicationDateRangeKpi)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateApp)

responseGetSegmentExportJobs :: GetSegmentExportJobsResponse -> TestTree
responseGetSegmentExportJobs =
  res
    "GetSegmentExportJobsResponse"
    "fixture/GetSegmentExportJobsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSegmentExportJobs)

responseDeleteEmailTemplate :: DeleteEmailTemplateResponse -> TestTree
responseDeleteEmailTemplate =
  res
    "DeleteEmailTemplateResponse"
    "fixture/DeleteEmailTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteEmailTemplate)

responseUpdateEmailTemplate :: UpdateEmailTemplateResponse -> TestTree
responseUpdateEmailTemplate =
  res
    "UpdateEmailTemplateResponse"
    "fixture/UpdateEmailTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateEmailTemplate)

responseGetSmsChannel :: GetSmsChannelResponse -> TestTree
responseGetSmsChannel =
  res
    "GetSmsChannelResponse"
    "fixture/GetSmsChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSmsChannel)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseDeleteApnsSandboxChannel :: DeleteApnsSandboxChannelResponse -> TestTree
responseDeleteApnsSandboxChannel =
  res
    "DeleteApnsSandboxChannelResponse"
    "fixture/DeleteApnsSandboxChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteApnsSandboxChannel)

responseUpdateApnsSandboxChannel :: UpdateApnsSandboxChannelResponse -> TestTree
responseUpdateApnsSandboxChannel =
  res
    "UpdateApnsSandboxChannelResponse"
    "fixture/UpdateApnsSandboxChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateApnsSandboxChannel)

responseGetCampaigns :: GetCampaignsResponse -> TestTree
responseGetCampaigns =
  res
    "GetCampaignsResponse"
    "fixture/GetCampaignsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCampaigns)

responseGetVoiceChannel :: GetVoiceChannelResponse -> TestTree
responseGetVoiceChannel =
  res
    "GetVoiceChannelResponse"
    "fixture/GetVoiceChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetVoiceChannel)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseListTemplateVersions :: ListTemplateVersionsResponse -> TestTree
responseListTemplateVersions =
  res
    "ListTemplateVersionsResponse"
    "fixture/ListTemplateVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTemplateVersions)

responseGetSmsTemplate :: GetSmsTemplateResponse -> TestTree
responseGetSmsTemplate =
  res
    "GetSmsTemplateResponse"
    "fixture/GetSmsTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSmsTemplate)

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents =
  res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutEvents)

responseUpdateApplicationSettings :: UpdateApplicationSettingsResponse -> TestTree
responseUpdateApplicationSettings =
  res
    "UpdateApplicationSettingsResponse"
    "fixture/UpdateApplicationSettingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateApplicationSettings)

responseGetJourneyExecutionActivityMetrics :: GetJourneyExecutionActivityMetricsResponse -> TestTree
responseGetJourneyExecutionActivityMetrics =
  res
    "GetJourneyExecutionActivityMetricsResponse"
    "fixture/GetJourneyExecutionActivityMetricsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetJourneyExecutionActivityMetrics)

responseGetSegments :: GetSegmentsResponse -> TestTree
responseGetSegments =
  res
    "GetSegmentsResponse"
    "fixture/GetSegmentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSegments)

responseGetExportJobs :: GetExportJobsResponse -> TestTree
responseGetExportJobs =
  res
    "GetExportJobsResponse"
    "fixture/GetExportJobsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetExportJobs)

responseCreateImportJob :: CreateImportJobResponse -> TestTree
responseCreateImportJob =
  res
    "CreateImportJobResponse"
    "fixture/CreateImportJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateImportJob)

responseGetRecommenderConfigurations :: GetRecommenderConfigurationsResponse -> TestTree
responseGetRecommenderConfigurations =
  res
    "GetRecommenderConfigurationsResponse"
    "fixture/GetRecommenderConfigurationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRecommenderConfigurations)

responseDeleteApnsVoipChannel :: DeleteApnsVoipChannelResponse -> TestTree
responseDeleteApnsVoipChannel =
  res
    "DeleteApnsVoipChannelResponse"
    "fixture/DeleteApnsVoipChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteApnsVoipChannel)

responseUpdateApnsVoipChannel :: UpdateApnsVoipChannelResponse -> TestTree
responseUpdateApnsVoipChannel =
  res
    "UpdateApnsVoipChannelResponse"
    "fixture/UpdateApnsVoipChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateApnsVoipChannel)

responseSendUsersMessages :: SendUsersMessagesResponse -> TestTree
responseSendUsersMessages =
  res
    "SendUsersMessagesResponse"
    "fixture/SendUsersMessagesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SendUsersMessages)

responseGetApplicationSettings :: GetApplicationSettingsResponse -> TestTree
responseGetApplicationSettings =
  res
    "GetApplicationSettingsResponse"
    "fixture/GetApplicationSettingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetApplicationSettings)

responseDeleteBaiduChannel :: DeleteBaiduChannelResponse -> TestTree
responseDeleteBaiduChannel =
  res
    "DeleteBaiduChannelResponse"
    "fixture/DeleteBaiduChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBaiduChannel)

responseUpdateBaiduChannel :: UpdateBaiduChannelResponse -> TestTree
responseUpdateBaiduChannel =
  res
    "UpdateBaiduChannelResponse"
    "fixture/UpdateBaiduChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateBaiduChannel)

responseCreateSmsTemplate :: CreateSmsTemplateResponse -> TestTree
responseCreateSmsTemplate =
  res
    "CreateSmsTemplateResponse"
    "fixture/CreateSmsTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSmsTemplate)

responsePhoneNumberValidate :: PhoneNumberValidateResponse -> TestTree
responsePhoneNumberValidate =
  res
    "PhoneNumberValidateResponse"
    "fixture/PhoneNumberValidateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PhoneNumberValidate)

responseListJourneys :: ListJourneysResponse -> TestTree
responseListJourneys =
  res
    "ListJourneysResponse"
    "fixture/ListJourneysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListJourneys)

responseGetApnsVoipChannel :: GetApnsVoipChannelResponse -> TestTree
responseGetApnsVoipChannel =
  res
    "GetApnsVoipChannelResponse"
    "fixture/GetApnsVoipChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetApnsVoipChannel)

responseGetEmailChannel :: GetEmailChannelResponse -> TestTree
responseGetEmailChannel =
  res
    "GetEmailChannelResponse"
    "fixture/GetEmailChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetEmailChannel)
