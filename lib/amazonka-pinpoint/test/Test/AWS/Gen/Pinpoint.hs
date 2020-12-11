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
--             mkGetGCMChannel
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
--         , requestGetAPNSVoipSandboxChannel $
--             mkGetAPNSVoipSandboxChannel
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
--         , requestUpdateADMChannel $
--             mkUpdateADMChannel
--
--         , requestDeleteADMChannel $
--             mkDeleteADMChannel
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
--         , requestGetADMChannel $
--             mkGetADMChannel
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
--         , requestUpdateAPNSVoipSandboxChannel $
--             mkUpdateAPNSVoipSandboxChannel
--
--         , requestDeleteAPNSVoipSandboxChannel $
--             mkDeleteAPNSVoipSandboxChannel
--
--         , requestUpdateGCMChannel $
--             mkUpdateGCMChannel
--
--         , requestDeleteGCMChannel $
--             mkDeleteGCMChannel
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
--         , requestDeleteAPNSChannel $
--             mkDeleteAPNSChannel
--
--         , requestUpdateAPNSChannel $
--             mkUpdateAPNSChannel
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
--         , requestGetAPNSChannel $
--             mkGetAPNSChannel
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
--         , requestGetAPNSSandboxChannel $
--             mkGetAPNSSandboxChannel
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
--         , requestDeleteAPNSSandboxChannel $
--             mkDeleteAPNSSandboxChannel
--
--         , requestUpdateAPNSSandboxChannel $
--             mkUpdateAPNSSandboxChannel
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
--         , requestDeleteAPNSVoipChannel $
--             mkDeleteAPNSVoipChannel
--
--         , requestUpdateAPNSVoipChannel $
--             mkUpdateAPNSVoipChannel
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
--         , requestGetAPNSVoipChannel $
--             mkGetAPNSVoipChannel
--
--         , requestGetEmailChannel $
--             mkGetEmailChannel
--
--           ]

--     , testGroup "response"
--         [ responseGetGCMChannel $
--             mkGetGCMChannelResponse
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
--         , responseGetAPNSVoipSandboxChannel $
--             mkGetAPNSVoipSandboxChannelResponse
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
--         , responseUpdateADMChannel $
--             mkUpdateADMChannelResponse
--
--         , responseDeleteADMChannel $
--             mkDeleteADMChannelResponse
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
--         , responseGetADMChannel $
--             mkGetADMChannelResponse
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
--         , responseUpdateAPNSVoipSandboxChannel $
--             mkUpdateAPNSVoipSandboxChannelResponse
--
--         , responseDeleteAPNSVoipSandboxChannel $
--             mkDeleteAPNSVoipSandboxChannelResponse
--
--         , responseUpdateGCMChannel $
--             mkUpdateGCMChannelResponse
--
--         , responseDeleteGCMChannel $
--             mkDeleteGCMChannelResponse
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
--         , responseDeleteAPNSChannel $
--             mkDeleteAPNSChannelResponse
--
--         , responseUpdateAPNSChannel $
--             mkUpdateAPNSChannelResponse
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
--         , responseGetAPNSChannel $
--             mkGetAPNSChannelResponse
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
--         , responseGetAPNSSandboxChannel $
--             mkGetAPNSSandboxChannelResponse
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
--         , responseDeleteAPNSSandboxChannel $
--             mkDeleteAPNSSandboxChannelResponse
--
--         , responseUpdateAPNSSandboxChannel $
--             mkUpdateAPNSSandboxChannelResponse
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
--         , responseDeleteAPNSVoipChannel $
--             mkDeleteAPNSVoipChannelResponse
--
--         , responseUpdateAPNSVoipChannel $
--             mkUpdateAPNSVoipChannelResponse
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
--         , responseGetAPNSVoipChannel $
--             mkGetAPNSVoipChannelResponse
--
--         , responseGetEmailChannel $
--             mkGetEmailChannelResponse
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
    pinpointService
    (Proxy :: Proxy GetGCMChannel)

responseGetSegmentImportJobs :: GetSegmentImportJobsResponse -> TestTree
responseGetSegmentImportJobs =
  res
    "GetSegmentImportJobsResponse"
    "fixture/GetSegmentImportJobsResponse.proto"
    pinpointService
    (Proxy :: Proxy GetSegmentImportJobs)

responseSendMessages :: SendMessagesResponse -> TestTree
responseSendMessages =
  res
    "SendMessagesResponse"
    "fixture/SendMessagesResponse.proto"
    pinpointService
    (Proxy :: Proxy SendMessages)

responseGetImportJob :: GetImportJobResponse -> TestTree
responseGetImportJob =
  res
    "GetImportJobResponse"
    "fixture/GetImportJobResponse.proto"
    pinpointService
    (Proxy :: Proxy GetImportJob)

responseDeleteSmsTemplate :: DeleteSmsTemplateResponse -> TestTree
responseDeleteSmsTemplate =
  res
    "DeleteSmsTemplateResponse"
    "fixture/DeleteSmsTemplateResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteSmsTemplate)

responseUpdateSmsTemplate :: UpdateSmsTemplateResponse -> TestTree
responseUpdateSmsTemplate =
  res
    "UpdateSmsTemplateResponse"
    "fixture/UpdateSmsTemplateResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateSmsTemplate)

responseGetAPNSVoipSandboxChannel :: GetAPNSVoipSandboxChannelResponse -> TestTree
responseGetAPNSVoipSandboxChannel =
  res
    "GetAPNSVoipSandboxChannelResponse"
    "fixture/GetAPNSVoipSandboxChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy GetAPNSVoipSandboxChannel)

responseGetSegmentVersions :: GetSegmentVersionsResponse -> TestTree
responseGetSegmentVersions =
  res
    "GetSegmentVersionsResponse"
    "fixture/GetSegmentVersionsResponse.proto"
    pinpointService
    (Proxy :: Proxy GetSegmentVersions)

responseDeleteCampaign :: DeleteCampaignResponse -> TestTree
responseDeleteCampaign =
  res
    "DeleteCampaignResponse"
    "fixture/DeleteCampaignResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteCampaign)

responseUpdateCampaign :: UpdateCampaignResponse -> TestTree
responseUpdateCampaign =
  res
    "UpdateCampaignResponse"
    "fixture/UpdateCampaignResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateCampaign)

responseGetSegmentVersion :: GetSegmentVersionResponse -> TestTree
responseGetSegmentVersion =
  res
    "GetSegmentVersionResponse"
    "fixture/GetSegmentVersionResponse.proto"
    pinpointService
    (Proxy :: Proxy GetSegmentVersion)

responseDeletePushTemplate :: DeletePushTemplateResponse -> TestTree
responseDeletePushTemplate =
  res
    "DeletePushTemplateResponse"
    "fixture/DeletePushTemplateResponse.proto"
    pinpointService
    (Proxy :: Proxy DeletePushTemplate)

responseUpdatePushTemplate :: UpdatePushTemplateResponse -> TestTree
responseUpdatePushTemplate =
  res
    "UpdatePushTemplateResponse"
    "fixture/UpdatePushTemplateResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdatePushTemplate)

responseCreateExportJob :: CreateExportJobResponse -> TestTree
responseCreateExportJob =
  res
    "CreateExportJobResponse"
    "fixture/CreateExportJobResponse.proto"
    pinpointService
    (Proxy :: Proxy CreateExportJob)

responseCreateSegment :: CreateSegmentResponse -> TestTree
responseCreateSegment =
  res
    "CreateSegmentResponse"
    "fixture/CreateSegmentResponse.proto"
    pinpointService
    (Proxy :: Proxy CreateSegment)

responseCreateRecommenderConfiguration :: CreateRecommenderConfigurationResponse -> TestTree
responseCreateRecommenderConfiguration =
  res
    "CreateRecommenderConfigurationResponse"
    "fixture/CreateRecommenderConfigurationResponse.proto"
    pinpointService
    (Proxy :: Proxy CreateRecommenderConfiguration)

responseCreateVoiceTemplate :: CreateVoiceTemplateResponse -> TestTree
responseCreateVoiceTemplate =
  res
    "CreateVoiceTemplateResponse"
    "fixture/CreateVoiceTemplateResponse.proto"
    pinpointService
    (Proxy :: Proxy CreateVoiceTemplate)

responseUpdateADMChannel :: UpdateADMChannelResponse -> TestTree
responseUpdateADMChannel =
  res
    "UpdateADMChannelResponse"
    "fixture/UpdateADMChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateADMChannel)

responseDeleteADMChannel :: DeleteADMChannelResponse -> TestTree
responseDeleteADMChannel =
  res
    "DeleteADMChannelResponse"
    "fixture/DeleteADMChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteADMChannel)

responseDeleteRecommenderConfiguration :: DeleteRecommenderConfigurationResponse -> TestTree
responseDeleteRecommenderConfiguration =
  res
    "DeleteRecommenderConfigurationResponse"
    "fixture/DeleteRecommenderConfigurationResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteRecommenderConfiguration)

responseUpdateRecommenderConfiguration :: UpdateRecommenderConfigurationResponse -> TestTree
responseUpdateRecommenderConfiguration =
  res
    "UpdateRecommenderConfigurationResponse"
    "fixture/UpdateRecommenderConfigurationResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateRecommenderConfiguration)

responseCreatePushTemplate :: CreatePushTemplateResponse -> TestTree
responseCreatePushTemplate =
  res
    "CreatePushTemplateResponse"
    "fixture/CreatePushTemplateResponse.proto"
    pinpointService
    (Proxy :: Proxy CreatePushTemplate)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteEndpoint)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateEndpoint)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    pinpointService
    (Proxy :: Proxy ListTagsForResource)

responseCreateCampaign :: CreateCampaignResponse -> TestTree
responseCreateCampaign =
  res
    "CreateCampaignResponse"
    "fixture/CreateCampaignResponse.proto"
    pinpointService
    (Proxy :: Proxy CreateCampaign)

responseGetEmailTemplate :: GetEmailTemplateResponse -> TestTree
responseGetEmailTemplate =
  res
    "GetEmailTemplateResponse"
    "fixture/GetEmailTemplateResponse.proto"
    pinpointService
    (Proxy :: Proxy GetEmailTemplate)

responseGetExportJob :: GetExportJobResponse -> TestTree
responseGetExportJob =
  res
    "GetExportJobResponse"
    "fixture/GetExportJobResponse.proto"
    pinpointService
    (Proxy :: Proxy GetExportJob)

responseGetEndpoint :: GetEndpointResponse -> TestTree
responseGetEndpoint =
  res
    "GetEndpointResponse"
    "fixture/GetEndpointResponse.proto"
    pinpointService
    (Proxy :: Proxy GetEndpoint)

responseGetSegment :: GetSegmentResponse -> TestTree
responseGetSegment =
  res
    "GetSegmentResponse"
    "fixture/GetSegmentResponse.proto"
    pinpointService
    (Proxy :: Proxy GetSegment)

responseGetRecommenderConfiguration :: GetRecommenderConfigurationResponse -> TestTree
responseGetRecommenderConfiguration =
  res
    "GetRecommenderConfigurationResponse"
    "fixture/GetRecommenderConfigurationResponse.proto"
    pinpointService
    (Proxy :: Proxy GetRecommenderConfiguration)

responseUpdateEndpointsBatch :: UpdateEndpointsBatchResponse -> TestTree
responseUpdateEndpointsBatch =
  res
    "UpdateEndpointsBatchResponse"
    "fixture/UpdateEndpointsBatchResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateEndpointsBatch)

responseGetADMChannel :: GetADMChannelResponse -> TestTree
responseGetADMChannel =
  res
    "GetADMChannelResponse"
    "fixture/GetADMChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy GetADMChannel)

responseGetCampaign :: GetCampaignResponse -> TestTree
responseGetCampaign =
  res
    "GetCampaignResponse"
    "fixture/GetCampaignResponse.proto"
    pinpointService
    (Proxy :: Proxy GetCampaign)

responseGetVoiceTemplate :: GetVoiceTemplateResponse -> TestTree
responseGetVoiceTemplate =
  res
    "GetVoiceTemplateResponse"
    "fixture/GetVoiceTemplateResponse.proto"
    pinpointService
    (Proxy :: Proxy GetVoiceTemplate)

responseGetPushTemplate :: GetPushTemplateResponse -> TestTree
responseGetPushTemplate =
  res
    "GetPushTemplateResponse"
    "fixture/GetPushTemplateResponse.proto"
    pinpointService
    (Proxy :: Proxy GetPushTemplate)

responseDeleteUserEndpoints :: DeleteUserEndpointsResponse -> TestTree
responseDeleteUserEndpoints =
  res
    "DeleteUserEndpointsResponse"
    "fixture/DeleteUserEndpointsResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteUserEndpoints)

responseCreateEmailTemplate :: CreateEmailTemplateResponse -> TestTree
responseCreateEmailTemplate =
  res
    "CreateEmailTemplateResponse"
    "fixture/CreateEmailTemplateResponse.proto"
    pinpointService
    (Proxy :: Proxy CreateEmailTemplate)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteApp)

responseUpdateAPNSVoipSandboxChannel :: UpdateAPNSVoipSandboxChannelResponse -> TestTree
responseUpdateAPNSVoipSandboxChannel =
  res
    "UpdateAPNSVoipSandboxChannelResponse"
    "fixture/UpdateAPNSVoipSandboxChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateAPNSVoipSandboxChannel)

responseDeleteAPNSVoipSandboxChannel :: DeleteAPNSVoipSandboxChannelResponse -> TestTree
responseDeleteAPNSVoipSandboxChannel =
  res
    "DeleteAPNSVoipSandboxChannelResponse"
    "fixture/DeleteAPNSVoipSandboxChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteAPNSVoipSandboxChannel)

responseUpdateGCMChannel :: UpdateGCMChannelResponse -> TestTree
responseUpdateGCMChannel =
  res
    "UpdateGCMChannelResponse"
    "fixture/UpdateGCMChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateGCMChannel)

responseDeleteGCMChannel :: DeleteGCMChannelResponse -> TestTree
responseDeleteGCMChannel =
  res
    "DeleteGCMChannelResponse"
    "fixture/DeleteGCMChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteGCMChannel)

responseGetCampaignActivities :: GetCampaignActivitiesResponse -> TestTree
responseGetCampaignActivities =
  res
    "GetCampaignActivitiesResponse"
    "fixture/GetCampaignActivitiesResponse.proto"
    pinpointService
    (Proxy :: Proxy GetCampaignActivities)

responseGetJourneyExecutionMetrics :: GetJourneyExecutionMetricsResponse -> TestTree
responseGetJourneyExecutionMetrics =
  res
    "GetJourneyExecutionMetricsResponse"
    "fixture/GetJourneyExecutionMetricsResponse.proto"
    pinpointService
    (Proxy :: Proxy GetJourneyExecutionMetrics)

responseUpdateJourneyState :: UpdateJourneyStateResponse -> TestTree
responseUpdateJourneyState =
  res
    "UpdateJourneyStateResponse"
    "fixture/UpdateJourneyStateResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateJourneyState)

responseGetEventStream :: GetEventStreamResponse -> TestTree
responseGetEventStream =
  res
    "GetEventStreamResponse"
    "fixture/GetEventStreamResponse.proto"
    pinpointService
    (Proxy :: Proxy GetEventStream)

responseGetChannels :: GetChannelsResponse -> TestTree
responseGetChannels =
  res
    "GetChannelsResponse"
    "fixture/GetChannelsResponse.proto"
    pinpointService
    (Proxy :: Proxy GetChannels)

responseGetJourney :: GetJourneyResponse -> TestTree
responseGetJourney =
  res
    "GetJourneyResponse"
    "fixture/GetJourneyResponse.proto"
    pinpointService
    (Proxy :: Proxy GetJourney)

responseDeleteEmailChannel :: DeleteEmailChannelResponse -> TestTree
responseDeleteEmailChannel =
  res
    "DeleteEmailChannelResponse"
    "fixture/DeleteEmailChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteEmailChannel)

responseUpdateEmailChannel :: UpdateEmailChannelResponse -> TestTree
responseUpdateEmailChannel =
  res
    "UpdateEmailChannelResponse"
    "fixture/UpdateEmailChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateEmailChannel)

responseGetBaiduChannel :: GetBaiduChannelResponse -> TestTree
responseGetBaiduChannel =
  res
    "GetBaiduChannelResponse"
    "fixture/GetBaiduChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy GetBaiduChannel)

responseDeleteAPNSChannel :: DeleteAPNSChannelResponse -> TestTree
responseDeleteAPNSChannel =
  res
    "DeleteAPNSChannelResponse"
    "fixture/DeleteAPNSChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteAPNSChannel)

responseUpdateAPNSChannel :: UpdateAPNSChannelResponse -> TestTree
responseUpdateAPNSChannel =
  res
    "UpdateAPNSChannelResponse"
    "fixture/UpdateAPNSChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateAPNSChannel)

responseRemoveAttributes :: RemoveAttributesResponse -> TestTree
responseRemoveAttributes =
  res
    "RemoveAttributesResponse"
    "fixture/RemoveAttributesResponse.proto"
    pinpointService
    (Proxy :: Proxy RemoveAttributes)

responseListTemplates :: ListTemplatesResponse -> TestTree
responseListTemplates =
  res
    "ListTemplatesResponse"
    "fixture/ListTemplatesResponse.proto"
    pinpointService
    (Proxy :: Proxy ListTemplates)

responsePutEventStream :: PutEventStreamResponse -> TestTree
responsePutEventStream =
  res
    "PutEventStreamResponse"
    "fixture/PutEventStreamResponse.proto"
    pinpointService
    (Proxy :: Proxy PutEventStream)

responseDeleteEventStream :: DeleteEventStreamResponse -> TestTree
responseDeleteEventStream =
  res
    "DeleteEventStreamResponse"
    "fixture/DeleteEventStreamResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteEventStream)

responseGetCampaignVersions :: GetCampaignVersionsResponse -> TestTree
responseGetCampaignVersions =
  res
    "GetCampaignVersionsResponse"
    "fixture/GetCampaignVersionsResponse.proto"
    pinpointService
    (Proxy :: Proxy GetCampaignVersions)

responseDeleteJourney :: DeleteJourneyResponse -> TestTree
responseDeleteJourney =
  res
    "DeleteJourneyResponse"
    "fixture/DeleteJourneyResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteJourney)

responseUpdateJourney :: UpdateJourneyResponse -> TestTree
responseUpdateJourney =
  res
    "UpdateJourneyResponse"
    "fixture/UpdateJourneyResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateJourney)

responseGetCampaignDateRangeKpi :: GetCampaignDateRangeKpiResponse -> TestTree
responseGetCampaignDateRangeKpi =
  res
    "GetCampaignDateRangeKpiResponse"
    "fixture/GetCampaignDateRangeKpiResponse.proto"
    pinpointService
    (Proxy :: Proxy GetCampaignDateRangeKpi)

responseGetAPNSChannel :: GetAPNSChannelResponse -> TestTree
responseGetAPNSChannel =
  res
    "GetAPNSChannelResponse"
    "fixture/GetAPNSChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy GetAPNSChannel)

responseUpdateVoiceChannel :: UpdateVoiceChannelResponse -> TestTree
responseUpdateVoiceChannel =
  res
    "UpdateVoiceChannelResponse"
    "fixture/UpdateVoiceChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateVoiceChannel)

responseDeleteVoiceChannel :: DeleteVoiceChannelResponse -> TestTree
responseDeleteVoiceChannel =
  res
    "DeleteVoiceChannelResponse"
    "fixture/DeleteVoiceChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteVoiceChannel)

responseGetApps :: GetAppsResponse -> TestTree
responseGetApps =
  res
    "GetAppsResponse"
    "fixture/GetAppsResponse.proto"
    pinpointService
    (Proxy :: Proxy GetApps)

responseGetAPNSSandboxChannel :: GetAPNSSandboxChannelResponse -> TestTree
responseGetAPNSSandboxChannel =
  res
    "GetAPNSSandboxChannelResponse"
    "fixture/GetAPNSSandboxChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy GetAPNSSandboxChannel)

responseCreateJourney :: CreateJourneyResponse -> TestTree
responseCreateJourney =
  res
    "CreateJourneyResponse"
    "fixture/CreateJourneyResponse.proto"
    pinpointService
    (Proxy :: Proxy CreateJourney)

responseGetUserEndpoints :: GetUserEndpointsResponse -> TestTree
responseGetUserEndpoints =
  res
    "GetUserEndpointsResponse"
    "fixture/GetUserEndpointsResponse.proto"
    pinpointService
    (Proxy :: Proxy GetUserEndpoints)

responseDeleteVoiceTemplate :: DeleteVoiceTemplateResponse -> TestTree
responseDeleteVoiceTemplate =
  res
    "DeleteVoiceTemplateResponse"
    "fixture/DeleteVoiceTemplateResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteVoiceTemplate)

responseUpdateVoiceTemplate :: UpdateVoiceTemplateResponse -> TestTree
responseUpdateVoiceTemplate =
  res
    "UpdateVoiceTemplateResponse"
    "fixture/UpdateVoiceTemplateResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateVoiceTemplate)

responseGetImportJobs :: GetImportJobsResponse -> TestTree
responseGetImportJobs =
  res
    "GetImportJobsResponse"
    "fixture/GetImportJobsResponse.proto"
    pinpointService
    (Proxy :: Proxy GetImportJobs)

responseGetJourneyDateRangeKpi :: GetJourneyDateRangeKpiResponse -> TestTree
responseGetJourneyDateRangeKpi =
  res
    "GetJourneyDateRangeKpiResponse"
    "fixture/GetJourneyDateRangeKpiResponse.proto"
    pinpointService
    (Proxy :: Proxy GetJourneyDateRangeKpi)

responseUpdateTemplateActiveVersion :: UpdateTemplateActiveVersionResponse -> TestTree
responseUpdateTemplateActiveVersion =
  res
    "UpdateTemplateActiveVersionResponse"
    "fixture/UpdateTemplateActiveVersionResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateTemplateActiveVersion)

responseDeleteSmsChannel :: DeleteSmsChannelResponse -> TestTree
responseDeleteSmsChannel =
  res
    "DeleteSmsChannelResponse"
    "fixture/DeleteSmsChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteSmsChannel)

responseUpdateSmsChannel :: UpdateSmsChannelResponse -> TestTree
responseUpdateSmsChannel =
  res
    "UpdateSmsChannelResponse"
    "fixture/UpdateSmsChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateSmsChannel)

responseGetApp :: GetAppResponse -> TestTree
responseGetApp =
  res
    "GetAppResponse"
    "fixture/GetAppResponse.proto"
    pinpointService
    (Proxy :: Proxy GetApp)

responseGetCampaignVersion :: GetCampaignVersionResponse -> TestTree
responseGetCampaignVersion =
  res
    "GetCampaignVersionResponse"
    "fixture/GetCampaignVersionResponse.proto"
    pinpointService
    (Proxy :: Proxy GetCampaignVersion)

responseDeleteSegment :: DeleteSegmentResponse -> TestTree
responseDeleteSegment =
  res
    "DeleteSegmentResponse"
    "fixture/DeleteSegmentResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteSegment)

responseUpdateSegment :: UpdateSegmentResponse -> TestTree
responseUpdateSegment =
  res
    "UpdateSegmentResponse"
    "fixture/UpdateSegmentResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateSegment)

responseGetApplicationDateRangeKpi :: GetApplicationDateRangeKpiResponse -> TestTree
responseGetApplicationDateRangeKpi =
  res
    "GetApplicationDateRangeKpiResponse"
    "fixture/GetApplicationDateRangeKpiResponse.proto"
    pinpointService
    (Proxy :: Proxy GetApplicationDateRangeKpi)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    pinpointService
    (Proxy :: Proxy CreateApp)

responseGetSegmentExportJobs :: GetSegmentExportJobsResponse -> TestTree
responseGetSegmentExportJobs =
  res
    "GetSegmentExportJobsResponse"
    "fixture/GetSegmentExportJobsResponse.proto"
    pinpointService
    (Proxy :: Proxy GetSegmentExportJobs)

responseDeleteEmailTemplate :: DeleteEmailTemplateResponse -> TestTree
responseDeleteEmailTemplate =
  res
    "DeleteEmailTemplateResponse"
    "fixture/DeleteEmailTemplateResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteEmailTemplate)

responseUpdateEmailTemplate :: UpdateEmailTemplateResponse -> TestTree
responseUpdateEmailTemplate =
  res
    "UpdateEmailTemplateResponse"
    "fixture/UpdateEmailTemplateResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateEmailTemplate)

responseGetSmsChannel :: GetSmsChannelResponse -> TestTree
responseGetSmsChannel =
  res
    "GetSmsChannelResponse"
    "fixture/GetSmsChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy GetSmsChannel)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    pinpointService
    (Proxy :: Proxy TagResource)

responseDeleteAPNSSandboxChannel :: DeleteAPNSSandboxChannelResponse -> TestTree
responseDeleteAPNSSandboxChannel =
  res
    "DeleteAPNSSandboxChannelResponse"
    "fixture/DeleteAPNSSandboxChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteAPNSSandboxChannel)

responseUpdateAPNSSandboxChannel :: UpdateAPNSSandboxChannelResponse -> TestTree
responseUpdateAPNSSandboxChannel =
  res
    "UpdateAPNSSandboxChannelResponse"
    "fixture/UpdateAPNSSandboxChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateAPNSSandboxChannel)

responseGetCampaigns :: GetCampaignsResponse -> TestTree
responseGetCampaigns =
  res
    "GetCampaignsResponse"
    "fixture/GetCampaignsResponse.proto"
    pinpointService
    (Proxy :: Proxy GetCampaigns)

responseGetVoiceChannel :: GetVoiceChannelResponse -> TestTree
responseGetVoiceChannel =
  res
    "GetVoiceChannelResponse"
    "fixture/GetVoiceChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy GetVoiceChannel)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    pinpointService
    (Proxy :: Proxy UntagResource)

responseListTemplateVersions :: ListTemplateVersionsResponse -> TestTree
responseListTemplateVersions =
  res
    "ListTemplateVersionsResponse"
    "fixture/ListTemplateVersionsResponse.proto"
    pinpointService
    (Proxy :: Proxy ListTemplateVersions)

responseGetSmsTemplate :: GetSmsTemplateResponse -> TestTree
responseGetSmsTemplate =
  res
    "GetSmsTemplateResponse"
    "fixture/GetSmsTemplateResponse.proto"
    pinpointService
    (Proxy :: Proxy GetSmsTemplate)

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents =
  res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    pinpointService
    (Proxy :: Proxy PutEvents)

responseUpdateApplicationSettings :: UpdateApplicationSettingsResponse -> TestTree
responseUpdateApplicationSettings =
  res
    "UpdateApplicationSettingsResponse"
    "fixture/UpdateApplicationSettingsResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateApplicationSettings)

responseGetJourneyExecutionActivityMetrics :: GetJourneyExecutionActivityMetricsResponse -> TestTree
responseGetJourneyExecutionActivityMetrics =
  res
    "GetJourneyExecutionActivityMetricsResponse"
    "fixture/GetJourneyExecutionActivityMetricsResponse.proto"
    pinpointService
    (Proxy :: Proxy GetJourneyExecutionActivityMetrics)

responseGetSegments :: GetSegmentsResponse -> TestTree
responseGetSegments =
  res
    "GetSegmentsResponse"
    "fixture/GetSegmentsResponse.proto"
    pinpointService
    (Proxy :: Proxy GetSegments)

responseGetExportJobs :: GetExportJobsResponse -> TestTree
responseGetExportJobs =
  res
    "GetExportJobsResponse"
    "fixture/GetExportJobsResponse.proto"
    pinpointService
    (Proxy :: Proxy GetExportJobs)

responseCreateImportJob :: CreateImportJobResponse -> TestTree
responseCreateImportJob =
  res
    "CreateImportJobResponse"
    "fixture/CreateImportJobResponse.proto"
    pinpointService
    (Proxy :: Proxy CreateImportJob)

responseGetRecommenderConfigurations :: GetRecommenderConfigurationsResponse -> TestTree
responseGetRecommenderConfigurations =
  res
    "GetRecommenderConfigurationsResponse"
    "fixture/GetRecommenderConfigurationsResponse.proto"
    pinpointService
    (Proxy :: Proxy GetRecommenderConfigurations)

responseDeleteAPNSVoipChannel :: DeleteAPNSVoipChannelResponse -> TestTree
responseDeleteAPNSVoipChannel =
  res
    "DeleteAPNSVoipChannelResponse"
    "fixture/DeleteAPNSVoipChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteAPNSVoipChannel)

responseUpdateAPNSVoipChannel :: UpdateAPNSVoipChannelResponse -> TestTree
responseUpdateAPNSVoipChannel =
  res
    "UpdateAPNSVoipChannelResponse"
    "fixture/UpdateAPNSVoipChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateAPNSVoipChannel)

responseSendUsersMessages :: SendUsersMessagesResponse -> TestTree
responseSendUsersMessages =
  res
    "SendUsersMessagesResponse"
    "fixture/SendUsersMessagesResponse.proto"
    pinpointService
    (Proxy :: Proxy SendUsersMessages)

responseGetApplicationSettings :: GetApplicationSettingsResponse -> TestTree
responseGetApplicationSettings =
  res
    "GetApplicationSettingsResponse"
    "fixture/GetApplicationSettingsResponse.proto"
    pinpointService
    (Proxy :: Proxy GetApplicationSettings)

responseDeleteBaiduChannel :: DeleteBaiduChannelResponse -> TestTree
responseDeleteBaiduChannel =
  res
    "DeleteBaiduChannelResponse"
    "fixture/DeleteBaiduChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy DeleteBaiduChannel)

responseUpdateBaiduChannel :: UpdateBaiduChannelResponse -> TestTree
responseUpdateBaiduChannel =
  res
    "UpdateBaiduChannelResponse"
    "fixture/UpdateBaiduChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy UpdateBaiduChannel)

responseCreateSmsTemplate :: CreateSmsTemplateResponse -> TestTree
responseCreateSmsTemplate =
  res
    "CreateSmsTemplateResponse"
    "fixture/CreateSmsTemplateResponse.proto"
    pinpointService
    (Proxy :: Proxy CreateSmsTemplate)

responsePhoneNumberValidate :: PhoneNumberValidateResponse -> TestTree
responsePhoneNumberValidate =
  res
    "PhoneNumberValidateResponse"
    "fixture/PhoneNumberValidateResponse.proto"
    pinpointService
    (Proxy :: Proxy PhoneNumberValidate)

responseListJourneys :: ListJourneysResponse -> TestTree
responseListJourneys =
  res
    "ListJourneysResponse"
    "fixture/ListJourneysResponse.proto"
    pinpointService
    (Proxy :: Proxy ListJourneys)

responseGetAPNSVoipChannel :: GetAPNSVoipChannelResponse -> TestTree
responseGetAPNSVoipChannel =
  res
    "GetAPNSVoipChannelResponse"
    "fixture/GetAPNSVoipChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy GetAPNSVoipChannel)

responseGetEmailChannel :: GetEmailChannelResponse -> TestTree
responseGetEmailChannel =
  res
    "GetEmailChannelResponse"
    "fixture/GetEmailChannelResponse.proto"
    pinpointService
    (Proxy :: Proxy GetEmailChannel)
