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
--         [ requestDeleteVoiceTemplate $
--             newDeleteVoiceTemplate
--
--         , requestGetImportJobs $
--             newGetImportJobs
--
--         , requestUpdatePushTemplate $
--             newUpdatePushTemplate
--
--         , requestDeleteCampaign $
--             newDeleteCampaign
--
--         , requestUpdateVoiceTemplate $
--             newUpdateVoiceTemplate
--
--         , requestUpdateCampaign $
--             newUpdateCampaign
--
--         , requestCreateRecommenderConfiguration $
--             newCreateRecommenderConfiguration'
--
--         , requestUpdateTemplateActiveVersion $
--             newUpdateTemplateActiveVersion
--
--         , requestDeletePushTemplate $
--             newDeletePushTemplate
--
--         , requestCreateJourney $
--             newCreateJourney
--
--         , requestGetImportJob $
--             newGetImportJob
--
--         , requestGetSegmentVersions $
--             newGetSegmentVersions
--
--         , requestGetApps $
--             newGetApps
--
--         , requestGetSegmentImportJobs $
--             newGetSegmentImportJobs
--
--         , requestGetApnsSandboxChannel $
--             newGetApnsSandboxChannel
--
--         , requestSendMessages $
--             newSendMessages
--
--         , requestCreateSmsTemplate $
--             newCreateSmsTemplate
--
--         , requestRemoveAttributes $
--             newRemoveAttributes
--
--         , requestGetApnsChannel $
--             newGetApnsChannel
--
--         , requestPhoneNumberValidate $
--             newPhoneNumberValidate
--
--         , requestGetEmailChannel $
--             newGetEmailChannel
--
--         , requestPutEventStream $
--             newPutEventStream
--
--         , requestGetJourneyExecutionActivityMetrics $
--             newGetJourneyExecutionActivityMetrics
--
--         , requestUpdateApnsChannel $
--             newUpdateApnsChannel
--
--         , requestDeleteApnsChannel $
--             newDeleteApnsChannel
--
--         , requestGetBaiduChannel $
--             newGetBaiduChannel
--
--         , requestGetChannels $
--             newGetChannels
--
--         , requestGetRecommenderConfigurations $
--             newGetRecommenderConfigurations
--
--         , requestUpdateGcmChannel $
--             newUpdateGcmChannel
--
--         , requestDeleteGcmChannel $
--             newDeleteGcmChannel
--
--         , requestGetJourneyExecutionMetrics $
--             newGetJourneyExecutionMetrics
--
--         , requestGetVoiceChannel $
--             newGetVoiceChannel
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateApnsVoipSandboxChannel $
--             newUpdateApnsVoipSandboxChannel
--
--         , requestDeleteApnsVoipSandboxChannel $
--             newDeleteApnsVoipSandboxChannel
--
--         , requestGetVoiceTemplate $
--             newGetVoiceTemplate
--
--         , requestGetSmsChannel $
--             newGetSmsChannel
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetEndpoint $
--             newGetEndpoint
--
--         , requestGetApplicationDateRangeKpi $
--             newGetApplicationDateRangeKpi
--
--         , requestGetAdmChannel $
--             newGetAdmChannel
--
--         , requestGetRecommenderConfiguration $
--             newGetRecommenderConfiguration
--
--         , requestGetSegmentExportJobs $
--             newGetSegmentExportJobs
--
--         , requestUpdateSegment $
--             newUpdateSegment
--
--         , requestDeleteSegment $
--             newDeleteSegment
--
--         , requestCreatePushTemplate $
--             newCreatePushTemplate
--
--         , requestDeleteAdmChannel $
--             newDeleteAdmChannel
--
--         , requestUpdateRecommenderConfiguration $
--             newUpdateRecommenderConfiguration'
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestCreateCampaign $
--             newCreateCampaign
--
--         , requestUpdateEndpoint $
--             newUpdateEndpoint
--
--         , requestGetEmailTemplate $
--             newGetEmailTemplate
--
--         , requestDeleteRecommenderConfiguration $
--             newDeleteRecommenderConfiguration
--
--         , requestUpdateAdmChannel $
--             newUpdateAdmChannel
--
--         , requestDeleteSmsChannel $
--             newDeleteSmsChannel
--
--         , requestGetJourneyDateRangeKpi $
--             newGetJourneyDateRangeKpi
--
--         , requestGetApp $
--             newGetApp
--
--         , requestCreateExportJob $
--             newCreateExportJob
--
--         , requestGetUserEndpoints $
--             newGetUserEndpoints
--
--         , requestGetSegmentVersion $
--             newGetSegmentVersion
--
--         , requestUpdateSmsChannel $
--             newUpdateSmsChannel
--
--         , requestCreateSegment $
--             newCreateSegment
--
--         , requestDeleteSmsTemplate $
--             newDeleteSmsTemplate
--
--         , requestUpdateSmsTemplate $
--             newUpdateSmsTemplate
--
--         , requestGetGcmChannel $
--             newGetGcmChannel
--
--         , requestDeleteVoiceChannel $
--             newDeleteVoiceChannel
--
--         , requestUpdateVoiceChannel $
--             newUpdateVoiceChannel
--
--         , requestGetApnsVoipSandboxChannel $
--             newGetApnsVoipSandboxChannel
--
--         , requestDeleteJourney $
--             newDeleteJourney
--
--         , requestGetCampaignDateRangeKpi $
--             newGetCampaignDateRangeKpi
--
--         , requestUpdateJourney $
--             newUpdateJourney
--
--         , requestListTemplates $
--             newListTemplates
--
--         , requestDeleteBaiduChannel $
--             newDeleteBaiduChannel
--
--         , requestGetCampaignVersions $
--             newGetCampaignVersions
--
--         , requestGetApplicationSettings $
--             newGetApplicationSettings
--
--         , requestGetApnsVoipChannel $
--             newGetApnsVoipChannel
--
--         , requestListJourneys $
--             newListJourneys
--
--         , requestDeleteEventStream $
--             newDeleteEventStream
--
--         , requestUpdateBaiduChannel $
--             newUpdateBaiduChannel
--
--         , requestGetExportJobs $
--             newGetExportJobs
--
--         , requestGetSegments $
--             newGetSegments
--
--         , requestGetJourney $
--             newGetJourney
--
--         , requestPutEvents $
--             newPutEvents
--
--         , requestDeleteApnsVoipChannel $
--             newDeleteApnsVoipChannel
--
--         , requestUpdateApnsVoipChannel $
--             newUpdateApnsVoipChannel
--
--         , requestCreateImportJob $
--             newCreateImportJob
--
--         , requestUpdateEmailChannel $
--             newUpdateEmailChannel
--
--         , requestGetEventStream $
--             newGetEventStream
--
--         , requestSendUsersMessages $
--             newSendUsersMessages
--
--         , requestDeleteEmailChannel $
--             newDeleteEmailChannel
--
--         , requestUpdateApplicationSettings $
--             newUpdateApplicationSettings
--
--         , requestUpdateJourneyState $
--             newUpdateJourneyState
--
--         , requestListTemplateVersions $
--             newListTemplateVersions
--
--         , requestDeleteApnsSandboxChannel $
--             newDeleteApnsSandboxChannel
--
--         , requestGetCampaignActivities $
--             newGetCampaignActivities
--
--         , requestUpdateApnsSandboxChannel $
--             newUpdateApnsSandboxChannel
--
--         , requestGetCampaigns $
--             newGetCampaigns
--
--         , requestGetSmsTemplate $
--             newGetSmsTemplate
--
--         , requestGetPushTemplate $
--             newGetPushTemplate
--
--         , requestGetCampaign $
--             newGetCampaign
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestDeleteUserEndpoints $
--             newDeleteUserEndpoints
--
--         , requestCreateEmailTemplate $
--             newCreateEmailTemplate
--
--         , requestUpdateEmailTemplate $
--             newUpdateEmailTemplate
--
--         , requestDeleteEmailTemplate $
--             newDeleteEmailTemplate
--
--         , requestCreateApp $
--             newCreateApp
--
--         , requestUpdateEndpointsBatch $
--             newUpdateEndpointsBatch
--
--         , requestGetExportJob $
--             newGetExportJob
--
--         , requestGetSegment $
--             newGetSegment
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetCampaignVersion $
--             newGetCampaignVersion
--
--         , requestCreateVoiceTemplate $
--             newCreateVoiceTemplate
--
--           ]

--     , testGroup "response"
--         [ responseDeleteVoiceTemplate $
--             newDeleteVoiceTemplateResponse
--
--         , responseGetImportJobs $
--             newGetImportJobsResponse
--
--         , responseUpdatePushTemplate $
--             newUpdatePushTemplateResponse
--
--         , responseDeleteCampaign $
--             newDeleteCampaignResponse
--
--         , responseUpdateVoiceTemplate $
--             newUpdateVoiceTemplateResponse
--
--         , responseUpdateCampaign $
--             newUpdateCampaignResponse
--
--         , responseCreateRecommenderConfiguration $
--             newCreateRecommenderConfigurationResponse
--
--         , responseUpdateTemplateActiveVersion $
--             newUpdateTemplateActiveVersionResponse
--
--         , responseDeletePushTemplate $
--             newDeletePushTemplateResponse
--
--         , responseCreateJourney $
--             newCreateJourneyResponse
--
--         , responseGetImportJob $
--             newGetImportJobResponse
--
--         , responseGetSegmentVersions $
--             newGetSegmentVersionsResponse
--
--         , responseGetApps $
--             newGetAppsResponse
--
--         , responseGetSegmentImportJobs $
--             newGetSegmentImportJobsResponse
--
--         , responseGetApnsSandboxChannel $
--             newGetApnsSandboxChannelResponse
--
--         , responseSendMessages $
--             newSendMessagesResponse
--
--         , responseCreateSmsTemplate $
--             newCreateSmsTemplateResponse
--
--         , responseRemoveAttributes $
--             newRemoveAttributesResponse
--
--         , responseGetApnsChannel $
--             newGetApnsChannelResponse
--
--         , responsePhoneNumberValidate $
--             newPhoneNumberValidateResponse
--
--         , responseGetEmailChannel $
--             newGetEmailChannelResponse
--
--         , responsePutEventStream $
--             newPutEventStreamResponse
--
--         , responseGetJourneyExecutionActivityMetrics $
--             newGetJourneyExecutionActivityMetricsResponse
--
--         , responseUpdateApnsChannel $
--             newUpdateApnsChannelResponse
--
--         , responseDeleteApnsChannel $
--             newDeleteApnsChannelResponse
--
--         , responseGetBaiduChannel $
--             newGetBaiduChannelResponse
--
--         , responseGetChannels $
--             newGetChannelsResponse
--
--         , responseGetRecommenderConfigurations $
--             newGetRecommenderConfigurationsResponse
--
--         , responseUpdateGcmChannel $
--             newUpdateGcmChannelResponse
--
--         , responseDeleteGcmChannel $
--             newDeleteGcmChannelResponse
--
--         , responseGetJourneyExecutionMetrics $
--             newGetJourneyExecutionMetricsResponse
--
--         , responseGetVoiceChannel $
--             newGetVoiceChannelResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApnsVoipSandboxChannel $
--             newUpdateApnsVoipSandboxChannelResponse
--
--         , responseDeleteApnsVoipSandboxChannel $
--             newDeleteApnsVoipSandboxChannelResponse
--
--         , responseGetVoiceTemplate $
--             newGetVoiceTemplateResponse
--
--         , responseGetSmsChannel $
--             newGetSmsChannelResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetEndpoint $
--             newGetEndpointResponse
--
--         , responseGetApplicationDateRangeKpi $
--             newGetApplicationDateRangeKpiResponse
--
--         , responseGetAdmChannel $
--             newGetAdmChannelResponse
--
--         , responseGetRecommenderConfiguration $
--             newGetRecommenderConfigurationResponse
--
--         , responseGetSegmentExportJobs $
--             newGetSegmentExportJobsResponse
--
--         , responseUpdateSegment $
--             newUpdateSegmentResponse
--
--         , responseDeleteSegment $
--             newDeleteSegmentResponse
--
--         , responseCreatePushTemplate $
--             newCreatePushTemplateResponse
--
--         , responseDeleteAdmChannel $
--             newDeleteAdmChannelResponse
--
--         , responseUpdateRecommenderConfiguration $
--             newUpdateRecommenderConfigurationResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseCreateCampaign $
--             newCreateCampaignResponse
--
--         , responseUpdateEndpoint $
--             newUpdateEndpointResponse
--
--         , responseGetEmailTemplate $
--             newGetEmailTemplateResponse
--
--         , responseDeleteRecommenderConfiguration $
--             newDeleteRecommenderConfigurationResponse
--
--         , responseUpdateAdmChannel $
--             newUpdateAdmChannelResponse
--
--         , responseDeleteSmsChannel $
--             newDeleteSmsChannelResponse
--
--         , responseGetJourneyDateRangeKpi $
--             newGetJourneyDateRangeKpiResponse
--
--         , responseGetApp $
--             newGetAppResponse
--
--         , responseCreateExportJob $
--             newCreateExportJobResponse
--
--         , responseGetUserEndpoints $
--             newGetUserEndpointsResponse
--
--         , responseGetSegmentVersion $
--             newGetSegmentVersionResponse
--
--         , responseUpdateSmsChannel $
--             newUpdateSmsChannelResponse
--
--         , responseCreateSegment $
--             newCreateSegmentResponse
--
--         , responseDeleteSmsTemplate $
--             newDeleteSmsTemplateResponse
--
--         , responseUpdateSmsTemplate $
--             newUpdateSmsTemplateResponse
--
--         , responseGetGcmChannel $
--             newGetGcmChannelResponse
--
--         , responseDeleteVoiceChannel $
--             newDeleteVoiceChannelResponse
--
--         , responseUpdateVoiceChannel $
--             newUpdateVoiceChannelResponse
--
--         , responseGetApnsVoipSandboxChannel $
--             newGetApnsVoipSandboxChannelResponse
--
--         , responseDeleteJourney $
--             newDeleteJourneyResponse
--
--         , responseGetCampaignDateRangeKpi $
--             newGetCampaignDateRangeKpiResponse
--
--         , responseUpdateJourney $
--             newUpdateJourneyResponse
--
--         , responseListTemplates $
--             newListTemplatesResponse
--
--         , responseDeleteBaiduChannel $
--             newDeleteBaiduChannelResponse
--
--         , responseGetCampaignVersions $
--             newGetCampaignVersionsResponse
--
--         , responseGetApplicationSettings $
--             newGetApplicationSettingsResponse
--
--         , responseGetApnsVoipChannel $
--             newGetApnsVoipChannelResponse
--
--         , responseListJourneys $
--             newListJourneysResponse
--
--         , responseDeleteEventStream $
--             newDeleteEventStreamResponse
--
--         , responseUpdateBaiduChannel $
--             newUpdateBaiduChannelResponse
--
--         , responseGetExportJobs $
--             newGetExportJobsResponse
--
--         , responseGetSegments $
--             newGetSegmentsResponse
--
--         , responseGetJourney $
--             newGetJourneyResponse
--
--         , responsePutEvents $
--             newPutEventsResponse
--
--         , responseDeleteApnsVoipChannel $
--             newDeleteApnsVoipChannelResponse
--
--         , responseUpdateApnsVoipChannel $
--             newUpdateApnsVoipChannelResponse
--
--         , responseCreateImportJob $
--             newCreateImportJobResponse
--
--         , responseUpdateEmailChannel $
--             newUpdateEmailChannelResponse
--
--         , responseGetEventStream $
--             newGetEventStreamResponse
--
--         , responseSendUsersMessages $
--             newSendUsersMessagesResponse
--
--         , responseDeleteEmailChannel $
--             newDeleteEmailChannelResponse
--
--         , responseUpdateApplicationSettings $
--             newUpdateApplicationSettingsResponse
--
--         , responseUpdateJourneyState $
--             newUpdateJourneyStateResponse
--
--         , responseListTemplateVersions $
--             newListTemplateVersionsResponse
--
--         , responseDeleteApnsSandboxChannel $
--             newDeleteApnsSandboxChannelResponse
--
--         , responseGetCampaignActivities $
--             newGetCampaignActivitiesResponse
--
--         , responseUpdateApnsSandboxChannel $
--             newUpdateApnsSandboxChannelResponse
--
--         , responseGetCampaigns $
--             newGetCampaignsResponse
--
--         , responseGetSmsTemplate $
--             newGetSmsTemplateResponse
--
--         , responseGetPushTemplate $
--             newGetPushTemplateResponse
--
--         , responseGetCampaign $
--             newGetCampaignResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseDeleteUserEndpoints $
--             newDeleteUserEndpointsResponse
--
--         , responseCreateEmailTemplate $
--             newCreateEmailTemplateResponse
--
--         , responseUpdateEmailTemplate $
--             newUpdateEmailTemplateResponse
--
--         , responseDeleteEmailTemplate $
--             newDeleteEmailTemplateResponse
--
--         , responseCreateApp $
--             newCreateAppResponse
--
--         , responseUpdateEndpointsBatch $
--             newUpdateEndpointsBatchResponse
--
--         , responseGetExportJob $
--             newGetExportJobResponse
--
--         , responseGetSegment $
--             newGetSegmentResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetCampaignVersion $
--             newGetCampaignVersionResponse
--
--         , responseCreateVoiceTemplate $
--             newCreateVoiceTemplateResponse
--
--           ]
--     ]

-- Requests

requestDeleteVoiceTemplate :: DeleteVoiceTemplate -> TestTree
requestDeleteVoiceTemplate =
  req
    "DeleteVoiceTemplate"
    "fixture/DeleteVoiceTemplate.yaml"

requestGetImportJobs :: GetImportJobs -> TestTree
requestGetImportJobs =
  req
    "GetImportJobs"
    "fixture/GetImportJobs.yaml"

requestUpdatePushTemplate :: UpdatePushTemplate -> TestTree
requestUpdatePushTemplate =
  req
    "UpdatePushTemplate"
    "fixture/UpdatePushTemplate.yaml"

requestDeleteCampaign :: DeleteCampaign -> TestTree
requestDeleteCampaign =
  req
    "DeleteCampaign"
    "fixture/DeleteCampaign.yaml"

requestUpdateVoiceTemplate :: UpdateVoiceTemplate -> TestTree
requestUpdateVoiceTemplate =
  req
    "UpdateVoiceTemplate"
    "fixture/UpdateVoiceTemplate.yaml"

requestUpdateCampaign :: UpdateCampaign -> TestTree
requestUpdateCampaign =
  req
    "UpdateCampaign"
    "fixture/UpdateCampaign.yaml"

requestCreateRecommenderConfiguration :: CreateRecommenderConfiguration' -> TestTree
requestCreateRecommenderConfiguration =
  req
    "CreateRecommenderConfiguration"
    "fixture/CreateRecommenderConfiguration.yaml"

requestUpdateTemplateActiveVersion :: UpdateTemplateActiveVersion -> TestTree
requestUpdateTemplateActiveVersion =
  req
    "UpdateTemplateActiveVersion"
    "fixture/UpdateTemplateActiveVersion.yaml"

requestDeletePushTemplate :: DeletePushTemplate -> TestTree
requestDeletePushTemplate =
  req
    "DeletePushTemplate"
    "fixture/DeletePushTemplate.yaml"

requestCreateJourney :: CreateJourney -> TestTree
requestCreateJourney =
  req
    "CreateJourney"
    "fixture/CreateJourney.yaml"

requestGetImportJob :: GetImportJob -> TestTree
requestGetImportJob =
  req
    "GetImportJob"
    "fixture/GetImportJob.yaml"

requestGetSegmentVersions :: GetSegmentVersions -> TestTree
requestGetSegmentVersions =
  req
    "GetSegmentVersions"
    "fixture/GetSegmentVersions.yaml"

requestGetApps :: GetApps -> TestTree
requestGetApps =
  req
    "GetApps"
    "fixture/GetApps.yaml"

requestGetSegmentImportJobs :: GetSegmentImportJobs -> TestTree
requestGetSegmentImportJobs =
  req
    "GetSegmentImportJobs"
    "fixture/GetSegmentImportJobs.yaml"

requestGetApnsSandboxChannel :: GetApnsSandboxChannel -> TestTree
requestGetApnsSandboxChannel =
  req
    "GetApnsSandboxChannel"
    "fixture/GetApnsSandboxChannel.yaml"

requestSendMessages :: SendMessages -> TestTree
requestSendMessages =
  req
    "SendMessages"
    "fixture/SendMessages.yaml"

requestCreateSmsTemplate :: CreateSmsTemplate -> TestTree
requestCreateSmsTemplate =
  req
    "CreateSmsTemplate"
    "fixture/CreateSmsTemplate.yaml"

requestRemoveAttributes :: RemoveAttributes -> TestTree
requestRemoveAttributes =
  req
    "RemoveAttributes"
    "fixture/RemoveAttributes.yaml"

requestGetApnsChannel :: GetApnsChannel -> TestTree
requestGetApnsChannel =
  req
    "GetApnsChannel"
    "fixture/GetApnsChannel.yaml"

requestPhoneNumberValidate :: PhoneNumberValidate -> TestTree
requestPhoneNumberValidate =
  req
    "PhoneNumberValidate"
    "fixture/PhoneNumberValidate.yaml"

requestGetEmailChannel :: GetEmailChannel -> TestTree
requestGetEmailChannel =
  req
    "GetEmailChannel"
    "fixture/GetEmailChannel.yaml"

requestPutEventStream :: PutEventStream -> TestTree
requestPutEventStream =
  req
    "PutEventStream"
    "fixture/PutEventStream.yaml"

requestGetJourneyExecutionActivityMetrics :: GetJourneyExecutionActivityMetrics -> TestTree
requestGetJourneyExecutionActivityMetrics =
  req
    "GetJourneyExecutionActivityMetrics"
    "fixture/GetJourneyExecutionActivityMetrics.yaml"

requestUpdateApnsChannel :: UpdateApnsChannel -> TestTree
requestUpdateApnsChannel =
  req
    "UpdateApnsChannel"
    "fixture/UpdateApnsChannel.yaml"

requestDeleteApnsChannel :: DeleteApnsChannel -> TestTree
requestDeleteApnsChannel =
  req
    "DeleteApnsChannel"
    "fixture/DeleteApnsChannel.yaml"

requestGetBaiduChannel :: GetBaiduChannel -> TestTree
requestGetBaiduChannel =
  req
    "GetBaiduChannel"
    "fixture/GetBaiduChannel.yaml"

requestGetChannels :: GetChannels -> TestTree
requestGetChannels =
  req
    "GetChannels"
    "fixture/GetChannels.yaml"

requestGetRecommenderConfigurations :: GetRecommenderConfigurations -> TestTree
requestGetRecommenderConfigurations =
  req
    "GetRecommenderConfigurations"
    "fixture/GetRecommenderConfigurations.yaml"

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

requestGetJourneyExecutionMetrics :: GetJourneyExecutionMetrics -> TestTree
requestGetJourneyExecutionMetrics =
  req
    "GetJourneyExecutionMetrics"
    "fixture/GetJourneyExecutionMetrics.yaml"

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

requestGetVoiceTemplate :: GetVoiceTemplate -> TestTree
requestGetVoiceTemplate =
  req
    "GetVoiceTemplate"
    "fixture/GetVoiceTemplate.yaml"

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

requestGetEndpoint :: GetEndpoint -> TestTree
requestGetEndpoint =
  req
    "GetEndpoint"
    "fixture/GetEndpoint.yaml"

requestGetApplicationDateRangeKpi :: GetApplicationDateRangeKpi -> TestTree
requestGetApplicationDateRangeKpi =
  req
    "GetApplicationDateRangeKpi"
    "fixture/GetApplicationDateRangeKpi.yaml"

requestGetAdmChannel :: GetAdmChannel -> TestTree
requestGetAdmChannel =
  req
    "GetAdmChannel"
    "fixture/GetAdmChannel.yaml"

requestGetRecommenderConfiguration :: GetRecommenderConfiguration -> TestTree
requestGetRecommenderConfiguration =
  req
    "GetRecommenderConfiguration"
    "fixture/GetRecommenderConfiguration.yaml"

requestGetSegmentExportJobs :: GetSegmentExportJobs -> TestTree
requestGetSegmentExportJobs =
  req
    "GetSegmentExportJobs"
    "fixture/GetSegmentExportJobs.yaml"

requestUpdateSegment :: UpdateSegment -> TestTree
requestUpdateSegment =
  req
    "UpdateSegment"
    "fixture/UpdateSegment.yaml"

requestDeleteSegment :: DeleteSegment -> TestTree
requestDeleteSegment =
  req
    "DeleteSegment"
    "fixture/DeleteSegment.yaml"

requestCreatePushTemplate :: CreatePushTemplate -> TestTree
requestCreatePushTemplate =
  req
    "CreatePushTemplate"
    "fixture/CreatePushTemplate.yaml"

requestDeleteAdmChannel :: DeleteAdmChannel -> TestTree
requestDeleteAdmChannel =
  req
    "DeleteAdmChannel"
    "fixture/DeleteAdmChannel.yaml"

requestUpdateRecommenderConfiguration :: UpdateRecommenderConfiguration' -> TestTree
requestUpdateRecommenderConfiguration =
  req
    "UpdateRecommenderConfiguration"
    "fixture/UpdateRecommenderConfiguration.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint =
  req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestCreateCampaign :: CreateCampaign -> TestTree
requestCreateCampaign =
  req
    "CreateCampaign"
    "fixture/CreateCampaign.yaml"

requestUpdateEndpoint :: UpdateEndpoint -> TestTree
requestUpdateEndpoint =
  req
    "UpdateEndpoint"
    "fixture/UpdateEndpoint.yaml"

requestGetEmailTemplate :: GetEmailTemplate -> TestTree
requestGetEmailTemplate =
  req
    "GetEmailTemplate"
    "fixture/GetEmailTemplate.yaml"

requestDeleteRecommenderConfiguration :: DeleteRecommenderConfiguration -> TestTree
requestDeleteRecommenderConfiguration =
  req
    "DeleteRecommenderConfiguration"
    "fixture/DeleteRecommenderConfiguration.yaml"

requestUpdateAdmChannel :: UpdateAdmChannel -> TestTree
requestUpdateAdmChannel =
  req
    "UpdateAdmChannel"
    "fixture/UpdateAdmChannel.yaml"

requestDeleteSmsChannel :: DeleteSmsChannel -> TestTree
requestDeleteSmsChannel =
  req
    "DeleteSmsChannel"
    "fixture/DeleteSmsChannel.yaml"

requestGetJourneyDateRangeKpi :: GetJourneyDateRangeKpi -> TestTree
requestGetJourneyDateRangeKpi =
  req
    "GetJourneyDateRangeKpi"
    "fixture/GetJourneyDateRangeKpi.yaml"

requestGetApp :: GetApp -> TestTree
requestGetApp =
  req
    "GetApp"
    "fixture/GetApp.yaml"

requestCreateExportJob :: CreateExportJob -> TestTree
requestCreateExportJob =
  req
    "CreateExportJob"
    "fixture/CreateExportJob.yaml"

requestGetUserEndpoints :: GetUserEndpoints -> TestTree
requestGetUserEndpoints =
  req
    "GetUserEndpoints"
    "fixture/GetUserEndpoints.yaml"

requestGetSegmentVersion :: GetSegmentVersion -> TestTree
requestGetSegmentVersion =
  req
    "GetSegmentVersion"
    "fixture/GetSegmentVersion.yaml"

requestUpdateSmsChannel :: UpdateSmsChannel -> TestTree
requestUpdateSmsChannel =
  req
    "UpdateSmsChannel"
    "fixture/UpdateSmsChannel.yaml"

requestCreateSegment :: CreateSegment -> TestTree
requestCreateSegment =
  req
    "CreateSegment"
    "fixture/CreateSegment.yaml"

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

requestGetGcmChannel :: GetGcmChannel -> TestTree
requestGetGcmChannel =
  req
    "GetGcmChannel"
    "fixture/GetGcmChannel.yaml"

requestDeleteVoiceChannel :: DeleteVoiceChannel -> TestTree
requestDeleteVoiceChannel =
  req
    "DeleteVoiceChannel"
    "fixture/DeleteVoiceChannel.yaml"

requestUpdateVoiceChannel :: UpdateVoiceChannel -> TestTree
requestUpdateVoiceChannel =
  req
    "UpdateVoiceChannel"
    "fixture/UpdateVoiceChannel.yaml"

requestGetApnsVoipSandboxChannel :: GetApnsVoipSandboxChannel -> TestTree
requestGetApnsVoipSandboxChannel =
  req
    "GetApnsVoipSandboxChannel"
    "fixture/GetApnsVoipSandboxChannel.yaml"

requestDeleteJourney :: DeleteJourney -> TestTree
requestDeleteJourney =
  req
    "DeleteJourney"
    "fixture/DeleteJourney.yaml"

requestGetCampaignDateRangeKpi :: GetCampaignDateRangeKpi -> TestTree
requestGetCampaignDateRangeKpi =
  req
    "GetCampaignDateRangeKpi"
    "fixture/GetCampaignDateRangeKpi.yaml"

requestUpdateJourney :: UpdateJourney -> TestTree
requestUpdateJourney =
  req
    "UpdateJourney"
    "fixture/UpdateJourney.yaml"

requestListTemplates :: ListTemplates -> TestTree
requestListTemplates =
  req
    "ListTemplates"
    "fixture/ListTemplates.yaml"

requestDeleteBaiduChannel :: DeleteBaiduChannel -> TestTree
requestDeleteBaiduChannel =
  req
    "DeleteBaiduChannel"
    "fixture/DeleteBaiduChannel.yaml"

requestGetCampaignVersions :: GetCampaignVersions -> TestTree
requestGetCampaignVersions =
  req
    "GetCampaignVersions"
    "fixture/GetCampaignVersions.yaml"

requestGetApplicationSettings :: GetApplicationSettings -> TestTree
requestGetApplicationSettings =
  req
    "GetApplicationSettings"
    "fixture/GetApplicationSettings.yaml"

requestGetApnsVoipChannel :: GetApnsVoipChannel -> TestTree
requestGetApnsVoipChannel =
  req
    "GetApnsVoipChannel"
    "fixture/GetApnsVoipChannel.yaml"

requestListJourneys :: ListJourneys -> TestTree
requestListJourneys =
  req
    "ListJourneys"
    "fixture/ListJourneys.yaml"

requestDeleteEventStream :: DeleteEventStream -> TestTree
requestDeleteEventStream =
  req
    "DeleteEventStream"
    "fixture/DeleteEventStream.yaml"

requestUpdateBaiduChannel :: UpdateBaiduChannel -> TestTree
requestUpdateBaiduChannel =
  req
    "UpdateBaiduChannel"
    "fixture/UpdateBaiduChannel.yaml"

requestGetExportJobs :: GetExportJobs -> TestTree
requestGetExportJobs =
  req
    "GetExportJobs"
    "fixture/GetExportJobs.yaml"

requestGetSegments :: GetSegments -> TestTree
requestGetSegments =
  req
    "GetSegments"
    "fixture/GetSegments.yaml"

requestGetJourney :: GetJourney -> TestTree
requestGetJourney =
  req
    "GetJourney"
    "fixture/GetJourney.yaml"

requestPutEvents :: PutEvents -> TestTree
requestPutEvents =
  req
    "PutEvents"
    "fixture/PutEvents.yaml"

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

requestCreateImportJob :: CreateImportJob -> TestTree
requestCreateImportJob =
  req
    "CreateImportJob"
    "fixture/CreateImportJob.yaml"

requestUpdateEmailChannel :: UpdateEmailChannel -> TestTree
requestUpdateEmailChannel =
  req
    "UpdateEmailChannel"
    "fixture/UpdateEmailChannel.yaml"

requestGetEventStream :: GetEventStream -> TestTree
requestGetEventStream =
  req
    "GetEventStream"
    "fixture/GetEventStream.yaml"

requestSendUsersMessages :: SendUsersMessages -> TestTree
requestSendUsersMessages =
  req
    "SendUsersMessages"
    "fixture/SendUsersMessages.yaml"

requestDeleteEmailChannel :: DeleteEmailChannel -> TestTree
requestDeleteEmailChannel =
  req
    "DeleteEmailChannel"
    "fixture/DeleteEmailChannel.yaml"

requestUpdateApplicationSettings :: UpdateApplicationSettings -> TestTree
requestUpdateApplicationSettings =
  req
    "UpdateApplicationSettings"
    "fixture/UpdateApplicationSettings.yaml"

requestUpdateJourneyState :: UpdateJourneyState -> TestTree
requestUpdateJourneyState =
  req
    "UpdateJourneyState"
    "fixture/UpdateJourneyState.yaml"

requestListTemplateVersions :: ListTemplateVersions -> TestTree
requestListTemplateVersions =
  req
    "ListTemplateVersions"
    "fixture/ListTemplateVersions.yaml"

requestDeleteApnsSandboxChannel :: DeleteApnsSandboxChannel -> TestTree
requestDeleteApnsSandboxChannel =
  req
    "DeleteApnsSandboxChannel"
    "fixture/DeleteApnsSandboxChannel.yaml"

requestGetCampaignActivities :: GetCampaignActivities -> TestTree
requestGetCampaignActivities =
  req
    "GetCampaignActivities"
    "fixture/GetCampaignActivities.yaml"

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

requestGetSmsTemplate :: GetSmsTemplate -> TestTree
requestGetSmsTemplate =
  req
    "GetSmsTemplate"
    "fixture/GetSmsTemplate.yaml"

requestGetPushTemplate :: GetPushTemplate -> TestTree
requestGetPushTemplate =
  req
    "GetPushTemplate"
    "fixture/GetPushTemplate.yaml"

requestGetCampaign :: GetCampaign -> TestTree
requestGetCampaign =
  req
    "GetCampaign"
    "fixture/GetCampaign.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

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

requestUpdateEmailTemplate :: UpdateEmailTemplate -> TestTree
requestUpdateEmailTemplate =
  req
    "UpdateEmailTemplate"
    "fixture/UpdateEmailTemplate.yaml"

requestDeleteEmailTemplate :: DeleteEmailTemplate -> TestTree
requestDeleteEmailTemplate =
  req
    "DeleteEmailTemplate"
    "fixture/DeleteEmailTemplate.yaml"

requestCreateApp :: CreateApp -> TestTree
requestCreateApp =
  req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestUpdateEndpointsBatch :: UpdateEndpointsBatch -> TestTree
requestUpdateEndpointsBatch =
  req
    "UpdateEndpointsBatch"
    "fixture/UpdateEndpointsBatch.yaml"

requestGetExportJob :: GetExportJob -> TestTree
requestGetExportJob =
  req
    "GetExportJob"
    "fixture/GetExportJob.yaml"

requestGetSegment :: GetSegment -> TestTree
requestGetSegment =
  req
    "GetSegment"
    "fixture/GetSegment.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetCampaignVersion :: GetCampaignVersion -> TestTree
requestGetCampaignVersion =
  req
    "GetCampaignVersion"
    "fixture/GetCampaignVersion.yaml"

requestCreateVoiceTemplate :: CreateVoiceTemplate -> TestTree
requestCreateVoiceTemplate =
  req
    "CreateVoiceTemplate"
    "fixture/CreateVoiceTemplate.yaml"

-- Responses

responseDeleteVoiceTemplate :: DeleteVoiceTemplateResponse -> TestTree
responseDeleteVoiceTemplate =
  res
    "DeleteVoiceTemplateResponse"
    "fixture/DeleteVoiceTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVoiceTemplate)

responseGetImportJobs :: GetImportJobsResponse -> TestTree
responseGetImportJobs =
  res
    "GetImportJobsResponse"
    "fixture/GetImportJobsResponse.proto"
    defaultService
    (Proxy :: Proxy GetImportJobs)

responseUpdatePushTemplate :: UpdatePushTemplateResponse -> TestTree
responseUpdatePushTemplate =
  res
    "UpdatePushTemplateResponse"
    "fixture/UpdatePushTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePushTemplate)

responseDeleteCampaign :: DeleteCampaignResponse -> TestTree
responseDeleteCampaign =
  res
    "DeleteCampaignResponse"
    "fixture/DeleteCampaignResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCampaign)

responseUpdateVoiceTemplate :: UpdateVoiceTemplateResponse -> TestTree
responseUpdateVoiceTemplate =
  res
    "UpdateVoiceTemplateResponse"
    "fixture/UpdateVoiceTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateVoiceTemplate)

responseUpdateCampaign :: UpdateCampaignResponse -> TestTree
responseUpdateCampaign =
  res
    "UpdateCampaignResponse"
    "fixture/UpdateCampaignResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCampaign)

responseCreateRecommenderConfiguration :: CreateRecommenderConfigurationResponse -> TestTree
responseCreateRecommenderConfiguration =
  res
    "CreateRecommenderConfigurationResponse"
    "fixture/CreateRecommenderConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRecommenderConfiguration')

responseUpdateTemplateActiveVersion :: UpdateTemplateActiveVersionResponse -> TestTree
responseUpdateTemplateActiveVersion =
  res
    "UpdateTemplateActiveVersionResponse"
    "fixture/UpdateTemplateActiveVersionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTemplateActiveVersion)

responseDeletePushTemplate :: DeletePushTemplateResponse -> TestTree
responseDeletePushTemplate =
  res
    "DeletePushTemplateResponse"
    "fixture/DeletePushTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePushTemplate)

responseCreateJourney :: CreateJourneyResponse -> TestTree
responseCreateJourney =
  res
    "CreateJourneyResponse"
    "fixture/CreateJourneyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateJourney)

responseGetImportJob :: GetImportJobResponse -> TestTree
responseGetImportJob =
  res
    "GetImportJobResponse"
    "fixture/GetImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy GetImportJob)

responseGetSegmentVersions :: GetSegmentVersionsResponse -> TestTree
responseGetSegmentVersions =
  res
    "GetSegmentVersionsResponse"
    "fixture/GetSegmentVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSegmentVersions)

responseGetApps :: GetAppsResponse -> TestTree
responseGetApps =
  res
    "GetAppsResponse"
    "fixture/GetAppsResponse.proto"
    defaultService
    (Proxy :: Proxy GetApps)

responseGetSegmentImportJobs :: GetSegmentImportJobsResponse -> TestTree
responseGetSegmentImportJobs =
  res
    "GetSegmentImportJobsResponse"
    "fixture/GetSegmentImportJobsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSegmentImportJobs)

responseGetApnsSandboxChannel :: GetApnsSandboxChannelResponse -> TestTree
responseGetApnsSandboxChannel =
  res
    "GetApnsSandboxChannelResponse"
    "fixture/GetApnsSandboxChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetApnsSandboxChannel)

responseSendMessages :: SendMessagesResponse -> TestTree
responseSendMessages =
  res
    "SendMessagesResponse"
    "fixture/SendMessagesResponse.proto"
    defaultService
    (Proxy :: Proxy SendMessages)

responseCreateSmsTemplate :: CreateSmsTemplateResponse -> TestTree
responseCreateSmsTemplate =
  res
    "CreateSmsTemplateResponse"
    "fixture/CreateSmsTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSmsTemplate)

responseRemoveAttributes :: RemoveAttributesResponse -> TestTree
responseRemoveAttributes =
  res
    "RemoveAttributesResponse"
    "fixture/RemoveAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveAttributes)

responseGetApnsChannel :: GetApnsChannelResponse -> TestTree
responseGetApnsChannel =
  res
    "GetApnsChannelResponse"
    "fixture/GetApnsChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetApnsChannel)

responsePhoneNumberValidate :: PhoneNumberValidateResponse -> TestTree
responsePhoneNumberValidate =
  res
    "PhoneNumberValidateResponse"
    "fixture/PhoneNumberValidateResponse.proto"
    defaultService
    (Proxy :: Proxy PhoneNumberValidate)

responseGetEmailChannel :: GetEmailChannelResponse -> TestTree
responseGetEmailChannel =
  res
    "GetEmailChannelResponse"
    "fixture/GetEmailChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetEmailChannel)

responsePutEventStream :: PutEventStreamResponse -> TestTree
responsePutEventStream =
  res
    "PutEventStreamResponse"
    "fixture/PutEventStreamResponse.proto"
    defaultService
    (Proxy :: Proxy PutEventStream)

responseGetJourneyExecutionActivityMetrics :: GetJourneyExecutionActivityMetricsResponse -> TestTree
responseGetJourneyExecutionActivityMetrics =
  res
    "GetJourneyExecutionActivityMetricsResponse"
    "fixture/GetJourneyExecutionActivityMetricsResponse.proto"
    defaultService
    (Proxy :: Proxy GetJourneyExecutionActivityMetrics)

responseUpdateApnsChannel :: UpdateApnsChannelResponse -> TestTree
responseUpdateApnsChannel =
  res
    "UpdateApnsChannelResponse"
    "fixture/UpdateApnsChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApnsChannel)

responseDeleteApnsChannel :: DeleteApnsChannelResponse -> TestTree
responseDeleteApnsChannel =
  res
    "DeleteApnsChannelResponse"
    "fixture/DeleteApnsChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApnsChannel)

responseGetBaiduChannel :: GetBaiduChannelResponse -> TestTree
responseGetBaiduChannel =
  res
    "GetBaiduChannelResponse"
    "fixture/GetBaiduChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetBaiduChannel)

responseGetChannels :: GetChannelsResponse -> TestTree
responseGetChannels =
  res
    "GetChannelsResponse"
    "fixture/GetChannelsResponse.proto"
    defaultService
    (Proxy :: Proxy GetChannels)

responseGetRecommenderConfigurations :: GetRecommenderConfigurationsResponse -> TestTree
responseGetRecommenderConfigurations =
  res
    "GetRecommenderConfigurationsResponse"
    "fixture/GetRecommenderConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRecommenderConfigurations)

responseUpdateGcmChannel :: UpdateGcmChannelResponse -> TestTree
responseUpdateGcmChannel =
  res
    "UpdateGcmChannelResponse"
    "fixture/UpdateGcmChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGcmChannel)

responseDeleteGcmChannel :: DeleteGcmChannelResponse -> TestTree
responseDeleteGcmChannel =
  res
    "DeleteGcmChannelResponse"
    "fixture/DeleteGcmChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGcmChannel)

responseGetJourneyExecutionMetrics :: GetJourneyExecutionMetricsResponse -> TestTree
responseGetJourneyExecutionMetrics =
  res
    "GetJourneyExecutionMetricsResponse"
    "fixture/GetJourneyExecutionMetricsResponse.proto"
    defaultService
    (Proxy :: Proxy GetJourneyExecutionMetrics)

responseGetVoiceChannel :: GetVoiceChannelResponse -> TestTree
responseGetVoiceChannel =
  res
    "GetVoiceChannelResponse"
    "fixture/GetVoiceChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetVoiceChannel)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseUpdateApnsVoipSandboxChannel :: UpdateApnsVoipSandboxChannelResponse -> TestTree
responseUpdateApnsVoipSandboxChannel =
  res
    "UpdateApnsVoipSandboxChannelResponse"
    "fixture/UpdateApnsVoipSandboxChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApnsVoipSandboxChannel)

responseDeleteApnsVoipSandboxChannel :: DeleteApnsVoipSandboxChannelResponse -> TestTree
responseDeleteApnsVoipSandboxChannel =
  res
    "DeleteApnsVoipSandboxChannelResponse"
    "fixture/DeleteApnsVoipSandboxChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApnsVoipSandboxChannel)

responseGetVoiceTemplate :: GetVoiceTemplateResponse -> TestTree
responseGetVoiceTemplate =
  res
    "GetVoiceTemplateResponse"
    "fixture/GetVoiceTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetVoiceTemplate)

responseGetSmsChannel :: GetSmsChannelResponse -> TestTree
responseGetSmsChannel =
  res
    "GetSmsChannelResponse"
    "fixture/GetSmsChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetSmsChannel)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseGetEndpoint :: GetEndpointResponse -> TestTree
responseGetEndpoint =
  res
    "GetEndpointResponse"
    "fixture/GetEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy GetEndpoint)

responseGetApplicationDateRangeKpi :: GetApplicationDateRangeKpiResponse -> TestTree
responseGetApplicationDateRangeKpi =
  res
    "GetApplicationDateRangeKpiResponse"
    "fixture/GetApplicationDateRangeKpiResponse.proto"
    defaultService
    (Proxy :: Proxy GetApplicationDateRangeKpi)

responseGetAdmChannel :: GetAdmChannelResponse -> TestTree
responseGetAdmChannel =
  res
    "GetAdmChannelResponse"
    "fixture/GetAdmChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetAdmChannel)

responseGetRecommenderConfiguration :: GetRecommenderConfigurationResponse -> TestTree
responseGetRecommenderConfiguration =
  res
    "GetRecommenderConfigurationResponse"
    "fixture/GetRecommenderConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetRecommenderConfiguration)

responseGetSegmentExportJobs :: GetSegmentExportJobsResponse -> TestTree
responseGetSegmentExportJobs =
  res
    "GetSegmentExportJobsResponse"
    "fixture/GetSegmentExportJobsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSegmentExportJobs)

responseUpdateSegment :: UpdateSegmentResponse -> TestTree
responseUpdateSegment =
  res
    "UpdateSegmentResponse"
    "fixture/UpdateSegmentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSegment)

responseDeleteSegment :: DeleteSegmentResponse -> TestTree
responseDeleteSegment =
  res
    "DeleteSegmentResponse"
    "fixture/DeleteSegmentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSegment)

responseCreatePushTemplate :: CreatePushTemplateResponse -> TestTree
responseCreatePushTemplate =
  res
    "CreatePushTemplateResponse"
    "fixture/CreatePushTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePushTemplate)

responseDeleteAdmChannel :: DeleteAdmChannelResponse -> TestTree
responseDeleteAdmChannel =
  res
    "DeleteAdmChannelResponse"
    "fixture/DeleteAdmChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAdmChannel)

responseUpdateRecommenderConfiguration :: UpdateRecommenderConfigurationResponse -> TestTree
responseUpdateRecommenderConfiguration =
  res
    "UpdateRecommenderConfigurationResponse"
    "fixture/UpdateRecommenderConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRecommenderConfiguration')

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEndpoint)

responseCreateCampaign :: CreateCampaignResponse -> TestTree
responseCreateCampaign =
  res
    "CreateCampaignResponse"
    "fixture/CreateCampaignResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCampaign)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEndpoint)

responseGetEmailTemplate :: GetEmailTemplateResponse -> TestTree
responseGetEmailTemplate =
  res
    "GetEmailTemplateResponse"
    "fixture/GetEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetEmailTemplate)

responseDeleteRecommenderConfiguration :: DeleteRecommenderConfigurationResponse -> TestTree
responseDeleteRecommenderConfiguration =
  res
    "DeleteRecommenderConfigurationResponse"
    "fixture/DeleteRecommenderConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRecommenderConfiguration)

responseUpdateAdmChannel :: UpdateAdmChannelResponse -> TestTree
responseUpdateAdmChannel =
  res
    "UpdateAdmChannelResponse"
    "fixture/UpdateAdmChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAdmChannel)

responseDeleteSmsChannel :: DeleteSmsChannelResponse -> TestTree
responseDeleteSmsChannel =
  res
    "DeleteSmsChannelResponse"
    "fixture/DeleteSmsChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSmsChannel)

responseGetJourneyDateRangeKpi :: GetJourneyDateRangeKpiResponse -> TestTree
responseGetJourneyDateRangeKpi =
  res
    "GetJourneyDateRangeKpiResponse"
    "fixture/GetJourneyDateRangeKpiResponse.proto"
    defaultService
    (Proxy :: Proxy GetJourneyDateRangeKpi)

responseGetApp :: GetAppResponse -> TestTree
responseGetApp =
  res
    "GetAppResponse"
    "fixture/GetAppResponse.proto"
    defaultService
    (Proxy :: Proxy GetApp)

responseCreateExportJob :: CreateExportJobResponse -> TestTree
responseCreateExportJob =
  res
    "CreateExportJobResponse"
    "fixture/CreateExportJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateExportJob)

responseGetUserEndpoints :: GetUserEndpointsResponse -> TestTree
responseGetUserEndpoints =
  res
    "GetUserEndpointsResponse"
    "fixture/GetUserEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy GetUserEndpoints)

responseGetSegmentVersion :: GetSegmentVersionResponse -> TestTree
responseGetSegmentVersion =
  res
    "GetSegmentVersionResponse"
    "fixture/GetSegmentVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetSegmentVersion)

responseUpdateSmsChannel :: UpdateSmsChannelResponse -> TestTree
responseUpdateSmsChannel =
  res
    "UpdateSmsChannelResponse"
    "fixture/UpdateSmsChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSmsChannel)

responseCreateSegment :: CreateSegmentResponse -> TestTree
responseCreateSegment =
  res
    "CreateSegmentResponse"
    "fixture/CreateSegmentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSegment)

responseDeleteSmsTemplate :: DeleteSmsTemplateResponse -> TestTree
responseDeleteSmsTemplate =
  res
    "DeleteSmsTemplateResponse"
    "fixture/DeleteSmsTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSmsTemplate)

responseUpdateSmsTemplate :: UpdateSmsTemplateResponse -> TestTree
responseUpdateSmsTemplate =
  res
    "UpdateSmsTemplateResponse"
    "fixture/UpdateSmsTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSmsTemplate)

responseGetGcmChannel :: GetGcmChannelResponse -> TestTree
responseGetGcmChannel =
  res
    "GetGcmChannelResponse"
    "fixture/GetGcmChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetGcmChannel)

responseDeleteVoiceChannel :: DeleteVoiceChannelResponse -> TestTree
responseDeleteVoiceChannel =
  res
    "DeleteVoiceChannelResponse"
    "fixture/DeleteVoiceChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVoiceChannel)

responseUpdateVoiceChannel :: UpdateVoiceChannelResponse -> TestTree
responseUpdateVoiceChannel =
  res
    "UpdateVoiceChannelResponse"
    "fixture/UpdateVoiceChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateVoiceChannel)

responseGetApnsVoipSandboxChannel :: GetApnsVoipSandboxChannelResponse -> TestTree
responseGetApnsVoipSandboxChannel =
  res
    "GetApnsVoipSandboxChannelResponse"
    "fixture/GetApnsVoipSandboxChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetApnsVoipSandboxChannel)

responseDeleteJourney :: DeleteJourneyResponse -> TestTree
responseDeleteJourney =
  res
    "DeleteJourneyResponse"
    "fixture/DeleteJourneyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteJourney)

responseGetCampaignDateRangeKpi :: GetCampaignDateRangeKpiResponse -> TestTree
responseGetCampaignDateRangeKpi =
  res
    "GetCampaignDateRangeKpiResponse"
    "fixture/GetCampaignDateRangeKpiResponse.proto"
    defaultService
    (Proxy :: Proxy GetCampaignDateRangeKpi)

responseUpdateJourney :: UpdateJourneyResponse -> TestTree
responseUpdateJourney =
  res
    "UpdateJourneyResponse"
    "fixture/UpdateJourneyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateJourney)

responseListTemplates :: ListTemplatesResponse -> TestTree
responseListTemplates =
  res
    "ListTemplatesResponse"
    "fixture/ListTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTemplates)

responseDeleteBaiduChannel :: DeleteBaiduChannelResponse -> TestTree
responseDeleteBaiduChannel =
  res
    "DeleteBaiduChannelResponse"
    "fixture/DeleteBaiduChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBaiduChannel)

responseGetCampaignVersions :: GetCampaignVersionsResponse -> TestTree
responseGetCampaignVersions =
  res
    "GetCampaignVersionsResponse"
    "fixture/GetCampaignVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetCampaignVersions)

responseGetApplicationSettings :: GetApplicationSettingsResponse -> TestTree
responseGetApplicationSettings =
  res
    "GetApplicationSettingsResponse"
    "fixture/GetApplicationSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy GetApplicationSettings)

responseGetApnsVoipChannel :: GetApnsVoipChannelResponse -> TestTree
responseGetApnsVoipChannel =
  res
    "GetApnsVoipChannelResponse"
    "fixture/GetApnsVoipChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetApnsVoipChannel)

responseListJourneys :: ListJourneysResponse -> TestTree
responseListJourneys =
  res
    "ListJourneysResponse"
    "fixture/ListJourneysResponse.proto"
    defaultService
    (Proxy :: Proxy ListJourneys)

responseDeleteEventStream :: DeleteEventStreamResponse -> TestTree
responseDeleteEventStream =
  res
    "DeleteEventStreamResponse"
    "fixture/DeleteEventStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEventStream)

responseUpdateBaiduChannel :: UpdateBaiduChannelResponse -> TestTree
responseUpdateBaiduChannel =
  res
    "UpdateBaiduChannelResponse"
    "fixture/UpdateBaiduChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBaiduChannel)

responseGetExportJobs :: GetExportJobsResponse -> TestTree
responseGetExportJobs =
  res
    "GetExportJobsResponse"
    "fixture/GetExportJobsResponse.proto"
    defaultService
    (Proxy :: Proxy GetExportJobs)

responseGetSegments :: GetSegmentsResponse -> TestTree
responseGetSegments =
  res
    "GetSegmentsResponse"
    "fixture/GetSegmentsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSegments)

responseGetJourney :: GetJourneyResponse -> TestTree
responseGetJourney =
  res
    "GetJourneyResponse"
    "fixture/GetJourneyResponse.proto"
    defaultService
    (Proxy :: Proxy GetJourney)

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents =
  res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    defaultService
    (Proxy :: Proxy PutEvents)

responseDeleteApnsVoipChannel :: DeleteApnsVoipChannelResponse -> TestTree
responseDeleteApnsVoipChannel =
  res
    "DeleteApnsVoipChannelResponse"
    "fixture/DeleteApnsVoipChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApnsVoipChannel)

responseUpdateApnsVoipChannel :: UpdateApnsVoipChannelResponse -> TestTree
responseUpdateApnsVoipChannel =
  res
    "UpdateApnsVoipChannelResponse"
    "fixture/UpdateApnsVoipChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApnsVoipChannel)

responseCreateImportJob :: CreateImportJobResponse -> TestTree
responseCreateImportJob =
  res
    "CreateImportJobResponse"
    "fixture/CreateImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImportJob)

responseUpdateEmailChannel :: UpdateEmailChannelResponse -> TestTree
responseUpdateEmailChannel =
  res
    "UpdateEmailChannelResponse"
    "fixture/UpdateEmailChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEmailChannel)

responseGetEventStream :: GetEventStreamResponse -> TestTree
responseGetEventStream =
  res
    "GetEventStreamResponse"
    "fixture/GetEventStreamResponse.proto"
    defaultService
    (Proxy :: Proxy GetEventStream)

responseSendUsersMessages :: SendUsersMessagesResponse -> TestTree
responseSendUsersMessages =
  res
    "SendUsersMessagesResponse"
    "fixture/SendUsersMessagesResponse.proto"
    defaultService
    (Proxy :: Proxy SendUsersMessages)

responseDeleteEmailChannel :: DeleteEmailChannelResponse -> TestTree
responseDeleteEmailChannel =
  res
    "DeleteEmailChannelResponse"
    "fixture/DeleteEmailChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEmailChannel)

responseUpdateApplicationSettings :: UpdateApplicationSettingsResponse -> TestTree
responseUpdateApplicationSettings =
  res
    "UpdateApplicationSettingsResponse"
    "fixture/UpdateApplicationSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApplicationSettings)

responseUpdateJourneyState :: UpdateJourneyStateResponse -> TestTree
responseUpdateJourneyState =
  res
    "UpdateJourneyStateResponse"
    "fixture/UpdateJourneyStateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateJourneyState)

responseListTemplateVersions :: ListTemplateVersionsResponse -> TestTree
responseListTemplateVersions =
  res
    "ListTemplateVersionsResponse"
    "fixture/ListTemplateVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTemplateVersions)

responseDeleteApnsSandboxChannel :: DeleteApnsSandboxChannelResponse -> TestTree
responseDeleteApnsSandboxChannel =
  res
    "DeleteApnsSandboxChannelResponse"
    "fixture/DeleteApnsSandboxChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApnsSandboxChannel)

responseGetCampaignActivities :: GetCampaignActivitiesResponse -> TestTree
responseGetCampaignActivities =
  res
    "GetCampaignActivitiesResponse"
    "fixture/GetCampaignActivitiesResponse.proto"
    defaultService
    (Proxy :: Proxy GetCampaignActivities)

responseUpdateApnsSandboxChannel :: UpdateApnsSandboxChannelResponse -> TestTree
responseUpdateApnsSandboxChannel =
  res
    "UpdateApnsSandboxChannelResponse"
    "fixture/UpdateApnsSandboxChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApnsSandboxChannel)

responseGetCampaigns :: GetCampaignsResponse -> TestTree
responseGetCampaigns =
  res
    "GetCampaignsResponse"
    "fixture/GetCampaignsResponse.proto"
    defaultService
    (Proxy :: Proxy GetCampaigns)

responseGetSmsTemplate :: GetSmsTemplateResponse -> TestTree
responseGetSmsTemplate =
  res
    "GetSmsTemplateResponse"
    "fixture/GetSmsTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetSmsTemplate)

responseGetPushTemplate :: GetPushTemplateResponse -> TestTree
responseGetPushTemplate =
  res
    "GetPushTemplateResponse"
    "fixture/GetPushTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetPushTemplate)

responseGetCampaign :: GetCampaignResponse -> TestTree
responseGetCampaign =
  res
    "GetCampaignResponse"
    "fixture/GetCampaignResponse.proto"
    defaultService
    (Proxy :: Proxy GetCampaign)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApp)

responseDeleteUserEndpoints :: DeleteUserEndpointsResponse -> TestTree
responseDeleteUserEndpoints =
  res
    "DeleteUserEndpointsResponse"
    "fixture/DeleteUserEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserEndpoints)

responseCreateEmailTemplate :: CreateEmailTemplateResponse -> TestTree
responseCreateEmailTemplate =
  res
    "CreateEmailTemplateResponse"
    "fixture/CreateEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEmailTemplate)

responseUpdateEmailTemplate :: UpdateEmailTemplateResponse -> TestTree
responseUpdateEmailTemplate =
  res
    "UpdateEmailTemplateResponse"
    "fixture/UpdateEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEmailTemplate)

responseDeleteEmailTemplate :: DeleteEmailTemplateResponse -> TestTree
responseDeleteEmailTemplate =
  res
    "DeleteEmailTemplateResponse"
    "fixture/DeleteEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEmailTemplate)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApp)

responseUpdateEndpointsBatch :: UpdateEndpointsBatchResponse -> TestTree
responseUpdateEndpointsBatch =
  res
    "UpdateEndpointsBatchResponse"
    "fixture/UpdateEndpointsBatchResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEndpointsBatch)

responseGetExportJob :: GetExportJobResponse -> TestTree
responseGetExportJob =
  res
    "GetExportJobResponse"
    "fixture/GetExportJobResponse.proto"
    defaultService
    (Proxy :: Proxy GetExportJob)

responseGetSegment :: GetSegmentResponse -> TestTree
responseGetSegment =
  res
    "GetSegmentResponse"
    "fixture/GetSegmentResponse.proto"
    defaultService
    (Proxy :: Proxy GetSegment)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseGetCampaignVersion :: GetCampaignVersionResponse -> TestTree
responseGetCampaignVersion =
  res
    "GetCampaignVersionResponse"
    "fixture/GetCampaignVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetCampaignVersion)

responseCreateVoiceTemplate :: CreateVoiceTemplateResponse -> TestTree
responseCreateVoiceTemplate =
  res
    "CreateVoiceTemplateResponse"
    "fixture/CreateVoiceTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVoiceTemplate)
