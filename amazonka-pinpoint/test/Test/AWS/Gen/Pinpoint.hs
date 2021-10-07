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
--         [ requestUpdateCampaign $
--             newUpdateCampaign
--
--         , requestUpdatePushTemplate $
--             newUpdatePushTemplate
--
--         , requestDeleteInAppTemplate $
--             newDeleteInAppTemplate
--
--         , requestUpdateInAppTemplate $
--             newUpdateInAppTemplate
--
--         , requestDeleteVoiceTemplate $
--             newDeleteVoiceTemplate
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
--         , requestGetImportJobs $
--             newGetImportJobs
--
--         , requestDeleteCampaign $
--             newDeleteCampaign
--
--         , requestUpdateVoiceTemplate $
--             newUpdateVoiceTemplate
--
--         , requestGetSegmentVersions $
--             newGetSegmentVersions
--
--         , requestGetApps $
--             newGetApps
--
--         , requestGetApnsSandboxChannel $
--             newGetApnsSandboxChannel
--
--         , requestGetImportJob $
--             newGetImportJob
--
--         , requestGetSegmentImportJobs $
--             newGetSegmentImportJobs
--
--         , requestCreateJourney $
--             newCreateJourney
--
--         , requestSendMessages $
--             newSendMessages
--
--         , requestPhoneNumberValidate $
--             newPhoneNumberValidate
--
--         , requestGetEmailChannel $
--             newGetEmailChannel
--
--         , requestGetApnsChannel $
--             newGetApnsChannel
--
--         , requestRemoveAttributes $
--             newRemoveAttributes
--
--         , requestCreateSmsTemplate $
--             newCreateSmsTemplate
--
--         , requestPutEventStream $
--             newPutEventStream
--
--         , requestDeleteApnsChannel $
--             newDeleteApnsChannel
--
--         , requestGetBaiduChannel $
--             newGetBaiduChannel
--
--         , requestGetJourneyExecutionActivityMetrics $
--             newGetJourneyExecutionActivityMetrics
--
--         , requestUpdateApnsChannel $
--             newUpdateApnsChannel
--
--         , requestGetChannels $
--             newGetChannels
--
--         , requestGetRecommenderConfigurations $
--             newGetRecommenderConfigurations
--
--         , requestUpdateApnsVoipSandboxChannel $
--             newUpdateApnsVoipSandboxChannel
--
--         , requestDeleteGcmChannel $
--             newDeleteGcmChannel
--
--         , requestUpdateGcmChannel $
--             newUpdateGcmChannel
--
--         , requestGetVoiceChannel $
--             newGetVoiceChannel
--
--         , requestGetJourneyExecutionMetrics $
--             newGetJourneyExecutionMetrics
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteApnsVoipSandboxChannel $
--             newDeleteApnsVoipSandboxChannel
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetSmsChannel $
--             newGetSmsChannel
--
--         , requestGetVoiceTemplate $
--             newGetVoiceTemplate
--
--         , requestGetInAppMessages $
--             newGetInAppMessages
--
--         , requestGetInAppTemplate $
--             newGetInAppTemplate
--
--         , requestGetEndpoint $
--             newGetEndpoint
--
--         , requestGetSegmentExportJobs $
--             newGetSegmentExportJobs
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
--         , requestUpdateSegment $
--             newUpdateSegment
--
--         , requestDeleteAdmChannel $
--             newDeleteAdmChannel
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestDeleteSegment $
--             newDeleteSegment
--
--         , requestDeleteRecommenderConfiguration $
--             newDeleteRecommenderConfiguration
--
--         , requestCreatePushTemplate $
--             newCreatePushTemplate
--
--         , requestUpdateEndpoint $
--             newUpdateEndpoint
--
--         , requestUpdateRecommenderConfiguration $
--             newUpdateRecommenderConfiguration'
--
--         , requestGetEmailTemplate $
--             newGetEmailTemplate
--
--         , requestCreateCampaign $
--             newCreateCampaign
--
--         , requestUpdateAdmChannel $
--             newUpdateAdmChannel
--
--         , requestGetApp $
--             newGetApp
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
--         , requestDeleteSmsChannel $
--             newDeleteSmsChannel
--
--         , requestGetUserEndpoints $
--             newGetUserEndpoints
--
--         , requestGetJourneyDateRangeKpi $
--             newGetJourneyDateRangeKpi
--
--         , requestCreateExportJob $
--             newCreateExportJob
--
--         , requestUpdateSmsTemplate $
--             newUpdateSmsTemplate
--
--         , requestDeleteVoiceChannel $
--             newDeleteVoiceChannel
--
--         , requestGetGcmChannel $
--             newGetGcmChannel
--
--         , requestUpdateVoiceChannel $
--             newUpdateVoiceChannel
--
--         , requestDeleteSmsTemplate $
--             newDeleteSmsTemplate
--
--         , requestGetApnsVoipSandboxChannel $
--             newGetApnsVoipSandboxChannel
--
--         , requestUpdateBaiduChannel $
--             newUpdateBaiduChannel
--
--         , requestDeleteEventStream $
--             newDeleteEventStream
--
--         , requestGetCampaignVersions $
--             newGetCampaignVersions
--
--         , requestDeleteBaiduChannel $
--             newDeleteBaiduChannel
--
--         , requestGetApplicationSettings $
--             newGetApplicationSettings
--
--         , requestGetCampaignDateRangeKpi $
--             newGetCampaignDateRangeKpi
--
--         , requestListJourneys $
--             newListJourneys
--
--         , requestGetApnsVoipChannel $
--             newGetApnsVoipChannel
--
--         , requestUpdateJourney $
--             newUpdateJourney
--
--         , requestListTemplates $
--             newListTemplates
--
--         , requestDeleteJourney $
--             newDeleteJourney
--
--         , requestUpdateEmailChannel $
--             newUpdateEmailChannel
--
--         , requestUpdateJourneyState $
--             newUpdateJourneyState
--
--         , requestCreateImportJob $
--             newCreateImportJob
--
--         , requestDeleteApnsVoipChannel $
--             newDeleteApnsVoipChannel
--
--         , requestGetJourney $
--             newGetJourney
--
--         , requestGetExportJobs $
--             newGetExportJobs
--
--         , requestPutEvents $
--             newPutEvents
--
--         , requestDeleteEmailChannel $
--             newDeleteEmailChannel
--
--         , requestUpdateApplicationSettings $
--             newUpdateApplicationSettings
--
--         , requestSendUsersMessages $
--             newSendUsersMessages
--
--         , requestGetSegments $
--             newGetSegments
--
--         , requestGetEventStream $
--             newGetEventStream
--
--         , requestUpdateApnsVoipChannel $
--             newUpdateApnsVoipChannel
--
--         , requestGetSmsTemplate $
--             newGetSmsTemplate
--
--         , requestDeleteApnsSandboxChannel $
--             newDeleteApnsSandboxChannel
--
--         , requestListTemplateVersions $
--             newListTemplateVersions
--
--         , requestGetCampaignActivities $
--             newGetCampaignActivities
--
--         , requestGetCampaigns $
--             newGetCampaigns
--
--         , requestUpdateApnsSandboxChannel $
--             newUpdateApnsSandboxChannel
--
--         , requestGetCampaign $
--             newGetCampaign
--
--         , requestDeleteUserEndpoints $
--             newDeleteUserEndpoints
--
--         , requestGetPushTemplate $
--             newGetPushTemplate
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestCreateEmailTemplate $
--             newCreateEmailTemplate
--
--         , requestGetSegment $
--             newGetSegment
--
--         , requestUpdateEndpointsBatch $
--             newUpdateEndpointsBatch
--
--         , requestCreateApp $
--             newCreateApp
--
--         , requestGetExportJob $
--             newGetExportJob
--
--         , requestDeleteEmailTemplate $
--             newDeleteEmailTemplate
--
--         , requestUpdateEmailTemplate $
--             newUpdateEmailTemplate
--
--         , requestCreateVoiceTemplate $
--             newCreateVoiceTemplate
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetCampaignVersion $
--             newGetCampaignVersion
--
--         , requestCreateInAppTemplate $
--             newCreateInAppTemplate
--
--           ]

--     , testGroup "response"
--         [ responseUpdateCampaign $
--             newUpdateCampaignResponse
--
--         , responseUpdatePushTemplate $
--             newUpdatePushTemplateResponse
--
--         , responseDeleteInAppTemplate $
--             newDeleteInAppTemplateResponse
--
--         , responseUpdateInAppTemplate $
--             newUpdateInAppTemplateResponse
--
--         , responseDeleteVoiceTemplate $
--             newDeleteVoiceTemplateResponse
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
--         , responseGetImportJobs $
--             newGetImportJobsResponse
--
--         , responseDeleteCampaign $
--             newDeleteCampaignResponse
--
--         , responseUpdateVoiceTemplate $
--             newUpdateVoiceTemplateResponse
--
--         , responseGetSegmentVersions $
--             newGetSegmentVersionsResponse
--
--         , responseGetApps $
--             newGetAppsResponse
--
--         , responseGetApnsSandboxChannel $
--             newGetApnsSandboxChannelResponse
--
--         , responseGetImportJob $
--             newGetImportJobResponse
--
--         , responseGetSegmentImportJobs $
--             newGetSegmentImportJobsResponse
--
--         , responseCreateJourney $
--             newCreateJourneyResponse
--
--         , responseSendMessages $
--             newSendMessagesResponse
--
--         , responsePhoneNumberValidate $
--             newPhoneNumberValidateResponse
--
--         , responseGetEmailChannel $
--             newGetEmailChannelResponse
--
--         , responseGetApnsChannel $
--             newGetApnsChannelResponse
--
--         , responseRemoveAttributes $
--             newRemoveAttributesResponse
--
--         , responseCreateSmsTemplate $
--             newCreateSmsTemplateResponse
--
--         , responsePutEventStream $
--             newPutEventStreamResponse
--
--         , responseDeleteApnsChannel $
--             newDeleteApnsChannelResponse
--
--         , responseGetBaiduChannel $
--             newGetBaiduChannelResponse
--
--         , responseGetJourneyExecutionActivityMetrics $
--             newGetJourneyExecutionActivityMetricsResponse
--
--         , responseUpdateApnsChannel $
--             newUpdateApnsChannelResponse
--
--         , responseGetChannels $
--             newGetChannelsResponse
--
--         , responseGetRecommenderConfigurations $
--             newGetRecommenderConfigurationsResponse
--
--         , responseUpdateApnsVoipSandboxChannel $
--             newUpdateApnsVoipSandboxChannelResponse
--
--         , responseDeleteGcmChannel $
--             newDeleteGcmChannelResponse
--
--         , responseUpdateGcmChannel $
--             newUpdateGcmChannelResponse
--
--         , responseGetVoiceChannel $
--             newGetVoiceChannelResponse
--
--         , responseGetJourneyExecutionMetrics $
--             newGetJourneyExecutionMetricsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteApnsVoipSandboxChannel $
--             newDeleteApnsVoipSandboxChannelResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetSmsChannel $
--             newGetSmsChannelResponse
--
--         , responseGetVoiceTemplate $
--             newGetVoiceTemplateResponse
--
--         , responseGetInAppMessages $
--             newGetInAppMessagesResponse
--
--         , responseGetInAppTemplate $
--             newGetInAppTemplateResponse
--
--         , responseGetEndpoint $
--             newGetEndpointResponse
--
--         , responseGetSegmentExportJobs $
--             newGetSegmentExportJobsResponse
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
--         , responseUpdateSegment $
--             newUpdateSegmentResponse
--
--         , responseDeleteAdmChannel $
--             newDeleteAdmChannelResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseDeleteSegment $
--             newDeleteSegmentResponse
--
--         , responseDeleteRecommenderConfiguration $
--             newDeleteRecommenderConfigurationResponse
--
--         , responseCreatePushTemplate $
--             newCreatePushTemplateResponse
--
--         , responseUpdateEndpoint $
--             newUpdateEndpointResponse
--
--         , responseUpdateRecommenderConfiguration $
--             newUpdateRecommenderConfigurationResponse
--
--         , responseGetEmailTemplate $
--             newGetEmailTemplateResponse
--
--         , responseCreateCampaign $
--             newCreateCampaignResponse
--
--         , responseUpdateAdmChannel $
--             newUpdateAdmChannelResponse
--
--         , responseGetApp $
--             newGetAppResponse
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
--         , responseDeleteSmsChannel $
--             newDeleteSmsChannelResponse
--
--         , responseGetUserEndpoints $
--             newGetUserEndpointsResponse
--
--         , responseGetJourneyDateRangeKpi $
--             newGetJourneyDateRangeKpiResponse
--
--         , responseCreateExportJob $
--             newCreateExportJobResponse
--
--         , responseUpdateSmsTemplate $
--             newUpdateSmsTemplateResponse
--
--         , responseDeleteVoiceChannel $
--             newDeleteVoiceChannelResponse
--
--         , responseGetGcmChannel $
--             newGetGcmChannelResponse
--
--         , responseUpdateVoiceChannel $
--             newUpdateVoiceChannelResponse
--
--         , responseDeleteSmsTemplate $
--             newDeleteSmsTemplateResponse
--
--         , responseGetApnsVoipSandboxChannel $
--             newGetApnsVoipSandboxChannelResponse
--
--         , responseUpdateBaiduChannel $
--             newUpdateBaiduChannelResponse
--
--         , responseDeleteEventStream $
--             newDeleteEventStreamResponse
--
--         , responseGetCampaignVersions $
--             newGetCampaignVersionsResponse
--
--         , responseDeleteBaiduChannel $
--             newDeleteBaiduChannelResponse
--
--         , responseGetApplicationSettings $
--             newGetApplicationSettingsResponse
--
--         , responseGetCampaignDateRangeKpi $
--             newGetCampaignDateRangeKpiResponse
--
--         , responseListJourneys $
--             newListJourneysResponse
--
--         , responseGetApnsVoipChannel $
--             newGetApnsVoipChannelResponse
--
--         , responseUpdateJourney $
--             newUpdateJourneyResponse
--
--         , responseListTemplates $
--             newListTemplatesResponse
--
--         , responseDeleteJourney $
--             newDeleteJourneyResponse
--
--         , responseUpdateEmailChannel $
--             newUpdateEmailChannelResponse
--
--         , responseUpdateJourneyState $
--             newUpdateJourneyStateResponse
--
--         , responseCreateImportJob $
--             newCreateImportJobResponse
--
--         , responseDeleteApnsVoipChannel $
--             newDeleteApnsVoipChannelResponse
--
--         , responseGetJourney $
--             newGetJourneyResponse
--
--         , responseGetExportJobs $
--             newGetExportJobsResponse
--
--         , responsePutEvents $
--             newPutEventsResponse
--
--         , responseDeleteEmailChannel $
--             newDeleteEmailChannelResponse
--
--         , responseUpdateApplicationSettings $
--             newUpdateApplicationSettingsResponse
--
--         , responseSendUsersMessages $
--             newSendUsersMessagesResponse
--
--         , responseGetSegments $
--             newGetSegmentsResponse
--
--         , responseGetEventStream $
--             newGetEventStreamResponse
--
--         , responseUpdateApnsVoipChannel $
--             newUpdateApnsVoipChannelResponse
--
--         , responseGetSmsTemplate $
--             newGetSmsTemplateResponse
--
--         , responseDeleteApnsSandboxChannel $
--             newDeleteApnsSandboxChannelResponse
--
--         , responseListTemplateVersions $
--             newListTemplateVersionsResponse
--
--         , responseGetCampaignActivities $
--             newGetCampaignActivitiesResponse
--
--         , responseGetCampaigns $
--             newGetCampaignsResponse
--
--         , responseUpdateApnsSandboxChannel $
--             newUpdateApnsSandboxChannelResponse
--
--         , responseGetCampaign $
--             newGetCampaignResponse
--
--         , responseDeleteUserEndpoints $
--             newDeleteUserEndpointsResponse
--
--         , responseGetPushTemplate $
--             newGetPushTemplateResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseCreateEmailTemplate $
--             newCreateEmailTemplateResponse
--
--         , responseGetSegment $
--             newGetSegmentResponse
--
--         , responseUpdateEndpointsBatch $
--             newUpdateEndpointsBatchResponse
--
--         , responseCreateApp $
--             newCreateAppResponse
--
--         , responseGetExportJob $
--             newGetExportJobResponse
--
--         , responseDeleteEmailTemplate $
--             newDeleteEmailTemplateResponse
--
--         , responseUpdateEmailTemplate $
--             newUpdateEmailTemplateResponse
--
--         , responseCreateVoiceTemplate $
--             newCreateVoiceTemplateResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetCampaignVersion $
--             newGetCampaignVersionResponse
--
--         , responseCreateInAppTemplate $
--             newCreateInAppTemplateResponse
--
--           ]
--     ]

-- Requests

requestUpdateCampaign :: UpdateCampaign -> TestTree
requestUpdateCampaign =
  req
    "UpdateCampaign"
    "fixture/UpdateCampaign.yaml"

requestUpdatePushTemplate :: UpdatePushTemplate -> TestTree
requestUpdatePushTemplate =
  req
    "UpdatePushTemplate"
    "fixture/UpdatePushTemplate.yaml"

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

requestDeleteVoiceTemplate :: DeleteVoiceTemplate -> TestTree
requestDeleteVoiceTemplate =
  req
    "DeleteVoiceTemplate"
    "fixture/DeleteVoiceTemplate.yaml"

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

requestGetImportJobs :: GetImportJobs -> TestTree
requestGetImportJobs =
  req
    "GetImportJobs"
    "fixture/GetImportJobs.yaml"

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

requestGetApnsSandboxChannel :: GetApnsSandboxChannel -> TestTree
requestGetApnsSandboxChannel =
  req
    "GetApnsSandboxChannel"
    "fixture/GetApnsSandboxChannel.yaml"

requestGetImportJob :: GetImportJob -> TestTree
requestGetImportJob =
  req
    "GetImportJob"
    "fixture/GetImportJob.yaml"

requestGetSegmentImportJobs :: GetSegmentImportJobs -> TestTree
requestGetSegmentImportJobs =
  req
    "GetSegmentImportJobs"
    "fixture/GetSegmentImportJobs.yaml"

requestCreateJourney :: CreateJourney -> TestTree
requestCreateJourney =
  req
    "CreateJourney"
    "fixture/CreateJourney.yaml"

requestSendMessages :: SendMessages -> TestTree
requestSendMessages =
  req
    "SendMessages"
    "fixture/SendMessages.yaml"

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

requestGetApnsChannel :: GetApnsChannel -> TestTree
requestGetApnsChannel =
  req
    "GetApnsChannel"
    "fixture/GetApnsChannel.yaml"

requestRemoveAttributes :: RemoveAttributes -> TestTree
requestRemoveAttributes =
  req
    "RemoveAttributes"
    "fixture/RemoveAttributes.yaml"

requestCreateSmsTemplate :: CreateSmsTemplate -> TestTree
requestCreateSmsTemplate =
  req
    "CreateSmsTemplate"
    "fixture/CreateSmsTemplate.yaml"

requestPutEventStream :: PutEventStream -> TestTree
requestPutEventStream =
  req
    "PutEventStream"
    "fixture/PutEventStream.yaml"

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

requestUpdateApnsVoipSandboxChannel :: UpdateApnsVoipSandboxChannel -> TestTree
requestUpdateApnsVoipSandboxChannel =
  req
    "UpdateApnsVoipSandboxChannel"
    "fixture/UpdateApnsVoipSandboxChannel.yaml"

requestDeleteGcmChannel :: DeleteGcmChannel -> TestTree
requestDeleteGcmChannel =
  req
    "DeleteGcmChannel"
    "fixture/DeleteGcmChannel.yaml"

requestUpdateGcmChannel :: UpdateGcmChannel -> TestTree
requestUpdateGcmChannel =
  req
    "UpdateGcmChannel"
    "fixture/UpdateGcmChannel.yaml"

requestGetVoiceChannel :: GetVoiceChannel -> TestTree
requestGetVoiceChannel =
  req
    "GetVoiceChannel"
    "fixture/GetVoiceChannel.yaml"

requestGetJourneyExecutionMetrics :: GetJourneyExecutionMetrics -> TestTree
requestGetJourneyExecutionMetrics =
  req
    "GetJourneyExecutionMetrics"
    "fixture/GetJourneyExecutionMetrics.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteApnsVoipSandboxChannel :: DeleteApnsVoipSandboxChannel -> TestTree
requestDeleteApnsVoipSandboxChannel =
  req
    "DeleteApnsVoipSandboxChannel"
    "fixture/DeleteApnsVoipSandboxChannel.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetSmsChannel :: GetSmsChannel -> TestTree
requestGetSmsChannel =
  req
    "GetSmsChannel"
    "fixture/GetSmsChannel.yaml"

requestGetVoiceTemplate :: GetVoiceTemplate -> TestTree
requestGetVoiceTemplate =
  req
    "GetVoiceTemplate"
    "fixture/GetVoiceTemplate.yaml"

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

requestGetEndpoint :: GetEndpoint -> TestTree
requestGetEndpoint =
  req
    "GetEndpoint"
    "fixture/GetEndpoint.yaml"

requestGetSegmentExportJobs :: GetSegmentExportJobs -> TestTree
requestGetSegmentExportJobs =
  req
    "GetSegmentExportJobs"
    "fixture/GetSegmentExportJobs.yaml"

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

requestUpdateSegment :: UpdateSegment -> TestTree
requestUpdateSegment =
  req
    "UpdateSegment"
    "fixture/UpdateSegment.yaml"

requestDeleteAdmChannel :: DeleteAdmChannel -> TestTree
requestDeleteAdmChannel =
  req
    "DeleteAdmChannel"
    "fixture/DeleteAdmChannel.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint =
  req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestDeleteSegment :: DeleteSegment -> TestTree
requestDeleteSegment =
  req
    "DeleteSegment"
    "fixture/DeleteSegment.yaml"

requestDeleteRecommenderConfiguration :: DeleteRecommenderConfiguration -> TestTree
requestDeleteRecommenderConfiguration =
  req
    "DeleteRecommenderConfiguration"
    "fixture/DeleteRecommenderConfiguration.yaml"

requestCreatePushTemplate :: CreatePushTemplate -> TestTree
requestCreatePushTemplate =
  req
    "CreatePushTemplate"
    "fixture/CreatePushTemplate.yaml"

requestUpdateEndpoint :: UpdateEndpoint -> TestTree
requestUpdateEndpoint =
  req
    "UpdateEndpoint"
    "fixture/UpdateEndpoint.yaml"

requestUpdateRecommenderConfiguration :: UpdateRecommenderConfiguration' -> TestTree
requestUpdateRecommenderConfiguration =
  req
    "UpdateRecommenderConfiguration"
    "fixture/UpdateRecommenderConfiguration.yaml"

requestGetEmailTemplate :: GetEmailTemplate -> TestTree
requestGetEmailTemplate =
  req
    "GetEmailTemplate"
    "fixture/GetEmailTemplate.yaml"

requestCreateCampaign :: CreateCampaign -> TestTree
requestCreateCampaign =
  req
    "CreateCampaign"
    "fixture/CreateCampaign.yaml"

requestUpdateAdmChannel :: UpdateAdmChannel -> TestTree
requestUpdateAdmChannel =
  req
    "UpdateAdmChannel"
    "fixture/UpdateAdmChannel.yaml"

requestGetApp :: GetApp -> TestTree
requestGetApp =
  req
    "GetApp"
    "fixture/GetApp.yaml"

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

requestDeleteSmsChannel :: DeleteSmsChannel -> TestTree
requestDeleteSmsChannel =
  req
    "DeleteSmsChannel"
    "fixture/DeleteSmsChannel.yaml"

requestGetUserEndpoints :: GetUserEndpoints -> TestTree
requestGetUserEndpoints =
  req
    "GetUserEndpoints"
    "fixture/GetUserEndpoints.yaml"

requestGetJourneyDateRangeKpi :: GetJourneyDateRangeKpi -> TestTree
requestGetJourneyDateRangeKpi =
  req
    "GetJourneyDateRangeKpi"
    "fixture/GetJourneyDateRangeKpi.yaml"

requestCreateExportJob :: CreateExportJob -> TestTree
requestCreateExportJob =
  req
    "CreateExportJob"
    "fixture/CreateExportJob.yaml"

requestUpdateSmsTemplate :: UpdateSmsTemplate -> TestTree
requestUpdateSmsTemplate =
  req
    "UpdateSmsTemplate"
    "fixture/UpdateSmsTemplate.yaml"

requestDeleteVoiceChannel :: DeleteVoiceChannel -> TestTree
requestDeleteVoiceChannel =
  req
    "DeleteVoiceChannel"
    "fixture/DeleteVoiceChannel.yaml"

requestGetGcmChannel :: GetGcmChannel -> TestTree
requestGetGcmChannel =
  req
    "GetGcmChannel"
    "fixture/GetGcmChannel.yaml"

requestUpdateVoiceChannel :: UpdateVoiceChannel -> TestTree
requestUpdateVoiceChannel =
  req
    "UpdateVoiceChannel"
    "fixture/UpdateVoiceChannel.yaml"

requestDeleteSmsTemplate :: DeleteSmsTemplate -> TestTree
requestDeleteSmsTemplate =
  req
    "DeleteSmsTemplate"
    "fixture/DeleteSmsTemplate.yaml"

requestGetApnsVoipSandboxChannel :: GetApnsVoipSandboxChannel -> TestTree
requestGetApnsVoipSandboxChannel =
  req
    "GetApnsVoipSandboxChannel"
    "fixture/GetApnsVoipSandboxChannel.yaml"

requestUpdateBaiduChannel :: UpdateBaiduChannel -> TestTree
requestUpdateBaiduChannel =
  req
    "UpdateBaiduChannel"
    "fixture/UpdateBaiduChannel.yaml"

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

requestDeleteBaiduChannel :: DeleteBaiduChannel -> TestTree
requestDeleteBaiduChannel =
  req
    "DeleteBaiduChannel"
    "fixture/DeleteBaiduChannel.yaml"

requestGetApplicationSettings :: GetApplicationSettings -> TestTree
requestGetApplicationSettings =
  req
    "GetApplicationSettings"
    "fixture/GetApplicationSettings.yaml"

requestGetCampaignDateRangeKpi :: GetCampaignDateRangeKpi -> TestTree
requestGetCampaignDateRangeKpi =
  req
    "GetCampaignDateRangeKpi"
    "fixture/GetCampaignDateRangeKpi.yaml"

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

requestDeleteJourney :: DeleteJourney -> TestTree
requestDeleteJourney =
  req
    "DeleteJourney"
    "fixture/DeleteJourney.yaml"

requestUpdateEmailChannel :: UpdateEmailChannel -> TestTree
requestUpdateEmailChannel =
  req
    "UpdateEmailChannel"
    "fixture/UpdateEmailChannel.yaml"

requestUpdateJourneyState :: UpdateJourneyState -> TestTree
requestUpdateJourneyState =
  req
    "UpdateJourneyState"
    "fixture/UpdateJourneyState.yaml"

requestCreateImportJob :: CreateImportJob -> TestTree
requestCreateImportJob =
  req
    "CreateImportJob"
    "fixture/CreateImportJob.yaml"

requestDeleteApnsVoipChannel :: DeleteApnsVoipChannel -> TestTree
requestDeleteApnsVoipChannel =
  req
    "DeleteApnsVoipChannel"
    "fixture/DeleteApnsVoipChannel.yaml"

requestGetJourney :: GetJourney -> TestTree
requestGetJourney =
  req
    "GetJourney"
    "fixture/GetJourney.yaml"

requestGetExportJobs :: GetExportJobs -> TestTree
requestGetExportJobs =
  req
    "GetExportJobs"
    "fixture/GetExportJobs.yaml"

requestPutEvents :: PutEvents -> TestTree
requestPutEvents =
  req
    "PutEvents"
    "fixture/PutEvents.yaml"

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

requestSendUsersMessages :: SendUsersMessages -> TestTree
requestSendUsersMessages =
  req
    "SendUsersMessages"
    "fixture/SendUsersMessages.yaml"

requestGetSegments :: GetSegments -> TestTree
requestGetSegments =
  req
    "GetSegments"
    "fixture/GetSegments.yaml"

requestGetEventStream :: GetEventStream -> TestTree
requestGetEventStream =
  req
    "GetEventStream"
    "fixture/GetEventStream.yaml"

requestUpdateApnsVoipChannel :: UpdateApnsVoipChannel -> TestTree
requestUpdateApnsVoipChannel =
  req
    "UpdateApnsVoipChannel"
    "fixture/UpdateApnsVoipChannel.yaml"

requestGetSmsTemplate :: GetSmsTemplate -> TestTree
requestGetSmsTemplate =
  req
    "GetSmsTemplate"
    "fixture/GetSmsTemplate.yaml"

requestDeleteApnsSandboxChannel :: DeleteApnsSandboxChannel -> TestTree
requestDeleteApnsSandboxChannel =
  req
    "DeleteApnsSandboxChannel"
    "fixture/DeleteApnsSandboxChannel.yaml"

requestListTemplateVersions :: ListTemplateVersions -> TestTree
requestListTemplateVersions =
  req
    "ListTemplateVersions"
    "fixture/ListTemplateVersions.yaml"

requestGetCampaignActivities :: GetCampaignActivities -> TestTree
requestGetCampaignActivities =
  req
    "GetCampaignActivities"
    "fixture/GetCampaignActivities.yaml"

requestGetCampaigns :: GetCampaigns -> TestTree
requestGetCampaigns =
  req
    "GetCampaigns"
    "fixture/GetCampaigns.yaml"

requestUpdateApnsSandboxChannel :: UpdateApnsSandboxChannel -> TestTree
requestUpdateApnsSandboxChannel =
  req
    "UpdateApnsSandboxChannel"
    "fixture/UpdateApnsSandboxChannel.yaml"

requestGetCampaign :: GetCampaign -> TestTree
requestGetCampaign =
  req
    "GetCampaign"
    "fixture/GetCampaign.yaml"

requestDeleteUserEndpoints :: DeleteUserEndpoints -> TestTree
requestDeleteUserEndpoints =
  req
    "DeleteUserEndpoints"
    "fixture/DeleteUserEndpoints.yaml"

requestGetPushTemplate :: GetPushTemplate -> TestTree
requestGetPushTemplate =
  req
    "GetPushTemplate"
    "fixture/GetPushTemplate.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestCreateEmailTemplate :: CreateEmailTemplate -> TestTree
requestCreateEmailTemplate =
  req
    "CreateEmailTemplate"
    "fixture/CreateEmailTemplate.yaml"

requestGetSegment :: GetSegment -> TestTree
requestGetSegment =
  req
    "GetSegment"
    "fixture/GetSegment.yaml"

requestUpdateEndpointsBatch :: UpdateEndpointsBatch -> TestTree
requestUpdateEndpointsBatch =
  req
    "UpdateEndpointsBatch"
    "fixture/UpdateEndpointsBatch.yaml"

requestCreateApp :: CreateApp -> TestTree
requestCreateApp =
  req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestGetExportJob :: GetExportJob -> TestTree
requestGetExportJob =
  req
    "GetExportJob"
    "fixture/GetExportJob.yaml"

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

requestCreateVoiceTemplate :: CreateVoiceTemplate -> TestTree
requestCreateVoiceTemplate =
  req
    "CreateVoiceTemplate"
    "fixture/CreateVoiceTemplate.yaml"

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

requestCreateInAppTemplate :: CreateInAppTemplate -> TestTree
requestCreateInAppTemplate =
  req
    "CreateInAppTemplate"
    "fixture/CreateInAppTemplate.yaml"

-- Responses

responseUpdateCampaign :: UpdateCampaignResponse -> TestTree
responseUpdateCampaign =
  res
    "UpdateCampaignResponse"
    "fixture/UpdateCampaignResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCampaign)

responseUpdatePushTemplate :: UpdatePushTemplateResponse -> TestTree
responseUpdatePushTemplate =
  res
    "UpdatePushTemplateResponse"
    "fixture/UpdatePushTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePushTemplate)

responseDeleteInAppTemplate :: DeleteInAppTemplateResponse -> TestTree
responseDeleteInAppTemplate =
  res
    "DeleteInAppTemplateResponse"
    "fixture/DeleteInAppTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInAppTemplate)

responseUpdateInAppTemplate :: UpdateInAppTemplateResponse -> TestTree
responseUpdateInAppTemplate =
  res
    "UpdateInAppTemplateResponse"
    "fixture/UpdateInAppTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateInAppTemplate)

responseDeleteVoiceTemplate :: DeleteVoiceTemplateResponse -> TestTree
responseDeleteVoiceTemplate =
  res
    "DeleteVoiceTemplateResponse"
    "fixture/DeleteVoiceTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVoiceTemplate)

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

responseGetImportJobs :: GetImportJobsResponse -> TestTree
responseGetImportJobs =
  res
    "GetImportJobsResponse"
    "fixture/GetImportJobsResponse.proto"
    defaultService
    (Proxy :: Proxy GetImportJobs)

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

responseGetApnsSandboxChannel :: GetApnsSandboxChannelResponse -> TestTree
responseGetApnsSandboxChannel =
  res
    "GetApnsSandboxChannelResponse"
    "fixture/GetApnsSandboxChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetApnsSandboxChannel)

responseGetImportJob :: GetImportJobResponse -> TestTree
responseGetImportJob =
  res
    "GetImportJobResponse"
    "fixture/GetImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy GetImportJob)

responseGetSegmentImportJobs :: GetSegmentImportJobsResponse -> TestTree
responseGetSegmentImportJobs =
  res
    "GetSegmentImportJobsResponse"
    "fixture/GetSegmentImportJobsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSegmentImportJobs)

responseCreateJourney :: CreateJourneyResponse -> TestTree
responseCreateJourney =
  res
    "CreateJourneyResponse"
    "fixture/CreateJourneyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateJourney)

responseSendMessages :: SendMessagesResponse -> TestTree
responseSendMessages =
  res
    "SendMessagesResponse"
    "fixture/SendMessagesResponse.proto"
    defaultService
    (Proxy :: Proxy SendMessages)

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

responseGetApnsChannel :: GetApnsChannelResponse -> TestTree
responseGetApnsChannel =
  res
    "GetApnsChannelResponse"
    "fixture/GetApnsChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetApnsChannel)

responseRemoveAttributes :: RemoveAttributesResponse -> TestTree
responseRemoveAttributes =
  res
    "RemoveAttributesResponse"
    "fixture/RemoveAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveAttributes)

responseCreateSmsTemplate :: CreateSmsTemplateResponse -> TestTree
responseCreateSmsTemplate =
  res
    "CreateSmsTemplateResponse"
    "fixture/CreateSmsTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSmsTemplate)

responsePutEventStream :: PutEventStreamResponse -> TestTree
responsePutEventStream =
  res
    "PutEventStreamResponse"
    "fixture/PutEventStreamResponse.proto"
    defaultService
    (Proxy :: Proxy PutEventStream)

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

responseUpdateApnsVoipSandboxChannel :: UpdateApnsVoipSandboxChannelResponse -> TestTree
responseUpdateApnsVoipSandboxChannel =
  res
    "UpdateApnsVoipSandboxChannelResponse"
    "fixture/UpdateApnsVoipSandboxChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApnsVoipSandboxChannel)

responseDeleteGcmChannel :: DeleteGcmChannelResponse -> TestTree
responseDeleteGcmChannel =
  res
    "DeleteGcmChannelResponse"
    "fixture/DeleteGcmChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGcmChannel)

responseUpdateGcmChannel :: UpdateGcmChannelResponse -> TestTree
responseUpdateGcmChannel =
  res
    "UpdateGcmChannelResponse"
    "fixture/UpdateGcmChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGcmChannel)

responseGetVoiceChannel :: GetVoiceChannelResponse -> TestTree
responseGetVoiceChannel =
  res
    "GetVoiceChannelResponse"
    "fixture/GetVoiceChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetVoiceChannel)

responseGetJourneyExecutionMetrics :: GetJourneyExecutionMetricsResponse -> TestTree
responseGetJourneyExecutionMetrics =
  res
    "GetJourneyExecutionMetricsResponse"
    "fixture/GetJourneyExecutionMetricsResponse.proto"
    defaultService
    (Proxy :: Proxy GetJourneyExecutionMetrics)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDeleteApnsVoipSandboxChannel :: DeleteApnsVoipSandboxChannelResponse -> TestTree
responseDeleteApnsVoipSandboxChannel =
  res
    "DeleteApnsVoipSandboxChannelResponse"
    "fixture/DeleteApnsVoipSandboxChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApnsVoipSandboxChannel)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseGetSmsChannel :: GetSmsChannelResponse -> TestTree
responseGetSmsChannel =
  res
    "GetSmsChannelResponse"
    "fixture/GetSmsChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetSmsChannel)

responseGetVoiceTemplate :: GetVoiceTemplateResponse -> TestTree
responseGetVoiceTemplate =
  res
    "GetVoiceTemplateResponse"
    "fixture/GetVoiceTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetVoiceTemplate)

responseGetInAppMessages :: GetInAppMessagesResponse -> TestTree
responseGetInAppMessages =
  res
    "GetInAppMessagesResponse"
    "fixture/GetInAppMessagesResponse.proto"
    defaultService
    (Proxy :: Proxy GetInAppMessages)

responseGetInAppTemplate :: GetInAppTemplateResponse -> TestTree
responseGetInAppTemplate =
  res
    "GetInAppTemplateResponse"
    "fixture/GetInAppTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetInAppTemplate)

responseGetEndpoint :: GetEndpointResponse -> TestTree
responseGetEndpoint =
  res
    "GetEndpointResponse"
    "fixture/GetEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy GetEndpoint)

responseGetSegmentExportJobs :: GetSegmentExportJobsResponse -> TestTree
responseGetSegmentExportJobs =
  res
    "GetSegmentExportJobsResponse"
    "fixture/GetSegmentExportJobsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSegmentExportJobs)

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

responseUpdateSegment :: UpdateSegmentResponse -> TestTree
responseUpdateSegment =
  res
    "UpdateSegmentResponse"
    "fixture/UpdateSegmentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSegment)

responseDeleteAdmChannel :: DeleteAdmChannelResponse -> TestTree
responseDeleteAdmChannel =
  res
    "DeleteAdmChannelResponse"
    "fixture/DeleteAdmChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAdmChannel)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEndpoint)

responseDeleteSegment :: DeleteSegmentResponse -> TestTree
responseDeleteSegment =
  res
    "DeleteSegmentResponse"
    "fixture/DeleteSegmentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSegment)

responseDeleteRecommenderConfiguration :: DeleteRecommenderConfigurationResponse -> TestTree
responseDeleteRecommenderConfiguration =
  res
    "DeleteRecommenderConfigurationResponse"
    "fixture/DeleteRecommenderConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRecommenderConfiguration)

responseCreatePushTemplate :: CreatePushTemplateResponse -> TestTree
responseCreatePushTemplate =
  res
    "CreatePushTemplateResponse"
    "fixture/CreatePushTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePushTemplate)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEndpoint)

responseUpdateRecommenderConfiguration :: UpdateRecommenderConfigurationResponse -> TestTree
responseUpdateRecommenderConfiguration =
  res
    "UpdateRecommenderConfigurationResponse"
    "fixture/UpdateRecommenderConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRecommenderConfiguration')

responseGetEmailTemplate :: GetEmailTemplateResponse -> TestTree
responseGetEmailTemplate =
  res
    "GetEmailTemplateResponse"
    "fixture/GetEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetEmailTemplate)

responseCreateCampaign :: CreateCampaignResponse -> TestTree
responseCreateCampaign =
  res
    "CreateCampaignResponse"
    "fixture/CreateCampaignResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCampaign)

responseUpdateAdmChannel :: UpdateAdmChannelResponse -> TestTree
responseUpdateAdmChannel =
  res
    "UpdateAdmChannelResponse"
    "fixture/UpdateAdmChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAdmChannel)

responseGetApp :: GetAppResponse -> TestTree
responseGetApp =
  res
    "GetAppResponse"
    "fixture/GetAppResponse.proto"
    defaultService
    (Proxy :: Proxy GetApp)

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

responseDeleteSmsChannel :: DeleteSmsChannelResponse -> TestTree
responseDeleteSmsChannel =
  res
    "DeleteSmsChannelResponse"
    "fixture/DeleteSmsChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSmsChannel)

responseGetUserEndpoints :: GetUserEndpointsResponse -> TestTree
responseGetUserEndpoints =
  res
    "GetUserEndpointsResponse"
    "fixture/GetUserEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy GetUserEndpoints)

responseGetJourneyDateRangeKpi :: GetJourneyDateRangeKpiResponse -> TestTree
responseGetJourneyDateRangeKpi =
  res
    "GetJourneyDateRangeKpiResponse"
    "fixture/GetJourneyDateRangeKpiResponse.proto"
    defaultService
    (Proxy :: Proxy GetJourneyDateRangeKpi)

responseCreateExportJob :: CreateExportJobResponse -> TestTree
responseCreateExportJob =
  res
    "CreateExportJobResponse"
    "fixture/CreateExportJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateExportJob)

responseUpdateSmsTemplate :: UpdateSmsTemplateResponse -> TestTree
responseUpdateSmsTemplate =
  res
    "UpdateSmsTemplateResponse"
    "fixture/UpdateSmsTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSmsTemplate)

responseDeleteVoiceChannel :: DeleteVoiceChannelResponse -> TestTree
responseDeleteVoiceChannel =
  res
    "DeleteVoiceChannelResponse"
    "fixture/DeleteVoiceChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVoiceChannel)

responseGetGcmChannel :: GetGcmChannelResponse -> TestTree
responseGetGcmChannel =
  res
    "GetGcmChannelResponse"
    "fixture/GetGcmChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetGcmChannel)

responseUpdateVoiceChannel :: UpdateVoiceChannelResponse -> TestTree
responseUpdateVoiceChannel =
  res
    "UpdateVoiceChannelResponse"
    "fixture/UpdateVoiceChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateVoiceChannel)

responseDeleteSmsTemplate :: DeleteSmsTemplateResponse -> TestTree
responseDeleteSmsTemplate =
  res
    "DeleteSmsTemplateResponse"
    "fixture/DeleteSmsTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSmsTemplate)

responseGetApnsVoipSandboxChannel :: GetApnsVoipSandboxChannelResponse -> TestTree
responseGetApnsVoipSandboxChannel =
  res
    "GetApnsVoipSandboxChannelResponse"
    "fixture/GetApnsVoipSandboxChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetApnsVoipSandboxChannel)

responseUpdateBaiduChannel :: UpdateBaiduChannelResponse -> TestTree
responseUpdateBaiduChannel =
  res
    "UpdateBaiduChannelResponse"
    "fixture/UpdateBaiduChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBaiduChannel)

responseDeleteEventStream :: DeleteEventStreamResponse -> TestTree
responseDeleteEventStream =
  res
    "DeleteEventStreamResponse"
    "fixture/DeleteEventStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEventStream)

responseGetCampaignVersions :: GetCampaignVersionsResponse -> TestTree
responseGetCampaignVersions =
  res
    "GetCampaignVersionsResponse"
    "fixture/GetCampaignVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetCampaignVersions)

responseDeleteBaiduChannel :: DeleteBaiduChannelResponse -> TestTree
responseDeleteBaiduChannel =
  res
    "DeleteBaiduChannelResponse"
    "fixture/DeleteBaiduChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBaiduChannel)

responseGetApplicationSettings :: GetApplicationSettingsResponse -> TestTree
responseGetApplicationSettings =
  res
    "GetApplicationSettingsResponse"
    "fixture/GetApplicationSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy GetApplicationSettings)

responseGetCampaignDateRangeKpi :: GetCampaignDateRangeKpiResponse -> TestTree
responseGetCampaignDateRangeKpi =
  res
    "GetCampaignDateRangeKpiResponse"
    "fixture/GetCampaignDateRangeKpiResponse.proto"
    defaultService
    (Proxy :: Proxy GetCampaignDateRangeKpi)

responseListJourneys :: ListJourneysResponse -> TestTree
responseListJourneys =
  res
    "ListJourneysResponse"
    "fixture/ListJourneysResponse.proto"
    defaultService
    (Proxy :: Proxy ListJourneys)

responseGetApnsVoipChannel :: GetApnsVoipChannelResponse -> TestTree
responseGetApnsVoipChannel =
  res
    "GetApnsVoipChannelResponse"
    "fixture/GetApnsVoipChannelResponse.proto"
    defaultService
    (Proxy :: Proxy GetApnsVoipChannel)

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

responseDeleteJourney :: DeleteJourneyResponse -> TestTree
responseDeleteJourney =
  res
    "DeleteJourneyResponse"
    "fixture/DeleteJourneyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteJourney)

responseUpdateEmailChannel :: UpdateEmailChannelResponse -> TestTree
responseUpdateEmailChannel =
  res
    "UpdateEmailChannelResponse"
    "fixture/UpdateEmailChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEmailChannel)

responseUpdateJourneyState :: UpdateJourneyStateResponse -> TestTree
responseUpdateJourneyState =
  res
    "UpdateJourneyStateResponse"
    "fixture/UpdateJourneyStateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateJourneyState)

responseCreateImportJob :: CreateImportJobResponse -> TestTree
responseCreateImportJob =
  res
    "CreateImportJobResponse"
    "fixture/CreateImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImportJob)

responseDeleteApnsVoipChannel :: DeleteApnsVoipChannelResponse -> TestTree
responseDeleteApnsVoipChannel =
  res
    "DeleteApnsVoipChannelResponse"
    "fixture/DeleteApnsVoipChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApnsVoipChannel)

responseGetJourney :: GetJourneyResponse -> TestTree
responseGetJourney =
  res
    "GetJourneyResponse"
    "fixture/GetJourneyResponse.proto"
    defaultService
    (Proxy :: Proxy GetJourney)

responseGetExportJobs :: GetExportJobsResponse -> TestTree
responseGetExportJobs =
  res
    "GetExportJobsResponse"
    "fixture/GetExportJobsResponse.proto"
    defaultService
    (Proxy :: Proxy GetExportJobs)

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents =
  res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    defaultService
    (Proxy :: Proxy PutEvents)

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

responseSendUsersMessages :: SendUsersMessagesResponse -> TestTree
responseSendUsersMessages =
  res
    "SendUsersMessagesResponse"
    "fixture/SendUsersMessagesResponse.proto"
    defaultService
    (Proxy :: Proxy SendUsersMessages)

responseGetSegments :: GetSegmentsResponse -> TestTree
responseGetSegments =
  res
    "GetSegmentsResponse"
    "fixture/GetSegmentsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSegments)

responseGetEventStream :: GetEventStreamResponse -> TestTree
responseGetEventStream =
  res
    "GetEventStreamResponse"
    "fixture/GetEventStreamResponse.proto"
    defaultService
    (Proxy :: Proxy GetEventStream)

responseUpdateApnsVoipChannel :: UpdateApnsVoipChannelResponse -> TestTree
responseUpdateApnsVoipChannel =
  res
    "UpdateApnsVoipChannelResponse"
    "fixture/UpdateApnsVoipChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApnsVoipChannel)

responseGetSmsTemplate :: GetSmsTemplateResponse -> TestTree
responseGetSmsTemplate =
  res
    "GetSmsTemplateResponse"
    "fixture/GetSmsTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetSmsTemplate)

responseDeleteApnsSandboxChannel :: DeleteApnsSandboxChannelResponse -> TestTree
responseDeleteApnsSandboxChannel =
  res
    "DeleteApnsSandboxChannelResponse"
    "fixture/DeleteApnsSandboxChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApnsSandboxChannel)

responseListTemplateVersions :: ListTemplateVersionsResponse -> TestTree
responseListTemplateVersions =
  res
    "ListTemplateVersionsResponse"
    "fixture/ListTemplateVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTemplateVersions)

responseGetCampaignActivities :: GetCampaignActivitiesResponse -> TestTree
responseGetCampaignActivities =
  res
    "GetCampaignActivitiesResponse"
    "fixture/GetCampaignActivitiesResponse.proto"
    defaultService
    (Proxy :: Proxy GetCampaignActivities)

responseGetCampaigns :: GetCampaignsResponse -> TestTree
responseGetCampaigns =
  res
    "GetCampaignsResponse"
    "fixture/GetCampaignsResponse.proto"
    defaultService
    (Proxy :: Proxy GetCampaigns)

responseUpdateApnsSandboxChannel :: UpdateApnsSandboxChannelResponse -> TestTree
responseUpdateApnsSandboxChannel =
  res
    "UpdateApnsSandboxChannelResponse"
    "fixture/UpdateApnsSandboxChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApnsSandboxChannel)

responseGetCampaign :: GetCampaignResponse -> TestTree
responseGetCampaign =
  res
    "GetCampaignResponse"
    "fixture/GetCampaignResponse.proto"
    defaultService
    (Proxy :: Proxy GetCampaign)

responseDeleteUserEndpoints :: DeleteUserEndpointsResponse -> TestTree
responseDeleteUserEndpoints =
  res
    "DeleteUserEndpointsResponse"
    "fixture/DeleteUserEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserEndpoints)

responseGetPushTemplate :: GetPushTemplateResponse -> TestTree
responseGetPushTemplate =
  res
    "GetPushTemplateResponse"
    "fixture/GetPushTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetPushTemplate)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApp)

responseCreateEmailTemplate :: CreateEmailTemplateResponse -> TestTree
responseCreateEmailTemplate =
  res
    "CreateEmailTemplateResponse"
    "fixture/CreateEmailTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEmailTemplate)

responseGetSegment :: GetSegmentResponse -> TestTree
responseGetSegment =
  res
    "GetSegmentResponse"
    "fixture/GetSegmentResponse.proto"
    defaultService
    (Proxy :: Proxy GetSegment)

responseUpdateEndpointsBatch :: UpdateEndpointsBatchResponse -> TestTree
responseUpdateEndpointsBatch =
  res
    "UpdateEndpointsBatchResponse"
    "fixture/UpdateEndpointsBatchResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEndpointsBatch)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApp)

responseGetExportJob :: GetExportJobResponse -> TestTree
responseGetExportJob =
  res
    "GetExportJobResponse"
    "fixture/GetExportJobResponse.proto"
    defaultService
    (Proxy :: Proxy GetExportJob)

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

responseCreateVoiceTemplate :: CreateVoiceTemplateResponse -> TestTree
responseCreateVoiceTemplate =
  res
    "CreateVoiceTemplateResponse"
    "fixture/CreateVoiceTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVoiceTemplate)

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

responseCreateInAppTemplate :: CreateInAppTemplateResponse -> TestTree
responseCreateInAppTemplate =
  res
    "CreateInAppTemplateResponse"
    "fixture/CreateInAppTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInAppTemplate)
