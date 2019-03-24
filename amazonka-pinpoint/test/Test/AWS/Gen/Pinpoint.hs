{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Pinpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         , requestCreateExportJob $
--             createExportJob
--
--         , requestCreateSegment $
--             createSegment
--
--         , requestUpdateADMChannel $
--             updateADMChannel
--
--         , requestDeleteADMChannel $
--             deleteADMChannel
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
--         , requestGetExportJob $
--             getExportJob
--
--         , requestGetEndpoint $
--             getEndpoint
--
--         , requestGetSegment $
--             getSegment
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
--         , requestDeleteUserEndpoints $
--             deleteUserEndpoints
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
--         , requestGetEventStream $
--             getEventStream
--
--         , requestGetChannels $
--             getChannels
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
--         , requestPutEventStream $
--             putEventStream
--
--         , requestDeleteEventStream $
--             deleteEventStream
--
--         , requestGetCampaignVersions $
--             getCampaignVersions
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
--         , requestGetUserEndpoints $
--             getUserEndpoints
--
--         , requestGetImportJobs $
--             getImportJobs
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
--         , requestCreateApp $
--             createApp
--
--         , requestGetSegmentExportJobs $
--             getSegmentExportJobs
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
--         , requestPutEvents $
--             putEvents
--
--         , requestUpdateApplicationSettings $
--             updateApplicationSettings
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
--         , requestPhoneNumberValidate $
--             phoneNumberValidate
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
--         , responseCreateExportJob $
--             createExportJobResponse
--
--         , responseCreateSegment $
--             createSegmentResponse
--
--         , responseUpdateADMChannel $
--             updateADMChannelResponse
--
--         , responseDeleteADMChannel $
--             deleteADMChannelResponse
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
--         , responseGetExportJob $
--             getExportJobResponse
--
--         , responseGetEndpoint $
--             getEndpointResponse
--
--         , responseGetSegment $
--             getSegmentResponse
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
--         , responseDeleteUserEndpoints $
--             deleteUserEndpointsResponse
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
--         , responseGetEventStream $
--             getEventStreamResponse
--
--         , responseGetChannels $
--             getChannelsResponse
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
--         , responsePutEventStream $
--             putEventStreamResponse
--
--         , responseDeleteEventStream $
--             deleteEventStreamResponse
--
--         , responseGetCampaignVersions $
--             getCampaignVersionsResponse
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
--         , responseGetUserEndpoints $
--             getUserEndpointsResponse
--
--         , responseGetImportJobs $
--             getImportJobsResponse
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
--         , responseCreateApp $
--             createAppResponse
--
--         , responseGetSegmentExportJobs $
--             getSegmentExportJobsResponse
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
--         , responsePutEvents $
--             putEventsResponse
--
--         , responseUpdateApplicationSettings $
--             updateApplicationSettingsResponse
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
--         , responsePhoneNumberValidate $
--             phoneNumberValidateResponse
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
requestGetGCMChannel = req
    "GetGCMChannel"
    "fixture/GetGCMChannel.yaml"

requestGetSegmentImportJobs :: GetSegmentImportJobs -> TestTree
requestGetSegmentImportJobs = req
    "GetSegmentImportJobs"
    "fixture/GetSegmentImportJobs.yaml"

requestSendMessages :: SendMessages -> TestTree
requestSendMessages = req
    "SendMessages"
    "fixture/SendMessages.yaml"

requestGetImportJob :: GetImportJob -> TestTree
requestGetImportJob = req
    "GetImportJob"
    "fixture/GetImportJob.yaml"

requestGetAPNSVoipSandboxChannel :: GetAPNSVoipSandboxChannel -> TestTree
requestGetAPNSVoipSandboxChannel = req
    "GetAPNSVoipSandboxChannel"
    "fixture/GetAPNSVoipSandboxChannel.yaml"

requestGetSegmentVersions :: GetSegmentVersions -> TestTree
requestGetSegmentVersions = req
    "GetSegmentVersions"
    "fixture/GetSegmentVersions.yaml"

requestDeleteCampaign :: DeleteCampaign -> TestTree
requestDeleteCampaign = req
    "DeleteCampaign"
    "fixture/DeleteCampaign.yaml"

requestUpdateCampaign :: UpdateCampaign -> TestTree
requestUpdateCampaign = req
    "UpdateCampaign"
    "fixture/UpdateCampaign.yaml"

requestGetSegmentVersion :: GetSegmentVersion -> TestTree
requestGetSegmentVersion = req
    "GetSegmentVersion"
    "fixture/GetSegmentVersion.yaml"

requestCreateExportJob :: CreateExportJob -> TestTree
requestCreateExportJob = req
    "CreateExportJob"
    "fixture/CreateExportJob.yaml"

requestCreateSegment :: CreateSegment -> TestTree
requestCreateSegment = req
    "CreateSegment"
    "fixture/CreateSegment.yaml"

requestUpdateADMChannel :: UpdateADMChannel -> TestTree
requestUpdateADMChannel = req
    "UpdateADMChannel"
    "fixture/UpdateADMChannel.yaml"

requestDeleteADMChannel :: DeleteADMChannel -> TestTree
requestDeleteADMChannel = req
    "DeleteADMChannel"
    "fixture/DeleteADMChannel.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint = req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestUpdateEndpoint :: UpdateEndpoint -> TestTree
requestUpdateEndpoint = req
    "UpdateEndpoint"
    "fixture/UpdateEndpoint.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateCampaign :: CreateCampaign -> TestTree
requestCreateCampaign = req
    "CreateCampaign"
    "fixture/CreateCampaign.yaml"

requestGetExportJob :: GetExportJob -> TestTree
requestGetExportJob = req
    "GetExportJob"
    "fixture/GetExportJob.yaml"

requestGetEndpoint :: GetEndpoint -> TestTree
requestGetEndpoint = req
    "GetEndpoint"
    "fixture/GetEndpoint.yaml"

requestGetSegment :: GetSegment -> TestTree
requestGetSegment = req
    "GetSegment"
    "fixture/GetSegment.yaml"

requestUpdateEndpointsBatch :: UpdateEndpointsBatch -> TestTree
requestUpdateEndpointsBatch = req
    "UpdateEndpointsBatch"
    "fixture/UpdateEndpointsBatch.yaml"

requestGetADMChannel :: GetADMChannel -> TestTree
requestGetADMChannel = req
    "GetADMChannel"
    "fixture/GetADMChannel.yaml"

requestGetCampaign :: GetCampaign -> TestTree
requestGetCampaign = req
    "GetCampaign"
    "fixture/GetCampaign.yaml"

requestDeleteUserEndpoints :: DeleteUserEndpoints -> TestTree
requestDeleteUserEndpoints = req
    "DeleteUserEndpoints"
    "fixture/DeleteUserEndpoints.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp = req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestUpdateAPNSVoipSandboxChannel :: UpdateAPNSVoipSandboxChannel -> TestTree
requestUpdateAPNSVoipSandboxChannel = req
    "UpdateAPNSVoipSandboxChannel"
    "fixture/UpdateAPNSVoipSandboxChannel.yaml"

requestDeleteAPNSVoipSandboxChannel :: DeleteAPNSVoipSandboxChannel -> TestTree
requestDeleteAPNSVoipSandboxChannel = req
    "DeleteAPNSVoipSandboxChannel"
    "fixture/DeleteAPNSVoipSandboxChannel.yaml"

requestUpdateGCMChannel :: UpdateGCMChannel -> TestTree
requestUpdateGCMChannel = req
    "UpdateGCMChannel"
    "fixture/UpdateGCMChannel.yaml"

requestDeleteGCMChannel :: DeleteGCMChannel -> TestTree
requestDeleteGCMChannel = req
    "DeleteGCMChannel"
    "fixture/DeleteGCMChannel.yaml"

requestGetCampaignActivities :: GetCampaignActivities -> TestTree
requestGetCampaignActivities = req
    "GetCampaignActivities"
    "fixture/GetCampaignActivities.yaml"

requestGetEventStream :: GetEventStream -> TestTree
requestGetEventStream = req
    "GetEventStream"
    "fixture/GetEventStream.yaml"

requestGetChannels :: GetChannels -> TestTree
requestGetChannels = req
    "GetChannels"
    "fixture/GetChannels.yaml"

requestDeleteEmailChannel :: DeleteEmailChannel -> TestTree
requestDeleteEmailChannel = req
    "DeleteEmailChannel"
    "fixture/DeleteEmailChannel.yaml"

requestUpdateEmailChannel :: UpdateEmailChannel -> TestTree
requestUpdateEmailChannel = req
    "UpdateEmailChannel"
    "fixture/UpdateEmailChannel.yaml"

requestGetBaiduChannel :: GetBaiduChannel -> TestTree
requestGetBaiduChannel = req
    "GetBaiduChannel"
    "fixture/GetBaiduChannel.yaml"

requestDeleteAPNSChannel :: DeleteAPNSChannel -> TestTree
requestDeleteAPNSChannel = req
    "DeleteAPNSChannel"
    "fixture/DeleteAPNSChannel.yaml"

requestUpdateAPNSChannel :: UpdateAPNSChannel -> TestTree
requestUpdateAPNSChannel = req
    "UpdateAPNSChannel"
    "fixture/UpdateAPNSChannel.yaml"

requestRemoveAttributes :: RemoveAttributes -> TestTree
requestRemoveAttributes = req
    "RemoveAttributes"
    "fixture/RemoveAttributes.yaml"

requestPutEventStream :: PutEventStream -> TestTree
requestPutEventStream = req
    "PutEventStream"
    "fixture/PutEventStream.yaml"

requestDeleteEventStream :: DeleteEventStream -> TestTree
requestDeleteEventStream = req
    "DeleteEventStream"
    "fixture/DeleteEventStream.yaml"

requestGetCampaignVersions :: GetCampaignVersions -> TestTree
requestGetCampaignVersions = req
    "GetCampaignVersions"
    "fixture/GetCampaignVersions.yaml"

requestGetAPNSChannel :: GetAPNSChannel -> TestTree
requestGetAPNSChannel = req
    "GetAPNSChannel"
    "fixture/GetAPNSChannel.yaml"

requestUpdateVoiceChannel :: UpdateVoiceChannel -> TestTree
requestUpdateVoiceChannel = req
    "UpdateVoiceChannel"
    "fixture/UpdateVoiceChannel.yaml"

requestDeleteVoiceChannel :: DeleteVoiceChannel -> TestTree
requestDeleteVoiceChannel = req
    "DeleteVoiceChannel"
    "fixture/DeleteVoiceChannel.yaml"

requestGetApps :: GetApps -> TestTree
requestGetApps = req
    "GetApps"
    "fixture/GetApps.yaml"

requestGetAPNSSandboxChannel :: GetAPNSSandboxChannel -> TestTree
requestGetAPNSSandboxChannel = req
    "GetAPNSSandboxChannel"
    "fixture/GetAPNSSandboxChannel.yaml"

requestGetUserEndpoints :: GetUserEndpoints -> TestTree
requestGetUserEndpoints = req
    "GetUserEndpoints"
    "fixture/GetUserEndpoints.yaml"

requestGetImportJobs :: GetImportJobs -> TestTree
requestGetImportJobs = req
    "GetImportJobs"
    "fixture/GetImportJobs.yaml"

requestDeleteSmsChannel :: DeleteSmsChannel -> TestTree
requestDeleteSmsChannel = req
    "DeleteSmsChannel"
    "fixture/DeleteSmsChannel.yaml"

requestUpdateSmsChannel :: UpdateSmsChannel -> TestTree
requestUpdateSmsChannel = req
    "UpdateSmsChannel"
    "fixture/UpdateSmsChannel.yaml"

requestGetApp :: GetApp -> TestTree
requestGetApp = req
    "GetApp"
    "fixture/GetApp.yaml"

requestGetCampaignVersion :: GetCampaignVersion -> TestTree
requestGetCampaignVersion = req
    "GetCampaignVersion"
    "fixture/GetCampaignVersion.yaml"

requestDeleteSegment :: DeleteSegment -> TestTree
requestDeleteSegment = req
    "DeleteSegment"
    "fixture/DeleteSegment.yaml"

requestUpdateSegment :: UpdateSegment -> TestTree
requestUpdateSegment = req
    "UpdateSegment"
    "fixture/UpdateSegment.yaml"

requestCreateApp :: CreateApp -> TestTree
requestCreateApp = req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestGetSegmentExportJobs :: GetSegmentExportJobs -> TestTree
requestGetSegmentExportJobs = req
    "GetSegmentExportJobs"
    "fixture/GetSegmentExportJobs.yaml"

requestGetSmsChannel :: GetSmsChannel -> TestTree
requestGetSmsChannel = req
    "GetSmsChannel"
    "fixture/GetSmsChannel.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestDeleteAPNSSandboxChannel :: DeleteAPNSSandboxChannel -> TestTree
requestDeleteAPNSSandboxChannel = req
    "DeleteAPNSSandboxChannel"
    "fixture/DeleteAPNSSandboxChannel.yaml"

requestUpdateAPNSSandboxChannel :: UpdateAPNSSandboxChannel -> TestTree
requestUpdateAPNSSandboxChannel = req
    "UpdateAPNSSandboxChannel"
    "fixture/UpdateAPNSSandboxChannel.yaml"

requestGetCampaigns :: GetCampaigns -> TestTree
requestGetCampaigns = req
    "GetCampaigns"
    "fixture/GetCampaigns.yaml"

requestGetVoiceChannel :: GetVoiceChannel -> TestTree
requestGetVoiceChannel = req
    "GetVoiceChannel"
    "fixture/GetVoiceChannel.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestPutEvents :: PutEvents -> TestTree
requestPutEvents = req
    "PutEvents"
    "fixture/PutEvents.yaml"

requestUpdateApplicationSettings :: UpdateApplicationSettings -> TestTree
requestUpdateApplicationSettings = req
    "UpdateApplicationSettings"
    "fixture/UpdateApplicationSettings.yaml"

requestGetSegments :: GetSegments -> TestTree
requestGetSegments = req
    "GetSegments"
    "fixture/GetSegments.yaml"

requestGetExportJobs :: GetExportJobs -> TestTree
requestGetExportJobs = req
    "GetExportJobs"
    "fixture/GetExportJobs.yaml"

requestCreateImportJob :: CreateImportJob -> TestTree
requestCreateImportJob = req
    "CreateImportJob"
    "fixture/CreateImportJob.yaml"

requestDeleteAPNSVoipChannel :: DeleteAPNSVoipChannel -> TestTree
requestDeleteAPNSVoipChannel = req
    "DeleteAPNSVoipChannel"
    "fixture/DeleteAPNSVoipChannel.yaml"

requestUpdateAPNSVoipChannel :: UpdateAPNSVoipChannel -> TestTree
requestUpdateAPNSVoipChannel = req
    "UpdateAPNSVoipChannel"
    "fixture/UpdateAPNSVoipChannel.yaml"

requestSendUsersMessages :: SendUsersMessages -> TestTree
requestSendUsersMessages = req
    "SendUsersMessages"
    "fixture/SendUsersMessages.yaml"

requestGetApplicationSettings :: GetApplicationSettings -> TestTree
requestGetApplicationSettings = req
    "GetApplicationSettings"
    "fixture/GetApplicationSettings.yaml"

requestDeleteBaiduChannel :: DeleteBaiduChannel -> TestTree
requestDeleteBaiduChannel = req
    "DeleteBaiduChannel"
    "fixture/DeleteBaiduChannel.yaml"

requestUpdateBaiduChannel :: UpdateBaiduChannel -> TestTree
requestUpdateBaiduChannel = req
    "UpdateBaiduChannel"
    "fixture/UpdateBaiduChannel.yaml"

requestPhoneNumberValidate :: PhoneNumberValidate -> TestTree
requestPhoneNumberValidate = req
    "PhoneNumberValidate"
    "fixture/PhoneNumberValidate.yaml"

requestGetAPNSVoipChannel :: GetAPNSVoipChannel -> TestTree
requestGetAPNSVoipChannel = req
    "GetAPNSVoipChannel"
    "fixture/GetAPNSVoipChannel.yaml"

requestGetEmailChannel :: GetEmailChannel -> TestTree
requestGetEmailChannel = req
    "GetEmailChannel"
    "fixture/GetEmailChannel.yaml"

-- Responses

responseGetGCMChannel :: GetGCMChannelResponse -> TestTree
responseGetGCMChannel = res
    "GetGCMChannelResponse"
    "fixture/GetGCMChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetGCMChannel)

responseGetSegmentImportJobs :: GetSegmentImportJobsResponse -> TestTree
responseGetSegmentImportJobs = res
    "GetSegmentImportJobsResponse"
    "fixture/GetSegmentImportJobsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetSegmentImportJobs)

responseSendMessages :: SendMessagesResponse -> TestTree
responseSendMessages = res
    "SendMessagesResponse"
    "fixture/SendMessagesResponse.proto"
    pinpoint
    (Proxy :: Proxy SendMessages)

responseGetImportJob :: GetImportJobResponse -> TestTree
responseGetImportJob = res
    "GetImportJobResponse"
    "fixture/GetImportJobResponse.proto"
    pinpoint
    (Proxy :: Proxy GetImportJob)

responseGetAPNSVoipSandboxChannel :: GetAPNSVoipSandboxChannelResponse -> TestTree
responseGetAPNSVoipSandboxChannel = res
    "GetAPNSVoipSandboxChannelResponse"
    "fixture/GetAPNSVoipSandboxChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetAPNSVoipSandboxChannel)

responseGetSegmentVersions :: GetSegmentVersionsResponse -> TestTree
responseGetSegmentVersions = res
    "GetSegmentVersionsResponse"
    "fixture/GetSegmentVersionsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetSegmentVersions)

responseDeleteCampaign :: DeleteCampaignResponse -> TestTree
responseDeleteCampaign = res
    "DeleteCampaignResponse"
    "fixture/DeleteCampaignResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteCampaign)

responseUpdateCampaign :: UpdateCampaignResponse -> TestTree
responseUpdateCampaign = res
    "UpdateCampaignResponse"
    "fixture/UpdateCampaignResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateCampaign)

responseGetSegmentVersion :: GetSegmentVersionResponse -> TestTree
responseGetSegmentVersion = res
    "GetSegmentVersionResponse"
    "fixture/GetSegmentVersionResponse.proto"
    pinpoint
    (Proxy :: Proxy GetSegmentVersion)

responseCreateExportJob :: CreateExportJobResponse -> TestTree
responseCreateExportJob = res
    "CreateExportJobResponse"
    "fixture/CreateExportJobResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateExportJob)

responseCreateSegment :: CreateSegmentResponse -> TestTree
responseCreateSegment = res
    "CreateSegmentResponse"
    "fixture/CreateSegmentResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateSegment)

responseUpdateADMChannel :: UpdateADMChannelResponse -> TestTree
responseUpdateADMChannel = res
    "UpdateADMChannelResponse"
    "fixture/UpdateADMChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateADMChannel)

responseDeleteADMChannel :: DeleteADMChannelResponse -> TestTree
responseDeleteADMChannel = res
    "DeleteADMChannelResponse"
    "fixture/DeleteADMChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteADMChannel)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint = res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteEndpoint)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint = res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateEndpoint)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    pinpoint
    (Proxy :: Proxy ListTagsForResource)

responseCreateCampaign :: CreateCampaignResponse -> TestTree
responseCreateCampaign = res
    "CreateCampaignResponse"
    "fixture/CreateCampaignResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateCampaign)

responseGetExportJob :: GetExportJobResponse -> TestTree
responseGetExportJob = res
    "GetExportJobResponse"
    "fixture/GetExportJobResponse.proto"
    pinpoint
    (Proxy :: Proxy GetExportJob)

responseGetEndpoint :: GetEndpointResponse -> TestTree
responseGetEndpoint = res
    "GetEndpointResponse"
    "fixture/GetEndpointResponse.proto"
    pinpoint
    (Proxy :: Proxy GetEndpoint)

responseGetSegment :: GetSegmentResponse -> TestTree
responseGetSegment = res
    "GetSegmentResponse"
    "fixture/GetSegmentResponse.proto"
    pinpoint
    (Proxy :: Proxy GetSegment)

responseUpdateEndpointsBatch :: UpdateEndpointsBatchResponse -> TestTree
responseUpdateEndpointsBatch = res
    "UpdateEndpointsBatchResponse"
    "fixture/UpdateEndpointsBatchResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateEndpointsBatch)

responseGetADMChannel :: GetADMChannelResponse -> TestTree
responseGetADMChannel = res
    "GetADMChannelResponse"
    "fixture/GetADMChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetADMChannel)

responseGetCampaign :: GetCampaignResponse -> TestTree
responseGetCampaign = res
    "GetCampaignResponse"
    "fixture/GetCampaignResponse.proto"
    pinpoint
    (Proxy :: Proxy GetCampaign)

responseDeleteUserEndpoints :: DeleteUserEndpointsResponse -> TestTree
responseDeleteUserEndpoints = res
    "DeleteUserEndpointsResponse"
    "fixture/DeleteUserEndpointsResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteUserEndpoints)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp = res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteApp)

responseUpdateAPNSVoipSandboxChannel :: UpdateAPNSVoipSandboxChannelResponse -> TestTree
responseUpdateAPNSVoipSandboxChannel = res
    "UpdateAPNSVoipSandboxChannelResponse"
    "fixture/UpdateAPNSVoipSandboxChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateAPNSVoipSandboxChannel)

responseDeleteAPNSVoipSandboxChannel :: DeleteAPNSVoipSandboxChannelResponse -> TestTree
responseDeleteAPNSVoipSandboxChannel = res
    "DeleteAPNSVoipSandboxChannelResponse"
    "fixture/DeleteAPNSVoipSandboxChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteAPNSVoipSandboxChannel)

responseUpdateGCMChannel :: UpdateGCMChannelResponse -> TestTree
responseUpdateGCMChannel = res
    "UpdateGCMChannelResponse"
    "fixture/UpdateGCMChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateGCMChannel)

responseDeleteGCMChannel :: DeleteGCMChannelResponse -> TestTree
responseDeleteGCMChannel = res
    "DeleteGCMChannelResponse"
    "fixture/DeleteGCMChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteGCMChannel)

responseGetCampaignActivities :: GetCampaignActivitiesResponse -> TestTree
responseGetCampaignActivities = res
    "GetCampaignActivitiesResponse"
    "fixture/GetCampaignActivitiesResponse.proto"
    pinpoint
    (Proxy :: Proxy GetCampaignActivities)

responseGetEventStream :: GetEventStreamResponse -> TestTree
responseGetEventStream = res
    "GetEventStreamResponse"
    "fixture/GetEventStreamResponse.proto"
    pinpoint
    (Proxy :: Proxy GetEventStream)

responseGetChannels :: GetChannelsResponse -> TestTree
responseGetChannels = res
    "GetChannelsResponse"
    "fixture/GetChannelsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetChannels)

responseDeleteEmailChannel :: DeleteEmailChannelResponse -> TestTree
responseDeleteEmailChannel = res
    "DeleteEmailChannelResponse"
    "fixture/DeleteEmailChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteEmailChannel)

responseUpdateEmailChannel :: UpdateEmailChannelResponse -> TestTree
responseUpdateEmailChannel = res
    "UpdateEmailChannelResponse"
    "fixture/UpdateEmailChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateEmailChannel)

responseGetBaiduChannel :: GetBaiduChannelResponse -> TestTree
responseGetBaiduChannel = res
    "GetBaiduChannelResponse"
    "fixture/GetBaiduChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetBaiduChannel)

responseDeleteAPNSChannel :: DeleteAPNSChannelResponse -> TestTree
responseDeleteAPNSChannel = res
    "DeleteAPNSChannelResponse"
    "fixture/DeleteAPNSChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteAPNSChannel)

responseUpdateAPNSChannel :: UpdateAPNSChannelResponse -> TestTree
responseUpdateAPNSChannel = res
    "UpdateAPNSChannelResponse"
    "fixture/UpdateAPNSChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateAPNSChannel)

responseRemoveAttributes :: RemoveAttributesResponse -> TestTree
responseRemoveAttributes = res
    "RemoveAttributesResponse"
    "fixture/RemoveAttributesResponse.proto"
    pinpoint
    (Proxy :: Proxy RemoveAttributes)

responsePutEventStream :: PutEventStreamResponse -> TestTree
responsePutEventStream = res
    "PutEventStreamResponse"
    "fixture/PutEventStreamResponse.proto"
    pinpoint
    (Proxy :: Proxy PutEventStream)

responseDeleteEventStream :: DeleteEventStreamResponse -> TestTree
responseDeleteEventStream = res
    "DeleteEventStreamResponse"
    "fixture/DeleteEventStreamResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteEventStream)

responseGetCampaignVersions :: GetCampaignVersionsResponse -> TestTree
responseGetCampaignVersions = res
    "GetCampaignVersionsResponse"
    "fixture/GetCampaignVersionsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetCampaignVersions)

responseGetAPNSChannel :: GetAPNSChannelResponse -> TestTree
responseGetAPNSChannel = res
    "GetAPNSChannelResponse"
    "fixture/GetAPNSChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetAPNSChannel)

responseUpdateVoiceChannel :: UpdateVoiceChannelResponse -> TestTree
responseUpdateVoiceChannel = res
    "UpdateVoiceChannelResponse"
    "fixture/UpdateVoiceChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateVoiceChannel)

responseDeleteVoiceChannel :: DeleteVoiceChannelResponse -> TestTree
responseDeleteVoiceChannel = res
    "DeleteVoiceChannelResponse"
    "fixture/DeleteVoiceChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteVoiceChannel)

responseGetApps :: GetAppsResponse -> TestTree
responseGetApps = res
    "GetAppsResponse"
    "fixture/GetAppsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetApps)

responseGetAPNSSandboxChannel :: GetAPNSSandboxChannelResponse -> TestTree
responseGetAPNSSandboxChannel = res
    "GetAPNSSandboxChannelResponse"
    "fixture/GetAPNSSandboxChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetAPNSSandboxChannel)

responseGetUserEndpoints :: GetUserEndpointsResponse -> TestTree
responseGetUserEndpoints = res
    "GetUserEndpointsResponse"
    "fixture/GetUserEndpointsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetUserEndpoints)

responseGetImportJobs :: GetImportJobsResponse -> TestTree
responseGetImportJobs = res
    "GetImportJobsResponse"
    "fixture/GetImportJobsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetImportJobs)

responseDeleteSmsChannel :: DeleteSmsChannelResponse -> TestTree
responseDeleteSmsChannel = res
    "DeleteSmsChannelResponse"
    "fixture/DeleteSmsChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteSmsChannel)

responseUpdateSmsChannel :: UpdateSmsChannelResponse -> TestTree
responseUpdateSmsChannel = res
    "UpdateSmsChannelResponse"
    "fixture/UpdateSmsChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateSmsChannel)

responseGetApp :: GetAppResponse -> TestTree
responseGetApp = res
    "GetAppResponse"
    "fixture/GetAppResponse.proto"
    pinpoint
    (Proxy :: Proxy GetApp)

responseGetCampaignVersion :: GetCampaignVersionResponse -> TestTree
responseGetCampaignVersion = res
    "GetCampaignVersionResponse"
    "fixture/GetCampaignVersionResponse.proto"
    pinpoint
    (Proxy :: Proxy GetCampaignVersion)

responseDeleteSegment :: DeleteSegmentResponse -> TestTree
responseDeleteSegment = res
    "DeleteSegmentResponse"
    "fixture/DeleteSegmentResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteSegment)

responseUpdateSegment :: UpdateSegmentResponse -> TestTree
responseUpdateSegment = res
    "UpdateSegmentResponse"
    "fixture/UpdateSegmentResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateSegment)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp = res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateApp)

responseGetSegmentExportJobs :: GetSegmentExportJobsResponse -> TestTree
responseGetSegmentExportJobs = res
    "GetSegmentExportJobsResponse"
    "fixture/GetSegmentExportJobsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetSegmentExportJobs)

responseGetSmsChannel :: GetSmsChannelResponse -> TestTree
responseGetSmsChannel = res
    "GetSmsChannelResponse"
    "fixture/GetSmsChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetSmsChannel)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    pinpoint
    (Proxy :: Proxy TagResource)

responseDeleteAPNSSandboxChannel :: DeleteAPNSSandboxChannelResponse -> TestTree
responseDeleteAPNSSandboxChannel = res
    "DeleteAPNSSandboxChannelResponse"
    "fixture/DeleteAPNSSandboxChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteAPNSSandboxChannel)

responseUpdateAPNSSandboxChannel :: UpdateAPNSSandboxChannelResponse -> TestTree
responseUpdateAPNSSandboxChannel = res
    "UpdateAPNSSandboxChannelResponse"
    "fixture/UpdateAPNSSandboxChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateAPNSSandboxChannel)

responseGetCampaigns :: GetCampaignsResponse -> TestTree
responseGetCampaigns = res
    "GetCampaignsResponse"
    "fixture/GetCampaignsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetCampaigns)

responseGetVoiceChannel :: GetVoiceChannelResponse -> TestTree
responseGetVoiceChannel = res
    "GetVoiceChannelResponse"
    "fixture/GetVoiceChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetVoiceChannel)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    pinpoint
    (Proxy :: Proxy UntagResource)

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents = res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    pinpoint
    (Proxy :: Proxy PutEvents)

responseUpdateApplicationSettings :: UpdateApplicationSettingsResponse -> TestTree
responseUpdateApplicationSettings = res
    "UpdateApplicationSettingsResponse"
    "fixture/UpdateApplicationSettingsResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateApplicationSettings)

responseGetSegments :: GetSegmentsResponse -> TestTree
responseGetSegments = res
    "GetSegmentsResponse"
    "fixture/GetSegmentsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetSegments)

responseGetExportJobs :: GetExportJobsResponse -> TestTree
responseGetExportJobs = res
    "GetExportJobsResponse"
    "fixture/GetExportJobsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetExportJobs)

responseCreateImportJob :: CreateImportJobResponse -> TestTree
responseCreateImportJob = res
    "CreateImportJobResponse"
    "fixture/CreateImportJobResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateImportJob)

responseDeleteAPNSVoipChannel :: DeleteAPNSVoipChannelResponse -> TestTree
responseDeleteAPNSVoipChannel = res
    "DeleteAPNSVoipChannelResponse"
    "fixture/DeleteAPNSVoipChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteAPNSVoipChannel)

responseUpdateAPNSVoipChannel :: UpdateAPNSVoipChannelResponse -> TestTree
responseUpdateAPNSVoipChannel = res
    "UpdateAPNSVoipChannelResponse"
    "fixture/UpdateAPNSVoipChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateAPNSVoipChannel)

responseSendUsersMessages :: SendUsersMessagesResponse -> TestTree
responseSendUsersMessages = res
    "SendUsersMessagesResponse"
    "fixture/SendUsersMessagesResponse.proto"
    pinpoint
    (Proxy :: Proxy SendUsersMessages)

responseGetApplicationSettings :: GetApplicationSettingsResponse -> TestTree
responseGetApplicationSettings = res
    "GetApplicationSettingsResponse"
    "fixture/GetApplicationSettingsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetApplicationSettings)

responseDeleteBaiduChannel :: DeleteBaiduChannelResponse -> TestTree
responseDeleteBaiduChannel = res
    "DeleteBaiduChannelResponse"
    "fixture/DeleteBaiduChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy DeleteBaiduChannel)

responseUpdateBaiduChannel :: UpdateBaiduChannelResponse -> TestTree
responseUpdateBaiduChannel = res
    "UpdateBaiduChannelResponse"
    "fixture/UpdateBaiduChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateBaiduChannel)

responsePhoneNumberValidate :: PhoneNumberValidateResponse -> TestTree
responsePhoneNumberValidate = res
    "PhoneNumberValidateResponse"
    "fixture/PhoneNumberValidateResponse.proto"
    pinpoint
    (Proxy :: Proxy PhoneNumberValidate)

responseGetAPNSVoipChannel :: GetAPNSVoipChannelResponse -> TestTree
responseGetAPNSVoipChannel = res
    "GetAPNSVoipChannelResponse"
    "fixture/GetAPNSVoipChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetAPNSVoipChannel)

responseGetEmailChannel :: GetEmailChannelResponse -> TestTree
responseGetEmailChannel = res
    "GetEmailChannelResponse"
    "fixture/GetEmailChannelResponse.proto"
    pinpoint
    (Proxy :: Proxy GetEmailChannel)
