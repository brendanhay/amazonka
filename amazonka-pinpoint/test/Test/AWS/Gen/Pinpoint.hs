{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Pinpoint
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Pinpoint where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Pinpoint
import Test.AWS.Pinpoint.Internal

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
--         , requestGetImportJob $
--             getImportJob
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
--         , requestCreateSegment $
--             createSegment
--
--         , requestUpdateEndpoint $
--             updateEndpoint
--
--         , requestCreateCampaign $
--             createCampaign
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
--         , requestGetCampaign $
--             getCampaign
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
--         , requestDeleteAPNSChannel $
--             deleteAPNSChannel
--
--         , requestUpdateAPNSChannel $
--             updateAPNSChannel
--
--         , requestGetCampaignVersions $
--             getCampaignVersions
--
--         , requestGetAPNSChannel $
--             getAPNSChannel
--
--         , requestGetImportJobs $
--             getImportJobs
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
--         , requestGetCampaigns $
--             getCampaigns
--
--         , requestUpdateApplicationSettings $
--             updateApplicationSettings
--
--         , requestGetSegments $
--             getSegments
--
--         , requestCreateImportJob $
--             createImportJob
--
--         , requestGetApplicationSettings $
--             getApplicationSettings
--
--           ]

--     , testGroup "response"
--         [ responseGetGCMChannel $
--             getGCMChannelResponse
--
--         , responseGetSegmentImportJobs $
--             getSegmentImportJobsResponse
--
--         , responseGetImportJob $
--             getImportJobResponse
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
--         , responseCreateSegment $
--             createSegmentResponse
--
--         , responseUpdateEndpoint $
--             updateEndpointResponse
--
--         , responseCreateCampaign $
--             createCampaignResponse
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
--         , responseGetCampaign $
--             getCampaignResponse
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
--         , responseDeleteAPNSChannel $
--             deleteAPNSChannelResponse
--
--         , responseUpdateAPNSChannel $
--             updateAPNSChannelResponse
--
--         , responseGetCampaignVersions $
--             getCampaignVersionsResponse
--
--         , responseGetAPNSChannel $
--             getAPNSChannelResponse
--
--         , responseGetImportJobs $
--             getImportJobsResponse
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
--         , responseGetCampaigns $
--             getCampaignsResponse
--
--         , responseUpdateApplicationSettings $
--             updateApplicationSettingsResponse
--
--         , responseGetSegments $
--             getSegmentsResponse
--
--         , responseCreateImportJob $
--             createImportJobResponse
--
--         , responseGetApplicationSettings $
--             getApplicationSettingsResponse
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

requestGetImportJob :: GetImportJob -> TestTree
requestGetImportJob = req
    "GetImportJob"
    "fixture/GetImportJob.yaml"

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

requestCreateSegment :: CreateSegment -> TestTree
requestCreateSegment = req
    "CreateSegment"
    "fixture/CreateSegment.yaml"

requestUpdateEndpoint :: UpdateEndpoint -> TestTree
requestUpdateEndpoint = req
    "UpdateEndpoint"
    "fixture/UpdateEndpoint.yaml"

requestCreateCampaign :: CreateCampaign -> TestTree
requestCreateCampaign = req
    "CreateCampaign"
    "fixture/CreateCampaign.yaml"

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

requestGetCampaign :: GetCampaign -> TestTree
requestGetCampaign = req
    "GetCampaign"
    "fixture/GetCampaign.yaml"

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

requestDeleteAPNSChannel :: DeleteAPNSChannel -> TestTree
requestDeleteAPNSChannel = req
    "DeleteAPNSChannel"
    "fixture/DeleteAPNSChannel.yaml"

requestUpdateAPNSChannel :: UpdateAPNSChannel -> TestTree
requestUpdateAPNSChannel = req
    "UpdateAPNSChannel"
    "fixture/UpdateAPNSChannel.yaml"

requestGetCampaignVersions :: GetCampaignVersions -> TestTree
requestGetCampaignVersions = req
    "GetCampaignVersions"
    "fixture/GetCampaignVersions.yaml"

requestGetAPNSChannel :: GetAPNSChannel -> TestTree
requestGetAPNSChannel = req
    "GetAPNSChannel"
    "fixture/GetAPNSChannel.yaml"

requestGetImportJobs :: GetImportJobs -> TestTree
requestGetImportJobs = req
    "GetImportJobs"
    "fixture/GetImportJobs.yaml"

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

requestGetCampaigns :: GetCampaigns -> TestTree
requestGetCampaigns = req
    "GetCampaigns"
    "fixture/GetCampaigns.yaml"

requestUpdateApplicationSettings :: UpdateApplicationSettings -> TestTree
requestUpdateApplicationSettings = req
    "UpdateApplicationSettings"
    "fixture/UpdateApplicationSettings.yaml"

requestGetSegments :: GetSegments -> TestTree
requestGetSegments = req
    "GetSegments"
    "fixture/GetSegments.yaml"

requestCreateImportJob :: CreateImportJob -> TestTree
requestCreateImportJob = req
    "CreateImportJob"
    "fixture/CreateImportJob.yaml"

requestGetApplicationSettings :: GetApplicationSettings -> TestTree
requestGetApplicationSettings = req
    "GetApplicationSettings"
    "fixture/GetApplicationSettings.yaml"

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

responseGetImportJob :: GetImportJobResponse -> TestTree
responseGetImportJob = res
    "GetImportJobResponse"
    "fixture/GetImportJobResponse.proto"
    pinpoint
    (Proxy :: Proxy GetImportJob)

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

responseCreateSegment :: CreateSegmentResponse -> TestTree
responseCreateSegment = res
    "CreateSegmentResponse"
    "fixture/CreateSegmentResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateSegment)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint = res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    pinpoint
    (Proxy :: Proxy UpdateEndpoint)

responseCreateCampaign :: CreateCampaignResponse -> TestTree
responseCreateCampaign = res
    "CreateCampaignResponse"
    "fixture/CreateCampaignResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateCampaign)

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

responseGetCampaign :: GetCampaignResponse -> TestTree
responseGetCampaign = res
    "GetCampaignResponse"
    "fixture/GetCampaignResponse.proto"
    pinpoint
    (Proxy :: Proxy GetCampaign)

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

responseGetImportJobs :: GetImportJobsResponse -> TestTree
responseGetImportJobs = res
    "GetImportJobsResponse"
    "fixture/GetImportJobsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetImportJobs)

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

responseGetCampaigns :: GetCampaignsResponse -> TestTree
responseGetCampaigns = res
    "GetCampaignsResponse"
    "fixture/GetCampaignsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetCampaigns)

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

responseCreateImportJob :: CreateImportJobResponse -> TestTree
responseCreateImportJob = res
    "CreateImportJobResponse"
    "fixture/CreateImportJobResponse.proto"
    pinpoint
    (Proxy :: Proxy CreateImportJob)

responseGetApplicationSettings :: GetApplicationSettingsResponse -> TestTree
responseGetApplicationSettings = res
    "GetApplicationSettingsResponse"
    "fixture/GetApplicationSettingsResponse.proto"
    pinpoint
    (Proxy :: Proxy GetApplicationSettings)
