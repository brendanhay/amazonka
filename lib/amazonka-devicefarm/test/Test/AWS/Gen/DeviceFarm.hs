{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DeviceFarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.DeviceFarm where

import Data.Proxy
import Network.AWS.DeviceFarm
import Test.AWS.DeviceFarm.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListProjects $
--             listProjects
--
--         , requestDeleteProject $
--             deleteProject
--
--         , requestUpdateProject $
--             updateProject
--
--         , requestUpdateNetworkProfile $
--             updateNetworkProfile
--
--         , requestDeleteNetworkProfile $
--             deleteNetworkProfile
--
--         , requestGetDevicePoolCompatibility $
--             getDevicePoolCompatibility
--
--         , requestInstallToRemoteAccessSession $
--             installToRemoteAccessSession
--
--         , requestListTests $
--             listTests
--
--         , requestListArtifacts $
--             listArtifacts
--
--         , requestListTestGridSessionActions $
--             listTestGridSessionActions
--
--         , requestCreateUpload $
--             createUpload
--
--         , requestGetDeviceInstance $
--             getDeviceInstance
--
--         , requestStopJob $
--             stopJob
--
--         , requestDeleteRemoteAccessSession $
--             deleteRemoteAccessSession
--
--         , requestListTestGridSessionArtifacts $
--             listTestGridSessionArtifacts
--
--         , requestListTestGridProjects $
--             listTestGridProjects
--
--         , requestDeleteUpload $
--             deleteUpload
--
--         , requestUpdateUpload $
--             updateUpload
--
--         , requestDeleteTestGridProject $
--             deleteTestGridProject
--
--         , requestUpdateTestGridProject $
--             updateTestGridProject
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestGetDevicePool $
--             getDevicePool
--
--         , requestListDevicePools $
--             listDevicePools
--
--         , requestUpdateDevicePool $
--             updateDevicePool
--
--         , requestDeleteDevicePool $
--             deleteDevicePool
--
--         , requestGetUpload $
--             getUpload
--
--         , requestListOfferingTransactions $
--             listOfferingTransactions
--
--         , requestCreateDevicePool $
--             createDevicePool
--
--         , requestDeleteRun $
--             deleteRun
--
--         , requestListRuns $
--             listRuns
--
--         , requestGetTest $
--             getTest
--
--         , requestUpdateDeviceInstance $
--             updateDeviceInstance
--
--         , requestGetNetworkProfile $
--             getNetworkProfile
--
--         , requestRenewOffering $
--             renewOffering
--
--         , requestDeleteInstanceProfile $
--             deleteInstanceProfile
--
--         , requestUpdateInstanceProfile $
--             updateInstanceProfile
--
--         , requestCreateInstanceProfile $
--             createInstanceProfile
--
--         , requestGetDevice $
--             getDevice
--
--         , requestListJobs $
--             listJobs
--
--         , requestGetTestGridSession $
--             getTestGridSession
--
--         , requestGetVPCEConfiguration $
--             getVPCEConfiguration
--
--         , requestStopRemoteAccessSession $
--             stopRemoteAccessSession
--
--         , requestCreateNetworkProfile $
--             createNetworkProfile
--
--         , requestDeleteVPCEConfiguration $
--             deleteVPCEConfiguration
--
--         , requestUpdateVPCEConfiguration $
--             updateVPCEConfiguration
--
--         , requestGetJob $
--             getJob
--
--         , requestGetInstanceProfile $
--             getInstanceProfile
--
--         , requestListNetworkProfiles $
--             listNetworkProfiles
--
--         , requestCreateVPCEConfiguration $
--             createVPCEConfiguration
--
--         , requestScheduleRun $
--             scheduleRun
--
--         , requestCreateTestGridProject $
--             createTestGridProject
--
--         , requestGetRun $
--             getRun
--
--         , requestListSamples $
--             listSamples
--
--         , requestListSuites $
--             listSuites
--
--         , requestListRemoteAccessSessions $
--             listRemoteAccessSessions
--
--         , requestGetAccountSettings $
--             getAccountSettings
--
--         , requestCreateRemoteAccessSession $
--             createRemoteAccessSession
--
--         , requestListOfferingPromotions $
--             listOfferingPromotions
--
--         , requestGetOfferingStatus $
--             getOfferingStatus
--
--         , requestListUploads $
--             listUploads
--
--         , requestGetTestGridProject $
--             getTestGridProject
--
--         , requestGetSuite $
--             getSuite
--
--         , requestTagResource $
--             tagResource
--
--         , requestGetRemoteAccessSession $
--             getRemoteAccessSession
--
--         , requestListDeviceInstances $
--             listDeviceInstances
--
--         , requestPurchaseOffering $
--             purchaseOffering
--
--         , requestListInstanceProfiles $
--             listInstanceProfiles
--
--         , requestUntagResource $
--             untagResource
--
--         , requestGetProject $
--             getProject
--
--         , requestListUniqueProblems $
--             listUniqueProblems
--
--         , requestListVPCEConfigurations $
--             listVPCEConfigurations
--
--         , requestStopRun $
--             stopRun
--
--         , requestListDevices $
--             listDevices
--
--         , requestCreateProject $
--             createProject
--
--         , requestListTestGridSessions $
--             listTestGridSessions
--
--         , requestCreateTestGridURL $
--             createTestGridURL
--
--         , requestListOfferings $
--             listOfferings
--
--           ]

--     , testGroup "response"
--         [ responseListProjects $
--             listProjectsResponse
--
--         , responseDeleteProject $
--             deleteProjectResponse
--
--         , responseUpdateProject $
--             updateProjectResponse
--
--         , responseUpdateNetworkProfile $
--             updateNetworkProfileResponse
--
--         , responseDeleteNetworkProfile $
--             deleteNetworkProfileResponse
--
--         , responseGetDevicePoolCompatibility $
--             getDevicePoolCompatibilityResponse
--
--         , responseInstallToRemoteAccessSession $
--             installToRemoteAccessSessionResponse
--
--         , responseListTests $
--             listTestsResponse
--
--         , responseListArtifacts $
--             listArtifactsResponse
--
--         , responseListTestGridSessionActions $
--             listTestGridSessionActionsResponse
--
--         , responseCreateUpload $
--             createUploadResponse
--
--         , responseGetDeviceInstance $
--             getDeviceInstanceResponse
--
--         , responseStopJob $
--             stopJobResponse
--
--         , responseDeleteRemoteAccessSession $
--             deleteRemoteAccessSessionResponse
--
--         , responseListTestGridSessionArtifacts $
--             listTestGridSessionArtifactsResponse
--
--         , responseListTestGridProjects $
--             listTestGridProjectsResponse
--
--         , responseDeleteUpload $
--             deleteUploadResponse
--
--         , responseUpdateUpload $
--             updateUploadResponse
--
--         , responseDeleteTestGridProject $
--             deleteTestGridProjectResponse
--
--         , responseUpdateTestGridProject $
--             updateTestGridProjectResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseGetDevicePool $
--             getDevicePoolResponse
--
--         , responseListDevicePools $
--             listDevicePoolsResponse
--
--         , responseUpdateDevicePool $
--             updateDevicePoolResponse
--
--         , responseDeleteDevicePool $
--             deleteDevicePoolResponse
--
--         , responseGetUpload $
--             getUploadResponse
--
--         , responseListOfferingTransactions $
--             listOfferingTransactionsResponse
--
--         , responseCreateDevicePool $
--             createDevicePoolResponse
--
--         , responseDeleteRun $
--             deleteRunResponse
--
--         , responseListRuns $
--             listRunsResponse
--
--         , responseGetTest $
--             getTestResponse
--
--         , responseUpdateDeviceInstance $
--             updateDeviceInstanceResponse
--
--         , responseGetNetworkProfile $
--             getNetworkProfileResponse
--
--         , responseRenewOffering $
--             renewOfferingResponse
--
--         , responseDeleteInstanceProfile $
--             deleteInstanceProfileResponse
--
--         , responseUpdateInstanceProfile $
--             updateInstanceProfileResponse
--
--         , responseCreateInstanceProfile $
--             createInstanceProfileResponse
--
--         , responseGetDevice $
--             getDeviceResponse
--
--         , responseListJobs $
--             listJobsResponse
--
--         , responseGetTestGridSession $
--             getTestGridSessionResponse
--
--         , responseGetVPCEConfiguration $
--             getVPCEConfigurationResponse
--
--         , responseStopRemoteAccessSession $
--             stopRemoteAccessSessionResponse
--
--         , responseCreateNetworkProfile $
--             createNetworkProfileResponse
--
--         , responseDeleteVPCEConfiguration $
--             deleteVPCEConfigurationResponse
--
--         , responseUpdateVPCEConfiguration $
--             updateVPCEConfigurationResponse
--
--         , responseGetJob $
--             getJobResponse
--
--         , responseGetInstanceProfile $
--             getInstanceProfileResponse
--
--         , responseListNetworkProfiles $
--             listNetworkProfilesResponse
--
--         , responseCreateVPCEConfiguration $
--             createVPCEConfigurationResponse
--
--         , responseScheduleRun $
--             scheduleRunResponse
--
--         , responseCreateTestGridProject $
--             createTestGridProjectResponse
--
--         , responseGetRun $
--             getRunResponse
--
--         , responseListSamples $
--             listSamplesResponse
--
--         , responseListSuites $
--             listSuitesResponse
--
--         , responseListRemoteAccessSessions $
--             listRemoteAccessSessionsResponse
--
--         , responseGetAccountSettings $
--             getAccountSettingsResponse
--
--         , responseCreateRemoteAccessSession $
--             createRemoteAccessSessionResponse
--
--         , responseListOfferingPromotions $
--             listOfferingPromotionsResponse
--
--         , responseGetOfferingStatus $
--             getOfferingStatusResponse
--
--         , responseListUploads $
--             listUploadsResponse
--
--         , responseGetTestGridProject $
--             getTestGridProjectResponse
--
--         , responseGetSuite $
--             getSuiteResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseGetRemoteAccessSession $
--             getRemoteAccessSessionResponse
--
--         , responseListDeviceInstances $
--             listDeviceInstancesResponse
--
--         , responsePurchaseOffering $
--             purchaseOfferingResponse
--
--         , responseListInstanceProfiles $
--             listInstanceProfilesResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseGetProject $
--             getProjectResponse
--
--         , responseListUniqueProblems $
--             listUniqueProblemsResponse
--
--         , responseListVPCEConfigurations $
--             listVPCEConfigurationsResponse
--
--         , responseStopRun $
--             stopRunResponse
--
--         , responseListDevices $
--             listDevicesResponse
--
--         , responseCreateProject $
--             createProjectResponse
--
--         , responseListTestGridSessions $
--             listTestGridSessionsResponse
--
--         , responseCreateTestGridURL $
--             createTestGridURLResponse
--
--         , responseListOfferings $
--             listOfferingsResponse
--
--           ]
--     ]

-- Requests

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject =
  req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

requestUpdateNetworkProfile :: UpdateNetworkProfile -> TestTree
requestUpdateNetworkProfile =
  req
    "UpdateNetworkProfile"
    "fixture/UpdateNetworkProfile.yaml"

requestDeleteNetworkProfile :: DeleteNetworkProfile -> TestTree
requestDeleteNetworkProfile =
  req
    "DeleteNetworkProfile"
    "fixture/DeleteNetworkProfile.yaml"

requestGetDevicePoolCompatibility :: GetDevicePoolCompatibility -> TestTree
requestGetDevicePoolCompatibility =
  req
    "GetDevicePoolCompatibility"
    "fixture/GetDevicePoolCompatibility.yaml"

requestInstallToRemoteAccessSession :: InstallToRemoteAccessSession -> TestTree
requestInstallToRemoteAccessSession =
  req
    "InstallToRemoteAccessSession"
    "fixture/InstallToRemoteAccessSession.yaml"

requestListTests :: ListTests -> TestTree
requestListTests =
  req
    "ListTests"
    "fixture/ListTests.yaml"

requestListArtifacts :: ListArtifacts -> TestTree
requestListArtifacts =
  req
    "ListArtifacts"
    "fixture/ListArtifacts.yaml"

requestListTestGridSessionActions :: ListTestGridSessionActions -> TestTree
requestListTestGridSessionActions =
  req
    "ListTestGridSessionActions"
    "fixture/ListTestGridSessionActions.yaml"

requestCreateUpload :: CreateUpload -> TestTree
requestCreateUpload =
  req
    "CreateUpload"
    "fixture/CreateUpload.yaml"

requestGetDeviceInstance :: GetDeviceInstance -> TestTree
requestGetDeviceInstance =
  req
    "GetDeviceInstance"
    "fixture/GetDeviceInstance.yaml"

requestStopJob :: StopJob -> TestTree
requestStopJob =
  req
    "StopJob"
    "fixture/StopJob.yaml"

requestDeleteRemoteAccessSession :: DeleteRemoteAccessSession -> TestTree
requestDeleteRemoteAccessSession =
  req
    "DeleteRemoteAccessSession"
    "fixture/DeleteRemoteAccessSession.yaml"

requestListTestGridSessionArtifacts :: ListTestGridSessionArtifacts -> TestTree
requestListTestGridSessionArtifacts =
  req
    "ListTestGridSessionArtifacts"
    "fixture/ListTestGridSessionArtifacts.yaml"

requestListTestGridProjects :: ListTestGridProjects -> TestTree
requestListTestGridProjects =
  req
    "ListTestGridProjects"
    "fixture/ListTestGridProjects.yaml"

requestDeleteUpload :: DeleteUpload -> TestTree
requestDeleteUpload =
  req
    "DeleteUpload"
    "fixture/DeleteUpload.yaml"

requestUpdateUpload :: UpdateUpload -> TestTree
requestUpdateUpload =
  req
    "UpdateUpload"
    "fixture/UpdateUpload.yaml"

requestDeleteTestGridProject :: DeleteTestGridProject -> TestTree
requestDeleteTestGridProject =
  req
    "DeleteTestGridProject"
    "fixture/DeleteTestGridProject.yaml"

requestUpdateTestGridProject :: UpdateTestGridProject -> TestTree
requestUpdateTestGridProject =
  req
    "UpdateTestGridProject"
    "fixture/UpdateTestGridProject.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetDevicePool :: GetDevicePool -> TestTree
requestGetDevicePool =
  req
    "GetDevicePool"
    "fixture/GetDevicePool.yaml"

requestListDevicePools :: ListDevicePools -> TestTree
requestListDevicePools =
  req
    "ListDevicePools"
    "fixture/ListDevicePools.yaml"

requestUpdateDevicePool :: UpdateDevicePool -> TestTree
requestUpdateDevicePool =
  req
    "UpdateDevicePool"
    "fixture/UpdateDevicePool.yaml"

requestDeleteDevicePool :: DeleteDevicePool -> TestTree
requestDeleteDevicePool =
  req
    "DeleteDevicePool"
    "fixture/DeleteDevicePool.yaml"

requestGetUpload :: GetUpload -> TestTree
requestGetUpload =
  req
    "GetUpload"
    "fixture/GetUpload.yaml"

requestListOfferingTransactions :: ListOfferingTransactions -> TestTree
requestListOfferingTransactions =
  req
    "ListOfferingTransactions"
    "fixture/ListOfferingTransactions.yaml"

requestCreateDevicePool :: CreateDevicePool -> TestTree
requestCreateDevicePool =
  req
    "CreateDevicePool"
    "fixture/CreateDevicePool.yaml"

requestDeleteRun :: DeleteRun -> TestTree
requestDeleteRun =
  req
    "DeleteRun"
    "fixture/DeleteRun.yaml"

requestListRuns :: ListRuns -> TestTree
requestListRuns =
  req
    "ListRuns"
    "fixture/ListRuns.yaml"

requestGetTest :: GetTest -> TestTree
requestGetTest =
  req
    "GetTest"
    "fixture/GetTest.yaml"

requestUpdateDeviceInstance :: UpdateDeviceInstance -> TestTree
requestUpdateDeviceInstance =
  req
    "UpdateDeviceInstance"
    "fixture/UpdateDeviceInstance.yaml"

requestGetNetworkProfile :: GetNetworkProfile -> TestTree
requestGetNetworkProfile =
  req
    "GetNetworkProfile"
    "fixture/GetNetworkProfile.yaml"

requestRenewOffering :: RenewOffering -> TestTree
requestRenewOffering =
  req
    "RenewOffering"
    "fixture/RenewOffering.yaml"

requestDeleteInstanceProfile :: DeleteInstanceProfile -> TestTree
requestDeleteInstanceProfile =
  req
    "DeleteInstanceProfile"
    "fixture/DeleteInstanceProfile.yaml"

requestUpdateInstanceProfile :: UpdateInstanceProfile -> TestTree
requestUpdateInstanceProfile =
  req
    "UpdateInstanceProfile"
    "fixture/UpdateInstanceProfile.yaml"

requestCreateInstanceProfile :: CreateInstanceProfile -> TestTree
requestCreateInstanceProfile =
  req
    "CreateInstanceProfile"
    "fixture/CreateInstanceProfile.yaml"

requestGetDevice :: GetDevice -> TestTree
requestGetDevice =
  req
    "GetDevice"
    "fixture/GetDevice.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestGetTestGridSession :: GetTestGridSession -> TestTree
requestGetTestGridSession =
  req
    "GetTestGridSession"
    "fixture/GetTestGridSession.yaml"

requestGetVPCEConfiguration :: GetVPCEConfiguration -> TestTree
requestGetVPCEConfiguration =
  req
    "GetVPCEConfiguration"
    "fixture/GetVPCEConfiguration.yaml"

requestStopRemoteAccessSession :: StopRemoteAccessSession -> TestTree
requestStopRemoteAccessSession =
  req
    "StopRemoteAccessSession"
    "fixture/StopRemoteAccessSession.yaml"

requestCreateNetworkProfile :: CreateNetworkProfile -> TestTree
requestCreateNetworkProfile =
  req
    "CreateNetworkProfile"
    "fixture/CreateNetworkProfile.yaml"

requestDeleteVPCEConfiguration :: DeleteVPCEConfiguration -> TestTree
requestDeleteVPCEConfiguration =
  req
    "DeleteVPCEConfiguration"
    "fixture/DeleteVPCEConfiguration.yaml"

requestUpdateVPCEConfiguration :: UpdateVPCEConfiguration -> TestTree
requestUpdateVPCEConfiguration =
  req
    "UpdateVPCEConfiguration"
    "fixture/UpdateVPCEConfiguration.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob =
  req
    "GetJob"
    "fixture/GetJob.yaml"

requestGetInstanceProfile :: GetInstanceProfile -> TestTree
requestGetInstanceProfile =
  req
    "GetInstanceProfile"
    "fixture/GetInstanceProfile.yaml"

requestListNetworkProfiles :: ListNetworkProfiles -> TestTree
requestListNetworkProfiles =
  req
    "ListNetworkProfiles"
    "fixture/ListNetworkProfiles.yaml"

requestCreateVPCEConfiguration :: CreateVPCEConfiguration -> TestTree
requestCreateVPCEConfiguration =
  req
    "CreateVPCEConfiguration"
    "fixture/CreateVPCEConfiguration.yaml"

requestScheduleRun :: ScheduleRun -> TestTree
requestScheduleRun =
  req
    "ScheduleRun"
    "fixture/ScheduleRun.yaml"

requestCreateTestGridProject :: CreateTestGridProject -> TestTree
requestCreateTestGridProject =
  req
    "CreateTestGridProject"
    "fixture/CreateTestGridProject.yaml"

requestGetRun :: GetRun -> TestTree
requestGetRun =
  req
    "GetRun"
    "fixture/GetRun.yaml"

requestListSamples :: ListSamples -> TestTree
requestListSamples =
  req
    "ListSamples"
    "fixture/ListSamples.yaml"

requestListSuites :: ListSuites -> TestTree
requestListSuites =
  req
    "ListSuites"
    "fixture/ListSuites.yaml"

requestListRemoteAccessSessions :: ListRemoteAccessSessions -> TestTree
requestListRemoteAccessSessions =
  req
    "ListRemoteAccessSessions"
    "fixture/ListRemoteAccessSessions.yaml"

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings =
  req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

requestCreateRemoteAccessSession :: CreateRemoteAccessSession -> TestTree
requestCreateRemoteAccessSession =
  req
    "CreateRemoteAccessSession"
    "fixture/CreateRemoteAccessSession.yaml"

requestListOfferingPromotions :: ListOfferingPromotions -> TestTree
requestListOfferingPromotions =
  req
    "ListOfferingPromotions"
    "fixture/ListOfferingPromotions.yaml"

requestGetOfferingStatus :: GetOfferingStatus -> TestTree
requestGetOfferingStatus =
  req
    "GetOfferingStatus"
    "fixture/GetOfferingStatus.yaml"

requestListUploads :: ListUploads -> TestTree
requestListUploads =
  req
    "ListUploads"
    "fixture/ListUploads.yaml"

requestGetTestGridProject :: GetTestGridProject -> TestTree
requestGetTestGridProject =
  req
    "GetTestGridProject"
    "fixture/GetTestGridProject.yaml"

requestGetSuite :: GetSuite -> TestTree
requestGetSuite =
  req
    "GetSuite"
    "fixture/GetSuite.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetRemoteAccessSession :: GetRemoteAccessSession -> TestTree
requestGetRemoteAccessSession =
  req
    "GetRemoteAccessSession"
    "fixture/GetRemoteAccessSession.yaml"

requestListDeviceInstances :: ListDeviceInstances -> TestTree
requestListDeviceInstances =
  req
    "ListDeviceInstances"
    "fixture/ListDeviceInstances.yaml"

requestPurchaseOffering :: PurchaseOffering -> TestTree
requestPurchaseOffering =
  req
    "PurchaseOffering"
    "fixture/PurchaseOffering.yaml"

requestListInstanceProfiles :: ListInstanceProfiles -> TestTree
requestListInstanceProfiles =
  req
    "ListInstanceProfiles"
    "fixture/ListInstanceProfiles.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetProject :: GetProject -> TestTree
requestGetProject =
  req
    "GetProject"
    "fixture/GetProject.yaml"

requestListUniqueProblems :: ListUniqueProblems -> TestTree
requestListUniqueProblems =
  req
    "ListUniqueProblems"
    "fixture/ListUniqueProblems.yaml"

requestListVPCEConfigurations :: ListVPCEConfigurations -> TestTree
requestListVPCEConfigurations =
  req
    "ListVPCEConfigurations"
    "fixture/ListVPCEConfigurations.yaml"

requestStopRun :: StopRun -> TestTree
requestStopRun =
  req
    "StopRun"
    "fixture/StopRun.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestListTestGridSessions :: ListTestGridSessions -> TestTree
requestListTestGridSessions =
  req
    "ListTestGridSessions"
    "fixture/ListTestGridSessions.yaml"

requestCreateTestGridURL :: CreateTestGridURL -> TestTree
requestCreateTestGridURL =
  req
    "CreateTestGridURL"
    "fixture/CreateTestGridURL.yaml"

requestListOfferings :: ListOfferings -> TestTree
requestListOfferings =
  req
    "ListOfferings"
    "fixture/ListOfferings.yaml"

-- Responses

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListProjects)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteProject)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    deviceFarm
    (Proxy :: Proxy UpdateProject)

responseUpdateNetworkProfile :: UpdateNetworkProfileResponse -> TestTree
responseUpdateNetworkProfile =
  res
    "UpdateNetworkProfileResponse"
    "fixture/UpdateNetworkProfileResponse.proto"
    deviceFarm
    (Proxy :: Proxy UpdateNetworkProfile)

responseDeleteNetworkProfile :: DeleteNetworkProfileResponse -> TestTree
responseDeleteNetworkProfile =
  res
    "DeleteNetworkProfileResponse"
    "fixture/DeleteNetworkProfileResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteNetworkProfile)

responseGetDevicePoolCompatibility :: GetDevicePoolCompatibilityResponse -> TestTree
responseGetDevicePoolCompatibility =
  res
    "GetDevicePoolCompatibilityResponse"
    "fixture/GetDevicePoolCompatibilityResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetDevicePoolCompatibility)

responseInstallToRemoteAccessSession :: InstallToRemoteAccessSessionResponse -> TestTree
responseInstallToRemoteAccessSession =
  res
    "InstallToRemoteAccessSessionResponse"
    "fixture/InstallToRemoteAccessSessionResponse.proto"
    deviceFarm
    (Proxy :: Proxy InstallToRemoteAccessSession)

responseListTests :: ListTestsResponse -> TestTree
responseListTests =
  res
    "ListTestsResponse"
    "fixture/ListTestsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListTests)

responseListArtifacts :: ListArtifactsResponse -> TestTree
responseListArtifacts =
  res
    "ListArtifactsResponse"
    "fixture/ListArtifactsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListArtifacts)

responseListTestGridSessionActions :: ListTestGridSessionActionsResponse -> TestTree
responseListTestGridSessionActions =
  res
    "ListTestGridSessionActionsResponse"
    "fixture/ListTestGridSessionActionsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListTestGridSessionActions)

responseCreateUpload :: CreateUploadResponse -> TestTree
responseCreateUpload =
  res
    "CreateUploadResponse"
    "fixture/CreateUploadResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateUpload)

responseGetDeviceInstance :: GetDeviceInstanceResponse -> TestTree
responseGetDeviceInstance =
  res
    "GetDeviceInstanceResponse"
    "fixture/GetDeviceInstanceResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetDeviceInstance)

responseStopJob :: StopJobResponse -> TestTree
responseStopJob =
  res
    "StopJobResponse"
    "fixture/StopJobResponse.proto"
    deviceFarm
    (Proxy :: Proxy StopJob)

responseDeleteRemoteAccessSession :: DeleteRemoteAccessSessionResponse -> TestTree
responseDeleteRemoteAccessSession =
  res
    "DeleteRemoteAccessSessionResponse"
    "fixture/DeleteRemoteAccessSessionResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteRemoteAccessSession)

responseListTestGridSessionArtifacts :: ListTestGridSessionArtifactsResponse -> TestTree
responseListTestGridSessionArtifacts =
  res
    "ListTestGridSessionArtifactsResponse"
    "fixture/ListTestGridSessionArtifactsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListTestGridSessionArtifacts)

responseListTestGridProjects :: ListTestGridProjectsResponse -> TestTree
responseListTestGridProjects =
  res
    "ListTestGridProjectsResponse"
    "fixture/ListTestGridProjectsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListTestGridProjects)

responseDeleteUpload :: DeleteUploadResponse -> TestTree
responseDeleteUpload =
  res
    "DeleteUploadResponse"
    "fixture/DeleteUploadResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteUpload)

responseUpdateUpload :: UpdateUploadResponse -> TestTree
responseUpdateUpload =
  res
    "UpdateUploadResponse"
    "fixture/UpdateUploadResponse.proto"
    deviceFarm
    (Proxy :: Proxy UpdateUpload)

responseDeleteTestGridProject :: DeleteTestGridProjectResponse -> TestTree
responseDeleteTestGridProject =
  res
    "DeleteTestGridProjectResponse"
    "fixture/DeleteTestGridProjectResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteTestGridProject)

responseUpdateTestGridProject :: UpdateTestGridProjectResponse -> TestTree
responseUpdateTestGridProject =
  res
    "UpdateTestGridProjectResponse"
    "fixture/UpdateTestGridProjectResponse.proto"
    deviceFarm
    (Proxy :: Proxy UpdateTestGridProject)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListTagsForResource)

responseGetDevicePool :: GetDevicePoolResponse -> TestTree
responseGetDevicePool =
  res
    "GetDevicePoolResponse"
    "fixture/GetDevicePoolResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetDevicePool)

responseListDevicePools :: ListDevicePoolsResponse -> TestTree
responseListDevicePools =
  res
    "ListDevicePoolsResponse"
    "fixture/ListDevicePoolsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListDevicePools)

responseUpdateDevicePool :: UpdateDevicePoolResponse -> TestTree
responseUpdateDevicePool =
  res
    "UpdateDevicePoolResponse"
    "fixture/UpdateDevicePoolResponse.proto"
    deviceFarm
    (Proxy :: Proxy UpdateDevicePool)

responseDeleteDevicePool :: DeleteDevicePoolResponse -> TestTree
responseDeleteDevicePool =
  res
    "DeleteDevicePoolResponse"
    "fixture/DeleteDevicePoolResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteDevicePool)

responseGetUpload :: GetUploadResponse -> TestTree
responseGetUpload =
  res
    "GetUploadResponse"
    "fixture/GetUploadResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetUpload)

responseListOfferingTransactions :: ListOfferingTransactionsResponse -> TestTree
responseListOfferingTransactions =
  res
    "ListOfferingTransactionsResponse"
    "fixture/ListOfferingTransactionsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListOfferingTransactions)

responseCreateDevicePool :: CreateDevicePoolResponse -> TestTree
responseCreateDevicePool =
  res
    "CreateDevicePoolResponse"
    "fixture/CreateDevicePoolResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateDevicePool)

responseDeleteRun :: DeleteRunResponse -> TestTree
responseDeleteRun =
  res
    "DeleteRunResponse"
    "fixture/DeleteRunResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteRun)

responseListRuns :: ListRunsResponse -> TestTree
responseListRuns =
  res
    "ListRunsResponse"
    "fixture/ListRunsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListRuns)

responseGetTest :: GetTestResponse -> TestTree
responseGetTest =
  res
    "GetTestResponse"
    "fixture/GetTestResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetTest)

responseUpdateDeviceInstance :: UpdateDeviceInstanceResponse -> TestTree
responseUpdateDeviceInstance =
  res
    "UpdateDeviceInstanceResponse"
    "fixture/UpdateDeviceInstanceResponse.proto"
    deviceFarm
    (Proxy :: Proxy UpdateDeviceInstance)

responseGetNetworkProfile :: GetNetworkProfileResponse -> TestTree
responseGetNetworkProfile =
  res
    "GetNetworkProfileResponse"
    "fixture/GetNetworkProfileResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetNetworkProfile)

responseRenewOffering :: RenewOfferingResponse -> TestTree
responseRenewOffering =
  res
    "RenewOfferingResponse"
    "fixture/RenewOfferingResponse.proto"
    deviceFarm
    (Proxy :: Proxy RenewOffering)

responseDeleteInstanceProfile :: DeleteInstanceProfileResponse -> TestTree
responseDeleteInstanceProfile =
  res
    "DeleteInstanceProfileResponse"
    "fixture/DeleteInstanceProfileResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteInstanceProfile)

responseUpdateInstanceProfile :: UpdateInstanceProfileResponse -> TestTree
responseUpdateInstanceProfile =
  res
    "UpdateInstanceProfileResponse"
    "fixture/UpdateInstanceProfileResponse.proto"
    deviceFarm
    (Proxy :: Proxy UpdateInstanceProfile)

responseCreateInstanceProfile :: CreateInstanceProfileResponse -> TestTree
responseCreateInstanceProfile =
  res
    "CreateInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateInstanceProfile)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice =
  res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetDevice)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListJobs)

responseGetTestGridSession :: GetTestGridSessionResponse -> TestTree
responseGetTestGridSession =
  res
    "GetTestGridSessionResponse"
    "fixture/GetTestGridSessionResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetTestGridSession)

responseGetVPCEConfiguration :: GetVPCEConfigurationResponse -> TestTree
responseGetVPCEConfiguration =
  res
    "GetVPCEConfigurationResponse"
    "fixture/GetVPCEConfigurationResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetVPCEConfiguration)

responseStopRemoteAccessSession :: StopRemoteAccessSessionResponse -> TestTree
responseStopRemoteAccessSession =
  res
    "StopRemoteAccessSessionResponse"
    "fixture/StopRemoteAccessSessionResponse.proto"
    deviceFarm
    (Proxy :: Proxy StopRemoteAccessSession)

responseCreateNetworkProfile :: CreateNetworkProfileResponse -> TestTree
responseCreateNetworkProfile =
  res
    "CreateNetworkProfileResponse"
    "fixture/CreateNetworkProfileResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateNetworkProfile)

responseDeleteVPCEConfiguration :: DeleteVPCEConfigurationResponse -> TestTree
responseDeleteVPCEConfiguration =
  res
    "DeleteVPCEConfigurationResponse"
    "fixture/DeleteVPCEConfigurationResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteVPCEConfiguration)

responseUpdateVPCEConfiguration :: UpdateVPCEConfigurationResponse -> TestTree
responseUpdateVPCEConfiguration =
  res
    "UpdateVPCEConfigurationResponse"
    "fixture/UpdateVPCEConfigurationResponse.proto"
    deviceFarm
    (Proxy :: Proxy UpdateVPCEConfiguration)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetJob)

responseGetInstanceProfile :: GetInstanceProfileResponse -> TestTree
responseGetInstanceProfile =
  res
    "GetInstanceProfileResponse"
    "fixture/GetInstanceProfileResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetInstanceProfile)

responseListNetworkProfiles :: ListNetworkProfilesResponse -> TestTree
responseListNetworkProfiles =
  res
    "ListNetworkProfilesResponse"
    "fixture/ListNetworkProfilesResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListNetworkProfiles)

responseCreateVPCEConfiguration :: CreateVPCEConfigurationResponse -> TestTree
responseCreateVPCEConfiguration =
  res
    "CreateVPCEConfigurationResponse"
    "fixture/CreateVPCEConfigurationResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateVPCEConfiguration)

responseScheduleRun :: ScheduleRunResponse -> TestTree
responseScheduleRun =
  res
    "ScheduleRunResponse"
    "fixture/ScheduleRunResponse.proto"
    deviceFarm
    (Proxy :: Proxy ScheduleRun)

responseCreateTestGridProject :: CreateTestGridProjectResponse -> TestTree
responseCreateTestGridProject =
  res
    "CreateTestGridProjectResponse"
    "fixture/CreateTestGridProjectResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateTestGridProject)

responseGetRun :: GetRunResponse -> TestTree
responseGetRun =
  res
    "GetRunResponse"
    "fixture/GetRunResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetRun)

responseListSamples :: ListSamplesResponse -> TestTree
responseListSamples =
  res
    "ListSamplesResponse"
    "fixture/ListSamplesResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListSamples)

responseListSuites :: ListSuitesResponse -> TestTree
responseListSuites =
  res
    "ListSuitesResponse"
    "fixture/ListSuitesResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListSuites)

responseListRemoteAccessSessions :: ListRemoteAccessSessionsResponse -> TestTree
responseListRemoteAccessSessions =
  res
    "ListRemoteAccessSessionsResponse"
    "fixture/ListRemoteAccessSessionsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListRemoteAccessSessions)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetAccountSettings)

responseCreateRemoteAccessSession :: CreateRemoteAccessSessionResponse -> TestTree
responseCreateRemoteAccessSession =
  res
    "CreateRemoteAccessSessionResponse"
    "fixture/CreateRemoteAccessSessionResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateRemoteAccessSession)

responseListOfferingPromotions :: ListOfferingPromotionsResponse -> TestTree
responseListOfferingPromotions =
  res
    "ListOfferingPromotionsResponse"
    "fixture/ListOfferingPromotionsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListOfferingPromotions)

responseGetOfferingStatus :: GetOfferingStatusResponse -> TestTree
responseGetOfferingStatus =
  res
    "GetOfferingStatusResponse"
    "fixture/GetOfferingStatusResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetOfferingStatus)

responseListUploads :: ListUploadsResponse -> TestTree
responseListUploads =
  res
    "ListUploadsResponse"
    "fixture/ListUploadsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListUploads)

responseGetTestGridProject :: GetTestGridProjectResponse -> TestTree
responseGetTestGridProject =
  res
    "GetTestGridProjectResponse"
    "fixture/GetTestGridProjectResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetTestGridProject)

responseGetSuite :: GetSuiteResponse -> TestTree
responseGetSuite =
  res
    "GetSuiteResponse"
    "fixture/GetSuiteResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetSuite)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    deviceFarm
    (Proxy :: Proxy TagResource)

responseGetRemoteAccessSession :: GetRemoteAccessSessionResponse -> TestTree
responseGetRemoteAccessSession =
  res
    "GetRemoteAccessSessionResponse"
    "fixture/GetRemoteAccessSessionResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetRemoteAccessSession)

responseListDeviceInstances :: ListDeviceInstancesResponse -> TestTree
responseListDeviceInstances =
  res
    "ListDeviceInstancesResponse"
    "fixture/ListDeviceInstancesResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListDeviceInstances)

responsePurchaseOffering :: PurchaseOfferingResponse -> TestTree
responsePurchaseOffering =
  res
    "PurchaseOfferingResponse"
    "fixture/PurchaseOfferingResponse.proto"
    deviceFarm
    (Proxy :: Proxy PurchaseOffering)

responseListInstanceProfiles :: ListInstanceProfilesResponse -> TestTree
responseListInstanceProfiles =
  res
    "ListInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListInstanceProfiles)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    deviceFarm
    (Proxy :: Proxy UntagResource)

responseGetProject :: GetProjectResponse -> TestTree
responseGetProject =
  res
    "GetProjectResponse"
    "fixture/GetProjectResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetProject)

responseListUniqueProblems :: ListUniqueProblemsResponse -> TestTree
responseListUniqueProblems =
  res
    "ListUniqueProblemsResponse"
    "fixture/ListUniqueProblemsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListUniqueProblems)

responseListVPCEConfigurations :: ListVPCEConfigurationsResponse -> TestTree
responseListVPCEConfigurations =
  res
    "ListVPCEConfigurationsResponse"
    "fixture/ListVPCEConfigurationsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListVPCEConfigurations)

responseStopRun :: StopRunResponse -> TestTree
responseStopRun =
  res
    "StopRunResponse"
    "fixture/StopRunResponse.proto"
    deviceFarm
    (Proxy :: Proxy StopRun)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListDevices)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateProject)

responseListTestGridSessions :: ListTestGridSessionsResponse -> TestTree
responseListTestGridSessions =
  res
    "ListTestGridSessionsResponse"
    "fixture/ListTestGridSessionsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListTestGridSessions)

responseCreateTestGridURL :: CreateTestGridURLResponse -> TestTree
responseCreateTestGridURL =
  res
    "CreateTestGridURLResponse"
    "fixture/CreateTestGridURLResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateTestGridURL)

responseListOfferings :: ListOfferingsResponse -> TestTree
responseListOfferings =
  res
    "ListOfferingsResponse"
    "fixture/ListOfferingsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListOfferings)
