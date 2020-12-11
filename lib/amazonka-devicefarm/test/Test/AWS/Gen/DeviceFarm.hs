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
--             mkListProjects
--
--         , requestDeleteProject $
--             mkDeleteProject
--
--         , requestUpdateProject $
--             mkUpdateProject
--
--         , requestUpdateNetworkProfile $
--             mkUpdateNetworkProfile
--
--         , requestDeleteNetworkProfile $
--             mkDeleteNetworkProfile
--
--         , requestGetDevicePoolCompatibility $
--             mkGetDevicePoolCompatibility
--
--         , requestInstallToRemoteAccessSession $
--             mkInstallToRemoteAccessSession
--
--         , requestListTests $
--             mkListTests
--
--         , requestListArtifacts $
--             mkListArtifacts
--
--         , requestListTestGridSessionActions $
--             mkListTestGridSessionActions
--
--         , requestCreateUpload $
--             mkCreateUpload
--
--         , requestGetDeviceInstance $
--             mkGetDeviceInstance
--
--         , requestStopJob $
--             mkStopJob
--
--         , requestDeleteRemoteAccessSession $
--             mkDeleteRemoteAccessSession
--
--         , requestListTestGridSessionArtifacts $
--             mkListTestGridSessionArtifacts
--
--         , requestListTestGridProjects $
--             mkListTestGridProjects
--
--         , requestDeleteUpload $
--             mkDeleteUpload
--
--         , requestUpdateUpload $
--             mkUpdateUpload
--
--         , requestDeleteTestGridProject $
--             mkDeleteTestGridProject
--
--         , requestUpdateTestGridProject $
--             mkUpdateTestGridProject
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestGetDevicePool $
--             mkGetDevicePool
--
--         , requestListDevicePools $
--             mkListDevicePools
--
--         , requestUpdateDevicePool $
--             mkUpdateDevicePool
--
--         , requestDeleteDevicePool $
--             mkDeleteDevicePool
--
--         , requestGetUpload $
--             mkGetUpload
--
--         , requestListOfferingTransactions $
--             mkListOfferingTransactions
--
--         , requestCreateDevicePool $
--             mkCreateDevicePool
--
--         , requestDeleteRun $
--             mkDeleteRun
--
--         , requestListRuns $
--             mkListRuns
--
--         , requestGetTest $
--             mkGetTest
--
--         , requestUpdateDeviceInstance $
--             mkUpdateDeviceInstance
--
--         , requestGetNetworkProfile $
--             mkGetNetworkProfile
--
--         , requestRenewOffering $
--             mkRenewOffering
--
--         , requestDeleteInstanceProfile $
--             mkDeleteInstanceProfile
--
--         , requestUpdateInstanceProfile $
--             mkUpdateInstanceProfile
--
--         , requestCreateInstanceProfile $
--             mkCreateInstanceProfile
--
--         , requestGetDevice $
--             mkGetDevice
--
--         , requestListJobs $
--             mkListJobs
--
--         , requestGetTestGridSession $
--             mkGetTestGridSession
--
--         , requestGetVPCEConfiguration $
--             mkGetVPCEConfiguration
--
--         , requestStopRemoteAccessSession $
--             mkStopRemoteAccessSession
--
--         , requestCreateNetworkProfile $
--             mkCreateNetworkProfile
--
--         , requestDeleteVPCEConfiguration $
--             mkDeleteVPCEConfiguration
--
--         , requestUpdateVPCEConfiguration $
--             mkUpdateVPCEConfiguration
--
--         , requestGetJob $
--             mkGetJob
--
--         , requestGetInstanceProfile $
--             mkGetInstanceProfile
--
--         , requestListNetworkProfiles $
--             mkListNetworkProfiles
--
--         , requestCreateVPCEConfiguration $
--             mkCreateVPCEConfiguration
--
--         , requestScheduleRun $
--             mkScheduleRun
--
--         , requestCreateTestGridProject $
--             mkCreateTestGridProject
--
--         , requestGetRun $
--             mkGetRun
--
--         , requestListSamples $
--             mkListSamples
--
--         , requestListSuites $
--             mkListSuites
--
--         , requestListRemoteAccessSessions $
--             mkListRemoteAccessSessions
--
--         , requestGetAccountSettings $
--             mkGetAccountSettings
--
--         , requestCreateRemoteAccessSession $
--             mkCreateRemoteAccessSession
--
--         , requestListOfferingPromotions $
--             mkListOfferingPromotions
--
--         , requestGetOfferingStatus $
--             mkGetOfferingStatus
--
--         , requestListUploads $
--             mkListUploads
--
--         , requestGetTestGridProject $
--             mkGetTestGridProject
--
--         , requestGetSuite $
--             mkGetSuite
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestGetRemoteAccessSession $
--             mkGetRemoteAccessSession
--
--         , requestListDeviceInstances $
--             mkListDeviceInstances
--
--         , requestPurchaseOffering $
--             mkPurchaseOffering
--
--         , requestListInstanceProfiles $
--             mkListInstanceProfiles
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestGetProject $
--             mkGetProject
--
--         , requestListUniqueProblems $
--             mkListUniqueProblems
--
--         , requestListVPCEConfigurations $
--             mkListVPCEConfigurations
--
--         , requestStopRun $
--             mkStopRun
--
--         , requestListDevices $
--             mkListDevices
--
--         , requestCreateProject $
--             mkCreateProject
--
--         , requestListTestGridSessions $
--             mkListTestGridSessions
--
--         , requestCreateTestGridURL $
--             mkCreateTestGridURL
--
--         , requestListOfferings $
--             mkListOfferings
--
--           ]

--     , testGroup "response"
--         [ responseListProjects $
--             mkListProjectsResponse
--
--         , responseDeleteProject $
--             mkDeleteProjectResponse
--
--         , responseUpdateProject $
--             mkUpdateProjectResponse
--
--         , responseUpdateNetworkProfile $
--             mkUpdateNetworkProfileResponse
--
--         , responseDeleteNetworkProfile $
--             mkDeleteNetworkProfileResponse
--
--         , responseGetDevicePoolCompatibility $
--             mkGetDevicePoolCompatibilityResponse
--
--         , responseInstallToRemoteAccessSession $
--             mkInstallToRemoteAccessSessionResponse
--
--         , responseListTests $
--             mkListTestsResponse
--
--         , responseListArtifacts $
--             mkListArtifactsResponse
--
--         , responseListTestGridSessionActions $
--             mkListTestGridSessionActionsResponse
--
--         , responseCreateUpload $
--             mkCreateUploadResponse
--
--         , responseGetDeviceInstance $
--             mkGetDeviceInstanceResponse
--
--         , responseStopJob $
--             mkStopJobResponse
--
--         , responseDeleteRemoteAccessSession $
--             mkDeleteRemoteAccessSessionResponse
--
--         , responseListTestGridSessionArtifacts $
--             mkListTestGridSessionArtifactsResponse
--
--         , responseListTestGridProjects $
--             mkListTestGridProjectsResponse
--
--         , responseDeleteUpload $
--             mkDeleteUploadResponse
--
--         , responseUpdateUpload $
--             mkUpdateUploadResponse
--
--         , responseDeleteTestGridProject $
--             mkDeleteTestGridProjectResponse
--
--         , responseUpdateTestGridProject $
--             mkUpdateTestGridProjectResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseGetDevicePool $
--             mkGetDevicePoolResponse
--
--         , responseListDevicePools $
--             mkListDevicePoolsResponse
--
--         , responseUpdateDevicePool $
--             mkUpdateDevicePoolResponse
--
--         , responseDeleteDevicePool $
--             mkDeleteDevicePoolResponse
--
--         , responseGetUpload $
--             mkGetUploadResponse
--
--         , responseListOfferingTransactions $
--             mkListOfferingTransactionsResponse
--
--         , responseCreateDevicePool $
--             mkCreateDevicePoolResponse
--
--         , responseDeleteRun $
--             mkDeleteRunResponse
--
--         , responseListRuns $
--             mkListRunsResponse
--
--         , responseGetTest $
--             mkGetTestResponse
--
--         , responseUpdateDeviceInstance $
--             mkUpdateDeviceInstanceResponse
--
--         , responseGetNetworkProfile $
--             mkGetNetworkProfileResponse
--
--         , responseRenewOffering $
--             mkRenewOfferingResponse
--
--         , responseDeleteInstanceProfile $
--             mkDeleteInstanceProfileResponse
--
--         , responseUpdateInstanceProfile $
--             mkUpdateInstanceProfileResponse
--
--         , responseCreateInstanceProfile $
--             mkCreateInstanceProfileResponse
--
--         , responseGetDevice $
--             mkGetDeviceResponse
--
--         , responseListJobs $
--             mkListJobsResponse
--
--         , responseGetTestGridSession $
--             mkGetTestGridSessionResponse
--
--         , responseGetVPCEConfiguration $
--             mkGetVPCEConfigurationResponse
--
--         , responseStopRemoteAccessSession $
--             mkStopRemoteAccessSessionResponse
--
--         , responseCreateNetworkProfile $
--             mkCreateNetworkProfileResponse
--
--         , responseDeleteVPCEConfiguration $
--             mkDeleteVPCEConfigurationResponse
--
--         , responseUpdateVPCEConfiguration $
--             mkUpdateVPCEConfigurationResponse
--
--         , responseGetJob $
--             mkGetJobResponse
--
--         , responseGetInstanceProfile $
--             mkGetInstanceProfileResponse
--
--         , responseListNetworkProfiles $
--             mkListNetworkProfilesResponse
--
--         , responseCreateVPCEConfiguration $
--             mkCreateVPCEConfigurationResponse
--
--         , responseScheduleRun $
--             mkScheduleRunResponse
--
--         , responseCreateTestGridProject $
--             mkCreateTestGridProjectResponse
--
--         , responseGetRun $
--             mkGetRunResponse
--
--         , responseListSamples $
--             mkListSamplesResponse
--
--         , responseListSuites $
--             mkListSuitesResponse
--
--         , responseListRemoteAccessSessions $
--             mkListRemoteAccessSessionsResponse
--
--         , responseGetAccountSettings $
--             mkGetAccountSettingsResponse
--
--         , responseCreateRemoteAccessSession $
--             mkCreateRemoteAccessSessionResponse
--
--         , responseListOfferingPromotions $
--             mkListOfferingPromotionsResponse
--
--         , responseGetOfferingStatus $
--             mkGetOfferingStatusResponse
--
--         , responseListUploads $
--             mkListUploadsResponse
--
--         , responseGetTestGridProject $
--             mkGetTestGridProjectResponse
--
--         , responseGetSuite $
--             mkGetSuiteResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseGetRemoteAccessSession $
--             mkGetRemoteAccessSessionResponse
--
--         , responseListDeviceInstances $
--             mkListDeviceInstancesResponse
--
--         , responsePurchaseOffering $
--             mkPurchaseOfferingResponse
--
--         , responseListInstanceProfiles $
--             mkListInstanceProfilesResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseGetProject $
--             mkGetProjectResponse
--
--         , responseListUniqueProblems $
--             mkListUniqueProblemsResponse
--
--         , responseListVPCEConfigurations $
--             mkListVPCEConfigurationsResponse
--
--         , responseStopRun $
--             mkStopRunResponse
--
--         , responseListDevices $
--             mkListDevicesResponse
--
--         , responseCreateProject $
--             mkCreateProjectResponse
--
--         , responseListTestGridSessions $
--             mkListTestGridSessionsResponse
--
--         , responseCreateTestGridURL $
--             mkCreateTestGridURLResponse
--
--         , responseListOfferings $
--             mkListOfferingsResponse
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
    deviceFarmService
    (Proxy :: Proxy ListProjects)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    deviceFarmService
    (Proxy :: Proxy DeleteProject)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    deviceFarmService
    (Proxy :: Proxy UpdateProject)

responseUpdateNetworkProfile :: UpdateNetworkProfileResponse -> TestTree
responseUpdateNetworkProfile =
  res
    "UpdateNetworkProfileResponse"
    "fixture/UpdateNetworkProfileResponse.proto"
    deviceFarmService
    (Proxy :: Proxy UpdateNetworkProfile)

responseDeleteNetworkProfile :: DeleteNetworkProfileResponse -> TestTree
responseDeleteNetworkProfile =
  res
    "DeleteNetworkProfileResponse"
    "fixture/DeleteNetworkProfileResponse.proto"
    deviceFarmService
    (Proxy :: Proxy DeleteNetworkProfile)

responseGetDevicePoolCompatibility :: GetDevicePoolCompatibilityResponse -> TestTree
responseGetDevicePoolCompatibility =
  res
    "GetDevicePoolCompatibilityResponse"
    "fixture/GetDevicePoolCompatibilityResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetDevicePoolCompatibility)

responseInstallToRemoteAccessSession :: InstallToRemoteAccessSessionResponse -> TestTree
responseInstallToRemoteAccessSession =
  res
    "InstallToRemoteAccessSessionResponse"
    "fixture/InstallToRemoteAccessSessionResponse.proto"
    deviceFarmService
    (Proxy :: Proxy InstallToRemoteAccessSession)

responseListTests :: ListTestsResponse -> TestTree
responseListTests =
  res
    "ListTestsResponse"
    "fixture/ListTestsResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListTests)

responseListArtifacts :: ListArtifactsResponse -> TestTree
responseListArtifacts =
  res
    "ListArtifactsResponse"
    "fixture/ListArtifactsResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListArtifacts)

responseListTestGridSessionActions :: ListTestGridSessionActionsResponse -> TestTree
responseListTestGridSessionActions =
  res
    "ListTestGridSessionActionsResponse"
    "fixture/ListTestGridSessionActionsResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListTestGridSessionActions)

responseCreateUpload :: CreateUploadResponse -> TestTree
responseCreateUpload =
  res
    "CreateUploadResponse"
    "fixture/CreateUploadResponse.proto"
    deviceFarmService
    (Proxy :: Proxy CreateUpload)

responseGetDeviceInstance :: GetDeviceInstanceResponse -> TestTree
responseGetDeviceInstance =
  res
    "GetDeviceInstanceResponse"
    "fixture/GetDeviceInstanceResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetDeviceInstance)

responseStopJob :: StopJobResponse -> TestTree
responseStopJob =
  res
    "StopJobResponse"
    "fixture/StopJobResponse.proto"
    deviceFarmService
    (Proxy :: Proxy StopJob)

responseDeleteRemoteAccessSession :: DeleteRemoteAccessSessionResponse -> TestTree
responseDeleteRemoteAccessSession =
  res
    "DeleteRemoteAccessSessionResponse"
    "fixture/DeleteRemoteAccessSessionResponse.proto"
    deviceFarmService
    (Proxy :: Proxy DeleteRemoteAccessSession)

responseListTestGridSessionArtifacts :: ListTestGridSessionArtifactsResponse -> TestTree
responseListTestGridSessionArtifacts =
  res
    "ListTestGridSessionArtifactsResponse"
    "fixture/ListTestGridSessionArtifactsResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListTestGridSessionArtifacts)

responseListTestGridProjects :: ListTestGridProjectsResponse -> TestTree
responseListTestGridProjects =
  res
    "ListTestGridProjectsResponse"
    "fixture/ListTestGridProjectsResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListTestGridProjects)

responseDeleteUpload :: DeleteUploadResponse -> TestTree
responseDeleteUpload =
  res
    "DeleteUploadResponse"
    "fixture/DeleteUploadResponse.proto"
    deviceFarmService
    (Proxy :: Proxy DeleteUpload)

responseUpdateUpload :: UpdateUploadResponse -> TestTree
responseUpdateUpload =
  res
    "UpdateUploadResponse"
    "fixture/UpdateUploadResponse.proto"
    deviceFarmService
    (Proxy :: Proxy UpdateUpload)

responseDeleteTestGridProject :: DeleteTestGridProjectResponse -> TestTree
responseDeleteTestGridProject =
  res
    "DeleteTestGridProjectResponse"
    "fixture/DeleteTestGridProjectResponse.proto"
    deviceFarmService
    (Proxy :: Proxy DeleteTestGridProject)

responseUpdateTestGridProject :: UpdateTestGridProjectResponse -> TestTree
responseUpdateTestGridProject =
  res
    "UpdateTestGridProjectResponse"
    "fixture/UpdateTestGridProjectResponse.proto"
    deviceFarmService
    (Proxy :: Proxy UpdateTestGridProject)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListTagsForResource)

responseGetDevicePool :: GetDevicePoolResponse -> TestTree
responseGetDevicePool =
  res
    "GetDevicePoolResponse"
    "fixture/GetDevicePoolResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetDevicePool)

responseListDevicePools :: ListDevicePoolsResponse -> TestTree
responseListDevicePools =
  res
    "ListDevicePoolsResponse"
    "fixture/ListDevicePoolsResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListDevicePools)

responseUpdateDevicePool :: UpdateDevicePoolResponse -> TestTree
responseUpdateDevicePool =
  res
    "UpdateDevicePoolResponse"
    "fixture/UpdateDevicePoolResponse.proto"
    deviceFarmService
    (Proxy :: Proxy UpdateDevicePool)

responseDeleteDevicePool :: DeleteDevicePoolResponse -> TestTree
responseDeleteDevicePool =
  res
    "DeleteDevicePoolResponse"
    "fixture/DeleteDevicePoolResponse.proto"
    deviceFarmService
    (Proxy :: Proxy DeleteDevicePool)

responseGetUpload :: GetUploadResponse -> TestTree
responseGetUpload =
  res
    "GetUploadResponse"
    "fixture/GetUploadResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetUpload)

responseListOfferingTransactions :: ListOfferingTransactionsResponse -> TestTree
responseListOfferingTransactions =
  res
    "ListOfferingTransactionsResponse"
    "fixture/ListOfferingTransactionsResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListOfferingTransactions)

responseCreateDevicePool :: CreateDevicePoolResponse -> TestTree
responseCreateDevicePool =
  res
    "CreateDevicePoolResponse"
    "fixture/CreateDevicePoolResponse.proto"
    deviceFarmService
    (Proxy :: Proxy CreateDevicePool)

responseDeleteRun :: DeleteRunResponse -> TestTree
responseDeleteRun =
  res
    "DeleteRunResponse"
    "fixture/DeleteRunResponse.proto"
    deviceFarmService
    (Proxy :: Proxy DeleteRun)

responseListRuns :: ListRunsResponse -> TestTree
responseListRuns =
  res
    "ListRunsResponse"
    "fixture/ListRunsResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListRuns)

responseGetTest :: GetTestResponse -> TestTree
responseGetTest =
  res
    "GetTestResponse"
    "fixture/GetTestResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetTest)

responseUpdateDeviceInstance :: UpdateDeviceInstanceResponse -> TestTree
responseUpdateDeviceInstance =
  res
    "UpdateDeviceInstanceResponse"
    "fixture/UpdateDeviceInstanceResponse.proto"
    deviceFarmService
    (Proxy :: Proxy UpdateDeviceInstance)

responseGetNetworkProfile :: GetNetworkProfileResponse -> TestTree
responseGetNetworkProfile =
  res
    "GetNetworkProfileResponse"
    "fixture/GetNetworkProfileResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetNetworkProfile)

responseRenewOffering :: RenewOfferingResponse -> TestTree
responseRenewOffering =
  res
    "RenewOfferingResponse"
    "fixture/RenewOfferingResponse.proto"
    deviceFarmService
    (Proxy :: Proxy RenewOffering)

responseDeleteInstanceProfile :: DeleteInstanceProfileResponse -> TestTree
responseDeleteInstanceProfile =
  res
    "DeleteInstanceProfileResponse"
    "fixture/DeleteInstanceProfileResponse.proto"
    deviceFarmService
    (Proxy :: Proxy DeleteInstanceProfile)

responseUpdateInstanceProfile :: UpdateInstanceProfileResponse -> TestTree
responseUpdateInstanceProfile =
  res
    "UpdateInstanceProfileResponse"
    "fixture/UpdateInstanceProfileResponse.proto"
    deviceFarmService
    (Proxy :: Proxy UpdateInstanceProfile)

responseCreateInstanceProfile :: CreateInstanceProfileResponse -> TestTree
responseCreateInstanceProfile =
  res
    "CreateInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse.proto"
    deviceFarmService
    (Proxy :: Proxy CreateInstanceProfile)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice =
  res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetDevice)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListJobs)

responseGetTestGridSession :: GetTestGridSessionResponse -> TestTree
responseGetTestGridSession =
  res
    "GetTestGridSessionResponse"
    "fixture/GetTestGridSessionResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetTestGridSession)

responseGetVPCEConfiguration :: GetVPCEConfigurationResponse -> TestTree
responseGetVPCEConfiguration =
  res
    "GetVPCEConfigurationResponse"
    "fixture/GetVPCEConfigurationResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetVPCEConfiguration)

responseStopRemoteAccessSession :: StopRemoteAccessSessionResponse -> TestTree
responseStopRemoteAccessSession =
  res
    "StopRemoteAccessSessionResponse"
    "fixture/StopRemoteAccessSessionResponse.proto"
    deviceFarmService
    (Proxy :: Proxy StopRemoteAccessSession)

responseCreateNetworkProfile :: CreateNetworkProfileResponse -> TestTree
responseCreateNetworkProfile =
  res
    "CreateNetworkProfileResponse"
    "fixture/CreateNetworkProfileResponse.proto"
    deviceFarmService
    (Proxy :: Proxy CreateNetworkProfile)

responseDeleteVPCEConfiguration :: DeleteVPCEConfigurationResponse -> TestTree
responseDeleteVPCEConfiguration =
  res
    "DeleteVPCEConfigurationResponse"
    "fixture/DeleteVPCEConfigurationResponse.proto"
    deviceFarmService
    (Proxy :: Proxy DeleteVPCEConfiguration)

responseUpdateVPCEConfiguration :: UpdateVPCEConfigurationResponse -> TestTree
responseUpdateVPCEConfiguration =
  res
    "UpdateVPCEConfigurationResponse"
    "fixture/UpdateVPCEConfigurationResponse.proto"
    deviceFarmService
    (Proxy :: Proxy UpdateVPCEConfiguration)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetJob)

responseGetInstanceProfile :: GetInstanceProfileResponse -> TestTree
responseGetInstanceProfile =
  res
    "GetInstanceProfileResponse"
    "fixture/GetInstanceProfileResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetInstanceProfile)

responseListNetworkProfiles :: ListNetworkProfilesResponse -> TestTree
responseListNetworkProfiles =
  res
    "ListNetworkProfilesResponse"
    "fixture/ListNetworkProfilesResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListNetworkProfiles)

responseCreateVPCEConfiguration :: CreateVPCEConfigurationResponse -> TestTree
responseCreateVPCEConfiguration =
  res
    "CreateVPCEConfigurationResponse"
    "fixture/CreateVPCEConfigurationResponse.proto"
    deviceFarmService
    (Proxy :: Proxy CreateVPCEConfiguration)

responseScheduleRun :: ScheduleRunResponse -> TestTree
responseScheduleRun =
  res
    "ScheduleRunResponse"
    "fixture/ScheduleRunResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ScheduleRun)

responseCreateTestGridProject :: CreateTestGridProjectResponse -> TestTree
responseCreateTestGridProject =
  res
    "CreateTestGridProjectResponse"
    "fixture/CreateTestGridProjectResponse.proto"
    deviceFarmService
    (Proxy :: Proxy CreateTestGridProject)

responseGetRun :: GetRunResponse -> TestTree
responseGetRun =
  res
    "GetRunResponse"
    "fixture/GetRunResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetRun)

responseListSamples :: ListSamplesResponse -> TestTree
responseListSamples =
  res
    "ListSamplesResponse"
    "fixture/ListSamplesResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListSamples)

responseListSuites :: ListSuitesResponse -> TestTree
responseListSuites =
  res
    "ListSuitesResponse"
    "fixture/ListSuitesResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListSuites)

responseListRemoteAccessSessions :: ListRemoteAccessSessionsResponse -> TestTree
responseListRemoteAccessSessions =
  res
    "ListRemoteAccessSessionsResponse"
    "fixture/ListRemoteAccessSessionsResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListRemoteAccessSessions)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetAccountSettings)

responseCreateRemoteAccessSession :: CreateRemoteAccessSessionResponse -> TestTree
responseCreateRemoteAccessSession =
  res
    "CreateRemoteAccessSessionResponse"
    "fixture/CreateRemoteAccessSessionResponse.proto"
    deviceFarmService
    (Proxy :: Proxy CreateRemoteAccessSession)

responseListOfferingPromotions :: ListOfferingPromotionsResponse -> TestTree
responseListOfferingPromotions =
  res
    "ListOfferingPromotionsResponse"
    "fixture/ListOfferingPromotionsResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListOfferingPromotions)

responseGetOfferingStatus :: GetOfferingStatusResponse -> TestTree
responseGetOfferingStatus =
  res
    "GetOfferingStatusResponse"
    "fixture/GetOfferingStatusResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetOfferingStatus)

responseListUploads :: ListUploadsResponse -> TestTree
responseListUploads =
  res
    "ListUploadsResponse"
    "fixture/ListUploadsResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListUploads)

responseGetTestGridProject :: GetTestGridProjectResponse -> TestTree
responseGetTestGridProject =
  res
    "GetTestGridProjectResponse"
    "fixture/GetTestGridProjectResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetTestGridProject)

responseGetSuite :: GetSuiteResponse -> TestTree
responseGetSuite =
  res
    "GetSuiteResponse"
    "fixture/GetSuiteResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetSuite)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    deviceFarmService
    (Proxy :: Proxy TagResource)

responseGetRemoteAccessSession :: GetRemoteAccessSessionResponse -> TestTree
responseGetRemoteAccessSession =
  res
    "GetRemoteAccessSessionResponse"
    "fixture/GetRemoteAccessSessionResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetRemoteAccessSession)

responseListDeviceInstances :: ListDeviceInstancesResponse -> TestTree
responseListDeviceInstances =
  res
    "ListDeviceInstancesResponse"
    "fixture/ListDeviceInstancesResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListDeviceInstances)

responsePurchaseOffering :: PurchaseOfferingResponse -> TestTree
responsePurchaseOffering =
  res
    "PurchaseOfferingResponse"
    "fixture/PurchaseOfferingResponse.proto"
    deviceFarmService
    (Proxy :: Proxy PurchaseOffering)

responseListInstanceProfiles :: ListInstanceProfilesResponse -> TestTree
responseListInstanceProfiles =
  res
    "ListInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListInstanceProfiles)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    deviceFarmService
    (Proxy :: Proxy UntagResource)

responseGetProject :: GetProjectResponse -> TestTree
responseGetProject =
  res
    "GetProjectResponse"
    "fixture/GetProjectResponse.proto"
    deviceFarmService
    (Proxy :: Proxy GetProject)

responseListUniqueProblems :: ListUniqueProblemsResponse -> TestTree
responseListUniqueProblems =
  res
    "ListUniqueProblemsResponse"
    "fixture/ListUniqueProblemsResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListUniqueProblems)

responseListVPCEConfigurations :: ListVPCEConfigurationsResponse -> TestTree
responseListVPCEConfigurations =
  res
    "ListVPCEConfigurationsResponse"
    "fixture/ListVPCEConfigurationsResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListVPCEConfigurations)

responseStopRun :: StopRunResponse -> TestTree
responseStopRun =
  res
    "StopRunResponse"
    "fixture/StopRunResponse.proto"
    deviceFarmService
    (Proxy :: Proxy StopRun)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListDevices)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    deviceFarmService
    (Proxy :: Proxy CreateProject)

responseListTestGridSessions :: ListTestGridSessionsResponse -> TestTree
responseListTestGridSessions =
  res
    "ListTestGridSessionsResponse"
    "fixture/ListTestGridSessionsResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListTestGridSessions)

responseCreateTestGridURL :: CreateTestGridURLResponse -> TestTree
responseCreateTestGridURL =
  res
    "CreateTestGridURLResponse"
    "fixture/CreateTestGridURLResponse.proto"
    deviceFarmService
    (Proxy :: Proxy CreateTestGridURL)

responseListOfferings :: ListOfferingsResponse -> TestTree
responseListOfferings =
  res
    "ListOfferingsResponse"
    "fixture/ListOfferingsResponse.proto"
    deviceFarmService
    (Proxy :: Proxy ListOfferings)
