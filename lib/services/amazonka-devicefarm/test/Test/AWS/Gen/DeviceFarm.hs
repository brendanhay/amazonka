{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DeviceFarm
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestCreateUpload $
--             newCreateUpload
--
--         , requestCreateTestGridProject $
--             newCreateTestGridProject
--
--         , requestListTestGridSessionArtifacts $
--             newListTestGridSessionArtifacts
--
--         , requestListTestGridSessionActions $
--             newListTestGridSessionActions
--
--         , requestListSamples $
--             newListSamples
--
--         , requestScheduleRun $
--             newScheduleRun
--
--         , requestListProjects $
--             newListProjects
--
--         , requestGetDevicePoolCompatibility $
--             newGetDevicePoolCompatibility
--
--         , requestCreateVPCEConfiguration $
--             newCreateVPCEConfiguration
--
--         , requestUpdateNetworkProfile $
--             newUpdateNetworkProfile
--
--         , requestListNetworkProfiles $
--             newListNetworkProfiles
--
--         , requestDeleteNetworkProfile $
--             newDeleteNetworkProfile
--
--         , requestInstallToRemoteAccessSession $
--             newInstallToRemoteAccessSession
--
--         , requestStopRun $
--             newStopRun
--
--         , requestListOfferings $
--             newListOfferings
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestGetJob $
--             newGetJob
--
--         , requestListDevices $
--             newListDevices
--
--         , requestCreateTestGridUrl $
--             newCreateTestGridUrl
--
--         , requestCreateNetworkProfile $
--             newCreateNetworkProfile
--
--         , requestGetDevice $
--             newGetDevice
--
--         , requestCreateInstanceProfile $
--             newCreateInstanceProfile
--
--         , requestStopRemoteAccessSession $
--             newStopRemoteAccessSession
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListRuns $
--             newListRuns
--
--         , requestUpdateDeviceInstance $
--             newUpdateDeviceInstance
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListDeviceInstances $
--             newListDeviceInstances
--
--         , requestCreateDevicePool $
--             newCreateDevicePool
--
--         , requestListDevicePools $
--             newListDevicePools
--
--         , requestGetTestGridProject $
--             newGetTestGridProject
--
--         , requestGetUpload $
--             newGetUpload
--
--         , requestUpdateUpload $
--             newUpdateUpload
--
--         , requestGetOfferingStatus $
--             newGetOfferingStatus
--
--         , requestDeleteUpload $
--             newDeleteUpload
--
--         , requestCreateRemoteAccessSession $
--             newCreateRemoteAccessSession
--
--         , requestListTestGridProjects $
--             newListTestGridProjects
--
--         , requestListUploads $
--             newListUploads
--
--         , requestListTests $
--             newListTests
--
--         , requestListRemoteAccessSessions $
--             newListRemoteAccessSessions
--
--         , requestGetRun $
--             newGetRun
--
--         , requestDeleteRemoteAccessSession $
--             newDeleteRemoteAccessSession
--
--         , requestGetDeviceInstance $
--             newGetDeviceInstance
--
--         , requestListSuites $
--             newListSuites
--
--         , requestListArtifacts $
--             newListArtifacts
--
--         , requestStopJob $
--             newStopJob
--
--         , requestUpdateProject $
--             newUpdateProject
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestGetInstanceProfile $
--             newGetInstanceProfile
--
--         , requestUpdateVPCEConfiguration $
--             newUpdateVPCEConfiguration
--
--         , requestListVPCEConfigurations $
--             newListVPCEConfigurations
--
--         , requestListTestGridSessions $
--             newListTestGridSessions
--
--         , requestDeleteVPCEConfiguration $
--             newDeleteVPCEConfiguration
--
--         , requestListUniqueProblems $
--             newListUniqueProblems
--
--         , requestGetVPCEConfiguration $
--             newGetVPCEConfiguration
--
--         , requestGetTestGridSession $
--             newGetTestGridSession
--
--         , requestListJobs $
--             newListJobs
--
--         , requestGetNetworkProfile $
--             newGetNetworkProfile
--
--         , requestGetProject $
--             newGetProject
--
--         , requestUpdateInstanceProfile $
--             newUpdateInstanceProfile
--
--         , requestListInstanceProfiles $
--             newListInstanceProfiles
--
--         , requestDeleteInstanceProfile $
--             newDeleteInstanceProfile
--
--         , requestRenewOffering $
--             newRenewOffering
--
--         , requestPurchaseOffering $
--             newPurchaseOffering
--
--         , requestDeleteRun $
--             newDeleteRun
--
--         , requestGetRemoteAccessSession $
--             newGetRemoteAccessSession
--
--         , requestGetSuite $
--             newGetSuite
--
--         , requestGetTest $
--             newGetTest
--
--         , requestListOfferingTransactions $
--             newListOfferingTransactions
--
--         , requestDeleteDevicePool $
--             newDeleteDevicePool
--
--         , requestUpdateDevicePool $
--             newUpdateDevicePool
--
--         , requestGetDevicePool $
--             newGetDevicePool
--
--         , requestDeleteTestGridProject $
--             newDeleteTestGridProject
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestUpdateTestGridProject $
--             newUpdateTestGridProject
--
--         , requestGetAccountSettings $
--             newGetAccountSettings
--
--         , requestListOfferingPromotions $
--             newListOfferingPromotions
--
--           ]

--     , testGroup "response"
--         [ responseCreateUpload $
--             newCreateUploadResponse
--
--         , responseCreateTestGridProject $
--             newCreateTestGridProjectResponse
--
--         , responseListTestGridSessionArtifacts $
--             newListTestGridSessionArtifactsResponse
--
--         , responseListTestGridSessionActions $
--             newListTestGridSessionActionsResponse
--
--         , responseListSamples $
--             newListSamplesResponse
--
--         , responseScheduleRun $
--             newScheduleRunResponse
--
--         , responseListProjects $
--             newListProjectsResponse
--
--         , responseGetDevicePoolCompatibility $
--             newGetDevicePoolCompatibilityResponse
--
--         , responseCreateVPCEConfiguration $
--             newCreateVPCEConfigurationResponse
--
--         , responseUpdateNetworkProfile $
--             newUpdateNetworkProfileResponse
--
--         , responseListNetworkProfiles $
--             newListNetworkProfilesResponse
--
--         , responseDeleteNetworkProfile $
--             newDeleteNetworkProfileResponse
--
--         , responseInstallToRemoteAccessSession $
--             newInstallToRemoteAccessSessionResponse
--
--         , responseStopRun $
--             newStopRunResponse
--
--         , responseListOfferings $
--             newListOfferingsResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseGetJob $
--             newGetJobResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--         , responseCreateTestGridUrl $
--             newCreateTestGridUrlResponse
--
--         , responseCreateNetworkProfile $
--             newCreateNetworkProfileResponse
--
--         , responseGetDevice $
--             newGetDeviceResponse
--
--         , responseCreateInstanceProfile $
--             newCreateInstanceProfileResponse
--
--         , responseStopRemoteAccessSession $
--             newStopRemoteAccessSessionResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListRuns $
--             newListRunsResponse
--
--         , responseUpdateDeviceInstance $
--             newUpdateDeviceInstanceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListDeviceInstances $
--             newListDeviceInstancesResponse
--
--         , responseCreateDevicePool $
--             newCreateDevicePoolResponse
--
--         , responseListDevicePools $
--             newListDevicePoolsResponse
--
--         , responseGetTestGridProject $
--             newGetTestGridProjectResponse
--
--         , responseGetUpload $
--             newGetUploadResponse
--
--         , responseUpdateUpload $
--             newUpdateUploadResponse
--
--         , responseGetOfferingStatus $
--             newGetOfferingStatusResponse
--
--         , responseDeleteUpload $
--             newDeleteUploadResponse
--
--         , responseCreateRemoteAccessSession $
--             newCreateRemoteAccessSessionResponse
--
--         , responseListTestGridProjects $
--             newListTestGridProjectsResponse
--
--         , responseListUploads $
--             newListUploadsResponse
--
--         , responseListTests $
--             newListTestsResponse
--
--         , responseListRemoteAccessSessions $
--             newListRemoteAccessSessionsResponse
--
--         , responseGetRun $
--             newGetRunResponse
--
--         , responseDeleteRemoteAccessSession $
--             newDeleteRemoteAccessSessionResponse
--
--         , responseGetDeviceInstance $
--             newGetDeviceInstanceResponse
--
--         , responseListSuites $
--             newListSuitesResponse
--
--         , responseListArtifacts $
--             newListArtifactsResponse
--
--         , responseStopJob $
--             newStopJobResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseGetInstanceProfile $
--             newGetInstanceProfileResponse
--
--         , responseUpdateVPCEConfiguration $
--             newUpdateVPCEConfigurationResponse
--
--         , responseListVPCEConfigurations $
--             newListVPCEConfigurationsResponse
--
--         , responseListTestGridSessions $
--             newListTestGridSessionsResponse
--
--         , responseDeleteVPCEConfiguration $
--             newDeleteVPCEConfigurationResponse
--
--         , responseListUniqueProblems $
--             newListUniqueProblemsResponse
--
--         , responseGetVPCEConfiguration $
--             newGetVPCEConfigurationResponse
--
--         , responseGetTestGridSession $
--             newGetTestGridSessionResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseGetNetworkProfile $
--             newGetNetworkProfileResponse
--
--         , responseGetProject $
--             newGetProjectResponse
--
--         , responseUpdateInstanceProfile $
--             newUpdateInstanceProfileResponse
--
--         , responseListInstanceProfiles $
--             newListInstanceProfilesResponse
--
--         , responseDeleteInstanceProfile $
--             newDeleteInstanceProfileResponse
--
--         , responseRenewOffering $
--             newRenewOfferingResponse
--
--         , responsePurchaseOffering $
--             newPurchaseOfferingResponse
--
--         , responseDeleteRun $
--             newDeleteRunResponse
--
--         , responseGetRemoteAccessSession $
--             newGetRemoteAccessSessionResponse
--
--         , responseGetSuite $
--             newGetSuiteResponse
--
--         , responseGetTest $
--             newGetTestResponse
--
--         , responseListOfferingTransactions $
--             newListOfferingTransactionsResponse
--
--         , responseDeleteDevicePool $
--             newDeleteDevicePoolResponse
--
--         , responseUpdateDevicePool $
--             newUpdateDevicePoolResponse
--
--         , responseGetDevicePool $
--             newGetDevicePoolResponse
--
--         , responseDeleteTestGridProject $
--             newDeleteTestGridProjectResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseUpdateTestGridProject $
--             newUpdateTestGridProjectResponse
--
--         , responseGetAccountSettings $
--             newGetAccountSettingsResponse
--
--         , responseListOfferingPromotions $
--             newListOfferingPromotionsResponse
--
--           ]
--     ]

-- Requests

requestCreateUpload :: CreateUpload -> TestTree
requestCreateUpload =
  req
    "CreateUpload"
    "fixture/CreateUpload.yaml"

requestCreateTestGridProject :: CreateTestGridProject -> TestTree
requestCreateTestGridProject =
  req
    "CreateTestGridProject"
    "fixture/CreateTestGridProject.yaml"

requestListTestGridSessionArtifacts :: ListTestGridSessionArtifacts -> TestTree
requestListTestGridSessionArtifacts =
  req
    "ListTestGridSessionArtifacts"
    "fixture/ListTestGridSessionArtifacts.yaml"

requestListTestGridSessionActions :: ListTestGridSessionActions -> TestTree
requestListTestGridSessionActions =
  req
    "ListTestGridSessionActions"
    "fixture/ListTestGridSessionActions.yaml"

requestListSamples :: ListSamples -> TestTree
requestListSamples =
  req
    "ListSamples"
    "fixture/ListSamples.yaml"

requestScheduleRun :: ScheduleRun -> TestTree
requestScheduleRun =
  req
    "ScheduleRun"
    "fixture/ScheduleRun.yaml"

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestGetDevicePoolCompatibility :: GetDevicePoolCompatibility -> TestTree
requestGetDevicePoolCompatibility =
  req
    "GetDevicePoolCompatibility"
    "fixture/GetDevicePoolCompatibility.yaml"

requestCreateVPCEConfiguration :: CreateVPCEConfiguration -> TestTree
requestCreateVPCEConfiguration =
  req
    "CreateVPCEConfiguration"
    "fixture/CreateVPCEConfiguration.yaml"

requestUpdateNetworkProfile :: UpdateNetworkProfile -> TestTree
requestUpdateNetworkProfile =
  req
    "UpdateNetworkProfile"
    "fixture/UpdateNetworkProfile.yaml"

requestListNetworkProfiles :: ListNetworkProfiles -> TestTree
requestListNetworkProfiles =
  req
    "ListNetworkProfiles"
    "fixture/ListNetworkProfiles.yaml"

requestDeleteNetworkProfile :: DeleteNetworkProfile -> TestTree
requestDeleteNetworkProfile =
  req
    "DeleteNetworkProfile"
    "fixture/DeleteNetworkProfile.yaml"

requestInstallToRemoteAccessSession :: InstallToRemoteAccessSession -> TestTree
requestInstallToRemoteAccessSession =
  req
    "InstallToRemoteAccessSession"
    "fixture/InstallToRemoteAccessSession.yaml"

requestStopRun :: StopRun -> TestTree
requestStopRun =
  req
    "StopRun"
    "fixture/StopRun.yaml"

requestListOfferings :: ListOfferings -> TestTree
requestListOfferings =
  req
    "ListOfferings"
    "fixture/ListOfferings.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob =
  req
    "GetJob"
    "fixture/GetJob.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

requestCreateTestGridUrl :: CreateTestGridUrl -> TestTree
requestCreateTestGridUrl =
  req
    "CreateTestGridUrl"
    "fixture/CreateTestGridUrl.yaml"

requestCreateNetworkProfile :: CreateNetworkProfile -> TestTree
requestCreateNetworkProfile =
  req
    "CreateNetworkProfile"
    "fixture/CreateNetworkProfile.yaml"

requestGetDevice :: GetDevice -> TestTree
requestGetDevice =
  req
    "GetDevice"
    "fixture/GetDevice.yaml"

requestCreateInstanceProfile :: CreateInstanceProfile -> TestTree
requestCreateInstanceProfile =
  req
    "CreateInstanceProfile"
    "fixture/CreateInstanceProfile.yaml"

requestStopRemoteAccessSession :: StopRemoteAccessSession -> TestTree
requestStopRemoteAccessSession =
  req
    "StopRemoteAccessSession"
    "fixture/StopRemoteAccessSession.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListRuns :: ListRuns -> TestTree
requestListRuns =
  req
    "ListRuns"
    "fixture/ListRuns.yaml"

requestUpdateDeviceInstance :: UpdateDeviceInstance -> TestTree
requestUpdateDeviceInstance =
  req
    "UpdateDeviceInstance"
    "fixture/UpdateDeviceInstance.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListDeviceInstances :: ListDeviceInstances -> TestTree
requestListDeviceInstances =
  req
    "ListDeviceInstances"
    "fixture/ListDeviceInstances.yaml"

requestCreateDevicePool :: CreateDevicePool -> TestTree
requestCreateDevicePool =
  req
    "CreateDevicePool"
    "fixture/CreateDevicePool.yaml"

requestListDevicePools :: ListDevicePools -> TestTree
requestListDevicePools =
  req
    "ListDevicePools"
    "fixture/ListDevicePools.yaml"

requestGetTestGridProject :: GetTestGridProject -> TestTree
requestGetTestGridProject =
  req
    "GetTestGridProject"
    "fixture/GetTestGridProject.yaml"

requestGetUpload :: GetUpload -> TestTree
requestGetUpload =
  req
    "GetUpload"
    "fixture/GetUpload.yaml"

requestUpdateUpload :: UpdateUpload -> TestTree
requestUpdateUpload =
  req
    "UpdateUpload"
    "fixture/UpdateUpload.yaml"

requestGetOfferingStatus :: GetOfferingStatus -> TestTree
requestGetOfferingStatus =
  req
    "GetOfferingStatus"
    "fixture/GetOfferingStatus.yaml"

requestDeleteUpload :: DeleteUpload -> TestTree
requestDeleteUpload =
  req
    "DeleteUpload"
    "fixture/DeleteUpload.yaml"

requestCreateRemoteAccessSession :: CreateRemoteAccessSession -> TestTree
requestCreateRemoteAccessSession =
  req
    "CreateRemoteAccessSession"
    "fixture/CreateRemoteAccessSession.yaml"

requestListTestGridProjects :: ListTestGridProjects -> TestTree
requestListTestGridProjects =
  req
    "ListTestGridProjects"
    "fixture/ListTestGridProjects.yaml"

requestListUploads :: ListUploads -> TestTree
requestListUploads =
  req
    "ListUploads"
    "fixture/ListUploads.yaml"

requestListTests :: ListTests -> TestTree
requestListTests =
  req
    "ListTests"
    "fixture/ListTests.yaml"

requestListRemoteAccessSessions :: ListRemoteAccessSessions -> TestTree
requestListRemoteAccessSessions =
  req
    "ListRemoteAccessSessions"
    "fixture/ListRemoteAccessSessions.yaml"

requestGetRun :: GetRun -> TestTree
requestGetRun =
  req
    "GetRun"
    "fixture/GetRun.yaml"

requestDeleteRemoteAccessSession :: DeleteRemoteAccessSession -> TestTree
requestDeleteRemoteAccessSession =
  req
    "DeleteRemoteAccessSession"
    "fixture/DeleteRemoteAccessSession.yaml"

requestGetDeviceInstance :: GetDeviceInstance -> TestTree
requestGetDeviceInstance =
  req
    "GetDeviceInstance"
    "fixture/GetDeviceInstance.yaml"

requestListSuites :: ListSuites -> TestTree
requestListSuites =
  req
    "ListSuites"
    "fixture/ListSuites.yaml"

requestListArtifacts :: ListArtifacts -> TestTree
requestListArtifacts =
  req
    "ListArtifacts"
    "fixture/ListArtifacts.yaml"

requestStopJob :: StopJob -> TestTree
requestStopJob =
  req
    "StopJob"
    "fixture/StopJob.yaml"

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject =
  req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestGetInstanceProfile :: GetInstanceProfile -> TestTree
requestGetInstanceProfile =
  req
    "GetInstanceProfile"
    "fixture/GetInstanceProfile.yaml"

requestUpdateVPCEConfiguration :: UpdateVPCEConfiguration -> TestTree
requestUpdateVPCEConfiguration =
  req
    "UpdateVPCEConfiguration"
    "fixture/UpdateVPCEConfiguration.yaml"

requestListVPCEConfigurations :: ListVPCEConfigurations -> TestTree
requestListVPCEConfigurations =
  req
    "ListVPCEConfigurations"
    "fixture/ListVPCEConfigurations.yaml"

requestListTestGridSessions :: ListTestGridSessions -> TestTree
requestListTestGridSessions =
  req
    "ListTestGridSessions"
    "fixture/ListTestGridSessions.yaml"

requestDeleteVPCEConfiguration :: DeleteVPCEConfiguration -> TestTree
requestDeleteVPCEConfiguration =
  req
    "DeleteVPCEConfiguration"
    "fixture/DeleteVPCEConfiguration.yaml"

requestListUniqueProblems :: ListUniqueProblems -> TestTree
requestListUniqueProblems =
  req
    "ListUniqueProblems"
    "fixture/ListUniqueProblems.yaml"

requestGetVPCEConfiguration :: GetVPCEConfiguration -> TestTree
requestGetVPCEConfiguration =
  req
    "GetVPCEConfiguration"
    "fixture/GetVPCEConfiguration.yaml"

requestGetTestGridSession :: GetTestGridSession -> TestTree
requestGetTestGridSession =
  req
    "GetTestGridSession"
    "fixture/GetTestGridSession.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestGetNetworkProfile :: GetNetworkProfile -> TestTree
requestGetNetworkProfile =
  req
    "GetNetworkProfile"
    "fixture/GetNetworkProfile.yaml"

requestGetProject :: GetProject -> TestTree
requestGetProject =
  req
    "GetProject"
    "fixture/GetProject.yaml"

requestUpdateInstanceProfile :: UpdateInstanceProfile -> TestTree
requestUpdateInstanceProfile =
  req
    "UpdateInstanceProfile"
    "fixture/UpdateInstanceProfile.yaml"

requestListInstanceProfiles :: ListInstanceProfiles -> TestTree
requestListInstanceProfiles =
  req
    "ListInstanceProfiles"
    "fixture/ListInstanceProfiles.yaml"

requestDeleteInstanceProfile :: DeleteInstanceProfile -> TestTree
requestDeleteInstanceProfile =
  req
    "DeleteInstanceProfile"
    "fixture/DeleteInstanceProfile.yaml"

requestRenewOffering :: RenewOffering -> TestTree
requestRenewOffering =
  req
    "RenewOffering"
    "fixture/RenewOffering.yaml"

requestPurchaseOffering :: PurchaseOffering -> TestTree
requestPurchaseOffering =
  req
    "PurchaseOffering"
    "fixture/PurchaseOffering.yaml"

requestDeleteRun :: DeleteRun -> TestTree
requestDeleteRun =
  req
    "DeleteRun"
    "fixture/DeleteRun.yaml"

requestGetRemoteAccessSession :: GetRemoteAccessSession -> TestTree
requestGetRemoteAccessSession =
  req
    "GetRemoteAccessSession"
    "fixture/GetRemoteAccessSession.yaml"

requestGetSuite :: GetSuite -> TestTree
requestGetSuite =
  req
    "GetSuite"
    "fixture/GetSuite.yaml"

requestGetTest :: GetTest -> TestTree
requestGetTest =
  req
    "GetTest"
    "fixture/GetTest.yaml"

requestListOfferingTransactions :: ListOfferingTransactions -> TestTree
requestListOfferingTransactions =
  req
    "ListOfferingTransactions"
    "fixture/ListOfferingTransactions.yaml"

requestDeleteDevicePool :: DeleteDevicePool -> TestTree
requestDeleteDevicePool =
  req
    "DeleteDevicePool"
    "fixture/DeleteDevicePool.yaml"

requestUpdateDevicePool :: UpdateDevicePool -> TestTree
requestUpdateDevicePool =
  req
    "UpdateDevicePool"
    "fixture/UpdateDevicePool.yaml"

requestGetDevicePool :: GetDevicePool -> TestTree
requestGetDevicePool =
  req
    "GetDevicePool"
    "fixture/GetDevicePool.yaml"

requestDeleteTestGridProject :: DeleteTestGridProject -> TestTree
requestDeleteTestGridProject =
  req
    "DeleteTestGridProject"
    "fixture/DeleteTestGridProject.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestUpdateTestGridProject :: UpdateTestGridProject -> TestTree
requestUpdateTestGridProject =
  req
    "UpdateTestGridProject"
    "fixture/UpdateTestGridProject.yaml"

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings =
  req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

requestListOfferingPromotions :: ListOfferingPromotions -> TestTree
requestListOfferingPromotions =
  req
    "ListOfferingPromotions"
    "fixture/ListOfferingPromotions.yaml"

-- Responses

responseCreateUpload :: CreateUploadResponse -> TestTree
responseCreateUpload =
  res
    "CreateUploadResponse"
    "fixture/CreateUploadResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUpload)

responseCreateTestGridProject :: CreateTestGridProjectResponse -> TestTree
responseCreateTestGridProject =
  res
    "CreateTestGridProjectResponse"
    "fixture/CreateTestGridProjectResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTestGridProject)

responseListTestGridSessionArtifacts :: ListTestGridSessionArtifactsResponse -> TestTree
responseListTestGridSessionArtifacts =
  res
    "ListTestGridSessionArtifactsResponse"
    "fixture/ListTestGridSessionArtifactsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTestGridSessionArtifacts)

responseListTestGridSessionActions :: ListTestGridSessionActionsResponse -> TestTree
responseListTestGridSessionActions =
  res
    "ListTestGridSessionActionsResponse"
    "fixture/ListTestGridSessionActionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTestGridSessionActions)

responseListSamples :: ListSamplesResponse -> TestTree
responseListSamples =
  res
    "ListSamplesResponse"
    "fixture/ListSamplesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSamples)

responseScheduleRun :: ScheduleRunResponse -> TestTree
responseScheduleRun =
  res
    "ScheduleRunResponse"
    "fixture/ScheduleRunResponse.proto"
    defaultService
    (Proxy :: Proxy ScheduleRun)

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProjects)

responseGetDevicePoolCompatibility :: GetDevicePoolCompatibilityResponse -> TestTree
responseGetDevicePoolCompatibility =
  res
    "GetDevicePoolCompatibilityResponse"
    "fixture/GetDevicePoolCompatibilityResponse.proto"
    defaultService
    (Proxy :: Proxy GetDevicePoolCompatibility)

responseCreateVPCEConfiguration :: CreateVPCEConfigurationResponse -> TestTree
responseCreateVPCEConfiguration =
  res
    "CreateVPCEConfigurationResponse"
    "fixture/CreateVPCEConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVPCEConfiguration)

responseUpdateNetworkProfile :: UpdateNetworkProfileResponse -> TestTree
responseUpdateNetworkProfile =
  res
    "UpdateNetworkProfileResponse"
    "fixture/UpdateNetworkProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNetworkProfile)

responseListNetworkProfiles :: ListNetworkProfilesResponse -> TestTree
responseListNetworkProfiles =
  res
    "ListNetworkProfilesResponse"
    "fixture/ListNetworkProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListNetworkProfiles)

responseDeleteNetworkProfile :: DeleteNetworkProfileResponse -> TestTree
responseDeleteNetworkProfile =
  res
    "DeleteNetworkProfileResponse"
    "fixture/DeleteNetworkProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNetworkProfile)

responseInstallToRemoteAccessSession :: InstallToRemoteAccessSessionResponse -> TestTree
responseInstallToRemoteAccessSession =
  res
    "InstallToRemoteAccessSessionResponse"
    "fixture/InstallToRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy :: Proxy InstallToRemoteAccessSession)

responseStopRun :: StopRunResponse -> TestTree
responseStopRun =
  res
    "StopRunResponse"
    "fixture/StopRunResponse.proto"
    defaultService
    (Proxy :: Proxy StopRun)

responseListOfferings :: ListOfferingsResponse -> TestTree
responseListOfferings =
  res
    "ListOfferingsResponse"
    "fixture/ListOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy ListOfferings)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProject)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    defaultService
    (Proxy :: Proxy GetJob)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDevices)

responseCreateTestGridUrl :: CreateTestGridUrlResponse -> TestTree
responseCreateTestGridUrl =
  res
    "CreateTestGridUrlResponse"
    "fixture/CreateTestGridUrlResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTestGridUrl)

responseCreateNetworkProfile :: CreateNetworkProfileResponse -> TestTree
responseCreateNetworkProfile =
  res
    "CreateNetworkProfileResponse"
    "fixture/CreateNetworkProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNetworkProfile)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice =
  res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy GetDevice)

responseCreateInstanceProfile :: CreateInstanceProfileResponse -> TestTree
responseCreateInstanceProfile =
  res
    "CreateInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInstanceProfile)

responseStopRemoteAccessSession :: StopRemoteAccessSessionResponse -> TestTree
responseStopRemoteAccessSession =
  res
    "StopRemoteAccessSessionResponse"
    "fixture/StopRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy :: Proxy StopRemoteAccessSession)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseListRuns :: ListRunsResponse -> TestTree
responseListRuns =
  res
    "ListRunsResponse"
    "fixture/ListRunsResponse.proto"
    defaultService
    (Proxy :: Proxy ListRuns)

responseUpdateDeviceInstance :: UpdateDeviceInstanceResponse -> TestTree
responseUpdateDeviceInstance =
  res
    "UpdateDeviceInstanceResponse"
    "fixture/UpdateDeviceInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDeviceInstance)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListDeviceInstances :: ListDeviceInstancesResponse -> TestTree
responseListDeviceInstances =
  res
    "ListDeviceInstancesResponse"
    "fixture/ListDeviceInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeviceInstances)

responseCreateDevicePool :: CreateDevicePoolResponse -> TestTree
responseCreateDevicePool =
  res
    "CreateDevicePoolResponse"
    "fixture/CreateDevicePoolResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDevicePool)

responseListDevicePools :: ListDevicePoolsResponse -> TestTree
responseListDevicePools =
  res
    "ListDevicePoolsResponse"
    "fixture/ListDevicePoolsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDevicePools)

responseGetTestGridProject :: GetTestGridProjectResponse -> TestTree
responseGetTestGridProject =
  res
    "GetTestGridProjectResponse"
    "fixture/GetTestGridProjectResponse.proto"
    defaultService
    (Proxy :: Proxy GetTestGridProject)

responseGetUpload :: GetUploadResponse -> TestTree
responseGetUpload =
  res
    "GetUploadResponse"
    "fixture/GetUploadResponse.proto"
    defaultService
    (Proxy :: Proxy GetUpload)

responseUpdateUpload :: UpdateUploadResponse -> TestTree
responseUpdateUpload =
  res
    "UpdateUploadResponse"
    "fixture/UpdateUploadResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUpload)

responseGetOfferingStatus :: GetOfferingStatusResponse -> TestTree
responseGetOfferingStatus =
  res
    "GetOfferingStatusResponse"
    "fixture/GetOfferingStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetOfferingStatus)

responseDeleteUpload :: DeleteUploadResponse -> TestTree
responseDeleteUpload =
  res
    "DeleteUploadResponse"
    "fixture/DeleteUploadResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUpload)

responseCreateRemoteAccessSession :: CreateRemoteAccessSessionResponse -> TestTree
responseCreateRemoteAccessSession =
  res
    "CreateRemoteAccessSessionResponse"
    "fixture/CreateRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRemoteAccessSession)

responseListTestGridProjects :: ListTestGridProjectsResponse -> TestTree
responseListTestGridProjects =
  res
    "ListTestGridProjectsResponse"
    "fixture/ListTestGridProjectsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTestGridProjects)

responseListUploads :: ListUploadsResponse -> TestTree
responseListUploads =
  res
    "ListUploadsResponse"
    "fixture/ListUploadsResponse.proto"
    defaultService
    (Proxy :: Proxy ListUploads)

responseListTests :: ListTestsResponse -> TestTree
responseListTests =
  res
    "ListTestsResponse"
    "fixture/ListTestsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTests)

responseListRemoteAccessSessions :: ListRemoteAccessSessionsResponse -> TestTree
responseListRemoteAccessSessions =
  res
    "ListRemoteAccessSessionsResponse"
    "fixture/ListRemoteAccessSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListRemoteAccessSessions)

responseGetRun :: GetRunResponse -> TestTree
responseGetRun =
  res
    "GetRunResponse"
    "fixture/GetRunResponse.proto"
    defaultService
    (Proxy :: Proxy GetRun)

responseDeleteRemoteAccessSession :: DeleteRemoteAccessSessionResponse -> TestTree
responseDeleteRemoteAccessSession =
  res
    "DeleteRemoteAccessSessionResponse"
    "fixture/DeleteRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRemoteAccessSession)

responseGetDeviceInstance :: GetDeviceInstanceResponse -> TestTree
responseGetDeviceInstance =
  res
    "GetDeviceInstanceResponse"
    "fixture/GetDeviceInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeviceInstance)

responseListSuites :: ListSuitesResponse -> TestTree
responseListSuites =
  res
    "ListSuitesResponse"
    "fixture/ListSuitesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSuites)

responseListArtifacts :: ListArtifactsResponse -> TestTree
responseListArtifacts =
  res
    "ListArtifactsResponse"
    "fixture/ListArtifactsResponse.proto"
    defaultService
    (Proxy :: Proxy ListArtifacts)

responseStopJob :: StopJobResponse -> TestTree
responseStopJob =
  res
    "StopJobResponse"
    "fixture/StopJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopJob)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProject)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProject)

responseGetInstanceProfile :: GetInstanceProfileResponse -> TestTree
responseGetInstanceProfile =
  res
    "GetInstanceProfileResponse"
    "fixture/GetInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstanceProfile)

responseUpdateVPCEConfiguration :: UpdateVPCEConfigurationResponse -> TestTree
responseUpdateVPCEConfiguration =
  res
    "UpdateVPCEConfigurationResponse"
    "fixture/UpdateVPCEConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateVPCEConfiguration)

responseListVPCEConfigurations :: ListVPCEConfigurationsResponse -> TestTree
responseListVPCEConfigurations =
  res
    "ListVPCEConfigurationsResponse"
    "fixture/ListVPCEConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListVPCEConfigurations)

responseListTestGridSessions :: ListTestGridSessionsResponse -> TestTree
responseListTestGridSessions =
  res
    "ListTestGridSessionsResponse"
    "fixture/ListTestGridSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTestGridSessions)

responseDeleteVPCEConfiguration :: DeleteVPCEConfigurationResponse -> TestTree
responseDeleteVPCEConfiguration =
  res
    "DeleteVPCEConfigurationResponse"
    "fixture/DeleteVPCEConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVPCEConfiguration)

responseListUniqueProblems :: ListUniqueProblemsResponse -> TestTree
responseListUniqueProblems =
  res
    "ListUniqueProblemsResponse"
    "fixture/ListUniqueProblemsResponse.proto"
    defaultService
    (Proxy :: Proxy ListUniqueProblems)

responseGetVPCEConfiguration :: GetVPCEConfigurationResponse -> TestTree
responseGetVPCEConfiguration =
  res
    "GetVPCEConfigurationResponse"
    "fixture/GetVPCEConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetVPCEConfiguration)

responseGetTestGridSession :: GetTestGridSessionResponse -> TestTree
responseGetTestGridSession =
  res
    "GetTestGridSessionResponse"
    "fixture/GetTestGridSessionResponse.proto"
    defaultService
    (Proxy :: Proxy GetTestGridSession)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobs)

responseGetNetworkProfile :: GetNetworkProfileResponse -> TestTree
responseGetNetworkProfile =
  res
    "GetNetworkProfileResponse"
    "fixture/GetNetworkProfileResponse.proto"
    defaultService
    (Proxy :: Proxy GetNetworkProfile)

responseGetProject :: GetProjectResponse -> TestTree
responseGetProject =
  res
    "GetProjectResponse"
    "fixture/GetProjectResponse.proto"
    defaultService
    (Proxy :: Proxy GetProject)

responseUpdateInstanceProfile :: UpdateInstanceProfileResponse -> TestTree
responseUpdateInstanceProfile =
  res
    "UpdateInstanceProfileResponse"
    "fixture/UpdateInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateInstanceProfile)

responseListInstanceProfiles :: ListInstanceProfilesResponse -> TestTree
responseListInstanceProfiles =
  res
    "ListInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstanceProfiles)

responseDeleteInstanceProfile :: DeleteInstanceProfileResponse -> TestTree
responseDeleteInstanceProfile =
  res
    "DeleteInstanceProfileResponse"
    "fixture/DeleteInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInstanceProfile)

responseRenewOffering :: RenewOfferingResponse -> TestTree
responseRenewOffering =
  res
    "RenewOfferingResponse"
    "fixture/RenewOfferingResponse.proto"
    defaultService
    (Proxy :: Proxy RenewOffering)

responsePurchaseOffering :: PurchaseOfferingResponse -> TestTree
responsePurchaseOffering =
  res
    "PurchaseOfferingResponse"
    "fixture/PurchaseOfferingResponse.proto"
    defaultService
    (Proxy :: Proxy PurchaseOffering)

responseDeleteRun :: DeleteRunResponse -> TestTree
responseDeleteRun =
  res
    "DeleteRunResponse"
    "fixture/DeleteRunResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRun)

responseGetRemoteAccessSession :: GetRemoteAccessSessionResponse -> TestTree
responseGetRemoteAccessSession =
  res
    "GetRemoteAccessSessionResponse"
    "fixture/GetRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy :: Proxy GetRemoteAccessSession)

responseGetSuite :: GetSuiteResponse -> TestTree
responseGetSuite =
  res
    "GetSuiteResponse"
    "fixture/GetSuiteResponse.proto"
    defaultService
    (Proxy :: Proxy GetSuite)

responseGetTest :: GetTestResponse -> TestTree
responseGetTest =
  res
    "GetTestResponse"
    "fixture/GetTestResponse.proto"
    defaultService
    (Proxy :: Proxy GetTest)

responseListOfferingTransactions :: ListOfferingTransactionsResponse -> TestTree
responseListOfferingTransactions =
  res
    "ListOfferingTransactionsResponse"
    "fixture/ListOfferingTransactionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListOfferingTransactions)

responseDeleteDevicePool :: DeleteDevicePoolResponse -> TestTree
responseDeleteDevicePool =
  res
    "DeleteDevicePoolResponse"
    "fixture/DeleteDevicePoolResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDevicePool)

responseUpdateDevicePool :: UpdateDevicePoolResponse -> TestTree
responseUpdateDevicePool =
  res
    "UpdateDevicePoolResponse"
    "fixture/UpdateDevicePoolResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDevicePool)

responseGetDevicePool :: GetDevicePoolResponse -> TestTree
responseGetDevicePool =
  res
    "GetDevicePoolResponse"
    "fixture/GetDevicePoolResponse.proto"
    defaultService
    (Proxy :: Proxy GetDevicePool)

responseDeleteTestGridProject :: DeleteTestGridProjectResponse -> TestTree
responseDeleteTestGridProject =
  res
    "DeleteTestGridProjectResponse"
    "fixture/DeleteTestGridProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTestGridProject)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseUpdateTestGridProject :: UpdateTestGridProjectResponse -> TestTree
responseUpdateTestGridProject =
  res
    "UpdateTestGridProjectResponse"
    "fixture/UpdateTestGridProjectResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTestGridProject)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccountSettings)

responseListOfferingPromotions :: ListOfferingPromotionsResponse -> TestTree
responseListOfferingPromotions =
  res
    "ListOfferingPromotionsResponse"
    "fixture/ListOfferingPromotionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListOfferingPromotions)
