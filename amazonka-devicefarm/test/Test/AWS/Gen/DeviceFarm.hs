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
--         , requestListTestGridSessionActions $
--             newListTestGridSessionActions
--
--         , requestCreateTestGridProject $
--             newCreateTestGridProject
--
--         , requestListSamples $
--             newListSamples
--
--         , requestListTestGridSessionArtifacts $
--             newListTestGridSessionArtifacts
--
--         , requestListNetworkProfiles $
--             newListNetworkProfiles
--
--         , requestScheduleRun $
--             newScheduleRun
--
--         , requestUpdateNetworkProfile $
--             newUpdateNetworkProfile
--
--         , requestInstallToRemoteAccessSession $
--             newInstallToRemoteAccessSession
--
--         , requestDeleteNetworkProfile $
--             newDeleteNetworkProfile
--
--         , requestGetDevicePoolCompatibility $
--             newGetDevicePoolCompatibility
--
--         , requestCreateVPCEConfiguration $
--             newCreateVPCEConfiguration
--
--         , requestListProjects $
--             newListProjects
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestCreateTestGridUrl $
--             newCreateTestGridUrl
--
--         , requestListOfferings $
--             newListOfferings
--
--         , requestListDevices $
--             newListDevices
--
--         , requestStopRun $
--             newStopRun
--
--         , requestCreateNetworkProfile $
--             newCreateNetworkProfile
--
--         , requestGetJob $
--             newGetJob
--
--         , requestCreateInstanceProfile $
--             newCreateInstanceProfile
--
--         , requestGetDevice $
--             newGetDevice
--
--         , requestStopRemoteAccessSession $
--             newStopRemoteAccessSession
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListDeviceInstances $
--             newListDeviceInstances
--
--         , requestUpdateDeviceInstance $
--             newUpdateDeviceInstance
--
--         , requestCreateDevicePool $
--             newCreateDevicePool
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListRuns $
--             newListRuns
--
--         , requestGetUpload $
--             newGetUpload
--
--         , requestGetTestGridProject $
--             newGetTestGridProject
--
--         , requestListDevicePools $
--             newListDevicePools
--
--         , requestGetOfferingStatus $
--             newGetOfferingStatus
--
--         , requestDeleteUpload $
--             newDeleteUpload
--
--         , requestListUploads $
--             newListUploads
--
--         , requestCreateRemoteAccessSession $
--             newCreateRemoteAccessSession
--
--         , requestUpdateUpload $
--             newUpdateUpload
--
--         , requestListTestGridProjects $
--             newListTestGridProjects
--
--         , requestListArtifacts $
--             newListArtifacts
--
--         , requestListRemoteAccessSessions $
--             newListRemoteAccessSessions
--
--         , requestGetRun $
--             newGetRun
--
--         , requestGetDeviceInstance $
--             newGetDeviceInstance
--
--         , requestListSuites $
--             newListSuites
--
--         , requestStopJob $
--             newStopJob
--
--         , requestListTests $
--             newListTests
--
--         , requestDeleteRemoteAccessSession $
--             newDeleteRemoteAccessSession
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestGetInstanceProfile $
--             newGetInstanceProfile
--
--         , requestUpdateProject $
--             newUpdateProject
--
--         , requestListTestGridSessions $
--             newListTestGridSessions
--
--         , requestListUniqueProblems $
--             newListUniqueProblems
--
--         , requestDeleteVPCEConfiguration $
--             newDeleteVPCEConfiguration
--
--         , requestUpdateVPCEConfiguration $
--             newUpdateVPCEConfiguration
--
--         , requestListVPCEConfigurations $
--             newListVPCEConfigurations
--
--         , requestGetVPCEConfiguration $
--             newGetVPCEConfiguration
--
--         , requestListJobs $
--             newListJobs
--
--         , requestGetTestGridSession $
--             newGetTestGridSession
--
--         , requestPurchaseOffering $
--             newPurchaseOffering
--
--         , requestGetProject $
--             newGetProject
--
--         , requestListInstanceProfiles $
--             newListInstanceProfiles
--
--         , requestGetNetworkProfile $
--             newGetNetworkProfile
--
--         , requestUpdateInstanceProfile $
--             newUpdateInstanceProfile
--
--         , requestDeleteInstanceProfile $
--             newDeleteInstanceProfile
--
--         , requestRenewOffering $
--             newRenewOffering
--
--         , requestGetRemoteAccessSession $
--             newGetRemoteAccessSession
--
--         , requestGetSuite $
--             newGetSuite
--
--         , requestDeleteRun $
--             newDeleteRun
--
--         , requestGetTest $
--             newGetTest
--
--         , requestDeleteDevicePool $
--             newDeleteDevicePool
--
--         , requestListOfferingTransactions $
--             newListOfferingTransactions
--
--         , requestUpdateDevicePool $
--             newUpdateDevicePool
--
--         , requestUpdateTestGridProject $
--             newUpdateTestGridProject
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeleteTestGridProject $
--             newDeleteTestGridProject
--
--         , requestListOfferingPromotions $
--             newListOfferingPromotions
--
--         , requestGetDevicePool $
--             newGetDevicePool
--
--         , requestGetAccountSettings $
--             newGetAccountSettings
--
--           ]

--     , testGroup "response"
--         [ responseCreateUpload $
--             newCreateUploadResponse
--
--         , responseListTestGridSessionActions $
--             newListTestGridSessionActionsResponse
--
--         , responseCreateTestGridProject $
--             newCreateTestGridProjectResponse
--
--         , responseListSamples $
--             newListSamplesResponse
--
--         , responseListTestGridSessionArtifacts $
--             newListTestGridSessionArtifactsResponse
--
--         , responseListNetworkProfiles $
--             newListNetworkProfilesResponse
--
--         , responseScheduleRun $
--             newScheduleRunResponse
--
--         , responseUpdateNetworkProfile $
--             newUpdateNetworkProfileResponse
--
--         , responseInstallToRemoteAccessSession $
--             newInstallToRemoteAccessSessionResponse
--
--         , responseDeleteNetworkProfile $
--             newDeleteNetworkProfileResponse
--
--         , responseGetDevicePoolCompatibility $
--             newGetDevicePoolCompatibilityResponse
--
--         , responseCreateVPCEConfiguration $
--             newCreateVPCEConfigurationResponse
--
--         , responseListProjects $
--             newListProjectsResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseCreateTestGridUrl $
--             newCreateTestGridUrlResponse
--
--         , responseListOfferings $
--             newListOfferingsResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--         , responseStopRun $
--             newStopRunResponse
--
--         , responseCreateNetworkProfile $
--             newCreateNetworkProfileResponse
--
--         , responseGetJob $
--             newGetJobResponse
--
--         , responseCreateInstanceProfile $
--             newCreateInstanceProfileResponse
--
--         , responseGetDevice $
--             newGetDeviceResponse
--
--         , responseStopRemoteAccessSession $
--             newStopRemoteAccessSessionResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListDeviceInstances $
--             newListDeviceInstancesResponse
--
--         , responseUpdateDeviceInstance $
--             newUpdateDeviceInstanceResponse
--
--         , responseCreateDevicePool $
--             newCreateDevicePoolResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListRuns $
--             newListRunsResponse
--
--         , responseGetUpload $
--             newGetUploadResponse
--
--         , responseGetTestGridProject $
--             newGetTestGridProjectResponse
--
--         , responseListDevicePools $
--             newListDevicePoolsResponse
--
--         , responseGetOfferingStatus $
--             newGetOfferingStatusResponse
--
--         , responseDeleteUpload $
--             newDeleteUploadResponse
--
--         , responseListUploads $
--             newListUploadsResponse
--
--         , responseCreateRemoteAccessSession $
--             newCreateRemoteAccessSessionResponse
--
--         , responseUpdateUpload $
--             newUpdateUploadResponse
--
--         , responseListTestGridProjects $
--             newListTestGridProjectsResponse
--
--         , responseListArtifacts $
--             newListArtifactsResponse
--
--         , responseListRemoteAccessSessions $
--             newListRemoteAccessSessionsResponse
--
--         , responseGetRun $
--             newGetRunResponse
--
--         , responseGetDeviceInstance $
--             newGetDeviceInstanceResponse
--
--         , responseListSuites $
--             newListSuitesResponse
--
--         , responseStopJob $
--             newStopJobResponse
--
--         , responseListTests $
--             newListTestsResponse
--
--         , responseDeleteRemoteAccessSession $
--             newDeleteRemoteAccessSessionResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseGetInstanceProfile $
--             newGetInstanceProfileResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--         , responseListTestGridSessions $
--             newListTestGridSessionsResponse
--
--         , responseListUniqueProblems $
--             newListUniqueProblemsResponse
--
--         , responseDeleteVPCEConfiguration $
--             newDeleteVPCEConfigurationResponse
--
--         , responseUpdateVPCEConfiguration $
--             newUpdateVPCEConfigurationResponse
--
--         , responseListVPCEConfigurations $
--             newListVPCEConfigurationsResponse
--
--         , responseGetVPCEConfiguration $
--             newGetVPCEConfigurationResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseGetTestGridSession $
--             newGetTestGridSessionResponse
--
--         , responsePurchaseOffering $
--             newPurchaseOfferingResponse
--
--         , responseGetProject $
--             newGetProjectResponse
--
--         , responseListInstanceProfiles $
--             newListInstanceProfilesResponse
--
--         , responseGetNetworkProfile $
--             newGetNetworkProfileResponse
--
--         , responseUpdateInstanceProfile $
--             newUpdateInstanceProfileResponse
--
--         , responseDeleteInstanceProfile $
--             newDeleteInstanceProfileResponse
--
--         , responseRenewOffering $
--             newRenewOfferingResponse
--
--         , responseGetRemoteAccessSession $
--             newGetRemoteAccessSessionResponse
--
--         , responseGetSuite $
--             newGetSuiteResponse
--
--         , responseDeleteRun $
--             newDeleteRunResponse
--
--         , responseGetTest $
--             newGetTestResponse
--
--         , responseDeleteDevicePool $
--             newDeleteDevicePoolResponse
--
--         , responseListOfferingTransactions $
--             newListOfferingTransactionsResponse
--
--         , responseUpdateDevicePool $
--             newUpdateDevicePoolResponse
--
--         , responseUpdateTestGridProject $
--             newUpdateTestGridProjectResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDeleteTestGridProject $
--             newDeleteTestGridProjectResponse
--
--         , responseListOfferingPromotions $
--             newListOfferingPromotionsResponse
--
--         , responseGetDevicePool $
--             newGetDevicePoolResponse
--
--         , responseGetAccountSettings $
--             newGetAccountSettingsResponse
--
--           ]
--     ]

-- Requests

requestCreateUpload :: CreateUpload -> TestTree
requestCreateUpload =
  req
    "CreateUpload"
    "fixture/CreateUpload.yaml"

requestListTestGridSessionActions :: ListTestGridSessionActions -> TestTree
requestListTestGridSessionActions =
  req
    "ListTestGridSessionActions"
    "fixture/ListTestGridSessionActions.yaml"

requestCreateTestGridProject :: CreateTestGridProject -> TestTree
requestCreateTestGridProject =
  req
    "CreateTestGridProject"
    "fixture/CreateTestGridProject.yaml"

requestListSamples :: ListSamples -> TestTree
requestListSamples =
  req
    "ListSamples"
    "fixture/ListSamples.yaml"

requestListTestGridSessionArtifacts :: ListTestGridSessionArtifacts -> TestTree
requestListTestGridSessionArtifacts =
  req
    "ListTestGridSessionArtifacts"
    "fixture/ListTestGridSessionArtifacts.yaml"

requestListNetworkProfiles :: ListNetworkProfiles -> TestTree
requestListNetworkProfiles =
  req
    "ListNetworkProfiles"
    "fixture/ListNetworkProfiles.yaml"

requestScheduleRun :: ScheduleRun -> TestTree
requestScheduleRun =
  req
    "ScheduleRun"
    "fixture/ScheduleRun.yaml"

requestUpdateNetworkProfile :: UpdateNetworkProfile -> TestTree
requestUpdateNetworkProfile =
  req
    "UpdateNetworkProfile"
    "fixture/UpdateNetworkProfile.yaml"

requestInstallToRemoteAccessSession :: InstallToRemoteAccessSession -> TestTree
requestInstallToRemoteAccessSession =
  req
    "InstallToRemoteAccessSession"
    "fixture/InstallToRemoteAccessSession.yaml"

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

requestCreateVPCEConfiguration :: CreateVPCEConfiguration -> TestTree
requestCreateVPCEConfiguration =
  req
    "CreateVPCEConfiguration"
    "fixture/CreateVPCEConfiguration.yaml"

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestCreateTestGridUrl :: CreateTestGridUrl -> TestTree
requestCreateTestGridUrl =
  req
    "CreateTestGridUrl"
    "fixture/CreateTestGridUrl.yaml"

requestListOfferings :: ListOfferings -> TestTree
requestListOfferings =
  req
    "ListOfferings"
    "fixture/ListOfferings.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

requestStopRun :: StopRun -> TestTree
requestStopRun =
  req
    "StopRun"
    "fixture/StopRun.yaml"

requestCreateNetworkProfile :: CreateNetworkProfile -> TestTree
requestCreateNetworkProfile =
  req
    "CreateNetworkProfile"
    "fixture/CreateNetworkProfile.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob =
  req
    "GetJob"
    "fixture/GetJob.yaml"

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

requestListDeviceInstances :: ListDeviceInstances -> TestTree
requestListDeviceInstances =
  req
    "ListDeviceInstances"
    "fixture/ListDeviceInstances.yaml"

requestUpdateDeviceInstance :: UpdateDeviceInstance -> TestTree
requestUpdateDeviceInstance =
  req
    "UpdateDeviceInstance"
    "fixture/UpdateDeviceInstance.yaml"

requestCreateDevicePool :: CreateDevicePool -> TestTree
requestCreateDevicePool =
  req
    "CreateDevicePool"
    "fixture/CreateDevicePool.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListRuns :: ListRuns -> TestTree
requestListRuns =
  req
    "ListRuns"
    "fixture/ListRuns.yaml"

requestGetUpload :: GetUpload -> TestTree
requestGetUpload =
  req
    "GetUpload"
    "fixture/GetUpload.yaml"

requestGetTestGridProject :: GetTestGridProject -> TestTree
requestGetTestGridProject =
  req
    "GetTestGridProject"
    "fixture/GetTestGridProject.yaml"

requestListDevicePools :: ListDevicePools -> TestTree
requestListDevicePools =
  req
    "ListDevicePools"
    "fixture/ListDevicePools.yaml"

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

requestListUploads :: ListUploads -> TestTree
requestListUploads =
  req
    "ListUploads"
    "fixture/ListUploads.yaml"

requestCreateRemoteAccessSession :: CreateRemoteAccessSession -> TestTree
requestCreateRemoteAccessSession =
  req
    "CreateRemoteAccessSession"
    "fixture/CreateRemoteAccessSession.yaml"

requestUpdateUpload :: UpdateUpload -> TestTree
requestUpdateUpload =
  req
    "UpdateUpload"
    "fixture/UpdateUpload.yaml"

requestListTestGridProjects :: ListTestGridProjects -> TestTree
requestListTestGridProjects =
  req
    "ListTestGridProjects"
    "fixture/ListTestGridProjects.yaml"

requestListArtifacts :: ListArtifacts -> TestTree
requestListArtifacts =
  req
    "ListArtifacts"
    "fixture/ListArtifacts.yaml"

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

requestStopJob :: StopJob -> TestTree
requestStopJob =
  req
    "StopJob"
    "fixture/StopJob.yaml"

requestListTests :: ListTests -> TestTree
requestListTests =
  req
    "ListTests"
    "fixture/ListTests.yaml"

requestDeleteRemoteAccessSession :: DeleteRemoteAccessSession -> TestTree
requestDeleteRemoteAccessSession =
  req
    "DeleteRemoteAccessSession"
    "fixture/DeleteRemoteAccessSession.yaml"

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

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject =
  req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

requestListTestGridSessions :: ListTestGridSessions -> TestTree
requestListTestGridSessions =
  req
    "ListTestGridSessions"
    "fixture/ListTestGridSessions.yaml"

requestListUniqueProblems :: ListUniqueProblems -> TestTree
requestListUniqueProblems =
  req
    "ListUniqueProblems"
    "fixture/ListUniqueProblems.yaml"

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

requestListVPCEConfigurations :: ListVPCEConfigurations -> TestTree
requestListVPCEConfigurations =
  req
    "ListVPCEConfigurations"
    "fixture/ListVPCEConfigurations.yaml"

requestGetVPCEConfiguration :: GetVPCEConfiguration -> TestTree
requestGetVPCEConfiguration =
  req
    "GetVPCEConfiguration"
    "fixture/GetVPCEConfiguration.yaml"

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

requestPurchaseOffering :: PurchaseOffering -> TestTree
requestPurchaseOffering =
  req
    "PurchaseOffering"
    "fixture/PurchaseOffering.yaml"

requestGetProject :: GetProject -> TestTree
requestGetProject =
  req
    "GetProject"
    "fixture/GetProject.yaml"

requestListInstanceProfiles :: ListInstanceProfiles -> TestTree
requestListInstanceProfiles =
  req
    "ListInstanceProfiles"
    "fixture/ListInstanceProfiles.yaml"

requestGetNetworkProfile :: GetNetworkProfile -> TestTree
requestGetNetworkProfile =
  req
    "GetNetworkProfile"
    "fixture/GetNetworkProfile.yaml"

requestUpdateInstanceProfile :: UpdateInstanceProfile -> TestTree
requestUpdateInstanceProfile =
  req
    "UpdateInstanceProfile"
    "fixture/UpdateInstanceProfile.yaml"

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

requestDeleteRun :: DeleteRun -> TestTree
requestDeleteRun =
  req
    "DeleteRun"
    "fixture/DeleteRun.yaml"

requestGetTest :: GetTest -> TestTree
requestGetTest =
  req
    "GetTest"
    "fixture/GetTest.yaml"

requestDeleteDevicePool :: DeleteDevicePool -> TestTree
requestDeleteDevicePool =
  req
    "DeleteDevicePool"
    "fixture/DeleteDevicePool.yaml"

requestListOfferingTransactions :: ListOfferingTransactions -> TestTree
requestListOfferingTransactions =
  req
    "ListOfferingTransactions"
    "fixture/ListOfferingTransactions.yaml"

requestUpdateDevicePool :: UpdateDevicePool -> TestTree
requestUpdateDevicePool =
  req
    "UpdateDevicePool"
    "fixture/UpdateDevicePool.yaml"

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

requestDeleteTestGridProject :: DeleteTestGridProject -> TestTree
requestDeleteTestGridProject =
  req
    "DeleteTestGridProject"
    "fixture/DeleteTestGridProject.yaml"

requestListOfferingPromotions :: ListOfferingPromotions -> TestTree
requestListOfferingPromotions =
  req
    "ListOfferingPromotions"
    "fixture/ListOfferingPromotions.yaml"

requestGetDevicePool :: GetDevicePool -> TestTree
requestGetDevicePool =
  req
    "GetDevicePool"
    "fixture/GetDevicePool.yaml"

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings =
  req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

-- Responses

responseCreateUpload :: CreateUploadResponse -> TestTree
responseCreateUpload =
  res
    "CreateUploadResponse"
    "fixture/CreateUploadResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUpload)

responseListTestGridSessionActions :: ListTestGridSessionActionsResponse -> TestTree
responseListTestGridSessionActions =
  res
    "ListTestGridSessionActionsResponse"
    "fixture/ListTestGridSessionActionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTestGridSessionActions)

responseCreateTestGridProject :: CreateTestGridProjectResponse -> TestTree
responseCreateTestGridProject =
  res
    "CreateTestGridProjectResponse"
    "fixture/CreateTestGridProjectResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTestGridProject)

responseListSamples :: ListSamplesResponse -> TestTree
responseListSamples =
  res
    "ListSamplesResponse"
    "fixture/ListSamplesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSamples)

responseListTestGridSessionArtifacts :: ListTestGridSessionArtifactsResponse -> TestTree
responseListTestGridSessionArtifacts =
  res
    "ListTestGridSessionArtifactsResponse"
    "fixture/ListTestGridSessionArtifactsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTestGridSessionArtifacts)

responseListNetworkProfiles :: ListNetworkProfilesResponse -> TestTree
responseListNetworkProfiles =
  res
    "ListNetworkProfilesResponse"
    "fixture/ListNetworkProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListNetworkProfiles)

responseScheduleRun :: ScheduleRunResponse -> TestTree
responseScheduleRun =
  res
    "ScheduleRunResponse"
    "fixture/ScheduleRunResponse.proto"
    defaultService
    (Proxy :: Proxy ScheduleRun)

responseUpdateNetworkProfile :: UpdateNetworkProfileResponse -> TestTree
responseUpdateNetworkProfile =
  res
    "UpdateNetworkProfileResponse"
    "fixture/UpdateNetworkProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNetworkProfile)

responseInstallToRemoteAccessSession :: InstallToRemoteAccessSessionResponse -> TestTree
responseInstallToRemoteAccessSession =
  res
    "InstallToRemoteAccessSessionResponse"
    "fixture/InstallToRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy :: Proxy InstallToRemoteAccessSession)

responseDeleteNetworkProfile :: DeleteNetworkProfileResponse -> TestTree
responseDeleteNetworkProfile =
  res
    "DeleteNetworkProfileResponse"
    "fixture/DeleteNetworkProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNetworkProfile)

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

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProjects)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProject)

responseCreateTestGridUrl :: CreateTestGridUrlResponse -> TestTree
responseCreateTestGridUrl =
  res
    "CreateTestGridUrlResponse"
    "fixture/CreateTestGridUrlResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTestGridUrl)

responseListOfferings :: ListOfferingsResponse -> TestTree
responseListOfferings =
  res
    "ListOfferingsResponse"
    "fixture/ListOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy ListOfferings)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDevices)

responseStopRun :: StopRunResponse -> TestTree
responseStopRun =
  res
    "StopRunResponse"
    "fixture/StopRunResponse.proto"
    defaultService
    (Proxy :: Proxy StopRun)

responseCreateNetworkProfile :: CreateNetworkProfileResponse -> TestTree
responseCreateNetworkProfile =
  res
    "CreateNetworkProfileResponse"
    "fixture/CreateNetworkProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNetworkProfile)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    defaultService
    (Proxy :: Proxy GetJob)

responseCreateInstanceProfile :: CreateInstanceProfileResponse -> TestTree
responseCreateInstanceProfile =
  res
    "CreateInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInstanceProfile)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice =
  res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy GetDevice)

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

responseListDeviceInstances :: ListDeviceInstancesResponse -> TestTree
responseListDeviceInstances =
  res
    "ListDeviceInstancesResponse"
    "fixture/ListDeviceInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeviceInstances)

responseUpdateDeviceInstance :: UpdateDeviceInstanceResponse -> TestTree
responseUpdateDeviceInstance =
  res
    "UpdateDeviceInstanceResponse"
    "fixture/UpdateDeviceInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDeviceInstance)

responseCreateDevicePool :: CreateDevicePoolResponse -> TestTree
responseCreateDevicePool =
  res
    "CreateDevicePoolResponse"
    "fixture/CreateDevicePoolResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDevicePool)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListRuns :: ListRunsResponse -> TestTree
responseListRuns =
  res
    "ListRunsResponse"
    "fixture/ListRunsResponse.proto"
    defaultService
    (Proxy :: Proxy ListRuns)

responseGetUpload :: GetUploadResponse -> TestTree
responseGetUpload =
  res
    "GetUploadResponse"
    "fixture/GetUploadResponse.proto"
    defaultService
    (Proxy :: Proxy GetUpload)

responseGetTestGridProject :: GetTestGridProjectResponse -> TestTree
responseGetTestGridProject =
  res
    "GetTestGridProjectResponse"
    "fixture/GetTestGridProjectResponse.proto"
    defaultService
    (Proxy :: Proxy GetTestGridProject)

responseListDevicePools :: ListDevicePoolsResponse -> TestTree
responseListDevicePools =
  res
    "ListDevicePoolsResponse"
    "fixture/ListDevicePoolsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDevicePools)

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

responseListUploads :: ListUploadsResponse -> TestTree
responseListUploads =
  res
    "ListUploadsResponse"
    "fixture/ListUploadsResponse.proto"
    defaultService
    (Proxy :: Proxy ListUploads)

responseCreateRemoteAccessSession :: CreateRemoteAccessSessionResponse -> TestTree
responseCreateRemoteAccessSession =
  res
    "CreateRemoteAccessSessionResponse"
    "fixture/CreateRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRemoteAccessSession)

responseUpdateUpload :: UpdateUploadResponse -> TestTree
responseUpdateUpload =
  res
    "UpdateUploadResponse"
    "fixture/UpdateUploadResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUpload)

responseListTestGridProjects :: ListTestGridProjectsResponse -> TestTree
responseListTestGridProjects =
  res
    "ListTestGridProjectsResponse"
    "fixture/ListTestGridProjectsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTestGridProjects)

responseListArtifacts :: ListArtifactsResponse -> TestTree
responseListArtifacts =
  res
    "ListArtifactsResponse"
    "fixture/ListArtifactsResponse.proto"
    defaultService
    (Proxy :: Proxy ListArtifacts)

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

responseStopJob :: StopJobResponse -> TestTree
responseStopJob =
  res
    "StopJobResponse"
    "fixture/StopJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopJob)

responseListTests :: ListTestsResponse -> TestTree
responseListTests =
  res
    "ListTestsResponse"
    "fixture/ListTestsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTests)

responseDeleteRemoteAccessSession :: DeleteRemoteAccessSessionResponse -> TestTree
responseDeleteRemoteAccessSession =
  res
    "DeleteRemoteAccessSessionResponse"
    "fixture/DeleteRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRemoteAccessSession)

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

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProject)

responseListTestGridSessions :: ListTestGridSessionsResponse -> TestTree
responseListTestGridSessions =
  res
    "ListTestGridSessionsResponse"
    "fixture/ListTestGridSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTestGridSessions)

responseListUniqueProblems :: ListUniqueProblemsResponse -> TestTree
responseListUniqueProblems =
  res
    "ListUniqueProblemsResponse"
    "fixture/ListUniqueProblemsResponse.proto"
    defaultService
    (Proxy :: Proxy ListUniqueProblems)

responseDeleteVPCEConfiguration :: DeleteVPCEConfigurationResponse -> TestTree
responseDeleteVPCEConfiguration =
  res
    "DeleteVPCEConfigurationResponse"
    "fixture/DeleteVPCEConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVPCEConfiguration)

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

responseGetVPCEConfiguration :: GetVPCEConfigurationResponse -> TestTree
responseGetVPCEConfiguration =
  res
    "GetVPCEConfigurationResponse"
    "fixture/GetVPCEConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetVPCEConfiguration)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobs)

responseGetTestGridSession :: GetTestGridSessionResponse -> TestTree
responseGetTestGridSession =
  res
    "GetTestGridSessionResponse"
    "fixture/GetTestGridSessionResponse.proto"
    defaultService
    (Proxy :: Proxy GetTestGridSession)

responsePurchaseOffering :: PurchaseOfferingResponse -> TestTree
responsePurchaseOffering =
  res
    "PurchaseOfferingResponse"
    "fixture/PurchaseOfferingResponse.proto"
    defaultService
    (Proxy :: Proxy PurchaseOffering)

responseGetProject :: GetProjectResponse -> TestTree
responseGetProject =
  res
    "GetProjectResponse"
    "fixture/GetProjectResponse.proto"
    defaultService
    (Proxy :: Proxy GetProject)

responseListInstanceProfiles :: ListInstanceProfilesResponse -> TestTree
responseListInstanceProfiles =
  res
    "ListInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstanceProfiles)

responseGetNetworkProfile :: GetNetworkProfileResponse -> TestTree
responseGetNetworkProfile =
  res
    "GetNetworkProfileResponse"
    "fixture/GetNetworkProfileResponse.proto"
    defaultService
    (Proxy :: Proxy GetNetworkProfile)

responseUpdateInstanceProfile :: UpdateInstanceProfileResponse -> TestTree
responseUpdateInstanceProfile =
  res
    "UpdateInstanceProfileResponse"
    "fixture/UpdateInstanceProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateInstanceProfile)

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

responseDeleteRun :: DeleteRunResponse -> TestTree
responseDeleteRun =
  res
    "DeleteRunResponse"
    "fixture/DeleteRunResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRun)

responseGetTest :: GetTestResponse -> TestTree
responseGetTest =
  res
    "GetTestResponse"
    "fixture/GetTestResponse.proto"
    defaultService
    (Proxy :: Proxy GetTest)

responseDeleteDevicePool :: DeleteDevicePoolResponse -> TestTree
responseDeleteDevicePool =
  res
    "DeleteDevicePoolResponse"
    "fixture/DeleteDevicePoolResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDevicePool)

responseListOfferingTransactions :: ListOfferingTransactionsResponse -> TestTree
responseListOfferingTransactions =
  res
    "ListOfferingTransactionsResponse"
    "fixture/ListOfferingTransactionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListOfferingTransactions)

responseUpdateDevicePool :: UpdateDevicePoolResponse -> TestTree
responseUpdateDevicePool =
  res
    "UpdateDevicePoolResponse"
    "fixture/UpdateDevicePoolResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDevicePool)

responseUpdateTestGridProject :: UpdateTestGridProjectResponse -> TestTree
responseUpdateTestGridProject =
  res
    "UpdateTestGridProjectResponse"
    "fixture/UpdateTestGridProjectResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTestGridProject)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDeleteTestGridProject :: DeleteTestGridProjectResponse -> TestTree
responseDeleteTestGridProject =
  res
    "DeleteTestGridProjectResponse"
    "fixture/DeleteTestGridProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTestGridProject)

responseListOfferingPromotions :: ListOfferingPromotionsResponse -> TestTree
responseListOfferingPromotions =
  res
    "ListOfferingPromotionsResponse"
    "fixture/ListOfferingPromotionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListOfferingPromotions)

responseGetDevicePool :: GetDevicePoolResponse -> TestTree
responseGetDevicePool =
  res
    "GetDevicePoolResponse"
    "fixture/GetDevicePoolResponse.proto"
    defaultService
    (Proxy :: Proxy GetDevicePool)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccountSettings)
