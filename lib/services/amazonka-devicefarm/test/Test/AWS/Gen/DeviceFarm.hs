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

import qualified Data.Proxy as Proxy
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
--             newListProjects
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestUpdateProject $
--             newUpdateProject
--
--         , requestUpdateNetworkProfile $
--             newUpdateNetworkProfile
--
--         , requestDeleteNetworkProfile $
--             newDeleteNetworkProfile
--
--         , requestGetDevicePoolCompatibility $
--             newGetDevicePoolCompatibility
--
--         , requestInstallToRemoteAccessSession $
--             newInstallToRemoteAccessSession
--
--         , requestListTests $
--             newListTests
--
--         , requestListArtifacts $
--             newListArtifacts
--
--         , requestListTestGridSessionActions $
--             newListTestGridSessionActions
--
--         , requestCreateUpload $
--             newCreateUpload
--
--         , requestGetDeviceInstance $
--             newGetDeviceInstance
--
--         , requestStopJob $
--             newStopJob
--
--         , requestDeleteRemoteAccessSession $
--             newDeleteRemoteAccessSession
--
--         , requestListTestGridSessionArtifacts $
--             newListTestGridSessionArtifacts
--
--         , requestListTestGridProjects $
--             newListTestGridProjects
--
--         , requestDeleteUpload $
--             newDeleteUpload
--
--         , requestUpdateUpload $
--             newUpdateUpload
--
--         , requestDeleteTestGridProject $
--             newDeleteTestGridProject
--
--         , requestUpdateTestGridProject $
--             newUpdateTestGridProject
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetDevicePool $
--             newGetDevicePool
--
--         , requestListDevicePools $
--             newListDevicePools
--
--         , requestUpdateDevicePool $
--             newUpdateDevicePool
--
--         , requestDeleteDevicePool $
--             newDeleteDevicePool
--
--         , requestGetUpload $
--             newGetUpload
--
--         , requestListOfferingTransactions $
--             newListOfferingTransactions
--
--         , requestCreateDevicePool $
--             newCreateDevicePool
--
--         , requestDeleteRun $
--             newDeleteRun
--
--         , requestListRuns $
--             newListRuns
--
--         , requestGetTest $
--             newGetTest
--
--         , requestUpdateDeviceInstance $
--             newUpdateDeviceInstance
--
--         , requestGetNetworkProfile $
--             newGetNetworkProfile
--
--         , requestRenewOffering $
--             newRenewOffering
--
--         , requestDeleteInstanceProfile $
--             newDeleteInstanceProfile
--
--         , requestUpdateInstanceProfile $
--             newUpdateInstanceProfile
--
--         , requestCreateInstanceProfile $
--             newCreateInstanceProfile
--
--         , requestGetDevice $
--             newGetDevice
--
--         , requestListJobs $
--             newListJobs
--
--         , requestGetTestGridSession $
--             newGetTestGridSession
--
--         , requestGetVPCEConfiguration $
--             newGetVPCEConfiguration
--
--         , requestStopRemoteAccessSession $
--             newStopRemoteAccessSession
--
--         , requestCreateNetworkProfile $
--             newCreateNetworkProfile
--
--         , requestDeleteVPCEConfiguration $
--             newDeleteVPCEConfiguration
--
--         , requestUpdateVPCEConfiguration $
--             newUpdateVPCEConfiguration
--
--         , requestGetJob $
--             newGetJob
--
--         , requestGetInstanceProfile $
--             newGetInstanceProfile
--
--         , requestListNetworkProfiles $
--             newListNetworkProfiles
--
--         , requestCreateVPCEConfiguration $
--             newCreateVPCEConfiguration
--
--         , requestScheduleRun $
--             newScheduleRun
--
--         , requestCreateTestGridProject $
--             newCreateTestGridProject
--
--         , requestGetRun $
--             newGetRun
--
--         , requestListSamples $
--             newListSamples
--
--         , requestListSuites $
--             newListSuites
--
--         , requestListRemoteAccessSessions $
--             newListRemoteAccessSessions
--
--         , requestGetAccountSettings $
--             newGetAccountSettings
--
--         , requestCreateRemoteAccessSession $
--             newCreateRemoteAccessSession
--
--         , requestListOfferingPromotions $
--             newListOfferingPromotions
--
--         , requestGetOfferingStatus $
--             newGetOfferingStatus
--
--         , requestListUploads $
--             newListUploads
--
--         , requestGetTestGridProject $
--             newGetTestGridProject
--
--         , requestGetSuite $
--             newGetSuite
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetRemoteAccessSession $
--             newGetRemoteAccessSession
--
--         , requestListDeviceInstances $
--             newListDeviceInstances
--
--         , requestPurchaseOffering $
--             newPurchaseOffering
--
--         , requestListInstanceProfiles $
--             newListInstanceProfiles
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetProject $
--             newGetProject
--
--         , requestListUniqueProblems $
--             newListUniqueProblems
--
--         , requestListVPCEConfigurations $
--             newListVPCEConfigurations
--
--         , requestStopRun $
--             newStopRun
--
--         , requestListDevices $
--             newListDevices
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestListTestGridSessions $
--             newListTestGridSessions
--
--         , requestCreateTestGridUrl $
--             newCreateTestGridUrl
--
--         , requestListOfferings $
--             newListOfferings
--
--           ]

--     , testGroup "response"
--         [ responseListProjects $
--             newListProjectsResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--         , responseUpdateNetworkProfile $
--             newUpdateNetworkProfileResponse
--
--         , responseDeleteNetworkProfile $
--             newDeleteNetworkProfileResponse
--
--         , responseGetDevicePoolCompatibility $
--             newGetDevicePoolCompatibilityResponse
--
--         , responseInstallToRemoteAccessSession $
--             newInstallToRemoteAccessSessionResponse
--
--         , responseListTests $
--             newListTestsResponse
--
--         , responseListArtifacts $
--             newListArtifactsResponse
--
--         , responseListTestGridSessionActions $
--             newListTestGridSessionActionsResponse
--
--         , responseCreateUpload $
--             newCreateUploadResponse
--
--         , responseGetDeviceInstance $
--             newGetDeviceInstanceResponse
--
--         , responseStopJob $
--             newStopJobResponse
--
--         , responseDeleteRemoteAccessSession $
--             newDeleteRemoteAccessSessionResponse
--
--         , responseListTestGridSessionArtifacts $
--             newListTestGridSessionArtifactsResponse
--
--         , responseListTestGridProjects $
--             newListTestGridProjectsResponse
--
--         , responseDeleteUpload $
--             newDeleteUploadResponse
--
--         , responseUpdateUpload $
--             newUpdateUploadResponse
--
--         , responseDeleteTestGridProject $
--             newDeleteTestGridProjectResponse
--
--         , responseUpdateTestGridProject $
--             newUpdateTestGridProjectResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetDevicePool $
--             newGetDevicePoolResponse
--
--         , responseListDevicePools $
--             newListDevicePoolsResponse
--
--         , responseUpdateDevicePool $
--             newUpdateDevicePoolResponse
--
--         , responseDeleteDevicePool $
--             newDeleteDevicePoolResponse
--
--         , responseGetUpload $
--             newGetUploadResponse
--
--         , responseListOfferingTransactions $
--             newListOfferingTransactionsResponse
--
--         , responseCreateDevicePool $
--             newCreateDevicePoolResponse
--
--         , responseDeleteRun $
--             newDeleteRunResponse
--
--         , responseListRuns $
--             newListRunsResponse
--
--         , responseGetTest $
--             newGetTestResponse
--
--         , responseUpdateDeviceInstance $
--             newUpdateDeviceInstanceResponse
--
--         , responseGetNetworkProfile $
--             newGetNetworkProfileResponse
--
--         , responseRenewOffering $
--             newRenewOfferingResponse
--
--         , responseDeleteInstanceProfile $
--             newDeleteInstanceProfileResponse
--
--         , responseUpdateInstanceProfile $
--             newUpdateInstanceProfileResponse
--
--         , responseCreateInstanceProfile $
--             newCreateInstanceProfileResponse
--
--         , responseGetDevice $
--             newGetDeviceResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseGetTestGridSession $
--             newGetTestGridSessionResponse
--
--         , responseGetVPCEConfiguration $
--             newGetVPCEConfigurationResponse
--
--         , responseStopRemoteAccessSession $
--             newStopRemoteAccessSessionResponse
--
--         , responseCreateNetworkProfile $
--             newCreateNetworkProfileResponse
--
--         , responseDeleteVPCEConfiguration $
--             newDeleteVPCEConfigurationResponse
--
--         , responseUpdateVPCEConfiguration $
--             newUpdateVPCEConfigurationResponse
--
--         , responseGetJob $
--             newGetJobResponse
--
--         , responseGetInstanceProfile $
--             newGetInstanceProfileResponse
--
--         , responseListNetworkProfiles $
--             newListNetworkProfilesResponse
--
--         , responseCreateVPCEConfiguration $
--             newCreateVPCEConfigurationResponse
--
--         , responseScheduleRun $
--             newScheduleRunResponse
--
--         , responseCreateTestGridProject $
--             newCreateTestGridProjectResponse
--
--         , responseGetRun $
--             newGetRunResponse
--
--         , responseListSamples $
--             newListSamplesResponse
--
--         , responseListSuites $
--             newListSuitesResponse
--
--         , responseListRemoteAccessSessions $
--             newListRemoteAccessSessionsResponse
--
--         , responseGetAccountSettings $
--             newGetAccountSettingsResponse
--
--         , responseCreateRemoteAccessSession $
--             newCreateRemoteAccessSessionResponse
--
--         , responseListOfferingPromotions $
--             newListOfferingPromotionsResponse
--
--         , responseGetOfferingStatus $
--             newGetOfferingStatusResponse
--
--         , responseListUploads $
--             newListUploadsResponse
--
--         , responseGetTestGridProject $
--             newGetTestGridProjectResponse
--
--         , responseGetSuite $
--             newGetSuiteResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetRemoteAccessSession $
--             newGetRemoteAccessSessionResponse
--
--         , responseListDeviceInstances $
--             newListDeviceInstancesResponse
--
--         , responsePurchaseOffering $
--             newPurchaseOfferingResponse
--
--         , responseListInstanceProfiles $
--             newListInstanceProfilesResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetProject $
--             newGetProjectResponse
--
--         , responseListUniqueProblems $
--             newListUniqueProblemsResponse
--
--         , responseListVPCEConfigurations $
--             newListVPCEConfigurationsResponse
--
--         , responseStopRun $
--             newStopRunResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseListTestGridSessions $
--             newListTestGridSessionsResponse
--
--         , responseCreateTestGridUrl $
--             newCreateTestGridUrlResponse
--
--         , responseListOfferings $
--             newListOfferingsResponse
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

-- Responses

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjects)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProject)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProject)

responseUpdateNetworkProfile :: UpdateNetworkProfileResponse -> TestTree
responseUpdateNetworkProfile =
  res
    "UpdateNetworkProfileResponse"
    "fixture/UpdateNetworkProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNetworkProfile)

responseDeleteNetworkProfile :: DeleteNetworkProfileResponse -> TestTree
responseDeleteNetworkProfile =
  res
    "DeleteNetworkProfileResponse"
    "fixture/DeleteNetworkProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkProfile)

responseGetDevicePoolCompatibility :: GetDevicePoolCompatibilityResponse -> TestTree
responseGetDevicePoolCompatibility =
  res
    "GetDevicePoolCompatibilityResponse"
    "fixture/GetDevicePoolCompatibilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevicePoolCompatibility)

responseInstallToRemoteAccessSession :: InstallToRemoteAccessSessionResponse -> TestTree
responseInstallToRemoteAccessSession =
  res
    "InstallToRemoteAccessSessionResponse"
    "fixture/InstallToRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InstallToRemoteAccessSession)

responseListTests :: ListTestsResponse -> TestTree
responseListTests =
  res
    "ListTestsResponse"
    "fixture/ListTestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTests)

responseListArtifacts :: ListArtifactsResponse -> TestTree
responseListArtifacts =
  res
    "ListArtifactsResponse"
    "fixture/ListArtifactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListArtifacts)

responseListTestGridSessionActions :: ListTestGridSessionActionsResponse -> TestTree
responseListTestGridSessionActions =
  res
    "ListTestGridSessionActionsResponse"
    "fixture/ListTestGridSessionActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTestGridSessionActions)

responseCreateUpload :: CreateUploadResponse -> TestTree
responseCreateUpload =
  res
    "CreateUploadResponse"
    "fixture/CreateUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUpload)

responseGetDeviceInstance :: GetDeviceInstanceResponse -> TestTree
responseGetDeviceInstance =
  res
    "GetDeviceInstanceResponse"
    "fixture/GetDeviceInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeviceInstance)

responseStopJob :: StopJobResponse -> TestTree
responseStopJob =
  res
    "StopJobResponse"
    "fixture/StopJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopJob)

responseDeleteRemoteAccessSession :: DeleteRemoteAccessSessionResponse -> TestTree
responseDeleteRemoteAccessSession =
  res
    "DeleteRemoteAccessSessionResponse"
    "fixture/DeleteRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRemoteAccessSession)

responseListTestGridSessionArtifacts :: ListTestGridSessionArtifactsResponse -> TestTree
responseListTestGridSessionArtifacts =
  res
    "ListTestGridSessionArtifactsResponse"
    "fixture/ListTestGridSessionArtifactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTestGridSessionArtifacts)

responseListTestGridProjects :: ListTestGridProjectsResponse -> TestTree
responseListTestGridProjects =
  res
    "ListTestGridProjectsResponse"
    "fixture/ListTestGridProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTestGridProjects)

responseDeleteUpload :: DeleteUploadResponse -> TestTree
responseDeleteUpload =
  res
    "DeleteUploadResponse"
    "fixture/DeleteUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUpload)

responseUpdateUpload :: UpdateUploadResponse -> TestTree
responseUpdateUpload =
  res
    "UpdateUploadResponse"
    "fixture/UpdateUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUpload)

responseDeleteTestGridProject :: DeleteTestGridProjectResponse -> TestTree
responseDeleteTestGridProject =
  res
    "DeleteTestGridProjectResponse"
    "fixture/DeleteTestGridProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTestGridProject)

responseUpdateTestGridProject :: UpdateTestGridProjectResponse -> TestTree
responseUpdateTestGridProject =
  res
    "UpdateTestGridProjectResponse"
    "fixture/UpdateTestGridProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTestGridProject)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseGetDevicePool :: GetDevicePoolResponse -> TestTree
responseGetDevicePool =
  res
    "GetDevicePoolResponse"
    "fixture/GetDevicePoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevicePool)

responseListDevicePools :: ListDevicePoolsResponse -> TestTree
responseListDevicePools =
  res
    "ListDevicePoolsResponse"
    "fixture/ListDevicePoolsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevicePools)

responseUpdateDevicePool :: UpdateDevicePoolResponse -> TestTree
responseUpdateDevicePool =
  res
    "UpdateDevicePoolResponse"
    "fixture/UpdateDevicePoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDevicePool)

responseDeleteDevicePool :: DeleteDevicePoolResponse -> TestTree
responseDeleteDevicePool =
  res
    "DeleteDevicePoolResponse"
    "fixture/DeleteDevicePoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDevicePool)

responseGetUpload :: GetUploadResponse -> TestTree
responseGetUpload =
  res
    "GetUploadResponse"
    "fixture/GetUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUpload)

responseListOfferingTransactions :: ListOfferingTransactionsResponse -> TestTree
responseListOfferingTransactions =
  res
    "ListOfferingTransactionsResponse"
    "fixture/ListOfferingTransactionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOfferingTransactions)

responseCreateDevicePool :: CreateDevicePoolResponse -> TestTree
responseCreateDevicePool =
  res
    "CreateDevicePoolResponse"
    "fixture/CreateDevicePoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDevicePool)

responseDeleteRun :: DeleteRunResponse -> TestTree
responseDeleteRun =
  res
    "DeleteRunResponse"
    "fixture/DeleteRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRun)

responseListRuns :: ListRunsResponse -> TestTree
responseListRuns =
  res
    "ListRunsResponse"
    "fixture/ListRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRuns)

responseGetTest :: GetTestResponse -> TestTree
responseGetTest =
  res
    "GetTestResponse"
    "fixture/GetTestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTest)

responseUpdateDeviceInstance :: UpdateDeviceInstanceResponse -> TestTree
responseUpdateDeviceInstance =
  res
    "UpdateDeviceInstanceResponse"
    "fixture/UpdateDeviceInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeviceInstance)

responseGetNetworkProfile :: GetNetworkProfileResponse -> TestTree
responseGetNetworkProfile =
  res
    "GetNetworkProfileResponse"
    "fixture/GetNetworkProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNetworkProfile)

responseRenewOffering :: RenewOfferingResponse -> TestTree
responseRenewOffering =
  res
    "RenewOfferingResponse"
    "fixture/RenewOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RenewOffering)

responseDeleteInstanceProfile :: DeleteInstanceProfileResponse -> TestTree
responseDeleteInstanceProfile =
  res
    "DeleteInstanceProfileResponse"
    "fixture/DeleteInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInstanceProfile)

responseUpdateInstanceProfile :: UpdateInstanceProfileResponse -> TestTree
responseUpdateInstanceProfile =
  res
    "UpdateInstanceProfileResponse"
    "fixture/UpdateInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInstanceProfile)

responseCreateInstanceProfile :: CreateInstanceProfileResponse -> TestTree
responseCreateInstanceProfile =
  res
    "CreateInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstanceProfile)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice =
  res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevice)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseGetTestGridSession :: GetTestGridSessionResponse -> TestTree
responseGetTestGridSession =
  res
    "GetTestGridSessionResponse"
    "fixture/GetTestGridSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTestGridSession)

responseGetVPCEConfiguration :: GetVPCEConfigurationResponse -> TestTree
responseGetVPCEConfiguration =
  res
    "GetVPCEConfigurationResponse"
    "fixture/GetVPCEConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVPCEConfiguration)

responseStopRemoteAccessSession :: StopRemoteAccessSessionResponse -> TestTree
responseStopRemoteAccessSession =
  res
    "StopRemoteAccessSessionResponse"
    "fixture/StopRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopRemoteAccessSession)

responseCreateNetworkProfile :: CreateNetworkProfileResponse -> TestTree
responseCreateNetworkProfile =
  res
    "CreateNetworkProfileResponse"
    "fixture/CreateNetworkProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetworkProfile)

responseDeleteVPCEConfiguration :: DeleteVPCEConfigurationResponse -> TestTree
responseDeleteVPCEConfiguration =
  res
    "DeleteVPCEConfigurationResponse"
    "fixture/DeleteVPCEConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVPCEConfiguration)

responseUpdateVPCEConfiguration :: UpdateVPCEConfigurationResponse -> TestTree
responseUpdateVPCEConfiguration =
  res
    "UpdateVPCEConfigurationResponse"
    "fixture/UpdateVPCEConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVPCEConfiguration)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJob)

responseGetInstanceProfile :: GetInstanceProfileResponse -> TestTree
responseGetInstanceProfile =
  res
    "GetInstanceProfileResponse"
    "fixture/GetInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceProfile)

responseListNetworkProfiles :: ListNetworkProfilesResponse -> TestTree
responseListNetworkProfiles =
  res
    "ListNetworkProfilesResponse"
    "fixture/ListNetworkProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNetworkProfiles)

responseCreateVPCEConfiguration :: CreateVPCEConfigurationResponse -> TestTree
responseCreateVPCEConfiguration =
  res
    "CreateVPCEConfigurationResponse"
    "fixture/CreateVPCEConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVPCEConfiguration)

responseScheduleRun :: ScheduleRunResponse -> TestTree
responseScheduleRun =
  res
    "ScheduleRunResponse"
    "fixture/ScheduleRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ScheduleRun)

responseCreateTestGridProject :: CreateTestGridProjectResponse -> TestTree
responseCreateTestGridProject =
  res
    "CreateTestGridProjectResponse"
    "fixture/CreateTestGridProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTestGridProject)

responseGetRun :: GetRunResponse -> TestTree
responseGetRun =
  res
    "GetRunResponse"
    "fixture/GetRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRun)

responseListSamples :: ListSamplesResponse -> TestTree
responseListSamples =
  res
    "ListSamplesResponse"
    "fixture/ListSamplesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSamples)

responseListSuites :: ListSuitesResponse -> TestTree
responseListSuites =
  res
    "ListSuitesResponse"
    "fixture/ListSuitesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSuites)

responseListRemoteAccessSessions :: ListRemoteAccessSessionsResponse -> TestTree
responseListRemoteAccessSessions =
  res
    "ListRemoteAccessSessionsResponse"
    "fixture/ListRemoteAccessSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRemoteAccessSessions)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountSettings)

responseCreateRemoteAccessSession :: CreateRemoteAccessSessionResponse -> TestTree
responseCreateRemoteAccessSession =
  res
    "CreateRemoteAccessSessionResponse"
    "fixture/CreateRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRemoteAccessSession)

responseListOfferingPromotions :: ListOfferingPromotionsResponse -> TestTree
responseListOfferingPromotions =
  res
    "ListOfferingPromotionsResponse"
    "fixture/ListOfferingPromotionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOfferingPromotions)

responseGetOfferingStatus :: GetOfferingStatusResponse -> TestTree
responseGetOfferingStatus =
  res
    "GetOfferingStatusResponse"
    "fixture/GetOfferingStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOfferingStatus)

responseListUploads :: ListUploadsResponse -> TestTree
responseListUploads =
  res
    "ListUploadsResponse"
    "fixture/ListUploadsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUploads)

responseGetTestGridProject :: GetTestGridProjectResponse -> TestTree
responseGetTestGridProject =
  res
    "GetTestGridProjectResponse"
    "fixture/GetTestGridProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTestGridProject)

responseGetSuite :: GetSuiteResponse -> TestTree
responseGetSuite =
  res
    "GetSuiteResponse"
    "fixture/GetSuiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSuite)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseGetRemoteAccessSession :: GetRemoteAccessSessionResponse -> TestTree
responseGetRemoteAccessSession =
  res
    "GetRemoteAccessSessionResponse"
    "fixture/GetRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRemoteAccessSession)

responseListDeviceInstances :: ListDeviceInstancesResponse -> TestTree
responseListDeviceInstances =
  res
    "ListDeviceInstancesResponse"
    "fixture/ListDeviceInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeviceInstances)

responsePurchaseOffering :: PurchaseOfferingResponse -> TestTree
responsePurchaseOffering =
  res
    "PurchaseOfferingResponse"
    "fixture/PurchaseOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseOffering)

responseListInstanceProfiles :: ListInstanceProfilesResponse -> TestTree
responseListInstanceProfiles =
  res
    "ListInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstanceProfiles)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseGetProject :: GetProjectResponse -> TestTree
responseGetProject =
  res
    "GetProjectResponse"
    "fixture/GetProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProject)

responseListUniqueProblems :: ListUniqueProblemsResponse -> TestTree
responseListUniqueProblems =
  res
    "ListUniqueProblemsResponse"
    "fixture/ListUniqueProblemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUniqueProblems)

responseListVPCEConfigurations :: ListVPCEConfigurationsResponse -> TestTree
responseListVPCEConfigurations =
  res
    "ListVPCEConfigurationsResponse"
    "fixture/ListVPCEConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVPCEConfigurations)

responseStopRun :: StopRunResponse -> TestTree
responseStopRun =
  res
    "StopRunResponse"
    "fixture/StopRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopRun)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevices)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)

responseListTestGridSessions :: ListTestGridSessionsResponse -> TestTree
responseListTestGridSessions =
  res
    "ListTestGridSessionsResponse"
    "fixture/ListTestGridSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTestGridSessions)

responseCreateTestGridUrl :: CreateTestGridUrlResponse -> TestTree
responseCreateTestGridUrl =
  res
    "CreateTestGridUrlResponse"
    "fixture/CreateTestGridUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTestGridUrl)

responseListOfferings :: ListOfferingsResponse -> TestTree
responseListOfferings =
  res
    "ListOfferingsResponse"
    "fixture/ListOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOfferings)
