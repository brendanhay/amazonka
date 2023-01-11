{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DeviceFarm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.DeviceFarm where

import Amazonka.DeviceFarm
import qualified Data.Proxy as Proxy
import Test.Amazonka.DeviceFarm.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateDevicePool $
--             newCreateDevicePool
--
--         , requestCreateInstanceProfile $
--             newCreateInstanceProfile
--
--         , requestCreateNetworkProfile $
--             newCreateNetworkProfile
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestCreateRemoteAccessSession $
--             newCreateRemoteAccessSession
--
--         , requestCreateTestGridProject $
--             newCreateTestGridProject
--
--         , requestCreateTestGridUrl $
--             newCreateTestGridUrl
--
--         , requestCreateUpload $
--             newCreateUpload
--
--         , requestCreateVPCEConfiguration $
--             newCreateVPCEConfiguration
--
--         , requestDeleteDevicePool $
--             newDeleteDevicePool
--
--         , requestDeleteInstanceProfile $
--             newDeleteInstanceProfile
--
--         , requestDeleteNetworkProfile $
--             newDeleteNetworkProfile
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestDeleteRemoteAccessSession $
--             newDeleteRemoteAccessSession
--
--         , requestDeleteRun $
--             newDeleteRun
--
--         , requestDeleteTestGridProject $
--             newDeleteTestGridProject
--
--         , requestDeleteUpload $
--             newDeleteUpload
--
--         , requestDeleteVPCEConfiguration $
--             newDeleteVPCEConfiguration
--
--         , requestGetAccountSettings $
--             newGetAccountSettings
--
--         , requestGetDevice $
--             newGetDevice
--
--         , requestGetDeviceInstance $
--             newGetDeviceInstance
--
--         , requestGetDevicePool $
--             newGetDevicePool
--
--         , requestGetDevicePoolCompatibility $
--             newGetDevicePoolCompatibility
--
--         , requestGetInstanceProfile $
--             newGetInstanceProfile
--
--         , requestGetJob $
--             newGetJob
--
--         , requestGetNetworkProfile $
--             newGetNetworkProfile
--
--         , requestGetOfferingStatus $
--             newGetOfferingStatus
--
--         , requestGetProject $
--             newGetProject
--
--         , requestGetRemoteAccessSession $
--             newGetRemoteAccessSession
--
--         , requestGetRun $
--             newGetRun
--
--         , requestGetSuite $
--             newGetSuite
--
--         , requestGetTest $
--             newGetTest
--
--         , requestGetTestGridProject $
--             newGetTestGridProject
--
--         , requestGetTestGridSession $
--             newGetTestGridSession
--
--         , requestGetUpload $
--             newGetUpload
--
--         , requestGetVPCEConfiguration $
--             newGetVPCEConfiguration
--
--         , requestInstallToRemoteAccessSession $
--             newInstallToRemoteAccessSession
--
--         , requestListArtifacts $
--             newListArtifacts
--
--         , requestListDeviceInstances $
--             newListDeviceInstances
--
--         , requestListDevicePools $
--             newListDevicePools
--
--         , requestListDevices $
--             newListDevices
--
--         , requestListInstanceProfiles $
--             newListInstanceProfiles
--
--         , requestListJobs $
--             newListJobs
--
--         , requestListNetworkProfiles $
--             newListNetworkProfiles
--
--         , requestListOfferingPromotions $
--             newListOfferingPromotions
--
--         , requestListOfferingTransactions $
--             newListOfferingTransactions
--
--         , requestListOfferings $
--             newListOfferings
--
--         , requestListProjects $
--             newListProjects
--
--         , requestListRemoteAccessSessions $
--             newListRemoteAccessSessions
--
--         , requestListRuns $
--             newListRuns
--
--         , requestListSamples $
--             newListSamples
--
--         , requestListSuites $
--             newListSuites
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTestGridProjects $
--             newListTestGridProjects
--
--         , requestListTestGridSessionActions $
--             newListTestGridSessionActions
--
--         , requestListTestGridSessionArtifacts $
--             newListTestGridSessionArtifacts
--
--         , requestListTestGridSessions $
--             newListTestGridSessions
--
--         , requestListTests $
--             newListTests
--
--         , requestListUniqueProblems $
--             newListUniqueProblems
--
--         , requestListUploads $
--             newListUploads
--
--         , requestListVPCEConfigurations $
--             newListVPCEConfigurations
--
--         , requestPurchaseOffering $
--             newPurchaseOffering
--
--         , requestRenewOffering $
--             newRenewOffering
--
--         , requestScheduleRun $
--             newScheduleRun
--
--         , requestStopJob $
--             newStopJob
--
--         , requestStopRemoteAccessSession $
--             newStopRemoteAccessSession
--
--         , requestStopRun $
--             newStopRun
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDeviceInstance $
--             newUpdateDeviceInstance
--
--         , requestUpdateDevicePool $
--             newUpdateDevicePool
--
--         , requestUpdateInstanceProfile $
--             newUpdateInstanceProfile
--
--         , requestUpdateNetworkProfile $
--             newUpdateNetworkProfile
--
--         , requestUpdateProject $
--             newUpdateProject
--
--         , requestUpdateTestGridProject $
--             newUpdateTestGridProject
--
--         , requestUpdateUpload $
--             newUpdateUpload
--
--         , requestUpdateVPCEConfiguration $
--             newUpdateVPCEConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseCreateDevicePool $
--             newCreateDevicePoolResponse
--
--         , responseCreateInstanceProfile $
--             newCreateInstanceProfileResponse
--
--         , responseCreateNetworkProfile $
--             newCreateNetworkProfileResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseCreateRemoteAccessSession $
--             newCreateRemoteAccessSessionResponse
--
--         , responseCreateTestGridProject $
--             newCreateTestGridProjectResponse
--
--         , responseCreateTestGridUrl $
--             newCreateTestGridUrlResponse
--
--         , responseCreateUpload $
--             newCreateUploadResponse
--
--         , responseCreateVPCEConfiguration $
--             newCreateVPCEConfigurationResponse
--
--         , responseDeleteDevicePool $
--             newDeleteDevicePoolResponse
--
--         , responseDeleteInstanceProfile $
--             newDeleteInstanceProfileResponse
--
--         , responseDeleteNetworkProfile $
--             newDeleteNetworkProfileResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseDeleteRemoteAccessSession $
--             newDeleteRemoteAccessSessionResponse
--
--         , responseDeleteRun $
--             newDeleteRunResponse
--
--         , responseDeleteTestGridProject $
--             newDeleteTestGridProjectResponse
--
--         , responseDeleteUpload $
--             newDeleteUploadResponse
--
--         , responseDeleteVPCEConfiguration $
--             newDeleteVPCEConfigurationResponse
--
--         , responseGetAccountSettings $
--             newGetAccountSettingsResponse
--
--         , responseGetDevice $
--             newGetDeviceResponse
--
--         , responseGetDeviceInstance $
--             newGetDeviceInstanceResponse
--
--         , responseGetDevicePool $
--             newGetDevicePoolResponse
--
--         , responseGetDevicePoolCompatibility $
--             newGetDevicePoolCompatibilityResponse
--
--         , responseGetInstanceProfile $
--             newGetInstanceProfileResponse
--
--         , responseGetJob $
--             newGetJobResponse
--
--         , responseGetNetworkProfile $
--             newGetNetworkProfileResponse
--
--         , responseGetOfferingStatus $
--             newGetOfferingStatusResponse
--
--         , responseGetProject $
--             newGetProjectResponse
--
--         , responseGetRemoteAccessSession $
--             newGetRemoteAccessSessionResponse
--
--         , responseGetRun $
--             newGetRunResponse
--
--         , responseGetSuite $
--             newGetSuiteResponse
--
--         , responseGetTest $
--             newGetTestResponse
--
--         , responseGetTestGridProject $
--             newGetTestGridProjectResponse
--
--         , responseGetTestGridSession $
--             newGetTestGridSessionResponse
--
--         , responseGetUpload $
--             newGetUploadResponse
--
--         , responseGetVPCEConfiguration $
--             newGetVPCEConfigurationResponse
--
--         , responseInstallToRemoteAccessSession $
--             newInstallToRemoteAccessSessionResponse
--
--         , responseListArtifacts $
--             newListArtifactsResponse
--
--         , responseListDeviceInstances $
--             newListDeviceInstancesResponse
--
--         , responseListDevicePools $
--             newListDevicePoolsResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--         , responseListInstanceProfiles $
--             newListInstanceProfilesResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseListNetworkProfiles $
--             newListNetworkProfilesResponse
--
--         , responseListOfferingPromotions $
--             newListOfferingPromotionsResponse
--
--         , responseListOfferingTransactions $
--             newListOfferingTransactionsResponse
--
--         , responseListOfferings $
--             newListOfferingsResponse
--
--         , responseListProjects $
--             newListProjectsResponse
--
--         , responseListRemoteAccessSessions $
--             newListRemoteAccessSessionsResponse
--
--         , responseListRuns $
--             newListRunsResponse
--
--         , responseListSamples $
--             newListSamplesResponse
--
--         , responseListSuites $
--             newListSuitesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTestGridProjects $
--             newListTestGridProjectsResponse
--
--         , responseListTestGridSessionActions $
--             newListTestGridSessionActionsResponse
--
--         , responseListTestGridSessionArtifacts $
--             newListTestGridSessionArtifactsResponse
--
--         , responseListTestGridSessions $
--             newListTestGridSessionsResponse
--
--         , responseListTests $
--             newListTestsResponse
--
--         , responseListUniqueProblems $
--             newListUniqueProblemsResponse
--
--         , responseListUploads $
--             newListUploadsResponse
--
--         , responseListVPCEConfigurations $
--             newListVPCEConfigurationsResponse
--
--         , responsePurchaseOffering $
--             newPurchaseOfferingResponse
--
--         , responseRenewOffering $
--             newRenewOfferingResponse
--
--         , responseScheduleRun $
--             newScheduleRunResponse
--
--         , responseStopJob $
--             newStopJobResponse
--
--         , responseStopRemoteAccessSession $
--             newStopRemoteAccessSessionResponse
--
--         , responseStopRun $
--             newStopRunResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDeviceInstance $
--             newUpdateDeviceInstanceResponse
--
--         , responseUpdateDevicePool $
--             newUpdateDevicePoolResponse
--
--         , responseUpdateInstanceProfile $
--             newUpdateInstanceProfileResponse
--
--         , responseUpdateNetworkProfile $
--             newUpdateNetworkProfileResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--         , responseUpdateTestGridProject $
--             newUpdateTestGridProjectResponse
--
--         , responseUpdateUpload $
--             newUpdateUploadResponse
--
--         , responseUpdateVPCEConfiguration $
--             newUpdateVPCEConfigurationResponse
--
--           ]
--     ]

-- Requests

requestCreateDevicePool :: CreateDevicePool -> TestTree
requestCreateDevicePool =
  req
    "CreateDevicePool"
    "fixture/CreateDevicePool.yaml"

requestCreateInstanceProfile :: CreateInstanceProfile -> TestTree
requestCreateInstanceProfile =
  req
    "CreateInstanceProfile"
    "fixture/CreateInstanceProfile.yaml"

requestCreateNetworkProfile :: CreateNetworkProfile -> TestTree
requestCreateNetworkProfile =
  req
    "CreateNetworkProfile"
    "fixture/CreateNetworkProfile.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestCreateRemoteAccessSession :: CreateRemoteAccessSession -> TestTree
requestCreateRemoteAccessSession =
  req
    "CreateRemoteAccessSession"
    "fixture/CreateRemoteAccessSession.yaml"

requestCreateTestGridProject :: CreateTestGridProject -> TestTree
requestCreateTestGridProject =
  req
    "CreateTestGridProject"
    "fixture/CreateTestGridProject.yaml"

requestCreateTestGridUrl :: CreateTestGridUrl -> TestTree
requestCreateTestGridUrl =
  req
    "CreateTestGridUrl"
    "fixture/CreateTestGridUrl.yaml"

requestCreateUpload :: CreateUpload -> TestTree
requestCreateUpload =
  req
    "CreateUpload"
    "fixture/CreateUpload.yaml"

requestCreateVPCEConfiguration :: CreateVPCEConfiguration -> TestTree
requestCreateVPCEConfiguration =
  req
    "CreateVPCEConfiguration"
    "fixture/CreateVPCEConfiguration.yaml"

requestDeleteDevicePool :: DeleteDevicePool -> TestTree
requestDeleteDevicePool =
  req
    "DeleteDevicePool"
    "fixture/DeleteDevicePool.yaml"

requestDeleteInstanceProfile :: DeleteInstanceProfile -> TestTree
requestDeleteInstanceProfile =
  req
    "DeleteInstanceProfile"
    "fixture/DeleteInstanceProfile.yaml"

requestDeleteNetworkProfile :: DeleteNetworkProfile -> TestTree
requestDeleteNetworkProfile =
  req
    "DeleteNetworkProfile"
    "fixture/DeleteNetworkProfile.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestDeleteRemoteAccessSession :: DeleteRemoteAccessSession -> TestTree
requestDeleteRemoteAccessSession =
  req
    "DeleteRemoteAccessSession"
    "fixture/DeleteRemoteAccessSession.yaml"

requestDeleteRun :: DeleteRun -> TestTree
requestDeleteRun =
  req
    "DeleteRun"
    "fixture/DeleteRun.yaml"

requestDeleteTestGridProject :: DeleteTestGridProject -> TestTree
requestDeleteTestGridProject =
  req
    "DeleteTestGridProject"
    "fixture/DeleteTestGridProject.yaml"

requestDeleteUpload :: DeleteUpload -> TestTree
requestDeleteUpload =
  req
    "DeleteUpload"
    "fixture/DeleteUpload.yaml"

requestDeleteVPCEConfiguration :: DeleteVPCEConfiguration -> TestTree
requestDeleteVPCEConfiguration =
  req
    "DeleteVPCEConfiguration"
    "fixture/DeleteVPCEConfiguration.yaml"

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings =
  req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

requestGetDevice :: GetDevice -> TestTree
requestGetDevice =
  req
    "GetDevice"
    "fixture/GetDevice.yaml"

requestGetDeviceInstance :: GetDeviceInstance -> TestTree
requestGetDeviceInstance =
  req
    "GetDeviceInstance"
    "fixture/GetDeviceInstance.yaml"

requestGetDevicePool :: GetDevicePool -> TestTree
requestGetDevicePool =
  req
    "GetDevicePool"
    "fixture/GetDevicePool.yaml"

requestGetDevicePoolCompatibility :: GetDevicePoolCompatibility -> TestTree
requestGetDevicePoolCompatibility =
  req
    "GetDevicePoolCompatibility"
    "fixture/GetDevicePoolCompatibility.yaml"

requestGetInstanceProfile :: GetInstanceProfile -> TestTree
requestGetInstanceProfile =
  req
    "GetInstanceProfile"
    "fixture/GetInstanceProfile.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob =
  req
    "GetJob"
    "fixture/GetJob.yaml"

requestGetNetworkProfile :: GetNetworkProfile -> TestTree
requestGetNetworkProfile =
  req
    "GetNetworkProfile"
    "fixture/GetNetworkProfile.yaml"

requestGetOfferingStatus :: GetOfferingStatus -> TestTree
requestGetOfferingStatus =
  req
    "GetOfferingStatus"
    "fixture/GetOfferingStatus.yaml"

requestGetProject :: GetProject -> TestTree
requestGetProject =
  req
    "GetProject"
    "fixture/GetProject.yaml"

requestGetRemoteAccessSession :: GetRemoteAccessSession -> TestTree
requestGetRemoteAccessSession =
  req
    "GetRemoteAccessSession"
    "fixture/GetRemoteAccessSession.yaml"

requestGetRun :: GetRun -> TestTree
requestGetRun =
  req
    "GetRun"
    "fixture/GetRun.yaml"

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

requestGetTestGridProject :: GetTestGridProject -> TestTree
requestGetTestGridProject =
  req
    "GetTestGridProject"
    "fixture/GetTestGridProject.yaml"

requestGetTestGridSession :: GetTestGridSession -> TestTree
requestGetTestGridSession =
  req
    "GetTestGridSession"
    "fixture/GetTestGridSession.yaml"

requestGetUpload :: GetUpload -> TestTree
requestGetUpload =
  req
    "GetUpload"
    "fixture/GetUpload.yaml"

requestGetVPCEConfiguration :: GetVPCEConfiguration -> TestTree
requestGetVPCEConfiguration =
  req
    "GetVPCEConfiguration"
    "fixture/GetVPCEConfiguration.yaml"

requestInstallToRemoteAccessSession :: InstallToRemoteAccessSession -> TestTree
requestInstallToRemoteAccessSession =
  req
    "InstallToRemoteAccessSession"
    "fixture/InstallToRemoteAccessSession.yaml"

requestListArtifacts :: ListArtifacts -> TestTree
requestListArtifacts =
  req
    "ListArtifacts"
    "fixture/ListArtifacts.yaml"

requestListDeviceInstances :: ListDeviceInstances -> TestTree
requestListDeviceInstances =
  req
    "ListDeviceInstances"
    "fixture/ListDeviceInstances.yaml"

requestListDevicePools :: ListDevicePools -> TestTree
requestListDevicePools =
  req
    "ListDevicePools"
    "fixture/ListDevicePools.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

requestListInstanceProfiles :: ListInstanceProfiles -> TestTree
requestListInstanceProfiles =
  req
    "ListInstanceProfiles"
    "fixture/ListInstanceProfiles.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestListNetworkProfiles :: ListNetworkProfiles -> TestTree
requestListNetworkProfiles =
  req
    "ListNetworkProfiles"
    "fixture/ListNetworkProfiles.yaml"

requestListOfferingPromotions :: ListOfferingPromotions -> TestTree
requestListOfferingPromotions =
  req
    "ListOfferingPromotions"
    "fixture/ListOfferingPromotions.yaml"

requestListOfferingTransactions :: ListOfferingTransactions -> TestTree
requestListOfferingTransactions =
  req
    "ListOfferingTransactions"
    "fixture/ListOfferingTransactions.yaml"

requestListOfferings :: ListOfferings -> TestTree
requestListOfferings =
  req
    "ListOfferings"
    "fixture/ListOfferings.yaml"

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestListRemoteAccessSessions :: ListRemoteAccessSessions -> TestTree
requestListRemoteAccessSessions =
  req
    "ListRemoteAccessSessions"
    "fixture/ListRemoteAccessSessions.yaml"

requestListRuns :: ListRuns -> TestTree
requestListRuns =
  req
    "ListRuns"
    "fixture/ListRuns.yaml"

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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTestGridProjects :: ListTestGridProjects -> TestTree
requestListTestGridProjects =
  req
    "ListTestGridProjects"
    "fixture/ListTestGridProjects.yaml"

requestListTestGridSessionActions :: ListTestGridSessionActions -> TestTree
requestListTestGridSessionActions =
  req
    "ListTestGridSessionActions"
    "fixture/ListTestGridSessionActions.yaml"

requestListTestGridSessionArtifacts :: ListTestGridSessionArtifacts -> TestTree
requestListTestGridSessionArtifacts =
  req
    "ListTestGridSessionArtifacts"
    "fixture/ListTestGridSessionArtifacts.yaml"

requestListTestGridSessions :: ListTestGridSessions -> TestTree
requestListTestGridSessions =
  req
    "ListTestGridSessions"
    "fixture/ListTestGridSessions.yaml"

requestListTests :: ListTests -> TestTree
requestListTests =
  req
    "ListTests"
    "fixture/ListTests.yaml"

requestListUniqueProblems :: ListUniqueProblems -> TestTree
requestListUniqueProblems =
  req
    "ListUniqueProblems"
    "fixture/ListUniqueProblems.yaml"

requestListUploads :: ListUploads -> TestTree
requestListUploads =
  req
    "ListUploads"
    "fixture/ListUploads.yaml"

requestListVPCEConfigurations :: ListVPCEConfigurations -> TestTree
requestListVPCEConfigurations =
  req
    "ListVPCEConfigurations"
    "fixture/ListVPCEConfigurations.yaml"

requestPurchaseOffering :: PurchaseOffering -> TestTree
requestPurchaseOffering =
  req
    "PurchaseOffering"
    "fixture/PurchaseOffering.yaml"

requestRenewOffering :: RenewOffering -> TestTree
requestRenewOffering =
  req
    "RenewOffering"
    "fixture/RenewOffering.yaml"

requestScheduleRun :: ScheduleRun -> TestTree
requestScheduleRun =
  req
    "ScheduleRun"
    "fixture/ScheduleRun.yaml"

requestStopJob :: StopJob -> TestTree
requestStopJob =
  req
    "StopJob"
    "fixture/StopJob.yaml"

requestStopRemoteAccessSession :: StopRemoteAccessSession -> TestTree
requestStopRemoteAccessSession =
  req
    "StopRemoteAccessSession"
    "fixture/StopRemoteAccessSession.yaml"

requestStopRun :: StopRun -> TestTree
requestStopRun =
  req
    "StopRun"
    "fixture/StopRun.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateDeviceInstance :: UpdateDeviceInstance -> TestTree
requestUpdateDeviceInstance =
  req
    "UpdateDeviceInstance"
    "fixture/UpdateDeviceInstance.yaml"

requestUpdateDevicePool :: UpdateDevicePool -> TestTree
requestUpdateDevicePool =
  req
    "UpdateDevicePool"
    "fixture/UpdateDevicePool.yaml"

requestUpdateInstanceProfile :: UpdateInstanceProfile -> TestTree
requestUpdateInstanceProfile =
  req
    "UpdateInstanceProfile"
    "fixture/UpdateInstanceProfile.yaml"

requestUpdateNetworkProfile :: UpdateNetworkProfile -> TestTree
requestUpdateNetworkProfile =
  req
    "UpdateNetworkProfile"
    "fixture/UpdateNetworkProfile.yaml"

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject =
  req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

requestUpdateTestGridProject :: UpdateTestGridProject -> TestTree
requestUpdateTestGridProject =
  req
    "UpdateTestGridProject"
    "fixture/UpdateTestGridProject.yaml"

requestUpdateUpload :: UpdateUpload -> TestTree
requestUpdateUpload =
  req
    "UpdateUpload"
    "fixture/UpdateUpload.yaml"

requestUpdateVPCEConfiguration :: UpdateVPCEConfiguration -> TestTree
requestUpdateVPCEConfiguration =
  req
    "UpdateVPCEConfiguration"
    "fixture/UpdateVPCEConfiguration.yaml"

-- Responses

responseCreateDevicePool :: CreateDevicePoolResponse -> TestTree
responseCreateDevicePool =
  res
    "CreateDevicePoolResponse"
    "fixture/CreateDevicePoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDevicePool)

responseCreateInstanceProfile :: CreateInstanceProfileResponse -> TestTree
responseCreateInstanceProfile =
  res
    "CreateInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstanceProfile)

responseCreateNetworkProfile :: CreateNetworkProfileResponse -> TestTree
responseCreateNetworkProfile =
  res
    "CreateNetworkProfileResponse"
    "fixture/CreateNetworkProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetworkProfile)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)

responseCreateRemoteAccessSession :: CreateRemoteAccessSessionResponse -> TestTree
responseCreateRemoteAccessSession =
  res
    "CreateRemoteAccessSessionResponse"
    "fixture/CreateRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRemoteAccessSession)

responseCreateTestGridProject :: CreateTestGridProjectResponse -> TestTree
responseCreateTestGridProject =
  res
    "CreateTestGridProjectResponse"
    "fixture/CreateTestGridProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTestGridProject)

responseCreateTestGridUrl :: CreateTestGridUrlResponse -> TestTree
responseCreateTestGridUrl =
  res
    "CreateTestGridUrlResponse"
    "fixture/CreateTestGridUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTestGridUrl)

responseCreateUpload :: CreateUploadResponse -> TestTree
responseCreateUpload =
  res
    "CreateUploadResponse"
    "fixture/CreateUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUpload)

responseCreateVPCEConfiguration :: CreateVPCEConfigurationResponse -> TestTree
responseCreateVPCEConfiguration =
  res
    "CreateVPCEConfigurationResponse"
    "fixture/CreateVPCEConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVPCEConfiguration)

responseDeleteDevicePool :: DeleteDevicePoolResponse -> TestTree
responseDeleteDevicePool =
  res
    "DeleteDevicePoolResponse"
    "fixture/DeleteDevicePoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDevicePool)

responseDeleteInstanceProfile :: DeleteInstanceProfileResponse -> TestTree
responseDeleteInstanceProfile =
  res
    "DeleteInstanceProfileResponse"
    "fixture/DeleteInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInstanceProfile)

responseDeleteNetworkProfile :: DeleteNetworkProfileResponse -> TestTree
responseDeleteNetworkProfile =
  res
    "DeleteNetworkProfileResponse"
    "fixture/DeleteNetworkProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkProfile)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProject)

responseDeleteRemoteAccessSession :: DeleteRemoteAccessSessionResponse -> TestTree
responseDeleteRemoteAccessSession =
  res
    "DeleteRemoteAccessSessionResponse"
    "fixture/DeleteRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRemoteAccessSession)

responseDeleteRun :: DeleteRunResponse -> TestTree
responseDeleteRun =
  res
    "DeleteRunResponse"
    "fixture/DeleteRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRun)

responseDeleteTestGridProject :: DeleteTestGridProjectResponse -> TestTree
responseDeleteTestGridProject =
  res
    "DeleteTestGridProjectResponse"
    "fixture/DeleteTestGridProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTestGridProject)

responseDeleteUpload :: DeleteUploadResponse -> TestTree
responseDeleteUpload =
  res
    "DeleteUploadResponse"
    "fixture/DeleteUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUpload)

responseDeleteVPCEConfiguration :: DeleteVPCEConfigurationResponse -> TestTree
responseDeleteVPCEConfiguration =
  res
    "DeleteVPCEConfigurationResponse"
    "fixture/DeleteVPCEConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVPCEConfiguration)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountSettings)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice =
  res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevice)

responseGetDeviceInstance :: GetDeviceInstanceResponse -> TestTree
responseGetDeviceInstance =
  res
    "GetDeviceInstanceResponse"
    "fixture/GetDeviceInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeviceInstance)

responseGetDevicePool :: GetDevicePoolResponse -> TestTree
responseGetDevicePool =
  res
    "GetDevicePoolResponse"
    "fixture/GetDevicePoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevicePool)

responseGetDevicePoolCompatibility :: GetDevicePoolCompatibilityResponse -> TestTree
responseGetDevicePoolCompatibility =
  res
    "GetDevicePoolCompatibilityResponse"
    "fixture/GetDevicePoolCompatibilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevicePoolCompatibility)

responseGetInstanceProfile :: GetInstanceProfileResponse -> TestTree
responseGetInstanceProfile =
  res
    "GetInstanceProfileResponse"
    "fixture/GetInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceProfile)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJob)

responseGetNetworkProfile :: GetNetworkProfileResponse -> TestTree
responseGetNetworkProfile =
  res
    "GetNetworkProfileResponse"
    "fixture/GetNetworkProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNetworkProfile)

responseGetOfferingStatus :: GetOfferingStatusResponse -> TestTree
responseGetOfferingStatus =
  res
    "GetOfferingStatusResponse"
    "fixture/GetOfferingStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOfferingStatus)

responseGetProject :: GetProjectResponse -> TestTree
responseGetProject =
  res
    "GetProjectResponse"
    "fixture/GetProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProject)

responseGetRemoteAccessSession :: GetRemoteAccessSessionResponse -> TestTree
responseGetRemoteAccessSession =
  res
    "GetRemoteAccessSessionResponse"
    "fixture/GetRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRemoteAccessSession)

responseGetRun :: GetRunResponse -> TestTree
responseGetRun =
  res
    "GetRunResponse"
    "fixture/GetRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRun)

responseGetSuite :: GetSuiteResponse -> TestTree
responseGetSuite =
  res
    "GetSuiteResponse"
    "fixture/GetSuiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSuite)

responseGetTest :: GetTestResponse -> TestTree
responseGetTest =
  res
    "GetTestResponse"
    "fixture/GetTestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTest)

responseGetTestGridProject :: GetTestGridProjectResponse -> TestTree
responseGetTestGridProject =
  res
    "GetTestGridProjectResponse"
    "fixture/GetTestGridProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTestGridProject)

responseGetTestGridSession :: GetTestGridSessionResponse -> TestTree
responseGetTestGridSession =
  res
    "GetTestGridSessionResponse"
    "fixture/GetTestGridSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTestGridSession)

responseGetUpload :: GetUploadResponse -> TestTree
responseGetUpload =
  res
    "GetUploadResponse"
    "fixture/GetUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUpload)

responseGetVPCEConfiguration :: GetVPCEConfigurationResponse -> TestTree
responseGetVPCEConfiguration =
  res
    "GetVPCEConfigurationResponse"
    "fixture/GetVPCEConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVPCEConfiguration)

responseInstallToRemoteAccessSession :: InstallToRemoteAccessSessionResponse -> TestTree
responseInstallToRemoteAccessSession =
  res
    "InstallToRemoteAccessSessionResponse"
    "fixture/InstallToRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InstallToRemoteAccessSession)

responseListArtifacts :: ListArtifactsResponse -> TestTree
responseListArtifacts =
  res
    "ListArtifactsResponse"
    "fixture/ListArtifactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListArtifacts)

responseListDeviceInstances :: ListDeviceInstancesResponse -> TestTree
responseListDeviceInstances =
  res
    "ListDeviceInstancesResponse"
    "fixture/ListDeviceInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeviceInstances)

responseListDevicePools :: ListDevicePoolsResponse -> TestTree
responseListDevicePools =
  res
    "ListDevicePoolsResponse"
    "fixture/ListDevicePoolsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevicePools)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevices)

responseListInstanceProfiles :: ListInstanceProfilesResponse -> TestTree
responseListInstanceProfiles =
  res
    "ListInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstanceProfiles)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseListNetworkProfiles :: ListNetworkProfilesResponse -> TestTree
responseListNetworkProfiles =
  res
    "ListNetworkProfilesResponse"
    "fixture/ListNetworkProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNetworkProfiles)

responseListOfferingPromotions :: ListOfferingPromotionsResponse -> TestTree
responseListOfferingPromotions =
  res
    "ListOfferingPromotionsResponse"
    "fixture/ListOfferingPromotionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOfferingPromotions)

responseListOfferingTransactions :: ListOfferingTransactionsResponse -> TestTree
responseListOfferingTransactions =
  res
    "ListOfferingTransactionsResponse"
    "fixture/ListOfferingTransactionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOfferingTransactions)

responseListOfferings :: ListOfferingsResponse -> TestTree
responseListOfferings =
  res
    "ListOfferingsResponse"
    "fixture/ListOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOfferings)

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjects)

responseListRemoteAccessSessions :: ListRemoteAccessSessionsResponse -> TestTree
responseListRemoteAccessSessions =
  res
    "ListRemoteAccessSessionsResponse"
    "fixture/ListRemoteAccessSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRemoteAccessSessions)

responseListRuns :: ListRunsResponse -> TestTree
responseListRuns =
  res
    "ListRunsResponse"
    "fixture/ListRunsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRuns)

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

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTestGridProjects :: ListTestGridProjectsResponse -> TestTree
responseListTestGridProjects =
  res
    "ListTestGridProjectsResponse"
    "fixture/ListTestGridProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTestGridProjects)

responseListTestGridSessionActions :: ListTestGridSessionActionsResponse -> TestTree
responseListTestGridSessionActions =
  res
    "ListTestGridSessionActionsResponse"
    "fixture/ListTestGridSessionActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTestGridSessionActions)

responseListTestGridSessionArtifacts :: ListTestGridSessionArtifactsResponse -> TestTree
responseListTestGridSessionArtifacts =
  res
    "ListTestGridSessionArtifactsResponse"
    "fixture/ListTestGridSessionArtifactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTestGridSessionArtifacts)

responseListTestGridSessions :: ListTestGridSessionsResponse -> TestTree
responseListTestGridSessions =
  res
    "ListTestGridSessionsResponse"
    "fixture/ListTestGridSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTestGridSessions)

responseListTests :: ListTestsResponse -> TestTree
responseListTests =
  res
    "ListTestsResponse"
    "fixture/ListTestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTests)

responseListUniqueProblems :: ListUniqueProblemsResponse -> TestTree
responseListUniqueProblems =
  res
    "ListUniqueProblemsResponse"
    "fixture/ListUniqueProblemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUniqueProblems)

responseListUploads :: ListUploadsResponse -> TestTree
responseListUploads =
  res
    "ListUploadsResponse"
    "fixture/ListUploadsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUploads)

responseListVPCEConfigurations :: ListVPCEConfigurationsResponse -> TestTree
responseListVPCEConfigurations =
  res
    "ListVPCEConfigurationsResponse"
    "fixture/ListVPCEConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVPCEConfigurations)

responsePurchaseOffering :: PurchaseOfferingResponse -> TestTree
responsePurchaseOffering =
  res
    "PurchaseOfferingResponse"
    "fixture/PurchaseOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseOffering)

responseRenewOffering :: RenewOfferingResponse -> TestTree
responseRenewOffering =
  res
    "RenewOfferingResponse"
    "fixture/RenewOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RenewOffering)

responseScheduleRun :: ScheduleRunResponse -> TestTree
responseScheduleRun =
  res
    "ScheduleRunResponse"
    "fixture/ScheduleRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ScheduleRun)

responseStopJob :: StopJobResponse -> TestTree
responseStopJob =
  res
    "StopJobResponse"
    "fixture/StopJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopJob)

responseStopRemoteAccessSession :: StopRemoteAccessSessionResponse -> TestTree
responseStopRemoteAccessSession =
  res
    "StopRemoteAccessSessionResponse"
    "fixture/StopRemoteAccessSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopRemoteAccessSession)

responseStopRun :: StopRunResponse -> TestTree
responseStopRun =
  res
    "StopRunResponse"
    "fixture/StopRunResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopRun)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateDeviceInstance :: UpdateDeviceInstanceResponse -> TestTree
responseUpdateDeviceInstance =
  res
    "UpdateDeviceInstanceResponse"
    "fixture/UpdateDeviceInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeviceInstance)

responseUpdateDevicePool :: UpdateDevicePoolResponse -> TestTree
responseUpdateDevicePool =
  res
    "UpdateDevicePoolResponse"
    "fixture/UpdateDevicePoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDevicePool)

responseUpdateInstanceProfile :: UpdateInstanceProfileResponse -> TestTree
responseUpdateInstanceProfile =
  res
    "UpdateInstanceProfileResponse"
    "fixture/UpdateInstanceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInstanceProfile)

responseUpdateNetworkProfile :: UpdateNetworkProfileResponse -> TestTree
responseUpdateNetworkProfile =
  res
    "UpdateNetworkProfileResponse"
    "fixture/UpdateNetworkProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNetworkProfile)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProject)

responseUpdateTestGridProject :: UpdateTestGridProjectResponse -> TestTree
responseUpdateTestGridProject =
  res
    "UpdateTestGridProjectResponse"
    "fixture/UpdateTestGridProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTestGridProject)

responseUpdateUpload :: UpdateUploadResponse -> TestTree
responseUpdateUpload =
  res
    "UpdateUploadResponse"
    "fixture/UpdateUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUpload)

responseUpdateVPCEConfiguration :: UpdateVPCEConfigurationResponse -> TestTree
responseUpdateVPCEConfiguration =
  res
    "UpdateVPCEConfigurationResponse"
    "fixture/UpdateVPCEConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVPCEConfiguration)
