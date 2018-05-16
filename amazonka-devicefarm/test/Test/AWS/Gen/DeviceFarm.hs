{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DeviceFarm
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         , requestCreateUpload $
--             createUpload
--
--         , requestGetDeviceInstance $
--             getDeviceInstance
--
--         , requestDeleteRemoteAccessSession $
--             deleteRemoteAccessSession
--
--         , requestDeleteUpload $
--             deleteUpload
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
--         , requestGetSuite $
--             getSuite
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
--         , responseCreateUpload $
--             createUploadResponse
--
--         , responseGetDeviceInstance $
--             getDeviceInstanceResponse
--
--         , responseDeleteRemoteAccessSession $
--             deleteRemoteAccessSessionResponse
--
--         , responseDeleteUpload $
--             deleteUploadResponse
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
--         , responseGetSuite $
--             getSuiteResponse
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
--         , responseListOfferings $
--             listOfferingsResponse
--
--           ]
--     ]

-- Requests

requestListProjects :: ListProjects -> TestTree
requestListProjects = req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject = req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject = req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

requestUpdateNetworkProfile :: UpdateNetworkProfile -> TestTree
requestUpdateNetworkProfile = req
    "UpdateNetworkProfile"
    "fixture/UpdateNetworkProfile.yaml"

requestDeleteNetworkProfile :: DeleteNetworkProfile -> TestTree
requestDeleteNetworkProfile = req
    "DeleteNetworkProfile"
    "fixture/DeleteNetworkProfile.yaml"

requestGetDevicePoolCompatibility :: GetDevicePoolCompatibility -> TestTree
requestGetDevicePoolCompatibility = req
    "GetDevicePoolCompatibility"
    "fixture/GetDevicePoolCompatibility.yaml"

requestInstallToRemoteAccessSession :: InstallToRemoteAccessSession -> TestTree
requestInstallToRemoteAccessSession = req
    "InstallToRemoteAccessSession"
    "fixture/InstallToRemoteAccessSession.yaml"

requestListTests :: ListTests -> TestTree
requestListTests = req
    "ListTests"
    "fixture/ListTests.yaml"

requestListArtifacts :: ListArtifacts -> TestTree
requestListArtifacts = req
    "ListArtifacts"
    "fixture/ListArtifacts.yaml"

requestCreateUpload :: CreateUpload -> TestTree
requestCreateUpload = req
    "CreateUpload"
    "fixture/CreateUpload.yaml"

requestGetDeviceInstance :: GetDeviceInstance -> TestTree
requestGetDeviceInstance = req
    "GetDeviceInstance"
    "fixture/GetDeviceInstance.yaml"

requestDeleteRemoteAccessSession :: DeleteRemoteAccessSession -> TestTree
requestDeleteRemoteAccessSession = req
    "DeleteRemoteAccessSession"
    "fixture/DeleteRemoteAccessSession.yaml"

requestDeleteUpload :: DeleteUpload -> TestTree
requestDeleteUpload = req
    "DeleteUpload"
    "fixture/DeleteUpload.yaml"

requestGetDevicePool :: GetDevicePool -> TestTree
requestGetDevicePool = req
    "GetDevicePool"
    "fixture/GetDevicePool.yaml"

requestListDevicePools :: ListDevicePools -> TestTree
requestListDevicePools = req
    "ListDevicePools"
    "fixture/ListDevicePools.yaml"

requestUpdateDevicePool :: UpdateDevicePool -> TestTree
requestUpdateDevicePool = req
    "UpdateDevicePool"
    "fixture/UpdateDevicePool.yaml"

requestDeleteDevicePool :: DeleteDevicePool -> TestTree
requestDeleteDevicePool = req
    "DeleteDevicePool"
    "fixture/DeleteDevicePool.yaml"

requestGetUpload :: GetUpload -> TestTree
requestGetUpload = req
    "GetUpload"
    "fixture/GetUpload.yaml"

requestListOfferingTransactions :: ListOfferingTransactions -> TestTree
requestListOfferingTransactions = req
    "ListOfferingTransactions"
    "fixture/ListOfferingTransactions.yaml"

requestCreateDevicePool :: CreateDevicePool -> TestTree
requestCreateDevicePool = req
    "CreateDevicePool"
    "fixture/CreateDevicePool.yaml"

requestDeleteRun :: DeleteRun -> TestTree
requestDeleteRun = req
    "DeleteRun"
    "fixture/DeleteRun.yaml"

requestListRuns :: ListRuns -> TestTree
requestListRuns = req
    "ListRuns"
    "fixture/ListRuns.yaml"

requestGetTest :: GetTest -> TestTree
requestGetTest = req
    "GetTest"
    "fixture/GetTest.yaml"

requestUpdateDeviceInstance :: UpdateDeviceInstance -> TestTree
requestUpdateDeviceInstance = req
    "UpdateDeviceInstance"
    "fixture/UpdateDeviceInstance.yaml"

requestGetNetworkProfile :: GetNetworkProfile -> TestTree
requestGetNetworkProfile = req
    "GetNetworkProfile"
    "fixture/GetNetworkProfile.yaml"

requestRenewOffering :: RenewOffering -> TestTree
requestRenewOffering = req
    "RenewOffering"
    "fixture/RenewOffering.yaml"

requestDeleteInstanceProfile :: DeleteInstanceProfile -> TestTree
requestDeleteInstanceProfile = req
    "DeleteInstanceProfile"
    "fixture/DeleteInstanceProfile.yaml"

requestUpdateInstanceProfile :: UpdateInstanceProfile -> TestTree
requestUpdateInstanceProfile = req
    "UpdateInstanceProfile"
    "fixture/UpdateInstanceProfile.yaml"

requestCreateInstanceProfile :: CreateInstanceProfile -> TestTree
requestCreateInstanceProfile = req
    "CreateInstanceProfile"
    "fixture/CreateInstanceProfile.yaml"

requestGetDevice :: GetDevice -> TestTree
requestGetDevice = req
    "GetDevice"
    "fixture/GetDevice.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs = req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestGetVPCEConfiguration :: GetVPCEConfiguration -> TestTree
requestGetVPCEConfiguration = req
    "GetVPCEConfiguration"
    "fixture/GetVPCEConfiguration.yaml"

requestStopRemoteAccessSession :: StopRemoteAccessSession -> TestTree
requestStopRemoteAccessSession = req
    "StopRemoteAccessSession"
    "fixture/StopRemoteAccessSession.yaml"

requestCreateNetworkProfile :: CreateNetworkProfile -> TestTree
requestCreateNetworkProfile = req
    "CreateNetworkProfile"
    "fixture/CreateNetworkProfile.yaml"

requestDeleteVPCEConfiguration :: DeleteVPCEConfiguration -> TestTree
requestDeleteVPCEConfiguration = req
    "DeleteVPCEConfiguration"
    "fixture/DeleteVPCEConfiguration.yaml"

requestUpdateVPCEConfiguration :: UpdateVPCEConfiguration -> TestTree
requestUpdateVPCEConfiguration = req
    "UpdateVPCEConfiguration"
    "fixture/UpdateVPCEConfiguration.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob = req
    "GetJob"
    "fixture/GetJob.yaml"

requestGetInstanceProfile :: GetInstanceProfile -> TestTree
requestGetInstanceProfile = req
    "GetInstanceProfile"
    "fixture/GetInstanceProfile.yaml"

requestListNetworkProfiles :: ListNetworkProfiles -> TestTree
requestListNetworkProfiles = req
    "ListNetworkProfiles"
    "fixture/ListNetworkProfiles.yaml"

requestCreateVPCEConfiguration :: CreateVPCEConfiguration -> TestTree
requestCreateVPCEConfiguration = req
    "CreateVPCEConfiguration"
    "fixture/CreateVPCEConfiguration.yaml"

requestScheduleRun :: ScheduleRun -> TestTree
requestScheduleRun = req
    "ScheduleRun"
    "fixture/ScheduleRun.yaml"

requestGetRun :: GetRun -> TestTree
requestGetRun = req
    "GetRun"
    "fixture/GetRun.yaml"

requestListSamples :: ListSamples -> TestTree
requestListSamples = req
    "ListSamples"
    "fixture/ListSamples.yaml"

requestListSuites :: ListSuites -> TestTree
requestListSuites = req
    "ListSuites"
    "fixture/ListSuites.yaml"

requestListRemoteAccessSessions :: ListRemoteAccessSessions -> TestTree
requestListRemoteAccessSessions = req
    "ListRemoteAccessSessions"
    "fixture/ListRemoteAccessSessions.yaml"

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings = req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

requestCreateRemoteAccessSession :: CreateRemoteAccessSession -> TestTree
requestCreateRemoteAccessSession = req
    "CreateRemoteAccessSession"
    "fixture/CreateRemoteAccessSession.yaml"

requestListOfferingPromotions :: ListOfferingPromotions -> TestTree
requestListOfferingPromotions = req
    "ListOfferingPromotions"
    "fixture/ListOfferingPromotions.yaml"

requestGetOfferingStatus :: GetOfferingStatus -> TestTree
requestGetOfferingStatus = req
    "GetOfferingStatus"
    "fixture/GetOfferingStatus.yaml"

requestListUploads :: ListUploads -> TestTree
requestListUploads = req
    "ListUploads"
    "fixture/ListUploads.yaml"

requestGetSuite :: GetSuite -> TestTree
requestGetSuite = req
    "GetSuite"
    "fixture/GetSuite.yaml"

requestGetRemoteAccessSession :: GetRemoteAccessSession -> TestTree
requestGetRemoteAccessSession = req
    "GetRemoteAccessSession"
    "fixture/GetRemoteAccessSession.yaml"

requestListDeviceInstances :: ListDeviceInstances -> TestTree
requestListDeviceInstances = req
    "ListDeviceInstances"
    "fixture/ListDeviceInstances.yaml"

requestPurchaseOffering :: PurchaseOffering -> TestTree
requestPurchaseOffering = req
    "PurchaseOffering"
    "fixture/PurchaseOffering.yaml"

requestListInstanceProfiles :: ListInstanceProfiles -> TestTree
requestListInstanceProfiles = req
    "ListInstanceProfiles"
    "fixture/ListInstanceProfiles.yaml"

requestGetProject :: GetProject -> TestTree
requestGetProject = req
    "GetProject"
    "fixture/GetProject.yaml"

requestListUniqueProblems :: ListUniqueProblems -> TestTree
requestListUniqueProblems = req
    "ListUniqueProblems"
    "fixture/ListUniqueProblems.yaml"

requestListVPCEConfigurations :: ListVPCEConfigurations -> TestTree
requestListVPCEConfigurations = req
    "ListVPCEConfigurations"
    "fixture/ListVPCEConfigurations.yaml"

requestStopRun :: StopRun -> TestTree
requestStopRun = req
    "StopRun"
    "fixture/StopRun.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices = req
    "ListDevices"
    "fixture/ListDevices.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject = req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestListOfferings :: ListOfferings -> TestTree
requestListOfferings = req
    "ListOfferings"
    "fixture/ListOfferings.yaml"

-- Responses

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects = res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListProjects)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject = res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteProject)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject = res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    deviceFarm
    (Proxy :: Proxy UpdateProject)

responseUpdateNetworkProfile :: UpdateNetworkProfileResponse -> TestTree
responseUpdateNetworkProfile = res
    "UpdateNetworkProfileResponse"
    "fixture/UpdateNetworkProfileResponse.proto"
    deviceFarm
    (Proxy :: Proxy UpdateNetworkProfile)

responseDeleteNetworkProfile :: DeleteNetworkProfileResponse -> TestTree
responseDeleteNetworkProfile = res
    "DeleteNetworkProfileResponse"
    "fixture/DeleteNetworkProfileResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteNetworkProfile)

responseGetDevicePoolCompatibility :: GetDevicePoolCompatibilityResponse -> TestTree
responseGetDevicePoolCompatibility = res
    "GetDevicePoolCompatibilityResponse"
    "fixture/GetDevicePoolCompatibilityResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetDevicePoolCompatibility)

responseInstallToRemoteAccessSession :: InstallToRemoteAccessSessionResponse -> TestTree
responseInstallToRemoteAccessSession = res
    "InstallToRemoteAccessSessionResponse"
    "fixture/InstallToRemoteAccessSessionResponse.proto"
    deviceFarm
    (Proxy :: Proxy InstallToRemoteAccessSession)

responseListTests :: ListTestsResponse -> TestTree
responseListTests = res
    "ListTestsResponse"
    "fixture/ListTestsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListTests)

responseListArtifacts :: ListArtifactsResponse -> TestTree
responseListArtifacts = res
    "ListArtifactsResponse"
    "fixture/ListArtifactsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListArtifacts)

responseCreateUpload :: CreateUploadResponse -> TestTree
responseCreateUpload = res
    "CreateUploadResponse"
    "fixture/CreateUploadResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateUpload)

responseGetDeviceInstance :: GetDeviceInstanceResponse -> TestTree
responseGetDeviceInstance = res
    "GetDeviceInstanceResponse"
    "fixture/GetDeviceInstanceResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetDeviceInstance)

responseDeleteRemoteAccessSession :: DeleteRemoteAccessSessionResponse -> TestTree
responseDeleteRemoteAccessSession = res
    "DeleteRemoteAccessSessionResponse"
    "fixture/DeleteRemoteAccessSessionResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteRemoteAccessSession)

responseDeleteUpload :: DeleteUploadResponse -> TestTree
responseDeleteUpload = res
    "DeleteUploadResponse"
    "fixture/DeleteUploadResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteUpload)

responseGetDevicePool :: GetDevicePoolResponse -> TestTree
responseGetDevicePool = res
    "GetDevicePoolResponse"
    "fixture/GetDevicePoolResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetDevicePool)

responseListDevicePools :: ListDevicePoolsResponse -> TestTree
responseListDevicePools = res
    "ListDevicePoolsResponse"
    "fixture/ListDevicePoolsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListDevicePools)

responseUpdateDevicePool :: UpdateDevicePoolResponse -> TestTree
responseUpdateDevicePool = res
    "UpdateDevicePoolResponse"
    "fixture/UpdateDevicePoolResponse.proto"
    deviceFarm
    (Proxy :: Proxy UpdateDevicePool)

responseDeleteDevicePool :: DeleteDevicePoolResponse -> TestTree
responseDeleteDevicePool = res
    "DeleteDevicePoolResponse"
    "fixture/DeleteDevicePoolResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteDevicePool)

responseGetUpload :: GetUploadResponse -> TestTree
responseGetUpload = res
    "GetUploadResponse"
    "fixture/GetUploadResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetUpload)

responseListOfferingTransactions :: ListOfferingTransactionsResponse -> TestTree
responseListOfferingTransactions = res
    "ListOfferingTransactionsResponse"
    "fixture/ListOfferingTransactionsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListOfferingTransactions)

responseCreateDevicePool :: CreateDevicePoolResponse -> TestTree
responseCreateDevicePool = res
    "CreateDevicePoolResponse"
    "fixture/CreateDevicePoolResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateDevicePool)

responseDeleteRun :: DeleteRunResponse -> TestTree
responseDeleteRun = res
    "DeleteRunResponse"
    "fixture/DeleteRunResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteRun)

responseListRuns :: ListRunsResponse -> TestTree
responseListRuns = res
    "ListRunsResponse"
    "fixture/ListRunsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListRuns)

responseGetTest :: GetTestResponse -> TestTree
responseGetTest = res
    "GetTestResponse"
    "fixture/GetTestResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetTest)

responseUpdateDeviceInstance :: UpdateDeviceInstanceResponse -> TestTree
responseUpdateDeviceInstance = res
    "UpdateDeviceInstanceResponse"
    "fixture/UpdateDeviceInstanceResponse.proto"
    deviceFarm
    (Proxy :: Proxy UpdateDeviceInstance)

responseGetNetworkProfile :: GetNetworkProfileResponse -> TestTree
responseGetNetworkProfile = res
    "GetNetworkProfileResponse"
    "fixture/GetNetworkProfileResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetNetworkProfile)

responseRenewOffering :: RenewOfferingResponse -> TestTree
responseRenewOffering = res
    "RenewOfferingResponse"
    "fixture/RenewOfferingResponse.proto"
    deviceFarm
    (Proxy :: Proxy RenewOffering)

responseDeleteInstanceProfile :: DeleteInstanceProfileResponse -> TestTree
responseDeleteInstanceProfile = res
    "DeleteInstanceProfileResponse"
    "fixture/DeleteInstanceProfileResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteInstanceProfile)

responseUpdateInstanceProfile :: UpdateInstanceProfileResponse -> TestTree
responseUpdateInstanceProfile = res
    "UpdateInstanceProfileResponse"
    "fixture/UpdateInstanceProfileResponse.proto"
    deviceFarm
    (Proxy :: Proxy UpdateInstanceProfile)

responseCreateInstanceProfile :: CreateInstanceProfileResponse -> TestTree
responseCreateInstanceProfile = res
    "CreateInstanceProfileResponse"
    "fixture/CreateInstanceProfileResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateInstanceProfile)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice = res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetDevice)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs = res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListJobs)

responseGetVPCEConfiguration :: GetVPCEConfigurationResponse -> TestTree
responseGetVPCEConfiguration = res
    "GetVPCEConfigurationResponse"
    "fixture/GetVPCEConfigurationResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetVPCEConfiguration)

responseStopRemoteAccessSession :: StopRemoteAccessSessionResponse -> TestTree
responseStopRemoteAccessSession = res
    "StopRemoteAccessSessionResponse"
    "fixture/StopRemoteAccessSessionResponse.proto"
    deviceFarm
    (Proxy :: Proxy StopRemoteAccessSession)

responseCreateNetworkProfile :: CreateNetworkProfileResponse -> TestTree
responseCreateNetworkProfile = res
    "CreateNetworkProfileResponse"
    "fixture/CreateNetworkProfileResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateNetworkProfile)

responseDeleteVPCEConfiguration :: DeleteVPCEConfigurationResponse -> TestTree
responseDeleteVPCEConfiguration = res
    "DeleteVPCEConfigurationResponse"
    "fixture/DeleteVPCEConfigurationResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteVPCEConfiguration)

responseUpdateVPCEConfiguration :: UpdateVPCEConfigurationResponse -> TestTree
responseUpdateVPCEConfiguration = res
    "UpdateVPCEConfigurationResponse"
    "fixture/UpdateVPCEConfigurationResponse.proto"
    deviceFarm
    (Proxy :: Proxy UpdateVPCEConfiguration)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob = res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetJob)

responseGetInstanceProfile :: GetInstanceProfileResponse -> TestTree
responseGetInstanceProfile = res
    "GetInstanceProfileResponse"
    "fixture/GetInstanceProfileResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetInstanceProfile)

responseListNetworkProfiles :: ListNetworkProfilesResponse -> TestTree
responseListNetworkProfiles = res
    "ListNetworkProfilesResponse"
    "fixture/ListNetworkProfilesResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListNetworkProfiles)

responseCreateVPCEConfiguration :: CreateVPCEConfigurationResponse -> TestTree
responseCreateVPCEConfiguration = res
    "CreateVPCEConfigurationResponse"
    "fixture/CreateVPCEConfigurationResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateVPCEConfiguration)

responseScheduleRun :: ScheduleRunResponse -> TestTree
responseScheduleRun = res
    "ScheduleRunResponse"
    "fixture/ScheduleRunResponse.proto"
    deviceFarm
    (Proxy :: Proxy ScheduleRun)

responseGetRun :: GetRunResponse -> TestTree
responseGetRun = res
    "GetRunResponse"
    "fixture/GetRunResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetRun)

responseListSamples :: ListSamplesResponse -> TestTree
responseListSamples = res
    "ListSamplesResponse"
    "fixture/ListSamplesResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListSamples)

responseListSuites :: ListSuitesResponse -> TestTree
responseListSuites = res
    "ListSuitesResponse"
    "fixture/ListSuitesResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListSuites)

responseListRemoteAccessSessions :: ListRemoteAccessSessionsResponse -> TestTree
responseListRemoteAccessSessions = res
    "ListRemoteAccessSessionsResponse"
    "fixture/ListRemoteAccessSessionsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListRemoteAccessSessions)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings = res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetAccountSettings)

responseCreateRemoteAccessSession :: CreateRemoteAccessSessionResponse -> TestTree
responseCreateRemoteAccessSession = res
    "CreateRemoteAccessSessionResponse"
    "fixture/CreateRemoteAccessSessionResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateRemoteAccessSession)

responseListOfferingPromotions :: ListOfferingPromotionsResponse -> TestTree
responseListOfferingPromotions = res
    "ListOfferingPromotionsResponse"
    "fixture/ListOfferingPromotionsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListOfferingPromotions)

responseGetOfferingStatus :: GetOfferingStatusResponse -> TestTree
responseGetOfferingStatus = res
    "GetOfferingStatusResponse"
    "fixture/GetOfferingStatusResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetOfferingStatus)

responseListUploads :: ListUploadsResponse -> TestTree
responseListUploads = res
    "ListUploadsResponse"
    "fixture/ListUploadsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListUploads)

responseGetSuite :: GetSuiteResponse -> TestTree
responseGetSuite = res
    "GetSuiteResponse"
    "fixture/GetSuiteResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetSuite)

responseGetRemoteAccessSession :: GetRemoteAccessSessionResponse -> TestTree
responseGetRemoteAccessSession = res
    "GetRemoteAccessSessionResponse"
    "fixture/GetRemoteAccessSessionResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetRemoteAccessSession)

responseListDeviceInstances :: ListDeviceInstancesResponse -> TestTree
responseListDeviceInstances = res
    "ListDeviceInstancesResponse"
    "fixture/ListDeviceInstancesResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListDeviceInstances)

responsePurchaseOffering :: PurchaseOfferingResponse -> TestTree
responsePurchaseOffering = res
    "PurchaseOfferingResponse"
    "fixture/PurchaseOfferingResponse.proto"
    deviceFarm
    (Proxy :: Proxy PurchaseOffering)

responseListInstanceProfiles :: ListInstanceProfilesResponse -> TestTree
responseListInstanceProfiles = res
    "ListInstanceProfilesResponse"
    "fixture/ListInstanceProfilesResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListInstanceProfiles)

responseGetProject :: GetProjectResponse -> TestTree
responseGetProject = res
    "GetProjectResponse"
    "fixture/GetProjectResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetProject)

responseListUniqueProblems :: ListUniqueProblemsResponse -> TestTree
responseListUniqueProblems = res
    "ListUniqueProblemsResponse"
    "fixture/ListUniqueProblemsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListUniqueProblems)

responseListVPCEConfigurations :: ListVPCEConfigurationsResponse -> TestTree
responseListVPCEConfigurations = res
    "ListVPCEConfigurationsResponse"
    "fixture/ListVPCEConfigurationsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListVPCEConfigurations)

responseStopRun :: StopRunResponse -> TestTree
responseStopRun = res
    "StopRunResponse"
    "fixture/StopRunResponse.proto"
    deviceFarm
    (Proxy :: Proxy StopRun)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices = res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListDevices)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject = res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateProject)

responseListOfferings :: ListOfferingsResponse -> TestTree
responseListOfferings = res
    "ListOfferingsResponse"
    "fixture/ListOfferingsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListOfferings)
