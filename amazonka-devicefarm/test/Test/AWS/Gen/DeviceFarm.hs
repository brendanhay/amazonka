{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DeviceFarm
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.DeviceFarm where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.DeviceFarm
import Test.AWS.DeviceFarm.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testListProjects $
--             listProjects
--
--         , testDeleteProject $
--             deleteProject
--
--         , testUpdateProject $
--             updateProject
--
--         , testGetDevicePoolCompatibility $
--             getDevicePoolCompatibility
--
--         , testListTests $
--             listTests
--
--         , testListArtifacts $
--             listArtifacts
--
--         , testCreateUpload $
--             createUpload
--
--         , testDeleteUpload $
--             deleteUpload
--
--         , testGetDevicePool $
--             getDevicePool
--
--         , testListDevicePools $
--             listDevicePools
--
--         , testUpdateDevicePool $
--             updateDevicePool
--
--         , testDeleteDevicePool $
--             deleteDevicePool
--
--         , testGetUpload $
--             getUpload
--
--         , testListOfferingTransactions $
--             listOfferingTransactions
--
--         , testCreateDevicePool $
--             createDevicePool
--
--         , testDeleteRun $
--             deleteRun
--
--         , testListRuns $
--             listRuns
--
--         , testGetTest $
--             getTest
--
--         , testRenewOffering $
--             renewOffering
--
--         , testGetDevice $
--             getDevice
--
--         , testListJobs $
--             listJobs
--
--         , testGetJob $
--             getJob
--
--         , testScheduleRun $
--             scheduleRun
--
--         , testGetRun $
--             getRun
--
--         , testListSamples $
--             listSamples
--
--         , testListSuites $
--             listSuites
--
--         , testGetAccountSettings $
--             getAccountSettings
--
--         , testGetOfferingStatus $
--             getOfferingStatus
--
--         , testListUploads $
--             listUploads
--
--         , testGetSuite $
--             getSuite
--
--         , testPurchaseOffering $
--             purchaseOffering
--
--         , testGetProject $
--             getProject
--
--         , testListUniqueProblems $
--             listUniqueProblems
--
--         , testStopRun $
--             stopRun
--
--         , testListDevices $
--             listDevices
--
--         , testCreateProject $
--             createProject
--
--         , testListOfferings $
--             listOfferings
--
--           ]

--     , testGroup "response"
--         [ testListProjectsResponse $
--             listProjectsResponse
--
--         , testDeleteProjectResponse $
--             deleteProjectResponse
--
--         , testUpdateProjectResponse $
--             updateProjectResponse
--
--         , testGetDevicePoolCompatibilityResponse $
--             getDevicePoolCompatibilityResponse
--
--         , testListTestsResponse $
--             listTestsResponse
--
--         , testListArtifactsResponse $
--             listArtifactsResponse
--
--         , testCreateUploadResponse $
--             createUploadResponse
--
--         , testDeleteUploadResponse $
--             deleteUploadResponse
--
--         , testGetDevicePoolResponse $
--             getDevicePoolResponse
--
--         , testListDevicePoolsResponse $
--             listDevicePoolsResponse
--
--         , testUpdateDevicePoolResponse $
--             updateDevicePoolResponse
--
--         , testDeleteDevicePoolResponse $
--             deleteDevicePoolResponse
--
--         , testGetUploadResponse $
--             getUploadResponse
--
--         , testListOfferingTransactionsResponse $
--             listOfferingTransactionsResponse
--
--         , testCreateDevicePoolResponse $
--             createDevicePoolResponse
--
--         , testDeleteRunResponse $
--             deleteRunResponse
--
--         , testListRunsResponse $
--             listRunsResponse
--
--         , testGetTestResponse $
--             getTestResponse
--
--         , testRenewOfferingResponse $
--             renewOfferingResponse
--
--         , testGetDeviceResponse $
--             getDeviceResponse
--
--         , testListJobsResponse $
--             listJobsResponse
--
--         , testGetJobResponse $
--             getJobResponse
--
--         , testScheduleRunResponse $
--             scheduleRunResponse
--
--         , testGetRunResponse $
--             getRunResponse
--
--         , testListSamplesResponse $
--             listSamplesResponse
--
--         , testListSuitesResponse $
--             listSuitesResponse
--
--         , testGetAccountSettingsResponse $
--             getAccountSettingsResponse
--
--         , testGetOfferingStatusResponse $
--             getOfferingStatusResponse
--
--         , testListUploadsResponse $
--             listUploadsResponse
--
--         , testGetSuiteResponse $
--             getSuiteResponse
--
--         , testPurchaseOfferingResponse $
--             purchaseOfferingResponse
--
--         , testGetProjectResponse $
--             getProjectResponse
--
--         , testListUniqueProblemsResponse $
--             listUniqueProblemsResponse
--
--         , testStopRunResponse $
--             stopRunResponse
--
--         , testListDevicesResponse $
--             listDevicesResponse
--
--         , testCreateProjectResponse $
--             createProjectResponse
--
--         , testListOfferingsResponse $
--             listOfferingsResponse
--
--           ]
--     ]

-- Requests

testListProjects :: ListProjects -> TestTree
testListProjects = req
    "ListProjects"
    "fixture/ListProjects.yaml"

testDeleteProject :: DeleteProject -> TestTree
testDeleteProject = req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

testUpdateProject :: UpdateProject -> TestTree
testUpdateProject = req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

testGetDevicePoolCompatibility :: GetDevicePoolCompatibility -> TestTree
testGetDevicePoolCompatibility = req
    "GetDevicePoolCompatibility"
    "fixture/GetDevicePoolCompatibility.yaml"

testListTests :: ListTests -> TestTree
testListTests = req
    "ListTests"
    "fixture/ListTests.yaml"

testListArtifacts :: ListArtifacts -> TestTree
testListArtifacts = req
    "ListArtifacts"
    "fixture/ListArtifacts.yaml"

testCreateUpload :: CreateUpload -> TestTree
testCreateUpload = req
    "CreateUpload"
    "fixture/CreateUpload.yaml"

testDeleteUpload :: DeleteUpload -> TestTree
testDeleteUpload = req
    "DeleteUpload"
    "fixture/DeleteUpload.yaml"

testGetDevicePool :: GetDevicePool -> TestTree
testGetDevicePool = req
    "GetDevicePool"
    "fixture/GetDevicePool.yaml"

testListDevicePools :: ListDevicePools -> TestTree
testListDevicePools = req
    "ListDevicePools"
    "fixture/ListDevicePools.yaml"

testUpdateDevicePool :: UpdateDevicePool -> TestTree
testUpdateDevicePool = req
    "UpdateDevicePool"
    "fixture/UpdateDevicePool.yaml"

testDeleteDevicePool :: DeleteDevicePool -> TestTree
testDeleteDevicePool = req
    "DeleteDevicePool"
    "fixture/DeleteDevicePool.yaml"

testGetUpload :: GetUpload -> TestTree
testGetUpload = req
    "GetUpload"
    "fixture/GetUpload.yaml"

testListOfferingTransactions :: ListOfferingTransactions -> TestTree
testListOfferingTransactions = req
    "ListOfferingTransactions"
    "fixture/ListOfferingTransactions.yaml"

testCreateDevicePool :: CreateDevicePool -> TestTree
testCreateDevicePool = req
    "CreateDevicePool"
    "fixture/CreateDevicePool.yaml"

testDeleteRun :: DeleteRun -> TestTree
testDeleteRun = req
    "DeleteRun"
    "fixture/DeleteRun.yaml"

testListRuns :: ListRuns -> TestTree
testListRuns = req
    "ListRuns"
    "fixture/ListRuns.yaml"

testGetTest :: GetTest -> TestTree
testGetTest = req
    "GetTest"
    "fixture/GetTest.yaml"

testRenewOffering :: RenewOffering -> TestTree
testRenewOffering = req
    "RenewOffering"
    "fixture/RenewOffering.yaml"

testGetDevice :: GetDevice -> TestTree
testGetDevice = req
    "GetDevice"
    "fixture/GetDevice.yaml"

testListJobs :: ListJobs -> TestTree
testListJobs = req
    "ListJobs"
    "fixture/ListJobs.yaml"

testGetJob :: GetJob -> TestTree
testGetJob = req
    "GetJob"
    "fixture/GetJob.yaml"

testScheduleRun :: ScheduleRun -> TestTree
testScheduleRun = req
    "ScheduleRun"
    "fixture/ScheduleRun.yaml"

testGetRun :: GetRun -> TestTree
testGetRun = req
    "GetRun"
    "fixture/GetRun.yaml"

testListSamples :: ListSamples -> TestTree
testListSamples = req
    "ListSamples"
    "fixture/ListSamples.yaml"

testListSuites :: ListSuites -> TestTree
testListSuites = req
    "ListSuites"
    "fixture/ListSuites.yaml"

testGetAccountSettings :: GetAccountSettings -> TestTree
testGetAccountSettings = req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

testGetOfferingStatus :: GetOfferingStatus -> TestTree
testGetOfferingStatus = req
    "GetOfferingStatus"
    "fixture/GetOfferingStatus.yaml"

testListUploads :: ListUploads -> TestTree
testListUploads = req
    "ListUploads"
    "fixture/ListUploads.yaml"

testGetSuite :: GetSuite -> TestTree
testGetSuite = req
    "GetSuite"
    "fixture/GetSuite.yaml"

testPurchaseOffering :: PurchaseOffering -> TestTree
testPurchaseOffering = req
    "PurchaseOffering"
    "fixture/PurchaseOffering.yaml"

testGetProject :: GetProject -> TestTree
testGetProject = req
    "GetProject"
    "fixture/GetProject.yaml"

testListUniqueProblems :: ListUniqueProblems -> TestTree
testListUniqueProblems = req
    "ListUniqueProblems"
    "fixture/ListUniqueProblems.yaml"

testStopRun :: StopRun -> TestTree
testStopRun = req
    "StopRun"
    "fixture/StopRun.yaml"

testListDevices :: ListDevices -> TestTree
testListDevices = req
    "ListDevices"
    "fixture/ListDevices.yaml"

testCreateProject :: CreateProject -> TestTree
testCreateProject = req
    "CreateProject"
    "fixture/CreateProject.yaml"

testListOfferings :: ListOfferings -> TestTree
testListOfferings = req
    "ListOfferings"
    "fixture/ListOfferings.yaml"

-- Responses

testListProjectsResponse :: ListProjectsResponse -> TestTree
testListProjectsResponse = res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListProjects)

testDeleteProjectResponse :: DeleteProjectResponse -> TestTree
testDeleteProjectResponse = res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteProject)

testUpdateProjectResponse :: UpdateProjectResponse -> TestTree
testUpdateProjectResponse = res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    deviceFarm
    (Proxy :: Proxy UpdateProject)

testGetDevicePoolCompatibilityResponse :: GetDevicePoolCompatibilityResponse -> TestTree
testGetDevicePoolCompatibilityResponse = res
    "GetDevicePoolCompatibilityResponse"
    "fixture/GetDevicePoolCompatibilityResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetDevicePoolCompatibility)

testListTestsResponse :: ListTestsResponse -> TestTree
testListTestsResponse = res
    "ListTestsResponse"
    "fixture/ListTestsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListTests)

testListArtifactsResponse :: ListArtifactsResponse -> TestTree
testListArtifactsResponse = res
    "ListArtifactsResponse"
    "fixture/ListArtifactsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListArtifacts)

testCreateUploadResponse :: CreateUploadResponse -> TestTree
testCreateUploadResponse = res
    "CreateUploadResponse"
    "fixture/CreateUploadResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateUpload)

testDeleteUploadResponse :: DeleteUploadResponse -> TestTree
testDeleteUploadResponse = res
    "DeleteUploadResponse"
    "fixture/DeleteUploadResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteUpload)

testGetDevicePoolResponse :: GetDevicePoolResponse -> TestTree
testGetDevicePoolResponse = res
    "GetDevicePoolResponse"
    "fixture/GetDevicePoolResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetDevicePool)

testListDevicePoolsResponse :: ListDevicePoolsResponse -> TestTree
testListDevicePoolsResponse = res
    "ListDevicePoolsResponse"
    "fixture/ListDevicePoolsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListDevicePools)

testUpdateDevicePoolResponse :: UpdateDevicePoolResponse -> TestTree
testUpdateDevicePoolResponse = res
    "UpdateDevicePoolResponse"
    "fixture/UpdateDevicePoolResponse.proto"
    deviceFarm
    (Proxy :: Proxy UpdateDevicePool)

testDeleteDevicePoolResponse :: DeleteDevicePoolResponse -> TestTree
testDeleteDevicePoolResponse = res
    "DeleteDevicePoolResponse"
    "fixture/DeleteDevicePoolResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteDevicePool)

testGetUploadResponse :: GetUploadResponse -> TestTree
testGetUploadResponse = res
    "GetUploadResponse"
    "fixture/GetUploadResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetUpload)

testListOfferingTransactionsResponse :: ListOfferingTransactionsResponse -> TestTree
testListOfferingTransactionsResponse = res
    "ListOfferingTransactionsResponse"
    "fixture/ListOfferingTransactionsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListOfferingTransactions)

testCreateDevicePoolResponse :: CreateDevicePoolResponse -> TestTree
testCreateDevicePoolResponse = res
    "CreateDevicePoolResponse"
    "fixture/CreateDevicePoolResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateDevicePool)

testDeleteRunResponse :: DeleteRunResponse -> TestTree
testDeleteRunResponse = res
    "DeleteRunResponse"
    "fixture/DeleteRunResponse.proto"
    deviceFarm
    (Proxy :: Proxy DeleteRun)

testListRunsResponse :: ListRunsResponse -> TestTree
testListRunsResponse = res
    "ListRunsResponse"
    "fixture/ListRunsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListRuns)

testGetTestResponse :: GetTestResponse -> TestTree
testGetTestResponse = res
    "GetTestResponse"
    "fixture/GetTestResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetTest)

testRenewOfferingResponse :: RenewOfferingResponse -> TestTree
testRenewOfferingResponse = res
    "RenewOfferingResponse"
    "fixture/RenewOfferingResponse.proto"
    deviceFarm
    (Proxy :: Proxy RenewOffering)

testGetDeviceResponse :: GetDeviceResponse -> TestTree
testGetDeviceResponse = res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetDevice)

testListJobsResponse :: ListJobsResponse -> TestTree
testListJobsResponse = res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListJobs)

testGetJobResponse :: GetJobResponse -> TestTree
testGetJobResponse = res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetJob)

testScheduleRunResponse :: ScheduleRunResponse -> TestTree
testScheduleRunResponse = res
    "ScheduleRunResponse"
    "fixture/ScheduleRunResponse.proto"
    deviceFarm
    (Proxy :: Proxy ScheduleRun)

testGetRunResponse :: GetRunResponse -> TestTree
testGetRunResponse = res
    "GetRunResponse"
    "fixture/GetRunResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetRun)

testListSamplesResponse :: ListSamplesResponse -> TestTree
testListSamplesResponse = res
    "ListSamplesResponse"
    "fixture/ListSamplesResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListSamples)

testListSuitesResponse :: ListSuitesResponse -> TestTree
testListSuitesResponse = res
    "ListSuitesResponse"
    "fixture/ListSuitesResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListSuites)

testGetAccountSettingsResponse :: GetAccountSettingsResponse -> TestTree
testGetAccountSettingsResponse = res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetAccountSettings)

testGetOfferingStatusResponse :: GetOfferingStatusResponse -> TestTree
testGetOfferingStatusResponse = res
    "GetOfferingStatusResponse"
    "fixture/GetOfferingStatusResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetOfferingStatus)

testListUploadsResponse :: ListUploadsResponse -> TestTree
testListUploadsResponse = res
    "ListUploadsResponse"
    "fixture/ListUploadsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListUploads)

testGetSuiteResponse :: GetSuiteResponse -> TestTree
testGetSuiteResponse = res
    "GetSuiteResponse"
    "fixture/GetSuiteResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetSuite)

testPurchaseOfferingResponse :: PurchaseOfferingResponse -> TestTree
testPurchaseOfferingResponse = res
    "PurchaseOfferingResponse"
    "fixture/PurchaseOfferingResponse.proto"
    deviceFarm
    (Proxy :: Proxy PurchaseOffering)

testGetProjectResponse :: GetProjectResponse -> TestTree
testGetProjectResponse = res
    "GetProjectResponse"
    "fixture/GetProjectResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetProject)

testListUniqueProblemsResponse :: ListUniqueProblemsResponse -> TestTree
testListUniqueProblemsResponse = res
    "ListUniqueProblemsResponse"
    "fixture/ListUniqueProblemsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListUniqueProblems)

testStopRunResponse :: StopRunResponse -> TestTree
testStopRunResponse = res
    "StopRunResponse"
    "fixture/StopRunResponse.proto"
    deviceFarm
    (Proxy :: Proxy StopRun)

testListDevicesResponse :: ListDevicesResponse -> TestTree
testListDevicesResponse = res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListDevices)

testCreateProjectResponse :: CreateProjectResponse -> TestTree
testCreateProjectResponse = res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    deviceFarm
    (Proxy :: Proxy CreateProject)

testListOfferingsResponse :: ListOfferingsResponse -> TestTree
testListOfferingsResponse = res
    "ListOfferingsResponse"
    "fixture/ListOfferingsResponse.proto"
    deviceFarm
    (Proxy :: Proxy ListOfferings)
