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
--         [ requestListProjects $
--             listProjects
--
--         , requestDeleteProject $
--             deleteProject
--
--         , requestUpdateProject $
--             updateProject
--
--         , requestGetDevicePoolCompatibility $
--             getDevicePoolCompatibility
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
--         , requestRenewOffering $
--             renewOffering
--
--         , requestGetDevice $
--             getDevice
--
--         , requestListJobs $
--             listJobs
--
--         , requestGetJob $
--             getJob
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
--         , requestGetAccountSettings $
--             getAccountSettings
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
--         , requestPurchaseOffering $
--             purchaseOffering
--
--         , requestGetProject $
--             getProject
--
--         , requestListUniqueProblems $
--             listUniqueProblems
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
--         , responseGetDevicePoolCompatibility $
--             getDevicePoolCompatibilityResponse
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
--         , responseRenewOffering $
--             renewOfferingResponse
--
--         , responseGetDevice $
--             getDeviceResponse
--
--         , responseListJobs $
--             listJobsResponse
--
--         , responseGetJob $
--             getJobResponse
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
--         , responseGetAccountSettings $
--             getAccountSettingsResponse
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
--         , responsePurchaseOffering $
--             purchaseOfferingResponse
--
--         , responseGetProject $
--             getProjectResponse
--
--         , responseListUniqueProblems $
--             listUniqueProblemsResponse
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

requestGetDevicePoolCompatibility :: GetDevicePoolCompatibility -> TestTree
requestGetDevicePoolCompatibility = req
    "GetDevicePoolCompatibility"
    "fixture/GetDevicePoolCompatibility.yaml"

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

requestRenewOffering :: RenewOffering -> TestTree
requestRenewOffering = req
    "RenewOffering"
    "fixture/RenewOffering.yaml"

requestGetDevice :: GetDevice -> TestTree
requestGetDevice = req
    "GetDevice"
    "fixture/GetDevice.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs = req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob = req
    "GetJob"
    "fixture/GetJob.yaml"

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

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings = req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

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

requestPurchaseOffering :: PurchaseOffering -> TestTree
requestPurchaseOffering = req
    "PurchaseOffering"
    "fixture/PurchaseOffering.yaml"

requestGetProject :: GetProject -> TestTree
requestGetProject = req
    "GetProject"
    "fixture/GetProject.yaml"

requestListUniqueProblems :: ListUniqueProblems -> TestTree
requestListUniqueProblems = req
    "ListUniqueProblems"
    "fixture/ListUniqueProblems.yaml"

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

responseGetDevicePoolCompatibility :: GetDevicePoolCompatibilityResponse -> TestTree
responseGetDevicePoolCompatibility = res
    "GetDevicePoolCompatibilityResponse"
    "fixture/GetDevicePoolCompatibilityResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetDevicePoolCompatibility)

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

responseRenewOffering :: RenewOfferingResponse -> TestTree
responseRenewOffering = res
    "RenewOfferingResponse"
    "fixture/RenewOfferingResponse.proto"
    deviceFarm
    (Proxy :: Proxy RenewOffering)

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

responseGetJob :: GetJobResponse -> TestTree
responseGetJob = res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetJob)

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

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings = res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    deviceFarm
    (Proxy :: Proxy GetAccountSettings)

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

responsePurchaseOffering :: PurchaseOfferingResponse -> TestTree
responsePurchaseOffering = res
    "PurchaseOfferingResponse"
    "fixture/PurchaseOfferingResponse.proto"
    deviceFarm
    (Proxy :: Proxy PurchaseOffering)

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
