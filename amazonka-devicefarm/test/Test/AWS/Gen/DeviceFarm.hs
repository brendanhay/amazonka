{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DeviceFarm
-- Copyright   : (c) 2013-2015 Brendan Hay
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
--         , testGetDevicePoolCompatibility $
--             getDevicePoolCompatibility
--
--         , testListTests $
--             listTests
--
--         , testCreateUpload $
--             createUpload
--
--         , testListArtifacts $
--             listArtifacts
--
--         , testGetDevicePool $
--             getDevicePool
--
--         , testGetUpload $
--             getUpload
--
--         , testListDevicePools $
--             listDevicePools
--
--         , testGetTest $
--             getTest
--
--         , testCreateDevicePool $
--             createDevicePool
--
--         , testListRuns $
--             listRuns
--
--         , testListJobs $
--             listJobs
--
--         , testGetDevice $
--             getDevice
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
--         , testListUploads $
--             listUploads
--
--         , testGetSuite $
--             getSuite
--
--         , testGetProject $
--             getProject
--
--         , testListDevices $
--             listDevices
--
--         , testListUniqueProblems $
--             listUniqueProblems
--
--         , testCreateProject $
--             createProject
--
--           ]

--     , testGroup "response"
--         [ testListProjectsResponse $
--             listProjectsResponse
--
--         , testGetDevicePoolCompatibilityResponse $
--             getDevicePoolCompatibilityResponse
--
--         , testListTestsResponse $
--             listTestsResponse
--
--         , testCreateUploadResponse $
--             createUploadResponse
--
--         , testListArtifactsResponse $
--             listArtifactsResponse
--
--         , testGetDevicePoolResponse $
--             getDevicePoolResponse
--
--         , testGetUploadResponse $
--             getUploadResponse
--
--         , testListDevicePoolsResponse $
--             listDevicePoolsResponse
--
--         , testGetTestResponse $
--             getTestResponse
--
--         , testCreateDevicePoolResponse $
--             createDevicePoolResponse
--
--         , testListRunsResponse $
--             listRunsResponse
--
--         , testListJobsResponse $
--             listJobsResponse
--
--         , testGetDeviceResponse $
--             getDeviceResponse
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
--         , testListUploadsResponse $
--             listUploadsResponse
--
--         , testGetSuiteResponse $
--             getSuiteResponse
--
--         , testGetProjectResponse $
--             getProjectResponse
--
--         , testListDevicesResponse $
--             listDevicesResponse
--
--         , testListUniqueProblemsResponse $
--             listUniqueProblemsResponse
--
--         , testCreateProjectResponse $
--             createProjectResponse
--
--           ]
--     ]

-- Requests

testListProjects :: ListProjects -> TestTree
testListProjects = req
    "ListProjects"
    "fixture/ListProjects"

testGetDevicePoolCompatibility :: GetDevicePoolCompatibility -> TestTree
testGetDevicePoolCompatibility = req
    "GetDevicePoolCompatibility"
    "fixture/GetDevicePoolCompatibility"

testListTests :: ListTests -> TestTree
testListTests = req
    "ListTests"
    "fixture/ListTests"

testCreateUpload :: CreateUpload -> TestTree
testCreateUpload = req
    "CreateUpload"
    "fixture/CreateUpload"

testListArtifacts :: ListArtifacts -> TestTree
testListArtifacts = req
    "ListArtifacts"
    "fixture/ListArtifacts"

testGetDevicePool :: GetDevicePool -> TestTree
testGetDevicePool = req
    "GetDevicePool"
    "fixture/GetDevicePool"

testGetUpload :: GetUpload -> TestTree
testGetUpload = req
    "GetUpload"
    "fixture/GetUpload"

testListDevicePools :: ListDevicePools -> TestTree
testListDevicePools = req
    "ListDevicePools"
    "fixture/ListDevicePools"

testGetTest :: GetTest -> TestTree
testGetTest = req
    "GetTest"
    "fixture/GetTest"

testCreateDevicePool :: CreateDevicePool -> TestTree
testCreateDevicePool = req
    "CreateDevicePool"
    "fixture/CreateDevicePool"

testListRuns :: ListRuns -> TestTree
testListRuns = req
    "ListRuns"
    "fixture/ListRuns"

testListJobs :: ListJobs -> TestTree
testListJobs = req
    "ListJobs"
    "fixture/ListJobs"

testGetDevice :: GetDevice -> TestTree
testGetDevice = req
    "GetDevice"
    "fixture/GetDevice"

testGetJob :: GetJob -> TestTree
testGetJob = req
    "GetJob"
    "fixture/GetJob"

testScheduleRun :: ScheduleRun -> TestTree
testScheduleRun = req
    "ScheduleRun"
    "fixture/ScheduleRun"

testGetRun :: GetRun -> TestTree
testGetRun = req
    "GetRun"
    "fixture/GetRun"

testListSamples :: ListSamples -> TestTree
testListSamples = req
    "ListSamples"
    "fixture/ListSamples"

testListSuites :: ListSuites -> TestTree
testListSuites = req
    "ListSuites"
    "fixture/ListSuites"

testGetAccountSettings :: GetAccountSettings -> TestTree
testGetAccountSettings = req
    "GetAccountSettings"
    "fixture/GetAccountSettings"

testListUploads :: ListUploads -> TestTree
testListUploads = req
    "ListUploads"
    "fixture/ListUploads"

testGetSuite :: GetSuite -> TestTree
testGetSuite = req
    "GetSuite"
    "fixture/GetSuite"

testGetProject :: GetProject -> TestTree
testGetProject = req
    "GetProject"
    "fixture/GetProject"

testListDevices :: ListDevices -> TestTree
testListDevices = req
    "ListDevices"
    "fixture/ListDevices"

testListUniqueProblems :: ListUniqueProblems -> TestTree
testListUniqueProblems = req
    "ListUniqueProblems"
    "fixture/ListUniqueProblems"

testCreateProject :: CreateProject -> TestTree
testCreateProject = req
    "CreateProject"
    "fixture/CreateProject"

-- Responses

testListProjectsResponse :: ListProjectsResponse -> TestTree
testListProjectsResponse = res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse"
    deviceFarm
    (Proxy :: Proxy ListProjects)

testGetDevicePoolCompatibilityResponse :: GetDevicePoolCompatibilityResponse -> TestTree
testGetDevicePoolCompatibilityResponse = res
    "GetDevicePoolCompatibilityResponse"
    "fixture/GetDevicePoolCompatibilityResponse"
    deviceFarm
    (Proxy :: Proxy GetDevicePoolCompatibility)

testListTestsResponse :: ListTestsResponse -> TestTree
testListTestsResponse = res
    "ListTestsResponse"
    "fixture/ListTestsResponse"
    deviceFarm
    (Proxy :: Proxy ListTests)

testCreateUploadResponse :: CreateUploadResponse -> TestTree
testCreateUploadResponse = res
    "CreateUploadResponse"
    "fixture/CreateUploadResponse"
    deviceFarm
    (Proxy :: Proxy CreateUpload)

testListArtifactsResponse :: ListArtifactsResponse -> TestTree
testListArtifactsResponse = res
    "ListArtifactsResponse"
    "fixture/ListArtifactsResponse"
    deviceFarm
    (Proxy :: Proxy ListArtifacts)

testGetDevicePoolResponse :: GetDevicePoolResponse -> TestTree
testGetDevicePoolResponse = res
    "GetDevicePoolResponse"
    "fixture/GetDevicePoolResponse"
    deviceFarm
    (Proxy :: Proxy GetDevicePool)

testGetUploadResponse :: GetUploadResponse -> TestTree
testGetUploadResponse = res
    "GetUploadResponse"
    "fixture/GetUploadResponse"
    deviceFarm
    (Proxy :: Proxy GetUpload)

testListDevicePoolsResponse :: ListDevicePoolsResponse -> TestTree
testListDevicePoolsResponse = res
    "ListDevicePoolsResponse"
    "fixture/ListDevicePoolsResponse"
    deviceFarm
    (Proxy :: Proxy ListDevicePools)

testGetTestResponse :: GetTestResponse -> TestTree
testGetTestResponse = res
    "GetTestResponse"
    "fixture/GetTestResponse"
    deviceFarm
    (Proxy :: Proxy GetTest)

testCreateDevicePoolResponse :: CreateDevicePoolResponse -> TestTree
testCreateDevicePoolResponse = res
    "CreateDevicePoolResponse"
    "fixture/CreateDevicePoolResponse"
    deviceFarm
    (Proxy :: Proxy CreateDevicePool)

testListRunsResponse :: ListRunsResponse -> TestTree
testListRunsResponse = res
    "ListRunsResponse"
    "fixture/ListRunsResponse"
    deviceFarm
    (Proxy :: Proxy ListRuns)

testListJobsResponse :: ListJobsResponse -> TestTree
testListJobsResponse = res
    "ListJobsResponse"
    "fixture/ListJobsResponse"
    deviceFarm
    (Proxy :: Proxy ListJobs)

testGetDeviceResponse :: GetDeviceResponse -> TestTree
testGetDeviceResponse = res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse"
    deviceFarm
    (Proxy :: Proxy GetDevice)

testGetJobResponse :: GetJobResponse -> TestTree
testGetJobResponse = res
    "GetJobResponse"
    "fixture/GetJobResponse"
    deviceFarm
    (Proxy :: Proxy GetJob)

testScheduleRunResponse :: ScheduleRunResponse -> TestTree
testScheduleRunResponse = res
    "ScheduleRunResponse"
    "fixture/ScheduleRunResponse"
    deviceFarm
    (Proxy :: Proxy ScheduleRun)

testGetRunResponse :: GetRunResponse -> TestTree
testGetRunResponse = res
    "GetRunResponse"
    "fixture/GetRunResponse"
    deviceFarm
    (Proxy :: Proxy GetRun)

testListSamplesResponse :: ListSamplesResponse -> TestTree
testListSamplesResponse = res
    "ListSamplesResponse"
    "fixture/ListSamplesResponse"
    deviceFarm
    (Proxy :: Proxy ListSamples)

testListSuitesResponse :: ListSuitesResponse -> TestTree
testListSuitesResponse = res
    "ListSuitesResponse"
    "fixture/ListSuitesResponse"
    deviceFarm
    (Proxy :: Proxy ListSuites)

testGetAccountSettingsResponse :: GetAccountSettingsResponse -> TestTree
testGetAccountSettingsResponse = res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse"
    deviceFarm
    (Proxy :: Proxy GetAccountSettings)

testListUploadsResponse :: ListUploadsResponse -> TestTree
testListUploadsResponse = res
    "ListUploadsResponse"
    "fixture/ListUploadsResponse"
    deviceFarm
    (Proxy :: Proxy ListUploads)

testGetSuiteResponse :: GetSuiteResponse -> TestTree
testGetSuiteResponse = res
    "GetSuiteResponse"
    "fixture/GetSuiteResponse"
    deviceFarm
    (Proxy :: Proxy GetSuite)

testGetProjectResponse :: GetProjectResponse -> TestTree
testGetProjectResponse = res
    "GetProjectResponse"
    "fixture/GetProjectResponse"
    deviceFarm
    (Proxy :: Proxy GetProject)

testListDevicesResponse :: ListDevicesResponse -> TestTree
testListDevicesResponse = res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse"
    deviceFarm
    (Proxy :: Proxy ListDevices)

testListUniqueProblemsResponse :: ListUniqueProblemsResponse -> TestTree
testListUniqueProblemsResponse = res
    "ListUniqueProblemsResponse"
    "fixture/ListUniqueProblemsResponse"
    deviceFarm
    (Proxy :: Proxy ListUniqueProblems)

testCreateProjectResponse :: CreateProjectResponse -> TestTree
testCreateProjectResponse = res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse"
    deviceFarm
    (Proxy :: Proxy CreateProject)
