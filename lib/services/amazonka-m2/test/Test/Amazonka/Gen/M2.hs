{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.M2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.M2 where

import Amazonka.M2
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.M2.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCancelBatchJobExecution $
--             newCancelBatchJobExecution
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestCreateDataSetImportTask $
--             newCreateDataSetImportTask
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestCreateEnvironment $
--             newCreateEnvironment
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestDeleteApplicationFromEnvironment $
--             newDeleteApplicationFromEnvironment
--
--         , requestDeleteEnvironment $
--             newDeleteEnvironment
--
--         , requestGetApplication $
--             newGetApplication
--
--         , requestGetApplicationVersion $
--             newGetApplicationVersion
--
--         , requestGetBatchJobExecution $
--             newGetBatchJobExecution
--
--         , requestGetDataSetDetails $
--             newGetDataSetDetails
--
--         , requestGetDataSetImportTask $
--             newGetDataSetImportTask
--
--         , requestGetDeployment $
--             newGetDeployment
--
--         , requestGetEnvironment $
--             newGetEnvironment
--
--         , requestListApplicationVersions $
--             newListApplicationVersions
--
--         , requestListApplications $
--             newListApplications
--
--         , requestListBatchJobDefinitions $
--             newListBatchJobDefinitions
--
--         , requestListBatchJobExecutions $
--             newListBatchJobExecutions
--
--         , requestListDataSetImportHistory $
--             newListDataSetImportHistory
--
--         , requestListDataSets $
--             newListDataSets
--
--         , requestListDeployments $
--             newListDeployments
--
--         , requestListEngineVersions $
--             newListEngineVersions
--
--         , requestListEnvironments $
--             newListEnvironments
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartApplication $
--             newStartApplication
--
--         , requestStartBatchJob $
--             newStartBatchJob
--
--         , requestStopApplication $
--             newStopApplication
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestUpdateEnvironment $
--             newUpdateEnvironment
--
--           ]

--     , testGroup "response"
--         [ responseCancelBatchJobExecution $
--             newCancelBatchJobExecutionResponse
--
--         , responseCreateApplication $
--             newCreateApplicationResponse
--
--         , responseCreateDataSetImportTask $
--             newCreateDataSetImportTaskResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseCreateEnvironment $
--             newCreateEnvironmentResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseDeleteApplicationFromEnvironment $
--             newDeleteApplicationFromEnvironmentResponse
--
--         , responseDeleteEnvironment $
--             newDeleteEnvironmentResponse
--
--         , responseGetApplication $
--             newGetApplicationResponse
--
--         , responseGetApplicationVersion $
--             newGetApplicationVersionResponse
--
--         , responseGetBatchJobExecution $
--             newGetBatchJobExecutionResponse
--
--         , responseGetDataSetDetails $
--             newGetDataSetDetailsResponse
--
--         , responseGetDataSetImportTask $
--             newGetDataSetImportTaskResponse
--
--         , responseGetDeployment $
--             newGetDeploymentResponse
--
--         , responseGetEnvironment $
--             newGetEnvironmentResponse
--
--         , responseListApplicationVersions $
--             newListApplicationVersionsResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseListBatchJobDefinitions $
--             newListBatchJobDefinitionsResponse
--
--         , responseListBatchJobExecutions $
--             newListBatchJobExecutionsResponse
--
--         , responseListDataSetImportHistory $
--             newListDataSetImportHistoryResponse
--
--         , responseListDataSets $
--             newListDataSetsResponse
--
--         , responseListDeployments $
--             newListDeploymentsResponse
--
--         , responseListEngineVersions $
--             newListEngineVersionsResponse
--
--         , responseListEnvironments $
--             newListEnvironmentsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartApplication $
--             newStartApplicationResponse
--
--         , responseStartBatchJob $
--             newStartBatchJobResponse
--
--         , responseStopApplication $
--             newStopApplicationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApplication $
--             newUpdateApplicationResponse
--
--         , responseUpdateEnvironment $
--             newUpdateEnvironmentResponse
--
--           ]
--     ]

-- Requests

requestCancelBatchJobExecution :: CancelBatchJobExecution -> TestTree
requestCancelBatchJobExecution =
  req
    "CancelBatchJobExecution"
    "fixture/CancelBatchJobExecution.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestCreateDataSetImportTask :: CreateDataSetImportTask -> TestTree
requestCreateDataSetImportTask =
  req
    "CreateDataSetImportTask"
    "fixture/CreateDataSetImportTask.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestCreateEnvironment :: CreateEnvironment -> TestTree
requestCreateEnvironment =
  req
    "CreateEnvironment"
    "fixture/CreateEnvironment.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestDeleteApplicationFromEnvironment :: DeleteApplicationFromEnvironment -> TestTree
requestDeleteApplicationFromEnvironment =
  req
    "DeleteApplicationFromEnvironment"
    "fixture/DeleteApplicationFromEnvironment.yaml"

requestDeleteEnvironment :: DeleteEnvironment -> TestTree
requestDeleteEnvironment =
  req
    "DeleteEnvironment"
    "fixture/DeleteEnvironment.yaml"

requestGetApplication :: GetApplication -> TestTree
requestGetApplication =
  req
    "GetApplication"
    "fixture/GetApplication.yaml"

requestGetApplicationVersion :: GetApplicationVersion -> TestTree
requestGetApplicationVersion =
  req
    "GetApplicationVersion"
    "fixture/GetApplicationVersion.yaml"

requestGetBatchJobExecution :: GetBatchJobExecution -> TestTree
requestGetBatchJobExecution =
  req
    "GetBatchJobExecution"
    "fixture/GetBatchJobExecution.yaml"

requestGetDataSetDetails :: GetDataSetDetails -> TestTree
requestGetDataSetDetails =
  req
    "GetDataSetDetails"
    "fixture/GetDataSetDetails.yaml"

requestGetDataSetImportTask :: GetDataSetImportTask -> TestTree
requestGetDataSetImportTask =
  req
    "GetDataSetImportTask"
    "fixture/GetDataSetImportTask.yaml"

requestGetDeployment :: GetDeployment -> TestTree
requestGetDeployment =
  req
    "GetDeployment"
    "fixture/GetDeployment.yaml"

requestGetEnvironment :: GetEnvironment -> TestTree
requestGetEnvironment =
  req
    "GetEnvironment"
    "fixture/GetEnvironment.yaml"

requestListApplicationVersions :: ListApplicationVersions -> TestTree
requestListApplicationVersions =
  req
    "ListApplicationVersions"
    "fixture/ListApplicationVersions.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestListBatchJobDefinitions :: ListBatchJobDefinitions -> TestTree
requestListBatchJobDefinitions =
  req
    "ListBatchJobDefinitions"
    "fixture/ListBatchJobDefinitions.yaml"

requestListBatchJobExecutions :: ListBatchJobExecutions -> TestTree
requestListBatchJobExecutions =
  req
    "ListBatchJobExecutions"
    "fixture/ListBatchJobExecutions.yaml"

requestListDataSetImportHistory :: ListDataSetImportHistory -> TestTree
requestListDataSetImportHistory =
  req
    "ListDataSetImportHistory"
    "fixture/ListDataSetImportHistory.yaml"

requestListDataSets :: ListDataSets -> TestTree
requestListDataSets =
  req
    "ListDataSets"
    "fixture/ListDataSets.yaml"

requestListDeployments :: ListDeployments -> TestTree
requestListDeployments =
  req
    "ListDeployments"
    "fixture/ListDeployments.yaml"

requestListEngineVersions :: ListEngineVersions -> TestTree
requestListEngineVersions =
  req
    "ListEngineVersions"
    "fixture/ListEngineVersions.yaml"

requestListEnvironments :: ListEnvironments -> TestTree
requestListEnvironments =
  req
    "ListEnvironments"
    "fixture/ListEnvironments.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartApplication :: StartApplication -> TestTree
requestStartApplication =
  req
    "StartApplication"
    "fixture/StartApplication.yaml"

requestStartBatchJob :: StartBatchJob -> TestTree
requestStartBatchJob =
  req
    "StartBatchJob"
    "fixture/StartBatchJob.yaml"

requestStopApplication :: StopApplication -> TestTree
requestStopApplication =
  req
    "StopApplication"
    "fixture/StopApplication.yaml"

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

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestUpdateEnvironment :: UpdateEnvironment -> TestTree
requestUpdateEnvironment =
  req
    "UpdateEnvironment"
    "fixture/UpdateEnvironment.yaml"

-- Responses

responseCancelBatchJobExecution :: CancelBatchJobExecutionResponse -> TestTree
responseCancelBatchJobExecution =
  res
    "CancelBatchJobExecutionResponse"
    "fixture/CancelBatchJobExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelBatchJobExecution)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplication)

responseCreateDataSetImportTask :: CreateDataSetImportTaskResponse -> TestTree
responseCreateDataSetImportTask =
  res
    "CreateDataSetImportTaskResponse"
    "fixture/CreateDataSetImportTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataSetImportTask)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeployment)

responseCreateEnvironment :: CreateEnvironmentResponse -> TestTree
responseCreateEnvironment =
  res
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEnvironment)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplication)

responseDeleteApplicationFromEnvironment :: DeleteApplicationFromEnvironmentResponse -> TestTree
responseDeleteApplicationFromEnvironment =
  res
    "DeleteApplicationFromEnvironmentResponse"
    "fixture/DeleteApplicationFromEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplicationFromEnvironment)

responseDeleteEnvironment :: DeleteEnvironmentResponse -> TestTree
responseDeleteEnvironment =
  res
    "DeleteEnvironmentResponse"
    "fixture/DeleteEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEnvironment)

responseGetApplication :: GetApplicationResponse -> TestTree
responseGetApplication =
  res
    "GetApplicationResponse"
    "fixture/GetApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplication)

responseGetApplicationVersion :: GetApplicationVersionResponse -> TestTree
responseGetApplicationVersion =
  res
    "GetApplicationVersionResponse"
    "fixture/GetApplicationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplicationVersion)

responseGetBatchJobExecution :: GetBatchJobExecutionResponse -> TestTree
responseGetBatchJobExecution =
  res
    "GetBatchJobExecutionResponse"
    "fixture/GetBatchJobExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBatchJobExecution)

responseGetDataSetDetails :: GetDataSetDetailsResponse -> TestTree
responseGetDataSetDetails =
  res
    "GetDataSetDetailsResponse"
    "fixture/GetDataSetDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataSetDetails)

responseGetDataSetImportTask :: GetDataSetImportTaskResponse -> TestTree
responseGetDataSetImportTask =
  res
    "GetDataSetImportTaskResponse"
    "fixture/GetDataSetImportTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataSetImportTask)

responseGetDeployment :: GetDeploymentResponse -> TestTree
responseGetDeployment =
  res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeployment)

responseGetEnvironment :: GetEnvironmentResponse -> TestTree
responseGetEnvironment =
  res
    "GetEnvironmentResponse"
    "fixture/GetEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnvironment)

responseListApplicationVersions :: ListApplicationVersionsResponse -> TestTree
responseListApplicationVersions =
  res
    "ListApplicationVersionsResponse"
    "fixture/ListApplicationVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplicationVersions)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplications)

responseListBatchJobDefinitions :: ListBatchJobDefinitionsResponse -> TestTree
responseListBatchJobDefinitions =
  res
    "ListBatchJobDefinitionsResponse"
    "fixture/ListBatchJobDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBatchJobDefinitions)

responseListBatchJobExecutions :: ListBatchJobExecutionsResponse -> TestTree
responseListBatchJobExecutions =
  res
    "ListBatchJobExecutionsResponse"
    "fixture/ListBatchJobExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBatchJobExecutions)

responseListDataSetImportHistory :: ListDataSetImportHistoryResponse -> TestTree
responseListDataSetImportHistory =
  res
    "ListDataSetImportHistoryResponse"
    "fixture/ListDataSetImportHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataSetImportHistory)

responseListDataSets :: ListDataSetsResponse -> TestTree
responseListDataSets =
  res
    "ListDataSetsResponse"
    "fixture/ListDataSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataSets)

responseListDeployments :: ListDeploymentsResponse -> TestTree
responseListDeployments =
  res
    "ListDeploymentsResponse"
    "fixture/ListDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeployments)

responseListEngineVersions :: ListEngineVersionsResponse -> TestTree
responseListEngineVersions =
  res
    "ListEngineVersionsResponse"
    "fixture/ListEngineVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEngineVersions)

responseListEnvironments :: ListEnvironmentsResponse -> TestTree
responseListEnvironments =
  res
    "ListEnvironmentsResponse"
    "fixture/ListEnvironmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironments)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartApplication :: StartApplicationResponse -> TestTree
responseStartApplication =
  res
    "StartApplicationResponse"
    "fixture/StartApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartApplication)

responseStartBatchJob :: StartBatchJobResponse -> TestTree
responseStartBatchJob =
  res
    "StartBatchJobResponse"
    "fixture/StartBatchJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartBatchJob)

responseStopApplication :: StopApplicationResponse -> TestTree
responseStopApplication =
  res
    "StopApplicationResponse"
    "fixture/StopApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopApplication)

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

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplication)

responseUpdateEnvironment :: UpdateEnvironmentResponse -> TestTree
responseUpdateEnvironment =
  res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEnvironment)
