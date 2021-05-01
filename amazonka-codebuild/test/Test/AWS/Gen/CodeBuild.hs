{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeBuild
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CodeBuild where

import Data.Proxy
import Network.AWS.CodeBuild
import Test.AWS.CodeBuild.Internal
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
--         [ requestDeleteReport $
--             newDeleteReport
--
--         , requestBatchGetReports $
--             newBatchGetReports
--
--         , requestListBuilds $
--             newListBuilds
--
--         , requestGetResourcePolicy $
--             newGetResourcePolicy
--
--         , requestListProjects $
--             newListProjects
--
--         , requestDescribeTestCases $
--             newDescribeTestCases
--
--         , requestListBuildsForProject $
--             newListBuildsForProject
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestListBuildBatches $
--             newListBuildBatches
--
--         , requestDeleteBuildBatch $
--             newDeleteBuildBatch
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestDeleteReportGroup $
--             newDeleteReportGroup
--
--         , requestBatchDeleteBuilds $
--             newBatchDeleteBuilds
--
--         , requestBatchGetReportGroups $
--             newBatchGetReportGroups
--
--         , requestUpdateReportGroup $
--             newUpdateReportGroup
--
--         , requestListBuildBatchesForProject $
--             newListBuildBatchesForProject
--
--         , requestCreateReportGroup $
--             newCreateReportGroup
--
--         , requestDescribeCodeCoverages $
--             newDescribeCodeCoverages
--
--         , requestStartBuildBatch $
--             newStartBuildBatch
--
--         , requestUpdateWebhook $
--             newUpdateWebhook
--
--         , requestRetryBuildBatch $
--             newRetryBuildBatch
--
--         , requestDeleteWebhook $
--             newDeleteWebhook
--
--         , requestStopBuildBatch $
--             newStopBuildBatch
--
--         , requestListSourceCredentials $
--             newListSourceCredentials
--
--         , requestCreateWebhook $
--             newCreateWebhook
--
--         , requestBatchGetBuilds $
--             newBatchGetBuilds
--
--         , requestListReports $
--             newListReports
--
--         , requestBatchGetProjects $
--             newBatchGetProjects
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestUpdateProject $
--             newUpdateProject
--
--         , requestDeleteSourceCredentials $
--             newDeleteSourceCredentials
--
--         , requestListSharedReportGroups $
--             newListSharedReportGroups
--
--         , requestStopBuild $
--             newStopBuild
--
--         , requestRetryBuild $
--             newRetryBuild
--
--         , requestBatchGetBuildBatches $
--             newBatchGetBuildBatches
--
--         , requestGetReportGroupTrend $
--             newGetReportGroupTrend
--
--         , requestStartBuild $
--             newStartBuild
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestListCuratedEnvironmentImages $
--             newListCuratedEnvironmentImages
--
--         , requestListReportGroups $
--             newListReportGroups
--
--         , requestImportSourceCredentials $
--             newImportSourceCredentials
--
--         , requestInvalidateProjectCache $
--             newInvalidateProjectCache
--
--         , requestListReportsForReportGroup $
--             newListReportsForReportGroup
--
--         , requestListSharedProjects $
--             newListSharedProjects
--
--           ]

--     , testGroup "response"
--         [ responseDeleteReport $
--             newDeleteReportResponse
--
--         , responseBatchGetReports $
--             newBatchGetReportsResponse
--
--         , responseListBuilds $
--             newListBuildsResponse
--
--         , responseGetResourcePolicy $
--             newGetResourcePolicyResponse
--
--         , responseListProjects $
--             newListProjectsResponse
--
--         , responseDescribeTestCases $
--             newDescribeTestCasesResponse
--
--         , responseListBuildsForProject $
--             newListBuildsForProjectResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseListBuildBatches $
--             newListBuildBatchesResponse
--
--         , responseDeleteBuildBatch $
--             newDeleteBuildBatchResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseDeleteReportGroup $
--             newDeleteReportGroupResponse
--
--         , responseBatchDeleteBuilds $
--             newBatchDeleteBuildsResponse
--
--         , responseBatchGetReportGroups $
--             newBatchGetReportGroupsResponse
--
--         , responseUpdateReportGroup $
--             newUpdateReportGroupResponse
--
--         , responseListBuildBatchesForProject $
--             newListBuildBatchesForProjectResponse
--
--         , responseCreateReportGroup $
--             newCreateReportGroupResponse
--
--         , responseDescribeCodeCoverages $
--             newDescribeCodeCoveragesResponse
--
--         , responseStartBuildBatch $
--             newStartBuildBatchResponse
--
--         , responseUpdateWebhook $
--             newUpdateWebhookResponse
--
--         , responseRetryBuildBatch $
--             newRetryBuildBatchResponse
--
--         , responseDeleteWebhook $
--             newDeleteWebhookResponse
--
--         , responseStopBuildBatch $
--             newStopBuildBatchResponse
--
--         , responseListSourceCredentials $
--             newListSourceCredentialsResponse
--
--         , responseCreateWebhook $
--             newCreateWebhookResponse
--
--         , responseBatchGetBuilds $
--             newBatchGetBuildsResponse
--
--         , responseListReports $
--             newListReportsResponse
--
--         , responseBatchGetProjects $
--             newBatchGetProjectsResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--         , responseDeleteSourceCredentials $
--             newDeleteSourceCredentialsResponse
--
--         , responseListSharedReportGroups $
--             newListSharedReportGroupsResponse
--
--         , responseStopBuild $
--             newStopBuildResponse
--
--         , responseRetryBuild $
--             newRetryBuildResponse
--
--         , responseBatchGetBuildBatches $
--             newBatchGetBuildBatchesResponse
--
--         , responseGetReportGroupTrend $
--             newGetReportGroupTrendResponse
--
--         , responseStartBuild $
--             newStartBuildResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseListCuratedEnvironmentImages $
--             newListCuratedEnvironmentImagesResponse
--
--         , responseListReportGroups $
--             newListReportGroupsResponse
--
--         , responseImportSourceCredentials $
--             newImportSourceCredentialsResponse
--
--         , responseInvalidateProjectCache $
--             newInvalidateProjectCacheResponse
--
--         , responseListReportsForReportGroup $
--             newListReportsForReportGroupResponse
--
--         , responseListSharedProjects $
--             newListSharedProjectsResponse
--
--           ]
--     ]

-- Requests

requestDeleteReport :: DeleteReport -> TestTree
requestDeleteReport =
  req
    "DeleteReport"
    "fixture/DeleteReport.yaml"

requestBatchGetReports :: BatchGetReports -> TestTree
requestBatchGetReports =
  req
    "BatchGetReports"
    "fixture/BatchGetReports.yaml"

requestListBuilds :: ListBuilds -> TestTree
requestListBuilds =
  req
    "ListBuilds"
    "fixture/ListBuilds.yaml"

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy =
  req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestDescribeTestCases :: DescribeTestCases -> TestTree
requestDescribeTestCases =
  req
    "DescribeTestCases"
    "fixture/DescribeTestCases.yaml"

requestListBuildsForProject :: ListBuildsForProject -> TestTree
requestListBuildsForProject =
  req
    "ListBuildsForProject"
    "fixture/ListBuildsForProject.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestListBuildBatches :: ListBuildBatches -> TestTree
requestListBuildBatches =
  req
    "ListBuildBatches"
    "fixture/ListBuildBatches.yaml"

requestDeleteBuildBatch :: DeleteBuildBatch -> TestTree
requestDeleteBuildBatch =
  req
    "DeleteBuildBatch"
    "fixture/DeleteBuildBatch.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestDeleteReportGroup :: DeleteReportGroup -> TestTree
requestDeleteReportGroup =
  req
    "DeleteReportGroup"
    "fixture/DeleteReportGroup.yaml"

requestBatchDeleteBuilds :: BatchDeleteBuilds -> TestTree
requestBatchDeleteBuilds =
  req
    "BatchDeleteBuilds"
    "fixture/BatchDeleteBuilds.yaml"

requestBatchGetReportGroups :: BatchGetReportGroups -> TestTree
requestBatchGetReportGroups =
  req
    "BatchGetReportGroups"
    "fixture/BatchGetReportGroups.yaml"

requestUpdateReportGroup :: UpdateReportGroup -> TestTree
requestUpdateReportGroup =
  req
    "UpdateReportGroup"
    "fixture/UpdateReportGroup.yaml"

requestListBuildBatchesForProject :: ListBuildBatchesForProject -> TestTree
requestListBuildBatchesForProject =
  req
    "ListBuildBatchesForProject"
    "fixture/ListBuildBatchesForProject.yaml"

requestCreateReportGroup :: CreateReportGroup -> TestTree
requestCreateReportGroup =
  req
    "CreateReportGroup"
    "fixture/CreateReportGroup.yaml"

requestDescribeCodeCoverages :: DescribeCodeCoverages -> TestTree
requestDescribeCodeCoverages =
  req
    "DescribeCodeCoverages"
    "fixture/DescribeCodeCoverages.yaml"

requestStartBuildBatch :: StartBuildBatch -> TestTree
requestStartBuildBatch =
  req
    "StartBuildBatch"
    "fixture/StartBuildBatch.yaml"

requestUpdateWebhook :: UpdateWebhook -> TestTree
requestUpdateWebhook =
  req
    "UpdateWebhook"
    "fixture/UpdateWebhook.yaml"

requestRetryBuildBatch :: RetryBuildBatch -> TestTree
requestRetryBuildBatch =
  req
    "RetryBuildBatch"
    "fixture/RetryBuildBatch.yaml"

requestDeleteWebhook :: DeleteWebhook -> TestTree
requestDeleteWebhook =
  req
    "DeleteWebhook"
    "fixture/DeleteWebhook.yaml"

requestStopBuildBatch :: StopBuildBatch -> TestTree
requestStopBuildBatch =
  req
    "StopBuildBatch"
    "fixture/StopBuildBatch.yaml"

requestListSourceCredentials :: ListSourceCredentials -> TestTree
requestListSourceCredentials =
  req
    "ListSourceCredentials"
    "fixture/ListSourceCredentials.yaml"

requestCreateWebhook :: CreateWebhook -> TestTree
requestCreateWebhook =
  req
    "CreateWebhook"
    "fixture/CreateWebhook.yaml"

requestBatchGetBuilds :: BatchGetBuilds -> TestTree
requestBatchGetBuilds =
  req
    "BatchGetBuilds"
    "fixture/BatchGetBuilds.yaml"

requestListReports :: ListReports -> TestTree
requestListReports =
  req
    "ListReports"
    "fixture/ListReports.yaml"

requestBatchGetProjects :: BatchGetProjects -> TestTree
requestBatchGetProjects =
  req
    "BatchGetProjects"
    "fixture/BatchGetProjects.yaml"

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

requestDeleteSourceCredentials :: DeleteSourceCredentials -> TestTree
requestDeleteSourceCredentials =
  req
    "DeleteSourceCredentials"
    "fixture/DeleteSourceCredentials.yaml"

requestListSharedReportGroups :: ListSharedReportGroups -> TestTree
requestListSharedReportGroups =
  req
    "ListSharedReportGroups"
    "fixture/ListSharedReportGroups.yaml"

requestStopBuild :: StopBuild -> TestTree
requestStopBuild =
  req
    "StopBuild"
    "fixture/StopBuild.yaml"

requestRetryBuild :: RetryBuild -> TestTree
requestRetryBuild =
  req
    "RetryBuild"
    "fixture/RetryBuild.yaml"

requestBatchGetBuildBatches :: BatchGetBuildBatches -> TestTree
requestBatchGetBuildBatches =
  req
    "BatchGetBuildBatches"
    "fixture/BatchGetBuildBatches.yaml"

requestGetReportGroupTrend :: GetReportGroupTrend -> TestTree
requestGetReportGroupTrend =
  req
    "GetReportGroupTrend"
    "fixture/GetReportGroupTrend.yaml"

requestStartBuild :: StartBuild -> TestTree
requestStartBuild =
  req
    "StartBuild"
    "fixture/StartBuild.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestListCuratedEnvironmentImages :: ListCuratedEnvironmentImages -> TestTree
requestListCuratedEnvironmentImages =
  req
    "ListCuratedEnvironmentImages"
    "fixture/ListCuratedEnvironmentImages.yaml"

requestListReportGroups :: ListReportGroups -> TestTree
requestListReportGroups =
  req
    "ListReportGroups"
    "fixture/ListReportGroups.yaml"

requestImportSourceCredentials :: ImportSourceCredentials -> TestTree
requestImportSourceCredentials =
  req
    "ImportSourceCredentials"
    "fixture/ImportSourceCredentials.yaml"

requestInvalidateProjectCache :: InvalidateProjectCache -> TestTree
requestInvalidateProjectCache =
  req
    "InvalidateProjectCache"
    "fixture/InvalidateProjectCache.yaml"

requestListReportsForReportGroup :: ListReportsForReportGroup -> TestTree
requestListReportsForReportGroup =
  req
    "ListReportsForReportGroup"
    "fixture/ListReportsForReportGroup.yaml"

requestListSharedProjects :: ListSharedProjects -> TestTree
requestListSharedProjects =
  req
    "ListSharedProjects"
    "fixture/ListSharedProjects.yaml"

-- Responses

responseDeleteReport :: DeleteReportResponse -> TestTree
responseDeleteReport =
  res
    "DeleteReportResponse"
    "fixture/DeleteReportResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReport)

responseBatchGetReports :: BatchGetReportsResponse -> TestTree
responseBatchGetReports =
  res
    "BatchGetReportsResponse"
    "fixture/BatchGetReportsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetReports)

responseListBuilds :: ListBuildsResponse -> TestTree
responseListBuilds =
  res
    "ListBuildsResponse"
    "fixture/ListBuildsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBuilds)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetResourcePolicy)

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProjects)

responseDescribeTestCases :: DescribeTestCasesResponse -> TestTree
responseDescribeTestCases =
  res
    "DescribeTestCasesResponse"
    "fixture/DescribeTestCasesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTestCases)

responseListBuildsForProject :: ListBuildsForProjectResponse -> TestTree
responseListBuildsForProject =
  res
    "ListBuildsForProjectResponse"
    "fixture/ListBuildsForProjectResponse.proto"
    defaultService
    (Proxy :: Proxy ListBuildsForProject)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProject)

responseListBuildBatches :: ListBuildBatchesResponse -> TestTree
responseListBuildBatches =
  res
    "ListBuildBatchesResponse"
    "fixture/ListBuildBatchesResponse.proto"
    defaultService
    (Proxy :: Proxy ListBuildBatches)

responseDeleteBuildBatch :: DeleteBuildBatchResponse -> TestTree
responseDeleteBuildBatch =
  res
    "DeleteBuildBatchResponse"
    "fixture/DeleteBuildBatchResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBuildBatch)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutResourcePolicy)

responseDeleteReportGroup :: DeleteReportGroupResponse -> TestTree
responseDeleteReportGroup =
  res
    "DeleteReportGroupResponse"
    "fixture/DeleteReportGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReportGroup)

responseBatchDeleteBuilds :: BatchDeleteBuildsResponse -> TestTree
responseBatchDeleteBuilds =
  res
    "BatchDeleteBuildsResponse"
    "fixture/BatchDeleteBuildsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDeleteBuilds)

responseBatchGetReportGroups :: BatchGetReportGroupsResponse -> TestTree
responseBatchGetReportGroups =
  res
    "BatchGetReportGroupsResponse"
    "fixture/BatchGetReportGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetReportGroups)

responseUpdateReportGroup :: UpdateReportGroupResponse -> TestTree
responseUpdateReportGroup =
  res
    "UpdateReportGroupResponse"
    "fixture/UpdateReportGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateReportGroup)

responseListBuildBatchesForProject :: ListBuildBatchesForProjectResponse -> TestTree
responseListBuildBatchesForProject =
  res
    "ListBuildBatchesForProjectResponse"
    "fixture/ListBuildBatchesForProjectResponse.proto"
    defaultService
    (Proxy :: Proxy ListBuildBatchesForProject)

responseCreateReportGroup :: CreateReportGroupResponse -> TestTree
responseCreateReportGroup =
  res
    "CreateReportGroupResponse"
    "fixture/CreateReportGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateReportGroup)

responseDescribeCodeCoverages :: DescribeCodeCoveragesResponse -> TestTree
responseDescribeCodeCoverages =
  res
    "DescribeCodeCoveragesResponse"
    "fixture/DescribeCodeCoveragesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCodeCoverages)

responseStartBuildBatch :: StartBuildBatchResponse -> TestTree
responseStartBuildBatch =
  res
    "StartBuildBatchResponse"
    "fixture/StartBuildBatchResponse.proto"
    defaultService
    (Proxy :: Proxy StartBuildBatch)

responseUpdateWebhook :: UpdateWebhookResponse -> TestTree
responseUpdateWebhook =
  res
    "UpdateWebhookResponse"
    "fixture/UpdateWebhookResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWebhook)

responseRetryBuildBatch :: RetryBuildBatchResponse -> TestTree
responseRetryBuildBatch =
  res
    "RetryBuildBatchResponse"
    "fixture/RetryBuildBatchResponse.proto"
    defaultService
    (Proxy :: Proxy RetryBuildBatch)

responseDeleteWebhook :: DeleteWebhookResponse -> TestTree
responseDeleteWebhook =
  res
    "DeleteWebhookResponse"
    "fixture/DeleteWebhookResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWebhook)

responseStopBuildBatch :: StopBuildBatchResponse -> TestTree
responseStopBuildBatch =
  res
    "StopBuildBatchResponse"
    "fixture/StopBuildBatchResponse.proto"
    defaultService
    (Proxy :: Proxy StopBuildBatch)

responseListSourceCredentials :: ListSourceCredentialsResponse -> TestTree
responseListSourceCredentials =
  res
    "ListSourceCredentialsResponse"
    "fixture/ListSourceCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSourceCredentials)

responseCreateWebhook :: CreateWebhookResponse -> TestTree
responseCreateWebhook =
  res
    "CreateWebhookResponse"
    "fixture/CreateWebhookResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWebhook)

responseBatchGetBuilds :: BatchGetBuildsResponse -> TestTree
responseBatchGetBuilds =
  res
    "BatchGetBuildsResponse"
    "fixture/BatchGetBuildsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetBuilds)

responseListReports :: ListReportsResponse -> TestTree
responseListReports =
  res
    "ListReportsResponse"
    "fixture/ListReportsResponse.proto"
    defaultService
    (Proxy :: Proxy ListReports)

responseBatchGetProjects :: BatchGetProjectsResponse -> TestTree
responseBatchGetProjects =
  res
    "BatchGetProjectsResponse"
    "fixture/BatchGetProjectsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetProjects)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProject)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProject)

responseDeleteSourceCredentials :: DeleteSourceCredentialsResponse -> TestTree
responseDeleteSourceCredentials =
  res
    "DeleteSourceCredentialsResponse"
    "fixture/DeleteSourceCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSourceCredentials)

responseListSharedReportGroups :: ListSharedReportGroupsResponse -> TestTree
responseListSharedReportGroups =
  res
    "ListSharedReportGroupsResponse"
    "fixture/ListSharedReportGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSharedReportGroups)

responseStopBuild :: StopBuildResponse -> TestTree
responseStopBuild =
  res
    "StopBuildResponse"
    "fixture/StopBuildResponse.proto"
    defaultService
    (Proxy :: Proxy StopBuild)

responseRetryBuild :: RetryBuildResponse -> TestTree
responseRetryBuild =
  res
    "RetryBuildResponse"
    "fixture/RetryBuildResponse.proto"
    defaultService
    (Proxy :: Proxy RetryBuild)

responseBatchGetBuildBatches :: BatchGetBuildBatchesResponse -> TestTree
responseBatchGetBuildBatches =
  res
    "BatchGetBuildBatchesResponse"
    "fixture/BatchGetBuildBatchesResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetBuildBatches)

responseGetReportGroupTrend :: GetReportGroupTrendResponse -> TestTree
responseGetReportGroupTrend =
  res
    "GetReportGroupTrendResponse"
    "fixture/GetReportGroupTrendResponse.proto"
    defaultService
    (Proxy :: Proxy GetReportGroupTrend)

responseStartBuild :: StartBuildResponse -> TestTree
responseStartBuild =
  res
    "StartBuildResponse"
    "fixture/StartBuildResponse.proto"
    defaultService
    (Proxy :: Proxy StartBuild)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResourcePolicy)

responseListCuratedEnvironmentImages :: ListCuratedEnvironmentImagesResponse -> TestTree
responseListCuratedEnvironmentImages =
  res
    "ListCuratedEnvironmentImagesResponse"
    "fixture/ListCuratedEnvironmentImagesResponse.proto"
    defaultService
    (Proxy :: Proxy ListCuratedEnvironmentImages)

responseListReportGroups :: ListReportGroupsResponse -> TestTree
responseListReportGroups =
  res
    "ListReportGroupsResponse"
    "fixture/ListReportGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListReportGroups)

responseImportSourceCredentials :: ImportSourceCredentialsResponse -> TestTree
responseImportSourceCredentials =
  res
    "ImportSourceCredentialsResponse"
    "fixture/ImportSourceCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy ImportSourceCredentials)

responseInvalidateProjectCache :: InvalidateProjectCacheResponse -> TestTree
responseInvalidateProjectCache =
  res
    "InvalidateProjectCacheResponse"
    "fixture/InvalidateProjectCacheResponse.proto"
    defaultService
    (Proxy :: Proxy InvalidateProjectCache)

responseListReportsForReportGroup :: ListReportsForReportGroupResponse -> TestTree
responseListReportsForReportGroup =
  res
    "ListReportsForReportGroupResponse"
    "fixture/ListReportsForReportGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ListReportsForReportGroup)

responseListSharedProjects :: ListSharedProjectsResponse -> TestTree
responseListSharedProjects =
  res
    "ListSharedProjectsResponse"
    "fixture/ListSharedProjectsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSharedProjects)
