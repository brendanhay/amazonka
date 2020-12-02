{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestListProjects $
--             listProjects
--
--         , requestDeleteProject $
--             deleteProject
--
--         , requestUpdateProject $
--             updateProject
--
--         , requestDeleteSourceCredentials $
--             deleteSourceCredentials
--
--         , requestListBuilds $
--             listBuilds
--
--         , requestListSourceCredentials $
--             listSourceCredentials
--
--         , requestListReports $
--             listReports
--
--         , requestDeleteReport $
--             deleteReport
--
--         , requestCreateWebhook $
--             createWebhook
--
--         , requestStopBuildBatch $
--             stopBuildBatch
--
--         , requestListSharedProjects $
--             listSharedProjects
--
--         , requestCreateReportGroup $
--             createReportGroup
--
--         , requestDescribeCodeCoverages $
--             describeCodeCoverages
--
--         , requestImportSourceCredentials $
--             importSourceCredentials
--
--         , requestListBuildBatchesForProject $
--             listBuildBatchesForProject
--
--         , requestBatchGetReportGroups $
--             batchGetReportGroups
--
--         , requestDeleteBuildBatch $
--             deleteBuildBatch
--
--         , requestStartBuild $
--             startBuild
--
--         , requestBatchGetBuildBatches $
--             batchGetBuildBatches
--
--         , requestRetryBuild $
--             retryBuild
--
--         , requestListBuildsForProject $
--             listBuildsForProject
--
--         , requestDescribeTestCases $
--             describeTestCases
--
--         , requestGetResourcePolicy $
--             getResourcePolicy
--
--         , requestBatchGetProjects $
--             batchGetProjects
--
--         , requestBatchGetBuilds $
--             batchGetBuilds
--
--         , requestBatchGetReports $
--             batchGetReports
--
--         , requestUpdateWebhook $
--             updateWebhook
--
--         , requestDeleteWebhook $
--             deleteWebhook
--
--         , requestStartBuildBatch $
--             startBuildBatch
--
--         , requestRetryBuildBatch $
--             retryBuildBatch
--
--         , requestListReportsForReportGroup $
--             listReportsForReportGroup
--
--         , requestInvalidateProjectCache $
--             invalidateProjectCache
--
--         , requestUpdateReportGroup $
--             updateReportGroup
--
--         , requestDeleteReportGroup $
--             deleteReportGroup
--
--         , requestBatchDeleteBuilds $
--             batchDeleteBuilds
--
--         , requestListReportGroups $
--             listReportGroups
--
--         , requestPutResourcePolicy $
--             putResourcePolicy
--
--         , requestDeleteResourcePolicy $
--             deleteResourcePolicy
--
--         , requestListCuratedEnvironmentImages $
--             listCuratedEnvironmentImages
--
--         , requestGetReportGroupTrend $
--             getReportGroupTrend
--
--         , requestStopBuild $
--             stopBuild
--
--         , requestListBuildBatches $
--             listBuildBatches
--
--         , requestCreateProject $
--             createProject
--
--         , requestListSharedReportGroups $
--             listSharedReportGroups
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
--         , responseDeleteSourceCredentials $
--             deleteSourceCredentialsResponse
--
--         , responseListBuilds $
--             listBuildsResponse
--
--         , responseListSourceCredentials $
--             listSourceCredentialsResponse
--
--         , responseListReports $
--             listReportsResponse
--
--         , responseDeleteReport $
--             deleteReportResponse
--
--         , responseCreateWebhook $
--             createWebhookResponse
--
--         , responseStopBuildBatch $
--             stopBuildBatchResponse
--
--         , responseListSharedProjects $
--             listSharedProjectsResponse
--
--         , responseCreateReportGroup $
--             createReportGroupResponse
--
--         , responseDescribeCodeCoverages $
--             describeCodeCoveragesResponse
--
--         , responseImportSourceCredentials $
--             importSourceCredentialsResponse
--
--         , responseListBuildBatchesForProject $
--             listBuildBatchesForProjectResponse
--
--         , responseBatchGetReportGroups $
--             batchGetReportGroupsResponse
--
--         , responseDeleteBuildBatch $
--             deleteBuildBatchResponse
--
--         , responseStartBuild $
--             startBuildResponse
--
--         , responseBatchGetBuildBatches $
--             batchGetBuildBatchesResponse
--
--         , responseRetryBuild $
--             retryBuildResponse
--
--         , responseListBuildsForProject $
--             listBuildsForProjectResponse
--
--         , responseDescribeTestCases $
--             describeTestCasesResponse
--
--         , responseGetResourcePolicy $
--             getResourcePolicyResponse
--
--         , responseBatchGetProjects $
--             batchGetProjectsResponse
--
--         , responseBatchGetBuilds $
--             batchGetBuildsResponse
--
--         , responseBatchGetReports $
--             batchGetReportsResponse
--
--         , responseUpdateWebhook $
--             updateWebhookResponse
--
--         , responseDeleteWebhook $
--             deleteWebhookResponse
--
--         , responseStartBuildBatch $
--             startBuildBatchResponse
--
--         , responseRetryBuildBatch $
--             retryBuildBatchResponse
--
--         , responseListReportsForReportGroup $
--             listReportsForReportGroupResponse
--
--         , responseInvalidateProjectCache $
--             invalidateProjectCacheResponse
--
--         , responseUpdateReportGroup $
--             updateReportGroupResponse
--
--         , responseDeleteReportGroup $
--             deleteReportGroupResponse
--
--         , responseBatchDeleteBuilds $
--             batchDeleteBuildsResponse
--
--         , responseListReportGroups $
--             listReportGroupsResponse
--
--         , responsePutResourcePolicy $
--             putResourcePolicyResponse
--
--         , responseDeleteResourcePolicy $
--             deleteResourcePolicyResponse
--
--         , responseListCuratedEnvironmentImages $
--             listCuratedEnvironmentImagesResponse
--
--         , responseGetReportGroupTrend $
--             getReportGroupTrendResponse
--
--         , responseStopBuild $
--             stopBuildResponse
--
--         , responseListBuildBatches $
--             listBuildBatchesResponse
--
--         , responseCreateProject $
--             createProjectResponse
--
--         , responseListSharedReportGroups $
--             listSharedReportGroupsResponse
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

requestDeleteSourceCredentials :: DeleteSourceCredentials -> TestTree
requestDeleteSourceCredentials =
  req
    "DeleteSourceCredentials"
    "fixture/DeleteSourceCredentials.yaml"

requestListBuilds :: ListBuilds -> TestTree
requestListBuilds =
  req
    "ListBuilds"
    "fixture/ListBuilds.yaml"

requestListSourceCredentials :: ListSourceCredentials -> TestTree
requestListSourceCredentials =
  req
    "ListSourceCredentials"
    "fixture/ListSourceCredentials.yaml"

requestListReports :: ListReports -> TestTree
requestListReports =
  req
    "ListReports"
    "fixture/ListReports.yaml"

requestDeleteReport :: DeleteReport -> TestTree
requestDeleteReport =
  req
    "DeleteReport"
    "fixture/DeleteReport.yaml"

requestCreateWebhook :: CreateWebhook -> TestTree
requestCreateWebhook =
  req
    "CreateWebhook"
    "fixture/CreateWebhook.yaml"

requestStopBuildBatch :: StopBuildBatch -> TestTree
requestStopBuildBatch =
  req
    "StopBuildBatch"
    "fixture/StopBuildBatch.yaml"

requestListSharedProjects :: ListSharedProjects -> TestTree
requestListSharedProjects =
  req
    "ListSharedProjects"
    "fixture/ListSharedProjects.yaml"

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

requestImportSourceCredentials :: ImportSourceCredentials -> TestTree
requestImportSourceCredentials =
  req
    "ImportSourceCredentials"
    "fixture/ImportSourceCredentials.yaml"

requestListBuildBatchesForProject :: ListBuildBatchesForProject -> TestTree
requestListBuildBatchesForProject =
  req
    "ListBuildBatchesForProject"
    "fixture/ListBuildBatchesForProject.yaml"

requestBatchGetReportGroups :: BatchGetReportGroups -> TestTree
requestBatchGetReportGroups =
  req
    "BatchGetReportGroups"
    "fixture/BatchGetReportGroups.yaml"

requestDeleteBuildBatch :: DeleteBuildBatch -> TestTree
requestDeleteBuildBatch =
  req
    "DeleteBuildBatch"
    "fixture/DeleteBuildBatch.yaml"

requestStartBuild :: StartBuild -> TestTree
requestStartBuild =
  req
    "StartBuild"
    "fixture/StartBuild.yaml"

requestBatchGetBuildBatches :: BatchGetBuildBatches -> TestTree
requestBatchGetBuildBatches =
  req
    "BatchGetBuildBatches"
    "fixture/BatchGetBuildBatches.yaml"

requestRetryBuild :: RetryBuild -> TestTree
requestRetryBuild =
  req
    "RetryBuild"
    "fixture/RetryBuild.yaml"

requestListBuildsForProject :: ListBuildsForProject -> TestTree
requestListBuildsForProject =
  req
    "ListBuildsForProject"
    "fixture/ListBuildsForProject.yaml"

requestDescribeTestCases :: DescribeTestCases -> TestTree
requestDescribeTestCases =
  req
    "DescribeTestCases"
    "fixture/DescribeTestCases.yaml"

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy =
  req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

requestBatchGetProjects :: BatchGetProjects -> TestTree
requestBatchGetProjects =
  req
    "BatchGetProjects"
    "fixture/BatchGetProjects.yaml"

requestBatchGetBuilds :: BatchGetBuilds -> TestTree
requestBatchGetBuilds =
  req
    "BatchGetBuilds"
    "fixture/BatchGetBuilds.yaml"

requestBatchGetReports :: BatchGetReports -> TestTree
requestBatchGetReports =
  req
    "BatchGetReports"
    "fixture/BatchGetReports.yaml"

requestUpdateWebhook :: UpdateWebhook -> TestTree
requestUpdateWebhook =
  req
    "UpdateWebhook"
    "fixture/UpdateWebhook.yaml"

requestDeleteWebhook :: DeleteWebhook -> TestTree
requestDeleteWebhook =
  req
    "DeleteWebhook"
    "fixture/DeleteWebhook.yaml"

requestStartBuildBatch :: StartBuildBatch -> TestTree
requestStartBuildBatch =
  req
    "StartBuildBatch"
    "fixture/StartBuildBatch.yaml"

requestRetryBuildBatch :: RetryBuildBatch -> TestTree
requestRetryBuildBatch =
  req
    "RetryBuildBatch"
    "fixture/RetryBuildBatch.yaml"

requestListReportsForReportGroup :: ListReportsForReportGroup -> TestTree
requestListReportsForReportGroup =
  req
    "ListReportsForReportGroup"
    "fixture/ListReportsForReportGroup.yaml"

requestInvalidateProjectCache :: InvalidateProjectCache -> TestTree
requestInvalidateProjectCache =
  req
    "InvalidateProjectCache"
    "fixture/InvalidateProjectCache.yaml"

requestUpdateReportGroup :: UpdateReportGroup -> TestTree
requestUpdateReportGroup =
  req
    "UpdateReportGroup"
    "fixture/UpdateReportGroup.yaml"

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

requestListReportGroups :: ListReportGroups -> TestTree
requestListReportGroups =
  req
    "ListReportGroups"
    "fixture/ListReportGroups.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

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

requestGetReportGroupTrend :: GetReportGroupTrend -> TestTree
requestGetReportGroupTrend =
  req
    "GetReportGroupTrend"
    "fixture/GetReportGroupTrend.yaml"

requestStopBuild :: StopBuild -> TestTree
requestStopBuild =
  req
    "StopBuild"
    "fixture/StopBuild.yaml"

requestListBuildBatches :: ListBuildBatches -> TestTree
requestListBuildBatches =
  req
    "ListBuildBatches"
    "fixture/ListBuildBatches.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestListSharedReportGroups :: ListSharedReportGroups -> TestTree
requestListSharedReportGroups =
  req
    "ListSharedReportGroups"
    "fixture/ListSharedReportGroups.yaml"

-- Responses

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    codeBuild
    (Proxy :: Proxy ListProjects)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    codeBuild
    (Proxy :: Proxy DeleteProject)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    codeBuild
    (Proxy :: Proxy UpdateProject)

responseDeleteSourceCredentials :: DeleteSourceCredentialsResponse -> TestTree
responseDeleteSourceCredentials =
  res
    "DeleteSourceCredentialsResponse"
    "fixture/DeleteSourceCredentialsResponse.proto"
    codeBuild
    (Proxy :: Proxy DeleteSourceCredentials)

responseListBuilds :: ListBuildsResponse -> TestTree
responseListBuilds =
  res
    "ListBuildsResponse"
    "fixture/ListBuildsResponse.proto"
    codeBuild
    (Proxy :: Proxy ListBuilds)

responseListSourceCredentials :: ListSourceCredentialsResponse -> TestTree
responseListSourceCredentials =
  res
    "ListSourceCredentialsResponse"
    "fixture/ListSourceCredentialsResponse.proto"
    codeBuild
    (Proxy :: Proxy ListSourceCredentials)

responseListReports :: ListReportsResponse -> TestTree
responseListReports =
  res
    "ListReportsResponse"
    "fixture/ListReportsResponse.proto"
    codeBuild
    (Proxy :: Proxy ListReports)

responseDeleteReport :: DeleteReportResponse -> TestTree
responseDeleteReport =
  res
    "DeleteReportResponse"
    "fixture/DeleteReportResponse.proto"
    codeBuild
    (Proxy :: Proxy DeleteReport)

responseCreateWebhook :: CreateWebhookResponse -> TestTree
responseCreateWebhook =
  res
    "CreateWebhookResponse"
    "fixture/CreateWebhookResponse.proto"
    codeBuild
    (Proxy :: Proxy CreateWebhook)

responseStopBuildBatch :: StopBuildBatchResponse -> TestTree
responseStopBuildBatch =
  res
    "StopBuildBatchResponse"
    "fixture/StopBuildBatchResponse.proto"
    codeBuild
    (Proxy :: Proxy StopBuildBatch)

responseListSharedProjects :: ListSharedProjectsResponse -> TestTree
responseListSharedProjects =
  res
    "ListSharedProjectsResponse"
    "fixture/ListSharedProjectsResponse.proto"
    codeBuild
    (Proxy :: Proxy ListSharedProjects)

responseCreateReportGroup :: CreateReportGroupResponse -> TestTree
responseCreateReportGroup =
  res
    "CreateReportGroupResponse"
    "fixture/CreateReportGroupResponse.proto"
    codeBuild
    (Proxy :: Proxy CreateReportGroup)

responseDescribeCodeCoverages :: DescribeCodeCoveragesResponse -> TestTree
responseDescribeCodeCoverages =
  res
    "DescribeCodeCoveragesResponse"
    "fixture/DescribeCodeCoveragesResponse.proto"
    codeBuild
    (Proxy :: Proxy DescribeCodeCoverages)

responseImportSourceCredentials :: ImportSourceCredentialsResponse -> TestTree
responseImportSourceCredentials =
  res
    "ImportSourceCredentialsResponse"
    "fixture/ImportSourceCredentialsResponse.proto"
    codeBuild
    (Proxy :: Proxy ImportSourceCredentials)

responseListBuildBatchesForProject :: ListBuildBatchesForProjectResponse -> TestTree
responseListBuildBatchesForProject =
  res
    "ListBuildBatchesForProjectResponse"
    "fixture/ListBuildBatchesForProjectResponse.proto"
    codeBuild
    (Proxy :: Proxy ListBuildBatchesForProject)

responseBatchGetReportGroups :: BatchGetReportGroupsResponse -> TestTree
responseBatchGetReportGroups =
  res
    "BatchGetReportGroupsResponse"
    "fixture/BatchGetReportGroupsResponse.proto"
    codeBuild
    (Proxy :: Proxy BatchGetReportGroups)

responseDeleteBuildBatch :: DeleteBuildBatchResponse -> TestTree
responseDeleteBuildBatch =
  res
    "DeleteBuildBatchResponse"
    "fixture/DeleteBuildBatchResponse.proto"
    codeBuild
    (Proxy :: Proxy DeleteBuildBatch)

responseStartBuild :: StartBuildResponse -> TestTree
responseStartBuild =
  res
    "StartBuildResponse"
    "fixture/StartBuildResponse.proto"
    codeBuild
    (Proxy :: Proxy StartBuild)

responseBatchGetBuildBatches :: BatchGetBuildBatchesResponse -> TestTree
responseBatchGetBuildBatches =
  res
    "BatchGetBuildBatchesResponse"
    "fixture/BatchGetBuildBatchesResponse.proto"
    codeBuild
    (Proxy :: Proxy BatchGetBuildBatches)

responseRetryBuild :: RetryBuildResponse -> TestTree
responseRetryBuild =
  res
    "RetryBuildResponse"
    "fixture/RetryBuildResponse.proto"
    codeBuild
    (Proxy :: Proxy RetryBuild)

responseListBuildsForProject :: ListBuildsForProjectResponse -> TestTree
responseListBuildsForProject =
  res
    "ListBuildsForProjectResponse"
    "fixture/ListBuildsForProjectResponse.proto"
    codeBuild
    (Proxy :: Proxy ListBuildsForProject)

responseDescribeTestCases :: DescribeTestCasesResponse -> TestTree
responseDescribeTestCases =
  res
    "DescribeTestCasesResponse"
    "fixture/DescribeTestCasesResponse.proto"
    codeBuild
    (Proxy :: Proxy DescribeTestCases)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    codeBuild
    (Proxy :: Proxy GetResourcePolicy)

responseBatchGetProjects :: BatchGetProjectsResponse -> TestTree
responseBatchGetProjects =
  res
    "BatchGetProjectsResponse"
    "fixture/BatchGetProjectsResponse.proto"
    codeBuild
    (Proxy :: Proxy BatchGetProjects)

responseBatchGetBuilds :: BatchGetBuildsResponse -> TestTree
responseBatchGetBuilds =
  res
    "BatchGetBuildsResponse"
    "fixture/BatchGetBuildsResponse.proto"
    codeBuild
    (Proxy :: Proxy BatchGetBuilds)

responseBatchGetReports :: BatchGetReportsResponse -> TestTree
responseBatchGetReports =
  res
    "BatchGetReportsResponse"
    "fixture/BatchGetReportsResponse.proto"
    codeBuild
    (Proxy :: Proxy BatchGetReports)

responseUpdateWebhook :: UpdateWebhookResponse -> TestTree
responseUpdateWebhook =
  res
    "UpdateWebhookResponse"
    "fixture/UpdateWebhookResponse.proto"
    codeBuild
    (Proxy :: Proxy UpdateWebhook)

responseDeleteWebhook :: DeleteWebhookResponse -> TestTree
responseDeleteWebhook =
  res
    "DeleteWebhookResponse"
    "fixture/DeleteWebhookResponse.proto"
    codeBuild
    (Proxy :: Proxy DeleteWebhook)

responseStartBuildBatch :: StartBuildBatchResponse -> TestTree
responseStartBuildBatch =
  res
    "StartBuildBatchResponse"
    "fixture/StartBuildBatchResponse.proto"
    codeBuild
    (Proxy :: Proxy StartBuildBatch)

responseRetryBuildBatch :: RetryBuildBatchResponse -> TestTree
responseRetryBuildBatch =
  res
    "RetryBuildBatchResponse"
    "fixture/RetryBuildBatchResponse.proto"
    codeBuild
    (Proxy :: Proxy RetryBuildBatch)

responseListReportsForReportGroup :: ListReportsForReportGroupResponse -> TestTree
responseListReportsForReportGroup =
  res
    "ListReportsForReportGroupResponse"
    "fixture/ListReportsForReportGroupResponse.proto"
    codeBuild
    (Proxy :: Proxy ListReportsForReportGroup)

responseInvalidateProjectCache :: InvalidateProjectCacheResponse -> TestTree
responseInvalidateProjectCache =
  res
    "InvalidateProjectCacheResponse"
    "fixture/InvalidateProjectCacheResponse.proto"
    codeBuild
    (Proxy :: Proxy InvalidateProjectCache)

responseUpdateReportGroup :: UpdateReportGroupResponse -> TestTree
responseUpdateReportGroup =
  res
    "UpdateReportGroupResponse"
    "fixture/UpdateReportGroupResponse.proto"
    codeBuild
    (Proxy :: Proxy UpdateReportGroup)

responseDeleteReportGroup :: DeleteReportGroupResponse -> TestTree
responseDeleteReportGroup =
  res
    "DeleteReportGroupResponse"
    "fixture/DeleteReportGroupResponse.proto"
    codeBuild
    (Proxy :: Proxy DeleteReportGroup)

responseBatchDeleteBuilds :: BatchDeleteBuildsResponse -> TestTree
responseBatchDeleteBuilds =
  res
    "BatchDeleteBuildsResponse"
    "fixture/BatchDeleteBuildsResponse.proto"
    codeBuild
    (Proxy :: Proxy BatchDeleteBuilds)

responseListReportGroups :: ListReportGroupsResponse -> TestTree
responseListReportGroups =
  res
    "ListReportGroupsResponse"
    "fixture/ListReportGroupsResponse.proto"
    codeBuild
    (Proxy :: Proxy ListReportGroups)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    codeBuild
    (Proxy :: Proxy PutResourcePolicy)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    codeBuild
    (Proxy :: Proxy DeleteResourcePolicy)

responseListCuratedEnvironmentImages :: ListCuratedEnvironmentImagesResponse -> TestTree
responseListCuratedEnvironmentImages =
  res
    "ListCuratedEnvironmentImagesResponse"
    "fixture/ListCuratedEnvironmentImagesResponse.proto"
    codeBuild
    (Proxy :: Proxy ListCuratedEnvironmentImages)

responseGetReportGroupTrend :: GetReportGroupTrendResponse -> TestTree
responseGetReportGroupTrend =
  res
    "GetReportGroupTrendResponse"
    "fixture/GetReportGroupTrendResponse.proto"
    codeBuild
    (Proxy :: Proxy GetReportGroupTrend)

responseStopBuild :: StopBuildResponse -> TestTree
responseStopBuild =
  res
    "StopBuildResponse"
    "fixture/StopBuildResponse.proto"
    codeBuild
    (Proxy :: Proxy StopBuild)

responseListBuildBatches :: ListBuildBatchesResponse -> TestTree
responseListBuildBatches =
  res
    "ListBuildBatchesResponse"
    "fixture/ListBuildBatchesResponse.proto"
    codeBuild
    (Proxy :: Proxy ListBuildBatches)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    codeBuild
    (Proxy :: Proxy CreateProject)

responseListSharedReportGroups :: ListSharedReportGroupsResponse -> TestTree
responseListSharedReportGroups =
  res
    "ListSharedReportGroupsResponse"
    "fixture/ListSharedReportGroupsResponse.proto"
    codeBuild
    (Proxy :: Proxy ListSharedReportGroups)
