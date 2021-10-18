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
--         [ requestListBuilds $
--             newListBuilds
--
--         , requestDeleteReport $
--             newDeleteReport
--
--         , requestBatchGetReports $
--             newBatchGetReports
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
--         , requestCreateProject $
--             newCreateProject
--
--         , requestListBuildsForProject $
--             newListBuildsForProject
--
--         , requestListBuildBatches $
--             newListBuildBatches
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestDeleteBuildBatch $
--             newDeleteBuildBatch
--
--         , requestUpdateReportGroup $
--             newUpdateReportGroup
--
--         , requestListBuildBatchesForProject $
--             newListBuildBatchesForProject
--
--         , requestBatchGetReportGroups $
--             newBatchGetReportGroups
--
--         , requestBatchDeleteBuilds $
--             newBatchDeleteBuilds
--
--         , requestDeleteReportGroup $
--             newDeleteReportGroup
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
--         , requestDeleteWebhook $
--             newDeleteWebhook
--
--         , requestUpdateProjectVisibility $
--             newUpdateProjectVisibility
--
--         , requestRetryBuildBatch $
--             newRetryBuildBatch
--
--         , requestStopBuildBatch $
--             newStopBuildBatch
--
--         , requestUpdateWebhook $
--             newUpdateWebhook
--
--         , requestBatchGetBuilds $
--             newBatchGetBuilds
--
--         , requestListReports $
--             newListReports
--
--         , requestCreateWebhook $
--             newCreateWebhook
--
--         , requestListSourceCredentials $
--             newListSourceCredentials
--
--         , requestUpdateProject $
--             newUpdateProject
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestDeleteSourceCredentials $
--             newDeleteSourceCredentials
--
--         , requestBatchGetProjects $
--             newBatchGetProjects
--
--         , requestListSharedReportGroups $
--             newListSharedReportGroups
--
--         , requestRetryBuild $
--             newRetryBuild
--
--         , requestStopBuild $
--             newStopBuild
--
--         , requestBatchGetBuildBatches $
--             newBatchGetBuildBatches
--
--         , requestStartBuild $
--             newStartBuild
--
--         , requestGetReportGroupTrend $
--             newGetReportGroupTrend
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
--         , requestInvalidateProjectCache $
--             newInvalidateProjectCache
--
--         , requestImportSourceCredentials $
--             newImportSourceCredentials
--
--         , requestListReportsForReportGroup $
--             newListReportsForReportGroup
--
--         , requestListSharedProjects $
--             newListSharedProjects
--
--           ]

--     , testGroup "response"
--         [ responseListBuilds $
--             newListBuildsResponse
--
--         , responseDeleteReport $
--             newDeleteReportResponse
--
--         , responseBatchGetReports $
--             newBatchGetReportsResponse
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
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseListBuildsForProject $
--             newListBuildsForProjectResponse
--
--         , responseListBuildBatches $
--             newListBuildBatchesResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseDeleteBuildBatch $
--             newDeleteBuildBatchResponse
--
--         , responseUpdateReportGroup $
--             newUpdateReportGroupResponse
--
--         , responseListBuildBatchesForProject $
--             newListBuildBatchesForProjectResponse
--
--         , responseBatchGetReportGroups $
--             newBatchGetReportGroupsResponse
--
--         , responseBatchDeleteBuilds $
--             newBatchDeleteBuildsResponse
--
--         , responseDeleteReportGroup $
--             newDeleteReportGroupResponse
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
--         , responseDeleteWebhook $
--             newDeleteWebhookResponse
--
--         , responseUpdateProjectVisibility $
--             newUpdateProjectVisibilityResponse
--
--         , responseRetryBuildBatch $
--             newRetryBuildBatchResponse
--
--         , responseStopBuildBatch $
--             newStopBuildBatchResponse
--
--         , responseUpdateWebhook $
--             newUpdateWebhookResponse
--
--         , responseBatchGetBuilds $
--             newBatchGetBuildsResponse
--
--         , responseListReports $
--             newListReportsResponse
--
--         , responseCreateWebhook $
--             newCreateWebhookResponse
--
--         , responseListSourceCredentials $
--             newListSourceCredentialsResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseDeleteSourceCredentials $
--             newDeleteSourceCredentialsResponse
--
--         , responseBatchGetProjects $
--             newBatchGetProjectsResponse
--
--         , responseListSharedReportGroups $
--             newListSharedReportGroupsResponse
--
--         , responseRetryBuild $
--             newRetryBuildResponse
--
--         , responseStopBuild $
--             newStopBuildResponse
--
--         , responseBatchGetBuildBatches $
--             newBatchGetBuildBatchesResponse
--
--         , responseStartBuild $
--             newStartBuildResponse
--
--         , responseGetReportGroupTrend $
--             newGetReportGroupTrendResponse
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
--         , responseInvalidateProjectCache $
--             newInvalidateProjectCacheResponse
--
--         , responseImportSourceCredentials $
--             newImportSourceCredentialsResponse
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

requestListBuilds :: ListBuilds -> TestTree
requestListBuilds =
  req
    "ListBuilds"
    "fixture/ListBuilds.yaml"

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

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestListBuildsForProject :: ListBuildsForProject -> TestTree
requestListBuildsForProject =
  req
    "ListBuildsForProject"
    "fixture/ListBuildsForProject.yaml"

requestListBuildBatches :: ListBuildBatches -> TestTree
requestListBuildBatches =
  req
    "ListBuildBatches"
    "fixture/ListBuildBatches.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestDeleteBuildBatch :: DeleteBuildBatch -> TestTree
requestDeleteBuildBatch =
  req
    "DeleteBuildBatch"
    "fixture/DeleteBuildBatch.yaml"

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

requestBatchGetReportGroups :: BatchGetReportGroups -> TestTree
requestBatchGetReportGroups =
  req
    "BatchGetReportGroups"
    "fixture/BatchGetReportGroups.yaml"

requestBatchDeleteBuilds :: BatchDeleteBuilds -> TestTree
requestBatchDeleteBuilds =
  req
    "BatchDeleteBuilds"
    "fixture/BatchDeleteBuilds.yaml"

requestDeleteReportGroup :: DeleteReportGroup -> TestTree
requestDeleteReportGroup =
  req
    "DeleteReportGroup"
    "fixture/DeleteReportGroup.yaml"

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

requestDeleteWebhook :: DeleteWebhook -> TestTree
requestDeleteWebhook =
  req
    "DeleteWebhook"
    "fixture/DeleteWebhook.yaml"

requestUpdateProjectVisibility :: UpdateProjectVisibility -> TestTree
requestUpdateProjectVisibility =
  req
    "UpdateProjectVisibility"
    "fixture/UpdateProjectVisibility.yaml"

requestRetryBuildBatch :: RetryBuildBatch -> TestTree
requestRetryBuildBatch =
  req
    "RetryBuildBatch"
    "fixture/RetryBuildBatch.yaml"

requestStopBuildBatch :: StopBuildBatch -> TestTree
requestStopBuildBatch =
  req
    "StopBuildBatch"
    "fixture/StopBuildBatch.yaml"

requestUpdateWebhook :: UpdateWebhook -> TestTree
requestUpdateWebhook =
  req
    "UpdateWebhook"
    "fixture/UpdateWebhook.yaml"

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

requestCreateWebhook :: CreateWebhook -> TestTree
requestCreateWebhook =
  req
    "CreateWebhook"
    "fixture/CreateWebhook.yaml"

requestListSourceCredentials :: ListSourceCredentials -> TestTree
requestListSourceCredentials =
  req
    "ListSourceCredentials"
    "fixture/ListSourceCredentials.yaml"

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject =
  req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestDeleteSourceCredentials :: DeleteSourceCredentials -> TestTree
requestDeleteSourceCredentials =
  req
    "DeleteSourceCredentials"
    "fixture/DeleteSourceCredentials.yaml"

requestBatchGetProjects :: BatchGetProjects -> TestTree
requestBatchGetProjects =
  req
    "BatchGetProjects"
    "fixture/BatchGetProjects.yaml"

requestListSharedReportGroups :: ListSharedReportGroups -> TestTree
requestListSharedReportGroups =
  req
    "ListSharedReportGroups"
    "fixture/ListSharedReportGroups.yaml"

requestRetryBuild :: RetryBuild -> TestTree
requestRetryBuild =
  req
    "RetryBuild"
    "fixture/RetryBuild.yaml"

requestStopBuild :: StopBuild -> TestTree
requestStopBuild =
  req
    "StopBuild"
    "fixture/StopBuild.yaml"

requestBatchGetBuildBatches :: BatchGetBuildBatches -> TestTree
requestBatchGetBuildBatches =
  req
    "BatchGetBuildBatches"
    "fixture/BatchGetBuildBatches.yaml"

requestStartBuild :: StartBuild -> TestTree
requestStartBuild =
  req
    "StartBuild"
    "fixture/StartBuild.yaml"

requestGetReportGroupTrend :: GetReportGroupTrend -> TestTree
requestGetReportGroupTrend =
  req
    "GetReportGroupTrend"
    "fixture/GetReportGroupTrend.yaml"

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

requestInvalidateProjectCache :: InvalidateProjectCache -> TestTree
requestInvalidateProjectCache =
  req
    "InvalidateProjectCache"
    "fixture/InvalidateProjectCache.yaml"

requestImportSourceCredentials :: ImportSourceCredentials -> TestTree
requestImportSourceCredentials =
  req
    "ImportSourceCredentials"
    "fixture/ImportSourceCredentials.yaml"

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

responseListBuilds :: ListBuildsResponse -> TestTree
responseListBuilds =
  res
    "ListBuildsResponse"
    "fixture/ListBuildsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBuilds)

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

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProject)

responseListBuildsForProject :: ListBuildsForProjectResponse -> TestTree
responseListBuildsForProject =
  res
    "ListBuildsForProjectResponse"
    "fixture/ListBuildsForProjectResponse.proto"
    defaultService
    (Proxy :: Proxy ListBuildsForProject)

responseListBuildBatches :: ListBuildBatchesResponse -> TestTree
responseListBuildBatches =
  res
    "ListBuildBatchesResponse"
    "fixture/ListBuildBatchesResponse.proto"
    defaultService
    (Proxy :: Proxy ListBuildBatches)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutResourcePolicy)

responseDeleteBuildBatch :: DeleteBuildBatchResponse -> TestTree
responseDeleteBuildBatch =
  res
    "DeleteBuildBatchResponse"
    "fixture/DeleteBuildBatchResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBuildBatch)

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

responseBatchGetReportGroups :: BatchGetReportGroupsResponse -> TestTree
responseBatchGetReportGroups =
  res
    "BatchGetReportGroupsResponse"
    "fixture/BatchGetReportGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetReportGroups)

responseBatchDeleteBuilds :: BatchDeleteBuildsResponse -> TestTree
responseBatchDeleteBuilds =
  res
    "BatchDeleteBuildsResponse"
    "fixture/BatchDeleteBuildsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDeleteBuilds)

responseDeleteReportGroup :: DeleteReportGroupResponse -> TestTree
responseDeleteReportGroup =
  res
    "DeleteReportGroupResponse"
    "fixture/DeleteReportGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReportGroup)

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

responseDeleteWebhook :: DeleteWebhookResponse -> TestTree
responseDeleteWebhook =
  res
    "DeleteWebhookResponse"
    "fixture/DeleteWebhookResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWebhook)

responseUpdateProjectVisibility :: UpdateProjectVisibilityResponse -> TestTree
responseUpdateProjectVisibility =
  res
    "UpdateProjectVisibilityResponse"
    "fixture/UpdateProjectVisibilityResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProjectVisibility)

responseRetryBuildBatch :: RetryBuildBatchResponse -> TestTree
responseRetryBuildBatch =
  res
    "RetryBuildBatchResponse"
    "fixture/RetryBuildBatchResponse.proto"
    defaultService
    (Proxy :: Proxy RetryBuildBatch)

responseStopBuildBatch :: StopBuildBatchResponse -> TestTree
responseStopBuildBatch =
  res
    "StopBuildBatchResponse"
    "fixture/StopBuildBatchResponse.proto"
    defaultService
    (Proxy :: Proxy StopBuildBatch)

responseUpdateWebhook :: UpdateWebhookResponse -> TestTree
responseUpdateWebhook =
  res
    "UpdateWebhookResponse"
    "fixture/UpdateWebhookResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWebhook)

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

responseCreateWebhook :: CreateWebhookResponse -> TestTree
responseCreateWebhook =
  res
    "CreateWebhookResponse"
    "fixture/CreateWebhookResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWebhook)

responseListSourceCredentials :: ListSourceCredentialsResponse -> TestTree
responseListSourceCredentials =
  res
    "ListSourceCredentialsResponse"
    "fixture/ListSourceCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSourceCredentials)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProject)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProject)

responseDeleteSourceCredentials :: DeleteSourceCredentialsResponse -> TestTree
responseDeleteSourceCredentials =
  res
    "DeleteSourceCredentialsResponse"
    "fixture/DeleteSourceCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSourceCredentials)

responseBatchGetProjects :: BatchGetProjectsResponse -> TestTree
responseBatchGetProjects =
  res
    "BatchGetProjectsResponse"
    "fixture/BatchGetProjectsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetProjects)

responseListSharedReportGroups :: ListSharedReportGroupsResponse -> TestTree
responseListSharedReportGroups =
  res
    "ListSharedReportGroupsResponse"
    "fixture/ListSharedReportGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSharedReportGroups)

responseRetryBuild :: RetryBuildResponse -> TestTree
responseRetryBuild =
  res
    "RetryBuildResponse"
    "fixture/RetryBuildResponse.proto"
    defaultService
    (Proxy :: Proxy RetryBuild)

responseStopBuild :: StopBuildResponse -> TestTree
responseStopBuild =
  res
    "StopBuildResponse"
    "fixture/StopBuildResponse.proto"
    defaultService
    (Proxy :: Proxy StopBuild)

responseBatchGetBuildBatches :: BatchGetBuildBatchesResponse -> TestTree
responseBatchGetBuildBatches =
  res
    "BatchGetBuildBatchesResponse"
    "fixture/BatchGetBuildBatchesResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetBuildBatches)

responseStartBuild :: StartBuildResponse -> TestTree
responseStartBuild =
  res
    "StartBuildResponse"
    "fixture/StartBuildResponse.proto"
    defaultService
    (Proxy :: Proxy StartBuild)

responseGetReportGroupTrend :: GetReportGroupTrendResponse -> TestTree
responseGetReportGroupTrend =
  res
    "GetReportGroupTrendResponse"
    "fixture/GetReportGroupTrendResponse.proto"
    defaultService
    (Proxy :: Proxy GetReportGroupTrend)

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

responseInvalidateProjectCache :: InvalidateProjectCacheResponse -> TestTree
responseInvalidateProjectCache =
  res
    "InvalidateProjectCacheResponse"
    "fixture/InvalidateProjectCacheResponse.proto"
    defaultService
    (Proxy :: Proxy InvalidateProjectCache)

responseImportSourceCredentials :: ImportSourceCredentialsResponse -> TestTree
responseImportSourceCredentials =
  res
    "ImportSourceCredentialsResponse"
    "fixture/ImportSourceCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy ImportSourceCredentials)

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
