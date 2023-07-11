{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CodeBuild
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CodeBuild where

import Amazonka.CodeBuild
import qualified Data.Proxy as Proxy
import Test.Amazonka.CodeBuild.Internal
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
--         [ requestBatchDeleteBuilds $
--             newBatchDeleteBuilds
--
--         , requestBatchGetBuildBatches $
--             newBatchGetBuildBatches
--
--         , requestBatchGetBuilds $
--             newBatchGetBuilds
--
--         , requestBatchGetProjects $
--             newBatchGetProjects
--
--         , requestBatchGetReportGroups $
--             newBatchGetReportGroups
--
--         , requestBatchGetReports $
--             newBatchGetReports
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestCreateReportGroup $
--             newCreateReportGroup
--
--         , requestCreateWebhook $
--             newCreateWebhook
--
--         , requestDeleteBuildBatch $
--             newDeleteBuildBatch
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestDeleteReport $
--             newDeleteReport
--
--         , requestDeleteReportGroup $
--             newDeleteReportGroup
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestDeleteSourceCredentials $
--             newDeleteSourceCredentials
--
--         , requestDeleteWebhook $
--             newDeleteWebhook
--
--         , requestDescribeCodeCoverages $
--             newDescribeCodeCoverages
--
--         , requestDescribeTestCases $
--             newDescribeTestCases
--
--         , requestGetReportGroupTrend $
--             newGetReportGroupTrend
--
--         , requestGetResourcePolicy $
--             newGetResourcePolicy
--
--         , requestImportSourceCredentials $
--             newImportSourceCredentials
--
--         , requestInvalidateProjectCache $
--             newInvalidateProjectCache
--
--         , requestListBuildBatches $
--             newListBuildBatches
--
--         , requestListBuildBatchesForProject $
--             newListBuildBatchesForProject
--
--         , requestListBuilds $
--             newListBuilds
--
--         , requestListBuildsForProject $
--             newListBuildsForProject
--
--         , requestListCuratedEnvironmentImages $
--             newListCuratedEnvironmentImages
--
--         , requestListProjects $
--             newListProjects
--
--         , requestListReportGroups $
--             newListReportGroups
--
--         , requestListReports $
--             newListReports
--
--         , requestListReportsForReportGroup $
--             newListReportsForReportGroup
--
--         , requestListSharedProjects $
--             newListSharedProjects
--
--         , requestListSharedReportGroups $
--             newListSharedReportGroups
--
--         , requestListSourceCredentials $
--             newListSourceCredentials
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestRetryBuild $
--             newRetryBuild
--
--         , requestRetryBuildBatch $
--             newRetryBuildBatch
--
--         , requestStartBuild $
--             newStartBuild
--
--         , requestStartBuildBatch $
--             newStartBuildBatch
--
--         , requestStopBuild $
--             newStopBuild
--
--         , requestStopBuildBatch $
--             newStopBuildBatch
--
--         , requestUpdateProject $
--             newUpdateProject
--
--         , requestUpdateProjectVisibility $
--             newUpdateProjectVisibility
--
--         , requestUpdateReportGroup $
--             newUpdateReportGroup
--
--         , requestUpdateWebhook $
--             newUpdateWebhook
--
--           ]

--     , testGroup "response"
--         [ responseBatchDeleteBuilds $
--             newBatchDeleteBuildsResponse
--
--         , responseBatchGetBuildBatches $
--             newBatchGetBuildBatchesResponse
--
--         , responseBatchGetBuilds $
--             newBatchGetBuildsResponse
--
--         , responseBatchGetProjects $
--             newBatchGetProjectsResponse
--
--         , responseBatchGetReportGroups $
--             newBatchGetReportGroupsResponse
--
--         , responseBatchGetReports $
--             newBatchGetReportsResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseCreateReportGroup $
--             newCreateReportGroupResponse
--
--         , responseCreateWebhook $
--             newCreateWebhookResponse
--
--         , responseDeleteBuildBatch $
--             newDeleteBuildBatchResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseDeleteReport $
--             newDeleteReportResponse
--
--         , responseDeleteReportGroup $
--             newDeleteReportGroupResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseDeleteSourceCredentials $
--             newDeleteSourceCredentialsResponse
--
--         , responseDeleteWebhook $
--             newDeleteWebhookResponse
--
--         , responseDescribeCodeCoverages $
--             newDescribeCodeCoveragesResponse
--
--         , responseDescribeTestCases $
--             newDescribeTestCasesResponse
--
--         , responseGetReportGroupTrend $
--             newGetReportGroupTrendResponse
--
--         , responseGetResourcePolicy $
--             newGetResourcePolicyResponse
--
--         , responseImportSourceCredentials $
--             newImportSourceCredentialsResponse
--
--         , responseInvalidateProjectCache $
--             newInvalidateProjectCacheResponse
--
--         , responseListBuildBatches $
--             newListBuildBatchesResponse
--
--         , responseListBuildBatchesForProject $
--             newListBuildBatchesForProjectResponse
--
--         , responseListBuilds $
--             newListBuildsResponse
--
--         , responseListBuildsForProject $
--             newListBuildsForProjectResponse
--
--         , responseListCuratedEnvironmentImages $
--             newListCuratedEnvironmentImagesResponse
--
--         , responseListProjects $
--             newListProjectsResponse
--
--         , responseListReportGroups $
--             newListReportGroupsResponse
--
--         , responseListReports $
--             newListReportsResponse
--
--         , responseListReportsForReportGroup $
--             newListReportsForReportGroupResponse
--
--         , responseListSharedProjects $
--             newListSharedProjectsResponse
--
--         , responseListSharedReportGroups $
--             newListSharedReportGroupsResponse
--
--         , responseListSourceCredentials $
--             newListSourceCredentialsResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseRetryBuild $
--             newRetryBuildResponse
--
--         , responseRetryBuildBatch $
--             newRetryBuildBatchResponse
--
--         , responseStartBuild $
--             newStartBuildResponse
--
--         , responseStartBuildBatch $
--             newStartBuildBatchResponse
--
--         , responseStopBuild $
--             newStopBuildResponse
--
--         , responseStopBuildBatch $
--             newStopBuildBatchResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--         , responseUpdateProjectVisibility $
--             newUpdateProjectVisibilityResponse
--
--         , responseUpdateReportGroup $
--             newUpdateReportGroupResponse
--
--         , responseUpdateWebhook $
--             newUpdateWebhookResponse
--
--           ]
--     ]

-- Requests

requestBatchDeleteBuilds :: BatchDeleteBuilds -> TestTree
requestBatchDeleteBuilds =
  req
    "BatchDeleteBuilds"
    "fixture/BatchDeleteBuilds.yaml"

requestBatchGetBuildBatches :: BatchGetBuildBatches -> TestTree
requestBatchGetBuildBatches =
  req
    "BatchGetBuildBatches"
    "fixture/BatchGetBuildBatches.yaml"

requestBatchGetBuilds :: BatchGetBuilds -> TestTree
requestBatchGetBuilds =
  req
    "BatchGetBuilds"
    "fixture/BatchGetBuilds.yaml"

requestBatchGetProjects :: BatchGetProjects -> TestTree
requestBatchGetProjects =
  req
    "BatchGetProjects"
    "fixture/BatchGetProjects.yaml"

requestBatchGetReportGroups :: BatchGetReportGroups -> TestTree
requestBatchGetReportGroups =
  req
    "BatchGetReportGroups"
    "fixture/BatchGetReportGroups.yaml"

requestBatchGetReports :: BatchGetReports -> TestTree
requestBatchGetReports =
  req
    "BatchGetReports"
    "fixture/BatchGetReports.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestCreateReportGroup :: CreateReportGroup -> TestTree
requestCreateReportGroup =
  req
    "CreateReportGroup"
    "fixture/CreateReportGroup.yaml"

requestCreateWebhook :: CreateWebhook -> TestTree
requestCreateWebhook =
  req
    "CreateWebhook"
    "fixture/CreateWebhook.yaml"

requestDeleteBuildBatch :: DeleteBuildBatch -> TestTree
requestDeleteBuildBatch =
  req
    "DeleteBuildBatch"
    "fixture/DeleteBuildBatch.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestDeleteReport :: DeleteReport -> TestTree
requestDeleteReport =
  req
    "DeleteReport"
    "fixture/DeleteReport.yaml"

requestDeleteReportGroup :: DeleteReportGroup -> TestTree
requestDeleteReportGroup =
  req
    "DeleteReportGroup"
    "fixture/DeleteReportGroup.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestDeleteSourceCredentials :: DeleteSourceCredentials -> TestTree
requestDeleteSourceCredentials =
  req
    "DeleteSourceCredentials"
    "fixture/DeleteSourceCredentials.yaml"

requestDeleteWebhook :: DeleteWebhook -> TestTree
requestDeleteWebhook =
  req
    "DeleteWebhook"
    "fixture/DeleteWebhook.yaml"

requestDescribeCodeCoverages :: DescribeCodeCoverages -> TestTree
requestDescribeCodeCoverages =
  req
    "DescribeCodeCoverages"
    "fixture/DescribeCodeCoverages.yaml"

requestDescribeTestCases :: DescribeTestCases -> TestTree
requestDescribeTestCases =
  req
    "DescribeTestCases"
    "fixture/DescribeTestCases.yaml"

requestGetReportGroupTrend :: GetReportGroupTrend -> TestTree
requestGetReportGroupTrend =
  req
    "GetReportGroupTrend"
    "fixture/GetReportGroupTrend.yaml"

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy =
  req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

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

requestListBuildBatches :: ListBuildBatches -> TestTree
requestListBuildBatches =
  req
    "ListBuildBatches"
    "fixture/ListBuildBatches.yaml"

requestListBuildBatchesForProject :: ListBuildBatchesForProject -> TestTree
requestListBuildBatchesForProject =
  req
    "ListBuildBatchesForProject"
    "fixture/ListBuildBatchesForProject.yaml"

requestListBuilds :: ListBuilds -> TestTree
requestListBuilds =
  req
    "ListBuilds"
    "fixture/ListBuilds.yaml"

requestListBuildsForProject :: ListBuildsForProject -> TestTree
requestListBuildsForProject =
  req
    "ListBuildsForProject"
    "fixture/ListBuildsForProject.yaml"

requestListCuratedEnvironmentImages :: ListCuratedEnvironmentImages -> TestTree
requestListCuratedEnvironmentImages =
  req
    "ListCuratedEnvironmentImages"
    "fixture/ListCuratedEnvironmentImages.yaml"

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestListReportGroups :: ListReportGroups -> TestTree
requestListReportGroups =
  req
    "ListReportGroups"
    "fixture/ListReportGroups.yaml"

requestListReports :: ListReports -> TestTree
requestListReports =
  req
    "ListReports"
    "fixture/ListReports.yaml"

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

requestListSharedReportGroups :: ListSharedReportGroups -> TestTree
requestListSharedReportGroups =
  req
    "ListSharedReportGroups"
    "fixture/ListSharedReportGroups.yaml"

requestListSourceCredentials :: ListSourceCredentials -> TestTree
requestListSourceCredentials =
  req
    "ListSourceCredentials"
    "fixture/ListSourceCredentials.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestRetryBuild :: RetryBuild -> TestTree
requestRetryBuild =
  req
    "RetryBuild"
    "fixture/RetryBuild.yaml"

requestRetryBuildBatch :: RetryBuildBatch -> TestTree
requestRetryBuildBatch =
  req
    "RetryBuildBatch"
    "fixture/RetryBuildBatch.yaml"

requestStartBuild :: StartBuild -> TestTree
requestStartBuild =
  req
    "StartBuild"
    "fixture/StartBuild.yaml"

requestStartBuildBatch :: StartBuildBatch -> TestTree
requestStartBuildBatch =
  req
    "StartBuildBatch"
    "fixture/StartBuildBatch.yaml"

requestStopBuild :: StopBuild -> TestTree
requestStopBuild =
  req
    "StopBuild"
    "fixture/StopBuild.yaml"

requestStopBuildBatch :: StopBuildBatch -> TestTree
requestStopBuildBatch =
  req
    "StopBuildBatch"
    "fixture/StopBuildBatch.yaml"

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject =
  req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

requestUpdateProjectVisibility :: UpdateProjectVisibility -> TestTree
requestUpdateProjectVisibility =
  req
    "UpdateProjectVisibility"
    "fixture/UpdateProjectVisibility.yaml"

requestUpdateReportGroup :: UpdateReportGroup -> TestTree
requestUpdateReportGroup =
  req
    "UpdateReportGroup"
    "fixture/UpdateReportGroup.yaml"

requestUpdateWebhook :: UpdateWebhook -> TestTree
requestUpdateWebhook =
  req
    "UpdateWebhook"
    "fixture/UpdateWebhook.yaml"

-- Responses

responseBatchDeleteBuilds :: BatchDeleteBuildsResponse -> TestTree
responseBatchDeleteBuilds =
  res
    "BatchDeleteBuildsResponse"
    "fixture/BatchDeleteBuildsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteBuilds)

responseBatchGetBuildBatches :: BatchGetBuildBatchesResponse -> TestTree
responseBatchGetBuildBatches =
  res
    "BatchGetBuildBatchesResponse"
    "fixture/BatchGetBuildBatchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetBuildBatches)

responseBatchGetBuilds :: BatchGetBuildsResponse -> TestTree
responseBatchGetBuilds =
  res
    "BatchGetBuildsResponse"
    "fixture/BatchGetBuildsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetBuilds)

responseBatchGetProjects :: BatchGetProjectsResponse -> TestTree
responseBatchGetProjects =
  res
    "BatchGetProjectsResponse"
    "fixture/BatchGetProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetProjects)

responseBatchGetReportGroups :: BatchGetReportGroupsResponse -> TestTree
responseBatchGetReportGroups =
  res
    "BatchGetReportGroupsResponse"
    "fixture/BatchGetReportGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetReportGroups)

responseBatchGetReports :: BatchGetReportsResponse -> TestTree
responseBatchGetReports =
  res
    "BatchGetReportsResponse"
    "fixture/BatchGetReportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetReports)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)

responseCreateReportGroup :: CreateReportGroupResponse -> TestTree
responseCreateReportGroup =
  res
    "CreateReportGroupResponse"
    "fixture/CreateReportGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReportGroup)

responseCreateWebhook :: CreateWebhookResponse -> TestTree
responseCreateWebhook =
  res
    "CreateWebhookResponse"
    "fixture/CreateWebhookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWebhook)

responseDeleteBuildBatch :: DeleteBuildBatchResponse -> TestTree
responseDeleteBuildBatch =
  res
    "DeleteBuildBatchResponse"
    "fixture/DeleteBuildBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBuildBatch)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProject)

responseDeleteReport :: DeleteReportResponse -> TestTree
responseDeleteReport =
  res
    "DeleteReportResponse"
    "fixture/DeleteReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReport)

responseDeleteReportGroup :: DeleteReportGroupResponse -> TestTree
responseDeleteReportGroup =
  res
    "DeleteReportGroupResponse"
    "fixture/DeleteReportGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReportGroup)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseDeleteSourceCredentials :: DeleteSourceCredentialsResponse -> TestTree
responseDeleteSourceCredentials =
  res
    "DeleteSourceCredentialsResponse"
    "fixture/DeleteSourceCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSourceCredentials)

responseDeleteWebhook :: DeleteWebhookResponse -> TestTree
responseDeleteWebhook =
  res
    "DeleteWebhookResponse"
    "fixture/DeleteWebhookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWebhook)

responseDescribeCodeCoverages :: DescribeCodeCoveragesResponse -> TestTree
responseDescribeCodeCoverages =
  res
    "DescribeCodeCoveragesResponse"
    "fixture/DescribeCodeCoveragesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCodeCoverages)

responseDescribeTestCases :: DescribeTestCasesResponse -> TestTree
responseDescribeTestCases =
  res
    "DescribeTestCasesResponse"
    "fixture/DescribeTestCasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTestCases)

responseGetReportGroupTrend :: GetReportGroupTrendResponse -> TestTree
responseGetReportGroupTrend =
  res
    "GetReportGroupTrendResponse"
    "fixture/GetReportGroupTrendResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReportGroupTrend)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicy)

responseImportSourceCredentials :: ImportSourceCredentialsResponse -> TestTree
responseImportSourceCredentials =
  res
    "ImportSourceCredentialsResponse"
    "fixture/ImportSourceCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportSourceCredentials)

responseInvalidateProjectCache :: InvalidateProjectCacheResponse -> TestTree
responseInvalidateProjectCache =
  res
    "InvalidateProjectCacheResponse"
    "fixture/InvalidateProjectCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InvalidateProjectCache)

responseListBuildBatches :: ListBuildBatchesResponse -> TestTree
responseListBuildBatches =
  res
    "ListBuildBatchesResponse"
    "fixture/ListBuildBatchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBuildBatches)

responseListBuildBatchesForProject :: ListBuildBatchesForProjectResponse -> TestTree
responseListBuildBatchesForProject =
  res
    "ListBuildBatchesForProjectResponse"
    "fixture/ListBuildBatchesForProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBuildBatchesForProject)

responseListBuilds :: ListBuildsResponse -> TestTree
responseListBuilds =
  res
    "ListBuildsResponse"
    "fixture/ListBuildsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBuilds)

responseListBuildsForProject :: ListBuildsForProjectResponse -> TestTree
responseListBuildsForProject =
  res
    "ListBuildsForProjectResponse"
    "fixture/ListBuildsForProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBuildsForProject)

responseListCuratedEnvironmentImages :: ListCuratedEnvironmentImagesResponse -> TestTree
responseListCuratedEnvironmentImages =
  res
    "ListCuratedEnvironmentImagesResponse"
    "fixture/ListCuratedEnvironmentImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCuratedEnvironmentImages)

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjects)

responseListReportGroups :: ListReportGroupsResponse -> TestTree
responseListReportGroups =
  res
    "ListReportGroupsResponse"
    "fixture/ListReportGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReportGroups)

responseListReports :: ListReportsResponse -> TestTree
responseListReports =
  res
    "ListReportsResponse"
    "fixture/ListReportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReports)

responseListReportsForReportGroup :: ListReportsForReportGroupResponse -> TestTree
responseListReportsForReportGroup =
  res
    "ListReportsForReportGroupResponse"
    "fixture/ListReportsForReportGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReportsForReportGroup)

responseListSharedProjects :: ListSharedProjectsResponse -> TestTree
responseListSharedProjects =
  res
    "ListSharedProjectsResponse"
    "fixture/ListSharedProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSharedProjects)

responseListSharedReportGroups :: ListSharedReportGroupsResponse -> TestTree
responseListSharedReportGroups =
  res
    "ListSharedReportGroupsResponse"
    "fixture/ListSharedReportGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSharedReportGroups)

responseListSourceCredentials :: ListSourceCredentialsResponse -> TestTree
responseListSourceCredentials =
  res
    "ListSourceCredentialsResponse"
    "fixture/ListSourceCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSourceCredentials)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responseRetryBuild :: RetryBuildResponse -> TestTree
responseRetryBuild =
  res
    "RetryBuildResponse"
    "fixture/RetryBuildResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetryBuild)

responseRetryBuildBatch :: RetryBuildBatchResponse -> TestTree
responseRetryBuildBatch =
  res
    "RetryBuildBatchResponse"
    "fixture/RetryBuildBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetryBuildBatch)

responseStartBuild :: StartBuildResponse -> TestTree
responseStartBuild =
  res
    "StartBuildResponse"
    "fixture/StartBuildResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartBuild)

responseStartBuildBatch :: StartBuildBatchResponse -> TestTree
responseStartBuildBatch =
  res
    "StartBuildBatchResponse"
    "fixture/StartBuildBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartBuildBatch)

responseStopBuild :: StopBuildResponse -> TestTree
responseStopBuild =
  res
    "StopBuildResponse"
    "fixture/StopBuildResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopBuild)

responseStopBuildBatch :: StopBuildBatchResponse -> TestTree
responseStopBuildBatch =
  res
    "StopBuildBatchResponse"
    "fixture/StopBuildBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopBuildBatch)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProject)

responseUpdateProjectVisibility :: UpdateProjectVisibilityResponse -> TestTree
responseUpdateProjectVisibility =
  res
    "UpdateProjectVisibilityResponse"
    "fixture/UpdateProjectVisibilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProjectVisibility)

responseUpdateReportGroup :: UpdateReportGroupResponse -> TestTree
responseUpdateReportGroup =
  res
    "UpdateReportGroupResponse"
    "fixture/UpdateReportGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReportGroup)

responseUpdateWebhook :: UpdateWebhookResponse -> TestTree
responseUpdateWebhook =
  res
    "UpdateWebhookResponse"
    "fixture/UpdateWebhookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWebhook)
