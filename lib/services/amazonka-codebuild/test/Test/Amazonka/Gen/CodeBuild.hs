{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CodeBuild
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestListProjects $
--             newListProjects
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
--         , requestListBuilds $
--             newListBuilds
--
--         , requestListSourceCredentials $
--             newListSourceCredentials
--
--         , requestListReports $
--             newListReports
--
--         , requestDeleteReport $
--             newDeleteReport
--
--         , requestCreateWebhook $
--             newCreateWebhook
--
--         , requestStopBuildBatch $
--             newStopBuildBatch
--
--         , requestListSharedProjects $
--             newListSharedProjects
--
--         , requestCreateReportGroup $
--             newCreateReportGroup
--
--         , requestDescribeCodeCoverages $
--             newDescribeCodeCoverages
--
--         , requestImportSourceCredentials $
--             newImportSourceCredentials
--
--         , requestListBuildBatchesForProject $
--             newListBuildBatchesForProject
--
--         , requestBatchGetReportGroups $
--             newBatchGetReportGroups
--
--         , requestDeleteBuildBatch $
--             newDeleteBuildBatch
--
--         , requestStartBuild $
--             newStartBuild
--
--         , requestBatchGetBuildBatches $
--             newBatchGetBuildBatches
--
--         , requestRetryBuild $
--             newRetryBuild
--
--         , requestListBuildsForProject $
--             newListBuildsForProject
--
--         , requestDescribeTestCases $
--             newDescribeTestCases
--
--         , requestGetResourcePolicy $
--             newGetResourcePolicy
--
--         , requestBatchGetProjects $
--             newBatchGetProjects
--
--         , requestBatchGetBuilds $
--             newBatchGetBuilds
--
--         , requestBatchGetReports $
--             newBatchGetReports
--
--         , requestUpdateWebhook $
--             newUpdateWebhook
--
--         , requestDeleteWebhook $
--             newDeleteWebhook
--
--         , requestStartBuildBatch $
--             newStartBuildBatch
--
--         , requestRetryBuildBatch $
--             newRetryBuildBatch
--
--         , requestUpdateProjectVisibility $
--             newUpdateProjectVisibility
--
--         , requestListReportsForReportGroup $
--             newListReportsForReportGroup
--
--         , requestInvalidateProjectCache $
--             newInvalidateProjectCache
--
--         , requestUpdateReportGroup $
--             newUpdateReportGroup
--
--         , requestDeleteReportGroup $
--             newDeleteReportGroup
--
--         , requestBatchDeleteBuilds $
--             newBatchDeleteBuilds
--
--         , requestListReportGroups $
--             newListReportGroups
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestListCuratedEnvironmentImages $
--             newListCuratedEnvironmentImages
--
--         , requestGetReportGroupTrend $
--             newGetReportGroupTrend
--
--         , requestStopBuild $
--             newStopBuild
--
--         , requestListBuildBatches $
--             newListBuildBatches
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestListSharedReportGroups $
--             newListSharedReportGroups
--
--           ]

--     , testGroup "response"
--         [ responseListProjects $
--             newListProjectsResponse
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
--         , responseListBuilds $
--             newListBuildsResponse
--
--         , responseListSourceCredentials $
--             newListSourceCredentialsResponse
--
--         , responseListReports $
--             newListReportsResponse
--
--         , responseDeleteReport $
--             newDeleteReportResponse
--
--         , responseCreateWebhook $
--             newCreateWebhookResponse
--
--         , responseStopBuildBatch $
--             newStopBuildBatchResponse
--
--         , responseListSharedProjects $
--             newListSharedProjectsResponse
--
--         , responseCreateReportGroup $
--             newCreateReportGroupResponse
--
--         , responseDescribeCodeCoverages $
--             newDescribeCodeCoveragesResponse
--
--         , responseImportSourceCredentials $
--             newImportSourceCredentialsResponse
--
--         , responseListBuildBatchesForProject $
--             newListBuildBatchesForProjectResponse
--
--         , responseBatchGetReportGroups $
--             newBatchGetReportGroupsResponse
--
--         , responseDeleteBuildBatch $
--             newDeleteBuildBatchResponse
--
--         , responseStartBuild $
--             newStartBuildResponse
--
--         , responseBatchGetBuildBatches $
--             newBatchGetBuildBatchesResponse
--
--         , responseRetryBuild $
--             newRetryBuildResponse
--
--         , responseListBuildsForProject $
--             newListBuildsForProjectResponse
--
--         , responseDescribeTestCases $
--             newDescribeTestCasesResponse
--
--         , responseGetResourcePolicy $
--             newGetResourcePolicyResponse
--
--         , responseBatchGetProjects $
--             newBatchGetProjectsResponse
--
--         , responseBatchGetBuilds $
--             newBatchGetBuildsResponse
--
--         , responseBatchGetReports $
--             newBatchGetReportsResponse
--
--         , responseUpdateWebhook $
--             newUpdateWebhookResponse
--
--         , responseDeleteWebhook $
--             newDeleteWebhookResponse
--
--         , responseStartBuildBatch $
--             newStartBuildBatchResponse
--
--         , responseRetryBuildBatch $
--             newRetryBuildBatchResponse
--
--         , responseUpdateProjectVisibility $
--             newUpdateProjectVisibilityResponse
--
--         , responseListReportsForReportGroup $
--             newListReportsForReportGroupResponse
--
--         , responseInvalidateProjectCache $
--             newInvalidateProjectCacheResponse
--
--         , responseUpdateReportGroup $
--             newUpdateReportGroupResponse
--
--         , responseDeleteReportGroup $
--             newDeleteReportGroupResponse
--
--         , responseBatchDeleteBuilds $
--             newBatchDeleteBuildsResponse
--
--         , responseListReportGroups $
--             newListReportGroupsResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseListCuratedEnvironmentImages $
--             newListCuratedEnvironmentImagesResponse
--
--         , responseGetReportGroupTrend $
--             newGetReportGroupTrendResponse
--
--         , responseStopBuild $
--             newStopBuildResponse
--
--         , responseListBuildBatches $
--             newListBuildBatchesResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseListSharedReportGroups $
--             newListSharedReportGroupsResponse
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

requestUpdateProjectVisibility :: UpdateProjectVisibility -> TestTree
requestUpdateProjectVisibility =
  req
    "UpdateProjectVisibility"
    "fixture/UpdateProjectVisibility.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjects)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProject)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProject)

responseDeleteSourceCredentials :: DeleteSourceCredentialsResponse -> TestTree
responseDeleteSourceCredentials =
  res
    "DeleteSourceCredentialsResponse"
    "fixture/DeleteSourceCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSourceCredentials)

responseListBuilds :: ListBuildsResponse -> TestTree
responseListBuilds =
  res
    "ListBuildsResponse"
    "fixture/ListBuildsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBuilds)

responseListSourceCredentials :: ListSourceCredentialsResponse -> TestTree
responseListSourceCredentials =
  res
    "ListSourceCredentialsResponse"
    "fixture/ListSourceCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSourceCredentials)

responseListReports :: ListReportsResponse -> TestTree
responseListReports =
  res
    "ListReportsResponse"
    "fixture/ListReportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReports)

responseDeleteReport :: DeleteReportResponse -> TestTree
responseDeleteReport =
  res
    "DeleteReportResponse"
    "fixture/DeleteReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReport)

responseCreateWebhook :: CreateWebhookResponse -> TestTree
responseCreateWebhook =
  res
    "CreateWebhookResponse"
    "fixture/CreateWebhookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWebhook)

responseStopBuildBatch :: StopBuildBatchResponse -> TestTree
responseStopBuildBatch =
  res
    "StopBuildBatchResponse"
    "fixture/StopBuildBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopBuildBatch)

responseListSharedProjects :: ListSharedProjectsResponse -> TestTree
responseListSharedProjects =
  res
    "ListSharedProjectsResponse"
    "fixture/ListSharedProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSharedProjects)

responseCreateReportGroup :: CreateReportGroupResponse -> TestTree
responseCreateReportGroup =
  res
    "CreateReportGroupResponse"
    "fixture/CreateReportGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReportGroup)

responseDescribeCodeCoverages :: DescribeCodeCoveragesResponse -> TestTree
responseDescribeCodeCoverages =
  res
    "DescribeCodeCoveragesResponse"
    "fixture/DescribeCodeCoveragesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCodeCoverages)

responseImportSourceCredentials :: ImportSourceCredentialsResponse -> TestTree
responseImportSourceCredentials =
  res
    "ImportSourceCredentialsResponse"
    "fixture/ImportSourceCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportSourceCredentials)

responseListBuildBatchesForProject :: ListBuildBatchesForProjectResponse -> TestTree
responseListBuildBatchesForProject =
  res
    "ListBuildBatchesForProjectResponse"
    "fixture/ListBuildBatchesForProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBuildBatchesForProject)

responseBatchGetReportGroups :: BatchGetReportGroupsResponse -> TestTree
responseBatchGetReportGroups =
  res
    "BatchGetReportGroupsResponse"
    "fixture/BatchGetReportGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetReportGroups)

responseDeleteBuildBatch :: DeleteBuildBatchResponse -> TestTree
responseDeleteBuildBatch =
  res
    "DeleteBuildBatchResponse"
    "fixture/DeleteBuildBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBuildBatch)

responseStartBuild :: StartBuildResponse -> TestTree
responseStartBuild =
  res
    "StartBuildResponse"
    "fixture/StartBuildResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartBuild)

responseBatchGetBuildBatches :: BatchGetBuildBatchesResponse -> TestTree
responseBatchGetBuildBatches =
  res
    "BatchGetBuildBatchesResponse"
    "fixture/BatchGetBuildBatchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetBuildBatches)

responseRetryBuild :: RetryBuildResponse -> TestTree
responseRetryBuild =
  res
    "RetryBuildResponse"
    "fixture/RetryBuildResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetryBuild)

responseListBuildsForProject :: ListBuildsForProjectResponse -> TestTree
responseListBuildsForProject =
  res
    "ListBuildsForProjectResponse"
    "fixture/ListBuildsForProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBuildsForProject)

responseDescribeTestCases :: DescribeTestCasesResponse -> TestTree
responseDescribeTestCases =
  res
    "DescribeTestCasesResponse"
    "fixture/DescribeTestCasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTestCases)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicy)

responseBatchGetProjects :: BatchGetProjectsResponse -> TestTree
responseBatchGetProjects =
  res
    "BatchGetProjectsResponse"
    "fixture/BatchGetProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetProjects)

responseBatchGetBuilds :: BatchGetBuildsResponse -> TestTree
responseBatchGetBuilds =
  res
    "BatchGetBuildsResponse"
    "fixture/BatchGetBuildsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetBuilds)

responseBatchGetReports :: BatchGetReportsResponse -> TestTree
responseBatchGetReports =
  res
    "BatchGetReportsResponse"
    "fixture/BatchGetReportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetReports)

responseUpdateWebhook :: UpdateWebhookResponse -> TestTree
responseUpdateWebhook =
  res
    "UpdateWebhookResponse"
    "fixture/UpdateWebhookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWebhook)

responseDeleteWebhook :: DeleteWebhookResponse -> TestTree
responseDeleteWebhook =
  res
    "DeleteWebhookResponse"
    "fixture/DeleteWebhookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWebhook)

responseStartBuildBatch :: StartBuildBatchResponse -> TestTree
responseStartBuildBatch =
  res
    "StartBuildBatchResponse"
    "fixture/StartBuildBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartBuildBatch)

responseRetryBuildBatch :: RetryBuildBatchResponse -> TestTree
responseRetryBuildBatch =
  res
    "RetryBuildBatchResponse"
    "fixture/RetryBuildBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetryBuildBatch)

responseUpdateProjectVisibility :: UpdateProjectVisibilityResponse -> TestTree
responseUpdateProjectVisibility =
  res
    "UpdateProjectVisibilityResponse"
    "fixture/UpdateProjectVisibilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProjectVisibility)

responseListReportsForReportGroup :: ListReportsForReportGroupResponse -> TestTree
responseListReportsForReportGroup =
  res
    "ListReportsForReportGroupResponse"
    "fixture/ListReportsForReportGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReportsForReportGroup)

responseInvalidateProjectCache :: InvalidateProjectCacheResponse -> TestTree
responseInvalidateProjectCache =
  res
    "InvalidateProjectCacheResponse"
    "fixture/InvalidateProjectCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InvalidateProjectCache)

responseUpdateReportGroup :: UpdateReportGroupResponse -> TestTree
responseUpdateReportGroup =
  res
    "UpdateReportGroupResponse"
    "fixture/UpdateReportGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReportGroup)

responseDeleteReportGroup :: DeleteReportGroupResponse -> TestTree
responseDeleteReportGroup =
  res
    "DeleteReportGroupResponse"
    "fixture/DeleteReportGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReportGroup)

responseBatchDeleteBuilds :: BatchDeleteBuildsResponse -> TestTree
responseBatchDeleteBuilds =
  res
    "BatchDeleteBuildsResponse"
    "fixture/BatchDeleteBuildsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteBuilds)

responseListReportGroups :: ListReportGroupsResponse -> TestTree
responseListReportGroups =
  res
    "ListReportGroupsResponse"
    "fixture/ListReportGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReportGroups)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseListCuratedEnvironmentImages :: ListCuratedEnvironmentImagesResponse -> TestTree
responseListCuratedEnvironmentImages =
  res
    "ListCuratedEnvironmentImagesResponse"
    "fixture/ListCuratedEnvironmentImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCuratedEnvironmentImages)

responseGetReportGroupTrend :: GetReportGroupTrendResponse -> TestTree
responseGetReportGroupTrend =
  res
    "GetReportGroupTrendResponse"
    "fixture/GetReportGroupTrendResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReportGroupTrend)

responseStopBuild :: StopBuildResponse -> TestTree
responseStopBuild =
  res
    "StopBuildResponse"
    "fixture/StopBuildResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopBuild)

responseListBuildBatches :: ListBuildBatchesResponse -> TestTree
responseListBuildBatches =
  res
    "ListBuildBatchesResponse"
    "fixture/ListBuildBatchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBuildBatches)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)

responseListSharedReportGroups :: ListSharedReportGroupsResponse -> TestTree
responseListSharedReportGroups =
  res
    "ListSharedReportGroupsResponse"
    "fixture/ListSharedReportGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSharedReportGroups)
