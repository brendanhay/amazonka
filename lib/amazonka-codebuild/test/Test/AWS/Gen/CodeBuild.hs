{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CodeBuild where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CodeBuild
import Test.AWS.CodeBuild.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListProjects $
--             mkListProjects
--
--         , requestDeleteProject $
--             mkDeleteProject
--
--         , requestUpdateProject $
--             mkUpdateProject
--
--         , requestDeleteSourceCredentials $
--             mkDeleteSourceCredentials
--
--         , requestListBuilds $
--             mkListBuilds
--
--         , requestListSourceCredentials $
--             mkListSourceCredentials
--
--         , requestListReports $
--             mkListReports
--
--         , requestDeleteReport $
--             mkDeleteReport
--
--         , requestCreateWebhook $
--             mkCreateWebhook
--
--         , requestStopBuildBatch $
--             mkStopBuildBatch
--
--         , requestListSharedProjects $
--             mkListSharedProjects
--
--         , requestCreateReportGroup $
--             mkCreateReportGroup
--
--         , requestDescribeCodeCoverages $
--             mkDescribeCodeCoverages
--
--         , requestImportSourceCredentials $
--             mkImportSourceCredentials
--
--         , requestListBuildBatchesForProject $
--             mkListBuildBatchesForProject
--
--         , requestBatchGetReportGroups $
--             mkBatchGetReportGroups
--
--         , requestDeleteBuildBatch $
--             mkDeleteBuildBatch
--
--         , requestStartBuild $
--             mkStartBuild
--
--         , requestBatchGetBuildBatches $
--             mkBatchGetBuildBatches
--
--         , requestRetryBuild $
--             mkRetryBuild
--
--         , requestListBuildsForProject $
--             mkListBuildsForProject
--
--         , requestDescribeTestCases $
--             mkDescribeTestCases
--
--         , requestGetResourcePolicy $
--             mkGetResourcePolicy
--
--         , requestBatchGetProjects $
--             mkBatchGetProjects
--
--         , requestBatchGetBuilds $
--             mkBatchGetBuilds
--
--         , requestBatchGetReports $
--             mkBatchGetReports
--
--         , requestUpdateWebhook $
--             mkUpdateWebhook
--
--         , requestDeleteWebhook $
--             mkDeleteWebhook
--
--         , requestStartBuildBatch $
--             mkStartBuildBatch
--
--         , requestRetryBuildBatch $
--             mkRetryBuildBatch
--
--         , requestListReportsForReportGroup $
--             mkListReportsForReportGroup
--
--         , requestInvalidateProjectCache $
--             mkInvalidateProjectCache
--
--         , requestUpdateReportGroup $
--             mkUpdateReportGroup
--
--         , requestDeleteReportGroup $
--             mkDeleteReportGroup
--
--         , requestBatchDeleteBuilds $
--             mkBatchDeleteBuilds
--
--         , requestListReportGroups $
--             mkListReportGroups
--
--         , requestPutResourcePolicy $
--             mkPutResourcePolicy
--
--         , requestDeleteResourcePolicy $
--             mkDeleteResourcePolicy
--
--         , requestListCuratedEnvironmentImages $
--             mkListCuratedEnvironmentImages
--
--         , requestGetReportGroupTrend $
--             mkGetReportGroupTrend
--
--         , requestStopBuild $
--             mkStopBuild
--
--         , requestListBuildBatches $
--             mkListBuildBatches
--
--         , requestCreateProject $
--             mkCreateProject
--
--         , requestListSharedReportGroups $
--             mkListSharedReportGroups
--
--           ]

--     , testGroup "response"
--         [ responseListProjects $
--             mkListProjectsResponse
--
--         , responseDeleteProject $
--             mkDeleteProjectResponse
--
--         , responseUpdateProject $
--             mkUpdateProjectResponse
--
--         , responseDeleteSourceCredentials $
--             mkDeleteSourceCredentialsResponse
--
--         , responseListBuilds $
--             mkListBuildsResponse
--
--         , responseListSourceCredentials $
--             mkListSourceCredentialsResponse
--
--         , responseListReports $
--             mkListReportsResponse
--
--         , responseDeleteReport $
--             mkDeleteReportResponse
--
--         , responseCreateWebhook $
--             mkCreateWebhookResponse
--
--         , responseStopBuildBatch $
--             mkStopBuildBatchResponse
--
--         , responseListSharedProjects $
--             mkListSharedProjectsResponse
--
--         , responseCreateReportGroup $
--             mkCreateReportGroupResponse
--
--         , responseDescribeCodeCoverages $
--             mkDescribeCodeCoveragesResponse
--
--         , responseImportSourceCredentials $
--             mkImportSourceCredentialsResponse
--
--         , responseListBuildBatchesForProject $
--             mkListBuildBatchesForProjectResponse
--
--         , responseBatchGetReportGroups $
--             mkBatchGetReportGroupsResponse
--
--         , responseDeleteBuildBatch $
--             mkDeleteBuildBatchResponse
--
--         , responseStartBuild $
--             mkStartBuildResponse
--
--         , responseBatchGetBuildBatches $
--             mkBatchGetBuildBatchesResponse
--
--         , responseRetryBuild $
--             mkRetryBuildResponse
--
--         , responseListBuildsForProject $
--             mkListBuildsForProjectResponse
--
--         , responseDescribeTestCases $
--             mkDescribeTestCasesResponse
--
--         , responseGetResourcePolicy $
--             mkGetResourcePolicyResponse
--
--         , responseBatchGetProjects $
--             mkBatchGetProjectsResponse
--
--         , responseBatchGetBuilds $
--             mkBatchGetBuildsResponse
--
--         , responseBatchGetReports $
--             mkBatchGetReportsResponse
--
--         , responseUpdateWebhook $
--             mkUpdateWebhookResponse
--
--         , responseDeleteWebhook $
--             mkDeleteWebhookResponse
--
--         , responseStartBuildBatch $
--             mkStartBuildBatchResponse
--
--         , responseRetryBuildBatch $
--             mkRetryBuildBatchResponse
--
--         , responseListReportsForReportGroup $
--             mkListReportsForReportGroupResponse
--
--         , responseInvalidateProjectCache $
--             mkInvalidateProjectCacheResponse
--
--         , responseUpdateReportGroup $
--             mkUpdateReportGroupResponse
--
--         , responseDeleteReportGroup $
--             mkDeleteReportGroupResponse
--
--         , responseBatchDeleteBuilds $
--             mkBatchDeleteBuildsResponse
--
--         , responseListReportGroups $
--             mkListReportGroupsResponse
--
--         , responsePutResourcePolicy $
--             mkPutResourcePolicyResponse
--
--         , responseDeleteResourcePolicy $
--             mkDeleteResourcePolicyResponse
--
--         , responseListCuratedEnvironmentImages $
--             mkListCuratedEnvironmentImagesResponse
--
--         , responseGetReportGroupTrend $
--             mkGetReportGroupTrendResponse
--
--         , responseStopBuild $
--             mkStopBuildResponse
--
--         , responseListBuildBatches $
--             mkListBuildBatchesResponse
--
--         , responseCreateProject $
--             mkCreateProjectResponse
--
--         , responseListSharedReportGroups $
--             mkListSharedReportGroupsResponse
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

requestDeleteSourceCredentials :: DeleteSourceCredentials -> TestTree
requestDeleteSourceCredentials = req
    "DeleteSourceCredentials"
    "fixture/DeleteSourceCredentials.yaml"

requestListBuilds :: ListBuilds -> TestTree
requestListBuilds = req
    "ListBuilds"
    "fixture/ListBuilds.yaml"

requestListSourceCredentials :: ListSourceCredentials -> TestTree
requestListSourceCredentials = req
    "ListSourceCredentials"
    "fixture/ListSourceCredentials.yaml"

requestListReports :: ListReports -> TestTree
requestListReports = req
    "ListReports"
    "fixture/ListReports.yaml"

requestDeleteReport :: DeleteReport -> TestTree
requestDeleteReport = req
    "DeleteReport"
    "fixture/DeleteReport.yaml"

requestCreateWebhook :: CreateWebhook -> TestTree
requestCreateWebhook = req
    "CreateWebhook"
    "fixture/CreateWebhook.yaml"

requestStopBuildBatch :: StopBuildBatch -> TestTree
requestStopBuildBatch = req
    "StopBuildBatch"
    "fixture/StopBuildBatch.yaml"

requestListSharedProjects :: ListSharedProjects -> TestTree
requestListSharedProjects = req
    "ListSharedProjects"
    "fixture/ListSharedProjects.yaml"

requestCreateReportGroup :: CreateReportGroup -> TestTree
requestCreateReportGroup = req
    "CreateReportGroup"
    "fixture/CreateReportGroup.yaml"

requestDescribeCodeCoverages :: DescribeCodeCoverages -> TestTree
requestDescribeCodeCoverages = req
    "DescribeCodeCoverages"
    "fixture/DescribeCodeCoverages.yaml"

requestImportSourceCredentials :: ImportSourceCredentials -> TestTree
requestImportSourceCredentials = req
    "ImportSourceCredentials"
    "fixture/ImportSourceCredentials.yaml"

requestListBuildBatchesForProject :: ListBuildBatchesForProject -> TestTree
requestListBuildBatchesForProject = req
    "ListBuildBatchesForProject"
    "fixture/ListBuildBatchesForProject.yaml"

requestBatchGetReportGroups :: BatchGetReportGroups -> TestTree
requestBatchGetReportGroups = req
    "BatchGetReportGroups"
    "fixture/BatchGetReportGroups.yaml"

requestDeleteBuildBatch :: DeleteBuildBatch -> TestTree
requestDeleteBuildBatch = req
    "DeleteBuildBatch"
    "fixture/DeleteBuildBatch.yaml"

requestStartBuild :: StartBuild -> TestTree
requestStartBuild = req
    "StartBuild"
    "fixture/StartBuild.yaml"

requestBatchGetBuildBatches :: BatchGetBuildBatches -> TestTree
requestBatchGetBuildBatches = req
    "BatchGetBuildBatches"
    "fixture/BatchGetBuildBatches.yaml"

requestRetryBuild :: RetryBuild -> TestTree
requestRetryBuild = req
    "RetryBuild"
    "fixture/RetryBuild.yaml"

requestListBuildsForProject :: ListBuildsForProject -> TestTree
requestListBuildsForProject = req
    "ListBuildsForProject"
    "fixture/ListBuildsForProject.yaml"

requestDescribeTestCases :: DescribeTestCases -> TestTree
requestDescribeTestCases = req
    "DescribeTestCases"
    "fixture/DescribeTestCases.yaml"

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy = req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

requestBatchGetProjects :: BatchGetProjects -> TestTree
requestBatchGetProjects = req
    "BatchGetProjects"
    "fixture/BatchGetProjects.yaml"

requestBatchGetBuilds :: BatchGetBuilds -> TestTree
requestBatchGetBuilds = req
    "BatchGetBuilds"
    "fixture/BatchGetBuilds.yaml"

requestBatchGetReports :: BatchGetReports -> TestTree
requestBatchGetReports = req
    "BatchGetReports"
    "fixture/BatchGetReports.yaml"

requestUpdateWebhook :: UpdateWebhook -> TestTree
requestUpdateWebhook = req
    "UpdateWebhook"
    "fixture/UpdateWebhook.yaml"

requestDeleteWebhook :: DeleteWebhook -> TestTree
requestDeleteWebhook = req
    "DeleteWebhook"
    "fixture/DeleteWebhook.yaml"

requestStartBuildBatch :: StartBuildBatch -> TestTree
requestStartBuildBatch = req
    "StartBuildBatch"
    "fixture/StartBuildBatch.yaml"

requestRetryBuildBatch :: RetryBuildBatch -> TestTree
requestRetryBuildBatch = req
    "RetryBuildBatch"
    "fixture/RetryBuildBatch.yaml"

requestListReportsForReportGroup :: ListReportsForReportGroup -> TestTree
requestListReportsForReportGroup = req
    "ListReportsForReportGroup"
    "fixture/ListReportsForReportGroup.yaml"

requestInvalidateProjectCache :: InvalidateProjectCache -> TestTree
requestInvalidateProjectCache = req
    "InvalidateProjectCache"
    "fixture/InvalidateProjectCache.yaml"

requestUpdateReportGroup :: UpdateReportGroup -> TestTree
requestUpdateReportGroup = req
    "UpdateReportGroup"
    "fixture/UpdateReportGroup.yaml"

requestDeleteReportGroup :: DeleteReportGroup -> TestTree
requestDeleteReportGroup = req
    "DeleteReportGroup"
    "fixture/DeleteReportGroup.yaml"

requestBatchDeleteBuilds :: BatchDeleteBuilds -> TestTree
requestBatchDeleteBuilds = req
    "BatchDeleteBuilds"
    "fixture/BatchDeleteBuilds.yaml"

requestListReportGroups :: ListReportGroups -> TestTree
requestListReportGroups = req
    "ListReportGroups"
    "fixture/ListReportGroups.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy = req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy = req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestListCuratedEnvironmentImages :: ListCuratedEnvironmentImages -> TestTree
requestListCuratedEnvironmentImages = req
    "ListCuratedEnvironmentImages"
    "fixture/ListCuratedEnvironmentImages.yaml"

requestGetReportGroupTrend :: GetReportGroupTrend -> TestTree
requestGetReportGroupTrend = req
    "GetReportGroupTrend"
    "fixture/GetReportGroupTrend.yaml"

requestStopBuild :: StopBuild -> TestTree
requestStopBuild = req
    "StopBuild"
    "fixture/StopBuild.yaml"

requestListBuildBatches :: ListBuildBatches -> TestTree
requestListBuildBatches = req
    "ListBuildBatches"
    "fixture/ListBuildBatches.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject = req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestListSharedReportGroups :: ListSharedReportGroups -> TestTree
requestListSharedReportGroups = req
    "ListSharedReportGroups"
    "fixture/ListSharedReportGroups.yaml"

-- Responses

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects = res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListProjects)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject = res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteProject)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject = res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateProject)

responseDeleteSourceCredentials :: DeleteSourceCredentialsResponse -> TestTree
responseDeleteSourceCredentials = res
    "DeleteSourceCredentialsResponse"
    "fixture/DeleteSourceCredentialsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSourceCredentials)

responseListBuilds :: ListBuildsResponse -> TestTree
responseListBuilds = res
    "ListBuildsResponse"
    "fixture/ListBuildsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBuilds)

responseListSourceCredentials :: ListSourceCredentialsResponse -> TestTree
responseListSourceCredentials = res
    "ListSourceCredentialsResponse"
    "fixture/ListSourceCredentialsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSourceCredentials)

responseListReports :: ListReportsResponse -> TestTree
responseListReports = res
    "ListReportsResponse"
    "fixture/ListReportsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListReports)

responseDeleteReport :: DeleteReportResponse -> TestTree
responseDeleteReport = res
    "DeleteReportResponse"
    "fixture/DeleteReportResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteReport)

responseCreateWebhook :: CreateWebhookResponse -> TestTree
responseCreateWebhook = res
    "CreateWebhookResponse"
    "fixture/CreateWebhookResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateWebhook)

responseStopBuildBatch :: StopBuildBatchResponse -> TestTree
responseStopBuildBatch = res
    "StopBuildBatchResponse"
    "fixture/StopBuildBatchResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopBuildBatch)

responseListSharedProjects :: ListSharedProjectsResponse -> TestTree
responseListSharedProjects = res
    "ListSharedProjectsResponse"
    "fixture/ListSharedProjectsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSharedProjects)

responseCreateReportGroup :: CreateReportGroupResponse -> TestTree
responseCreateReportGroup = res
    "CreateReportGroupResponse"
    "fixture/CreateReportGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateReportGroup)

responseDescribeCodeCoverages :: DescribeCodeCoveragesResponse -> TestTree
responseDescribeCodeCoverages = res
    "DescribeCodeCoveragesResponse"
    "fixture/DescribeCodeCoveragesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCodeCoverages)

responseImportSourceCredentials :: ImportSourceCredentialsResponse -> TestTree
responseImportSourceCredentials = res
    "ImportSourceCredentialsResponse"
    "fixture/ImportSourceCredentialsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ImportSourceCredentials)

responseListBuildBatchesForProject :: ListBuildBatchesForProjectResponse -> TestTree
responseListBuildBatchesForProject = res
    "ListBuildBatchesForProjectResponse"
    "fixture/ListBuildBatchesForProjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBuildBatchesForProject)

responseBatchGetReportGroups :: BatchGetReportGroupsResponse -> TestTree
responseBatchGetReportGroups = res
    "BatchGetReportGroupsResponse"
    "fixture/BatchGetReportGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchGetReportGroups)

responseDeleteBuildBatch :: DeleteBuildBatchResponse -> TestTree
responseDeleteBuildBatch = res
    "DeleteBuildBatchResponse"
    "fixture/DeleteBuildBatchResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBuildBatch)

responseStartBuild :: StartBuildResponse -> TestTree
responseStartBuild = res
    "StartBuildResponse"
    "fixture/StartBuildResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartBuild)

responseBatchGetBuildBatches :: BatchGetBuildBatchesResponse -> TestTree
responseBatchGetBuildBatches = res
    "BatchGetBuildBatchesResponse"
    "fixture/BatchGetBuildBatchesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchGetBuildBatches)

responseRetryBuild :: RetryBuildResponse -> TestTree
responseRetryBuild = res
    "RetryBuildResponse"
    "fixture/RetryBuildResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RetryBuild)

responseListBuildsForProject :: ListBuildsForProjectResponse -> TestTree
responseListBuildsForProject = res
    "ListBuildsForProjectResponse"
    "fixture/ListBuildsForProjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBuildsForProject)

responseDescribeTestCases :: DescribeTestCasesResponse -> TestTree
responseDescribeTestCases = res
    "DescribeTestCasesResponse"
    "fixture/DescribeTestCasesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTestCases)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy = res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetResourcePolicy)

responseBatchGetProjects :: BatchGetProjectsResponse -> TestTree
responseBatchGetProjects = res
    "BatchGetProjectsResponse"
    "fixture/BatchGetProjectsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchGetProjects)

responseBatchGetBuilds :: BatchGetBuildsResponse -> TestTree
responseBatchGetBuilds = res
    "BatchGetBuildsResponse"
    "fixture/BatchGetBuildsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchGetBuilds)

responseBatchGetReports :: BatchGetReportsResponse -> TestTree
responseBatchGetReports = res
    "BatchGetReportsResponse"
    "fixture/BatchGetReportsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchGetReports)

responseUpdateWebhook :: UpdateWebhookResponse -> TestTree
responseUpdateWebhook = res
    "UpdateWebhookResponse"
    "fixture/UpdateWebhookResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateWebhook)

responseDeleteWebhook :: DeleteWebhookResponse -> TestTree
responseDeleteWebhook = res
    "DeleteWebhookResponse"
    "fixture/DeleteWebhookResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteWebhook)

responseStartBuildBatch :: StartBuildBatchResponse -> TestTree
responseStartBuildBatch = res
    "StartBuildBatchResponse"
    "fixture/StartBuildBatchResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartBuildBatch)

responseRetryBuildBatch :: RetryBuildBatchResponse -> TestTree
responseRetryBuildBatch = res
    "RetryBuildBatchResponse"
    "fixture/RetryBuildBatchResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RetryBuildBatch)

responseListReportsForReportGroup :: ListReportsForReportGroupResponse -> TestTree
responseListReportsForReportGroup = res
    "ListReportsForReportGroupResponse"
    "fixture/ListReportsForReportGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListReportsForReportGroup)

responseInvalidateProjectCache :: InvalidateProjectCacheResponse -> TestTree
responseInvalidateProjectCache = res
    "InvalidateProjectCacheResponse"
    "fixture/InvalidateProjectCacheResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy InvalidateProjectCache)

responseUpdateReportGroup :: UpdateReportGroupResponse -> TestTree
responseUpdateReportGroup = res
    "UpdateReportGroupResponse"
    "fixture/UpdateReportGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateReportGroup)

responseDeleteReportGroup :: DeleteReportGroupResponse -> TestTree
responseDeleteReportGroup = res
    "DeleteReportGroupResponse"
    "fixture/DeleteReportGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteReportGroup)

responseBatchDeleteBuilds :: BatchDeleteBuildsResponse -> TestTree
responseBatchDeleteBuilds = res
    "BatchDeleteBuildsResponse"
    "fixture/BatchDeleteBuildsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchDeleteBuilds)

responseListReportGroups :: ListReportGroupsResponse -> TestTree
responseListReportGroups = res
    "ListReportGroupsResponse"
    "fixture/ListReportGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListReportGroups)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy = res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutResourcePolicy)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy = res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteResourcePolicy)

responseListCuratedEnvironmentImages :: ListCuratedEnvironmentImagesResponse -> TestTree
responseListCuratedEnvironmentImages = res
    "ListCuratedEnvironmentImagesResponse"
    "fixture/ListCuratedEnvironmentImagesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListCuratedEnvironmentImages)

responseGetReportGroupTrend :: GetReportGroupTrendResponse -> TestTree
responseGetReportGroupTrend = res
    "GetReportGroupTrendResponse"
    "fixture/GetReportGroupTrendResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetReportGroupTrend)

responseStopBuild :: StopBuildResponse -> TestTree
responseStopBuild = res
    "StopBuildResponse"
    "fixture/StopBuildResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopBuild)

responseListBuildBatches :: ListBuildBatchesResponse -> TestTree
responseListBuildBatches = res
    "ListBuildBatchesResponse"
    "fixture/ListBuildBatchesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBuildBatches)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject = res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateProject)

responseListSharedReportGroups :: ListSharedReportGroupsResponse -> TestTree
responseListSharedReportGroups = res
    "ListSharedReportGroupsResponse"
    "fixture/ListSharedReportGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSharedReportGroups)
