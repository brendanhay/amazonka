{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeBuild
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         , requestListBuilds $
--             listBuilds
--
--         , requestCreateWebhook $
--             createWebhook
--
--         , requestStartBuild $
--             startBuild
--
--         , requestListBuildsForProject $
--             listBuildsForProject
--
--         , requestBatchGetProjects $
--             batchGetProjects
--
--         , requestBatchGetBuilds $
--             batchGetBuilds
--
--         , requestUpdateWebhook $
--             updateWebhook
--
--         , requestDeleteWebhook $
--             deleteWebhook
--
--         , requestInvalidateProjectCache $
--             invalidateProjectCache
--
--         , requestBatchDeleteBuilds $
--             batchDeleteBuilds
--
--         , requestListCuratedEnvironmentImages $
--             listCuratedEnvironmentImages
--
--         , requestStopBuild $
--             stopBuild
--
--         , requestCreateProject $
--             createProject
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
--         , responseListBuilds $
--             listBuildsResponse
--
--         , responseCreateWebhook $
--             createWebhookResponse
--
--         , responseStartBuild $
--             startBuildResponse
--
--         , responseListBuildsForProject $
--             listBuildsForProjectResponse
--
--         , responseBatchGetProjects $
--             batchGetProjectsResponse
--
--         , responseBatchGetBuilds $
--             batchGetBuildsResponse
--
--         , responseUpdateWebhook $
--             updateWebhookResponse
--
--         , responseDeleteWebhook $
--             deleteWebhookResponse
--
--         , responseInvalidateProjectCache $
--             invalidateProjectCacheResponse
--
--         , responseBatchDeleteBuilds $
--             batchDeleteBuildsResponse
--
--         , responseListCuratedEnvironmentImages $
--             listCuratedEnvironmentImagesResponse
--
--         , responseStopBuild $
--             stopBuildResponse
--
--         , responseCreateProject $
--             createProjectResponse
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

requestListBuilds :: ListBuilds -> TestTree
requestListBuilds = req
    "ListBuilds"
    "fixture/ListBuilds.yaml"

requestCreateWebhook :: CreateWebhook -> TestTree
requestCreateWebhook = req
    "CreateWebhook"
    "fixture/CreateWebhook.yaml"

requestStartBuild :: StartBuild -> TestTree
requestStartBuild = req
    "StartBuild"
    "fixture/StartBuild.yaml"

requestListBuildsForProject :: ListBuildsForProject -> TestTree
requestListBuildsForProject = req
    "ListBuildsForProject"
    "fixture/ListBuildsForProject.yaml"

requestBatchGetProjects :: BatchGetProjects -> TestTree
requestBatchGetProjects = req
    "BatchGetProjects"
    "fixture/BatchGetProjects.yaml"

requestBatchGetBuilds :: BatchGetBuilds -> TestTree
requestBatchGetBuilds = req
    "BatchGetBuilds"
    "fixture/BatchGetBuilds.yaml"

requestUpdateWebhook :: UpdateWebhook -> TestTree
requestUpdateWebhook = req
    "UpdateWebhook"
    "fixture/UpdateWebhook.yaml"

requestDeleteWebhook :: DeleteWebhook -> TestTree
requestDeleteWebhook = req
    "DeleteWebhook"
    "fixture/DeleteWebhook.yaml"

requestInvalidateProjectCache :: InvalidateProjectCache -> TestTree
requestInvalidateProjectCache = req
    "InvalidateProjectCache"
    "fixture/InvalidateProjectCache.yaml"

requestBatchDeleteBuilds :: BatchDeleteBuilds -> TestTree
requestBatchDeleteBuilds = req
    "BatchDeleteBuilds"
    "fixture/BatchDeleteBuilds.yaml"

requestListCuratedEnvironmentImages :: ListCuratedEnvironmentImages -> TestTree
requestListCuratedEnvironmentImages = req
    "ListCuratedEnvironmentImages"
    "fixture/ListCuratedEnvironmentImages.yaml"

requestStopBuild :: StopBuild -> TestTree
requestStopBuild = req
    "StopBuild"
    "fixture/StopBuild.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject = req
    "CreateProject"
    "fixture/CreateProject.yaml"

-- Responses

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects = res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    codeBuild
    (Proxy :: Proxy ListProjects)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject = res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    codeBuild
    (Proxy :: Proxy DeleteProject)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject = res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    codeBuild
    (Proxy :: Proxy UpdateProject)

responseListBuilds :: ListBuildsResponse -> TestTree
responseListBuilds = res
    "ListBuildsResponse"
    "fixture/ListBuildsResponse.proto"
    codeBuild
    (Proxy :: Proxy ListBuilds)

responseCreateWebhook :: CreateWebhookResponse -> TestTree
responseCreateWebhook = res
    "CreateWebhookResponse"
    "fixture/CreateWebhookResponse.proto"
    codeBuild
    (Proxy :: Proxy CreateWebhook)

responseStartBuild :: StartBuildResponse -> TestTree
responseStartBuild = res
    "StartBuildResponse"
    "fixture/StartBuildResponse.proto"
    codeBuild
    (Proxy :: Proxy StartBuild)

responseListBuildsForProject :: ListBuildsForProjectResponse -> TestTree
responseListBuildsForProject = res
    "ListBuildsForProjectResponse"
    "fixture/ListBuildsForProjectResponse.proto"
    codeBuild
    (Proxy :: Proxy ListBuildsForProject)

responseBatchGetProjects :: BatchGetProjectsResponse -> TestTree
responseBatchGetProjects = res
    "BatchGetProjectsResponse"
    "fixture/BatchGetProjectsResponse.proto"
    codeBuild
    (Proxy :: Proxy BatchGetProjects)

responseBatchGetBuilds :: BatchGetBuildsResponse -> TestTree
responseBatchGetBuilds = res
    "BatchGetBuildsResponse"
    "fixture/BatchGetBuildsResponse.proto"
    codeBuild
    (Proxy :: Proxy BatchGetBuilds)

responseUpdateWebhook :: UpdateWebhookResponse -> TestTree
responseUpdateWebhook = res
    "UpdateWebhookResponse"
    "fixture/UpdateWebhookResponse.proto"
    codeBuild
    (Proxy :: Proxy UpdateWebhook)

responseDeleteWebhook :: DeleteWebhookResponse -> TestTree
responseDeleteWebhook = res
    "DeleteWebhookResponse"
    "fixture/DeleteWebhookResponse.proto"
    codeBuild
    (Proxy :: Proxy DeleteWebhook)

responseInvalidateProjectCache :: InvalidateProjectCacheResponse -> TestTree
responseInvalidateProjectCache = res
    "InvalidateProjectCacheResponse"
    "fixture/InvalidateProjectCacheResponse.proto"
    codeBuild
    (Proxy :: Proxy InvalidateProjectCache)

responseBatchDeleteBuilds :: BatchDeleteBuildsResponse -> TestTree
responseBatchDeleteBuilds = res
    "BatchDeleteBuildsResponse"
    "fixture/BatchDeleteBuildsResponse.proto"
    codeBuild
    (Proxy :: Proxy BatchDeleteBuilds)

responseListCuratedEnvironmentImages :: ListCuratedEnvironmentImagesResponse -> TestTree
responseListCuratedEnvironmentImages = res
    "ListCuratedEnvironmentImagesResponse"
    "fixture/ListCuratedEnvironmentImagesResponse.proto"
    codeBuild
    (Proxy :: Proxy ListCuratedEnvironmentImages)

responseStopBuild :: StopBuildResponse -> TestTree
responseStopBuild = res
    "StopBuildResponse"
    "fixture/StopBuildResponse.proto"
    codeBuild
    (Proxy :: Proxy StopBuild)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject = res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    codeBuild
    (Proxy :: Proxy CreateProject)
