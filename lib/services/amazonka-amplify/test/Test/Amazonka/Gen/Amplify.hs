{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Amplify
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Amplify where

import Amazonka.Amplify
import qualified Data.Proxy as Proxy
import Test.Amazonka.Amplify.Internal
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
--         [ requestGetDomainAssociation $
--             newGetDomainAssociation
--
--         , requestListArtifacts $
--             newListArtifacts
--
--         , requestStopJob $
--             newStopJob
--
--         , requestGetBackendEnvironment $
--             newGetBackendEnvironment
--
--         , requestCreateWebhook $
--             newCreateWebhook
--
--         , requestGetBranch $
--             newGetBranch
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateDomainAssociation $
--             newCreateDomainAssociation
--
--         , requestGetWebhook $
--             newGetWebhook
--
--         , requestDeleteBranch $
--             newDeleteBranch
--
--         , requestUpdateBranch $
--             newUpdateBranch
--
--         , requestCreateBackendEnvironment $
--             newCreateBackendEnvironment
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestCreateBranch $
--             newCreateBranch
--
--         , requestGenerateAccessLogs $
--             newGenerateAccessLogs
--
--         , requestListApps $
--             newListApps
--
--         , requestListBranches $
--             newListBranches
--
--         , requestDeleteBackendEnvironment $
--             newDeleteBackendEnvironment
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestUpdateApp $
--             newUpdateApp
--
--         , requestGetArtifactUrl $
--             newGetArtifactUrl
--
--         , requestListJobs $
--             newListJobs
--
--         , requestDeleteJob $
--             newDeleteJob
--
--         , requestGetJob $
--             newGetJob
--
--         , requestStartJob $
--             newStartJob
--
--         , requestGetApp $
--             newGetApp
--
--         , requestUpdateWebhook $
--             newUpdateWebhook
--
--         , requestDeleteWebhook $
--             newDeleteWebhook
--
--         , requestListWebhooks $
--             newListWebhooks
--
--         , requestCreateApp $
--             newCreateApp
--
--         , requestDeleteDomainAssociation $
--             newDeleteDomainAssociation
--
--         , requestUpdateDomainAssociation $
--             newUpdateDomainAssociation
--
--         , requestListDomainAssociations $
--             newListDomainAssociations
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListBackendEnvironments $
--             newListBackendEnvironments
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestStartDeployment $
--             newStartDeployment
--
--           ]

--     , testGroup "response"
--         [ responseGetDomainAssociation $
--             newGetDomainAssociationResponse
--
--         , responseListArtifacts $
--             newListArtifactsResponse
--
--         , responseStopJob $
--             newStopJobResponse
--
--         , responseGetBackendEnvironment $
--             newGetBackendEnvironmentResponse
--
--         , responseCreateWebhook $
--             newCreateWebhookResponse
--
--         , responseGetBranch $
--             newGetBranchResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateDomainAssociation $
--             newCreateDomainAssociationResponse
--
--         , responseGetWebhook $
--             newGetWebhookResponse
--
--         , responseDeleteBranch $
--             newDeleteBranchResponse
--
--         , responseUpdateBranch $
--             newUpdateBranchResponse
--
--         , responseCreateBackendEnvironment $
--             newCreateBackendEnvironmentResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseCreateBranch $
--             newCreateBranchResponse
--
--         , responseGenerateAccessLogs $
--             newGenerateAccessLogsResponse
--
--         , responseListApps $
--             newListAppsResponse
--
--         , responseListBranches $
--             newListBranchesResponse
--
--         , responseDeleteBackendEnvironment $
--             newDeleteBackendEnvironmentResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseUpdateApp $
--             newUpdateAppResponse
--
--         , responseGetArtifactUrl $
--             newGetArtifactUrlResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseDeleteJob $
--             newDeleteJobResponse
--
--         , responseGetJob $
--             newGetJobResponse
--
--         , responseStartJob $
--             newStartJobResponse
--
--         , responseGetApp $
--             newGetAppResponse
--
--         , responseUpdateWebhook $
--             newUpdateWebhookResponse
--
--         , responseDeleteWebhook $
--             newDeleteWebhookResponse
--
--         , responseListWebhooks $
--             newListWebhooksResponse
--
--         , responseCreateApp $
--             newCreateAppResponse
--
--         , responseDeleteDomainAssociation $
--             newDeleteDomainAssociationResponse
--
--         , responseUpdateDomainAssociation $
--             newUpdateDomainAssociationResponse
--
--         , responseListDomainAssociations $
--             newListDomainAssociationsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListBackendEnvironments $
--             newListBackendEnvironmentsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseStartDeployment $
--             newStartDeploymentResponse
--
--           ]
--     ]

-- Requests

requestGetDomainAssociation :: GetDomainAssociation -> TestTree
requestGetDomainAssociation =
  req
    "GetDomainAssociation"
    "fixture/GetDomainAssociation.yaml"

requestListArtifacts :: ListArtifacts -> TestTree
requestListArtifacts =
  req
    "ListArtifacts"
    "fixture/ListArtifacts.yaml"

requestStopJob :: StopJob -> TestTree
requestStopJob =
  req
    "StopJob"
    "fixture/StopJob.yaml"

requestGetBackendEnvironment :: GetBackendEnvironment -> TestTree
requestGetBackendEnvironment =
  req
    "GetBackendEnvironment"
    "fixture/GetBackendEnvironment.yaml"

requestCreateWebhook :: CreateWebhook -> TestTree
requestCreateWebhook =
  req
    "CreateWebhook"
    "fixture/CreateWebhook.yaml"

requestGetBranch :: GetBranch -> TestTree
requestGetBranch =
  req
    "GetBranch"
    "fixture/GetBranch.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateDomainAssociation :: CreateDomainAssociation -> TestTree
requestCreateDomainAssociation =
  req
    "CreateDomainAssociation"
    "fixture/CreateDomainAssociation.yaml"

requestGetWebhook :: GetWebhook -> TestTree
requestGetWebhook =
  req
    "GetWebhook"
    "fixture/GetWebhook.yaml"

requestDeleteBranch :: DeleteBranch -> TestTree
requestDeleteBranch =
  req
    "DeleteBranch"
    "fixture/DeleteBranch.yaml"

requestUpdateBranch :: UpdateBranch -> TestTree
requestUpdateBranch =
  req
    "UpdateBranch"
    "fixture/UpdateBranch.yaml"

requestCreateBackendEnvironment :: CreateBackendEnvironment -> TestTree
requestCreateBackendEnvironment =
  req
    "CreateBackendEnvironment"
    "fixture/CreateBackendEnvironment.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestCreateBranch :: CreateBranch -> TestTree
requestCreateBranch =
  req
    "CreateBranch"
    "fixture/CreateBranch.yaml"

requestGenerateAccessLogs :: GenerateAccessLogs -> TestTree
requestGenerateAccessLogs =
  req
    "GenerateAccessLogs"
    "fixture/GenerateAccessLogs.yaml"

requestListApps :: ListApps -> TestTree
requestListApps =
  req
    "ListApps"
    "fixture/ListApps.yaml"

requestListBranches :: ListBranches -> TestTree
requestListBranches =
  req
    "ListBranches"
    "fixture/ListBranches.yaml"

requestDeleteBackendEnvironment :: DeleteBackendEnvironment -> TestTree
requestDeleteBackendEnvironment =
  req
    "DeleteBackendEnvironment"
    "fixture/DeleteBackendEnvironment.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestUpdateApp :: UpdateApp -> TestTree
requestUpdateApp =
  req
    "UpdateApp"
    "fixture/UpdateApp.yaml"

requestGetArtifactUrl :: GetArtifactUrl -> TestTree
requestGetArtifactUrl =
  req
    "GetArtifactUrl"
    "fixture/GetArtifactUrl.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestDeleteJob :: DeleteJob -> TestTree
requestDeleteJob =
  req
    "DeleteJob"
    "fixture/DeleteJob.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob =
  req
    "GetJob"
    "fixture/GetJob.yaml"

requestStartJob :: StartJob -> TestTree
requestStartJob =
  req
    "StartJob"
    "fixture/StartJob.yaml"

requestGetApp :: GetApp -> TestTree
requestGetApp =
  req
    "GetApp"
    "fixture/GetApp.yaml"

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

requestListWebhooks :: ListWebhooks -> TestTree
requestListWebhooks =
  req
    "ListWebhooks"
    "fixture/ListWebhooks.yaml"

requestCreateApp :: CreateApp -> TestTree
requestCreateApp =
  req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestDeleteDomainAssociation :: DeleteDomainAssociation -> TestTree
requestDeleteDomainAssociation =
  req
    "DeleteDomainAssociation"
    "fixture/DeleteDomainAssociation.yaml"

requestUpdateDomainAssociation :: UpdateDomainAssociation -> TestTree
requestUpdateDomainAssociation =
  req
    "UpdateDomainAssociation"
    "fixture/UpdateDomainAssociation.yaml"

requestListDomainAssociations :: ListDomainAssociations -> TestTree
requestListDomainAssociations =
  req
    "ListDomainAssociations"
    "fixture/ListDomainAssociations.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListBackendEnvironments :: ListBackendEnvironments -> TestTree
requestListBackendEnvironments =
  req
    "ListBackendEnvironments"
    "fixture/ListBackendEnvironments.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestStartDeployment :: StartDeployment -> TestTree
requestStartDeployment =
  req
    "StartDeployment"
    "fixture/StartDeployment.yaml"

-- Responses

responseGetDomainAssociation :: GetDomainAssociationResponse -> TestTree
responseGetDomainAssociation =
  res
    "GetDomainAssociationResponse"
    "fixture/GetDomainAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomainAssociation)

responseListArtifacts :: ListArtifactsResponse -> TestTree
responseListArtifacts =
  res
    "ListArtifactsResponse"
    "fixture/ListArtifactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListArtifacts)

responseStopJob :: StopJobResponse -> TestTree
responseStopJob =
  res
    "StopJobResponse"
    "fixture/StopJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopJob)

responseGetBackendEnvironment :: GetBackendEnvironmentResponse -> TestTree
responseGetBackendEnvironment =
  res
    "GetBackendEnvironmentResponse"
    "fixture/GetBackendEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackendEnvironment)

responseCreateWebhook :: CreateWebhookResponse -> TestTree
responseCreateWebhook =
  res
    "CreateWebhookResponse"
    "fixture/CreateWebhookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWebhook)

responseGetBranch :: GetBranchResponse -> TestTree
responseGetBranch =
  res
    "GetBranchResponse"
    "fixture/GetBranchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBranch)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseCreateDomainAssociation :: CreateDomainAssociationResponse -> TestTree
responseCreateDomainAssociation =
  res
    "CreateDomainAssociationResponse"
    "fixture/CreateDomainAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomainAssociation)

responseGetWebhook :: GetWebhookResponse -> TestTree
responseGetWebhook =
  res
    "GetWebhookResponse"
    "fixture/GetWebhookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWebhook)

responseDeleteBranch :: DeleteBranchResponse -> TestTree
responseDeleteBranch =
  res
    "DeleteBranchResponse"
    "fixture/DeleteBranchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBranch)

responseUpdateBranch :: UpdateBranchResponse -> TestTree
responseUpdateBranch =
  res
    "UpdateBranchResponse"
    "fixture/UpdateBranchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBranch)

responseCreateBackendEnvironment :: CreateBackendEnvironmentResponse -> TestTree
responseCreateBackendEnvironment =
  res
    "CreateBackendEnvironmentResponse"
    "fixture/CreateBackendEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackendEnvironment)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeployment)

responseCreateBranch :: CreateBranchResponse -> TestTree
responseCreateBranch =
  res
    "CreateBranchResponse"
    "fixture/CreateBranchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBranch)

responseGenerateAccessLogs :: GenerateAccessLogsResponse -> TestTree
responseGenerateAccessLogs =
  res
    "GenerateAccessLogsResponse"
    "fixture/GenerateAccessLogsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateAccessLogs)

responseListApps :: ListAppsResponse -> TestTree
responseListApps =
  res
    "ListAppsResponse"
    "fixture/ListAppsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApps)

responseListBranches :: ListBranchesResponse -> TestTree
responseListBranches =
  res
    "ListBranchesResponse"
    "fixture/ListBranchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBranches)

responseDeleteBackendEnvironment :: DeleteBackendEnvironmentResponse -> TestTree
responseDeleteBackendEnvironment =
  res
    "DeleteBackendEnvironmentResponse"
    "fixture/DeleteBackendEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackendEnvironment)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApp)

responseUpdateApp :: UpdateAppResponse -> TestTree
responseUpdateApp =
  res
    "UpdateAppResponse"
    "fixture/UpdateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApp)

responseGetArtifactUrl :: GetArtifactUrlResponse -> TestTree
responseGetArtifactUrl =
  res
    "GetArtifactUrlResponse"
    "fixture/GetArtifactUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetArtifactUrl)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJob)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJob)

responseStartJob :: StartJobResponse -> TestTree
responseStartJob =
  res
    "StartJobResponse"
    "fixture/StartJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartJob)

responseGetApp :: GetAppResponse -> TestTree
responseGetApp =
  res
    "GetAppResponse"
    "fixture/GetAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApp)

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

responseListWebhooks :: ListWebhooksResponse -> TestTree
responseListWebhooks =
  res
    "ListWebhooksResponse"
    "fixture/ListWebhooksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWebhooks)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApp)

responseDeleteDomainAssociation :: DeleteDomainAssociationResponse -> TestTree
responseDeleteDomainAssociation =
  res
    "DeleteDomainAssociationResponse"
    "fixture/DeleteDomainAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomainAssociation)

responseUpdateDomainAssociation :: UpdateDomainAssociationResponse -> TestTree
responseUpdateDomainAssociation =
  res
    "UpdateDomainAssociationResponse"
    "fixture/UpdateDomainAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainAssociation)

responseListDomainAssociations :: ListDomainAssociationsResponse -> TestTree
responseListDomainAssociations =
  res
    "ListDomainAssociationsResponse"
    "fixture/ListDomainAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomainAssociations)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListBackendEnvironments :: ListBackendEnvironmentsResponse -> TestTree
responseListBackendEnvironments =
  res
    "ListBackendEnvironmentsResponse"
    "fixture/ListBackendEnvironmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackendEnvironments)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseStartDeployment :: StartDeploymentResponse -> TestTree
responseStartDeployment =
  res
    "StartDeploymentResponse"
    "fixture/StartDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDeployment)
