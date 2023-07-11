{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Amplify
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         [ requestCreateApp $
--             newCreateApp
--
--         , requestCreateBackendEnvironment $
--             newCreateBackendEnvironment
--
--         , requestCreateBranch $
--             newCreateBranch
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestCreateDomainAssociation $
--             newCreateDomainAssociation
--
--         , requestCreateWebhook $
--             newCreateWebhook
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestDeleteBackendEnvironment $
--             newDeleteBackendEnvironment
--
--         , requestDeleteBranch $
--             newDeleteBranch
--
--         , requestDeleteDomainAssociation $
--             newDeleteDomainAssociation
--
--         , requestDeleteJob $
--             newDeleteJob
--
--         , requestDeleteWebhook $
--             newDeleteWebhook
--
--         , requestGenerateAccessLogs $
--             newGenerateAccessLogs
--
--         , requestGetApp $
--             newGetApp
--
--         , requestGetArtifactUrl $
--             newGetArtifactUrl
--
--         , requestGetBackendEnvironment $
--             newGetBackendEnvironment
--
--         , requestGetBranch $
--             newGetBranch
--
--         , requestGetDomainAssociation $
--             newGetDomainAssociation
--
--         , requestGetJob $
--             newGetJob
--
--         , requestGetWebhook $
--             newGetWebhook
--
--         , requestListApps $
--             newListApps
--
--         , requestListArtifacts $
--             newListArtifacts
--
--         , requestListBackendEnvironments $
--             newListBackendEnvironments
--
--         , requestListBranches $
--             newListBranches
--
--         , requestListDomainAssociations $
--             newListDomainAssociations
--
--         , requestListJobs $
--             newListJobs
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListWebhooks $
--             newListWebhooks
--
--         , requestStartDeployment $
--             newStartDeployment
--
--         , requestStartJob $
--             newStartJob
--
--         , requestStopJob $
--             newStopJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateApp $
--             newUpdateApp
--
--         , requestUpdateBranch $
--             newUpdateBranch
--
--         , requestUpdateDomainAssociation $
--             newUpdateDomainAssociation
--
--         , requestUpdateWebhook $
--             newUpdateWebhook
--
--           ]

--     , testGroup "response"
--         [ responseCreateApp $
--             newCreateAppResponse
--
--         , responseCreateBackendEnvironment $
--             newCreateBackendEnvironmentResponse
--
--         , responseCreateBranch $
--             newCreateBranchResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseCreateDomainAssociation $
--             newCreateDomainAssociationResponse
--
--         , responseCreateWebhook $
--             newCreateWebhookResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseDeleteBackendEnvironment $
--             newDeleteBackendEnvironmentResponse
--
--         , responseDeleteBranch $
--             newDeleteBranchResponse
--
--         , responseDeleteDomainAssociation $
--             newDeleteDomainAssociationResponse
--
--         , responseDeleteJob $
--             newDeleteJobResponse
--
--         , responseDeleteWebhook $
--             newDeleteWebhookResponse
--
--         , responseGenerateAccessLogs $
--             newGenerateAccessLogsResponse
--
--         , responseGetApp $
--             newGetAppResponse
--
--         , responseGetArtifactUrl $
--             newGetArtifactUrlResponse
--
--         , responseGetBackendEnvironment $
--             newGetBackendEnvironmentResponse
--
--         , responseGetBranch $
--             newGetBranchResponse
--
--         , responseGetDomainAssociation $
--             newGetDomainAssociationResponse
--
--         , responseGetJob $
--             newGetJobResponse
--
--         , responseGetWebhook $
--             newGetWebhookResponse
--
--         , responseListApps $
--             newListAppsResponse
--
--         , responseListArtifacts $
--             newListArtifactsResponse
--
--         , responseListBackendEnvironments $
--             newListBackendEnvironmentsResponse
--
--         , responseListBranches $
--             newListBranchesResponse
--
--         , responseListDomainAssociations $
--             newListDomainAssociationsResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListWebhooks $
--             newListWebhooksResponse
--
--         , responseStartDeployment $
--             newStartDeploymentResponse
--
--         , responseStartJob $
--             newStartJobResponse
--
--         , responseStopJob $
--             newStopJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApp $
--             newUpdateAppResponse
--
--         , responseUpdateBranch $
--             newUpdateBranchResponse
--
--         , responseUpdateDomainAssociation $
--             newUpdateDomainAssociationResponse
--
--         , responseUpdateWebhook $
--             newUpdateWebhookResponse
--
--           ]
--     ]

-- Requests

requestCreateApp :: CreateApp -> TestTree
requestCreateApp =
  req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestCreateBackendEnvironment :: CreateBackendEnvironment -> TestTree
requestCreateBackendEnvironment =
  req
    "CreateBackendEnvironment"
    "fixture/CreateBackendEnvironment.yaml"

requestCreateBranch :: CreateBranch -> TestTree
requestCreateBranch =
  req
    "CreateBranch"
    "fixture/CreateBranch.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestCreateDomainAssociation :: CreateDomainAssociation -> TestTree
requestCreateDomainAssociation =
  req
    "CreateDomainAssociation"
    "fixture/CreateDomainAssociation.yaml"

requestCreateWebhook :: CreateWebhook -> TestTree
requestCreateWebhook =
  req
    "CreateWebhook"
    "fixture/CreateWebhook.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestDeleteBackendEnvironment :: DeleteBackendEnvironment -> TestTree
requestDeleteBackendEnvironment =
  req
    "DeleteBackendEnvironment"
    "fixture/DeleteBackendEnvironment.yaml"

requestDeleteBranch :: DeleteBranch -> TestTree
requestDeleteBranch =
  req
    "DeleteBranch"
    "fixture/DeleteBranch.yaml"

requestDeleteDomainAssociation :: DeleteDomainAssociation -> TestTree
requestDeleteDomainAssociation =
  req
    "DeleteDomainAssociation"
    "fixture/DeleteDomainAssociation.yaml"

requestDeleteJob :: DeleteJob -> TestTree
requestDeleteJob =
  req
    "DeleteJob"
    "fixture/DeleteJob.yaml"

requestDeleteWebhook :: DeleteWebhook -> TestTree
requestDeleteWebhook =
  req
    "DeleteWebhook"
    "fixture/DeleteWebhook.yaml"

requestGenerateAccessLogs :: GenerateAccessLogs -> TestTree
requestGenerateAccessLogs =
  req
    "GenerateAccessLogs"
    "fixture/GenerateAccessLogs.yaml"

requestGetApp :: GetApp -> TestTree
requestGetApp =
  req
    "GetApp"
    "fixture/GetApp.yaml"

requestGetArtifactUrl :: GetArtifactUrl -> TestTree
requestGetArtifactUrl =
  req
    "GetArtifactUrl"
    "fixture/GetArtifactUrl.yaml"

requestGetBackendEnvironment :: GetBackendEnvironment -> TestTree
requestGetBackendEnvironment =
  req
    "GetBackendEnvironment"
    "fixture/GetBackendEnvironment.yaml"

requestGetBranch :: GetBranch -> TestTree
requestGetBranch =
  req
    "GetBranch"
    "fixture/GetBranch.yaml"

requestGetDomainAssociation :: GetDomainAssociation -> TestTree
requestGetDomainAssociation =
  req
    "GetDomainAssociation"
    "fixture/GetDomainAssociation.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob =
  req
    "GetJob"
    "fixture/GetJob.yaml"

requestGetWebhook :: GetWebhook -> TestTree
requestGetWebhook =
  req
    "GetWebhook"
    "fixture/GetWebhook.yaml"

requestListApps :: ListApps -> TestTree
requestListApps =
  req
    "ListApps"
    "fixture/ListApps.yaml"

requestListArtifacts :: ListArtifacts -> TestTree
requestListArtifacts =
  req
    "ListArtifacts"
    "fixture/ListArtifacts.yaml"

requestListBackendEnvironments :: ListBackendEnvironments -> TestTree
requestListBackendEnvironments =
  req
    "ListBackendEnvironments"
    "fixture/ListBackendEnvironments.yaml"

requestListBranches :: ListBranches -> TestTree
requestListBranches =
  req
    "ListBranches"
    "fixture/ListBranches.yaml"

requestListDomainAssociations :: ListDomainAssociations -> TestTree
requestListDomainAssociations =
  req
    "ListDomainAssociations"
    "fixture/ListDomainAssociations.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListWebhooks :: ListWebhooks -> TestTree
requestListWebhooks =
  req
    "ListWebhooks"
    "fixture/ListWebhooks.yaml"

requestStartDeployment :: StartDeployment -> TestTree
requestStartDeployment =
  req
    "StartDeployment"
    "fixture/StartDeployment.yaml"

requestStartJob :: StartJob -> TestTree
requestStartJob =
  req
    "StartJob"
    "fixture/StartJob.yaml"

requestStopJob :: StopJob -> TestTree
requestStopJob =
  req
    "StopJob"
    "fixture/StopJob.yaml"

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

requestUpdateApp :: UpdateApp -> TestTree
requestUpdateApp =
  req
    "UpdateApp"
    "fixture/UpdateApp.yaml"

requestUpdateBranch :: UpdateBranch -> TestTree
requestUpdateBranch =
  req
    "UpdateBranch"
    "fixture/UpdateBranch.yaml"

requestUpdateDomainAssociation :: UpdateDomainAssociation -> TestTree
requestUpdateDomainAssociation =
  req
    "UpdateDomainAssociation"
    "fixture/UpdateDomainAssociation.yaml"

requestUpdateWebhook :: UpdateWebhook -> TestTree
requestUpdateWebhook =
  req
    "UpdateWebhook"
    "fixture/UpdateWebhook.yaml"

-- Responses

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApp)

responseCreateBackendEnvironment :: CreateBackendEnvironmentResponse -> TestTree
responseCreateBackendEnvironment =
  res
    "CreateBackendEnvironmentResponse"
    "fixture/CreateBackendEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackendEnvironment)

responseCreateBranch :: CreateBranchResponse -> TestTree
responseCreateBranch =
  res
    "CreateBranchResponse"
    "fixture/CreateBranchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBranch)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeployment)

responseCreateDomainAssociation :: CreateDomainAssociationResponse -> TestTree
responseCreateDomainAssociation =
  res
    "CreateDomainAssociationResponse"
    "fixture/CreateDomainAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomainAssociation)

responseCreateWebhook :: CreateWebhookResponse -> TestTree
responseCreateWebhook =
  res
    "CreateWebhookResponse"
    "fixture/CreateWebhookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWebhook)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApp)

responseDeleteBackendEnvironment :: DeleteBackendEnvironmentResponse -> TestTree
responseDeleteBackendEnvironment =
  res
    "DeleteBackendEnvironmentResponse"
    "fixture/DeleteBackendEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackendEnvironment)

responseDeleteBranch :: DeleteBranchResponse -> TestTree
responseDeleteBranch =
  res
    "DeleteBranchResponse"
    "fixture/DeleteBranchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBranch)

responseDeleteDomainAssociation :: DeleteDomainAssociationResponse -> TestTree
responseDeleteDomainAssociation =
  res
    "DeleteDomainAssociationResponse"
    "fixture/DeleteDomainAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomainAssociation)

responseDeleteJob :: DeleteJobResponse -> TestTree
responseDeleteJob =
  res
    "DeleteJobResponse"
    "fixture/DeleteJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJob)

responseDeleteWebhook :: DeleteWebhookResponse -> TestTree
responseDeleteWebhook =
  res
    "DeleteWebhookResponse"
    "fixture/DeleteWebhookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWebhook)

responseGenerateAccessLogs :: GenerateAccessLogsResponse -> TestTree
responseGenerateAccessLogs =
  res
    "GenerateAccessLogsResponse"
    "fixture/GenerateAccessLogsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateAccessLogs)

responseGetApp :: GetAppResponse -> TestTree
responseGetApp =
  res
    "GetAppResponse"
    "fixture/GetAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApp)

responseGetArtifactUrl :: GetArtifactUrlResponse -> TestTree
responseGetArtifactUrl =
  res
    "GetArtifactUrlResponse"
    "fixture/GetArtifactUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetArtifactUrl)

responseGetBackendEnvironment :: GetBackendEnvironmentResponse -> TestTree
responseGetBackendEnvironment =
  res
    "GetBackendEnvironmentResponse"
    "fixture/GetBackendEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackendEnvironment)

responseGetBranch :: GetBranchResponse -> TestTree
responseGetBranch =
  res
    "GetBranchResponse"
    "fixture/GetBranchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBranch)

responseGetDomainAssociation :: GetDomainAssociationResponse -> TestTree
responseGetDomainAssociation =
  res
    "GetDomainAssociationResponse"
    "fixture/GetDomainAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomainAssociation)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJob)

responseGetWebhook :: GetWebhookResponse -> TestTree
responseGetWebhook =
  res
    "GetWebhookResponse"
    "fixture/GetWebhookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWebhook)

responseListApps :: ListAppsResponse -> TestTree
responseListApps =
  res
    "ListAppsResponse"
    "fixture/ListAppsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApps)

responseListArtifacts :: ListArtifactsResponse -> TestTree
responseListArtifacts =
  res
    "ListArtifactsResponse"
    "fixture/ListArtifactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListArtifacts)

responseListBackendEnvironments :: ListBackendEnvironmentsResponse -> TestTree
responseListBackendEnvironments =
  res
    "ListBackendEnvironmentsResponse"
    "fixture/ListBackendEnvironmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackendEnvironments)

responseListBranches :: ListBranchesResponse -> TestTree
responseListBranches =
  res
    "ListBranchesResponse"
    "fixture/ListBranchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBranches)

responseListDomainAssociations :: ListDomainAssociationsResponse -> TestTree
responseListDomainAssociations =
  res
    "ListDomainAssociationsResponse"
    "fixture/ListDomainAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomainAssociations)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListWebhooks :: ListWebhooksResponse -> TestTree
responseListWebhooks =
  res
    "ListWebhooksResponse"
    "fixture/ListWebhooksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWebhooks)

responseStartDeployment :: StartDeploymentResponse -> TestTree
responseStartDeployment =
  res
    "StartDeploymentResponse"
    "fixture/StartDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDeployment)

responseStartJob :: StartJobResponse -> TestTree
responseStartJob =
  res
    "StartJobResponse"
    "fixture/StartJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartJob)

responseStopJob :: StopJobResponse -> TestTree
responseStopJob =
  res
    "StopJobResponse"
    "fixture/StopJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopJob)

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

responseUpdateApp :: UpdateAppResponse -> TestTree
responseUpdateApp =
  res
    "UpdateAppResponse"
    "fixture/UpdateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApp)

responseUpdateBranch :: UpdateBranchResponse -> TestTree
responseUpdateBranch =
  res
    "UpdateBranchResponse"
    "fixture/UpdateBranchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBranch)

responseUpdateDomainAssociation :: UpdateDomainAssociationResponse -> TestTree
responseUpdateDomainAssociation =
  res
    "UpdateDomainAssociationResponse"
    "fixture/UpdateDomainAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainAssociation)

responseUpdateWebhook :: UpdateWebhookResponse -> TestTree
responseUpdateWebhook =
  res
    "UpdateWebhookResponse"
    "fixture/UpdateWebhookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWebhook)
