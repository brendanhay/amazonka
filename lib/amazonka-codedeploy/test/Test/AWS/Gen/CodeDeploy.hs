{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeDeploy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CodeDeploy where

import Data.Proxy
import Network.AWS.CodeDeploy
import Test.AWS.CodeDeploy.Internal
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
--         [ requestRemoveTagsFromOnPremisesInstances $
--             mkRemoveTagsFromOnPremisesInstances
--
--         , requestBatchGetDeploymentGroups $
--             mkBatchGetDeploymentGroups
--
--         , requestDeleteDeploymentGroup $
--             mkDeleteDeploymentGroup
--
--         , requestUpdateDeploymentGroup $
--             mkUpdateDeploymentGroup
--
--         , requestListOnPremisesInstances $
--             mkListOnPremisesInstances
--
--         , requestCreateDeploymentConfig $
--             mkCreateDeploymentConfig
--
--         , requestGetApplicationRevision $
--             mkGetApplicationRevision
--
--         , requestGetDeployment $
--             mkGetDeployment
--
--         , requestDeleteDeploymentConfig $
--             mkDeleteDeploymentConfig
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestGetDeploymentConfig $
--             mkGetDeploymentConfig
--
--         , requestCreateDeployment $
--             mkCreateDeployment
--
--         , requestBatchGetApplicationRevisions $
--             mkBatchGetApplicationRevisions
--
--         , requestBatchGetDeployments $
--             mkBatchGetDeployments
--
--         , requestGetOnPremisesInstance $
--             mkGetOnPremisesInstance
--
--         , requestRegisterApplicationRevision $
--             mkRegisterApplicationRevision
--
--         , requestContinueDeployment $
--             mkContinueDeployment
--
--         , requestBatchGetApplications $
--             mkBatchGetApplications
--
--         , requestDeleteApplication $
--             mkDeleteApplication
--
--         , requestUpdateApplication $
--             mkUpdateApplication
--
--         , requestDeleteGitHubAccountToken $
--             mkDeleteGitHubAccountToken
--
--         , requestDeregisterOnPremisesInstance $
--             mkDeregisterOnPremisesInstance
--
--         , requestPutLifecycleEventHookExecutionStatus $
--             mkPutLifecycleEventHookExecutionStatus
--
--         , requestGetDeploymentTarget $
--             mkGetDeploymentTarget
--
--         , requestCreateApplication $
--             mkCreateApplication
--
--         , requestBatchGetDeploymentTargets $
--             mkBatchGetDeploymentTargets
--
--         , requestStopDeployment $
--             mkStopDeployment
--
--         , requestListGitHubAccountTokenNames $
--             mkListGitHubAccountTokenNames
--
--         , requestGetApplication $
--             mkGetApplication
--
--         , requestListDeploymentGroups $
--             mkListDeploymentGroups
--
--         , requestBatchGetOnPremisesInstances $
--             mkBatchGetOnPremisesInstances
--
--         , requestRegisterOnPremisesInstance $
--             mkRegisterOnPremisesInstance
--
--         , requestCreateDeploymentGroup $
--             mkCreateDeploymentGroup
--
--         , requestListDeploymentConfigs $
--             mkListDeploymentConfigs
--
--         , requestGetDeploymentGroup $
--             mkGetDeploymentGroup
--
--         , requestListDeployments $
--             mkListDeployments
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestListApplicationRevisions $
--             mkListApplicationRevisions
--
--         , requestListApplications $
--             mkListApplications
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestDeleteResourcesByExternalId $
--             mkDeleteResourcesByExternalId
--
--         , requestAddTagsToOnPremisesInstances $
--             mkAddTagsToOnPremisesInstances
--
--         , requestListDeploymentTargets $
--             mkListDeploymentTargets
--
--           ]

--     , testGroup "response"
--         [ responseRemoveTagsFromOnPremisesInstances $
--             mkRemoveTagsFromOnPremisesInstancesResponse
--
--         , responseBatchGetDeploymentGroups $
--             mkBatchGetDeploymentGroupsResponse
--
--         , responseDeleteDeploymentGroup $
--             mkDeleteDeploymentGroupResponse
--
--         , responseUpdateDeploymentGroup $
--             mkUpdateDeploymentGroupResponse
--
--         , responseListOnPremisesInstances $
--             mkListOnPremisesInstancesResponse
--
--         , responseCreateDeploymentConfig $
--             mkCreateDeploymentConfigResponse
--
--         , responseGetApplicationRevision $
--             mkGetApplicationRevisionResponse
--
--         , responseGetDeployment $
--             mkGetDeploymentResponse
--
--         , responseDeleteDeploymentConfig $
--             mkDeleteDeploymentConfigResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseGetDeploymentConfig $
--             mkGetDeploymentConfigResponse
--
--         , responseCreateDeployment $
--             mkCreateDeploymentResponse
--
--         , responseBatchGetApplicationRevisions $
--             mkBatchGetApplicationRevisionsResponse
--
--         , responseBatchGetDeployments $
--             mkBatchGetDeploymentsResponse
--
--         , responseGetOnPremisesInstance $
--             mkGetOnPremisesInstanceResponse
--
--         , responseRegisterApplicationRevision $
--             mkRegisterApplicationRevisionResponse
--
--         , responseContinueDeployment $
--             mkContinueDeploymentResponse
--
--         , responseBatchGetApplications $
--             mkBatchGetApplicationsResponse
--
--         , responseDeleteApplication $
--             mkDeleteApplicationResponse
--
--         , responseUpdateApplication $
--             mkUpdateApplicationResponse
--
--         , responseDeleteGitHubAccountToken $
--             mkDeleteGitHubAccountTokenResponse
--
--         , responseDeregisterOnPremisesInstance $
--             mkDeregisterOnPremisesInstanceResponse
--
--         , responsePutLifecycleEventHookExecutionStatus $
--             mkPutLifecycleEventHookExecutionStatusResponse
--
--         , responseGetDeploymentTarget $
--             mkGetDeploymentTargetResponse
--
--         , responseCreateApplication $
--             mkCreateApplicationResponse
--
--         , responseBatchGetDeploymentTargets $
--             mkBatchGetDeploymentTargetsResponse
--
--         , responseStopDeployment $
--             mkStopDeploymentResponse
--
--         , responseListGitHubAccountTokenNames $
--             mkListGitHubAccountTokenNamesResponse
--
--         , responseGetApplication $
--             mkGetApplicationResponse
--
--         , responseListDeploymentGroups $
--             mkListDeploymentGroupsResponse
--
--         , responseBatchGetOnPremisesInstances $
--             mkBatchGetOnPremisesInstancesResponse
--
--         , responseRegisterOnPremisesInstance $
--             mkRegisterOnPremisesInstanceResponse
--
--         , responseCreateDeploymentGroup $
--             mkCreateDeploymentGroupResponse
--
--         , responseListDeploymentConfigs $
--             mkListDeploymentConfigsResponse
--
--         , responseGetDeploymentGroup $
--             mkGetDeploymentGroupResponse
--
--         , responseListDeployments $
--             mkListDeploymentsResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseListApplicationRevisions $
--             mkListApplicationRevisionsResponse
--
--         , responseListApplications $
--             mkListApplicationsResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseDeleteResourcesByExternalId $
--             mkDeleteResourcesByExternalIdResponse
--
--         , responseAddTagsToOnPremisesInstances $
--             mkAddTagsToOnPremisesInstancesResponse
--
--         , responseListDeploymentTargets $
--             mkListDeploymentTargetsResponse
--
--           ]
--     ]

-- Requests

requestRemoveTagsFromOnPremisesInstances :: RemoveTagsFromOnPremisesInstances -> TestTree
requestRemoveTagsFromOnPremisesInstances =
  req
    "RemoveTagsFromOnPremisesInstances"
    "fixture/RemoveTagsFromOnPremisesInstances.yaml"

requestBatchGetDeploymentGroups :: BatchGetDeploymentGroups -> TestTree
requestBatchGetDeploymentGroups =
  req
    "BatchGetDeploymentGroups"
    "fixture/BatchGetDeploymentGroups.yaml"

requestDeleteDeploymentGroup :: DeleteDeploymentGroup -> TestTree
requestDeleteDeploymentGroup =
  req
    "DeleteDeploymentGroup"
    "fixture/DeleteDeploymentGroup.yaml"

requestUpdateDeploymentGroup :: UpdateDeploymentGroup -> TestTree
requestUpdateDeploymentGroup =
  req
    "UpdateDeploymentGroup"
    "fixture/UpdateDeploymentGroup.yaml"

requestListOnPremisesInstances :: ListOnPremisesInstances -> TestTree
requestListOnPremisesInstances =
  req
    "ListOnPremisesInstances"
    "fixture/ListOnPremisesInstances.yaml"

requestCreateDeploymentConfig :: CreateDeploymentConfig -> TestTree
requestCreateDeploymentConfig =
  req
    "CreateDeploymentConfig"
    "fixture/CreateDeploymentConfig.yaml"

requestGetApplicationRevision :: GetApplicationRevision -> TestTree
requestGetApplicationRevision =
  req
    "GetApplicationRevision"
    "fixture/GetApplicationRevision.yaml"

requestGetDeployment :: GetDeployment -> TestTree
requestGetDeployment =
  req
    "GetDeployment"
    "fixture/GetDeployment.yaml"

requestDeleteDeploymentConfig :: DeleteDeploymentConfig -> TestTree
requestDeleteDeploymentConfig =
  req
    "DeleteDeploymentConfig"
    "fixture/DeleteDeploymentConfig.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetDeploymentConfig :: GetDeploymentConfig -> TestTree
requestGetDeploymentConfig =
  req
    "GetDeploymentConfig"
    "fixture/GetDeploymentConfig.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestBatchGetApplicationRevisions :: BatchGetApplicationRevisions -> TestTree
requestBatchGetApplicationRevisions =
  req
    "BatchGetApplicationRevisions"
    "fixture/BatchGetApplicationRevisions.yaml"

requestBatchGetDeployments :: BatchGetDeployments -> TestTree
requestBatchGetDeployments =
  req
    "BatchGetDeployments"
    "fixture/BatchGetDeployments.yaml"

requestGetOnPremisesInstance :: GetOnPremisesInstance -> TestTree
requestGetOnPremisesInstance =
  req
    "GetOnPremisesInstance"
    "fixture/GetOnPremisesInstance.yaml"

requestRegisterApplicationRevision :: RegisterApplicationRevision -> TestTree
requestRegisterApplicationRevision =
  req
    "RegisterApplicationRevision"
    "fixture/RegisterApplicationRevision.yaml"

requestContinueDeployment :: ContinueDeployment -> TestTree
requestContinueDeployment =
  req
    "ContinueDeployment"
    "fixture/ContinueDeployment.yaml"

requestBatchGetApplications :: BatchGetApplications -> TestTree
requestBatchGetApplications =
  req
    "BatchGetApplications"
    "fixture/BatchGetApplications.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestDeleteGitHubAccountToken :: DeleteGitHubAccountToken -> TestTree
requestDeleteGitHubAccountToken =
  req
    "DeleteGitHubAccountToken"
    "fixture/DeleteGitHubAccountToken.yaml"

requestDeregisterOnPremisesInstance :: DeregisterOnPremisesInstance -> TestTree
requestDeregisterOnPremisesInstance =
  req
    "DeregisterOnPremisesInstance"
    "fixture/DeregisterOnPremisesInstance.yaml"

requestPutLifecycleEventHookExecutionStatus :: PutLifecycleEventHookExecutionStatus -> TestTree
requestPutLifecycleEventHookExecutionStatus =
  req
    "PutLifecycleEventHookExecutionStatus"
    "fixture/PutLifecycleEventHookExecutionStatus.yaml"

requestGetDeploymentTarget :: GetDeploymentTarget -> TestTree
requestGetDeploymentTarget =
  req
    "GetDeploymentTarget"
    "fixture/GetDeploymentTarget.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestBatchGetDeploymentTargets :: BatchGetDeploymentTargets -> TestTree
requestBatchGetDeploymentTargets =
  req
    "BatchGetDeploymentTargets"
    "fixture/BatchGetDeploymentTargets.yaml"

requestStopDeployment :: StopDeployment -> TestTree
requestStopDeployment =
  req
    "StopDeployment"
    "fixture/StopDeployment.yaml"

requestListGitHubAccountTokenNames :: ListGitHubAccountTokenNames -> TestTree
requestListGitHubAccountTokenNames =
  req
    "ListGitHubAccountTokenNames"
    "fixture/ListGitHubAccountTokenNames.yaml"

requestGetApplication :: GetApplication -> TestTree
requestGetApplication =
  req
    "GetApplication"
    "fixture/GetApplication.yaml"

requestListDeploymentGroups :: ListDeploymentGroups -> TestTree
requestListDeploymentGroups =
  req
    "ListDeploymentGroups"
    "fixture/ListDeploymentGroups.yaml"

requestBatchGetOnPremisesInstances :: BatchGetOnPremisesInstances -> TestTree
requestBatchGetOnPremisesInstances =
  req
    "BatchGetOnPremisesInstances"
    "fixture/BatchGetOnPremisesInstances.yaml"

requestRegisterOnPremisesInstance :: RegisterOnPremisesInstance -> TestTree
requestRegisterOnPremisesInstance =
  req
    "RegisterOnPremisesInstance"
    "fixture/RegisterOnPremisesInstance.yaml"

requestCreateDeploymentGroup :: CreateDeploymentGroup -> TestTree
requestCreateDeploymentGroup =
  req
    "CreateDeploymentGroup"
    "fixture/CreateDeploymentGroup.yaml"

requestListDeploymentConfigs :: ListDeploymentConfigs -> TestTree
requestListDeploymentConfigs =
  req
    "ListDeploymentConfigs"
    "fixture/ListDeploymentConfigs.yaml"

requestGetDeploymentGroup :: GetDeploymentGroup -> TestTree
requestGetDeploymentGroup =
  req
    "GetDeploymentGroup"
    "fixture/GetDeploymentGroup.yaml"

requestListDeployments :: ListDeployments -> TestTree
requestListDeployments =
  req
    "ListDeployments"
    "fixture/ListDeployments.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListApplicationRevisions :: ListApplicationRevisions -> TestTree
requestListApplicationRevisions =
  req
    "ListApplicationRevisions"
    "fixture/ListApplicationRevisions.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteResourcesByExternalId :: DeleteResourcesByExternalId -> TestTree
requestDeleteResourcesByExternalId =
  req
    "DeleteResourcesByExternalId"
    "fixture/DeleteResourcesByExternalId.yaml"

requestAddTagsToOnPremisesInstances :: AddTagsToOnPremisesInstances -> TestTree
requestAddTagsToOnPremisesInstances =
  req
    "AddTagsToOnPremisesInstances"
    "fixture/AddTagsToOnPremisesInstances.yaml"

requestListDeploymentTargets :: ListDeploymentTargets -> TestTree
requestListDeploymentTargets =
  req
    "ListDeploymentTargets"
    "fixture/ListDeploymentTargets.yaml"

-- Responses

responseRemoveTagsFromOnPremisesInstances :: RemoveTagsFromOnPremisesInstancesResponse -> TestTree
responseRemoveTagsFromOnPremisesInstances =
  res
    "RemoveTagsFromOnPremisesInstancesResponse"
    "fixture/RemoveTagsFromOnPremisesInstancesResponse.proto"
    codeDeployService
    (Proxy :: Proxy RemoveTagsFromOnPremisesInstances)

responseBatchGetDeploymentGroups :: BatchGetDeploymentGroupsResponse -> TestTree
responseBatchGetDeploymentGroups =
  res
    "BatchGetDeploymentGroupsResponse"
    "fixture/BatchGetDeploymentGroupsResponse.proto"
    codeDeployService
    (Proxy :: Proxy BatchGetDeploymentGroups)

responseDeleteDeploymentGroup :: DeleteDeploymentGroupResponse -> TestTree
responseDeleteDeploymentGroup =
  res
    "DeleteDeploymentGroupResponse"
    "fixture/DeleteDeploymentGroupResponse.proto"
    codeDeployService
    (Proxy :: Proxy DeleteDeploymentGroup)

responseUpdateDeploymentGroup :: UpdateDeploymentGroupResponse -> TestTree
responseUpdateDeploymentGroup =
  res
    "UpdateDeploymentGroupResponse"
    "fixture/UpdateDeploymentGroupResponse.proto"
    codeDeployService
    (Proxy :: Proxy UpdateDeploymentGroup)

responseListOnPremisesInstances :: ListOnPremisesInstancesResponse -> TestTree
responseListOnPremisesInstances =
  res
    "ListOnPremisesInstancesResponse"
    "fixture/ListOnPremisesInstancesResponse.proto"
    codeDeployService
    (Proxy :: Proxy ListOnPremisesInstances)

responseCreateDeploymentConfig :: CreateDeploymentConfigResponse -> TestTree
responseCreateDeploymentConfig =
  res
    "CreateDeploymentConfigResponse"
    "fixture/CreateDeploymentConfigResponse.proto"
    codeDeployService
    (Proxy :: Proxy CreateDeploymentConfig)

responseGetApplicationRevision :: GetApplicationRevisionResponse -> TestTree
responseGetApplicationRevision =
  res
    "GetApplicationRevisionResponse"
    "fixture/GetApplicationRevisionResponse.proto"
    codeDeployService
    (Proxy :: Proxy GetApplicationRevision)

responseGetDeployment :: GetDeploymentResponse -> TestTree
responseGetDeployment =
  res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    codeDeployService
    (Proxy :: Proxy GetDeployment)

responseDeleteDeploymentConfig :: DeleteDeploymentConfigResponse -> TestTree
responseDeleteDeploymentConfig =
  res
    "DeleteDeploymentConfigResponse"
    "fixture/DeleteDeploymentConfigResponse.proto"
    codeDeployService
    (Proxy :: Proxy DeleteDeploymentConfig)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    codeDeployService
    (Proxy :: Proxy ListTagsForResource)

responseGetDeploymentConfig :: GetDeploymentConfigResponse -> TestTree
responseGetDeploymentConfig =
  res
    "GetDeploymentConfigResponse"
    "fixture/GetDeploymentConfigResponse.proto"
    codeDeployService
    (Proxy :: Proxy GetDeploymentConfig)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    codeDeployService
    (Proxy :: Proxy CreateDeployment)

responseBatchGetApplicationRevisions :: BatchGetApplicationRevisionsResponse -> TestTree
responseBatchGetApplicationRevisions =
  res
    "BatchGetApplicationRevisionsResponse"
    "fixture/BatchGetApplicationRevisionsResponse.proto"
    codeDeployService
    (Proxy :: Proxy BatchGetApplicationRevisions)

responseBatchGetDeployments :: BatchGetDeploymentsResponse -> TestTree
responseBatchGetDeployments =
  res
    "BatchGetDeploymentsResponse"
    "fixture/BatchGetDeploymentsResponse.proto"
    codeDeployService
    (Proxy :: Proxy BatchGetDeployments)

responseGetOnPremisesInstance :: GetOnPremisesInstanceResponse -> TestTree
responseGetOnPremisesInstance =
  res
    "GetOnPremisesInstanceResponse"
    "fixture/GetOnPremisesInstanceResponse.proto"
    codeDeployService
    (Proxy :: Proxy GetOnPremisesInstance)

responseRegisterApplicationRevision :: RegisterApplicationRevisionResponse -> TestTree
responseRegisterApplicationRevision =
  res
    "RegisterApplicationRevisionResponse"
    "fixture/RegisterApplicationRevisionResponse.proto"
    codeDeployService
    (Proxy :: Proxy RegisterApplicationRevision)

responseContinueDeployment :: ContinueDeploymentResponse -> TestTree
responseContinueDeployment =
  res
    "ContinueDeploymentResponse"
    "fixture/ContinueDeploymentResponse.proto"
    codeDeployService
    (Proxy :: Proxy ContinueDeployment)

responseBatchGetApplications :: BatchGetApplicationsResponse -> TestTree
responseBatchGetApplications =
  res
    "BatchGetApplicationsResponse"
    "fixture/BatchGetApplicationsResponse.proto"
    codeDeployService
    (Proxy :: Proxy BatchGetApplications)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    codeDeployService
    (Proxy :: Proxy DeleteApplication)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    codeDeployService
    (Proxy :: Proxy UpdateApplication)

responseDeleteGitHubAccountToken :: DeleteGitHubAccountTokenResponse -> TestTree
responseDeleteGitHubAccountToken =
  res
    "DeleteGitHubAccountTokenResponse"
    "fixture/DeleteGitHubAccountTokenResponse.proto"
    codeDeployService
    (Proxy :: Proxy DeleteGitHubAccountToken)

responseDeregisterOnPremisesInstance :: DeregisterOnPremisesInstanceResponse -> TestTree
responseDeregisterOnPremisesInstance =
  res
    "DeregisterOnPremisesInstanceResponse"
    "fixture/DeregisterOnPremisesInstanceResponse.proto"
    codeDeployService
    (Proxy :: Proxy DeregisterOnPremisesInstance)

responsePutLifecycleEventHookExecutionStatus :: PutLifecycleEventHookExecutionStatusResponse -> TestTree
responsePutLifecycleEventHookExecutionStatus =
  res
    "PutLifecycleEventHookExecutionStatusResponse"
    "fixture/PutLifecycleEventHookExecutionStatusResponse.proto"
    codeDeployService
    (Proxy :: Proxy PutLifecycleEventHookExecutionStatus)

responseGetDeploymentTarget :: GetDeploymentTargetResponse -> TestTree
responseGetDeploymentTarget =
  res
    "GetDeploymentTargetResponse"
    "fixture/GetDeploymentTargetResponse.proto"
    codeDeployService
    (Proxy :: Proxy GetDeploymentTarget)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    codeDeployService
    (Proxy :: Proxy CreateApplication)

responseBatchGetDeploymentTargets :: BatchGetDeploymentTargetsResponse -> TestTree
responseBatchGetDeploymentTargets =
  res
    "BatchGetDeploymentTargetsResponse"
    "fixture/BatchGetDeploymentTargetsResponse.proto"
    codeDeployService
    (Proxy :: Proxy BatchGetDeploymentTargets)

responseStopDeployment :: StopDeploymentResponse -> TestTree
responseStopDeployment =
  res
    "StopDeploymentResponse"
    "fixture/StopDeploymentResponse.proto"
    codeDeployService
    (Proxy :: Proxy StopDeployment)

responseListGitHubAccountTokenNames :: ListGitHubAccountTokenNamesResponse -> TestTree
responseListGitHubAccountTokenNames =
  res
    "ListGitHubAccountTokenNamesResponse"
    "fixture/ListGitHubAccountTokenNamesResponse.proto"
    codeDeployService
    (Proxy :: Proxy ListGitHubAccountTokenNames)

responseGetApplication :: GetApplicationResponse -> TestTree
responseGetApplication =
  res
    "GetApplicationResponse"
    "fixture/GetApplicationResponse.proto"
    codeDeployService
    (Proxy :: Proxy GetApplication)

responseListDeploymentGroups :: ListDeploymentGroupsResponse -> TestTree
responseListDeploymentGroups =
  res
    "ListDeploymentGroupsResponse"
    "fixture/ListDeploymentGroupsResponse.proto"
    codeDeployService
    (Proxy :: Proxy ListDeploymentGroups)

responseBatchGetOnPremisesInstances :: BatchGetOnPremisesInstancesResponse -> TestTree
responseBatchGetOnPremisesInstances =
  res
    "BatchGetOnPremisesInstancesResponse"
    "fixture/BatchGetOnPremisesInstancesResponse.proto"
    codeDeployService
    (Proxy :: Proxy BatchGetOnPremisesInstances)

responseRegisterOnPremisesInstance :: RegisterOnPremisesInstanceResponse -> TestTree
responseRegisterOnPremisesInstance =
  res
    "RegisterOnPremisesInstanceResponse"
    "fixture/RegisterOnPremisesInstanceResponse.proto"
    codeDeployService
    (Proxy :: Proxy RegisterOnPremisesInstance)

responseCreateDeploymentGroup :: CreateDeploymentGroupResponse -> TestTree
responseCreateDeploymentGroup =
  res
    "CreateDeploymentGroupResponse"
    "fixture/CreateDeploymentGroupResponse.proto"
    codeDeployService
    (Proxy :: Proxy CreateDeploymentGroup)

responseListDeploymentConfigs :: ListDeploymentConfigsResponse -> TestTree
responseListDeploymentConfigs =
  res
    "ListDeploymentConfigsResponse"
    "fixture/ListDeploymentConfigsResponse.proto"
    codeDeployService
    (Proxy :: Proxy ListDeploymentConfigs)

responseGetDeploymentGroup :: GetDeploymentGroupResponse -> TestTree
responseGetDeploymentGroup =
  res
    "GetDeploymentGroupResponse"
    "fixture/GetDeploymentGroupResponse.proto"
    codeDeployService
    (Proxy :: Proxy GetDeploymentGroup)

responseListDeployments :: ListDeploymentsResponse -> TestTree
responseListDeployments =
  res
    "ListDeploymentsResponse"
    "fixture/ListDeploymentsResponse.proto"
    codeDeployService
    (Proxy :: Proxy ListDeployments)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    codeDeployService
    (Proxy :: Proxy TagResource)

responseListApplicationRevisions :: ListApplicationRevisionsResponse -> TestTree
responseListApplicationRevisions =
  res
    "ListApplicationRevisionsResponse"
    "fixture/ListApplicationRevisionsResponse.proto"
    codeDeployService
    (Proxy :: Proxy ListApplicationRevisions)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    codeDeployService
    (Proxy :: Proxy ListApplications)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    codeDeployService
    (Proxy :: Proxy UntagResource)

responseDeleteResourcesByExternalId :: DeleteResourcesByExternalIdResponse -> TestTree
responseDeleteResourcesByExternalId =
  res
    "DeleteResourcesByExternalIdResponse"
    "fixture/DeleteResourcesByExternalIdResponse.proto"
    codeDeployService
    (Proxy :: Proxy DeleteResourcesByExternalId)

responseAddTagsToOnPremisesInstances :: AddTagsToOnPremisesInstancesResponse -> TestTree
responseAddTagsToOnPremisesInstances =
  res
    "AddTagsToOnPremisesInstancesResponse"
    "fixture/AddTagsToOnPremisesInstancesResponse.proto"
    codeDeployService
    (Proxy :: Proxy AddTagsToOnPremisesInstances)

responseListDeploymentTargets :: ListDeploymentTargetsResponse -> TestTree
responseListDeploymentTargets =
  res
    "ListDeploymentTargetsResponse"
    "fixture/ListDeploymentTargetsResponse.proto"
    codeDeployService
    (Proxy :: Proxy ListDeploymentTargets)
