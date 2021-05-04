{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeDeploy
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestBatchGetOnPremisesInstances $
--             newBatchGetOnPremisesInstances
--
--         , requestGetApplicationRevision $
--             newGetApplicationRevision
--
--         , requestBatchGetDeploymentGroups $
--             newBatchGetDeploymentGroups
--
--         , requestCreateDeploymentConfig $
--             newCreateDeploymentConfig
--
--         , requestStopDeployment $
--             newStopDeployment
--
--         , requestListDeploymentTargets $
--             newListDeploymentTargets
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestAddTagsToOnPremisesInstances $
--             newAddTagsToOnPremisesInstances
--
--         , requestGetDeploymentTarget $
--             newGetDeploymentTarget
--
--         , requestDeleteResourcesByExternalId $
--             newDeleteResourcesByExternalId
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestBatchGetApplications $
--             newBatchGetApplications
--
--         , requestBatchGetApplicationRevisions $
--             newBatchGetApplicationRevisions
--
--         , requestListDeployments $
--             newListDeployments
--
--         , requestTagResource $
--             newTagResource
--
--         , requestContinueDeployment $
--             newContinueDeployment
--
--         , requestGetDeploymentConfig $
--             newGetDeploymentConfig
--
--         , requestDeleteDeploymentConfig $
--             newDeleteDeploymentConfig
--
--         , requestCreateDeploymentGroup $
--             newCreateDeploymentGroup
--
--         , requestListDeploymentConfigs $
--             newListDeploymentConfigs
--
--         , requestDeleteDeploymentGroup $
--             newDeleteDeploymentGroup
--
--         , requestListDeploymentGroups $
--             newListDeploymentGroups
--
--         , requestListOnPremisesInstances $
--             newListOnPremisesInstances
--
--         , requestUpdateDeploymentGroup $
--             newUpdateDeploymentGroup
--
--         , requestGetDeployment $
--             newGetDeployment
--
--         , requestRegisterOnPremisesInstance $
--             newRegisterOnPremisesInstance
--
--         , requestRemoveTagsFromOnPremisesInstances $
--             newRemoveTagsFromOnPremisesInstances
--
--         , requestGetApplication $
--             newGetApplication
--
--         , requestBatchGetDeploymentTargets $
--             newBatchGetDeploymentTargets
--
--         , requestListGitHubAccountTokenNames $
--             newListGitHubAccountTokenNames
--
--         , requestDeleteGitHubAccountToken $
--             newDeleteGitHubAccountToken
--
--         , requestPutLifecycleEventHookExecutionStatus $
--             newPutLifecycleEventHookExecutionStatus
--
--         , requestDeregisterOnPremisesInstance $
--             newDeregisterOnPremisesInstance
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestListApplications $
--             newListApplications
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestRegisterApplicationRevision $
--             newRegisterApplicationRevision
--
--         , requestGetOnPremisesInstance $
--             newGetOnPremisesInstance
--
--         , requestListApplicationRevisions $
--             newListApplicationRevisions
--
--         , requestBatchGetDeployments $
--             newBatchGetDeployments
--
--         , requestGetDeploymentGroup $
--             newGetDeploymentGroup
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--           ]

--     , testGroup "response"
--         [ responseBatchGetOnPremisesInstances $
--             newBatchGetOnPremisesInstancesResponse
--
--         , responseGetApplicationRevision $
--             newGetApplicationRevisionResponse
--
--         , responseBatchGetDeploymentGroups $
--             newBatchGetDeploymentGroupsResponse
--
--         , responseCreateDeploymentConfig $
--             newCreateDeploymentConfigResponse
--
--         , responseStopDeployment $
--             newStopDeploymentResponse
--
--         , responseListDeploymentTargets $
--             newListDeploymentTargetsResponse
--
--         , responseCreateApplication $
--             newCreateApplicationResponse
--
--         , responseAddTagsToOnPremisesInstances $
--             newAddTagsToOnPremisesInstancesResponse
--
--         , responseGetDeploymentTarget $
--             newGetDeploymentTargetResponse
--
--         , responseDeleteResourcesByExternalId $
--             newDeleteResourcesByExternalIdResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseBatchGetApplications $
--             newBatchGetApplicationsResponse
--
--         , responseBatchGetApplicationRevisions $
--             newBatchGetApplicationRevisionsResponse
--
--         , responseListDeployments $
--             newListDeploymentsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseContinueDeployment $
--             newContinueDeploymentResponse
--
--         , responseGetDeploymentConfig $
--             newGetDeploymentConfigResponse
--
--         , responseDeleteDeploymentConfig $
--             newDeleteDeploymentConfigResponse
--
--         , responseCreateDeploymentGroup $
--             newCreateDeploymentGroupResponse
--
--         , responseListDeploymentConfigs $
--             newListDeploymentConfigsResponse
--
--         , responseDeleteDeploymentGroup $
--             newDeleteDeploymentGroupResponse
--
--         , responseListDeploymentGroups $
--             newListDeploymentGroupsResponse
--
--         , responseListOnPremisesInstances $
--             newListOnPremisesInstancesResponse
--
--         , responseUpdateDeploymentGroup $
--             newUpdateDeploymentGroupResponse
--
--         , responseGetDeployment $
--             newGetDeploymentResponse
--
--         , responseRegisterOnPremisesInstance $
--             newRegisterOnPremisesInstanceResponse
--
--         , responseRemoveTagsFromOnPremisesInstances $
--             newRemoveTagsFromOnPremisesInstancesResponse
--
--         , responseGetApplication $
--             newGetApplicationResponse
--
--         , responseBatchGetDeploymentTargets $
--             newBatchGetDeploymentTargetsResponse
--
--         , responseListGitHubAccountTokenNames $
--             newListGitHubAccountTokenNamesResponse
--
--         , responseDeleteGitHubAccountToken $
--             newDeleteGitHubAccountTokenResponse
--
--         , responsePutLifecycleEventHookExecutionStatus $
--             newPutLifecycleEventHookExecutionStatusResponse
--
--         , responseDeregisterOnPremisesInstance $
--             newDeregisterOnPremisesInstanceResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseUpdateApplication $
--             newUpdateApplicationResponse
--
--         , responseRegisterApplicationRevision $
--             newRegisterApplicationRevisionResponse
--
--         , responseGetOnPremisesInstance $
--             newGetOnPremisesInstanceResponse
--
--         , responseListApplicationRevisions $
--             newListApplicationRevisionsResponse
--
--         , responseBatchGetDeployments $
--             newBatchGetDeploymentsResponse
--
--         , responseGetDeploymentGroup $
--             newGetDeploymentGroupResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--           ]
--     ]

-- Requests

requestBatchGetOnPremisesInstances :: BatchGetOnPremisesInstances -> TestTree
requestBatchGetOnPremisesInstances =
  req
    "BatchGetOnPremisesInstances"
    "fixture/BatchGetOnPremisesInstances.yaml"

requestGetApplicationRevision :: GetApplicationRevision -> TestTree
requestGetApplicationRevision =
  req
    "GetApplicationRevision"
    "fixture/GetApplicationRevision.yaml"

requestBatchGetDeploymentGroups :: BatchGetDeploymentGroups -> TestTree
requestBatchGetDeploymentGroups =
  req
    "BatchGetDeploymentGroups"
    "fixture/BatchGetDeploymentGroups.yaml"

requestCreateDeploymentConfig :: CreateDeploymentConfig -> TestTree
requestCreateDeploymentConfig =
  req
    "CreateDeploymentConfig"
    "fixture/CreateDeploymentConfig.yaml"

requestStopDeployment :: StopDeployment -> TestTree
requestStopDeployment =
  req
    "StopDeployment"
    "fixture/StopDeployment.yaml"

requestListDeploymentTargets :: ListDeploymentTargets -> TestTree
requestListDeploymentTargets =
  req
    "ListDeploymentTargets"
    "fixture/ListDeploymentTargets.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestAddTagsToOnPremisesInstances :: AddTagsToOnPremisesInstances -> TestTree
requestAddTagsToOnPremisesInstances =
  req
    "AddTagsToOnPremisesInstances"
    "fixture/AddTagsToOnPremisesInstances.yaml"

requestGetDeploymentTarget :: GetDeploymentTarget -> TestTree
requestGetDeploymentTarget =
  req
    "GetDeploymentTarget"
    "fixture/GetDeploymentTarget.yaml"

requestDeleteResourcesByExternalId :: DeleteResourcesByExternalId -> TestTree
requestDeleteResourcesByExternalId =
  req
    "DeleteResourcesByExternalId"
    "fixture/DeleteResourcesByExternalId.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestBatchGetApplications :: BatchGetApplications -> TestTree
requestBatchGetApplications =
  req
    "BatchGetApplications"
    "fixture/BatchGetApplications.yaml"

requestBatchGetApplicationRevisions :: BatchGetApplicationRevisions -> TestTree
requestBatchGetApplicationRevisions =
  req
    "BatchGetApplicationRevisions"
    "fixture/BatchGetApplicationRevisions.yaml"

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

requestContinueDeployment :: ContinueDeployment -> TestTree
requestContinueDeployment =
  req
    "ContinueDeployment"
    "fixture/ContinueDeployment.yaml"

requestGetDeploymentConfig :: GetDeploymentConfig -> TestTree
requestGetDeploymentConfig =
  req
    "GetDeploymentConfig"
    "fixture/GetDeploymentConfig.yaml"

requestDeleteDeploymentConfig :: DeleteDeploymentConfig -> TestTree
requestDeleteDeploymentConfig =
  req
    "DeleteDeploymentConfig"
    "fixture/DeleteDeploymentConfig.yaml"

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

requestDeleteDeploymentGroup :: DeleteDeploymentGroup -> TestTree
requestDeleteDeploymentGroup =
  req
    "DeleteDeploymentGroup"
    "fixture/DeleteDeploymentGroup.yaml"

requestListDeploymentGroups :: ListDeploymentGroups -> TestTree
requestListDeploymentGroups =
  req
    "ListDeploymentGroups"
    "fixture/ListDeploymentGroups.yaml"

requestListOnPremisesInstances :: ListOnPremisesInstances -> TestTree
requestListOnPremisesInstances =
  req
    "ListOnPremisesInstances"
    "fixture/ListOnPremisesInstances.yaml"

requestUpdateDeploymentGroup :: UpdateDeploymentGroup -> TestTree
requestUpdateDeploymentGroup =
  req
    "UpdateDeploymentGroup"
    "fixture/UpdateDeploymentGroup.yaml"

requestGetDeployment :: GetDeployment -> TestTree
requestGetDeployment =
  req
    "GetDeployment"
    "fixture/GetDeployment.yaml"

requestRegisterOnPremisesInstance :: RegisterOnPremisesInstance -> TestTree
requestRegisterOnPremisesInstance =
  req
    "RegisterOnPremisesInstance"
    "fixture/RegisterOnPremisesInstance.yaml"

requestRemoveTagsFromOnPremisesInstances :: RemoveTagsFromOnPremisesInstances -> TestTree
requestRemoveTagsFromOnPremisesInstances =
  req
    "RemoveTagsFromOnPremisesInstances"
    "fixture/RemoveTagsFromOnPremisesInstances.yaml"

requestGetApplication :: GetApplication -> TestTree
requestGetApplication =
  req
    "GetApplication"
    "fixture/GetApplication.yaml"

requestBatchGetDeploymentTargets :: BatchGetDeploymentTargets -> TestTree
requestBatchGetDeploymentTargets =
  req
    "BatchGetDeploymentTargets"
    "fixture/BatchGetDeploymentTargets.yaml"

requestListGitHubAccountTokenNames :: ListGitHubAccountTokenNames -> TestTree
requestListGitHubAccountTokenNames =
  req
    "ListGitHubAccountTokenNames"
    "fixture/ListGitHubAccountTokenNames.yaml"

requestDeleteGitHubAccountToken :: DeleteGitHubAccountToken -> TestTree
requestDeleteGitHubAccountToken =
  req
    "DeleteGitHubAccountToken"
    "fixture/DeleteGitHubAccountToken.yaml"

requestPutLifecycleEventHookExecutionStatus :: PutLifecycleEventHookExecutionStatus -> TestTree
requestPutLifecycleEventHookExecutionStatus =
  req
    "PutLifecycleEventHookExecutionStatus"
    "fixture/PutLifecycleEventHookExecutionStatus.yaml"

requestDeregisterOnPremisesInstance :: DeregisterOnPremisesInstance -> TestTree
requestDeregisterOnPremisesInstance =
  req
    "DeregisterOnPremisesInstance"
    "fixture/DeregisterOnPremisesInstance.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestRegisterApplicationRevision :: RegisterApplicationRevision -> TestTree
requestRegisterApplicationRevision =
  req
    "RegisterApplicationRevision"
    "fixture/RegisterApplicationRevision.yaml"

requestGetOnPremisesInstance :: GetOnPremisesInstance -> TestTree
requestGetOnPremisesInstance =
  req
    "GetOnPremisesInstance"
    "fixture/GetOnPremisesInstance.yaml"

requestListApplicationRevisions :: ListApplicationRevisions -> TestTree
requestListApplicationRevisions =
  req
    "ListApplicationRevisions"
    "fixture/ListApplicationRevisions.yaml"

requestBatchGetDeployments :: BatchGetDeployments -> TestTree
requestBatchGetDeployments =
  req
    "BatchGetDeployments"
    "fixture/BatchGetDeployments.yaml"

requestGetDeploymentGroup :: GetDeploymentGroup -> TestTree
requestGetDeploymentGroup =
  req
    "GetDeploymentGroup"
    "fixture/GetDeploymentGroup.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

-- Responses

responseBatchGetOnPremisesInstances :: BatchGetOnPremisesInstancesResponse -> TestTree
responseBatchGetOnPremisesInstances =
  res
    "BatchGetOnPremisesInstancesResponse"
    "fixture/BatchGetOnPremisesInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetOnPremisesInstances)

responseGetApplicationRevision :: GetApplicationRevisionResponse -> TestTree
responseGetApplicationRevision =
  res
    "GetApplicationRevisionResponse"
    "fixture/GetApplicationRevisionResponse.proto"
    defaultService
    (Proxy :: Proxy GetApplicationRevision)

responseBatchGetDeploymentGroups :: BatchGetDeploymentGroupsResponse -> TestTree
responseBatchGetDeploymentGroups =
  res
    "BatchGetDeploymentGroupsResponse"
    "fixture/BatchGetDeploymentGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetDeploymentGroups)

responseCreateDeploymentConfig :: CreateDeploymentConfigResponse -> TestTree
responseCreateDeploymentConfig =
  res
    "CreateDeploymentConfigResponse"
    "fixture/CreateDeploymentConfigResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeploymentConfig)

responseStopDeployment :: StopDeploymentResponse -> TestTree
responseStopDeployment =
  res
    "StopDeploymentResponse"
    "fixture/StopDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy StopDeployment)

responseListDeploymentTargets :: ListDeploymentTargetsResponse -> TestTree
responseListDeploymentTargets =
  res
    "ListDeploymentTargetsResponse"
    "fixture/ListDeploymentTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeploymentTargets)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApplication)

responseAddTagsToOnPremisesInstances :: AddTagsToOnPremisesInstancesResponse -> TestTree
responseAddTagsToOnPremisesInstances =
  res
    "AddTagsToOnPremisesInstancesResponse"
    "fixture/AddTagsToOnPremisesInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy AddTagsToOnPremisesInstances)

responseGetDeploymentTarget :: GetDeploymentTargetResponse -> TestTree
responseGetDeploymentTarget =
  res
    "GetDeploymentTargetResponse"
    "fixture/GetDeploymentTargetResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeploymentTarget)

responseDeleteResourcesByExternalId :: DeleteResourcesByExternalIdResponse -> TestTree
responseDeleteResourcesByExternalId =
  res
    "DeleteResourcesByExternalIdResponse"
    "fixture/DeleteResourcesByExternalIdResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResourcesByExternalId)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseBatchGetApplications :: BatchGetApplicationsResponse -> TestTree
responseBatchGetApplications =
  res
    "BatchGetApplicationsResponse"
    "fixture/BatchGetApplicationsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetApplications)

responseBatchGetApplicationRevisions :: BatchGetApplicationRevisionsResponse -> TestTree
responseBatchGetApplicationRevisions =
  res
    "BatchGetApplicationRevisionsResponse"
    "fixture/BatchGetApplicationRevisionsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetApplicationRevisions)

responseListDeployments :: ListDeploymentsResponse -> TestTree
responseListDeployments =
  res
    "ListDeploymentsResponse"
    "fixture/ListDeploymentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeployments)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseContinueDeployment :: ContinueDeploymentResponse -> TestTree
responseContinueDeployment =
  res
    "ContinueDeploymentResponse"
    "fixture/ContinueDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy ContinueDeployment)

responseGetDeploymentConfig :: GetDeploymentConfigResponse -> TestTree
responseGetDeploymentConfig =
  res
    "GetDeploymentConfigResponse"
    "fixture/GetDeploymentConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeploymentConfig)

responseDeleteDeploymentConfig :: DeleteDeploymentConfigResponse -> TestTree
responseDeleteDeploymentConfig =
  res
    "DeleteDeploymentConfigResponse"
    "fixture/DeleteDeploymentConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDeploymentConfig)

responseCreateDeploymentGroup :: CreateDeploymentGroupResponse -> TestTree
responseCreateDeploymentGroup =
  res
    "CreateDeploymentGroupResponse"
    "fixture/CreateDeploymentGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeploymentGroup)

responseListDeploymentConfigs :: ListDeploymentConfigsResponse -> TestTree
responseListDeploymentConfigs =
  res
    "ListDeploymentConfigsResponse"
    "fixture/ListDeploymentConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeploymentConfigs)

responseDeleteDeploymentGroup :: DeleteDeploymentGroupResponse -> TestTree
responseDeleteDeploymentGroup =
  res
    "DeleteDeploymentGroupResponse"
    "fixture/DeleteDeploymentGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDeploymentGroup)

responseListDeploymentGroups :: ListDeploymentGroupsResponse -> TestTree
responseListDeploymentGroups =
  res
    "ListDeploymentGroupsResponse"
    "fixture/ListDeploymentGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeploymentGroups)

responseListOnPremisesInstances :: ListOnPremisesInstancesResponse -> TestTree
responseListOnPremisesInstances =
  res
    "ListOnPremisesInstancesResponse"
    "fixture/ListOnPremisesInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy ListOnPremisesInstances)

responseUpdateDeploymentGroup :: UpdateDeploymentGroupResponse -> TestTree
responseUpdateDeploymentGroup =
  res
    "UpdateDeploymentGroupResponse"
    "fixture/UpdateDeploymentGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDeploymentGroup)

responseGetDeployment :: GetDeploymentResponse -> TestTree
responseGetDeployment =
  res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeployment)

responseRegisterOnPremisesInstance :: RegisterOnPremisesInstanceResponse -> TestTree
responseRegisterOnPremisesInstance =
  res
    "RegisterOnPremisesInstanceResponse"
    "fixture/RegisterOnPremisesInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterOnPremisesInstance)

responseRemoveTagsFromOnPremisesInstances :: RemoveTagsFromOnPremisesInstancesResponse -> TestTree
responseRemoveTagsFromOnPremisesInstances =
  res
    "RemoveTagsFromOnPremisesInstancesResponse"
    "fixture/RemoveTagsFromOnPremisesInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTagsFromOnPremisesInstances)

responseGetApplication :: GetApplicationResponse -> TestTree
responseGetApplication =
  res
    "GetApplicationResponse"
    "fixture/GetApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy GetApplication)

responseBatchGetDeploymentTargets :: BatchGetDeploymentTargetsResponse -> TestTree
responseBatchGetDeploymentTargets =
  res
    "BatchGetDeploymentTargetsResponse"
    "fixture/BatchGetDeploymentTargetsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetDeploymentTargets)

responseListGitHubAccountTokenNames :: ListGitHubAccountTokenNamesResponse -> TestTree
responseListGitHubAccountTokenNames =
  res
    "ListGitHubAccountTokenNamesResponse"
    "fixture/ListGitHubAccountTokenNamesResponse.proto"
    defaultService
    (Proxy :: Proxy ListGitHubAccountTokenNames)

responseDeleteGitHubAccountToken :: DeleteGitHubAccountTokenResponse -> TestTree
responseDeleteGitHubAccountToken =
  res
    "DeleteGitHubAccountTokenResponse"
    "fixture/DeleteGitHubAccountTokenResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGitHubAccountToken)

responsePutLifecycleEventHookExecutionStatus :: PutLifecycleEventHookExecutionStatusResponse -> TestTree
responsePutLifecycleEventHookExecutionStatus =
  res
    "PutLifecycleEventHookExecutionStatusResponse"
    "fixture/PutLifecycleEventHookExecutionStatusResponse.proto"
    defaultService
    (Proxy :: Proxy PutLifecycleEventHookExecutionStatus)

responseDeregisterOnPremisesInstance :: DeregisterOnPremisesInstanceResponse -> TestTree
responseDeregisterOnPremisesInstance =
  res
    "DeregisterOnPremisesInstanceResponse"
    "fixture/DeregisterOnPremisesInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterOnPremisesInstance)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApplication)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListApplications)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApplication)

responseRegisterApplicationRevision :: RegisterApplicationRevisionResponse -> TestTree
responseRegisterApplicationRevision =
  res
    "RegisterApplicationRevisionResponse"
    "fixture/RegisterApplicationRevisionResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterApplicationRevision)

responseGetOnPremisesInstance :: GetOnPremisesInstanceResponse -> TestTree
responseGetOnPremisesInstance =
  res
    "GetOnPremisesInstanceResponse"
    "fixture/GetOnPremisesInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy GetOnPremisesInstance)

responseListApplicationRevisions :: ListApplicationRevisionsResponse -> TestTree
responseListApplicationRevisions =
  res
    "ListApplicationRevisionsResponse"
    "fixture/ListApplicationRevisionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListApplicationRevisions)

responseBatchGetDeployments :: BatchGetDeploymentsResponse -> TestTree
responseBatchGetDeployments =
  res
    "BatchGetDeploymentsResponse"
    "fixture/BatchGetDeploymentsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetDeployments)

responseGetDeploymentGroup :: GetDeploymentGroupResponse -> TestTree
responseGetDeploymentGroup =
  res
    "GetDeploymentGroupResponse"
    "fixture/GetDeploymentGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeploymentGroup)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeployment)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)
