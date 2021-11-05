{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CodeDeploy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CodeDeploy where

import Amazonka.CodeDeploy
import qualified Data.Proxy as Proxy
import Test.Amazonka.CodeDeploy.Internal
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
--         [ requestRemoveTagsFromOnPremisesInstances $
--             newRemoveTagsFromOnPremisesInstances
--
--         , requestBatchGetDeploymentGroups $
--             newBatchGetDeploymentGroups
--
--         , requestDeleteDeploymentGroup $
--             newDeleteDeploymentGroup
--
--         , requestUpdateDeploymentGroup $
--             newUpdateDeploymentGroup
--
--         , requestListOnPremisesInstances $
--             newListOnPremisesInstances
--
--         , requestCreateDeploymentConfig $
--             newCreateDeploymentConfig
--
--         , requestGetApplicationRevision $
--             newGetApplicationRevision
--
--         , requestGetDeployment $
--             newGetDeployment
--
--         , requestDeleteDeploymentConfig $
--             newDeleteDeploymentConfig
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetDeploymentConfig $
--             newGetDeploymentConfig
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestBatchGetApplicationRevisions $
--             newBatchGetApplicationRevisions
--
--         , requestBatchGetDeployments $
--             newBatchGetDeployments
--
--         , requestGetOnPremisesInstance $
--             newGetOnPremisesInstance
--
--         , requestRegisterApplicationRevision $
--             newRegisterApplicationRevision
--
--         , requestContinueDeployment $
--             newContinueDeployment
--
--         , requestBatchGetApplications $
--             newBatchGetApplications
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestDeleteGitHubAccountToken $
--             newDeleteGitHubAccountToken
--
--         , requestDeregisterOnPremisesInstance $
--             newDeregisterOnPremisesInstance
--
--         , requestPutLifecycleEventHookExecutionStatus $
--             newPutLifecycleEventHookExecutionStatus
--
--         , requestGetDeploymentTarget $
--             newGetDeploymentTarget
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestBatchGetDeploymentTargets $
--             newBatchGetDeploymentTargets
--
--         , requestStopDeployment $
--             newStopDeployment
--
--         , requestListGitHubAccountTokenNames $
--             newListGitHubAccountTokenNames
--
--         , requestGetApplication $
--             newGetApplication
--
--         , requestListDeploymentGroups $
--             newListDeploymentGroups
--
--         , requestBatchGetOnPremisesInstances $
--             newBatchGetOnPremisesInstances
--
--         , requestRegisterOnPremisesInstance $
--             newRegisterOnPremisesInstance
--
--         , requestCreateDeploymentGroup $
--             newCreateDeploymentGroup
--
--         , requestListDeploymentConfigs $
--             newListDeploymentConfigs
--
--         , requestGetDeploymentGroup $
--             newGetDeploymentGroup
--
--         , requestListDeployments $
--             newListDeployments
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListApplicationRevisions $
--             newListApplicationRevisions
--
--         , requestListApplications $
--             newListApplications
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteResourcesByExternalId $
--             newDeleteResourcesByExternalId
--
--         , requestAddTagsToOnPremisesInstances $
--             newAddTagsToOnPremisesInstances
--
--         , requestListDeploymentTargets $
--             newListDeploymentTargets
--
--           ]

--     , testGroup "response"
--         [ responseRemoveTagsFromOnPremisesInstances $
--             newRemoveTagsFromOnPremisesInstancesResponse
--
--         , responseBatchGetDeploymentGroups $
--             newBatchGetDeploymentGroupsResponse
--
--         , responseDeleteDeploymentGroup $
--             newDeleteDeploymentGroupResponse
--
--         , responseUpdateDeploymentGroup $
--             newUpdateDeploymentGroupResponse
--
--         , responseListOnPremisesInstances $
--             newListOnPremisesInstancesResponse
--
--         , responseCreateDeploymentConfig $
--             newCreateDeploymentConfigResponse
--
--         , responseGetApplicationRevision $
--             newGetApplicationRevisionResponse
--
--         , responseGetDeployment $
--             newGetDeploymentResponse
--
--         , responseDeleteDeploymentConfig $
--             newDeleteDeploymentConfigResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetDeploymentConfig $
--             newGetDeploymentConfigResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseBatchGetApplicationRevisions $
--             newBatchGetApplicationRevisionsResponse
--
--         , responseBatchGetDeployments $
--             newBatchGetDeploymentsResponse
--
--         , responseGetOnPremisesInstance $
--             newGetOnPremisesInstanceResponse
--
--         , responseRegisterApplicationRevision $
--             newRegisterApplicationRevisionResponse
--
--         , responseContinueDeployment $
--             newContinueDeploymentResponse
--
--         , responseBatchGetApplications $
--             newBatchGetApplicationsResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseUpdateApplication $
--             newUpdateApplicationResponse
--
--         , responseDeleteGitHubAccountToken $
--             newDeleteGitHubAccountTokenResponse
--
--         , responseDeregisterOnPremisesInstance $
--             newDeregisterOnPremisesInstanceResponse
--
--         , responsePutLifecycleEventHookExecutionStatus $
--             newPutLifecycleEventHookExecutionStatusResponse
--
--         , responseGetDeploymentTarget $
--             newGetDeploymentTargetResponse
--
--         , responseCreateApplication $
--             newCreateApplicationResponse
--
--         , responseBatchGetDeploymentTargets $
--             newBatchGetDeploymentTargetsResponse
--
--         , responseStopDeployment $
--             newStopDeploymentResponse
--
--         , responseListGitHubAccountTokenNames $
--             newListGitHubAccountTokenNamesResponse
--
--         , responseGetApplication $
--             newGetApplicationResponse
--
--         , responseListDeploymentGroups $
--             newListDeploymentGroupsResponse
--
--         , responseBatchGetOnPremisesInstances $
--             newBatchGetOnPremisesInstancesResponse
--
--         , responseRegisterOnPremisesInstance $
--             newRegisterOnPremisesInstanceResponse
--
--         , responseCreateDeploymentGroup $
--             newCreateDeploymentGroupResponse
--
--         , responseListDeploymentConfigs $
--             newListDeploymentConfigsResponse
--
--         , responseGetDeploymentGroup $
--             newGetDeploymentGroupResponse
--
--         , responseListDeployments $
--             newListDeploymentsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListApplicationRevisions $
--             newListApplicationRevisionsResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteResourcesByExternalId $
--             newDeleteResourcesByExternalIdResponse
--
--         , responseAddTagsToOnPremisesInstances $
--             newAddTagsToOnPremisesInstancesResponse
--
--         , responseListDeploymentTargets $
--             newListDeploymentTargetsResponse
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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromOnPremisesInstances)

responseBatchGetDeploymentGroups :: BatchGetDeploymentGroupsResponse -> TestTree
responseBatchGetDeploymentGroups =
  res
    "BatchGetDeploymentGroupsResponse"
    "fixture/BatchGetDeploymentGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetDeploymentGroups)

responseDeleteDeploymentGroup :: DeleteDeploymentGroupResponse -> TestTree
responseDeleteDeploymentGroup =
  res
    "DeleteDeploymentGroupResponse"
    "fixture/DeleteDeploymentGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeploymentGroup)

responseUpdateDeploymentGroup :: UpdateDeploymentGroupResponse -> TestTree
responseUpdateDeploymentGroup =
  res
    "UpdateDeploymentGroupResponse"
    "fixture/UpdateDeploymentGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeploymentGroup)

responseListOnPremisesInstances :: ListOnPremisesInstancesResponse -> TestTree
responseListOnPremisesInstances =
  res
    "ListOnPremisesInstancesResponse"
    "fixture/ListOnPremisesInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOnPremisesInstances)

responseCreateDeploymentConfig :: CreateDeploymentConfigResponse -> TestTree
responseCreateDeploymentConfig =
  res
    "CreateDeploymentConfigResponse"
    "fixture/CreateDeploymentConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeploymentConfig)

responseGetApplicationRevision :: GetApplicationRevisionResponse -> TestTree
responseGetApplicationRevision =
  res
    "GetApplicationRevisionResponse"
    "fixture/GetApplicationRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplicationRevision)

responseGetDeployment :: GetDeploymentResponse -> TestTree
responseGetDeployment =
  res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeployment)

responseDeleteDeploymentConfig :: DeleteDeploymentConfigResponse -> TestTree
responseDeleteDeploymentConfig =
  res
    "DeleteDeploymentConfigResponse"
    "fixture/DeleteDeploymentConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeploymentConfig)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseGetDeploymentConfig :: GetDeploymentConfigResponse -> TestTree
responseGetDeploymentConfig =
  res
    "GetDeploymentConfigResponse"
    "fixture/GetDeploymentConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeploymentConfig)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeployment)

responseBatchGetApplicationRevisions :: BatchGetApplicationRevisionsResponse -> TestTree
responseBatchGetApplicationRevisions =
  res
    "BatchGetApplicationRevisionsResponse"
    "fixture/BatchGetApplicationRevisionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetApplicationRevisions)

responseBatchGetDeployments :: BatchGetDeploymentsResponse -> TestTree
responseBatchGetDeployments =
  res
    "BatchGetDeploymentsResponse"
    "fixture/BatchGetDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetDeployments)

responseGetOnPremisesInstance :: GetOnPremisesInstanceResponse -> TestTree
responseGetOnPremisesInstance =
  res
    "GetOnPremisesInstanceResponse"
    "fixture/GetOnPremisesInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOnPremisesInstance)

responseRegisterApplicationRevision :: RegisterApplicationRevisionResponse -> TestTree
responseRegisterApplicationRevision =
  res
    "RegisterApplicationRevisionResponse"
    "fixture/RegisterApplicationRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterApplicationRevision)

responseContinueDeployment :: ContinueDeploymentResponse -> TestTree
responseContinueDeployment =
  res
    "ContinueDeploymentResponse"
    "fixture/ContinueDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ContinueDeployment)

responseBatchGetApplications :: BatchGetApplicationsResponse -> TestTree
responseBatchGetApplications =
  res
    "BatchGetApplicationsResponse"
    "fixture/BatchGetApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetApplications)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplication)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplication)

responseDeleteGitHubAccountToken :: DeleteGitHubAccountTokenResponse -> TestTree
responseDeleteGitHubAccountToken =
  res
    "DeleteGitHubAccountTokenResponse"
    "fixture/DeleteGitHubAccountTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGitHubAccountToken)

responseDeregisterOnPremisesInstance :: DeregisterOnPremisesInstanceResponse -> TestTree
responseDeregisterOnPremisesInstance =
  res
    "DeregisterOnPremisesInstanceResponse"
    "fixture/DeregisterOnPremisesInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterOnPremisesInstance)

responsePutLifecycleEventHookExecutionStatus :: PutLifecycleEventHookExecutionStatusResponse -> TestTree
responsePutLifecycleEventHookExecutionStatus =
  res
    "PutLifecycleEventHookExecutionStatusResponse"
    "fixture/PutLifecycleEventHookExecutionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLifecycleEventHookExecutionStatus)

responseGetDeploymentTarget :: GetDeploymentTargetResponse -> TestTree
responseGetDeploymentTarget =
  res
    "GetDeploymentTargetResponse"
    "fixture/GetDeploymentTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeploymentTarget)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplication)

responseBatchGetDeploymentTargets :: BatchGetDeploymentTargetsResponse -> TestTree
responseBatchGetDeploymentTargets =
  res
    "BatchGetDeploymentTargetsResponse"
    "fixture/BatchGetDeploymentTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetDeploymentTargets)

responseStopDeployment :: StopDeploymentResponse -> TestTree
responseStopDeployment =
  res
    "StopDeploymentResponse"
    "fixture/StopDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopDeployment)

responseListGitHubAccountTokenNames :: ListGitHubAccountTokenNamesResponse -> TestTree
responseListGitHubAccountTokenNames =
  res
    "ListGitHubAccountTokenNamesResponse"
    "fixture/ListGitHubAccountTokenNamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGitHubAccountTokenNames)

responseGetApplication :: GetApplicationResponse -> TestTree
responseGetApplication =
  res
    "GetApplicationResponse"
    "fixture/GetApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplication)

responseListDeploymentGroups :: ListDeploymentGroupsResponse -> TestTree
responseListDeploymentGroups =
  res
    "ListDeploymentGroupsResponse"
    "fixture/ListDeploymentGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeploymentGroups)

responseBatchGetOnPremisesInstances :: BatchGetOnPremisesInstancesResponse -> TestTree
responseBatchGetOnPremisesInstances =
  res
    "BatchGetOnPremisesInstancesResponse"
    "fixture/BatchGetOnPremisesInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetOnPremisesInstances)

responseRegisterOnPremisesInstance :: RegisterOnPremisesInstanceResponse -> TestTree
responseRegisterOnPremisesInstance =
  res
    "RegisterOnPremisesInstanceResponse"
    "fixture/RegisterOnPremisesInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterOnPremisesInstance)

responseCreateDeploymentGroup :: CreateDeploymentGroupResponse -> TestTree
responseCreateDeploymentGroup =
  res
    "CreateDeploymentGroupResponse"
    "fixture/CreateDeploymentGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeploymentGroup)

responseListDeploymentConfigs :: ListDeploymentConfigsResponse -> TestTree
responseListDeploymentConfigs =
  res
    "ListDeploymentConfigsResponse"
    "fixture/ListDeploymentConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeploymentConfigs)

responseGetDeploymentGroup :: GetDeploymentGroupResponse -> TestTree
responseGetDeploymentGroup =
  res
    "GetDeploymentGroupResponse"
    "fixture/GetDeploymentGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeploymentGroup)

responseListDeployments :: ListDeploymentsResponse -> TestTree
responseListDeployments =
  res
    "ListDeploymentsResponse"
    "fixture/ListDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeployments)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListApplicationRevisions :: ListApplicationRevisionsResponse -> TestTree
responseListApplicationRevisions =
  res
    "ListApplicationRevisionsResponse"
    "fixture/ListApplicationRevisionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplicationRevisions)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplications)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDeleteResourcesByExternalId :: DeleteResourcesByExternalIdResponse -> TestTree
responseDeleteResourcesByExternalId =
  res
    "DeleteResourcesByExternalIdResponse"
    "fixture/DeleteResourcesByExternalIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcesByExternalId)

responseAddTagsToOnPremisesInstances :: AddTagsToOnPremisesInstancesResponse -> TestTree
responseAddTagsToOnPremisesInstances =
  res
    "AddTagsToOnPremisesInstancesResponse"
    "fixture/AddTagsToOnPremisesInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToOnPremisesInstances)

responseListDeploymentTargets :: ListDeploymentTargetsResponse -> TestTree
responseListDeploymentTargets =
  res
    "ListDeploymentTargetsResponse"
    "fixture/ListDeploymentTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeploymentTargets)
