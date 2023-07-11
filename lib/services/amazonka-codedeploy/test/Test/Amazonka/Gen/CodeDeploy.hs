{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CodeDeploy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         [ requestAddTagsToOnPremisesInstances $
--             newAddTagsToOnPremisesInstances
--
--         , requestBatchGetApplicationRevisions $
--             newBatchGetApplicationRevisions
--
--         , requestBatchGetApplications $
--             newBatchGetApplications
--
--         , requestBatchGetDeploymentGroups $
--             newBatchGetDeploymentGroups
--
--         , requestBatchGetDeploymentTargets $
--             newBatchGetDeploymentTargets
--
--         , requestBatchGetDeployments $
--             newBatchGetDeployments
--
--         , requestBatchGetOnPremisesInstances $
--             newBatchGetOnPremisesInstances
--
--         , requestContinueDeployment $
--             newContinueDeployment
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestCreateDeploymentConfig $
--             newCreateDeploymentConfig
--
--         , requestCreateDeploymentGroup $
--             newCreateDeploymentGroup
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestDeleteDeploymentConfig $
--             newDeleteDeploymentConfig
--
--         , requestDeleteDeploymentGroup $
--             newDeleteDeploymentGroup
--
--         , requestDeleteGitHubAccountToken $
--             newDeleteGitHubAccountToken
--
--         , requestDeleteResourcesByExternalId $
--             newDeleteResourcesByExternalId
--
--         , requestDeregisterOnPremisesInstance $
--             newDeregisterOnPremisesInstance
--
--         , requestGetApplication $
--             newGetApplication
--
--         , requestGetApplicationRevision $
--             newGetApplicationRevision
--
--         , requestGetDeployment $
--             newGetDeployment
--
--         , requestGetDeploymentConfig $
--             newGetDeploymentConfig
--
--         , requestGetDeploymentGroup $
--             newGetDeploymentGroup
--
--         , requestGetDeploymentTarget $
--             newGetDeploymentTarget
--
--         , requestGetOnPremisesInstance $
--             newGetOnPremisesInstance
--
--         , requestListApplicationRevisions $
--             newListApplicationRevisions
--
--         , requestListApplications $
--             newListApplications
--
--         , requestListDeploymentConfigs $
--             newListDeploymentConfigs
--
--         , requestListDeploymentGroups $
--             newListDeploymentGroups
--
--         , requestListDeploymentTargets $
--             newListDeploymentTargets
--
--         , requestListDeployments $
--             newListDeployments
--
--         , requestListGitHubAccountTokenNames $
--             newListGitHubAccountTokenNames
--
--         , requestListOnPremisesInstances $
--             newListOnPremisesInstances
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutLifecycleEventHookExecutionStatus $
--             newPutLifecycleEventHookExecutionStatus
--
--         , requestRegisterApplicationRevision $
--             newRegisterApplicationRevision
--
--         , requestRegisterOnPremisesInstance $
--             newRegisterOnPremisesInstance
--
--         , requestRemoveTagsFromOnPremisesInstances $
--             newRemoveTagsFromOnPremisesInstances
--
--         , requestStopDeployment $
--             newStopDeployment
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestUpdateDeploymentGroup $
--             newUpdateDeploymentGroup
--
--           ]

--     , testGroup "response"
--         [ responseAddTagsToOnPremisesInstances $
--             newAddTagsToOnPremisesInstancesResponse
--
--         , responseBatchGetApplicationRevisions $
--             newBatchGetApplicationRevisionsResponse
--
--         , responseBatchGetApplications $
--             newBatchGetApplicationsResponse
--
--         , responseBatchGetDeploymentGroups $
--             newBatchGetDeploymentGroupsResponse
--
--         , responseBatchGetDeploymentTargets $
--             newBatchGetDeploymentTargetsResponse
--
--         , responseBatchGetDeployments $
--             newBatchGetDeploymentsResponse
--
--         , responseBatchGetOnPremisesInstances $
--             newBatchGetOnPremisesInstancesResponse
--
--         , responseContinueDeployment $
--             newContinueDeploymentResponse
--
--         , responseCreateApplication $
--             newCreateApplicationResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseCreateDeploymentConfig $
--             newCreateDeploymentConfigResponse
--
--         , responseCreateDeploymentGroup $
--             newCreateDeploymentGroupResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseDeleteDeploymentConfig $
--             newDeleteDeploymentConfigResponse
--
--         , responseDeleteDeploymentGroup $
--             newDeleteDeploymentGroupResponse
--
--         , responseDeleteGitHubAccountToken $
--             newDeleteGitHubAccountTokenResponse
--
--         , responseDeleteResourcesByExternalId $
--             newDeleteResourcesByExternalIdResponse
--
--         , responseDeregisterOnPremisesInstance $
--             newDeregisterOnPremisesInstanceResponse
--
--         , responseGetApplication $
--             newGetApplicationResponse
--
--         , responseGetApplicationRevision $
--             newGetApplicationRevisionResponse
--
--         , responseGetDeployment $
--             newGetDeploymentResponse
--
--         , responseGetDeploymentConfig $
--             newGetDeploymentConfigResponse
--
--         , responseGetDeploymentGroup $
--             newGetDeploymentGroupResponse
--
--         , responseGetDeploymentTarget $
--             newGetDeploymentTargetResponse
--
--         , responseGetOnPremisesInstance $
--             newGetOnPremisesInstanceResponse
--
--         , responseListApplicationRevisions $
--             newListApplicationRevisionsResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseListDeploymentConfigs $
--             newListDeploymentConfigsResponse
--
--         , responseListDeploymentGroups $
--             newListDeploymentGroupsResponse
--
--         , responseListDeploymentTargets $
--             newListDeploymentTargetsResponse
--
--         , responseListDeployments $
--             newListDeploymentsResponse
--
--         , responseListGitHubAccountTokenNames $
--             newListGitHubAccountTokenNamesResponse
--
--         , responseListOnPremisesInstances $
--             newListOnPremisesInstancesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutLifecycleEventHookExecutionStatus $
--             newPutLifecycleEventHookExecutionStatusResponse
--
--         , responseRegisterApplicationRevision $
--             newRegisterApplicationRevisionResponse
--
--         , responseRegisterOnPremisesInstance $
--             newRegisterOnPremisesInstanceResponse
--
--         , responseRemoveTagsFromOnPremisesInstances $
--             newRemoveTagsFromOnPremisesInstancesResponse
--
--         , responseStopDeployment $
--             newStopDeploymentResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApplication $
--             newUpdateApplicationResponse
--
--         , responseUpdateDeploymentGroup $
--             newUpdateDeploymentGroupResponse
--
--           ]
--     ]

-- Requests

requestAddTagsToOnPremisesInstances :: AddTagsToOnPremisesInstances -> TestTree
requestAddTagsToOnPremisesInstances =
  req
    "AddTagsToOnPremisesInstances"
    "fixture/AddTagsToOnPremisesInstances.yaml"

requestBatchGetApplicationRevisions :: BatchGetApplicationRevisions -> TestTree
requestBatchGetApplicationRevisions =
  req
    "BatchGetApplicationRevisions"
    "fixture/BatchGetApplicationRevisions.yaml"

requestBatchGetApplications :: BatchGetApplications -> TestTree
requestBatchGetApplications =
  req
    "BatchGetApplications"
    "fixture/BatchGetApplications.yaml"

requestBatchGetDeploymentGroups :: BatchGetDeploymentGroups -> TestTree
requestBatchGetDeploymentGroups =
  req
    "BatchGetDeploymentGroups"
    "fixture/BatchGetDeploymentGroups.yaml"

requestBatchGetDeploymentTargets :: BatchGetDeploymentTargets -> TestTree
requestBatchGetDeploymentTargets =
  req
    "BatchGetDeploymentTargets"
    "fixture/BatchGetDeploymentTargets.yaml"

requestBatchGetDeployments :: BatchGetDeployments -> TestTree
requestBatchGetDeployments =
  req
    "BatchGetDeployments"
    "fixture/BatchGetDeployments.yaml"

requestBatchGetOnPremisesInstances :: BatchGetOnPremisesInstances -> TestTree
requestBatchGetOnPremisesInstances =
  req
    "BatchGetOnPremisesInstances"
    "fixture/BatchGetOnPremisesInstances.yaml"

requestContinueDeployment :: ContinueDeployment -> TestTree
requestContinueDeployment =
  req
    "ContinueDeployment"
    "fixture/ContinueDeployment.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestCreateDeploymentConfig :: CreateDeploymentConfig -> TestTree
requestCreateDeploymentConfig =
  req
    "CreateDeploymentConfig"
    "fixture/CreateDeploymentConfig.yaml"

requestCreateDeploymentGroup :: CreateDeploymentGroup -> TestTree
requestCreateDeploymentGroup =
  req
    "CreateDeploymentGroup"
    "fixture/CreateDeploymentGroup.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestDeleteDeploymentConfig :: DeleteDeploymentConfig -> TestTree
requestDeleteDeploymentConfig =
  req
    "DeleteDeploymentConfig"
    "fixture/DeleteDeploymentConfig.yaml"

requestDeleteDeploymentGroup :: DeleteDeploymentGroup -> TestTree
requestDeleteDeploymentGroup =
  req
    "DeleteDeploymentGroup"
    "fixture/DeleteDeploymentGroup.yaml"

requestDeleteGitHubAccountToken :: DeleteGitHubAccountToken -> TestTree
requestDeleteGitHubAccountToken =
  req
    "DeleteGitHubAccountToken"
    "fixture/DeleteGitHubAccountToken.yaml"

requestDeleteResourcesByExternalId :: DeleteResourcesByExternalId -> TestTree
requestDeleteResourcesByExternalId =
  req
    "DeleteResourcesByExternalId"
    "fixture/DeleteResourcesByExternalId.yaml"

requestDeregisterOnPremisesInstance :: DeregisterOnPremisesInstance -> TestTree
requestDeregisterOnPremisesInstance =
  req
    "DeregisterOnPremisesInstance"
    "fixture/DeregisterOnPremisesInstance.yaml"

requestGetApplication :: GetApplication -> TestTree
requestGetApplication =
  req
    "GetApplication"
    "fixture/GetApplication.yaml"

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

requestGetDeploymentConfig :: GetDeploymentConfig -> TestTree
requestGetDeploymentConfig =
  req
    "GetDeploymentConfig"
    "fixture/GetDeploymentConfig.yaml"

requestGetDeploymentGroup :: GetDeploymentGroup -> TestTree
requestGetDeploymentGroup =
  req
    "GetDeploymentGroup"
    "fixture/GetDeploymentGroup.yaml"

requestGetDeploymentTarget :: GetDeploymentTarget -> TestTree
requestGetDeploymentTarget =
  req
    "GetDeploymentTarget"
    "fixture/GetDeploymentTarget.yaml"

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

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestListDeploymentConfigs :: ListDeploymentConfigs -> TestTree
requestListDeploymentConfigs =
  req
    "ListDeploymentConfigs"
    "fixture/ListDeploymentConfigs.yaml"

requestListDeploymentGroups :: ListDeploymentGroups -> TestTree
requestListDeploymentGroups =
  req
    "ListDeploymentGroups"
    "fixture/ListDeploymentGroups.yaml"

requestListDeploymentTargets :: ListDeploymentTargets -> TestTree
requestListDeploymentTargets =
  req
    "ListDeploymentTargets"
    "fixture/ListDeploymentTargets.yaml"

requestListDeployments :: ListDeployments -> TestTree
requestListDeployments =
  req
    "ListDeployments"
    "fixture/ListDeployments.yaml"

requestListGitHubAccountTokenNames :: ListGitHubAccountTokenNames -> TestTree
requestListGitHubAccountTokenNames =
  req
    "ListGitHubAccountTokenNames"
    "fixture/ListGitHubAccountTokenNames.yaml"

requestListOnPremisesInstances :: ListOnPremisesInstances -> TestTree
requestListOnPremisesInstances =
  req
    "ListOnPremisesInstances"
    "fixture/ListOnPremisesInstances.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutLifecycleEventHookExecutionStatus :: PutLifecycleEventHookExecutionStatus -> TestTree
requestPutLifecycleEventHookExecutionStatus =
  req
    "PutLifecycleEventHookExecutionStatus"
    "fixture/PutLifecycleEventHookExecutionStatus.yaml"

requestRegisterApplicationRevision :: RegisterApplicationRevision -> TestTree
requestRegisterApplicationRevision =
  req
    "RegisterApplicationRevision"
    "fixture/RegisterApplicationRevision.yaml"

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

requestStopDeployment :: StopDeployment -> TestTree
requestStopDeployment =
  req
    "StopDeployment"
    "fixture/StopDeployment.yaml"

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

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestUpdateDeploymentGroup :: UpdateDeploymentGroup -> TestTree
requestUpdateDeploymentGroup =
  req
    "UpdateDeploymentGroup"
    "fixture/UpdateDeploymentGroup.yaml"

-- Responses

responseAddTagsToOnPremisesInstances :: AddTagsToOnPremisesInstancesResponse -> TestTree
responseAddTagsToOnPremisesInstances =
  res
    "AddTagsToOnPremisesInstancesResponse"
    "fixture/AddTagsToOnPremisesInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTagsToOnPremisesInstances)

responseBatchGetApplicationRevisions :: BatchGetApplicationRevisionsResponse -> TestTree
responseBatchGetApplicationRevisions =
  res
    "BatchGetApplicationRevisionsResponse"
    "fixture/BatchGetApplicationRevisionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetApplicationRevisions)

responseBatchGetApplications :: BatchGetApplicationsResponse -> TestTree
responseBatchGetApplications =
  res
    "BatchGetApplicationsResponse"
    "fixture/BatchGetApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetApplications)

responseBatchGetDeploymentGroups :: BatchGetDeploymentGroupsResponse -> TestTree
responseBatchGetDeploymentGroups =
  res
    "BatchGetDeploymentGroupsResponse"
    "fixture/BatchGetDeploymentGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetDeploymentGroups)

responseBatchGetDeploymentTargets :: BatchGetDeploymentTargetsResponse -> TestTree
responseBatchGetDeploymentTargets =
  res
    "BatchGetDeploymentTargetsResponse"
    "fixture/BatchGetDeploymentTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetDeploymentTargets)

responseBatchGetDeployments :: BatchGetDeploymentsResponse -> TestTree
responseBatchGetDeployments =
  res
    "BatchGetDeploymentsResponse"
    "fixture/BatchGetDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetDeployments)

responseBatchGetOnPremisesInstances :: BatchGetOnPremisesInstancesResponse -> TestTree
responseBatchGetOnPremisesInstances =
  res
    "BatchGetOnPremisesInstancesResponse"
    "fixture/BatchGetOnPremisesInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetOnPremisesInstances)

responseContinueDeployment :: ContinueDeploymentResponse -> TestTree
responseContinueDeployment =
  res
    "ContinueDeploymentResponse"
    "fixture/ContinueDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ContinueDeployment)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplication)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeployment)

responseCreateDeploymentConfig :: CreateDeploymentConfigResponse -> TestTree
responseCreateDeploymentConfig =
  res
    "CreateDeploymentConfigResponse"
    "fixture/CreateDeploymentConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeploymentConfig)

responseCreateDeploymentGroup :: CreateDeploymentGroupResponse -> TestTree
responseCreateDeploymentGroup =
  res
    "CreateDeploymentGroupResponse"
    "fixture/CreateDeploymentGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeploymentGroup)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplication)

responseDeleteDeploymentConfig :: DeleteDeploymentConfigResponse -> TestTree
responseDeleteDeploymentConfig =
  res
    "DeleteDeploymentConfigResponse"
    "fixture/DeleteDeploymentConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeploymentConfig)

responseDeleteDeploymentGroup :: DeleteDeploymentGroupResponse -> TestTree
responseDeleteDeploymentGroup =
  res
    "DeleteDeploymentGroupResponse"
    "fixture/DeleteDeploymentGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeploymentGroup)

responseDeleteGitHubAccountToken :: DeleteGitHubAccountTokenResponse -> TestTree
responseDeleteGitHubAccountToken =
  res
    "DeleteGitHubAccountTokenResponse"
    "fixture/DeleteGitHubAccountTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGitHubAccountToken)

responseDeleteResourcesByExternalId :: DeleteResourcesByExternalIdResponse -> TestTree
responseDeleteResourcesByExternalId =
  res
    "DeleteResourcesByExternalIdResponse"
    "fixture/DeleteResourcesByExternalIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcesByExternalId)

responseDeregisterOnPremisesInstance :: DeregisterOnPremisesInstanceResponse -> TestTree
responseDeregisterOnPremisesInstance =
  res
    "DeregisterOnPremisesInstanceResponse"
    "fixture/DeregisterOnPremisesInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterOnPremisesInstance)

responseGetApplication :: GetApplicationResponse -> TestTree
responseGetApplication =
  res
    "GetApplicationResponse"
    "fixture/GetApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplication)

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

responseGetDeploymentConfig :: GetDeploymentConfigResponse -> TestTree
responseGetDeploymentConfig =
  res
    "GetDeploymentConfigResponse"
    "fixture/GetDeploymentConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeploymentConfig)

responseGetDeploymentGroup :: GetDeploymentGroupResponse -> TestTree
responseGetDeploymentGroup =
  res
    "GetDeploymentGroupResponse"
    "fixture/GetDeploymentGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeploymentGroup)

responseGetDeploymentTarget :: GetDeploymentTargetResponse -> TestTree
responseGetDeploymentTarget =
  res
    "GetDeploymentTargetResponse"
    "fixture/GetDeploymentTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeploymentTarget)

responseGetOnPremisesInstance :: GetOnPremisesInstanceResponse -> TestTree
responseGetOnPremisesInstance =
  res
    "GetOnPremisesInstanceResponse"
    "fixture/GetOnPremisesInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOnPremisesInstance)

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

responseListDeploymentConfigs :: ListDeploymentConfigsResponse -> TestTree
responseListDeploymentConfigs =
  res
    "ListDeploymentConfigsResponse"
    "fixture/ListDeploymentConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeploymentConfigs)

responseListDeploymentGroups :: ListDeploymentGroupsResponse -> TestTree
responseListDeploymentGroups =
  res
    "ListDeploymentGroupsResponse"
    "fixture/ListDeploymentGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeploymentGroups)

responseListDeploymentTargets :: ListDeploymentTargetsResponse -> TestTree
responseListDeploymentTargets =
  res
    "ListDeploymentTargetsResponse"
    "fixture/ListDeploymentTargetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeploymentTargets)

responseListDeployments :: ListDeploymentsResponse -> TestTree
responseListDeployments =
  res
    "ListDeploymentsResponse"
    "fixture/ListDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeployments)

responseListGitHubAccountTokenNames :: ListGitHubAccountTokenNamesResponse -> TestTree
responseListGitHubAccountTokenNames =
  res
    "ListGitHubAccountTokenNamesResponse"
    "fixture/ListGitHubAccountTokenNamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGitHubAccountTokenNames)

responseListOnPremisesInstances :: ListOnPremisesInstancesResponse -> TestTree
responseListOnPremisesInstances =
  res
    "ListOnPremisesInstancesResponse"
    "fixture/ListOnPremisesInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOnPremisesInstances)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutLifecycleEventHookExecutionStatus :: PutLifecycleEventHookExecutionStatusResponse -> TestTree
responsePutLifecycleEventHookExecutionStatus =
  res
    "PutLifecycleEventHookExecutionStatusResponse"
    "fixture/PutLifecycleEventHookExecutionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLifecycleEventHookExecutionStatus)

responseRegisterApplicationRevision :: RegisterApplicationRevisionResponse -> TestTree
responseRegisterApplicationRevision =
  res
    "RegisterApplicationRevisionResponse"
    "fixture/RegisterApplicationRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterApplicationRevision)

responseRegisterOnPremisesInstance :: RegisterOnPremisesInstanceResponse -> TestTree
responseRegisterOnPremisesInstance =
  res
    "RegisterOnPremisesInstanceResponse"
    "fixture/RegisterOnPremisesInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterOnPremisesInstance)

responseRemoveTagsFromOnPremisesInstances :: RemoveTagsFromOnPremisesInstancesResponse -> TestTree
responseRemoveTagsFromOnPremisesInstances =
  res
    "RemoveTagsFromOnPremisesInstancesResponse"
    "fixture/RemoveTagsFromOnPremisesInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTagsFromOnPremisesInstances)

responseStopDeployment :: StopDeploymentResponse -> TestTree
responseStopDeployment =
  res
    "StopDeploymentResponse"
    "fixture/StopDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopDeployment)

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

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplication)

responseUpdateDeploymentGroup :: UpdateDeploymentGroupResponse -> TestTree
responseUpdateDeploymentGroup =
  res
    "UpdateDeploymentGroupResponse"
    "fixture/UpdateDeploymentGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeploymentGroup)
