{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeDeploy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CodeDeploy where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CodeDeploy
import Test.AWS.CodeDeploy.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testRemoveTagsFromOnPremisesInstances $
--             removeTagsFromOnPremisesInstances
--
--         , testDeleteDeploymentGroup $
--             deleteDeploymentGroup
--
--         , testUpdateDeploymentGroup $
--             updateDeploymentGroup
--
--         , testListOnPremisesInstances $
--             listOnPremisesInstances
--
--         , testCreateDeploymentConfig $
--             createDeploymentConfig
--
--         , testGetApplicationRevision $
--             getApplicationRevision
--
--         , testGetDeployment $
--             getDeployment
--
--         , testDeleteDeploymentConfig $
--             deleteDeploymentConfig
--
--         , testGetDeploymentConfig $
--             getDeploymentConfig
--
--         , testCreateDeployment $
--             createDeployment
--
--         , testBatchGetDeployments $
--             batchGetDeployments
--
--         , testGetOnPremisesInstance $
--             getOnPremisesInstance
--
--         , testRegisterApplicationRevision $
--             registerApplicationRevision
--
--         , testBatchGetApplications $
--             batchGetApplications
--
--         , testDeleteApplication $
--             deleteApplication
--
--         , testUpdateApplication $
--             updateApplication
--
--         , testGetDeploymentInstance $
--             getDeploymentInstance
--
--         , testDeregisterOnPremisesInstance $
--             deregisterOnPremisesInstance
--
--         , testCreateApplication $
--             createApplication
--
--         , testStopDeployment $
--             stopDeployment
--
--         , testGetApplication $
--             getApplication
--
--         , testListDeploymentGroups $
--             listDeploymentGroups
--
--         , testBatchGetOnPremisesInstances $
--             batchGetOnPremisesInstances
--
--         , testRegisterOnPremisesInstance $
--             registerOnPremisesInstance
--
--         , testCreateDeploymentGroup $
--             createDeploymentGroup
--
--         , testListDeploymentConfigs $
--             listDeploymentConfigs
--
--         , testGetDeploymentGroup $
--             getDeploymentGroup
--
--         , testListDeployments $
--             listDeployments
--
--         , testListApplicationRevisions $
--             listApplicationRevisions
--
--         , testListApplications $
--             listApplications
--
--         , testAddTagsToOnPremisesInstances $
--             addTagsToOnPremisesInstances
--
--         , testListDeploymentInstances $
--             listDeploymentInstances
--
--           ]

--     , testGroup "response"
--         [ testRemoveTagsFromOnPremisesInstancesResponse $
--             removeTagsFromOnPremisesInstancesResponse
--
--         , testDeleteDeploymentGroupResponse $
--             deleteDeploymentGroupResponse
--
--         , testUpdateDeploymentGroupResponse $
--             updateDeploymentGroupResponse
--
--         , testListOnPremisesInstancesResponse $
--             listOnPremisesInstancesResponse
--
--         , testCreateDeploymentConfigResponse $
--             createDeploymentConfigResponse
--
--         , testGetApplicationRevisionResponse $
--             getApplicationRevisionResponse
--
--         , testGetDeploymentResponse $
--             getDeploymentResponse
--
--         , testDeleteDeploymentConfigResponse $
--             deleteDeploymentConfigResponse
--
--         , testGetDeploymentConfigResponse $
--             getDeploymentConfigResponse
--
--         , testCreateDeploymentResponse $
--             createDeploymentResponse
--
--         , testBatchGetDeploymentsResponse $
--             batchGetDeploymentsResponse
--
--         , testGetOnPremisesInstanceResponse $
--             getOnPremisesInstanceResponse
--
--         , testRegisterApplicationRevisionResponse $
--             registerApplicationRevisionResponse
--
--         , testBatchGetApplicationsResponse $
--             batchGetApplicationsResponse
--
--         , testDeleteApplicationResponse $
--             deleteApplicationResponse
--
--         , testUpdateApplicationResponse $
--             updateApplicationResponse
--
--         , testGetDeploymentInstanceResponse $
--             getDeploymentInstanceResponse
--
--         , testDeregisterOnPremisesInstanceResponse $
--             deregisterOnPremisesInstanceResponse
--
--         , testCreateApplicationResponse $
--             createApplicationResponse
--
--         , testStopDeploymentResponse $
--             stopDeploymentResponse
--
--         , testGetApplicationResponse $
--             getApplicationResponse
--
--         , testListDeploymentGroupsResponse $
--             listDeploymentGroupsResponse
--
--         , testBatchGetOnPremisesInstancesResponse $
--             batchGetOnPremisesInstancesResponse
--
--         , testRegisterOnPremisesInstanceResponse $
--             registerOnPremisesInstanceResponse
--
--         , testCreateDeploymentGroupResponse $
--             createDeploymentGroupResponse
--
--         , testListDeploymentConfigsResponse $
--             listDeploymentConfigsResponse
--
--         , testGetDeploymentGroupResponse $
--             getDeploymentGroupResponse
--
--         , testListDeploymentsResponse $
--             listDeploymentsResponse
--
--         , testListApplicationRevisionsResponse $
--             listApplicationRevisionsResponse
--
--         , testListApplicationsResponse $
--             listApplicationsResponse
--
--         , testAddTagsToOnPremisesInstancesResponse $
--             addTagsToOnPremisesInstancesResponse
--
--         , testListDeploymentInstancesResponse $
--             listDeploymentInstancesResponse
--
--           ]
--     ]

-- Requests

testRemoveTagsFromOnPremisesInstances :: RemoveTagsFromOnPremisesInstances -> TestTree
testRemoveTagsFromOnPremisesInstances = req
    "RemoveTagsFromOnPremisesInstances"
    "fixture/RemoveTagsFromOnPremisesInstances.yaml"

testDeleteDeploymentGroup :: DeleteDeploymentGroup -> TestTree
testDeleteDeploymentGroup = req
    "DeleteDeploymentGroup"
    "fixture/DeleteDeploymentGroup.yaml"

testUpdateDeploymentGroup :: UpdateDeploymentGroup -> TestTree
testUpdateDeploymentGroup = req
    "UpdateDeploymentGroup"
    "fixture/UpdateDeploymentGroup.yaml"

testListOnPremisesInstances :: ListOnPremisesInstances -> TestTree
testListOnPremisesInstances = req
    "ListOnPremisesInstances"
    "fixture/ListOnPremisesInstances.yaml"

testCreateDeploymentConfig :: CreateDeploymentConfig -> TestTree
testCreateDeploymentConfig = req
    "CreateDeploymentConfig"
    "fixture/CreateDeploymentConfig.yaml"

testGetApplicationRevision :: GetApplicationRevision -> TestTree
testGetApplicationRevision = req
    "GetApplicationRevision"
    "fixture/GetApplicationRevision.yaml"

testGetDeployment :: GetDeployment -> TestTree
testGetDeployment = req
    "GetDeployment"
    "fixture/GetDeployment.yaml"

testDeleteDeploymentConfig :: DeleteDeploymentConfig -> TestTree
testDeleteDeploymentConfig = req
    "DeleteDeploymentConfig"
    "fixture/DeleteDeploymentConfig.yaml"

testGetDeploymentConfig :: GetDeploymentConfig -> TestTree
testGetDeploymentConfig = req
    "GetDeploymentConfig"
    "fixture/GetDeploymentConfig.yaml"

testCreateDeployment :: CreateDeployment -> TestTree
testCreateDeployment = req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

testBatchGetDeployments :: BatchGetDeployments -> TestTree
testBatchGetDeployments = req
    "BatchGetDeployments"
    "fixture/BatchGetDeployments.yaml"

testGetOnPremisesInstance :: GetOnPremisesInstance -> TestTree
testGetOnPremisesInstance = req
    "GetOnPremisesInstance"
    "fixture/GetOnPremisesInstance.yaml"

testRegisterApplicationRevision :: RegisterApplicationRevision -> TestTree
testRegisterApplicationRevision = req
    "RegisterApplicationRevision"
    "fixture/RegisterApplicationRevision.yaml"

testBatchGetApplications :: BatchGetApplications -> TestTree
testBatchGetApplications = req
    "BatchGetApplications"
    "fixture/BatchGetApplications.yaml"

testDeleteApplication :: DeleteApplication -> TestTree
testDeleteApplication = req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

testUpdateApplication :: UpdateApplication -> TestTree
testUpdateApplication = req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

testGetDeploymentInstance :: GetDeploymentInstance -> TestTree
testGetDeploymentInstance = req
    "GetDeploymentInstance"
    "fixture/GetDeploymentInstance.yaml"

testDeregisterOnPremisesInstance :: DeregisterOnPremisesInstance -> TestTree
testDeregisterOnPremisesInstance = req
    "DeregisterOnPremisesInstance"
    "fixture/DeregisterOnPremisesInstance.yaml"

testCreateApplication :: CreateApplication -> TestTree
testCreateApplication = req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

testStopDeployment :: StopDeployment -> TestTree
testStopDeployment = req
    "StopDeployment"
    "fixture/StopDeployment.yaml"

testGetApplication :: GetApplication -> TestTree
testGetApplication = req
    "GetApplication"
    "fixture/GetApplication.yaml"

testListDeploymentGroups :: ListDeploymentGroups -> TestTree
testListDeploymentGroups = req
    "ListDeploymentGroups"
    "fixture/ListDeploymentGroups.yaml"

testBatchGetOnPremisesInstances :: BatchGetOnPremisesInstances -> TestTree
testBatchGetOnPremisesInstances = req
    "BatchGetOnPremisesInstances"
    "fixture/BatchGetOnPremisesInstances.yaml"

testRegisterOnPremisesInstance :: RegisterOnPremisesInstance -> TestTree
testRegisterOnPremisesInstance = req
    "RegisterOnPremisesInstance"
    "fixture/RegisterOnPremisesInstance.yaml"

testCreateDeploymentGroup :: CreateDeploymentGroup -> TestTree
testCreateDeploymentGroup = req
    "CreateDeploymentGroup"
    "fixture/CreateDeploymentGroup.yaml"

testListDeploymentConfigs :: ListDeploymentConfigs -> TestTree
testListDeploymentConfigs = req
    "ListDeploymentConfigs"
    "fixture/ListDeploymentConfigs.yaml"

testGetDeploymentGroup :: GetDeploymentGroup -> TestTree
testGetDeploymentGroup = req
    "GetDeploymentGroup"
    "fixture/GetDeploymentGroup.yaml"

testListDeployments :: ListDeployments -> TestTree
testListDeployments = req
    "ListDeployments"
    "fixture/ListDeployments.yaml"

testListApplicationRevisions :: ListApplicationRevisions -> TestTree
testListApplicationRevisions = req
    "ListApplicationRevisions"
    "fixture/ListApplicationRevisions.yaml"

testListApplications :: ListApplications -> TestTree
testListApplications = req
    "ListApplications"
    "fixture/ListApplications.yaml"

testAddTagsToOnPremisesInstances :: AddTagsToOnPremisesInstances -> TestTree
testAddTagsToOnPremisesInstances = req
    "AddTagsToOnPremisesInstances"
    "fixture/AddTagsToOnPremisesInstances.yaml"

testListDeploymentInstances :: ListDeploymentInstances -> TestTree
testListDeploymentInstances = req
    "ListDeploymentInstances"
    "fixture/ListDeploymentInstances.yaml"

-- Responses

testRemoveTagsFromOnPremisesInstancesResponse :: RemoveTagsFromOnPremisesInstancesResponse -> TestTree
testRemoveTagsFromOnPremisesInstancesResponse = res
    "RemoveTagsFromOnPremisesInstancesResponse"
    "fixture/RemoveTagsFromOnPremisesInstancesResponse.proto"
    codeDeploy
    (Proxy :: Proxy RemoveTagsFromOnPremisesInstances)

testDeleteDeploymentGroupResponse :: DeleteDeploymentGroupResponse -> TestTree
testDeleteDeploymentGroupResponse = res
    "DeleteDeploymentGroupResponse"
    "fixture/DeleteDeploymentGroupResponse.proto"
    codeDeploy
    (Proxy :: Proxy DeleteDeploymentGroup)

testUpdateDeploymentGroupResponse :: UpdateDeploymentGroupResponse -> TestTree
testUpdateDeploymentGroupResponse = res
    "UpdateDeploymentGroupResponse"
    "fixture/UpdateDeploymentGroupResponse.proto"
    codeDeploy
    (Proxy :: Proxy UpdateDeploymentGroup)

testListOnPremisesInstancesResponse :: ListOnPremisesInstancesResponse -> TestTree
testListOnPremisesInstancesResponse = res
    "ListOnPremisesInstancesResponse"
    "fixture/ListOnPremisesInstancesResponse.proto"
    codeDeploy
    (Proxy :: Proxy ListOnPremisesInstances)

testCreateDeploymentConfigResponse :: CreateDeploymentConfigResponse -> TestTree
testCreateDeploymentConfigResponse = res
    "CreateDeploymentConfigResponse"
    "fixture/CreateDeploymentConfigResponse.proto"
    codeDeploy
    (Proxy :: Proxy CreateDeploymentConfig)

testGetApplicationRevisionResponse :: GetApplicationRevisionResponse -> TestTree
testGetApplicationRevisionResponse = res
    "GetApplicationRevisionResponse"
    "fixture/GetApplicationRevisionResponse.proto"
    codeDeploy
    (Proxy :: Proxy GetApplicationRevision)

testGetDeploymentResponse :: GetDeploymentResponse -> TestTree
testGetDeploymentResponse = res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    codeDeploy
    (Proxy :: Proxy GetDeployment)

testDeleteDeploymentConfigResponse :: DeleteDeploymentConfigResponse -> TestTree
testDeleteDeploymentConfigResponse = res
    "DeleteDeploymentConfigResponse"
    "fixture/DeleteDeploymentConfigResponse.proto"
    codeDeploy
    (Proxy :: Proxy DeleteDeploymentConfig)

testGetDeploymentConfigResponse :: GetDeploymentConfigResponse -> TestTree
testGetDeploymentConfigResponse = res
    "GetDeploymentConfigResponse"
    "fixture/GetDeploymentConfigResponse.proto"
    codeDeploy
    (Proxy :: Proxy GetDeploymentConfig)

testCreateDeploymentResponse :: CreateDeploymentResponse -> TestTree
testCreateDeploymentResponse = res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    codeDeploy
    (Proxy :: Proxy CreateDeployment)

testBatchGetDeploymentsResponse :: BatchGetDeploymentsResponse -> TestTree
testBatchGetDeploymentsResponse = res
    "BatchGetDeploymentsResponse"
    "fixture/BatchGetDeploymentsResponse.proto"
    codeDeploy
    (Proxy :: Proxy BatchGetDeployments)

testGetOnPremisesInstanceResponse :: GetOnPremisesInstanceResponse -> TestTree
testGetOnPremisesInstanceResponse = res
    "GetOnPremisesInstanceResponse"
    "fixture/GetOnPremisesInstanceResponse.proto"
    codeDeploy
    (Proxy :: Proxy GetOnPremisesInstance)

testRegisterApplicationRevisionResponse :: RegisterApplicationRevisionResponse -> TestTree
testRegisterApplicationRevisionResponse = res
    "RegisterApplicationRevisionResponse"
    "fixture/RegisterApplicationRevisionResponse.proto"
    codeDeploy
    (Proxy :: Proxy RegisterApplicationRevision)

testBatchGetApplicationsResponse :: BatchGetApplicationsResponse -> TestTree
testBatchGetApplicationsResponse = res
    "BatchGetApplicationsResponse"
    "fixture/BatchGetApplicationsResponse.proto"
    codeDeploy
    (Proxy :: Proxy BatchGetApplications)

testDeleteApplicationResponse :: DeleteApplicationResponse -> TestTree
testDeleteApplicationResponse = res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    codeDeploy
    (Proxy :: Proxy DeleteApplication)

testUpdateApplicationResponse :: UpdateApplicationResponse -> TestTree
testUpdateApplicationResponse = res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    codeDeploy
    (Proxy :: Proxy UpdateApplication)

testGetDeploymentInstanceResponse :: GetDeploymentInstanceResponse -> TestTree
testGetDeploymentInstanceResponse = res
    "GetDeploymentInstanceResponse"
    "fixture/GetDeploymentInstanceResponse.proto"
    codeDeploy
    (Proxy :: Proxy GetDeploymentInstance)

testDeregisterOnPremisesInstanceResponse :: DeregisterOnPremisesInstanceResponse -> TestTree
testDeregisterOnPremisesInstanceResponse = res
    "DeregisterOnPremisesInstanceResponse"
    "fixture/DeregisterOnPremisesInstanceResponse.proto"
    codeDeploy
    (Proxy :: Proxy DeregisterOnPremisesInstance)

testCreateApplicationResponse :: CreateApplicationResponse -> TestTree
testCreateApplicationResponse = res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    codeDeploy
    (Proxy :: Proxy CreateApplication)

testStopDeploymentResponse :: StopDeploymentResponse -> TestTree
testStopDeploymentResponse = res
    "StopDeploymentResponse"
    "fixture/StopDeploymentResponse.proto"
    codeDeploy
    (Proxy :: Proxy StopDeployment)

testGetApplicationResponse :: GetApplicationResponse -> TestTree
testGetApplicationResponse = res
    "GetApplicationResponse"
    "fixture/GetApplicationResponse.proto"
    codeDeploy
    (Proxy :: Proxy GetApplication)

testListDeploymentGroupsResponse :: ListDeploymentGroupsResponse -> TestTree
testListDeploymentGroupsResponse = res
    "ListDeploymentGroupsResponse"
    "fixture/ListDeploymentGroupsResponse.proto"
    codeDeploy
    (Proxy :: Proxy ListDeploymentGroups)

testBatchGetOnPremisesInstancesResponse :: BatchGetOnPremisesInstancesResponse -> TestTree
testBatchGetOnPremisesInstancesResponse = res
    "BatchGetOnPremisesInstancesResponse"
    "fixture/BatchGetOnPremisesInstancesResponse.proto"
    codeDeploy
    (Proxy :: Proxy BatchGetOnPremisesInstances)

testRegisterOnPremisesInstanceResponse :: RegisterOnPremisesInstanceResponse -> TestTree
testRegisterOnPremisesInstanceResponse = res
    "RegisterOnPremisesInstanceResponse"
    "fixture/RegisterOnPremisesInstanceResponse.proto"
    codeDeploy
    (Proxy :: Proxy RegisterOnPremisesInstance)

testCreateDeploymentGroupResponse :: CreateDeploymentGroupResponse -> TestTree
testCreateDeploymentGroupResponse = res
    "CreateDeploymentGroupResponse"
    "fixture/CreateDeploymentGroupResponse.proto"
    codeDeploy
    (Proxy :: Proxy CreateDeploymentGroup)

testListDeploymentConfigsResponse :: ListDeploymentConfigsResponse -> TestTree
testListDeploymentConfigsResponse = res
    "ListDeploymentConfigsResponse"
    "fixture/ListDeploymentConfigsResponse.proto"
    codeDeploy
    (Proxy :: Proxy ListDeploymentConfigs)

testGetDeploymentGroupResponse :: GetDeploymentGroupResponse -> TestTree
testGetDeploymentGroupResponse = res
    "GetDeploymentGroupResponse"
    "fixture/GetDeploymentGroupResponse.proto"
    codeDeploy
    (Proxy :: Proxy GetDeploymentGroup)

testListDeploymentsResponse :: ListDeploymentsResponse -> TestTree
testListDeploymentsResponse = res
    "ListDeploymentsResponse"
    "fixture/ListDeploymentsResponse.proto"
    codeDeploy
    (Proxy :: Proxy ListDeployments)

testListApplicationRevisionsResponse :: ListApplicationRevisionsResponse -> TestTree
testListApplicationRevisionsResponse = res
    "ListApplicationRevisionsResponse"
    "fixture/ListApplicationRevisionsResponse.proto"
    codeDeploy
    (Proxy :: Proxy ListApplicationRevisions)

testListApplicationsResponse :: ListApplicationsResponse -> TestTree
testListApplicationsResponse = res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    codeDeploy
    (Proxy :: Proxy ListApplications)

testAddTagsToOnPremisesInstancesResponse :: AddTagsToOnPremisesInstancesResponse -> TestTree
testAddTagsToOnPremisesInstancesResponse = res
    "AddTagsToOnPremisesInstancesResponse"
    "fixture/AddTagsToOnPremisesInstancesResponse.proto"
    codeDeploy
    (Proxy :: Proxy AddTagsToOnPremisesInstances)

testListDeploymentInstancesResponse :: ListDeploymentInstancesResponse -> TestTree
testListDeploymentInstancesResponse = res
    "ListDeploymentInstancesResponse"
    "fixture/ListDeploymentInstancesResponse.proto"
    codeDeploy
    (Proxy :: Proxy ListDeploymentInstances)
