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
--         , testGetDeployment $
--             getDeployment
--
--         , testCreateDeploymentConfig $
--             createDeploymentConfig
--
--         , testUpdateDeploymentGroup $
--             updateDeploymentGroup
--
--         , testDeleteDeploymentGroup $
--             deleteDeploymentGroup
--
--         , testListOnPremisesInstances $
--             listOnPremisesInstances
--
--         , testGetApplicationRevision $
--             getApplicationRevision
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
--         , testGetOnPremisesInstance $
--             getOnPremisesInstance
--
--         , testBatchGetDeployments $
--             batchGetDeployments
--
--         , testRegisterApplicationRevision $
--             registerApplicationRevision
--
--         , testDeleteApplication $
--             deleteApplication
--
--         , testUpdateApplication $
--             updateApplication
--
--         , testBatchGetApplications $
--             batchGetApplications
--
--         , testCreateApplication $
--             createApplication
--
--         , testDeregisterOnPremisesInstance $
--             deregisterOnPremisesInstance
--
--         , testGetDeploymentInstance $
--             getDeploymentInstance
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
--         , testRegisterOnPremisesInstance $
--             registerOnPremisesInstance
--
--         , testBatchGetOnPremisesInstances $
--             batchGetOnPremisesInstances
--
--         , testListDeploymentConfigs $
--             listDeploymentConfigs
--
--         , testCreateDeploymentGroup $
--             createDeploymentGroup
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
--         , testGetDeploymentResponse $
--             getDeploymentResponse
--
--         , testCreateDeploymentConfigResponse $
--             createDeploymentConfigResponse
--
--         , testUpdateDeploymentGroupResponse $
--             updateDeploymentGroupResponse
--
--         , testDeleteDeploymentGroupResponse $
--             deleteDeploymentGroupResponse
--
--         , testListOnPremisesInstancesResponse $
--             listOnPremisesInstancesResponse
--
--         , testGetApplicationRevisionResponse $
--             getApplicationRevisionResponse
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
--         , testGetOnPremisesInstanceResponse $
--             getOnPremisesInstanceResponse
--
--         , testBatchGetDeploymentsResponse $
--             batchGetDeploymentsResponse
--
--         , testRegisterApplicationRevisionResponse $
--             registerApplicationRevisionResponse
--
--         , testDeleteApplicationResponse $
--             deleteApplicationResponse
--
--         , testUpdateApplicationResponse $
--             updateApplicationResponse
--
--         , testBatchGetApplicationsResponse $
--             batchGetApplicationsResponse
--
--         , testCreateApplicationResponse $
--             createApplicationResponse
--
--         , testDeregisterOnPremisesInstanceResponse $
--             deregisterOnPremisesInstanceResponse
--
--         , testGetDeploymentInstanceResponse $
--             getDeploymentInstanceResponse
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
--         , testRegisterOnPremisesInstanceResponse $
--             registerOnPremisesInstanceResponse
--
--         , testBatchGetOnPremisesInstancesResponse $
--             batchGetOnPremisesInstancesResponse
--
--         , testListDeploymentConfigsResponse $
--             listDeploymentConfigsResponse
--
--         , testCreateDeploymentGroupResponse $
--             createDeploymentGroupResponse
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

testGetDeployment :: GetDeployment -> TestTree
testGetDeployment = req
    "GetDeployment"
    "fixture/GetDeployment.yaml"

testCreateDeploymentConfig :: CreateDeploymentConfig -> TestTree
testCreateDeploymentConfig = req
    "CreateDeploymentConfig"
    "fixture/CreateDeploymentConfig.yaml"

testUpdateDeploymentGroup :: UpdateDeploymentGroup -> TestTree
testUpdateDeploymentGroup = req
    "UpdateDeploymentGroup"
    "fixture/UpdateDeploymentGroup.yaml"

testDeleteDeploymentGroup :: DeleteDeploymentGroup -> TestTree
testDeleteDeploymentGroup = req
    "DeleteDeploymentGroup"
    "fixture/DeleteDeploymentGroup.yaml"

testListOnPremisesInstances :: ListOnPremisesInstances -> TestTree
testListOnPremisesInstances = req
    "ListOnPremisesInstances"
    "fixture/ListOnPremisesInstances.yaml"

testGetApplicationRevision :: GetApplicationRevision -> TestTree
testGetApplicationRevision = req
    "GetApplicationRevision"
    "fixture/GetApplicationRevision.yaml"

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

testGetOnPremisesInstance :: GetOnPremisesInstance -> TestTree
testGetOnPremisesInstance = req
    "GetOnPremisesInstance"
    "fixture/GetOnPremisesInstance.yaml"

testBatchGetDeployments :: BatchGetDeployments -> TestTree
testBatchGetDeployments = req
    "BatchGetDeployments"
    "fixture/BatchGetDeployments.yaml"

testRegisterApplicationRevision :: RegisterApplicationRevision -> TestTree
testRegisterApplicationRevision = req
    "RegisterApplicationRevision"
    "fixture/RegisterApplicationRevision.yaml"

testDeleteApplication :: DeleteApplication -> TestTree
testDeleteApplication = req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

testUpdateApplication :: UpdateApplication -> TestTree
testUpdateApplication = req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

testBatchGetApplications :: BatchGetApplications -> TestTree
testBatchGetApplications = req
    "BatchGetApplications"
    "fixture/BatchGetApplications.yaml"

testCreateApplication :: CreateApplication -> TestTree
testCreateApplication = req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

testDeregisterOnPremisesInstance :: DeregisterOnPremisesInstance -> TestTree
testDeregisterOnPremisesInstance = req
    "DeregisterOnPremisesInstance"
    "fixture/DeregisterOnPremisesInstance.yaml"

testGetDeploymentInstance :: GetDeploymentInstance -> TestTree
testGetDeploymentInstance = req
    "GetDeploymentInstance"
    "fixture/GetDeploymentInstance.yaml"

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

testRegisterOnPremisesInstance :: RegisterOnPremisesInstance -> TestTree
testRegisterOnPremisesInstance = req
    "RegisterOnPremisesInstance"
    "fixture/RegisterOnPremisesInstance.yaml"

testBatchGetOnPremisesInstances :: BatchGetOnPremisesInstances -> TestTree
testBatchGetOnPremisesInstances = req
    "BatchGetOnPremisesInstances"
    "fixture/BatchGetOnPremisesInstances.yaml"

testListDeploymentConfigs :: ListDeploymentConfigs -> TestTree
testListDeploymentConfigs = req
    "ListDeploymentConfigs"
    "fixture/ListDeploymentConfigs.yaml"

testCreateDeploymentGroup :: CreateDeploymentGroup -> TestTree
testCreateDeploymentGroup = req
    "CreateDeploymentGroup"
    "fixture/CreateDeploymentGroup.yaml"

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

testGetDeploymentResponse :: GetDeploymentResponse -> TestTree
testGetDeploymentResponse = res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    codeDeploy
    (Proxy :: Proxy GetDeployment)

testCreateDeploymentConfigResponse :: CreateDeploymentConfigResponse -> TestTree
testCreateDeploymentConfigResponse = res
    "CreateDeploymentConfigResponse"
    "fixture/CreateDeploymentConfigResponse.proto"
    codeDeploy
    (Proxy :: Proxy CreateDeploymentConfig)

testUpdateDeploymentGroupResponse :: UpdateDeploymentGroupResponse -> TestTree
testUpdateDeploymentGroupResponse = res
    "UpdateDeploymentGroupResponse"
    "fixture/UpdateDeploymentGroupResponse.proto"
    codeDeploy
    (Proxy :: Proxy UpdateDeploymentGroup)

testDeleteDeploymentGroupResponse :: DeleteDeploymentGroupResponse -> TestTree
testDeleteDeploymentGroupResponse = res
    "DeleteDeploymentGroupResponse"
    "fixture/DeleteDeploymentGroupResponse.proto"
    codeDeploy
    (Proxy :: Proxy DeleteDeploymentGroup)

testListOnPremisesInstancesResponse :: ListOnPremisesInstancesResponse -> TestTree
testListOnPremisesInstancesResponse = res
    "ListOnPremisesInstancesResponse"
    "fixture/ListOnPremisesInstancesResponse.proto"
    codeDeploy
    (Proxy :: Proxy ListOnPremisesInstances)

testGetApplicationRevisionResponse :: GetApplicationRevisionResponse -> TestTree
testGetApplicationRevisionResponse = res
    "GetApplicationRevisionResponse"
    "fixture/GetApplicationRevisionResponse.proto"
    codeDeploy
    (Proxy :: Proxy GetApplicationRevision)

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

testGetOnPremisesInstanceResponse :: GetOnPremisesInstanceResponse -> TestTree
testGetOnPremisesInstanceResponse = res
    "GetOnPremisesInstanceResponse"
    "fixture/GetOnPremisesInstanceResponse.proto"
    codeDeploy
    (Proxy :: Proxy GetOnPremisesInstance)

testBatchGetDeploymentsResponse :: BatchGetDeploymentsResponse -> TestTree
testBatchGetDeploymentsResponse = res
    "BatchGetDeploymentsResponse"
    "fixture/BatchGetDeploymentsResponse.proto"
    codeDeploy
    (Proxy :: Proxy BatchGetDeployments)

testRegisterApplicationRevisionResponse :: RegisterApplicationRevisionResponse -> TestTree
testRegisterApplicationRevisionResponse = res
    "RegisterApplicationRevisionResponse"
    "fixture/RegisterApplicationRevisionResponse.proto"
    codeDeploy
    (Proxy :: Proxy RegisterApplicationRevision)

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

testBatchGetApplicationsResponse :: BatchGetApplicationsResponse -> TestTree
testBatchGetApplicationsResponse = res
    "BatchGetApplicationsResponse"
    "fixture/BatchGetApplicationsResponse.proto"
    codeDeploy
    (Proxy :: Proxy BatchGetApplications)

testCreateApplicationResponse :: CreateApplicationResponse -> TestTree
testCreateApplicationResponse = res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    codeDeploy
    (Proxy :: Proxy CreateApplication)

testDeregisterOnPremisesInstanceResponse :: DeregisterOnPremisesInstanceResponse -> TestTree
testDeregisterOnPremisesInstanceResponse = res
    "DeregisterOnPremisesInstanceResponse"
    "fixture/DeregisterOnPremisesInstanceResponse.proto"
    codeDeploy
    (Proxy :: Proxy DeregisterOnPremisesInstance)

testGetDeploymentInstanceResponse :: GetDeploymentInstanceResponse -> TestTree
testGetDeploymentInstanceResponse = res
    "GetDeploymentInstanceResponse"
    "fixture/GetDeploymentInstanceResponse.proto"
    codeDeploy
    (Proxy :: Proxy GetDeploymentInstance)

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

testRegisterOnPremisesInstanceResponse :: RegisterOnPremisesInstanceResponse -> TestTree
testRegisterOnPremisesInstanceResponse = res
    "RegisterOnPremisesInstanceResponse"
    "fixture/RegisterOnPremisesInstanceResponse.proto"
    codeDeploy
    (Proxy :: Proxy RegisterOnPremisesInstance)

testBatchGetOnPremisesInstancesResponse :: BatchGetOnPremisesInstancesResponse -> TestTree
testBatchGetOnPremisesInstancesResponse = res
    "BatchGetOnPremisesInstancesResponse"
    "fixture/BatchGetOnPremisesInstancesResponse.proto"
    codeDeploy
    (Proxy :: Proxy BatchGetOnPremisesInstances)

testListDeploymentConfigsResponse :: ListDeploymentConfigsResponse -> TestTree
testListDeploymentConfigsResponse = res
    "ListDeploymentConfigsResponse"
    "fixture/ListDeploymentConfigsResponse.proto"
    codeDeploy
    (Proxy :: Proxy ListDeploymentConfigs)

testCreateDeploymentGroupResponse :: CreateDeploymentGroupResponse -> TestTree
testCreateDeploymentGroupResponse = res
    "CreateDeploymentGroupResponse"
    "fixture/CreateDeploymentGroupResponse.proto"
    codeDeploy
    (Proxy :: Proxy CreateDeploymentGroup)

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
