-- Module      : Test.AWS.Gen.CodeDeploy
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.CodeDeploy where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.CodeDeploy

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
testRemoveTagsFromOnPremisesInstances = undefined

testGetDeployment :: GetDeployment -> TestTree
testGetDeployment = undefined

testCreateDeploymentConfig :: CreateDeploymentConfig -> TestTree
testCreateDeploymentConfig = undefined

testUpdateDeploymentGroup :: UpdateDeploymentGroup -> TestTree
testUpdateDeploymentGroup = undefined

testDeleteDeploymentGroup :: DeleteDeploymentGroup -> TestTree
testDeleteDeploymentGroup = undefined

testListOnPremisesInstances :: ListOnPremisesInstances -> TestTree
testListOnPremisesInstances = undefined

testGetApplicationRevision :: GetApplicationRevision -> TestTree
testGetApplicationRevision = undefined

testDeleteDeploymentConfig :: DeleteDeploymentConfig -> TestTree
testDeleteDeploymentConfig = undefined

testGetDeploymentConfig :: GetDeploymentConfig -> TestTree
testGetDeploymentConfig = undefined

testCreateDeployment :: CreateDeployment -> TestTree
testCreateDeployment = undefined

testGetOnPremisesInstance :: GetOnPremisesInstance -> TestTree
testGetOnPremisesInstance = undefined

testBatchGetDeployments :: BatchGetDeployments -> TestTree
testBatchGetDeployments = undefined

testRegisterApplicationRevision :: RegisterApplicationRevision -> TestTree
testRegisterApplicationRevision = undefined

testDeleteApplication :: DeleteApplication -> TestTree
testDeleteApplication = undefined

testUpdateApplication :: UpdateApplication -> TestTree
testUpdateApplication = undefined

testBatchGetApplications :: BatchGetApplications -> TestTree
testBatchGetApplications = undefined

testCreateApplication :: CreateApplication -> TestTree
testCreateApplication = undefined

testDeregisterOnPremisesInstance :: DeregisterOnPremisesInstance -> TestTree
testDeregisterOnPremisesInstance = undefined

testGetDeploymentInstance :: GetDeploymentInstance -> TestTree
testGetDeploymentInstance = undefined

testStopDeployment :: StopDeployment -> TestTree
testStopDeployment = undefined

testGetApplication :: GetApplication -> TestTree
testGetApplication = undefined

testListDeploymentGroups :: ListDeploymentGroups -> TestTree
testListDeploymentGroups = undefined

testRegisterOnPremisesInstance :: RegisterOnPremisesInstance -> TestTree
testRegisterOnPremisesInstance = undefined

testBatchGetOnPremisesInstances :: BatchGetOnPremisesInstances -> TestTree
testBatchGetOnPremisesInstances = undefined

testListDeploymentConfigs :: ListDeploymentConfigs -> TestTree
testListDeploymentConfigs = undefined

testCreateDeploymentGroup :: CreateDeploymentGroup -> TestTree
testCreateDeploymentGroup = undefined

testGetDeploymentGroup :: GetDeploymentGroup -> TestTree
testGetDeploymentGroup = undefined

testListDeployments :: ListDeployments -> TestTree
testListDeployments = undefined

testListApplicationRevisions :: ListApplicationRevisions -> TestTree
testListApplicationRevisions = undefined

testListApplications :: ListApplications -> TestTree
testListApplications = undefined

testAddTagsToOnPremisesInstances :: AddTagsToOnPremisesInstances -> TestTree
testAddTagsToOnPremisesInstances = undefined

testListDeploymentInstances :: ListDeploymentInstances -> TestTree
testListDeploymentInstances = undefined

-- Responses

testRemoveTagsFromOnPremisesInstancesResponse :: RemoveTagsFromOnPremisesInstancesResponse -> TestTree
testRemoveTagsFromOnPremisesInstancesResponse = resp
    "RemoveTagsFromOnPremisesInstancesResponse"
    "fixture/RemoveTagsFromOnPremisesInstancesResponse"
    (Proxy :: Proxy RemoveTagsFromOnPremisesInstances)

testGetDeploymentResponse :: GetDeploymentResponse -> TestTree
testGetDeploymentResponse = resp
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse"
    (Proxy :: Proxy GetDeployment)

testCreateDeploymentConfigResponse :: CreateDeploymentConfigResponse -> TestTree
testCreateDeploymentConfigResponse = resp
    "CreateDeploymentConfigResponse"
    "fixture/CreateDeploymentConfigResponse"
    (Proxy :: Proxy CreateDeploymentConfig)

testUpdateDeploymentGroupResponse :: UpdateDeploymentGroupResponse -> TestTree
testUpdateDeploymentGroupResponse = resp
    "UpdateDeploymentGroupResponse"
    "fixture/UpdateDeploymentGroupResponse"
    (Proxy :: Proxy UpdateDeploymentGroup)

testDeleteDeploymentGroupResponse :: DeleteDeploymentGroupResponse -> TestTree
testDeleteDeploymentGroupResponse = resp
    "DeleteDeploymentGroupResponse"
    "fixture/DeleteDeploymentGroupResponse"
    (Proxy :: Proxy DeleteDeploymentGroup)

testListOnPremisesInstancesResponse :: ListOnPremisesInstancesResponse -> TestTree
testListOnPremisesInstancesResponse = resp
    "ListOnPremisesInstancesResponse"
    "fixture/ListOnPremisesInstancesResponse"
    (Proxy :: Proxy ListOnPremisesInstances)

testGetApplicationRevisionResponse :: GetApplicationRevisionResponse -> TestTree
testGetApplicationRevisionResponse = resp
    "GetApplicationRevisionResponse"
    "fixture/GetApplicationRevisionResponse"
    (Proxy :: Proxy GetApplicationRevision)

testDeleteDeploymentConfigResponse :: DeleteDeploymentConfigResponse -> TestTree
testDeleteDeploymentConfigResponse = resp
    "DeleteDeploymentConfigResponse"
    "fixture/DeleteDeploymentConfigResponse"
    (Proxy :: Proxy DeleteDeploymentConfig)

testGetDeploymentConfigResponse :: GetDeploymentConfigResponse -> TestTree
testGetDeploymentConfigResponse = resp
    "GetDeploymentConfigResponse"
    "fixture/GetDeploymentConfigResponse"
    (Proxy :: Proxy GetDeploymentConfig)

testCreateDeploymentResponse :: CreateDeploymentResponse -> TestTree
testCreateDeploymentResponse = resp
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse"
    (Proxy :: Proxy CreateDeployment)

testGetOnPremisesInstanceResponse :: GetOnPremisesInstanceResponse -> TestTree
testGetOnPremisesInstanceResponse = resp
    "GetOnPremisesInstanceResponse"
    "fixture/GetOnPremisesInstanceResponse"
    (Proxy :: Proxy GetOnPremisesInstance)

testBatchGetDeploymentsResponse :: BatchGetDeploymentsResponse -> TestTree
testBatchGetDeploymentsResponse = resp
    "BatchGetDeploymentsResponse"
    "fixture/BatchGetDeploymentsResponse"
    (Proxy :: Proxy BatchGetDeployments)

testRegisterApplicationRevisionResponse :: RegisterApplicationRevisionResponse -> TestTree
testRegisterApplicationRevisionResponse = resp
    "RegisterApplicationRevisionResponse"
    "fixture/RegisterApplicationRevisionResponse"
    (Proxy :: Proxy RegisterApplicationRevision)

testDeleteApplicationResponse :: DeleteApplicationResponse -> TestTree
testDeleteApplicationResponse = resp
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse"
    (Proxy :: Proxy DeleteApplication)

testUpdateApplicationResponse :: UpdateApplicationResponse -> TestTree
testUpdateApplicationResponse = resp
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse"
    (Proxy :: Proxy UpdateApplication)

testBatchGetApplicationsResponse :: BatchGetApplicationsResponse -> TestTree
testBatchGetApplicationsResponse = resp
    "BatchGetApplicationsResponse"
    "fixture/BatchGetApplicationsResponse"
    (Proxy :: Proxy BatchGetApplications)

testCreateApplicationResponse :: CreateApplicationResponse -> TestTree
testCreateApplicationResponse = resp
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse"
    (Proxy :: Proxy CreateApplication)

testDeregisterOnPremisesInstanceResponse :: DeregisterOnPremisesInstanceResponse -> TestTree
testDeregisterOnPremisesInstanceResponse = resp
    "DeregisterOnPremisesInstanceResponse"
    "fixture/DeregisterOnPremisesInstanceResponse"
    (Proxy :: Proxy DeregisterOnPremisesInstance)

testGetDeploymentInstanceResponse :: GetDeploymentInstanceResponse -> TestTree
testGetDeploymentInstanceResponse = resp
    "GetDeploymentInstanceResponse"
    "fixture/GetDeploymentInstanceResponse"
    (Proxy :: Proxy GetDeploymentInstance)

testStopDeploymentResponse :: StopDeploymentResponse -> TestTree
testStopDeploymentResponse = resp
    "StopDeploymentResponse"
    "fixture/StopDeploymentResponse"
    (Proxy :: Proxy StopDeployment)

testGetApplicationResponse :: GetApplicationResponse -> TestTree
testGetApplicationResponse = resp
    "GetApplicationResponse"
    "fixture/GetApplicationResponse"
    (Proxy :: Proxy GetApplication)

testListDeploymentGroupsResponse :: ListDeploymentGroupsResponse -> TestTree
testListDeploymentGroupsResponse = resp
    "ListDeploymentGroupsResponse"
    "fixture/ListDeploymentGroupsResponse"
    (Proxy :: Proxy ListDeploymentGroups)

testRegisterOnPremisesInstanceResponse :: RegisterOnPremisesInstanceResponse -> TestTree
testRegisterOnPremisesInstanceResponse = resp
    "RegisterOnPremisesInstanceResponse"
    "fixture/RegisterOnPremisesInstanceResponse"
    (Proxy :: Proxy RegisterOnPremisesInstance)

testBatchGetOnPremisesInstancesResponse :: BatchGetOnPremisesInstancesResponse -> TestTree
testBatchGetOnPremisesInstancesResponse = resp
    "BatchGetOnPremisesInstancesResponse"
    "fixture/BatchGetOnPremisesInstancesResponse"
    (Proxy :: Proxy BatchGetOnPremisesInstances)

testListDeploymentConfigsResponse :: ListDeploymentConfigsResponse -> TestTree
testListDeploymentConfigsResponse = resp
    "ListDeploymentConfigsResponse"
    "fixture/ListDeploymentConfigsResponse"
    (Proxy :: Proxy ListDeploymentConfigs)

testCreateDeploymentGroupResponse :: CreateDeploymentGroupResponse -> TestTree
testCreateDeploymentGroupResponse = resp
    "CreateDeploymentGroupResponse"
    "fixture/CreateDeploymentGroupResponse"
    (Proxy :: Proxy CreateDeploymentGroup)

testGetDeploymentGroupResponse :: GetDeploymentGroupResponse -> TestTree
testGetDeploymentGroupResponse = resp
    "GetDeploymentGroupResponse"
    "fixture/GetDeploymentGroupResponse"
    (Proxy :: Proxy GetDeploymentGroup)

testListDeploymentsResponse :: ListDeploymentsResponse -> TestTree
testListDeploymentsResponse = resp
    "ListDeploymentsResponse"
    "fixture/ListDeploymentsResponse"
    (Proxy :: Proxy ListDeployments)

testListApplicationRevisionsResponse :: ListApplicationRevisionsResponse -> TestTree
testListApplicationRevisionsResponse = resp
    "ListApplicationRevisionsResponse"
    "fixture/ListApplicationRevisionsResponse"
    (Proxy :: Proxy ListApplicationRevisions)

testListApplicationsResponse :: ListApplicationsResponse -> TestTree
testListApplicationsResponse = resp
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse"
    (Proxy :: Proxy ListApplications)

testAddTagsToOnPremisesInstancesResponse :: AddTagsToOnPremisesInstancesResponse -> TestTree
testAddTagsToOnPremisesInstancesResponse = resp
    "AddTagsToOnPremisesInstancesResponse"
    "fixture/AddTagsToOnPremisesInstancesResponse"
    (Proxy :: Proxy AddTagsToOnPremisesInstances)

testListDeploymentInstancesResponse :: ListDeploymentInstancesResponse -> TestTree
testListDeploymentInstancesResponse = resp
    "ListDeploymentInstancesResponse"
    "fixture/ListDeploymentInstancesResponse"
    (Proxy :: Proxy ListDeploymentInstances)
