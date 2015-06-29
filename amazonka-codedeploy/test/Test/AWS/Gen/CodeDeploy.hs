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
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ removeTagsFromOnPremisesInstancesTest $
--             removeTagsFromOnPremisesInstances
--
--         , getDeploymentTest $
--             getDeployment
--
--         , createDeploymentConfigTest $
--             createDeploymentConfig
--
--         , updateDeploymentGroupTest $
--             updateDeploymentGroup
--
--         , deleteDeploymentGroupTest $
--             deleteDeploymentGroup
--
--         , listOnPremisesInstancesTest $
--             listOnPremisesInstances
--
--         , getApplicationRevisionTest $
--             getApplicationRevision
--
--         , deleteDeploymentConfigTest $
--             deleteDeploymentConfig
--
--         , getDeploymentConfigTest $
--             getDeploymentConfig
--
--         , createDeploymentTest $
--             createDeployment
--
--         , getOnPremisesInstanceTest $
--             getOnPremisesInstance
--
--         , batchGetDeploymentsTest $
--             batchGetDeployments
--
--         , registerApplicationRevisionTest $
--             registerApplicationRevision
--
--         , deleteApplicationTest $
--             deleteApplication
--
--         , updateApplicationTest $
--             updateApplication
--
--         , batchGetApplicationsTest $
--             batchGetApplications
--
--         , createApplicationTest $
--             createApplication
--
--         , deregisterOnPremisesInstanceTest $
--             deregisterOnPremisesInstance
--
--         , getDeploymentInstanceTest $
--             getDeploymentInstance
--
--         , stopDeploymentTest $
--             stopDeployment
--
--         , getApplicationTest $
--             getApplication
--
--         , listDeploymentGroupsTest $
--             listDeploymentGroups
--
--         , registerOnPremisesInstanceTest $
--             registerOnPremisesInstance
--
--         , batchGetOnPremisesInstancesTest $
--             batchGetOnPremisesInstances
--
--         , listDeploymentConfigsTest $
--             listDeploymentConfigs
--
--         , createDeploymentGroupTest $
--             createDeploymentGroup
--
--         , getDeploymentGroupTest $
--             getDeploymentGroup
--
--         , listDeploymentsTest $
--             listDeployments
--
--         , listApplicationRevisionsTest $
--             listApplicationRevisions
--
--         , listApplicationsTest $
--             listApplications
--
--         , addTagsToOnPremisesInstancesTest $
--             addTagsToOnPremisesInstances
--
--         , listDeploymentInstancesTest $
--             listDeploymentInstances
--
--           ]

--     , testGroup "response"
--         [ removeTagsFromOnPremisesInstancesResponseTest $
--             removeTagsFromOnPremisesInstancesResponse
--
--         , getDeploymentResponseTest $
--             getDeploymentResponse
--
--         , createDeploymentConfigResponseTest $
--             createDeploymentConfigResponse
--
--         , updateDeploymentGroupResponseTest $
--             updateDeploymentGroupResponse
--
--         , deleteDeploymentGroupResponseTest $
--             deleteDeploymentGroupResponse
--
--         , listOnPremisesInstancesResponseTest $
--             listOnPremisesInstancesResponse
--
--         , getApplicationRevisionResponseTest $
--             getApplicationRevisionResponse
--
--         , deleteDeploymentConfigResponseTest $
--             deleteDeploymentConfigResponse
--
--         , getDeploymentConfigResponseTest $
--             getDeploymentConfigResponse
--
--         , createDeploymentResponseTest $
--             createDeploymentResponse
--
--         , getOnPremisesInstanceResponseTest $
--             getOnPremisesInstanceResponse
--
--         , batchGetDeploymentsResponseTest $
--             batchGetDeploymentsResponse
--
--         , registerApplicationRevisionResponseTest $
--             registerApplicationRevisionResponse
--
--         , deleteApplicationResponseTest $
--             deleteApplicationResponse
--
--         , updateApplicationResponseTest $
--             updateApplicationResponse
--
--         , batchGetApplicationsResponseTest $
--             batchGetApplicationsResponse
--
--         , createApplicationResponseTest $
--             createApplicationResponse
--
--         , deregisterOnPremisesInstanceResponseTest $
--             deregisterOnPremisesInstanceResponse
--
--         , getDeploymentInstanceResponseTest $
--             getDeploymentInstanceResponse
--
--         , stopDeploymentResponseTest $
--             stopDeploymentResponse
--
--         , getApplicationResponseTest $
--             getApplicationResponse
--
--         , listDeploymentGroupsResponseTest $
--             listDeploymentGroupsResponse
--
--         , registerOnPremisesInstanceResponseTest $
--             registerOnPremisesInstanceResponse
--
--         , batchGetOnPremisesInstancesResponseTest $
--             batchGetOnPremisesInstancesResponse
--
--         , listDeploymentConfigsResponseTest $
--             listDeploymentConfigsResponse
--
--         , createDeploymentGroupResponseTest $
--             createDeploymentGroupResponse
--
--         , getDeploymentGroupResponseTest $
--             getDeploymentGroupResponse
--
--         , listDeploymentsResponseTest $
--             listDeploymentsResponse
--
--         , listApplicationRevisionsResponseTest $
--             listApplicationRevisionsResponse
--
--         , listApplicationsResponseTest $
--             listApplicationsResponse
--
--         , addTagsToOnPremisesInstancesResponseTest $
--             addTagsToOnPremisesInstancesResponse
--
--         , listDeploymentInstancesResponseTest $
--             listDeploymentInstancesResponse
--
--           ]
--     ]

-- Requests

removeTagsFromOnPremisesInstancesTest :: RemoveTagsFromOnPremisesInstances -> TestTree
removeTagsFromOnPremisesInstancesTest = undefined

getDeploymentTest :: GetDeployment -> TestTree
getDeploymentTest = undefined

createDeploymentConfigTest :: CreateDeploymentConfig -> TestTree
createDeploymentConfigTest = undefined

updateDeploymentGroupTest :: UpdateDeploymentGroup -> TestTree
updateDeploymentGroupTest = undefined

deleteDeploymentGroupTest :: DeleteDeploymentGroup -> TestTree
deleteDeploymentGroupTest = undefined

listOnPremisesInstancesTest :: ListOnPremisesInstances -> TestTree
listOnPremisesInstancesTest = undefined

getApplicationRevisionTest :: GetApplicationRevision -> TestTree
getApplicationRevisionTest = undefined

deleteDeploymentConfigTest :: DeleteDeploymentConfig -> TestTree
deleteDeploymentConfigTest = undefined

getDeploymentConfigTest :: GetDeploymentConfig -> TestTree
getDeploymentConfigTest = undefined

createDeploymentTest :: CreateDeployment -> TestTree
createDeploymentTest = undefined

getOnPremisesInstanceTest :: GetOnPremisesInstance -> TestTree
getOnPremisesInstanceTest = undefined

batchGetDeploymentsTest :: BatchGetDeployments -> TestTree
batchGetDeploymentsTest = undefined

registerApplicationRevisionTest :: RegisterApplicationRevision -> TestTree
registerApplicationRevisionTest = undefined

deleteApplicationTest :: DeleteApplication -> TestTree
deleteApplicationTest = undefined

updateApplicationTest :: UpdateApplication -> TestTree
updateApplicationTest = undefined

batchGetApplicationsTest :: BatchGetApplications -> TestTree
batchGetApplicationsTest = undefined

createApplicationTest :: CreateApplication -> TestTree
createApplicationTest = undefined

deregisterOnPremisesInstanceTest :: DeregisterOnPremisesInstance -> TestTree
deregisterOnPremisesInstanceTest = undefined

getDeploymentInstanceTest :: GetDeploymentInstance -> TestTree
getDeploymentInstanceTest = undefined

stopDeploymentTest :: StopDeployment -> TestTree
stopDeploymentTest = undefined

getApplicationTest :: GetApplication -> TestTree
getApplicationTest = undefined

listDeploymentGroupsTest :: ListDeploymentGroups -> TestTree
listDeploymentGroupsTest = undefined

registerOnPremisesInstanceTest :: RegisterOnPremisesInstance -> TestTree
registerOnPremisesInstanceTest = undefined

batchGetOnPremisesInstancesTest :: BatchGetOnPremisesInstances -> TestTree
batchGetOnPremisesInstancesTest = undefined

listDeploymentConfigsTest :: ListDeploymentConfigs -> TestTree
listDeploymentConfigsTest = undefined

createDeploymentGroupTest :: CreateDeploymentGroup -> TestTree
createDeploymentGroupTest = undefined

getDeploymentGroupTest :: GetDeploymentGroup -> TestTree
getDeploymentGroupTest = undefined

listDeploymentsTest :: ListDeployments -> TestTree
listDeploymentsTest = undefined

listApplicationRevisionsTest :: ListApplicationRevisions -> TestTree
listApplicationRevisionsTest = undefined

listApplicationsTest :: ListApplications -> TestTree
listApplicationsTest = undefined

addTagsToOnPremisesInstancesTest :: AddTagsToOnPremisesInstances -> TestTree
addTagsToOnPremisesInstancesTest = undefined

listDeploymentInstancesTest :: ListDeploymentInstances -> TestTree
listDeploymentInstancesTest = undefined

-- Responses

removeTagsFromOnPremisesInstancesResponseTest :: RemoveTagsFromOnPremisesInstancesResponse -> TestTree
removeTagsFromOnPremisesInstancesResponseTest = resp
    "RemoveTagsFromOnPremisesInstancesResponse"
    "fixture/RemoveTagsFromOnPremisesInstancesResponse"
    (Proxy :: Proxy RemoveTagsFromOnPremisesInstances)

getDeploymentResponseTest :: GetDeploymentResponse -> TestTree
getDeploymentResponseTest = resp
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse"
    (Proxy :: Proxy GetDeployment)

createDeploymentConfigResponseTest :: CreateDeploymentConfigResponse -> TestTree
createDeploymentConfigResponseTest = resp
    "CreateDeploymentConfigResponse"
    "fixture/CreateDeploymentConfigResponse"
    (Proxy :: Proxy CreateDeploymentConfig)

updateDeploymentGroupResponseTest :: UpdateDeploymentGroupResponse -> TestTree
updateDeploymentGroupResponseTest = resp
    "UpdateDeploymentGroupResponse"
    "fixture/UpdateDeploymentGroupResponse"
    (Proxy :: Proxy UpdateDeploymentGroup)

deleteDeploymentGroupResponseTest :: DeleteDeploymentGroupResponse -> TestTree
deleteDeploymentGroupResponseTest = resp
    "DeleteDeploymentGroupResponse"
    "fixture/DeleteDeploymentGroupResponse"
    (Proxy :: Proxy DeleteDeploymentGroup)

listOnPremisesInstancesResponseTest :: ListOnPremisesInstancesResponse -> TestTree
listOnPremisesInstancesResponseTest = resp
    "ListOnPremisesInstancesResponse"
    "fixture/ListOnPremisesInstancesResponse"
    (Proxy :: Proxy ListOnPremisesInstances)

getApplicationRevisionResponseTest :: GetApplicationRevisionResponse -> TestTree
getApplicationRevisionResponseTest = resp
    "GetApplicationRevisionResponse"
    "fixture/GetApplicationRevisionResponse"
    (Proxy :: Proxy GetApplicationRevision)

deleteDeploymentConfigResponseTest :: DeleteDeploymentConfigResponse -> TestTree
deleteDeploymentConfigResponseTest = resp
    "DeleteDeploymentConfigResponse"
    "fixture/DeleteDeploymentConfigResponse"
    (Proxy :: Proxy DeleteDeploymentConfig)

getDeploymentConfigResponseTest :: GetDeploymentConfigResponse -> TestTree
getDeploymentConfigResponseTest = resp
    "GetDeploymentConfigResponse"
    "fixture/GetDeploymentConfigResponse"
    (Proxy :: Proxy GetDeploymentConfig)

createDeploymentResponseTest :: CreateDeploymentResponse -> TestTree
createDeploymentResponseTest = resp
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse"
    (Proxy :: Proxy CreateDeployment)

getOnPremisesInstanceResponseTest :: GetOnPremisesInstanceResponse -> TestTree
getOnPremisesInstanceResponseTest = resp
    "GetOnPremisesInstanceResponse"
    "fixture/GetOnPremisesInstanceResponse"
    (Proxy :: Proxy GetOnPremisesInstance)

batchGetDeploymentsResponseTest :: BatchGetDeploymentsResponse -> TestTree
batchGetDeploymentsResponseTest = resp
    "BatchGetDeploymentsResponse"
    "fixture/BatchGetDeploymentsResponse"
    (Proxy :: Proxy BatchGetDeployments)

registerApplicationRevisionResponseTest :: RegisterApplicationRevisionResponse -> TestTree
registerApplicationRevisionResponseTest = resp
    "RegisterApplicationRevisionResponse"
    "fixture/RegisterApplicationRevisionResponse"
    (Proxy :: Proxy RegisterApplicationRevision)

deleteApplicationResponseTest :: DeleteApplicationResponse -> TestTree
deleteApplicationResponseTest = resp
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse"
    (Proxy :: Proxy DeleteApplication)

updateApplicationResponseTest :: UpdateApplicationResponse -> TestTree
updateApplicationResponseTest = resp
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse"
    (Proxy :: Proxy UpdateApplication)

batchGetApplicationsResponseTest :: BatchGetApplicationsResponse -> TestTree
batchGetApplicationsResponseTest = resp
    "BatchGetApplicationsResponse"
    "fixture/BatchGetApplicationsResponse"
    (Proxy :: Proxy BatchGetApplications)

createApplicationResponseTest :: CreateApplicationResponse -> TestTree
createApplicationResponseTest = resp
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse"
    (Proxy :: Proxy CreateApplication)

deregisterOnPremisesInstanceResponseTest :: DeregisterOnPremisesInstanceResponse -> TestTree
deregisterOnPremisesInstanceResponseTest = resp
    "DeregisterOnPremisesInstanceResponse"
    "fixture/DeregisterOnPremisesInstanceResponse"
    (Proxy :: Proxy DeregisterOnPremisesInstance)

getDeploymentInstanceResponseTest :: GetDeploymentInstanceResponse -> TestTree
getDeploymentInstanceResponseTest = resp
    "GetDeploymentInstanceResponse"
    "fixture/GetDeploymentInstanceResponse"
    (Proxy :: Proxy GetDeploymentInstance)

stopDeploymentResponseTest :: StopDeploymentResponse -> TestTree
stopDeploymentResponseTest = resp
    "StopDeploymentResponse"
    "fixture/StopDeploymentResponse"
    (Proxy :: Proxy StopDeployment)

getApplicationResponseTest :: GetApplicationResponse -> TestTree
getApplicationResponseTest = resp
    "GetApplicationResponse"
    "fixture/GetApplicationResponse"
    (Proxy :: Proxy GetApplication)

listDeploymentGroupsResponseTest :: ListDeploymentGroupsResponse -> TestTree
listDeploymentGroupsResponseTest = resp
    "ListDeploymentGroupsResponse"
    "fixture/ListDeploymentGroupsResponse"
    (Proxy :: Proxy ListDeploymentGroups)

registerOnPremisesInstanceResponseTest :: RegisterOnPremisesInstanceResponse -> TestTree
registerOnPremisesInstanceResponseTest = resp
    "RegisterOnPremisesInstanceResponse"
    "fixture/RegisterOnPremisesInstanceResponse"
    (Proxy :: Proxy RegisterOnPremisesInstance)

batchGetOnPremisesInstancesResponseTest :: BatchGetOnPremisesInstancesResponse -> TestTree
batchGetOnPremisesInstancesResponseTest = resp
    "BatchGetOnPremisesInstancesResponse"
    "fixture/BatchGetOnPremisesInstancesResponse"
    (Proxy :: Proxy BatchGetOnPremisesInstances)

listDeploymentConfigsResponseTest :: ListDeploymentConfigsResponse -> TestTree
listDeploymentConfigsResponseTest = resp
    "ListDeploymentConfigsResponse"
    "fixture/ListDeploymentConfigsResponse"
    (Proxy :: Proxy ListDeploymentConfigs)

createDeploymentGroupResponseTest :: CreateDeploymentGroupResponse -> TestTree
createDeploymentGroupResponseTest = resp
    "CreateDeploymentGroupResponse"
    "fixture/CreateDeploymentGroupResponse"
    (Proxy :: Proxy CreateDeploymentGroup)

getDeploymentGroupResponseTest :: GetDeploymentGroupResponse -> TestTree
getDeploymentGroupResponseTest = resp
    "GetDeploymentGroupResponse"
    "fixture/GetDeploymentGroupResponse"
    (Proxy :: Proxy GetDeploymentGroup)

listDeploymentsResponseTest :: ListDeploymentsResponse -> TestTree
listDeploymentsResponseTest = resp
    "ListDeploymentsResponse"
    "fixture/ListDeploymentsResponse"
    (Proxy :: Proxy ListDeployments)

listApplicationRevisionsResponseTest :: ListApplicationRevisionsResponse -> TestTree
listApplicationRevisionsResponseTest = resp
    "ListApplicationRevisionsResponse"
    "fixture/ListApplicationRevisionsResponse"
    (Proxy :: Proxy ListApplicationRevisions)

listApplicationsResponseTest :: ListApplicationsResponse -> TestTree
listApplicationsResponseTest = resp
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse"
    (Proxy :: Proxy ListApplications)

addTagsToOnPremisesInstancesResponseTest :: AddTagsToOnPremisesInstancesResponse -> TestTree
addTagsToOnPremisesInstancesResponseTest = resp
    "AddTagsToOnPremisesInstancesResponse"
    "fixture/AddTagsToOnPremisesInstancesResponse"
    (Proxy :: Proxy AddTagsToOnPremisesInstances)

listDeploymentInstancesResponseTest :: ListDeploymentInstancesResponse -> TestTree
listDeploymentInstancesResponseTest = resp
    "ListDeploymentInstancesResponse"
    "fixture/ListDeploymentInstancesResponse"
    (Proxy :: Proxy ListDeploymentInstances)
