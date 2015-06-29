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
--         [ addTagsToOnPremisesInstancesTest $
--             addTagsToOnPremisesInstances
--
--         , batchGetApplicationsTest $
--             batchGetApplications
--
--         , batchGetDeploymentsTest $
--             batchGetDeployments
--
--         , batchGetOnPremisesInstancesTest $
--             batchGetOnPremisesInstances
--
--         , createApplicationTest $
--             createApplication
--
--         , createDeploymentTest $
--             createDeployment
--
--         , createDeploymentConfigTest $
--             createDeploymentConfig
--
--         , createDeploymentGroupTest $
--             createDeploymentGroup
--
--         , deleteApplicationTest $
--             deleteApplication
--
--         , deleteDeploymentConfigTest $
--             deleteDeploymentConfig
--
--         , deleteDeploymentGroupTest $
--             deleteDeploymentGroup
--
--         , deregisterOnPremisesInstanceTest $
--             deregisterOnPremisesInstance
--
--         , getApplicationTest $
--             getApplication
--
--         , getApplicationRevisionTest $
--             getApplicationRevision
--
--         , getDeploymentTest $
--             getDeployment
--
--         , getDeploymentConfigTest $
--             getDeploymentConfig
--
--         , getDeploymentGroupTest $
--             getDeploymentGroup
--
--         , getDeploymentInstanceTest $
--             getDeploymentInstance
--
--         , getOnPremisesInstanceTest $
--             getOnPremisesInstance
--
--         , listApplicationRevisionsTest $
--             listApplicationRevisions
--
--         , listApplicationsTest $
--             listApplications
--
--         , listDeploymentConfigsTest $
--             listDeploymentConfigs
--
--         , listDeploymentGroupsTest $
--             listDeploymentGroups
--
--         , listDeploymentInstancesTest $
--             listDeploymentInstances
--
--         , listDeploymentsTest $
--             listDeployments
--
--         , listOnPremisesInstancesTest $
--             listOnPremisesInstances
--
--         , registerApplicationRevisionTest $
--             registerApplicationRevision
--
--         , registerOnPremisesInstanceTest $
--             registerOnPremisesInstance
--
--         , removeTagsFromOnPremisesInstancesTest $
--             removeTagsFromOnPremisesInstances
--
--         , stopDeploymentTest $
--             stopDeployment
--
--         , updateApplicationTest $
--             updateApplication
--
--         , updateDeploymentGroupTest $
--             updateDeploymentGroup
--
--           ]

--     , testGroup "response"
--         [ addTagsToOnPremisesInstancesResponseTest $
--             addTagsToOnPremisesInstancesResponse
--
--         , batchGetApplicationsResponseTest $
--             batchGetApplicationsResponse
--
--         , batchGetDeploymentsResponseTest $
--             batchGetDeploymentsResponse
--
--         , batchGetOnPremisesInstancesResponseTest $
--             batchGetOnPremisesInstancesResponse
--
--         , createApplicationResponseTest $
--             createApplicationResponse
--
--         , createDeploymentResponseTest $
--             createDeploymentResponse
--
--         , createDeploymentConfigResponseTest $
--             createDeploymentConfigResponse
--
--         , createDeploymentGroupResponseTest $
--             createDeploymentGroupResponse
--
--         , deleteApplicationResponseTest $
--             deleteApplicationResponse
--
--         , deleteDeploymentConfigResponseTest $
--             deleteDeploymentConfigResponse
--
--         , deleteDeploymentGroupResponseTest $
--             deleteDeploymentGroupResponse
--
--         , deregisterOnPremisesInstanceResponseTest $
--             deregisterOnPremisesInstanceResponse
--
--         , getApplicationResponseTest $
--             getApplicationResponse
--
--         , getApplicationRevisionResponseTest $
--             getApplicationRevisionResponse
--
--         , getDeploymentResponseTest $
--             getDeploymentResponse
--
--         , getDeploymentConfigResponseTest $
--             getDeploymentConfigResponse
--
--         , getDeploymentGroupResponseTest $
--             getDeploymentGroupResponse
--
--         , getDeploymentInstanceResponseTest $
--             getDeploymentInstanceResponse
--
--         , getOnPremisesInstanceResponseTest $
--             getOnPremisesInstanceResponse
--
--         , listApplicationRevisionsResponseTest $
--             listApplicationRevisionsResponse
--
--         , listApplicationsResponseTest $
--             listApplicationsResponse
--
--         , listDeploymentConfigsResponseTest $
--             listDeploymentConfigsResponse
--
--         , listDeploymentGroupsResponseTest $
--             listDeploymentGroupsResponse
--
--         , listDeploymentInstancesResponseTest $
--             listDeploymentInstancesResponse
--
--         , listDeploymentsResponseTest $
--             listDeploymentsResponse
--
--         , listOnPremisesInstancesResponseTest $
--             listOnPremisesInstancesResponse
--
--         , registerApplicationRevisionResponseTest $
--             registerApplicationRevisionResponse
--
--         , registerOnPremisesInstanceResponseTest $
--             registerOnPremisesInstanceResponse
--
--         , removeTagsFromOnPremisesInstancesResponseTest $
--             removeTagsFromOnPremisesInstancesResponse
--
--         , stopDeploymentResponseTest $
--             stopDeploymentResponse
--
--         , updateApplicationResponseTest $
--             updateApplicationResponse
--
--         , updateDeploymentGroupResponseTest $
--             updateDeploymentGroupResponse
--
--           ]
--     ]

-- Requests

addTagsToOnPremisesInstancesTest :: AddTagsToOnPremisesInstances -> TestTree
addTagsToOnPremisesInstancesTest = undefined

batchGetApplicationsTest :: BatchGetApplications -> TestTree
batchGetApplicationsTest = undefined

batchGetDeploymentsTest :: BatchGetDeployments -> TestTree
batchGetDeploymentsTest = undefined

batchGetOnPremisesInstancesTest :: BatchGetOnPremisesInstances -> TestTree
batchGetOnPremisesInstancesTest = undefined

createApplicationTest :: CreateApplication -> TestTree
createApplicationTest = undefined

createDeploymentTest :: CreateDeployment -> TestTree
createDeploymentTest = undefined

createDeploymentConfigTest :: CreateDeploymentConfig -> TestTree
createDeploymentConfigTest = undefined

createDeploymentGroupTest :: CreateDeploymentGroup -> TestTree
createDeploymentGroupTest = undefined

deleteApplicationTest :: DeleteApplication -> TestTree
deleteApplicationTest = undefined

deleteDeploymentConfigTest :: DeleteDeploymentConfig -> TestTree
deleteDeploymentConfigTest = undefined

deleteDeploymentGroupTest :: DeleteDeploymentGroup -> TestTree
deleteDeploymentGroupTest = undefined

deregisterOnPremisesInstanceTest :: DeregisterOnPremisesInstance -> TestTree
deregisterOnPremisesInstanceTest = undefined

getApplicationTest :: GetApplication -> TestTree
getApplicationTest = undefined

getApplicationRevisionTest :: GetApplicationRevision -> TestTree
getApplicationRevisionTest = undefined

getDeploymentTest :: GetDeployment -> TestTree
getDeploymentTest = undefined

getDeploymentConfigTest :: GetDeploymentConfig -> TestTree
getDeploymentConfigTest = undefined

getDeploymentGroupTest :: GetDeploymentGroup -> TestTree
getDeploymentGroupTest = undefined

getDeploymentInstanceTest :: GetDeploymentInstance -> TestTree
getDeploymentInstanceTest = undefined

getOnPremisesInstanceTest :: GetOnPremisesInstance -> TestTree
getOnPremisesInstanceTest = undefined

listApplicationRevisionsTest :: ListApplicationRevisions -> TestTree
listApplicationRevisionsTest = undefined

listApplicationsTest :: ListApplications -> TestTree
listApplicationsTest = undefined

listDeploymentConfigsTest :: ListDeploymentConfigs -> TestTree
listDeploymentConfigsTest = undefined

listDeploymentGroupsTest :: ListDeploymentGroups -> TestTree
listDeploymentGroupsTest = undefined

listDeploymentInstancesTest :: ListDeploymentInstances -> TestTree
listDeploymentInstancesTest = undefined

listDeploymentsTest :: ListDeployments -> TestTree
listDeploymentsTest = undefined

listOnPremisesInstancesTest :: ListOnPremisesInstances -> TestTree
listOnPremisesInstancesTest = undefined

registerApplicationRevisionTest :: RegisterApplicationRevision -> TestTree
registerApplicationRevisionTest = undefined

registerOnPremisesInstanceTest :: RegisterOnPremisesInstance -> TestTree
registerOnPremisesInstanceTest = undefined

removeTagsFromOnPremisesInstancesTest :: RemoveTagsFromOnPremisesInstances -> TestTree
removeTagsFromOnPremisesInstancesTest = undefined

stopDeploymentTest :: StopDeployment -> TestTree
stopDeploymentTest = undefined

updateApplicationTest :: UpdateApplication -> TestTree
updateApplicationTest = undefined

updateDeploymentGroupTest :: UpdateDeploymentGroup -> TestTree
updateDeploymentGroupTest = undefined

-- Responses

addTagsToOnPremisesInstancesResponseTest :: AddTagsToOnPremisesInstancesResponse -> TestTree
addTagsToOnPremisesInstancesResponseTest = resp
    "addTagsToOnPremisesInstancesResponse"
    "fixture/AddTagsToOnPremisesInstancesResponse"
    (Proxy :: Proxy AddTagsToOnPremisesInstances)

batchGetApplicationsResponseTest :: BatchGetApplicationsResponse -> TestTree
batchGetApplicationsResponseTest = resp
    "batchGetApplicationsResponse"
    "fixture/BatchGetApplicationsResponse"
    (Proxy :: Proxy BatchGetApplications)

batchGetDeploymentsResponseTest :: BatchGetDeploymentsResponse -> TestTree
batchGetDeploymentsResponseTest = resp
    "batchGetDeploymentsResponse"
    "fixture/BatchGetDeploymentsResponse"
    (Proxy :: Proxy BatchGetDeployments)

batchGetOnPremisesInstancesResponseTest :: BatchGetOnPremisesInstancesResponse -> TestTree
batchGetOnPremisesInstancesResponseTest = resp
    "batchGetOnPremisesInstancesResponse"
    "fixture/BatchGetOnPremisesInstancesResponse"
    (Proxy :: Proxy BatchGetOnPremisesInstances)

createApplicationResponseTest :: CreateApplicationResponse -> TestTree
createApplicationResponseTest = resp
    "createApplicationResponse"
    "fixture/CreateApplicationResponse"
    (Proxy :: Proxy CreateApplication)

createDeploymentResponseTest :: CreateDeploymentResponse -> TestTree
createDeploymentResponseTest = resp
    "createDeploymentResponse"
    "fixture/CreateDeploymentResponse"
    (Proxy :: Proxy CreateDeployment)

createDeploymentConfigResponseTest :: CreateDeploymentConfigResponse -> TestTree
createDeploymentConfigResponseTest = resp
    "createDeploymentConfigResponse"
    "fixture/CreateDeploymentConfigResponse"
    (Proxy :: Proxy CreateDeploymentConfig)

createDeploymentGroupResponseTest :: CreateDeploymentGroupResponse -> TestTree
createDeploymentGroupResponseTest = resp
    "createDeploymentGroupResponse"
    "fixture/CreateDeploymentGroupResponse"
    (Proxy :: Proxy CreateDeploymentGroup)

deleteApplicationResponseTest :: DeleteApplicationResponse -> TestTree
deleteApplicationResponseTest = resp
    "deleteApplicationResponse"
    "fixture/DeleteApplicationResponse"
    (Proxy :: Proxy DeleteApplication)

deleteDeploymentConfigResponseTest :: DeleteDeploymentConfigResponse -> TestTree
deleteDeploymentConfigResponseTest = resp
    "deleteDeploymentConfigResponse"
    "fixture/DeleteDeploymentConfigResponse"
    (Proxy :: Proxy DeleteDeploymentConfig)

deleteDeploymentGroupResponseTest :: DeleteDeploymentGroupResponse -> TestTree
deleteDeploymentGroupResponseTest = resp
    "deleteDeploymentGroupResponse"
    "fixture/DeleteDeploymentGroupResponse"
    (Proxy :: Proxy DeleteDeploymentGroup)

deregisterOnPremisesInstanceResponseTest :: DeregisterOnPremisesInstanceResponse -> TestTree
deregisterOnPremisesInstanceResponseTest = resp
    "deregisterOnPremisesInstanceResponse"
    "fixture/DeregisterOnPremisesInstanceResponse"
    (Proxy :: Proxy DeregisterOnPremisesInstance)

getApplicationResponseTest :: GetApplicationResponse -> TestTree
getApplicationResponseTest = resp
    "getApplicationResponse"
    "fixture/GetApplicationResponse"
    (Proxy :: Proxy GetApplication)

getApplicationRevisionResponseTest :: GetApplicationRevisionResponse -> TestTree
getApplicationRevisionResponseTest = resp
    "getApplicationRevisionResponse"
    "fixture/GetApplicationRevisionResponse"
    (Proxy :: Proxy GetApplicationRevision)

getDeploymentResponseTest :: GetDeploymentResponse -> TestTree
getDeploymentResponseTest = resp
    "getDeploymentResponse"
    "fixture/GetDeploymentResponse"
    (Proxy :: Proxy GetDeployment)

getDeploymentConfigResponseTest :: GetDeploymentConfigResponse -> TestTree
getDeploymentConfigResponseTest = resp
    "getDeploymentConfigResponse"
    "fixture/GetDeploymentConfigResponse"
    (Proxy :: Proxy GetDeploymentConfig)

getDeploymentGroupResponseTest :: GetDeploymentGroupResponse -> TestTree
getDeploymentGroupResponseTest = resp
    "getDeploymentGroupResponse"
    "fixture/GetDeploymentGroupResponse"
    (Proxy :: Proxy GetDeploymentGroup)

getDeploymentInstanceResponseTest :: GetDeploymentInstanceResponse -> TestTree
getDeploymentInstanceResponseTest = resp
    "getDeploymentInstanceResponse"
    "fixture/GetDeploymentInstanceResponse"
    (Proxy :: Proxy GetDeploymentInstance)

getOnPremisesInstanceResponseTest :: GetOnPremisesInstanceResponse -> TestTree
getOnPremisesInstanceResponseTest = resp
    "getOnPremisesInstanceResponse"
    "fixture/GetOnPremisesInstanceResponse"
    (Proxy :: Proxy GetOnPremisesInstance)

listApplicationRevisionsResponseTest :: ListApplicationRevisionsResponse -> TestTree
listApplicationRevisionsResponseTest = resp
    "listApplicationRevisionsResponse"
    "fixture/ListApplicationRevisionsResponse"
    (Proxy :: Proxy ListApplicationRevisions)

listApplicationsResponseTest :: ListApplicationsResponse -> TestTree
listApplicationsResponseTest = resp
    "listApplicationsResponse"
    "fixture/ListApplicationsResponse"
    (Proxy :: Proxy ListApplications)

listDeploymentConfigsResponseTest :: ListDeploymentConfigsResponse -> TestTree
listDeploymentConfigsResponseTest = resp
    "listDeploymentConfigsResponse"
    "fixture/ListDeploymentConfigsResponse"
    (Proxy :: Proxy ListDeploymentConfigs)

listDeploymentGroupsResponseTest :: ListDeploymentGroupsResponse -> TestTree
listDeploymentGroupsResponseTest = resp
    "listDeploymentGroupsResponse"
    "fixture/ListDeploymentGroupsResponse"
    (Proxy :: Proxy ListDeploymentGroups)

listDeploymentInstancesResponseTest :: ListDeploymentInstancesResponse -> TestTree
listDeploymentInstancesResponseTest = resp
    "listDeploymentInstancesResponse"
    "fixture/ListDeploymentInstancesResponse"
    (Proxy :: Proxy ListDeploymentInstances)

listDeploymentsResponseTest :: ListDeploymentsResponse -> TestTree
listDeploymentsResponseTest = resp
    "listDeploymentsResponse"
    "fixture/ListDeploymentsResponse"
    (Proxy :: Proxy ListDeployments)

listOnPremisesInstancesResponseTest :: ListOnPremisesInstancesResponse -> TestTree
listOnPremisesInstancesResponseTest = resp
    "listOnPremisesInstancesResponse"
    "fixture/ListOnPremisesInstancesResponse"
    (Proxy :: Proxy ListOnPremisesInstances)

registerApplicationRevisionResponseTest :: RegisterApplicationRevisionResponse -> TestTree
registerApplicationRevisionResponseTest = resp
    "registerApplicationRevisionResponse"
    "fixture/RegisterApplicationRevisionResponse"
    (Proxy :: Proxy RegisterApplicationRevision)

registerOnPremisesInstanceResponseTest :: RegisterOnPremisesInstanceResponse -> TestTree
registerOnPremisesInstanceResponseTest = resp
    "registerOnPremisesInstanceResponse"
    "fixture/RegisterOnPremisesInstanceResponse"
    (Proxy :: Proxy RegisterOnPremisesInstance)

removeTagsFromOnPremisesInstancesResponseTest :: RemoveTagsFromOnPremisesInstancesResponse -> TestTree
removeTagsFromOnPremisesInstancesResponseTest = resp
    "removeTagsFromOnPremisesInstancesResponse"
    "fixture/RemoveTagsFromOnPremisesInstancesResponse"
    (Proxy :: Proxy RemoveTagsFromOnPremisesInstances)

stopDeploymentResponseTest :: StopDeploymentResponse -> TestTree
stopDeploymentResponseTest = resp
    "stopDeploymentResponse"
    "fixture/StopDeploymentResponse"
    (Proxy :: Proxy StopDeployment)

updateApplicationResponseTest :: UpdateApplicationResponse -> TestTree
updateApplicationResponseTest = resp
    "updateApplicationResponse"
    "fixture/UpdateApplicationResponse"
    (Proxy :: Proxy UpdateApplication)

updateDeploymentGroupResponseTest :: UpdateDeploymentGroupResponse -> TestTree
updateDeploymentGroupResponseTest = resp
    "updateDeploymentGroupResponse"
    "fixture/UpdateDeploymentGroupResponse"
    (Proxy :: Proxy UpdateDeploymentGroup)
