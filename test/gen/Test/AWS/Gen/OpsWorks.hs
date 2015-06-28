-- Module      : Test.AWS.Gen.OpsWorks
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

module Test.AWS.Gen.OpsWorks where

import           Data.Proxy
import           Network.AWS.OpsWorks
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ describeRDSDBInstancesTest $
--             describeRDSDBInstances
--
--         , deleteStackTest $
--             deleteStack
--
--         , updateStackTest $
--             updateStack
--
--         , createLayerTest $
--             createLayer
--
--         , setLoadBasedAutoScalingTest $
--             setLoadBasedAutoScaling
--
--         , unassignVolumeTest $
--             unassignVolume
--
--         , deregisterRDSDBInstanceTest $
--             deregisterRDSDBInstance
--
--         , createInstanceTest $
--             createInstance
--
--         , registerElasticIPTest $
--             registerElasticIP
--
--         , describeAgentVersionsTest $
--             describeAgentVersions
--
--         , describeLayersTest $
--             describeLayers
--
--         , createDeploymentTest $
--             createDeployment
--
--         , deleteAppTest $
--             deleteApp
--
--         , updateAppTest $
--             updateApp
--
--         , deleteInstanceTest $
--             deleteInstance
--
--         , updateInstanceTest $
--             updateInstance
--
--         , describeStacksTest $
--             describeStacks
--
--         , deregisterVolumeTest $
--             deregisterVolume
--
--         , assignInstanceTest $
--             assignInstance
--
--         , rebootInstanceTest $
--             rebootInstance
--
--         , describeTimeBasedAutoScalingTest $
--             describeTimeBasedAutoScaling
--
--         , updateRDSDBInstanceTest $
--             updateRDSDBInstance
--
--         , stopStackTest $
--             stopStack
--
--         , describeVolumesTest $
--             describeVolumes
--
--         , disassociateElasticIPTest $
--             disassociateElasticIP
--
--         , stopInstanceTest $
--             stopInstance
--
--         , registerVolumeTest $
--             registerVolume
--
--         , setTimeBasedAutoScalingTest $
--             setTimeBasedAutoScaling
--
--         , deregisterElasticIPTest $
--             deregisterElasticIP
--
--         , attachElasticLoadBalancerTest $
--             attachElasticLoadBalancer
--
--         , describeUserProfilesTest $
--             describeUserProfiles
--
--         , describeStackSummaryTest $
--             describeStackSummary
--
--         , describeAppsTest $
--             describeApps
--
--         , updateMyUserProfileTest $
--             updateMyUserProfile
--
--         , describeInstancesTest $
--             describeInstances
--
--         , describeDeploymentsTest $
--             describeDeployments
--
--         , createStackTest $
--             createStack
--
--         , grantAccessTest $
--             grantAccess
--
--         , describeElasticIPsTest $
--             describeElasticIPs
--
--         , deleteLayerTest $
--             deleteLayer
--
--         , updateLayerTest $
--             updateLayer
--
--         , cloneStackTest $
--             cloneStack
--
--         , getHostnameSuggestionTest $
--             getHostnameSuggestion
--
--         , createAppTest $
--             createApp
--
--         , describePermissionsTest $
--             describePermissions
--
--         , updateElasticIPTest $
--             updateElasticIP
--
--         , describeLoadBasedAutoScalingTest $
--             describeLoadBasedAutoScaling
--
--         , registerInstanceTest $
--             registerInstance
--
--         , associateElasticIPTest $
--             associateElasticIP
--
--         , detachElasticLoadBalancerTest $
--             detachElasticLoadBalancer
--
--         , describeStackProvisioningParametersTest $
--             describeStackProvisioningParameters
--
--         , describeMyUserProfileTest $
--             describeMyUserProfile
--
--         , unassignInstanceTest $
--             unassignInstance
--
--         , registerRDSDBInstanceTest $
--             registerRDSDBInstance
--
--         , deleteUserProfileTest $
--             deleteUserProfile
--
--         , updateUserProfileTest $
--             updateUserProfile
--
--         , describeServiceErrorsTest $
--             describeServiceErrors
--
--         , startStackTest $
--             startStack
--
--         , createUserProfileTest $
--             createUserProfile
--
--         , describeCommandsTest $
--             describeCommands
--
--         , describeElasticLoadBalancersTest $
--             describeElasticLoadBalancers
--
--         , deregisterInstanceTest $
--             deregisterInstance
--
--         , describeRAIDArraysTest $
--             describeRAIDArrays
--
--         , setPermissionTest $
--             setPermission
--
--         , updateVolumeTest $
--             updateVolume
--
--         , assignVolumeTest $
--             assignVolume
--
--         , startInstanceTest $
--             startInstance
--
--           ]

--     , testGroup "response"
--         [ describeRDSDBInstancesResponseTest $
--             describeRDSDBInstancesResponse
--
--         , deleteStackResponseTest $
--             deleteStackResponse
--
--         , updateStackResponseTest $
--             updateStackResponse
--
--         , createLayerResponseTest $
--             createLayerResponse
--
--         , setLoadBasedAutoScalingResponseTest $
--             setLoadBasedAutoScalingResponse
--
--         , unassignVolumeResponseTest $
--             unassignVolumeResponse
--
--         , deregisterRDSDBInstanceResponseTest $
--             deregisterRDSDBInstanceResponse
--
--         , createInstanceResponseTest $
--             createInstanceResponse
--
--         , registerElasticIPResponseTest $
--             registerElasticIPResponse
--
--         , describeAgentVersionsResponseTest $
--             describeAgentVersionsResponse
--
--         , describeLayersResponseTest $
--             describeLayersResponse
--
--         , createDeploymentResponseTest $
--             createDeploymentResponse
--
--         , deleteAppResponseTest $
--             deleteAppResponse
--
--         , updateAppResponseTest $
--             updateAppResponse
--
--         , deleteInstanceResponseTest $
--             deleteInstanceResponse
--
--         , updateInstanceResponseTest $
--             updateInstanceResponse
--
--         , describeStacksResponseTest $
--             describeStacksResponse
--
--         , deregisterVolumeResponseTest $
--             deregisterVolumeResponse
--
--         , assignInstanceResponseTest $
--             assignInstanceResponse
--
--         , rebootInstanceResponseTest $
--             rebootInstanceResponse
--
--         , describeTimeBasedAutoScalingResponseTest $
--             describeTimeBasedAutoScalingResponse
--
--         , updateRDSDBInstanceResponseTest $
--             updateRDSDBInstanceResponse
--
--         , stopStackResponseTest $
--             stopStackResponse
--
--         , describeVolumesResponseTest $
--             describeVolumesResponse
--
--         , disassociateElasticIPResponseTest $
--             disassociateElasticIPResponse
--
--         , stopInstanceResponseTest $
--             stopInstanceResponse
--
--         , registerVolumeResponseTest $
--             registerVolumeResponse
--
--         , setTimeBasedAutoScalingResponseTest $
--             setTimeBasedAutoScalingResponse
--
--         , deregisterElasticIPResponseTest $
--             deregisterElasticIPResponse
--
--         , attachElasticLoadBalancerResponseTest $
--             attachElasticLoadBalancerResponse
--
--         , describeUserProfilesResponseTest $
--             describeUserProfilesResponse
--
--         , describeStackSummaryResponseTest $
--             describeStackSummaryResponse
--
--         , describeAppsResponseTest $
--             describeAppsResponse
--
--         , updateMyUserProfileResponseTest $
--             updateMyUserProfileResponse
--
--         , describeInstancesResponseTest $
--             describeInstancesResponse
--
--         , describeDeploymentsResponseTest $
--             describeDeploymentsResponse
--
--         , createStackResponseTest $
--             createStackResponse
--
--         , grantAccessResponseTest $
--             grantAccessResponse
--
--         , describeElasticIPsResponseTest $
--             describeElasticIPsResponse
--
--         , deleteLayerResponseTest $
--             deleteLayerResponse
--
--         , updateLayerResponseTest $
--             updateLayerResponse
--
--         , cloneStackResponseTest $
--             cloneStackResponse
--
--         , getHostnameSuggestionResponseTest $
--             getHostnameSuggestionResponse
--
--         , createAppResponseTest $
--             createAppResponse
--
--         , describePermissionsResponseTest $
--             describePermissionsResponse
--
--         , updateElasticIPResponseTest $
--             updateElasticIPResponse
--
--         , describeLoadBasedAutoScalingResponseTest $
--             describeLoadBasedAutoScalingResponse
--
--         , registerInstanceResponseTest $
--             registerInstanceResponse
--
--         , associateElasticIPResponseTest $
--             associateElasticIPResponse
--
--         , detachElasticLoadBalancerResponseTest $
--             detachElasticLoadBalancerResponse
--
--         , describeStackProvisioningParametersResponseTest $
--             describeStackProvisioningParametersResponse
--
--         , describeMyUserProfileResponseTest $
--             describeMyUserProfileResponse
--
--         , unassignInstanceResponseTest $
--             unassignInstanceResponse
--
--         , registerRDSDBInstanceResponseTest $
--             registerRDSDBInstanceResponse
--
--         , deleteUserProfileResponseTest $
--             deleteUserProfileResponse
--
--         , updateUserProfileResponseTest $
--             updateUserProfileResponse
--
--         , describeServiceErrorsResponseTest $
--             describeServiceErrorsResponse
--
--         , startStackResponseTest $
--             startStackResponse
--
--         , createUserProfileResponseTest $
--             createUserProfileResponse
--
--         , describeCommandsResponseTest $
--             describeCommandsResponse
--
--         , describeElasticLoadBalancersResponseTest $
--             describeElasticLoadBalancersResponse
--
--         , deregisterInstanceResponseTest $
--             deregisterInstanceResponse
--
--         , describeRAIDArraysResponseTest $
--             describeRAIDArraysResponse
--
--         , setPermissionResponseTest $
--             setPermissionResponse
--
--         , updateVolumeResponseTest $
--             updateVolumeResponse
--
--         , assignVolumeResponseTest $
--             assignVolumeResponse
--
--         , startInstanceResponseTest $
--             startInstanceResponse
--
--           ]
--     ]

-- Requests

describeRDSDBInstancesTest :: DescribeRDSDBInstances -> TestTree
describeRDSDBInstancesTest = undefined

deleteStackTest :: DeleteStack -> TestTree
deleteStackTest = undefined

updateStackTest :: UpdateStack -> TestTree
updateStackTest = undefined

createLayerTest :: CreateLayer -> TestTree
createLayerTest = undefined

setLoadBasedAutoScalingTest :: SetLoadBasedAutoScaling -> TestTree
setLoadBasedAutoScalingTest = undefined

unassignVolumeTest :: UnassignVolume -> TestTree
unassignVolumeTest = undefined

deregisterRDSDBInstanceTest :: DeregisterRDSDBInstance -> TestTree
deregisterRDSDBInstanceTest = undefined

createInstanceTest :: CreateInstance -> TestTree
createInstanceTest = undefined

registerElasticIPTest :: RegisterElasticIP -> TestTree
registerElasticIPTest = undefined

describeAgentVersionsTest :: DescribeAgentVersions -> TestTree
describeAgentVersionsTest = undefined

describeLayersTest :: DescribeLayers -> TestTree
describeLayersTest = undefined

createDeploymentTest :: CreateDeployment -> TestTree
createDeploymentTest = undefined

deleteAppTest :: DeleteApp -> TestTree
deleteAppTest = undefined

updateAppTest :: UpdateApp -> TestTree
updateAppTest = undefined

deleteInstanceTest :: DeleteInstance -> TestTree
deleteInstanceTest = undefined

updateInstanceTest :: UpdateInstance -> TestTree
updateInstanceTest = undefined

describeStacksTest :: DescribeStacks -> TestTree
describeStacksTest = undefined

deregisterVolumeTest :: DeregisterVolume -> TestTree
deregisterVolumeTest = undefined

assignInstanceTest :: AssignInstance -> TestTree
assignInstanceTest = undefined

rebootInstanceTest :: RebootInstance -> TestTree
rebootInstanceTest = undefined

describeTimeBasedAutoScalingTest :: DescribeTimeBasedAutoScaling -> TestTree
describeTimeBasedAutoScalingTest = undefined

updateRDSDBInstanceTest :: UpdateRDSDBInstance -> TestTree
updateRDSDBInstanceTest = undefined

stopStackTest :: StopStack -> TestTree
stopStackTest = undefined

describeVolumesTest :: DescribeVolumes -> TestTree
describeVolumesTest = undefined

disassociateElasticIPTest :: DisassociateElasticIP -> TestTree
disassociateElasticIPTest = undefined

stopInstanceTest :: StopInstance -> TestTree
stopInstanceTest = undefined

registerVolumeTest :: RegisterVolume -> TestTree
registerVolumeTest = undefined

setTimeBasedAutoScalingTest :: SetTimeBasedAutoScaling -> TestTree
setTimeBasedAutoScalingTest = undefined

deregisterElasticIPTest :: DeregisterElasticIP -> TestTree
deregisterElasticIPTest = undefined

attachElasticLoadBalancerTest :: AttachElasticLoadBalancer -> TestTree
attachElasticLoadBalancerTest = undefined

describeUserProfilesTest :: DescribeUserProfiles -> TestTree
describeUserProfilesTest = undefined

describeStackSummaryTest :: DescribeStackSummary -> TestTree
describeStackSummaryTest = undefined

describeAppsTest :: DescribeApps -> TestTree
describeAppsTest = undefined

updateMyUserProfileTest :: UpdateMyUserProfile -> TestTree
updateMyUserProfileTest = undefined

describeInstancesTest :: DescribeInstances -> TestTree
describeInstancesTest = undefined

describeDeploymentsTest :: DescribeDeployments -> TestTree
describeDeploymentsTest = undefined

createStackTest :: CreateStack -> TestTree
createStackTest = undefined

grantAccessTest :: GrantAccess -> TestTree
grantAccessTest = undefined

describeElasticIPsTest :: DescribeElasticIPs -> TestTree
describeElasticIPsTest = undefined

deleteLayerTest :: DeleteLayer -> TestTree
deleteLayerTest = undefined

updateLayerTest :: UpdateLayer -> TestTree
updateLayerTest = undefined

cloneStackTest :: CloneStack -> TestTree
cloneStackTest = undefined

getHostnameSuggestionTest :: GetHostnameSuggestion -> TestTree
getHostnameSuggestionTest = undefined

createAppTest :: CreateApp -> TestTree
createAppTest = undefined

describePermissionsTest :: DescribePermissions -> TestTree
describePermissionsTest = undefined

updateElasticIPTest :: UpdateElasticIP -> TestTree
updateElasticIPTest = undefined

describeLoadBasedAutoScalingTest :: DescribeLoadBasedAutoScaling -> TestTree
describeLoadBasedAutoScalingTest = undefined

registerInstanceTest :: RegisterInstance -> TestTree
registerInstanceTest = undefined

associateElasticIPTest :: AssociateElasticIP -> TestTree
associateElasticIPTest = undefined

detachElasticLoadBalancerTest :: DetachElasticLoadBalancer -> TestTree
detachElasticLoadBalancerTest = undefined

describeStackProvisioningParametersTest :: DescribeStackProvisioningParameters -> TestTree
describeStackProvisioningParametersTest = undefined

describeMyUserProfileTest :: DescribeMyUserProfile -> TestTree
describeMyUserProfileTest = undefined

unassignInstanceTest :: UnassignInstance -> TestTree
unassignInstanceTest = undefined

registerRDSDBInstanceTest :: RegisterRDSDBInstance -> TestTree
registerRDSDBInstanceTest = undefined

deleteUserProfileTest :: DeleteUserProfile -> TestTree
deleteUserProfileTest = undefined

updateUserProfileTest :: UpdateUserProfile -> TestTree
updateUserProfileTest = undefined

describeServiceErrorsTest :: DescribeServiceErrors -> TestTree
describeServiceErrorsTest = undefined

startStackTest :: StartStack -> TestTree
startStackTest = undefined

createUserProfileTest :: CreateUserProfile -> TestTree
createUserProfileTest = undefined

describeCommandsTest :: DescribeCommands -> TestTree
describeCommandsTest = undefined

describeElasticLoadBalancersTest :: DescribeElasticLoadBalancers -> TestTree
describeElasticLoadBalancersTest = undefined

deregisterInstanceTest :: DeregisterInstance -> TestTree
deregisterInstanceTest = undefined

describeRAIDArraysTest :: DescribeRAIDArrays -> TestTree
describeRAIDArraysTest = undefined

setPermissionTest :: SetPermission -> TestTree
setPermissionTest = undefined

updateVolumeTest :: UpdateVolume -> TestTree
updateVolumeTest = undefined

assignVolumeTest :: AssignVolume -> TestTree
assignVolumeTest = undefined

startInstanceTest :: StartInstance -> TestTree
startInstanceTest = undefined

-- Responses

describeRDSDBInstancesResponseTest :: DescribeRDSDBInstancesResponse -> TestTree
describeRDSDBInstancesResponseTest = resp
    "DescribeRDSDBInstancesResponse"
    "fixture/OpsWorks/DescribeRDSDBInstancesResponse"
    (Proxy :: Proxy DescribeRDSDBInstances)

deleteStackResponseTest :: DeleteStackResponse -> TestTree
deleteStackResponseTest = resp
    "DeleteStackResponse"
    "fixture/OpsWorks/DeleteStackResponse"
    (Proxy :: Proxy DeleteStack)

updateStackResponseTest :: UpdateStackResponse -> TestTree
updateStackResponseTest = resp
    "UpdateStackResponse"
    "fixture/OpsWorks/UpdateStackResponse"
    (Proxy :: Proxy UpdateStack)

createLayerResponseTest :: CreateLayerResponse -> TestTree
createLayerResponseTest = resp
    "CreateLayerResponse"
    "fixture/OpsWorks/CreateLayerResponse"
    (Proxy :: Proxy CreateLayer)

setLoadBasedAutoScalingResponseTest :: SetLoadBasedAutoScalingResponse -> TestTree
setLoadBasedAutoScalingResponseTest = resp
    "SetLoadBasedAutoScalingResponse"
    "fixture/OpsWorks/SetLoadBasedAutoScalingResponse"
    (Proxy :: Proxy SetLoadBasedAutoScaling)

unassignVolumeResponseTest :: UnassignVolumeResponse -> TestTree
unassignVolumeResponseTest = resp
    "UnassignVolumeResponse"
    "fixture/OpsWorks/UnassignVolumeResponse"
    (Proxy :: Proxy UnassignVolume)

deregisterRDSDBInstanceResponseTest :: DeregisterRDSDBInstanceResponse -> TestTree
deregisterRDSDBInstanceResponseTest = resp
    "DeregisterRDSDBInstanceResponse"
    "fixture/OpsWorks/DeregisterRDSDBInstanceResponse"
    (Proxy :: Proxy DeregisterRDSDBInstance)

createInstanceResponseTest :: CreateInstanceResponse -> TestTree
createInstanceResponseTest = resp
    "CreateInstanceResponse"
    "fixture/OpsWorks/CreateInstanceResponse"
    (Proxy :: Proxy CreateInstance)

registerElasticIPResponseTest :: RegisterElasticIPResponse -> TestTree
registerElasticIPResponseTest = resp
    "RegisterElasticIPResponse"
    "fixture/OpsWorks/RegisterElasticIPResponse"
    (Proxy :: Proxy RegisterElasticIP)

describeAgentVersionsResponseTest :: DescribeAgentVersionsResponse -> TestTree
describeAgentVersionsResponseTest = resp
    "DescribeAgentVersionsResponse"
    "fixture/OpsWorks/DescribeAgentVersionsResponse"
    (Proxy :: Proxy DescribeAgentVersions)

describeLayersResponseTest :: DescribeLayersResponse -> TestTree
describeLayersResponseTest = resp
    "DescribeLayersResponse"
    "fixture/OpsWorks/DescribeLayersResponse"
    (Proxy :: Proxy DescribeLayers)

createDeploymentResponseTest :: CreateDeploymentResponse -> TestTree
createDeploymentResponseTest = resp
    "CreateDeploymentResponse"
    "fixture/OpsWorks/CreateDeploymentResponse"
    (Proxy :: Proxy CreateDeployment)

deleteAppResponseTest :: DeleteAppResponse -> TestTree
deleteAppResponseTest = resp
    "DeleteAppResponse"
    "fixture/OpsWorks/DeleteAppResponse"
    (Proxy :: Proxy DeleteApp)

updateAppResponseTest :: UpdateAppResponse -> TestTree
updateAppResponseTest = resp
    "UpdateAppResponse"
    "fixture/OpsWorks/UpdateAppResponse"
    (Proxy :: Proxy UpdateApp)

deleteInstanceResponseTest :: DeleteInstanceResponse -> TestTree
deleteInstanceResponseTest = resp
    "DeleteInstanceResponse"
    "fixture/OpsWorks/DeleteInstanceResponse"
    (Proxy :: Proxy DeleteInstance)

updateInstanceResponseTest :: UpdateInstanceResponse -> TestTree
updateInstanceResponseTest = resp
    "UpdateInstanceResponse"
    "fixture/OpsWorks/UpdateInstanceResponse"
    (Proxy :: Proxy UpdateInstance)

describeStacksResponseTest :: DescribeStacksResponse -> TestTree
describeStacksResponseTest = resp
    "DescribeStacksResponse"
    "fixture/OpsWorks/DescribeStacksResponse"
    (Proxy :: Proxy DescribeStacks)

deregisterVolumeResponseTest :: DeregisterVolumeResponse -> TestTree
deregisterVolumeResponseTest = resp
    "DeregisterVolumeResponse"
    "fixture/OpsWorks/DeregisterVolumeResponse"
    (Proxy :: Proxy DeregisterVolume)

assignInstanceResponseTest :: AssignInstanceResponse -> TestTree
assignInstanceResponseTest = resp
    "AssignInstanceResponse"
    "fixture/OpsWorks/AssignInstanceResponse"
    (Proxy :: Proxy AssignInstance)

rebootInstanceResponseTest :: RebootInstanceResponse -> TestTree
rebootInstanceResponseTest = resp
    "RebootInstanceResponse"
    "fixture/OpsWorks/RebootInstanceResponse"
    (Proxy :: Proxy RebootInstance)

describeTimeBasedAutoScalingResponseTest :: DescribeTimeBasedAutoScalingResponse -> TestTree
describeTimeBasedAutoScalingResponseTest = resp
    "DescribeTimeBasedAutoScalingResponse"
    "fixture/OpsWorks/DescribeTimeBasedAutoScalingResponse"
    (Proxy :: Proxy DescribeTimeBasedAutoScaling)

updateRDSDBInstanceResponseTest :: UpdateRDSDBInstanceResponse -> TestTree
updateRDSDBInstanceResponseTest = resp
    "UpdateRDSDBInstanceResponse"
    "fixture/OpsWorks/UpdateRDSDBInstanceResponse"
    (Proxy :: Proxy UpdateRDSDBInstance)

stopStackResponseTest :: StopStackResponse -> TestTree
stopStackResponseTest = resp
    "StopStackResponse"
    "fixture/OpsWorks/StopStackResponse"
    (Proxy :: Proxy StopStack)

describeVolumesResponseTest :: DescribeVolumesResponse -> TestTree
describeVolumesResponseTest = resp
    "DescribeVolumesResponse"
    "fixture/OpsWorks/DescribeVolumesResponse"
    (Proxy :: Proxy DescribeVolumes)

disassociateElasticIPResponseTest :: DisassociateElasticIPResponse -> TestTree
disassociateElasticIPResponseTest = resp
    "DisassociateElasticIPResponse"
    "fixture/OpsWorks/DisassociateElasticIPResponse"
    (Proxy :: Proxy DisassociateElasticIP)

stopInstanceResponseTest :: StopInstanceResponse -> TestTree
stopInstanceResponseTest = resp
    "StopInstanceResponse"
    "fixture/OpsWorks/StopInstanceResponse"
    (Proxy :: Proxy StopInstance)

registerVolumeResponseTest :: RegisterVolumeResponse -> TestTree
registerVolumeResponseTest = resp
    "RegisterVolumeResponse"
    "fixture/OpsWorks/RegisterVolumeResponse"
    (Proxy :: Proxy RegisterVolume)

setTimeBasedAutoScalingResponseTest :: SetTimeBasedAutoScalingResponse -> TestTree
setTimeBasedAutoScalingResponseTest = resp
    "SetTimeBasedAutoScalingResponse"
    "fixture/OpsWorks/SetTimeBasedAutoScalingResponse"
    (Proxy :: Proxy SetTimeBasedAutoScaling)

deregisterElasticIPResponseTest :: DeregisterElasticIPResponse -> TestTree
deregisterElasticIPResponseTest = resp
    "DeregisterElasticIPResponse"
    "fixture/OpsWorks/DeregisterElasticIPResponse"
    (Proxy :: Proxy DeregisterElasticIP)

attachElasticLoadBalancerResponseTest :: AttachElasticLoadBalancerResponse -> TestTree
attachElasticLoadBalancerResponseTest = resp
    "AttachElasticLoadBalancerResponse"
    "fixture/OpsWorks/AttachElasticLoadBalancerResponse"
    (Proxy :: Proxy AttachElasticLoadBalancer)

describeUserProfilesResponseTest :: DescribeUserProfilesResponse -> TestTree
describeUserProfilesResponseTest = resp
    "DescribeUserProfilesResponse"
    "fixture/OpsWorks/DescribeUserProfilesResponse"
    (Proxy :: Proxy DescribeUserProfiles)

describeStackSummaryResponseTest :: DescribeStackSummaryResponse -> TestTree
describeStackSummaryResponseTest = resp
    "DescribeStackSummaryResponse"
    "fixture/OpsWorks/DescribeStackSummaryResponse"
    (Proxy :: Proxy DescribeStackSummary)

describeAppsResponseTest :: DescribeAppsResponse -> TestTree
describeAppsResponseTest = resp
    "DescribeAppsResponse"
    "fixture/OpsWorks/DescribeAppsResponse"
    (Proxy :: Proxy DescribeApps)

updateMyUserProfileResponseTest :: UpdateMyUserProfileResponse -> TestTree
updateMyUserProfileResponseTest = resp
    "UpdateMyUserProfileResponse"
    "fixture/OpsWorks/UpdateMyUserProfileResponse"
    (Proxy :: Proxy UpdateMyUserProfile)

describeInstancesResponseTest :: DescribeInstancesResponse -> TestTree
describeInstancesResponseTest = resp
    "DescribeInstancesResponse"
    "fixture/OpsWorks/DescribeInstancesResponse"
    (Proxy :: Proxy DescribeInstances)

describeDeploymentsResponseTest :: DescribeDeploymentsResponse -> TestTree
describeDeploymentsResponseTest = resp
    "DescribeDeploymentsResponse"
    "fixture/OpsWorks/DescribeDeploymentsResponse"
    (Proxy :: Proxy DescribeDeployments)

createStackResponseTest :: CreateStackResponse -> TestTree
createStackResponseTest = resp
    "CreateStackResponse"
    "fixture/OpsWorks/CreateStackResponse"
    (Proxy :: Proxy CreateStack)

grantAccessResponseTest :: GrantAccessResponse -> TestTree
grantAccessResponseTest = resp
    "GrantAccessResponse"
    "fixture/OpsWorks/GrantAccessResponse"
    (Proxy :: Proxy GrantAccess)

describeElasticIPsResponseTest :: DescribeElasticIPsResponse -> TestTree
describeElasticIPsResponseTest = resp
    "DescribeElasticIPsResponse"
    "fixture/OpsWorks/DescribeElasticIPsResponse"
    (Proxy :: Proxy DescribeElasticIPs)

deleteLayerResponseTest :: DeleteLayerResponse -> TestTree
deleteLayerResponseTest = resp
    "DeleteLayerResponse"
    "fixture/OpsWorks/DeleteLayerResponse"
    (Proxy :: Proxy DeleteLayer)

updateLayerResponseTest :: UpdateLayerResponse -> TestTree
updateLayerResponseTest = resp
    "UpdateLayerResponse"
    "fixture/OpsWorks/UpdateLayerResponse"
    (Proxy :: Proxy UpdateLayer)

cloneStackResponseTest :: CloneStackResponse -> TestTree
cloneStackResponseTest = resp
    "CloneStackResponse"
    "fixture/OpsWorks/CloneStackResponse"
    (Proxy :: Proxy CloneStack)

getHostnameSuggestionResponseTest :: GetHostnameSuggestionResponse -> TestTree
getHostnameSuggestionResponseTest = resp
    "GetHostnameSuggestionResponse"
    "fixture/OpsWorks/GetHostnameSuggestionResponse"
    (Proxy :: Proxy GetHostnameSuggestion)

createAppResponseTest :: CreateAppResponse -> TestTree
createAppResponseTest = resp
    "CreateAppResponse"
    "fixture/OpsWorks/CreateAppResponse"
    (Proxy :: Proxy CreateApp)

describePermissionsResponseTest :: DescribePermissionsResponse -> TestTree
describePermissionsResponseTest = resp
    "DescribePermissionsResponse"
    "fixture/OpsWorks/DescribePermissionsResponse"
    (Proxy :: Proxy DescribePermissions)

updateElasticIPResponseTest :: UpdateElasticIPResponse -> TestTree
updateElasticIPResponseTest = resp
    "UpdateElasticIPResponse"
    "fixture/OpsWorks/UpdateElasticIPResponse"
    (Proxy :: Proxy UpdateElasticIP)

describeLoadBasedAutoScalingResponseTest :: DescribeLoadBasedAutoScalingResponse -> TestTree
describeLoadBasedAutoScalingResponseTest = resp
    "DescribeLoadBasedAutoScalingResponse"
    "fixture/OpsWorks/DescribeLoadBasedAutoScalingResponse"
    (Proxy :: Proxy DescribeLoadBasedAutoScaling)

registerInstanceResponseTest :: RegisterInstanceResponse -> TestTree
registerInstanceResponseTest = resp
    "RegisterInstanceResponse"
    "fixture/OpsWorks/RegisterInstanceResponse"
    (Proxy :: Proxy RegisterInstance)

associateElasticIPResponseTest :: AssociateElasticIPResponse -> TestTree
associateElasticIPResponseTest = resp
    "AssociateElasticIPResponse"
    "fixture/OpsWorks/AssociateElasticIPResponse"
    (Proxy :: Proxy AssociateElasticIP)

detachElasticLoadBalancerResponseTest :: DetachElasticLoadBalancerResponse -> TestTree
detachElasticLoadBalancerResponseTest = resp
    "DetachElasticLoadBalancerResponse"
    "fixture/OpsWorks/DetachElasticLoadBalancerResponse"
    (Proxy :: Proxy DetachElasticLoadBalancer)

describeStackProvisioningParametersResponseTest :: DescribeStackProvisioningParametersResponse -> TestTree
describeStackProvisioningParametersResponseTest = resp
    "DescribeStackProvisioningParametersResponse"
    "fixture/OpsWorks/DescribeStackProvisioningParametersResponse"
    (Proxy :: Proxy DescribeStackProvisioningParameters)

describeMyUserProfileResponseTest :: DescribeMyUserProfileResponse -> TestTree
describeMyUserProfileResponseTest = resp
    "DescribeMyUserProfileResponse"
    "fixture/OpsWorks/DescribeMyUserProfileResponse"
    (Proxy :: Proxy DescribeMyUserProfile)

unassignInstanceResponseTest :: UnassignInstanceResponse -> TestTree
unassignInstanceResponseTest = resp
    "UnassignInstanceResponse"
    "fixture/OpsWorks/UnassignInstanceResponse"
    (Proxy :: Proxy UnassignInstance)

registerRDSDBInstanceResponseTest :: RegisterRDSDBInstanceResponse -> TestTree
registerRDSDBInstanceResponseTest = resp
    "RegisterRDSDBInstanceResponse"
    "fixture/OpsWorks/RegisterRDSDBInstanceResponse"
    (Proxy :: Proxy RegisterRDSDBInstance)

deleteUserProfileResponseTest :: DeleteUserProfileResponse -> TestTree
deleteUserProfileResponseTest = resp
    "DeleteUserProfileResponse"
    "fixture/OpsWorks/DeleteUserProfileResponse"
    (Proxy :: Proxy DeleteUserProfile)

updateUserProfileResponseTest :: UpdateUserProfileResponse -> TestTree
updateUserProfileResponseTest = resp
    "UpdateUserProfileResponse"
    "fixture/OpsWorks/UpdateUserProfileResponse"
    (Proxy :: Proxy UpdateUserProfile)

describeServiceErrorsResponseTest :: DescribeServiceErrorsResponse -> TestTree
describeServiceErrorsResponseTest = resp
    "DescribeServiceErrorsResponse"
    "fixture/OpsWorks/DescribeServiceErrorsResponse"
    (Proxy :: Proxy DescribeServiceErrors)

startStackResponseTest :: StartStackResponse -> TestTree
startStackResponseTest = resp
    "StartStackResponse"
    "fixture/OpsWorks/StartStackResponse"
    (Proxy :: Proxy StartStack)

createUserProfileResponseTest :: CreateUserProfileResponse -> TestTree
createUserProfileResponseTest = resp
    "CreateUserProfileResponse"
    "fixture/OpsWorks/CreateUserProfileResponse"
    (Proxy :: Proxy CreateUserProfile)

describeCommandsResponseTest :: DescribeCommandsResponse -> TestTree
describeCommandsResponseTest = resp
    "DescribeCommandsResponse"
    "fixture/OpsWorks/DescribeCommandsResponse"
    (Proxy :: Proxy DescribeCommands)

describeElasticLoadBalancersResponseTest :: DescribeElasticLoadBalancersResponse -> TestTree
describeElasticLoadBalancersResponseTest = resp
    "DescribeElasticLoadBalancersResponse"
    "fixture/OpsWorks/DescribeElasticLoadBalancersResponse"
    (Proxy :: Proxy DescribeElasticLoadBalancers)

deregisterInstanceResponseTest :: DeregisterInstanceResponse -> TestTree
deregisterInstanceResponseTest = resp
    "DeregisterInstanceResponse"
    "fixture/OpsWorks/DeregisterInstanceResponse"
    (Proxy :: Proxy DeregisterInstance)

describeRAIDArraysResponseTest :: DescribeRAIDArraysResponse -> TestTree
describeRAIDArraysResponseTest = resp
    "DescribeRAIDArraysResponse"
    "fixture/OpsWorks/DescribeRAIDArraysResponse"
    (Proxy :: Proxy DescribeRAIDArrays)

setPermissionResponseTest :: SetPermissionResponse -> TestTree
setPermissionResponseTest = resp
    "SetPermissionResponse"
    "fixture/OpsWorks/SetPermissionResponse"
    (Proxy :: Proxy SetPermission)

updateVolumeResponseTest :: UpdateVolumeResponse -> TestTree
updateVolumeResponseTest = resp
    "UpdateVolumeResponse"
    "fixture/OpsWorks/UpdateVolumeResponse"
    (Proxy :: Proxy UpdateVolume)

assignVolumeResponseTest :: AssignVolumeResponse -> TestTree
assignVolumeResponseTest = resp
    "AssignVolumeResponse"
    "fixture/OpsWorks/AssignVolumeResponse"
    (Proxy :: Proxy AssignVolume)

startInstanceResponseTest :: StartInstanceResponse -> TestTree
startInstanceResponseTest = resp
    "StartInstanceResponse"
    "fixture/OpsWorks/StartInstanceResponse"
    (Proxy :: Proxy StartInstance)
