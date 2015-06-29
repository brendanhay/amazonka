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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.OpsWorks

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ assignInstanceTest $
--             assignInstance
--
--         , assignVolumeTest $
--             assignVolume
--
--         , associateElasticIPTest $
--             associateElasticIP
--
--         , attachElasticLoadBalancerTest $
--             attachElasticLoadBalancer
--
--         , cloneStackTest $
--             cloneStack
--
--         , createAppTest $
--             createApp
--
--         , createDeploymentTest $
--             createDeployment
--
--         , createInstanceTest $
--             createInstance
--
--         , createLayerTest $
--             createLayer
--
--         , createStackTest $
--             createStack
--
--         , createUserProfileTest $
--             createUserProfile
--
--         , deleteAppTest $
--             deleteApp
--
--         , deleteInstanceTest $
--             deleteInstance
--
--         , deleteLayerTest $
--             deleteLayer
--
--         , deleteStackTest $
--             deleteStack
--
--         , deleteUserProfileTest $
--             deleteUserProfile
--
--         , deregisterElasticIPTest $
--             deregisterElasticIP
--
--         , deregisterInstanceTest $
--             deregisterInstance
--
--         , deregisterRDSDBInstanceTest $
--             deregisterRDSDBInstance
--
--         , deregisterVolumeTest $
--             deregisterVolume
--
--         , describeAgentVersionsTest $
--             describeAgentVersions
--
--         , describeAppsTest $
--             describeApps
--
--         , describeCommandsTest $
--             describeCommands
--
--         , describeDeploymentsTest $
--             describeDeployments
--
--         , describeElasticIPsTest $
--             describeElasticIPs
--
--         , describeElasticLoadBalancersTest $
--             describeElasticLoadBalancers
--
--         , describeInstancesTest $
--             describeInstances
--
--         , describeLayersTest $
--             describeLayers
--
--         , describeLoadBasedAutoScalingTest $
--             describeLoadBasedAutoScaling
--
--         , describeMyUserProfileTest $
--             describeMyUserProfile
--
--         , describePermissionsTest $
--             describePermissions
--
--         , describeRAIDArraysTest $
--             describeRAIDArrays
--
--         , describeRDSDBInstancesTest $
--             describeRDSDBInstances
--
--         , describeServiceErrorsTest $
--             describeServiceErrors
--
--         , describeStackProvisioningParametersTest $
--             describeStackProvisioningParameters
--
--         , describeStackSummaryTest $
--             describeStackSummary
--
--         , describeStacksTest $
--             describeStacks
--
--         , describeTimeBasedAutoScalingTest $
--             describeTimeBasedAutoScaling
--
--         , describeUserProfilesTest $
--             describeUserProfiles
--
--         , describeVolumesTest $
--             describeVolumes
--
--         , detachElasticLoadBalancerTest $
--             detachElasticLoadBalancer
--
--         , disassociateElasticIPTest $
--             disassociateElasticIP
--
--         , getHostnameSuggestionTest $
--             getHostnameSuggestion
--
--         , grantAccessTest $
--             grantAccess
--
--         , rebootInstanceTest $
--             rebootInstance
--
--         , registerElasticIPTest $
--             registerElasticIP
--
--         , registerInstanceTest $
--             registerInstance
--
--         , registerRDSDBInstanceTest $
--             registerRDSDBInstance
--
--         , registerVolumeTest $
--             registerVolume
--
--         , setLoadBasedAutoScalingTest $
--             setLoadBasedAutoScaling
--
--         , setPermissionTest $
--             setPermission
--
--         , setTimeBasedAutoScalingTest $
--             setTimeBasedAutoScaling
--
--         , startInstanceTest $
--             startInstance
--
--         , startStackTest $
--             startStack
--
--         , stopInstanceTest $
--             stopInstance
--
--         , stopStackTest $
--             stopStack
--
--         , unassignInstanceTest $
--             unassignInstance
--
--         , unassignVolumeTest $
--             unassignVolume
--
--         , updateAppTest $
--             updateApp
--
--         , updateElasticIPTest $
--             updateElasticIP
--
--         , updateInstanceTest $
--             updateInstance
--
--         , updateLayerTest $
--             updateLayer
--
--         , updateMyUserProfileTest $
--             updateMyUserProfile
--
--         , updateRDSDBInstanceTest $
--             updateRDSDBInstance
--
--         , updateStackTest $
--             updateStack
--
--         , updateUserProfileTest $
--             updateUserProfile
--
--         , updateVolumeTest $
--             updateVolume
--
--           ]

--     , testGroup "response"
--         [ assignInstanceResponseTest $
--             assignInstanceResponse
--
--         , assignVolumeResponseTest $
--             assignVolumeResponse
--
--         , associateElasticIPResponseTest $
--             associateElasticIPResponse
--
--         , attachElasticLoadBalancerResponseTest $
--             attachElasticLoadBalancerResponse
--
--         , cloneStackResponseTest $
--             cloneStackResponse
--
--         , createAppResponseTest $
--             createAppResponse
--
--         , createDeploymentResponseTest $
--             createDeploymentResponse
--
--         , createInstanceResponseTest $
--             createInstanceResponse
--
--         , createLayerResponseTest $
--             createLayerResponse
--
--         , createStackResponseTest $
--             createStackResponse
--
--         , createUserProfileResponseTest $
--             createUserProfileResponse
--
--         , deleteAppResponseTest $
--             deleteAppResponse
--
--         , deleteInstanceResponseTest $
--             deleteInstanceResponse
--
--         , deleteLayerResponseTest $
--             deleteLayerResponse
--
--         , deleteStackResponseTest $
--             deleteStackResponse
--
--         , deleteUserProfileResponseTest $
--             deleteUserProfileResponse
--
--         , deregisterElasticIPResponseTest $
--             deregisterElasticIPResponse
--
--         , deregisterInstanceResponseTest $
--             deregisterInstanceResponse
--
--         , deregisterRDSDBInstanceResponseTest $
--             deregisterRDSDBInstanceResponse
--
--         , deregisterVolumeResponseTest $
--             deregisterVolumeResponse
--
--         , describeAgentVersionsResponseTest $
--             describeAgentVersionsResponse
--
--         , describeAppsResponseTest $
--             describeAppsResponse
--
--         , describeCommandsResponseTest $
--             describeCommandsResponse
--
--         , describeDeploymentsResponseTest $
--             describeDeploymentsResponse
--
--         , describeElasticIPsResponseTest $
--             describeElasticIPsResponse
--
--         , describeElasticLoadBalancersResponseTest $
--             describeElasticLoadBalancersResponse
--
--         , describeInstancesResponseTest $
--             describeInstancesResponse
--
--         , describeLayersResponseTest $
--             describeLayersResponse
--
--         , describeLoadBasedAutoScalingResponseTest $
--             describeLoadBasedAutoScalingResponse
--
--         , describeMyUserProfileResponseTest $
--             describeMyUserProfileResponse
--
--         , describePermissionsResponseTest $
--             describePermissionsResponse
--
--         , describeRAIDArraysResponseTest $
--             describeRAIDArraysResponse
--
--         , describeRDSDBInstancesResponseTest $
--             describeRDSDBInstancesResponse
--
--         , describeServiceErrorsResponseTest $
--             describeServiceErrorsResponse
--
--         , describeStackProvisioningParametersResponseTest $
--             describeStackProvisioningParametersResponse
--
--         , describeStackSummaryResponseTest $
--             describeStackSummaryResponse
--
--         , describeStacksResponseTest $
--             describeStacksResponse
--
--         , describeTimeBasedAutoScalingResponseTest $
--             describeTimeBasedAutoScalingResponse
--
--         , describeUserProfilesResponseTest $
--             describeUserProfilesResponse
--
--         , describeVolumesResponseTest $
--             describeVolumesResponse
--
--         , detachElasticLoadBalancerResponseTest $
--             detachElasticLoadBalancerResponse
--
--         , disassociateElasticIPResponseTest $
--             disassociateElasticIPResponse
--
--         , getHostnameSuggestionResponseTest $
--             getHostnameSuggestionResponse
--
--         , grantAccessResponseTest $
--             grantAccessResponse
--
--         , rebootInstanceResponseTest $
--             rebootInstanceResponse
--
--         , registerElasticIPResponseTest $
--             registerElasticIPResponse
--
--         , registerInstanceResponseTest $
--             registerInstanceResponse
--
--         , registerRDSDBInstanceResponseTest $
--             registerRDSDBInstanceResponse
--
--         , registerVolumeResponseTest $
--             registerVolumeResponse
--
--         , setLoadBasedAutoScalingResponseTest $
--             setLoadBasedAutoScalingResponse
--
--         , setPermissionResponseTest $
--             setPermissionResponse
--
--         , setTimeBasedAutoScalingResponseTest $
--             setTimeBasedAutoScalingResponse
--
--         , startInstanceResponseTest $
--             startInstanceResponse
--
--         , startStackResponseTest $
--             startStackResponse
--
--         , stopInstanceResponseTest $
--             stopInstanceResponse
--
--         , stopStackResponseTest $
--             stopStackResponse
--
--         , unassignInstanceResponseTest $
--             unassignInstanceResponse
--
--         , unassignVolumeResponseTest $
--             unassignVolumeResponse
--
--         , updateAppResponseTest $
--             updateAppResponse
--
--         , updateElasticIPResponseTest $
--             updateElasticIPResponse
--
--         , updateInstanceResponseTest $
--             updateInstanceResponse
--
--         , updateLayerResponseTest $
--             updateLayerResponse
--
--         , updateMyUserProfileResponseTest $
--             updateMyUserProfileResponse
--
--         , updateRDSDBInstanceResponseTest $
--             updateRDSDBInstanceResponse
--
--         , updateStackResponseTest $
--             updateStackResponse
--
--         , updateUserProfileResponseTest $
--             updateUserProfileResponse
--
--         , updateVolumeResponseTest $
--             updateVolumeResponse
--
--           ]
--     ]

-- Requests

assignInstanceTest :: AssignInstance -> TestTree
assignInstanceTest = undefined

assignVolumeTest :: AssignVolume -> TestTree
assignVolumeTest = undefined

associateElasticIPTest :: AssociateElasticIP -> TestTree
associateElasticIPTest = undefined

attachElasticLoadBalancerTest :: AttachElasticLoadBalancer -> TestTree
attachElasticLoadBalancerTest = undefined

cloneStackTest :: CloneStack -> TestTree
cloneStackTest = undefined

createAppTest :: CreateApp -> TestTree
createAppTest = undefined

createDeploymentTest :: CreateDeployment -> TestTree
createDeploymentTest = undefined

createInstanceTest :: CreateInstance -> TestTree
createInstanceTest = undefined

createLayerTest :: CreateLayer -> TestTree
createLayerTest = undefined

createStackTest :: CreateStack -> TestTree
createStackTest = undefined

createUserProfileTest :: CreateUserProfile -> TestTree
createUserProfileTest = undefined

deleteAppTest :: DeleteApp -> TestTree
deleteAppTest = undefined

deleteInstanceTest :: DeleteInstance -> TestTree
deleteInstanceTest = undefined

deleteLayerTest :: DeleteLayer -> TestTree
deleteLayerTest = undefined

deleteStackTest :: DeleteStack -> TestTree
deleteStackTest = undefined

deleteUserProfileTest :: DeleteUserProfile -> TestTree
deleteUserProfileTest = undefined

deregisterElasticIPTest :: DeregisterElasticIP -> TestTree
deregisterElasticIPTest = undefined

deregisterInstanceTest :: DeregisterInstance -> TestTree
deregisterInstanceTest = undefined

deregisterRDSDBInstanceTest :: DeregisterRDSDBInstance -> TestTree
deregisterRDSDBInstanceTest = undefined

deregisterVolumeTest :: DeregisterVolume -> TestTree
deregisterVolumeTest = undefined

describeAgentVersionsTest :: DescribeAgentVersions -> TestTree
describeAgentVersionsTest = undefined

describeAppsTest :: DescribeApps -> TestTree
describeAppsTest = undefined

describeCommandsTest :: DescribeCommands -> TestTree
describeCommandsTest = undefined

describeDeploymentsTest :: DescribeDeployments -> TestTree
describeDeploymentsTest = undefined

describeElasticIPsTest :: DescribeElasticIPs -> TestTree
describeElasticIPsTest = undefined

describeElasticLoadBalancersTest :: DescribeElasticLoadBalancers -> TestTree
describeElasticLoadBalancersTest = undefined

describeInstancesTest :: DescribeInstances -> TestTree
describeInstancesTest = undefined

describeLayersTest :: DescribeLayers -> TestTree
describeLayersTest = undefined

describeLoadBasedAutoScalingTest :: DescribeLoadBasedAutoScaling -> TestTree
describeLoadBasedAutoScalingTest = undefined

describeMyUserProfileTest :: DescribeMyUserProfile -> TestTree
describeMyUserProfileTest = undefined

describePermissionsTest :: DescribePermissions -> TestTree
describePermissionsTest = undefined

describeRAIDArraysTest :: DescribeRAIDArrays -> TestTree
describeRAIDArraysTest = undefined

describeRDSDBInstancesTest :: DescribeRDSDBInstances -> TestTree
describeRDSDBInstancesTest = undefined

describeServiceErrorsTest :: DescribeServiceErrors -> TestTree
describeServiceErrorsTest = undefined

describeStackProvisioningParametersTest :: DescribeStackProvisioningParameters -> TestTree
describeStackProvisioningParametersTest = undefined

describeStackSummaryTest :: DescribeStackSummary -> TestTree
describeStackSummaryTest = undefined

describeStacksTest :: DescribeStacks -> TestTree
describeStacksTest = undefined

describeTimeBasedAutoScalingTest :: DescribeTimeBasedAutoScaling -> TestTree
describeTimeBasedAutoScalingTest = undefined

describeUserProfilesTest :: DescribeUserProfiles -> TestTree
describeUserProfilesTest = undefined

describeVolumesTest :: DescribeVolumes -> TestTree
describeVolumesTest = undefined

detachElasticLoadBalancerTest :: DetachElasticLoadBalancer -> TestTree
detachElasticLoadBalancerTest = undefined

disassociateElasticIPTest :: DisassociateElasticIP -> TestTree
disassociateElasticIPTest = undefined

getHostnameSuggestionTest :: GetHostnameSuggestion -> TestTree
getHostnameSuggestionTest = undefined

grantAccessTest :: GrantAccess -> TestTree
grantAccessTest = undefined

rebootInstanceTest :: RebootInstance -> TestTree
rebootInstanceTest = undefined

registerElasticIPTest :: RegisterElasticIP -> TestTree
registerElasticIPTest = undefined

registerInstanceTest :: RegisterInstance -> TestTree
registerInstanceTest = undefined

registerRDSDBInstanceTest :: RegisterRDSDBInstance -> TestTree
registerRDSDBInstanceTest = undefined

registerVolumeTest :: RegisterVolume -> TestTree
registerVolumeTest = undefined

setLoadBasedAutoScalingTest :: SetLoadBasedAutoScaling -> TestTree
setLoadBasedAutoScalingTest = undefined

setPermissionTest :: SetPermission -> TestTree
setPermissionTest = undefined

setTimeBasedAutoScalingTest :: SetTimeBasedAutoScaling -> TestTree
setTimeBasedAutoScalingTest = undefined

startInstanceTest :: StartInstance -> TestTree
startInstanceTest = undefined

startStackTest :: StartStack -> TestTree
startStackTest = undefined

stopInstanceTest :: StopInstance -> TestTree
stopInstanceTest = undefined

stopStackTest :: StopStack -> TestTree
stopStackTest = undefined

unassignInstanceTest :: UnassignInstance -> TestTree
unassignInstanceTest = undefined

unassignVolumeTest :: UnassignVolume -> TestTree
unassignVolumeTest = undefined

updateAppTest :: UpdateApp -> TestTree
updateAppTest = undefined

updateElasticIPTest :: UpdateElasticIP -> TestTree
updateElasticIPTest = undefined

updateInstanceTest :: UpdateInstance -> TestTree
updateInstanceTest = undefined

updateLayerTest :: UpdateLayer -> TestTree
updateLayerTest = undefined

updateMyUserProfileTest :: UpdateMyUserProfile -> TestTree
updateMyUserProfileTest = undefined

updateRDSDBInstanceTest :: UpdateRDSDBInstance -> TestTree
updateRDSDBInstanceTest = undefined

updateStackTest :: UpdateStack -> TestTree
updateStackTest = undefined

updateUserProfileTest :: UpdateUserProfile -> TestTree
updateUserProfileTest = undefined

updateVolumeTest :: UpdateVolume -> TestTree
updateVolumeTest = undefined

-- Responses

assignInstanceResponseTest :: AssignInstanceResponse -> TestTree
assignInstanceResponseTest = resp
    "assignInstanceResponse"
    "fixture/AssignInstanceResponse"
    (Proxy :: Proxy AssignInstance)

assignVolumeResponseTest :: AssignVolumeResponse -> TestTree
assignVolumeResponseTest = resp
    "assignVolumeResponse"
    "fixture/AssignVolumeResponse"
    (Proxy :: Proxy AssignVolume)

associateElasticIPResponseTest :: AssociateElasticIPResponse -> TestTree
associateElasticIPResponseTest = resp
    "associateElasticIPResponse"
    "fixture/AssociateElasticIPResponse"
    (Proxy :: Proxy AssociateElasticIP)

attachElasticLoadBalancerResponseTest :: AttachElasticLoadBalancerResponse -> TestTree
attachElasticLoadBalancerResponseTest = resp
    "attachElasticLoadBalancerResponse"
    "fixture/AttachElasticLoadBalancerResponse"
    (Proxy :: Proxy AttachElasticLoadBalancer)

cloneStackResponseTest :: CloneStackResponse -> TestTree
cloneStackResponseTest = resp
    "cloneStackResponse"
    "fixture/CloneStackResponse"
    (Proxy :: Proxy CloneStack)

createAppResponseTest :: CreateAppResponse -> TestTree
createAppResponseTest = resp
    "createAppResponse"
    "fixture/CreateAppResponse"
    (Proxy :: Proxy CreateApp)

createDeploymentResponseTest :: CreateDeploymentResponse -> TestTree
createDeploymentResponseTest = resp
    "createDeploymentResponse"
    "fixture/CreateDeploymentResponse"
    (Proxy :: Proxy CreateDeployment)

createInstanceResponseTest :: CreateInstanceResponse -> TestTree
createInstanceResponseTest = resp
    "createInstanceResponse"
    "fixture/CreateInstanceResponse"
    (Proxy :: Proxy CreateInstance)

createLayerResponseTest :: CreateLayerResponse -> TestTree
createLayerResponseTest = resp
    "createLayerResponse"
    "fixture/CreateLayerResponse"
    (Proxy :: Proxy CreateLayer)

createStackResponseTest :: CreateStackResponse -> TestTree
createStackResponseTest = resp
    "createStackResponse"
    "fixture/CreateStackResponse"
    (Proxy :: Proxy CreateStack)

createUserProfileResponseTest :: CreateUserProfileResponse -> TestTree
createUserProfileResponseTest = resp
    "createUserProfileResponse"
    "fixture/CreateUserProfileResponse"
    (Proxy :: Proxy CreateUserProfile)

deleteAppResponseTest :: DeleteAppResponse -> TestTree
deleteAppResponseTest = resp
    "deleteAppResponse"
    "fixture/DeleteAppResponse"
    (Proxy :: Proxy DeleteApp)

deleteInstanceResponseTest :: DeleteInstanceResponse -> TestTree
deleteInstanceResponseTest = resp
    "deleteInstanceResponse"
    "fixture/DeleteInstanceResponse"
    (Proxy :: Proxy DeleteInstance)

deleteLayerResponseTest :: DeleteLayerResponse -> TestTree
deleteLayerResponseTest = resp
    "deleteLayerResponse"
    "fixture/DeleteLayerResponse"
    (Proxy :: Proxy DeleteLayer)

deleteStackResponseTest :: DeleteStackResponse -> TestTree
deleteStackResponseTest = resp
    "deleteStackResponse"
    "fixture/DeleteStackResponse"
    (Proxy :: Proxy DeleteStack)

deleteUserProfileResponseTest :: DeleteUserProfileResponse -> TestTree
deleteUserProfileResponseTest = resp
    "deleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse"
    (Proxy :: Proxy DeleteUserProfile)

deregisterElasticIPResponseTest :: DeregisterElasticIPResponse -> TestTree
deregisterElasticIPResponseTest = resp
    "deregisterElasticIPResponse"
    "fixture/DeregisterElasticIPResponse"
    (Proxy :: Proxy DeregisterElasticIP)

deregisterInstanceResponseTest :: DeregisterInstanceResponse -> TestTree
deregisterInstanceResponseTest = resp
    "deregisterInstanceResponse"
    "fixture/DeregisterInstanceResponse"
    (Proxy :: Proxy DeregisterInstance)

deregisterRDSDBInstanceResponseTest :: DeregisterRDSDBInstanceResponse -> TestTree
deregisterRDSDBInstanceResponseTest = resp
    "deregisterRDSDBInstanceResponse"
    "fixture/DeregisterRDSDBInstanceResponse"
    (Proxy :: Proxy DeregisterRDSDBInstance)

deregisterVolumeResponseTest :: DeregisterVolumeResponse -> TestTree
deregisterVolumeResponseTest = resp
    "deregisterVolumeResponse"
    "fixture/DeregisterVolumeResponse"
    (Proxy :: Proxy DeregisterVolume)

describeAgentVersionsResponseTest :: DescribeAgentVersionsResponse -> TestTree
describeAgentVersionsResponseTest = resp
    "describeAgentVersionsResponse"
    "fixture/DescribeAgentVersionsResponse"
    (Proxy :: Proxy DescribeAgentVersions)

describeAppsResponseTest :: DescribeAppsResponse -> TestTree
describeAppsResponseTest = resp
    "describeAppsResponse"
    "fixture/DescribeAppsResponse"
    (Proxy :: Proxy DescribeApps)

describeCommandsResponseTest :: DescribeCommandsResponse -> TestTree
describeCommandsResponseTest = resp
    "describeCommandsResponse"
    "fixture/DescribeCommandsResponse"
    (Proxy :: Proxy DescribeCommands)

describeDeploymentsResponseTest :: DescribeDeploymentsResponse -> TestTree
describeDeploymentsResponseTest = resp
    "describeDeploymentsResponse"
    "fixture/DescribeDeploymentsResponse"
    (Proxy :: Proxy DescribeDeployments)

describeElasticIPsResponseTest :: DescribeElasticIPsResponse -> TestTree
describeElasticIPsResponseTest = resp
    "describeElasticIPsResponse"
    "fixture/DescribeElasticIPsResponse"
    (Proxy :: Proxy DescribeElasticIPs)

describeElasticLoadBalancersResponseTest :: DescribeElasticLoadBalancersResponse -> TestTree
describeElasticLoadBalancersResponseTest = resp
    "describeElasticLoadBalancersResponse"
    "fixture/DescribeElasticLoadBalancersResponse"
    (Proxy :: Proxy DescribeElasticLoadBalancers)

describeInstancesResponseTest :: DescribeInstancesResponse -> TestTree
describeInstancesResponseTest = resp
    "describeInstancesResponse"
    "fixture/DescribeInstancesResponse"
    (Proxy :: Proxy DescribeInstances)

describeLayersResponseTest :: DescribeLayersResponse -> TestTree
describeLayersResponseTest = resp
    "describeLayersResponse"
    "fixture/DescribeLayersResponse"
    (Proxy :: Proxy DescribeLayers)

describeLoadBasedAutoScalingResponseTest :: DescribeLoadBasedAutoScalingResponse -> TestTree
describeLoadBasedAutoScalingResponseTest = resp
    "describeLoadBasedAutoScalingResponse"
    "fixture/DescribeLoadBasedAutoScalingResponse"
    (Proxy :: Proxy DescribeLoadBasedAutoScaling)

describeMyUserProfileResponseTest :: DescribeMyUserProfileResponse -> TestTree
describeMyUserProfileResponseTest = resp
    "describeMyUserProfileResponse"
    "fixture/DescribeMyUserProfileResponse"
    (Proxy :: Proxy DescribeMyUserProfile)

describePermissionsResponseTest :: DescribePermissionsResponse -> TestTree
describePermissionsResponseTest = resp
    "describePermissionsResponse"
    "fixture/DescribePermissionsResponse"
    (Proxy :: Proxy DescribePermissions)

describeRAIDArraysResponseTest :: DescribeRAIDArraysResponse -> TestTree
describeRAIDArraysResponseTest = resp
    "describeRAIDArraysResponse"
    "fixture/DescribeRAIDArraysResponse"
    (Proxy :: Proxy DescribeRAIDArrays)

describeRDSDBInstancesResponseTest :: DescribeRDSDBInstancesResponse -> TestTree
describeRDSDBInstancesResponseTest = resp
    "describeRDSDBInstancesResponse"
    "fixture/DescribeRDSDBInstancesResponse"
    (Proxy :: Proxy DescribeRDSDBInstances)

describeServiceErrorsResponseTest :: DescribeServiceErrorsResponse -> TestTree
describeServiceErrorsResponseTest = resp
    "describeServiceErrorsResponse"
    "fixture/DescribeServiceErrorsResponse"
    (Proxy :: Proxy DescribeServiceErrors)

describeStackProvisioningParametersResponseTest :: DescribeStackProvisioningParametersResponse -> TestTree
describeStackProvisioningParametersResponseTest = resp
    "describeStackProvisioningParametersResponse"
    "fixture/DescribeStackProvisioningParametersResponse"
    (Proxy :: Proxy DescribeStackProvisioningParameters)

describeStackSummaryResponseTest :: DescribeStackSummaryResponse -> TestTree
describeStackSummaryResponseTest = resp
    "describeStackSummaryResponse"
    "fixture/DescribeStackSummaryResponse"
    (Proxy :: Proxy DescribeStackSummary)

describeStacksResponseTest :: DescribeStacksResponse -> TestTree
describeStacksResponseTest = resp
    "describeStacksResponse"
    "fixture/DescribeStacksResponse"
    (Proxy :: Proxy DescribeStacks)

describeTimeBasedAutoScalingResponseTest :: DescribeTimeBasedAutoScalingResponse -> TestTree
describeTimeBasedAutoScalingResponseTest = resp
    "describeTimeBasedAutoScalingResponse"
    "fixture/DescribeTimeBasedAutoScalingResponse"
    (Proxy :: Proxy DescribeTimeBasedAutoScaling)

describeUserProfilesResponseTest :: DescribeUserProfilesResponse -> TestTree
describeUserProfilesResponseTest = resp
    "describeUserProfilesResponse"
    "fixture/DescribeUserProfilesResponse"
    (Proxy :: Proxy DescribeUserProfiles)

describeVolumesResponseTest :: DescribeVolumesResponse -> TestTree
describeVolumesResponseTest = resp
    "describeVolumesResponse"
    "fixture/DescribeVolumesResponse"
    (Proxy :: Proxy DescribeVolumes)

detachElasticLoadBalancerResponseTest :: DetachElasticLoadBalancerResponse -> TestTree
detachElasticLoadBalancerResponseTest = resp
    "detachElasticLoadBalancerResponse"
    "fixture/DetachElasticLoadBalancerResponse"
    (Proxy :: Proxy DetachElasticLoadBalancer)

disassociateElasticIPResponseTest :: DisassociateElasticIPResponse -> TestTree
disassociateElasticIPResponseTest = resp
    "disassociateElasticIPResponse"
    "fixture/DisassociateElasticIPResponse"
    (Proxy :: Proxy DisassociateElasticIP)

getHostnameSuggestionResponseTest :: GetHostnameSuggestionResponse -> TestTree
getHostnameSuggestionResponseTest = resp
    "getHostnameSuggestionResponse"
    "fixture/GetHostnameSuggestionResponse"
    (Proxy :: Proxy GetHostnameSuggestion)

grantAccessResponseTest :: GrantAccessResponse -> TestTree
grantAccessResponseTest = resp
    "grantAccessResponse"
    "fixture/GrantAccessResponse"
    (Proxy :: Proxy GrantAccess)

rebootInstanceResponseTest :: RebootInstanceResponse -> TestTree
rebootInstanceResponseTest = resp
    "rebootInstanceResponse"
    "fixture/RebootInstanceResponse"
    (Proxy :: Proxy RebootInstance)

registerElasticIPResponseTest :: RegisterElasticIPResponse -> TestTree
registerElasticIPResponseTest = resp
    "registerElasticIPResponse"
    "fixture/RegisterElasticIPResponse"
    (Proxy :: Proxy RegisterElasticIP)

registerInstanceResponseTest :: RegisterInstanceResponse -> TestTree
registerInstanceResponseTest = resp
    "registerInstanceResponse"
    "fixture/RegisterInstanceResponse"
    (Proxy :: Proxy RegisterInstance)

registerRDSDBInstanceResponseTest :: RegisterRDSDBInstanceResponse -> TestTree
registerRDSDBInstanceResponseTest = resp
    "registerRDSDBInstanceResponse"
    "fixture/RegisterRDSDBInstanceResponse"
    (Proxy :: Proxy RegisterRDSDBInstance)

registerVolumeResponseTest :: RegisterVolumeResponse -> TestTree
registerVolumeResponseTest = resp
    "registerVolumeResponse"
    "fixture/RegisterVolumeResponse"
    (Proxy :: Proxy RegisterVolume)

setLoadBasedAutoScalingResponseTest :: SetLoadBasedAutoScalingResponse -> TestTree
setLoadBasedAutoScalingResponseTest = resp
    "setLoadBasedAutoScalingResponse"
    "fixture/SetLoadBasedAutoScalingResponse"
    (Proxy :: Proxy SetLoadBasedAutoScaling)

setPermissionResponseTest :: SetPermissionResponse -> TestTree
setPermissionResponseTest = resp
    "setPermissionResponse"
    "fixture/SetPermissionResponse"
    (Proxy :: Proxy SetPermission)

setTimeBasedAutoScalingResponseTest :: SetTimeBasedAutoScalingResponse -> TestTree
setTimeBasedAutoScalingResponseTest = resp
    "setTimeBasedAutoScalingResponse"
    "fixture/SetTimeBasedAutoScalingResponse"
    (Proxy :: Proxy SetTimeBasedAutoScaling)

startInstanceResponseTest :: StartInstanceResponse -> TestTree
startInstanceResponseTest = resp
    "startInstanceResponse"
    "fixture/StartInstanceResponse"
    (Proxy :: Proxy StartInstance)

startStackResponseTest :: StartStackResponse -> TestTree
startStackResponseTest = resp
    "startStackResponse"
    "fixture/StartStackResponse"
    (Proxy :: Proxy StartStack)

stopInstanceResponseTest :: StopInstanceResponse -> TestTree
stopInstanceResponseTest = resp
    "stopInstanceResponse"
    "fixture/StopInstanceResponse"
    (Proxy :: Proxy StopInstance)

stopStackResponseTest :: StopStackResponse -> TestTree
stopStackResponseTest = resp
    "stopStackResponse"
    "fixture/StopStackResponse"
    (Proxy :: Proxy StopStack)

unassignInstanceResponseTest :: UnassignInstanceResponse -> TestTree
unassignInstanceResponseTest = resp
    "unassignInstanceResponse"
    "fixture/UnassignInstanceResponse"
    (Proxy :: Proxy UnassignInstance)

unassignVolumeResponseTest :: UnassignVolumeResponse -> TestTree
unassignVolumeResponseTest = resp
    "unassignVolumeResponse"
    "fixture/UnassignVolumeResponse"
    (Proxy :: Proxy UnassignVolume)

updateAppResponseTest :: UpdateAppResponse -> TestTree
updateAppResponseTest = resp
    "updateAppResponse"
    "fixture/UpdateAppResponse"
    (Proxy :: Proxy UpdateApp)

updateElasticIPResponseTest :: UpdateElasticIPResponse -> TestTree
updateElasticIPResponseTest = resp
    "updateElasticIPResponse"
    "fixture/UpdateElasticIPResponse"
    (Proxy :: Proxy UpdateElasticIP)

updateInstanceResponseTest :: UpdateInstanceResponse -> TestTree
updateInstanceResponseTest = resp
    "updateInstanceResponse"
    "fixture/UpdateInstanceResponse"
    (Proxy :: Proxy UpdateInstance)

updateLayerResponseTest :: UpdateLayerResponse -> TestTree
updateLayerResponseTest = resp
    "updateLayerResponse"
    "fixture/UpdateLayerResponse"
    (Proxy :: Proxy UpdateLayer)

updateMyUserProfileResponseTest :: UpdateMyUserProfileResponse -> TestTree
updateMyUserProfileResponseTest = resp
    "updateMyUserProfileResponse"
    "fixture/UpdateMyUserProfileResponse"
    (Proxy :: Proxy UpdateMyUserProfile)

updateRDSDBInstanceResponseTest :: UpdateRDSDBInstanceResponse -> TestTree
updateRDSDBInstanceResponseTest = resp
    "updateRDSDBInstanceResponse"
    "fixture/UpdateRDSDBInstanceResponse"
    (Proxy :: Proxy UpdateRDSDBInstance)

updateStackResponseTest :: UpdateStackResponse -> TestTree
updateStackResponseTest = resp
    "updateStackResponse"
    "fixture/UpdateStackResponse"
    (Proxy :: Proxy UpdateStack)

updateUserProfileResponseTest :: UpdateUserProfileResponse -> TestTree
updateUserProfileResponseTest = resp
    "updateUserProfileResponse"
    "fixture/UpdateUserProfileResponse"
    (Proxy :: Proxy UpdateUserProfile)

updateVolumeResponseTest :: UpdateVolumeResponse -> TestTree
updateVolumeResponseTest = resp
    "updateVolumeResponse"
    "fixture/UpdateVolumeResponse"
    (Proxy :: Proxy UpdateVolume)
