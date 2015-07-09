{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.OpsWorks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.OpsWorks where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.OpsWorks

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDescribeRDSDBInstances $
--             describeRDSDBInstances
--
--         , testDeleteStack $
--             deleteStack
--
--         , testUpdateStack $
--             updateStack
--
--         , testCreateLayer $
--             createLayer
--
--         , testSetLoadBasedAutoScaling $
--             setLoadBasedAutoScaling
--
--         , testUnassignVolume $
--             unassignVolume
--
--         , testDeregisterRDSDBInstance $
--             deregisterRDSDBInstance
--
--         , testCreateInstance $
--             createInstance
--
--         , testRegisterElasticIP $
--             registerElasticIP
--
--         , testDescribeAgentVersions $
--             describeAgentVersions
--
--         , testDescribeLayers $
--             describeLayers
--
--         , testCreateDeployment $
--             createDeployment
--
--         , testDeleteApp $
--             deleteApp
--
--         , testUpdateApp $
--             updateApp
--
--         , testDeleteInstance $
--             deleteInstance
--
--         , testUpdateInstance $
--             updateInstance
--
--         , testDescribeStacks $
--             describeStacks
--
--         , testDeregisterVolume $
--             deregisterVolume
--
--         , testAssignInstance $
--             assignInstance
--
--         , testRebootInstance $
--             rebootInstance
--
--         , testDescribeTimeBasedAutoScaling $
--             describeTimeBasedAutoScaling
--
--         , testUpdateRDSDBInstance $
--             updateRDSDBInstance
--
--         , testStopStack $
--             stopStack
--
--         , testDescribeVolumes $
--             describeVolumes
--
--         , testDisassociateElasticIP $
--             disassociateElasticIP
--
--         , testStopInstance $
--             stopInstance
--
--         , testRegisterVolume $
--             registerVolume
--
--         , testSetTimeBasedAutoScaling $
--             setTimeBasedAutoScaling
--
--         , testDeregisterElasticIP $
--             deregisterElasticIP
--
--         , testAttachElasticLoadBalancer $
--             attachElasticLoadBalancer
--
--         , testDescribeUserProfiles $
--             describeUserProfiles
--
--         , testDescribeStackSummary $
--             describeStackSummary
--
--         , testDescribeApps $
--             describeApps
--
--         , testUpdateMyUserProfile $
--             updateMyUserProfile
--
--         , testDescribeInstances $
--             describeInstances
--
--         , testDescribeDeployments $
--             describeDeployments
--
--         , testCreateStack $
--             createStack
--
--         , testGrantAccess $
--             grantAccess
--
--         , testDescribeElasticIPs $
--             describeElasticIPs
--
--         , testDeleteLayer $
--             deleteLayer
--
--         , testUpdateLayer $
--             updateLayer
--
--         , testCloneStack $
--             cloneStack
--
--         , testGetHostnameSuggestion $
--             getHostnameSuggestion
--
--         , testCreateApp $
--             createApp
--
--         , testDescribePermissions $
--             describePermissions
--
--         , testUpdateElasticIP $
--             updateElasticIP
--
--         , testDescribeLoadBasedAutoScaling $
--             describeLoadBasedAutoScaling
--
--         , testRegisterInstance $
--             registerInstance
--
--         , testAssociateElasticIP $
--             associateElasticIP
--
--         , testDetachElasticLoadBalancer $
--             detachElasticLoadBalancer
--
--         , testDescribeStackProvisioningParameters $
--             describeStackProvisioningParameters
--
--         , testDescribeMyUserProfile $
--             describeMyUserProfile
--
--         , testUnassignInstance $
--             unassignInstance
--
--         , testRegisterRDSDBInstance $
--             registerRDSDBInstance
--
--         , testDeleteUserProfile $
--             deleteUserProfile
--
--         , testUpdateUserProfile $
--             updateUserProfile
--
--         , testDescribeServiceErrors $
--             describeServiceErrors
--
--         , testStartStack $
--             startStack
--
--         , testCreateUserProfile $
--             createUserProfile
--
--         , testDescribeCommands $
--             describeCommands
--
--         , testDescribeElasticLoadBalancers $
--             describeElasticLoadBalancers
--
--         , testDeregisterInstance $
--             deregisterInstance
--
--         , testDescribeRAIDArrays $
--             describeRAIDArrays
--
--         , testSetPermission $
--             setPermission
--
--         , testUpdateVolume $
--             updateVolume
--
--         , testAssignVolume $
--             assignVolume
--
--         , testStartInstance $
--             startInstance
--
--           ]

--     , testGroup "response"
--         [ testDescribeRDSDBInstancesResponse $
--             describeRDSDBInstancesResponse
--
--         , testDeleteStackResponse $
--             deleteStackResponse
--
--         , testUpdateStackResponse $
--             updateStackResponse
--
--         , testCreateLayerResponse $
--             createLayerResponse
--
--         , testSetLoadBasedAutoScalingResponse $
--             setLoadBasedAutoScalingResponse
--
--         , testUnassignVolumeResponse $
--             unassignVolumeResponse
--
--         , testDeregisterRDSDBInstanceResponse $
--             deregisterRDSDBInstanceResponse
--
--         , testCreateInstanceResponse $
--             createInstanceResponse
--
--         , testRegisterElasticIPResponse $
--             registerElasticIPResponse
--
--         , testDescribeAgentVersionsResponse $
--             describeAgentVersionsResponse
--
--         , testDescribeLayersResponse $
--             describeLayersResponse
--
--         , testCreateDeploymentResponse $
--             createDeploymentResponse
--
--         , testDeleteAppResponse $
--             deleteAppResponse
--
--         , testUpdateAppResponse $
--             updateAppResponse
--
--         , testDeleteInstanceResponse $
--             deleteInstanceResponse
--
--         , testUpdateInstanceResponse $
--             updateInstanceResponse
--
--         , testDescribeStacksResponse $
--             describeStacksResponse
--
--         , testDeregisterVolumeResponse $
--             deregisterVolumeResponse
--
--         , testAssignInstanceResponse $
--             assignInstanceResponse
--
--         , testRebootInstanceResponse $
--             rebootInstanceResponse
--
--         , testDescribeTimeBasedAutoScalingResponse $
--             describeTimeBasedAutoScalingResponse
--
--         , testUpdateRDSDBInstanceResponse $
--             updateRDSDBInstanceResponse
--
--         , testStopStackResponse $
--             stopStackResponse
--
--         , testDescribeVolumesResponse $
--             describeVolumesResponse
--
--         , testDisassociateElasticIPResponse $
--             disassociateElasticIPResponse
--
--         , testStopInstanceResponse $
--             stopInstanceResponse
--
--         , testRegisterVolumeResponse $
--             registerVolumeResponse
--
--         , testSetTimeBasedAutoScalingResponse $
--             setTimeBasedAutoScalingResponse
--
--         , testDeregisterElasticIPResponse $
--             deregisterElasticIPResponse
--
--         , testAttachElasticLoadBalancerResponse $
--             attachElasticLoadBalancerResponse
--
--         , testDescribeUserProfilesResponse $
--             describeUserProfilesResponse
--
--         , testDescribeStackSummaryResponse $
--             describeStackSummaryResponse
--
--         , testDescribeAppsResponse $
--             describeAppsResponse
--
--         , testUpdateMyUserProfileResponse $
--             updateMyUserProfileResponse
--
--         , testDescribeInstancesResponse $
--             describeInstancesResponse
--
--         , testDescribeDeploymentsResponse $
--             describeDeploymentsResponse
--
--         , testCreateStackResponse $
--             createStackResponse
--
--         , testGrantAccessResponse $
--             grantAccessResponse
--
--         , testDescribeElasticIPsResponse $
--             describeElasticIPsResponse
--
--         , testDeleteLayerResponse $
--             deleteLayerResponse
--
--         , testUpdateLayerResponse $
--             updateLayerResponse
--
--         , testCloneStackResponse $
--             cloneStackResponse
--
--         , testGetHostnameSuggestionResponse $
--             getHostnameSuggestionResponse
--
--         , testCreateAppResponse $
--             createAppResponse
--
--         , testDescribePermissionsResponse $
--             describePermissionsResponse
--
--         , testUpdateElasticIPResponse $
--             updateElasticIPResponse
--
--         , testDescribeLoadBasedAutoScalingResponse $
--             describeLoadBasedAutoScalingResponse
--
--         , testRegisterInstanceResponse $
--             registerInstanceResponse
--
--         , testAssociateElasticIPResponse $
--             associateElasticIPResponse
--
--         , testDetachElasticLoadBalancerResponse $
--             detachElasticLoadBalancerResponse
--
--         , testDescribeStackProvisioningParametersResponse $
--             describeStackProvisioningParametersResponse
--
--         , testDescribeMyUserProfileResponse $
--             describeMyUserProfileResponse
--
--         , testUnassignInstanceResponse $
--             unassignInstanceResponse
--
--         , testRegisterRDSDBInstanceResponse $
--             registerRDSDBInstanceResponse
--
--         , testDeleteUserProfileResponse $
--             deleteUserProfileResponse
--
--         , testUpdateUserProfileResponse $
--             updateUserProfileResponse
--
--         , testDescribeServiceErrorsResponse $
--             describeServiceErrorsResponse
--
--         , testStartStackResponse $
--             startStackResponse
--
--         , testCreateUserProfileResponse $
--             createUserProfileResponse
--
--         , testDescribeCommandsResponse $
--             describeCommandsResponse
--
--         , testDescribeElasticLoadBalancersResponse $
--             describeElasticLoadBalancersResponse
--
--         , testDeregisterInstanceResponse $
--             deregisterInstanceResponse
--
--         , testDescribeRAIDArraysResponse $
--             describeRAIDArraysResponse
--
--         , testSetPermissionResponse $
--             setPermissionResponse
--
--         , testUpdateVolumeResponse $
--             updateVolumeResponse
--
--         , testAssignVolumeResponse $
--             assignVolumeResponse
--
--         , testStartInstanceResponse $
--             startInstanceResponse
--
--           ]
--     ]

-- Requests

testDescribeRDSDBInstances :: DescribeRDSDBInstances -> TestTree
testDescribeRDSDBInstances = undefined

testDeleteStack :: DeleteStack -> TestTree
testDeleteStack = undefined

testUpdateStack :: UpdateStack -> TestTree
testUpdateStack = undefined

testCreateLayer :: CreateLayer -> TestTree
testCreateLayer = undefined

testSetLoadBasedAutoScaling :: SetLoadBasedAutoScaling -> TestTree
testSetLoadBasedAutoScaling = undefined

testUnassignVolume :: UnassignVolume -> TestTree
testUnassignVolume = undefined

testDeregisterRDSDBInstance :: DeregisterRDSDBInstance -> TestTree
testDeregisterRDSDBInstance = undefined

testCreateInstance :: CreateInstance -> TestTree
testCreateInstance = undefined

testRegisterElasticIP :: RegisterElasticIP -> TestTree
testRegisterElasticIP = undefined

testDescribeAgentVersions :: DescribeAgentVersions -> TestTree
testDescribeAgentVersions = undefined

testDescribeLayers :: DescribeLayers -> TestTree
testDescribeLayers = undefined

testCreateDeployment :: CreateDeployment -> TestTree
testCreateDeployment = undefined

testDeleteApp :: DeleteApp -> TestTree
testDeleteApp = undefined

testUpdateApp :: UpdateApp -> TestTree
testUpdateApp = undefined

testDeleteInstance :: DeleteInstance -> TestTree
testDeleteInstance = undefined

testUpdateInstance :: UpdateInstance -> TestTree
testUpdateInstance = undefined

testDescribeStacks :: DescribeStacks -> TestTree
testDescribeStacks = undefined

testDeregisterVolume :: DeregisterVolume -> TestTree
testDeregisterVolume = undefined

testAssignInstance :: AssignInstance -> TestTree
testAssignInstance = undefined

testRebootInstance :: RebootInstance -> TestTree
testRebootInstance = undefined

testDescribeTimeBasedAutoScaling :: DescribeTimeBasedAutoScaling -> TestTree
testDescribeTimeBasedAutoScaling = undefined

testUpdateRDSDBInstance :: UpdateRDSDBInstance -> TestTree
testUpdateRDSDBInstance = undefined

testStopStack :: StopStack -> TestTree
testStopStack = undefined

testDescribeVolumes :: DescribeVolumes -> TestTree
testDescribeVolumes = undefined

testDisassociateElasticIP :: DisassociateElasticIP -> TestTree
testDisassociateElasticIP = undefined

testStopInstance :: StopInstance -> TestTree
testStopInstance = undefined

testRegisterVolume :: RegisterVolume -> TestTree
testRegisterVolume = undefined

testSetTimeBasedAutoScaling :: SetTimeBasedAutoScaling -> TestTree
testSetTimeBasedAutoScaling = undefined

testDeregisterElasticIP :: DeregisterElasticIP -> TestTree
testDeregisterElasticIP = undefined

testAttachElasticLoadBalancer :: AttachElasticLoadBalancer -> TestTree
testAttachElasticLoadBalancer = undefined

testDescribeUserProfiles :: DescribeUserProfiles -> TestTree
testDescribeUserProfiles = undefined

testDescribeStackSummary :: DescribeStackSummary -> TestTree
testDescribeStackSummary = undefined

testDescribeApps :: DescribeApps -> TestTree
testDescribeApps = undefined

testUpdateMyUserProfile :: UpdateMyUserProfile -> TestTree
testUpdateMyUserProfile = undefined

testDescribeInstances :: DescribeInstances -> TestTree
testDescribeInstances = undefined

testDescribeDeployments :: DescribeDeployments -> TestTree
testDescribeDeployments = undefined

testCreateStack :: CreateStack -> TestTree
testCreateStack = undefined

testGrantAccess :: GrantAccess -> TestTree
testGrantAccess = undefined

testDescribeElasticIPs :: DescribeElasticIPs -> TestTree
testDescribeElasticIPs = undefined

testDeleteLayer :: DeleteLayer -> TestTree
testDeleteLayer = undefined

testUpdateLayer :: UpdateLayer -> TestTree
testUpdateLayer = undefined

testCloneStack :: CloneStack -> TestTree
testCloneStack = undefined

testGetHostnameSuggestion :: GetHostnameSuggestion -> TestTree
testGetHostnameSuggestion = undefined

testCreateApp :: CreateApp -> TestTree
testCreateApp = undefined

testDescribePermissions :: DescribePermissions -> TestTree
testDescribePermissions = undefined

testUpdateElasticIP :: UpdateElasticIP -> TestTree
testUpdateElasticIP = undefined

testDescribeLoadBasedAutoScaling :: DescribeLoadBasedAutoScaling -> TestTree
testDescribeLoadBasedAutoScaling = undefined

testRegisterInstance :: RegisterInstance -> TestTree
testRegisterInstance = undefined

testAssociateElasticIP :: AssociateElasticIP -> TestTree
testAssociateElasticIP = undefined

testDetachElasticLoadBalancer :: DetachElasticLoadBalancer -> TestTree
testDetachElasticLoadBalancer = undefined

testDescribeStackProvisioningParameters :: DescribeStackProvisioningParameters -> TestTree
testDescribeStackProvisioningParameters = undefined

testDescribeMyUserProfile :: DescribeMyUserProfile -> TestTree
testDescribeMyUserProfile = undefined

testUnassignInstance :: UnassignInstance -> TestTree
testUnassignInstance = undefined

testRegisterRDSDBInstance :: RegisterRDSDBInstance -> TestTree
testRegisterRDSDBInstance = undefined

testDeleteUserProfile :: DeleteUserProfile -> TestTree
testDeleteUserProfile = undefined

testUpdateUserProfile :: UpdateUserProfile -> TestTree
testUpdateUserProfile = undefined

testDescribeServiceErrors :: DescribeServiceErrors -> TestTree
testDescribeServiceErrors = undefined

testStartStack :: StartStack -> TestTree
testStartStack = undefined

testCreateUserProfile :: CreateUserProfile -> TestTree
testCreateUserProfile = undefined

testDescribeCommands :: DescribeCommands -> TestTree
testDescribeCommands = undefined

testDescribeElasticLoadBalancers :: DescribeElasticLoadBalancers -> TestTree
testDescribeElasticLoadBalancers = undefined

testDeregisterInstance :: DeregisterInstance -> TestTree
testDeregisterInstance = undefined

testDescribeRAIDArrays :: DescribeRAIDArrays -> TestTree
testDescribeRAIDArrays = undefined

testSetPermission :: SetPermission -> TestTree
testSetPermission = undefined

testUpdateVolume :: UpdateVolume -> TestTree
testUpdateVolume = undefined

testAssignVolume :: AssignVolume -> TestTree
testAssignVolume = undefined

testStartInstance :: StartInstance -> TestTree
testStartInstance = undefined

-- Responses

testDescribeRDSDBInstancesResponse :: DescribeRDSDBInstancesResponse -> TestTree
testDescribeRDSDBInstancesResponse = resp
    "DescribeRDSDBInstancesResponse"
    "fixture/DescribeRDSDBInstancesResponse"
    (Proxy :: Proxy DescribeRDSDBInstances)

testDeleteStackResponse :: DeleteStackResponse -> TestTree
testDeleteStackResponse = resp
    "DeleteStackResponse"
    "fixture/DeleteStackResponse"
    (Proxy :: Proxy DeleteStack)

testUpdateStackResponse :: UpdateStackResponse -> TestTree
testUpdateStackResponse = resp
    "UpdateStackResponse"
    "fixture/UpdateStackResponse"
    (Proxy :: Proxy UpdateStack)

testCreateLayerResponse :: CreateLayerResponse -> TestTree
testCreateLayerResponse = resp
    "CreateLayerResponse"
    "fixture/CreateLayerResponse"
    (Proxy :: Proxy CreateLayer)

testSetLoadBasedAutoScalingResponse :: SetLoadBasedAutoScalingResponse -> TestTree
testSetLoadBasedAutoScalingResponse = resp
    "SetLoadBasedAutoScalingResponse"
    "fixture/SetLoadBasedAutoScalingResponse"
    (Proxy :: Proxy SetLoadBasedAutoScaling)

testUnassignVolumeResponse :: UnassignVolumeResponse -> TestTree
testUnassignVolumeResponse = resp
    "UnassignVolumeResponse"
    "fixture/UnassignVolumeResponse"
    (Proxy :: Proxy UnassignVolume)

testDeregisterRDSDBInstanceResponse :: DeregisterRDSDBInstanceResponse -> TestTree
testDeregisterRDSDBInstanceResponse = resp
    "DeregisterRDSDBInstanceResponse"
    "fixture/DeregisterRDSDBInstanceResponse"
    (Proxy :: Proxy DeregisterRDSDBInstance)

testCreateInstanceResponse :: CreateInstanceResponse -> TestTree
testCreateInstanceResponse = resp
    "CreateInstanceResponse"
    "fixture/CreateInstanceResponse"
    (Proxy :: Proxy CreateInstance)

testRegisterElasticIPResponse :: RegisterElasticIPResponse -> TestTree
testRegisterElasticIPResponse = resp
    "RegisterElasticIPResponse"
    "fixture/RegisterElasticIPResponse"
    (Proxy :: Proxy RegisterElasticIP)

testDescribeAgentVersionsResponse :: DescribeAgentVersionsResponse -> TestTree
testDescribeAgentVersionsResponse = resp
    "DescribeAgentVersionsResponse"
    "fixture/DescribeAgentVersionsResponse"
    (Proxy :: Proxy DescribeAgentVersions)

testDescribeLayersResponse :: DescribeLayersResponse -> TestTree
testDescribeLayersResponse = resp
    "DescribeLayersResponse"
    "fixture/DescribeLayersResponse"
    (Proxy :: Proxy DescribeLayers)

testCreateDeploymentResponse :: CreateDeploymentResponse -> TestTree
testCreateDeploymentResponse = resp
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse"
    (Proxy :: Proxy CreateDeployment)

testDeleteAppResponse :: DeleteAppResponse -> TestTree
testDeleteAppResponse = resp
    "DeleteAppResponse"
    "fixture/DeleteAppResponse"
    (Proxy :: Proxy DeleteApp)

testUpdateAppResponse :: UpdateAppResponse -> TestTree
testUpdateAppResponse = resp
    "UpdateAppResponse"
    "fixture/UpdateAppResponse"
    (Proxy :: Proxy UpdateApp)

testDeleteInstanceResponse :: DeleteInstanceResponse -> TestTree
testDeleteInstanceResponse = resp
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse"
    (Proxy :: Proxy DeleteInstance)

testUpdateInstanceResponse :: UpdateInstanceResponse -> TestTree
testUpdateInstanceResponse = resp
    "UpdateInstanceResponse"
    "fixture/UpdateInstanceResponse"
    (Proxy :: Proxy UpdateInstance)

testDescribeStacksResponse :: DescribeStacksResponse -> TestTree
testDescribeStacksResponse = resp
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse"
    (Proxy :: Proxy DescribeStacks)

testDeregisterVolumeResponse :: DeregisterVolumeResponse -> TestTree
testDeregisterVolumeResponse = resp
    "DeregisterVolumeResponse"
    "fixture/DeregisterVolumeResponse"
    (Proxy :: Proxy DeregisterVolume)

testAssignInstanceResponse :: AssignInstanceResponse -> TestTree
testAssignInstanceResponse = resp
    "AssignInstanceResponse"
    "fixture/AssignInstanceResponse"
    (Proxy :: Proxy AssignInstance)

testRebootInstanceResponse :: RebootInstanceResponse -> TestTree
testRebootInstanceResponse = resp
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse"
    (Proxy :: Proxy RebootInstance)

testDescribeTimeBasedAutoScalingResponse :: DescribeTimeBasedAutoScalingResponse -> TestTree
testDescribeTimeBasedAutoScalingResponse = resp
    "DescribeTimeBasedAutoScalingResponse"
    "fixture/DescribeTimeBasedAutoScalingResponse"
    (Proxy :: Proxy DescribeTimeBasedAutoScaling)

testUpdateRDSDBInstanceResponse :: UpdateRDSDBInstanceResponse -> TestTree
testUpdateRDSDBInstanceResponse = resp
    "UpdateRDSDBInstanceResponse"
    "fixture/UpdateRDSDBInstanceResponse"
    (Proxy :: Proxy UpdateRDSDBInstance)

testStopStackResponse :: StopStackResponse -> TestTree
testStopStackResponse = resp
    "StopStackResponse"
    "fixture/StopStackResponse"
    (Proxy :: Proxy StopStack)

testDescribeVolumesResponse :: DescribeVolumesResponse -> TestTree
testDescribeVolumesResponse = resp
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse"
    (Proxy :: Proxy DescribeVolumes)

testDisassociateElasticIPResponse :: DisassociateElasticIPResponse -> TestTree
testDisassociateElasticIPResponse = resp
    "DisassociateElasticIPResponse"
    "fixture/DisassociateElasticIPResponse"
    (Proxy :: Proxy DisassociateElasticIP)

testStopInstanceResponse :: StopInstanceResponse -> TestTree
testStopInstanceResponse = resp
    "StopInstanceResponse"
    "fixture/StopInstanceResponse"
    (Proxy :: Proxy StopInstance)

testRegisterVolumeResponse :: RegisterVolumeResponse -> TestTree
testRegisterVolumeResponse = resp
    "RegisterVolumeResponse"
    "fixture/RegisterVolumeResponse"
    (Proxy :: Proxy RegisterVolume)

testSetTimeBasedAutoScalingResponse :: SetTimeBasedAutoScalingResponse -> TestTree
testSetTimeBasedAutoScalingResponse = resp
    "SetTimeBasedAutoScalingResponse"
    "fixture/SetTimeBasedAutoScalingResponse"
    (Proxy :: Proxy SetTimeBasedAutoScaling)

testDeregisterElasticIPResponse :: DeregisterElasticIPResponse -> TestTree
testDeregisterElasticIPResponse = resp
    "DeregisterElasticIPResponse"
    "fixture/DeregisterElasticIPResponse"
    (Proxy :: Proxy DeregisterElasticIP)

testAttachElasticLoadBalancerResponse :: AttachElasticLoadBalancerResponse -> TestTree
testAttachElasticLoadBalancerResponse = resp
    "AttachElasticLoadBalancerResponse"
    "fixture/AttachElasticLoadBalancerResponse"
    (Proxy :: Proxy AttachElasticLoadBalancer)

testDescribeUserProfilesResponse :: DescribeUserProfilesResponse -> TestTree
testDescribeUserProfilesResponse = resp
    "DescribeUserProfilesResponse"
    "fixture/DescribeUserProfilesResponse"
    (Proxy :: Proxy DescribeUserProfiles)

testDescribeStackSummaryResponse :: DescribeStackSummaryResponse -> TestTree
testDescribeStackSummaryResponse = resp
    "DescribeStackSummaryResponse"
    "fixture/DescribeStackSummaryResponse"
    (Proxy :: Proxy DescribeStackSummary)

testDescribeAppsResponse :: DescribeAppsResponse -> TestTree
testDescribeAppsResponse = resp
    "DescribeAppsResponse"
    "fixture/DescribeAppsResponse"
    (Proxy :: Proxy DescribeApps)

testUpdateMyUserProfileResponse :: UpdateMyUserProfileResponse -> TestTree
testUpdateMyUserProfileResponse = resp
    "UpdateMyUserProfileResponse"
    "fixture/UpdateMyUserProfileResponse"
    (Proxy :: Proxy UpdateMyUserProfile)

testDescribeInstancesResponse :: DescribeInstancesResponse -> TestTree
testDescribeInstancesResponse = resp
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse"
    (Proxy :: Proxy DescribeInstances)

testDescribeDeploymentsResponse :: DescribeDeploymentsResponse -> TestTree
testDescribeDeploymentsResponse = resp
    "DescribeDeploymentsResponse"
    "fixture/DescribeDeploymentsResponse"
    (Proxy :: Proxy DescribeDeployments)

testCreateStackResponse :: CreateStackResponse -> TestTree
testCreateStackResponse = resp
    "CreateStackResponse"
    "fixture/CreateStackResponse"
    (Proxy :: Proxy CreateStack)

testGrantAccessResponse :: GrantAccessResponse -> TestTree
testGrantAccessResponse = resp
    "GrantAccessResponse"
    "fixture/GrantAccessResponse"
    (Proxy :: Proxy GrantAccess)

testDescribeElasticIPsResponse :: DescribeElasticIPsResponse -> TestTree
testDescribeElasticIPsResponse = resp
    "DescribeElasticIPsResponse"
    "fixture/DescribeElasticIPsResponse"
    (Proxy :: Proxy DescribeElasticIPs)

testDeleteLayerResponse :: DeleteLayerResponse -> TestTree
testDeleteLayerResponse = resp
    "DeleteLayerResponse"
    "fixture/DeleteLayerResponse"
    (Proxy :: Proxy DeleteLayer)

testUpdateLayerResponse :: UpdateLayerResponse -> TestTree
testUpdateLayerResponse = resp
    "UpdateLayerResponse"
    "fixture/UpdateLayerResponse"
    (Proxy :: Proxy UpdateLayer)

testCloneStackResponse :: CloneStackResponse -> TestTree
testCloneStackResponse = resp
    "CloneStackResponse"
    "fixture/CloneStackResponse"
    (Proxy :: Proxy CloneStack)

testGetHostnameSuggestionResponse :: GetHostnameSuggestionResponse -> TestTree
testGetHostnameSuggestionResponse = resp
    "GetHostnameSuggestionResponse"
    "fixture/GetHostnameSuggestionResponse"
    (Proxy :: Proxy GetHostnameSuggestion)

testCreateAppResponse :: CreateAppResponse -> TestTree
testCreateAppResponse = resp
    "CreateAppResponse"
    "fixture/CreateAppResponse"
    (Proxy :: Proxy CreateApp)

testDescribePermissionsResponse :: DescribePermissionsResponse -> TestTree
testDescribePermissionsResponse = resp
    "DescribePermissionsResponse"
    "fixture/DescribePermissionsResponse"
    (Proxy :: Proxy DescribePermissions)

testUpdateElasticIPResponse :: UpdateElasticIPResponse -> TestTree
testUpdateElasticIPResponse = resp
    "UpdateElasticIPResponse"
    "fixture/UpdateElasticIPResponse"
    (Proxy :: Proxy UpdateElasticIP)

testDescribeLoadBasedAutoScalingResponse :: DescribeLoadBasedAutoScalingResponse -> TestTree
testDescribeLoadBasedAutoScalingResponse = resp
    "DescribeLoadBasedAutoScalingResponse"
    "fixture/DescribeLoadBasedAutoScalingResponse"
    (Proxy :: Proxy DescribeLoadBasedAutoScaling)

testRegisterInstanceResponse :: RegisterInstanceResponse -> TestTree
testRegisterInstanceResponse = resp
    "RegisterInstanceResponse"
    "fixture/RegisterInstanceResponse"
    (Proxy :: Proxy RegisterInstance)

testAssociateElasticIPResponse :: AssociateElasticIPResponse -> TestTree
testAssociateElasticIPResponse = resp
    "AssociateElasticIPResponse"
    "fixture/AssociateElasticIPResponse"
    (Proxy :: Proxy AssociateElasticIP)

testDetachElasticLoadBalancerResponse :: DetachElasticLoadBalancerResponse -> TestTree
testDetachElasticLoadBalancerResponse = resp
    "DetachElasticLoadBalancerResponse"
    "fixture/DetachElasticLoadBalancerResponse"
    (Proxy :: Proxy DetachElasticLoadBalancer)

testDescribeStackProvisioningParametersResponse :: DescribeStackProvisioningParametersResponse -> TestTree
testDescribeStackProvisioningParametersResponse = resp
    "DescribeStackProvisioningParametersResponse"
    "fixture/DescribeStackProvisioningParametersResponse"
    (Proxy :: Proxy DescribeStackProvisioningParameters)

testDescribeMyUserProfileResponse :: DescribeMyUserProfileResponse -> TestTree
testDescribeMyUserProfileResponse = resp
    "DescribeMyUserProfileResponse"
    "fixture/DescribeMyUserProfileResponse"
    (Proxy :: Proxy DescribeMyUserProfile)

testUnassignInstanceResponse :: UnassignInstanceResponse -> TestTree
testUnassignInstanceResponse = resp
    "UnassignInstanceResponse"
    "fixture/UnassignInstanceResponse"
    (Proxy :: Proxy UnassignInstance)

testRegisterRDSDBInstanceResponse :: RegisterRDSDBInstanceResponse -> TestTree
testRegisterRDSDBInstanceResponse = resp
    "RegisterRDSDBInstanceResponse"
    "fixture/RegisterRDSDBInstanceResponse"
    (Proxy :: Proxy RegisterRDSDBInstance)

testDeleteUserProfileResponse :: DeleteUserProfileResponse -> TestTree
testDeleteUserProfileResponse = resp
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse"
    (Proxy :: Proxy DeleteUserProfile)

testUpdateUserProfileResponse :: UpdateUserProfileResponse -> TestTree
testUpdateUserProfileResponse = resp
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse"
    (Proxy :: Proxy UpdateUserProfile)

testDescribeServiceErrorsResponse :: DescribeServiceErrorsResponse -> TestTree
testDescribeServiceErrorsResponse = resp
    "DescribeServiceErrorsResponse"
    "fixture/DescribeServiceErrorsResponse"
    (Proxy :: Proxy DescribeServiceErrors)

testStartStackResponse :: StartStackResponse -> TestTree
testStartStackResponse = resp
    "StartStackResponse"
    "fixture/StartStackResponse"
    (Proxy :: Proxy StartStack)

testCreateUserProfileResponse :: CreateUserProfileResponse -> TestTree
testCreateUserProfileResponse = resp
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse"
    (Proxy :: Proxy CreateUserProfile)

testDescribeCommandsResponse :: DescribeCommandsResponse -> TestTree
testDescribeCommandsResponse = resp
    "DescribeCommandsResponse"
    "fixture/DescribeCommandsResponse"
    (Proxy :: Proxy DescribeCommands)

testDescribeElasticLoadBalancersResponse :: DescribeElasticLoadBalancersResponse -> TestTree
testDescribeElasticLoadBalancersResponse = resp
    "DescribeElasticLoadBalancersResponse"
    "fixture/DescribeElasticLoadBalancersResponse"
    (Proxy :: Proxy DescribeElasticLoadBalancers)

testDeregisterInstanceResponse :: DeregisterInstanceResponse -> TestTree
testDeregisterInstanceResponse = resp
    "DeregisterInstanceResponse"
    "fixture/DeregisterInstanceResponse"
    (Proxy :: Proxy DeregisterInstance)

testDescribeRAIDArraysResponse :: DescribeRAIDArraysResponse -> TestTree
testDescribeRAIDArraysResponse = resp
    "DescribeRAIDArraysResponse"
    "fixture/DescribeRAIDArraysResponse"
    (Proxy :: Proxy DescribeRAIDArrays)

testSetPermissionResponse :: SetPermissionResponse -> TestTree
testSetPermissionResponse = resp
    "SetPermissionResponse"
    "fixture/SetPermissionResponse"
    (Proxy :: Proxy SetPermission)

testUpdateVolumeResponse :: UpdateVolumeResponse -> TestTree
testUpdateVolumeResponse = resp
    "UpdateVolumeResponse"
    "fixture/UpdateVolumeResponse"
    (Proxy :: Proxy UpdateVolume)

testAssignVolumeResponse :: AssignVolumeResponse -> TestTree
testAssignVolumeResponse = resp
    "AssignVolumeResponse"
    "fixture/AssignVolumeResponse"
    (Proxy :: Proxy AssignVolume)

testStartInstanceResponse :: StartInstanceResponse -> TestTree
testStartInstanceResponse = resp
    "StartInstanceResponse"
    "fixture/StartInstanceResponse"
    (Proxy :: Proxy StartInstance)

instance Out AgentVersion
instance Out App
instance Out AppAttributesKeys
instance Out AppType
instance Out Architecture
instance Out AssignInstance
instance Out AssignInstanceResponse
instance Out AssignVolume
instance Out AssignVolumeResponse
instance Out AssociateElasticIP
instance Out AssociateElasticIPResponse
instance Out AttachElasticLoadBalancer
instance Out AttachElasticLoadBalancerResponse
instance Out AutoScalingThresholds
instance Out AutoScalingType
instance Out BlockDeviceMapping
instance Out ChefConfiguration
instance Out CloneStack
instance Out CloneStackResponse
instance Out Command
instance Out CreateApp
instance Out CreateAppResponse
instance Out CreateDeployment
instance Out CreateDeploymentResponse
instance Out CreateInstance
instance Out CreateInstanceResponse
instance Out CreateLayer
instance Out CreateLayerResponse
instance Out CreateStack
instance Out CreateStackResponse
instance Out CreateUserProfile
instance Out CreateUserProfileResponse
instance Out DataSource
instance Out DeleteApp
instance Out DeleteAppResponse
instance Out DeleteInstance
instance Out DeleteInstanceResponse
instance Out DeleteLayer
instance Out DeleteLayerResponse
instance Out DeleteStack
instance Out DeleteStackResponse
instance Out DeleteUserProfile
instance Out DeleteUserProfileResponse
instance Out Deployment
instance Out DeploymentCommand
instance Out DeploymentCommandName
instance Out DeregisterElasticIP
instance Out DeregisterElasticIPResponse
instance Out DeregisterInstance
instance Out DeregisterInstanceResponse
instance Out DeregisterRDSDBInstance
instance Out DeregisterRDSDBInstanceResponse
instance Out DeregisterVolume
instance Out DeregisterVolumeResponse
instance Out DescribeAgentVersions
instance Out DescribeAgentVersionsResponse
instance Out DescribeApps
instance Out DescribeAppsResponse
instance Out DescribeCommands
instance Out DescribeCommandsResponse
instance Out DescribeDeployments
instance Out DescribeDeploymentsResponse
instance Out DescribeElasticIPs
instance Out DescribeElasticIPsResponse
instance Out DescribeElasticLoadBalancers
instance Out DescribeElasticLoadBalancersResponse
instance Out DescribeInstances
instance Out DescribeInstancesResponse
instance Out DescribeLayers
instance Out DescribeLayersResponse
instance Out DescribeLoadBasedAutoScaling
instance Out DescribeLoadBasedAutoScalingResponse
instance Out DescribeMyUserProfile
instance Out DescribeMyUserProfileResponse
instance Out DescribePermissions
instance Out DescribePermissionsResponse
instance Out DescribeRAIDArrays
instance Out DescribeRAIDArraysResponse
instance Out DescribeRDSDBInstances
instance Out DescribeRDSDBInstancesResponse
instance Out DescribeServiceErrors
instance Out DescribeServiceErrorsResponse
instance Out DescribeStackProvisioningParameters
instance Out DescribeStackProvisioningParametersResponse
instance Out DescribeStackSummary
instance Out DescribeStackSummaryResponse
instance Out DescribeStacks
instance Out DescribeStacksResponse
instance Out DescribeTimeBasedAutoScaling
instance Out DescribeTimeBasedAutoScalingResponse
instance Out DescribeUserProfiles
instance Out DescribeUserProfilesResponse
instance Out DescribeVolumes
instance Out DescribeVolumesResponse
instance Out DetachElasticLoadBalancer
instance Out DetachElasticLoadBalancerResponse
instance Out DisassociateElasticIP
instance Out DisassociateElasticIPResponse
instance Out EBSBlockDevice
instance Out ElasticIP
instance Out ElasticLoadBalancer
instance Out EnvironmentVariable
instance Out GetHostnameSuggestion
instance Out GetHostnameSuggestionResponse
instance Out GrantAccess
instance Out GrantAccessResponse
instance Out Instance
instance Out InstanceIdentity
instance Out InstancesCount
instance Out Layer
instance Out LayerAttributesKeys
instance Out LayerType
instance Out LifecycleEventConfiguration
instance Out LoadBasedAutoScalingConfiguration
instance Out Permission
instance Out RAIDArray
instance Out RDSDBInstance
instance Out RebootInstance
instance Out RebootInstanceResponse
instance Out Recipes
instance Out RegisterElasticIP
instance Out RegisterElasticIPResponse
instance Out RegisterInstance
instance Out RegisterInstanceResponse
instance Out RegisterRDSDBInstance
instance Out RegisterRDSDBInstanceResponse
instance Out RegisterVolume
instance Out RegisterVolumeResponse
instance Out ReportedOS
instance Out RootDeviceType
instance Out SSLConfiguration
instance Out SelfUserProfile
instance Out ServiceError'
instance Out SetLoadBasedAutoScaling
instance Out SetLoadBasedAutoScalingResponse
instance Out SetPermission
instance Out SetPermissionResponse
instance Out SetTimeBasedAutoScaling
instance Out SetTimeBasedAutoScalingResponse
instance Out ShutdownEventConfiguration
instance Out Source
instance Out SourceType
instance Out Stack
instance Out StackAttributesKeys
instance Out StackConfigurationManager
instance Out StackSummary
instance Out StartInstance
instance Out StartInstanceResponse
instance Out StartStack
instance Out StartStackResponse
instance Out StopInstance
instance Out StopInstanceResponse
instance Out StopStack
instance Out StopStackResponse
instance Out TemporaryCredential
instance Out TimeBasedAutoScalingConfiguration
instance Out UnassignInstance
instance Out UnassignInstanceResponse
instance Out UnassignVolume
instance Out UnassignVolumeResponse
instance Out UpdateApp
instance Out UpdateAppResponse
instance Out UpdateElasticIP
instance Out UpdateElasticIPResponse
instance Out UpdateInstance
instance Out UpdateInstanceResponse
instance Out UpdateLayer
instance Out UpdateLayerResponse
instance Out UpdateMyUserProfile
instance Out UpdateMyUserProfileResponse
instance Out UpdateRDSDBInstance
instance Out UpdateRDSDBInstanceResponse
instance Out UpdateStack
instance Out UpdateStackResponse
instance Out UpdateUserProfile
instance Out UpdateUserProfileResponse
instance Out UpdateVolume
instance Out UpdateVolumeResponse
instance Out UserProfile
instance Out VirtualizationType
instance Out Volume
instance Out VolumeConfiguration
instance Out VolumeType
instance Out WeeklyAutoScalingSchedule
