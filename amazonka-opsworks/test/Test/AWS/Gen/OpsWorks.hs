{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

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
import Test.AWS.OpsWorks.Internal

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
--         , testDescribeRAIdArrays $
--             describeRAIdArrays
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
--         , testDescribeRAIdArraysResponse $
--             describeRAIdArraysResponse
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
testDescribeRDSDBInstances = req
    "DescribeRDSDBInstances"
    "fixture/DescribeRDSDBInstances"

testDeleteStack :: DeleteStack -> TestTree
testDeleteStack = req
    "DeleteStack"
    "fixture/DeleteStack"

testUpdateStack :: UpdateStack -> TestTree
testUpdateStack = req
    "UpdateStack"
    "fixture/UpdateStack"

testCreateLayer :: CreateLayer -> TestTree
testCreateLayer = req
    "CreateLayer"
    "fixture/CreateLayer"

testSetLoadBasedAutoScaling :: SetLoadBasedAutoScaling -> TestTree
testSetLoadBasedAutoScaling = req
    "SetLoadBasedAutoScaling"
    "fixture/SetLoadBasedAutoScaling"

testUnassignVolume :: UnassignVolume -> TestTree
testUnassignVolume = req
    "UnassignVolume"
    "fixture/UnassignVolume"

testDeregisterRDSDBInstance :: DeregisterRDSDBInstance -> TestTree
testDeregisterRDSDBInstance = req
    "DeregisterRDSDBInstance"
    "fixture/DeregisterRDSDBInstance"

testCreateInstance :: CreateInstance -> TestTree
testCreateInstance = req
    "CreateInstance"
    "fixture/CreateInstance"

testRegisterElasticIP :: RegisterElasticIP -> TestTree
testRegisterElasticIP = req
    "RegisterElasticIP"
    "fixture/RegisterElasticIP"

testDescribeAgentVersions :: DescribeAgentVersions -> TestTree
testDescribeAgentVersions = req
    "DescribeAgentVersions"
    "fixture/DescribeAgentVersions"

testDescribeLayers :: DescribeLayers -> TestTree
testDescribeLayers = req
    "DescribeLayers"
    "fixture/DescribeLayers"

testCreateDeployment :: CreateDeployment -> TestTree
testCreateDeployment = req
    "CreateDeployment"
    "fixture/CreateDeployment"

testDeleteApp :: DeleteApp -> TestTree
testDeleteApp = req
    "DeleteApp"
    "fixture/DeleteApp"

testUpdateApp :: UpdateApp -> TestTree
testUpdateApp = req
    "UpdateApp"
    "fixture/UpdateApp"

testDeleteInstance :: DeleteInstance -> TestTree
testDeleteInstance = req
    "DeleteInstance"
    "fixture/DeleteInstance"

testUpdateInstance :: UpdateInstance -> TestTree
testUpdateInstance = req
    "UpdateInstance"
    "fixture/UpdateInstance"

testDescribeStacks :: DescribeStacks -> TestTree
testDescribeStacks = req
    "DescribeStacks"
    "fixture/DescribeStacks"

testDeregisterVolume :: DeregisterVolume -> TestTree
testDeregisterVolume = req
    "DeregisterVolume"
    "fixture/DeregisterVolume"

testAssignInstance :: AssignInstance -> TestTree
testAssignInstance = req
    "AssignInstance"
    "fixture/AssignInstance"

testRebootInstance :: RebootInstance -> TestTree
testRebootInstance = req
    "RebootInstance"
    "fixture/RebootInstance"

testDescribeTimeBasedAutoScaling :: DescribeTimeBasedAutoScaling -> TestTree
testDescribeTimeBasedAutoScaling = req
    "DescribeTimeBasedAutoScaling"
    "fixture/DescribeTimeBasedAutoScaling"

testUpdateRDSDBInstance :: UpdateRDSDBInstance -> TestTree
testUpdateRDSDBInstance = req
    "UpdateRDSDBInstance"
    "fixture/UpdateRDSDBInstance"

testStopStack :: StopStack -> TestTree
testStopStack = req
    "StopStack"
    "fixture/StopStack"

testDescribeVolumes :: DescribeVolumes -> TestTree
testDescribeVolumes = req
    "DescribeVolumes"
    "fixture/DescribeVolumes"

testDisassociateElasticIP :: DisassociateElasticIP -> TestTree
testDisassociateElasticIP = req
    "DisassociateElasticIP"
    "fixture/DisassociateElasticIP"

testStopInstance :: StopInstance -> TestTree
testStopInstance = req
    "StopInstance"
    "fixture/StopInstance"

testRegisterVolume :: RegisterVolume -> TestTree
testRegisterVolume = req
    "RegisterVolume"
    "fixture/RegisterVolume"

testSetTimeBasedAutoScaling :: SetTimeBasedAutoScaling -> TestTree
testSetTimeBasedAutoScaling = req
    "SetTimeBasedAutoScaling"
    "fixture/SetTimeBasedAutoScaling"

testDeregisterElasticIP :: DeregisterElasticIP -> TestTree
testDeregisterElasticIP = req
    "DeregisterElasticIP"
    "fixture/DeregisterElasticIP"

testAttachElasticLoadBalancer :: AttachElasticLoadBalancer -> TestTree
testAttachElasticLoadBalancer = req
    "AttachElasticLoadBalancer"
    "fixture/AttachElasticLoadBalancer"

testDescribeUserProfiles :: DescribeUserProfiles -> TestTree
testDescribeUserProfiles = req
    "DescribeUserProfiles"
    "fixture/DescribeUserProfiles"

testDescribeStackSummary :: DescribeStackSummary -> TestTree
testDescribeStackSummary = req
    "DescribeStackSummary"
    "fixture/DescribeStackSummary"

testDescribeApps :: DescribeApps -> TestTree
testDescribeApps = req
    "DescribeApps"
    "fixture/DescribeApps"

testUpdateMyUserProfile :: UpdateMyUserProfile -> TestTree
testUpdateMyUserProfile = req
    "UpdateMyUserProfile"
    "fixture/UpdateMyUserProfile"

testDescribeInstances :: DescribeInstances -> TestTree
testDescribeInstances = req
    "DescribeInstances"
    "fixture/DescribeInstances"

testDescribeDeployments :: DescribeDeployments -> TestTree
testDescribeDeployments = req
    "DescribeDeployments"
    "fixture/DescribeDeployments"

testCreateStack :: CreateStack -> TestTree
testCreateStack = req
    "CreateStack"
    "fixture/CreateStack"

testGrantAccess :: GrantAccess -> TestTree
testGrantAccess = req
    "GrantAccess"
    "fixture/GrantAccess"

testDescribeElasticIPs :: DescribeElasticIPs -> TestTree
testDescribeElasticIPs = req
    "DescribeElasticIPs"
    "fixture/DescribeElasticIPs"

testDeleteLayer :: DeleteLayer -> TestTree
testDeleteLayer = req
    "DeleteLayer"
    "fixture/DeleteLayer"

testUpdateLayer :: UpdateLayer -> TestTree
testUpdateLayer = req
    "UpdateLayer"
    "fixture/UpdateLayer"

testCloneStack :: CloneStack -> TestTree
testCloneStack = req
    "CloneStack"
    "fixture/CloneStack"

testGetHostnameSuggestion :: GetHostnameSuggestion -> TestTree
testGetHostnameSuggestion = req
    "GetHostnameSuggestion"
    "fixture/GetHostnameSuggestion"

testCreateApp :: CreateApp -> TestTree
testCreateApp = req
    "CreateApp"
    "fixture/CreateApp"

testDescribePermissions :: DescribePermissions -> TestTree
testDescribePermissions = req
    "DescribePermissions"
    "fixture/DescribePermissions"

testUpdateElasticIP :: UpdateElasticIP -> TestTree
testUpdateElasticIP = req
    "UpdateElasticIP"
    "fixture/UpdateElasticIP"

testDescribeLoadBasedAutoScaling :: DescribeLoadBasedAutoScaling -> TestTree
testDescribeLoadBasedAutoScaling = req
    "DescribeLoadBasedAutoScaling"
    "fixture/DescribeLoadBasedAutoScaling"

testRegisterInstance :: RegisterInstance -> TestTree
testRegisterInstance = req
    "RegisterInstance"
    "fixture/RegisterInstance"

testAssociateElasticIP :: AssociateElasticIP -> TestTree
testAssociateElasticIP = req
    "AssociateElasticIP"
    "fixture/AssociateElasticIP"

testDetachElasticLoadBalancer :: DetachElasticLoadBalancer -> TestTree
testDetachElasticLoadBalancer = req
    "DetachElasticLoadBalancer"
    "fixture/DetachElasticLoadBalancer"

testDescribeStackProvisioningParameters :: DescribeStackProvisioningParameters -> TestTree
testDescribeStackProvisioningParameters = req
    "DescribeStackProvisioningParameters"
    "fixture/DescribeStackProvisioningParameters"

testDescribeMyUserProfile :: DescribeMyUserProfile -> TestTree
testDescribeMyUserProfile = req
    "DescribeMyUserProfile"
    "fixture/DescribeMyUserProfile"

testUnassignInstance :: UnassignInstance -> TestTree
testUnassignInstance = req
    "UnassignInstance"
    "fixture/UnassignInstance"

testRegisterRDSDBInstance :: RegisterRDSDBInstance -> TestTree
testRegisterRDSDBInstance = req
    "RegisterRDSDBInstance"
    "fixture/RegisterRDSDBInstance"

testDeleteUserProfile :: DeleteUserProfile -> TestTree
testDeleteUserProfile = req
    "DeleteUserProfile"
    "fixture/DeleteUserProfile"

testUpdateUserProfile :: UpdateUserProfile -> TestTree
testUpdateUserProfile = req
    "UpdateUserProfile"
    "fixture/UpdateUserProfile"

testDescribeServiceErrors :: DescribeServiceErrors -> TestTree
testDescribeServiceErrors = req
    "DescribeServiceErrors"
    "fixture/DescribeServiceErrors"

testStartStack :: StartStack -> TestTree
testStartStack = req
    "StartStack"
    "fixture/StartStack"

testCreateUserProfile :: CreateUserProfile -> TestTree
testCreateUserProfile = req
    "CreateUserProfile"
    "fixture/CreateUserProfile"

testDescribeCommands :: DescribeCommands -> TestTree
testDescribeCommands = req
    "DescribeCommands"
    "fixture/DescribeCommands"

testDescribeElasticLoadBalancers :: DescribeElasticLoadBalancers -> TestTree
testDescribeElasticLoadBalancers = req
    "DescribeElasticLoadBalancers"
    "fixture/DescribeElasticLoadBalancers"

testDeregisterInstance :: DeregisterInstance -> TestTree
testDeregisterInstance = req
    "DeregisterInstance"
    "fixture/DeregisterInstance"

testDescribeRAIdArrays :: DescribeRAIdArrays -> TestTree
testDescribeRAIdArrays = req
    "DescribeRAIdArrays"
    "fixture/DescribeRAIdArrays"

testSetPermission :: SetPermission -> TestTree
testSetPermission = req
    "SetPermission"
    "fixture/SetPermission"

testUpdateVolume :: UpdateVolume -> TestTree
testUpdateVolume = req
    "UpdateVolume"
    "fixture/UpdateVolume"

testAssignVolume :: AssignVolume -> TestTree
testAssignVolume = req
    "AssignVolume"
    "fixture/AssignVolume"

testStartInstance :: StartInstance -> TestTree
testStartInstance = req
    "StartInstance"
    "fixture/StartInstance"

-- Responses

testDescribeRDSDBInstancesResponse :: DescribeRDSDBInstancesResponse -> TestTree
testDescribeRDSDBInstancesResponse = res
    "DescribeRDSDBInstancesResponse"
    "fixture/DescribeRDSDBInstancesResponse"
    (Proxy :: Proxy DescribeRDSDBInstances)

testDeleteStackResponse :: DeleteStackResponse -> TestTree
testDeleteStackResponse = res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse"
    (Proxy :: Proxy DeleteStack)

testUpdateStackResponse :: UpdateStackResponse -> TestTree
testUpdateStackResponse = res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse"
    (Proxy :: Proxy UpdateStack)

testCreateLayerResponse :: CreateLayerResponse -> TestTree
testCreateLayerResponse = res
    "CreateLayerResponse"
    "fixture/CreateLayerResponse"
    (Proxy :: Proxy CreateLayer)

testSetLoadBasedAutoScalingResponse :: SetLoadBasedAutoScalingResponse -> TestTree
testSetLoadBasedAutoScalingResponse = res
    "SetLoadBasedAutoScalingResponse"
    "fixture/SetLoadBasedAutoScalingResponse"
    (Proxy :: Proxy SetLoadBasedAutoScaling)

testUnassignVolumeResponse :: UnassignVolumeResponse -> TestTree
testUnassignVolumeResponse = res
    "UnassignVolumeResponse"
    "fixture/UnassignVolumeResponse"
    (Proxy :: Proxy UnassignVolume)

testDeregisterRDSDBInstanceResponse :: DeregisterRDSDBInstanceResponse -> TestTree
testDeregisterRDSDBInstanceResponse = res
    "DeregisterRDSDBInstanceResponse"
    "fixture/DeregisterRDSDBInstanceResponse"
    (Proxy :: Proxy DeregisterRDSDBInstance)

testCreateInstanceResponse :: CreateInstanceResponse -> TestTree
testCreateInstanceResponse = res
    "CreateInstanceResponse"
    "fixture/CreateInstanceResponse"
    (Proxy :: Proxy CreateInstance)

testRegisterElasticIPResponse :: RegisterElasticIPResponse -> TestTree
testRegisterElasticIPResponse = res
    "RegisterElasticIPResponse"
    "fixture/RegisterElasticIPResponse"
    (Proxy :: Proxy RegisterElasticIP)

testDescribeAgentVersionsResponse :: DescribeAgentVersionsResponse -> TestTree
testDescribeAgentVersionsResponse = res
    "DescribeAgentVersionsResponse"
    "fixture/DescribeAgentVersionsResponse"
    (Proxy :: Proxy DescribeAgentVersions)

testDescribeLayersResponse :: DescribeLayersResponse -> TestTree
testDescribeLayersResponse = res
    "DescribeLayersResponse"
    "fixture/DescribeLayersResponse"
    (Proxy :: Proxy DescribeLayers)

testCreateDeploymentResponse :: CreateDeploymentResponse -> TestTree
testCreateDeploymentResponse = res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse"
    (Proxy :: Proxy CreateDeployment)

testDeleteAppResponse :: DeleteAppResponse -> TestTree
testDeleteAppResponse = res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse"
    (Proxy :: Proxy DeleteApp)

testUpdateAppResponse :: UpdateAppResponse -> TestTree
testUpdateAppResponse = res
    "UpdateAppResponse"
    "fixture/UpdateAppResponse"
    (Proxy :: Proxy UpdateApp)

testDeleteInstanceResponse :: DeleteInstanceResponse -> TestTree
testDeleteInstanceResponse = res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse"
    (Proxy :: Proxy DeleteInstance)

testUpdateInstanceResponse :: UpdateInstanceResponse -> TestTree
testUpdateInstanceResponse = res
    "UpdateInstanceResponse"
    "fixture/UpdateInstanceResponse"
    (Proxy :: Proxy UpdateInstance)

testDescribeStacksResponse :: DescribeStacksResponse -> TestTree
testDescribeStacksResponse = res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse"
    (Proxy :: Proxy DescribeStacks)

testDeregisterVolumeResponse :: DeregisterVolumeResponse -> TestTree
testDeregisterVolumeResponse = res
    "DeregisterVolumeResponse"
    "fixture/DeregisterVolumeResponse"
    (Proxy :: Proxy DeregisterVolume)

testAssignInstanceResponse :: AssignInstanceResponse -> TestTree
testAssignInstanceResponse = res
    "AssignInstanceResponse"
    "fixture/AssignInstanceResponse"
    (Proxy :: Proxy AssignInstance)

testRebootInstanceResponse :: RebootInstanceResponse -> TestTree
testRebootInstanceResponse = res
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse"
    (Proxy :: Proxy RebootInstance)

testDescribeTimeBasedAutoScalingResponse :: DescribeTimeBasedAutoScalingResponse -> TestTree
testDescribeTimeBasedAutoScalingResponse = res
    "DescribeTimeBasedAutoScalingResponse"
    "fixture/DescribeTimeBasedAutoScalingResponse"
    (Proxy :: Proxy DescribeTimeBasedAutoScaling)

testUpdateRDSDBInstanceResponse :: UpdateRDSDBInstanceResponse -> TestTree
testUpdateRDSDBInstanceResponse = res
    "UpdateRDSDBInstanceResponse"
    "fixture/UpdateRDSDBInstanceResponse"
    (Proxy :: Proxy UpdateRDSDBInstance)

testStopStackResponse :: StopStackResponse -> TestTree
testStopStackResponse = res
    "StopStackResponse"
    "fixture/StopStackResponse"
    (Proxy :: Proxy StopStack)

testDescribeVolumesResponse :: DescribeVolumesResponse -> TestTree
testDescribeVolumesResponse = res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse"
    (Proxy :: Proxy DescribeVolumes)

testDisassociateElasticIPResponse :: DisassociateElasticIPResponse -> TestTree
testDisassociateElasticIPResponse = res
    "DisassociateElasticIPResponse"
    "fixture/DisassociateElasticIPResponse"
    (Proxy :: Proxy DisassociateElasticIP)

testStopInstanceResponse :: StopInstanceResponse -> TestTree
testStopInstanceResponse = res
    "StopInstanceResponse"
    "fixture/StopInstanceResponse"
    (Proxy :: Proxy StopInstance)

testRegisterVolumeResponse :: RegisterVolumeResponse -> TestTree
testRegisterVolumeResponse = res
    "RegisterVolumeResponse"
    "fixture/RegisterVolumeResponse"
    (Proxy :: Proxy RegisterVolume)

testSetTimeBasedAutoScalingResponse :: SetTimeBasedAutoScalingResponse -> TestTree
testSetTimeBasedAutoScalingResponse = res
    "SetTimeBasedAutoScalingResponse"
    "fixture/SetTimeBasedAutoScalingResponse"
    (Proxy :: Proxy SetTimeBasedAutoScaling)

testDeregisterElasticIPResponse :: DeregisterElasticIPResponse -> TestTree
testDeregisterElasticIPResponse = res
    "DeregisterElasticIPResponse"
    "fixture/DeregisterElasticIPResponse"
    (Proxy :: Proxy DeregisterElasticIP)

testAttachElasticLoadBalancerResponse :: AttachElasticLoadBalancerResponse -> TestTree
testAttachElasticLoadBalancerResponse = res
    "AttachElasticLoadBalancerResponse"
    "fixture/AttachElasticLoadBalancerResponse"
    (Proxy :: Proxy AttachElasticLoadBalancer)

testDescribeUserProfilesResponse :: DescribeUserProfilesResponse -> TestTree
testDescribeUserProfilesResponse = res
    "DescribeUserProfilesResponse"
    "fixture/DescribeUserProfilesResponse"
    (Proxy :: Proxy DescribeUserProfiles)

testDescribeStackSummaryResponse :: DescribeStackSummaryResponse -> TestTree
testDescribeStackSummaryResponse = res
    "DescribeStackSummaryResponse"
    "fixture/DescribeStackSummaryResponse"
    (Proxy :: Proxy DescribeStackSummary)

testDescribeAppsResponse :: DescribeAppsResponse -> TestTree
testDescribeAppsResponse = res
    "DescribeAppsResponse"
    "fixture/DescribeAppsResponse"
    (Proxy :: Proxy DescribeApps)

testUpdateMyUserProfileResponse :: UpdateMyUserProfileResponse -> TestTree
testUpdateMyUserProfileResponse = res
    "UpdateMyUserProfileResponse"
    "fixture/UpdateMyUserProfileResponse"
    (Proxy :: Proxy UpdateMyUserProfile)

testDescribeInstancesResponse :: DescribeInstancesResponse -> TestTree
testDescribeInstancesResponse = res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse"
    (Proxy :: Proxy DescribeInstances)

testDescribeDeploymentsResponse :: DescribeDeploymentsResponse -> TestTree
testDescribeDeploymentsResponse = res
    "DescribeDeploymentsResponse"
    "fixture/DescribeDeploymentsResponse"
    (Proxy :: Proxy DescribeDeployments)

testCreateStackResponse :: CreateStackResponse -> TestTree
testCreateStackResponse = res
    "CreateStackResponse"
    "fixture/CreateStackResponse"
    (Proxy :: Proxy CreateStack)

testGrantAccessResponse :: GrantAccessResponse -> TestTree
testGrantAccessResponse = res
    "GrantAccessResponse"
    "fixture/GrantAccessResponse"
    (Proxy :: Proxy GrantAccess)

testDescribeElasticIPsResponse :: DescribeElasticIPsResponse -> TestTree
testDescribeElasticIPsResponse = res
    "DescribeElasticIPsResponse"
    "fixture/DescribeElasticIPsResponse"
    (Proxy :: Proxy DescribeElasticIPs)

testDeleteLayerResponse :: DeleteLayerResponse -> TestTree
testDeleteLayerResponse = res
    "DeleteLayerResponse"
    "fixture/DeleteLayerResponse"
    (Proxy :: Proxy DeleteLayer)

testUpdateLayerResponse :: UpdateLayerResponse -> TestTree
testUpdateLayerResponse = res
    "UpdateLayerResponse"
    "fixture/UpdateLayerResponse"
    (Proxy :: Proxy UpdateLayer)

testCloneStackResponse :: CloneStackResponse -> TestTree
testCloneStackResponse = res
    "CloneStackResponse"
    "fixture/CloneStackResponse"
    (Proxy :: Proxy CloneStack)

testGetHostnameSuggestionResponse :: GetHostnameSuggestionResponse -> TestTree
testGetHostnameSuggestionResponse = res
    "GetHostnameSuggestionResponse"
    "fixture/GetHostnameSuggestionResponse"
    (Proxy :: Proxy GetHostnameSuggestion)

testCreateAppResponse :: CreateAppResponse -> TestTree
testCreateAppResponse = res
    "CreateAppResponse"
    "fixture/CreateAppResponse"
    (Proxy :: Proxy CreateApp)

testDescribePermissionsResponse :: DescribePermissionsResponse -> TestTree
testDescribePermissionsResponse = res
    "DescribePermissionsResponse"
    "fixture/DescribePermissionsResponse"
    (Proxy :: Proxy DescribePermissions)

testUpdateElasticIPResponse :: UpdateElasticIPResponse -> TestTree
testUpdateElasticIPResponse = res
    "UpdateElasticIPResponse"
    "fixture/UpdateElasticIPResponse"
    (Proxy :: Proxy UpdateElasticIP)

testDescribeLoadBasedAutoScalingResponse :: DescribeLoadBasedAutoScalingResponse -> TestTree
testDescribeLoadBasedAutoScalingResponse = res
    "DescribeLoadBasedAutoScalingResponse"
    "fixture/DescribeLoadBasedAutoScalingResponse"
    (Proxy :: Proxy DescribeLoadBasedAutoScaling)

testRegisterInstanceResponse :: RegisterInstanceResponse -> TestTree
testRegisterInstanceResponse = res
    "RegisterInstanceResponse"
    "fixture/RegisterInstanceResponse"
    (Proxy :: Proxy RegisterInstance)

testAssociateElasticIPResponse :: AssociateElasticIPResponse -> TestTree
testAssociateElasticIPResponse = res
    "AssociateElasticIPResponse"
    "fixture/AssociateElasticIPResponse"
    (Proxy :: Proxy AssociateElasticIP)

testDetachElasticLoadBalancerResponse :: DetachElasticLoadBalancerResponse -> TestTree
testDetachElasticLoadBalancerResponse = res
    "DetachElasticLoadBalancerResponse"
    "fixture/DetachElasticLoadBalancerResponse"
    (Proxy :: Proxy DetachElasticLoadBalancer)

testDescribeStackProvisioningParametersResponse :: DescribeStackProvisioningParametersResponse -> TestTree
testDescribeStackProvisioningParametersResponse = res
    "DescribeStackProvisioningParametersResponse"
    "fixture/DescribeStackProvisioningParametersResponse"
    (Proxy :: Proxy DescribeStackProvisioningParameters)

testDescribeMyUserProfileResponse :: DescribeMyUserProfileResponse -> TestTree
testDescribeMyUserProfileResponse = res
    "DescribeMyUserProfileResponse"
    "fixture/DescribeMyUserProfileResponse"
    (Proxy :: Proxy DescribeMyUserProfile)

testUnassignInstanceResponse :: UnassignInstanceResponse -> TestTree
testUnassignInstanceResponse = res
    "UnassignInstanceResponse"
    "fixture/UnassignInstanceResponse"
    (Proxy :: Proxy UnassignInstance)

testRegisterRDSDBInstanceResponse :: RegisterRDSDBInstanceResponse -> TestTree
testRegisterRDSDBInstanceResponse = res
    "RegisterRDSDBInstanceResponse"
    "fixture/RegisterRDSDBInstanceResponse"
    (Proxy :: Proxy RegisterRDSDBInstance)

testDeleteUserProfileResponse :: DeleteUserProfileResponse -> TestTree
testDeleteUserProfileResponse = res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse"
    (Proxy :: Proxy DeleteUserProfile)

testUpdateUserProfileResponse :: UpdateUserProfileResponse -> TestTree
testUpdateUserProfileResponse = res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse"
    (Proxy :: Proxy UpdateUserProfile)

testDescribeServiceErrorsResponse :: DescribeServiceErrorsResponse -> TestTree
testDescribeServiceErrorsResponse = res
    "DescribeServiceErrorsResponse"
    "fixture/DescribeServiceErrorsResponse"
    (Proxy :: Proxy DescribeServiceErrors)

testStartStackResponse :: StartStackResponse -> TestTree
testStartStackResponse = res
    "StartStackResponse"
    "fixture/StartStackResponse"
    (Proxy :: Proxy StartStack)

testCreateUserProfileResponse :: CreateUserProfileResponse -> TestTree
testCreateUserProfileResponse = res
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse"
    (Proxy :: Proxy CreateUserProfile)

testDescribeCommandsResponse :: DescribeCommandsResponse -> TestTree
testDescribeCommandsResponse = res
    "DescribeCommandsResponse"
    "fixture/DescribeCommandsResponse"
    (Proxy :: Proxy DescribeCommands)

testDescribeElasticLoadBalancersResponse :: DescribeElasticLoadBalancersResponse -> TestTree
testDescribeElasticLoadBalancersResponse = res
    "DescribeElasticLoadBalancersResponse"
    "fixture/DescribeElasticLoadBalancersResponse"
    (Proxy :: Proxy DescribeElasticLoadBalancers)

testDeregisterInstanceResponse :: DeregisterInstanceResponse -> TestTree
testDeregisterInstanceResponse = res
    "DeregisterInstanceResponse"
    "fixture/DeregisterInstanceResponse"
    (Proxy :: Proxy DeregisterInstance)

testDescribeRAIdArraysResponse :: DescribeRAIdArraysResponse -> TestTree
testDescribeRAIdArraysResponse = res
    "DescribeRAIdArraysResponse"
    "fixture/DescribeRAIdArraysResponse"
    (Proxy :: Proxy DescribeRAIdArrays)

testSetPermissionResponse :: SetPermissionResponse -> TestTree
testSetPermissionResponse = res
    "SetPermissionResponse"
    "fixture/SetPermissionResponse"
    (Proxy :: Proxy SetPermission)

testUpdateVolumeResponse :: UpdateVolumeResponse -> TestTree
testUpdateVolumeResponse = res
    "UpdateVolumeResponse"
    "fixture/UpdateVolumeResponse"
    (Proxy :: Proxy UpdateVolume)

testAssignVolumeResponse :: AssignVolumeResponse -> TestTree
testAssignVolumeResponse = res
    "AssignVolumeResponse"
    "fixture/AssignVolumeResponse"
    (Proxy :: Proxy AssignVolume)

testStartInstanceResponse :: StartInstanceResponse -> TestTree
testStartInstanceResponse = res
    "StartInstanceResponse"
    "fixture/StartInstanceResponse"
    (Proxy :: Proxy StartInstance)
