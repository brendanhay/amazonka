{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.OpsWorks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
--         , testRegisterEcsCluster $
--             registerEcsCluster
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
--         , testDeregisterEcsCluster $
--             deregisterEcsCluster
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
--         , testDescribeEcsClusters $
--             describeEcsClusters
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
--         , testRegisterEcsClusterResponse $
--             registerEcsClusterResponse
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
--         , testDeregisterEcsClusterResponse $
--             deregisterEcsClusterResponse
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
--         , testDescribeEcsClustersResponse $
--             describeEcsClustersResponse
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
    "fixture/DescribeRDSDBInstances.yaml"

testDeleteStack :: DeleteStack -> TestTree
testDeleteStack = req
    "DeleteStack"
    "fixture/DeleteStack.yaml"

testUpdateStack :: UpdateStack -> TestTree
testUpdateStack = req
    "UpdateStack"
    "fixture/UpdateStack.yaml"

testCreateLayer :: CreateLayer -> TestTree
testCreateLayer = req
    "CreateLayer"
    "fixture/CreateLayer.yaml"

testSetLoadBasedAutoScaling :: SetLoadBasedAutoScaling -> TestTree
testSetLoadBasedAutoScaling = req
    "SetLoadBasedAutoScaling"
    "fixture/SetLoadBasedAutoScaling.yaml"

testUnassignVolume :: UnassignVolume -> TestTree
testUnassignVolume = req
    "UnassignVolume"
    "fixture/UnassignVolume.yaml"

testDeregisterRDSDBInstance :: DeregisterRDSDBInstance -> TestTree
testDeregisterRDSDBInstance = req
    "DeregisterRDSDBInstance"
    "fixture/DeregisterRDSDBInstance.yaml"

testCreateInstance :: CreateInstance -> TestTree
testCreateInstance = req
    "CreateInstance"
    "fixture/CreateInstance.yaml"

testRegisterElasticIP :: RegisterElasticIP -> TestTree
testRegisterElasticIP = req
    "RegisterElasticIP"
    "fixture/RegisterElasticIP.yaml"

testDescribeAgentVersions :: DescribeAgentVersions -> TestTree
testDescribeAgentVersions = req
    "DescribeAgentVersions"
    "fixture/DescribeAgentVersions.yaml"

testDescribeLayers :: DescribeLayers -> TestTree
testDescribeLayers = req
    "DescribeLayers"
    "fixture/DescribeLayers.yaml"

testCreateDeployment :: CreateDeployment -> TestTree
testCreateDeployment = req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

testDeleteApp :: DeleteApp -> TestTree
testDeleteApp = req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

testUpdateApp :: UpdateApp -> TestTree
testUpdateApp = req
    "UpdateApp"
    "fixture/UpdateApp.yaml"

testDeleteInstance :: DeleteInstance -> TestTree
testDeleteInstance = req
    "DeleteInstance"
    "fixture/DeleteInstance.yaml"

testUpdateInstance :: UpdateInstance -> TestTree
testUpdateInstance = req
    "UpdateInstance"
    "fixture/UpdateInstance.yaml"

testDescribeStacks :: DescribeStacks -> TestTree
testDescribeStacks = req
    "DescribeStacks"
    "fixture/DescribeStacks.yaml"

testDeregisterVolume :: DeregisterVolume -> TestTree
testDeregisterVolume = req
    "DeregisterVolume"
    "fixture/DeregisterVolume.yaml"

testAssignInstance :: AssignInstance -> TestTree
testAssignInstance = req
    "AssignInstance"
    "fixture/AssignInstance.yaml"

testRebootInstance :: RebootInstance -> TestTree
testRebootInstance = req
    "RebootInstance"
    "fixture/RebootInstance.yaml"

testDescribeTimeBasedAutoScaling :: DescribeTimeBasedAutoScaling -> TestTree
testDescribeTimeBasedAutoScaling = req
    "DescribeTimeBasedAutoScaling"
    "fixture/DescribeTimeBasedAutoScaling.yaml"

testUpdateRDSDBInstance :: UpdateRDSDBInstance -> TestTree
testUpdateRDSDBInstance = req
    "UpdateRDSDBInstance"
    "fixture/UpdateRDSDBInstance.yaml"

testStopStack :: StopStack -> TestTree
testStopStack = req
    "StopStack"
    "fixture/StopStack.yaml"

testDescribeVolumes :: DescribeVolumes -> TestTree
testDescribeVolumes = req
    "DescribeVolumes"
    "fixture/DescribeVolumes.yaml"

testDisassociateElasticIP :: DisassociateElasticIP -> TestTree
testDisassociateElasticIP = req
    "DisassociateElasticIP"
    "fixture/DisassociateElasticIP.yaml"

testRegisterEcsCluster :: RegisterEcsCluster -> TestTree
testRegisterEcsCluster = req
    "RegisterEcsCluster"
    "fixture/RegisterEcsCluster.yaml"

testStopInstance :: StopInstance -> TestTree
testStopInstance = req
    "StopInstance"
    "fixture/StopInstance.yaml"

testRegisterVolume :: RegisterVolume -> TestTree
testRegisterVolume = req
    "RegisterVolume"
    "fixture/RegisterVolume.yaml"

testSetTimeBasedAutoScaling :: SetTimeBasedAutoScaling -> TestTree
testSetTimeBasedAutoScaling = req
    "SetTimeBasedAutoScaling"
    "fixture/SetTimeBasedAutoScaling.yaml"

testDeregisterElasticIP :: DeregisterElasticIP -> TestTree
testDeregisterElasticIP = req
    "DeregisterElasticIP"
    "fixture/DeregisterElasticIP.yaml"

testAttachElasticLoadBalancer :: AttachElasticLoadBalancer -> TestTree
testAttachElasticLoadBalancer = req
    "AttachElasticLoadBalancer"
    "fixture/AttachElasticLoadBalancer.yaml"

testDescribeUserProfiles :: DescribeUserProfiles -> TestTree
testDescribeUserProfiles = req
    "DescribeUserProfiles"
    "fixture/DescribeUserProfiles.yaml"

testDescribeStackSummary :: DescribeStackSummary -> TestTree
testDescribeStackSummary = req
    "DescribeStackSummary"
    "fixture/DescribeStackSummary.yaml"

testDeregisterEcsCluster :: DeregisterEcsCluster -> TestTree
testDeregisterEcsCluster = req
    "DeregisterEcsCluster"
    "fixture/DeregisterEcsCluster.yaml"

testDescribeApps :: DescribeApps -> TestTree
testDescribeApps = req
    "DescribeApps"
    "fixture/DescribeApps.yaml"

testUpdateMyUserProfile :: UpdateMyUserProfile -> TestTree
testUpdateMyUserProfile = req
    "UpdateMyUserProfile"
    "fixture/UpdateMyUserProfile.yaml"

testDescribeInstances :: DescribeInstances -> TestTree
testDescribeInstances = req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

testDescribeDeployments :: DescribeDeployments -> TestTree
testDescribeDeployments = req
    "DescribeDeployments"
    "fixture/DescribeDeployments.yaml"

testCreateStack :: CreateStack -> TestTree
testCreateStack = req
    "CreateStack"
    "fixture/CreateStack.yaml"

testGrantAccess :: GrantAccess -> TestTree
testGrantAccess = req
    "GrantAccess"
    "fixture/GrantAccess.yaml"

testDescribeElasticIPs :: DescribeElasticIPs -> TestTree
testDescribeElasticIPs = req
    "DescribeElasticIPs"
    "fixture/DescribeElasticIPs.yaml"

testDeleteLayer :: DeleteLayer -> TestTree
testDeleteLayer = req
    "DeleteLayer"
    "fixture/DeleteLayer.yaml"

testUpdateLayer :: UpdateLayer -> TestTree
testUpdateLayer = req
    "UpdateLayer"
    "fixture/UpdateLayer.yaml"

testCloneStack :: CloneStack -> TestTree
testCloneStack = req
    "CloneStack"
    "fixture/CloneStack.yaml"

testGetHostnameSuggestion :: GetHostnameSuggestion -> TestTree
testGetHostnameSuggestion = req
    "GetHostnameSuggestion"
    "fixture/GetHostnameSuggestion.yaml"

testCreateApp :: CreateApp -> TestTree
testCreateApp = req
    "CreateApp"
    "fixture/CreateApp.yaml"

testDescribePermissions :: DescribePermissions -> TestTree
testDescribePermissions = req
    "DescribePermissions"
    "fixture/DescribePermissions.yaml"

testUpdateElasticIP :: UpdateElasticIP -> TestTree
testUpdateElasticIP = req
    "UpdateElasticIP"
    "fixture/UpdateElasticIP.yaml"

testDescribeLoadBasedAutoScaling :: DescribeLoadBasedAutoScaling -> TestTree
testDescribeLoadBasedAutoScaling = req
    "DescribeLoadBasedAutoScaling"
    "fixture/DescribeLoadBasedAutoScaling.yaml"

testRegisterInstance :: RegisterInstance -> TestTree
testRegisterInstance = req
    "RegisterInstance"
    "fixture/RegisterInstance.yaml"

testAssociateElasticIP :: AssociateElasticIP -> TestTree
testAssociateElasticIP = req
    "AssociateElasticIP"
    "fixture/AssociateElasticIP.yaml"

testDetachElasticLoadBalancer :: DetachElasticLoadBalancer -> TestTree
testDetachElasticLoadBalancer = req
    "DetachElasticLoadBalancer"
    "fixture/DetachElasticLoadBalancer.yaml"

testDescribeStackProvisioningParameters :: DescribeStackProvisioningParameters -> TestTree
testDescribeStackProvisioningParameters = req
    "DescribeStackProvisioningParameters"
    "fixture/DescribeStackProvisioningParameters.yaml"

testDescribeMyUserProfile :: DescribeMyUserProfile -> TestTree
testDescribeMyUserProfile = req
    "DescribeMyUserProfile"
    "fixture/DescribeMyUserProfile.yaml"

testUnassignInstance :: UnassignInstance -> TestTree
testUnassignInstance = req
    "UnassignInstance"
    "fixture/UnassignInstance.yaml"

testRegisterRDSDBInstance :: RegisterRDSDBInstance -> TestTree
testRegisterRDSDBInstance = req
    "RegisterRDSDBInstance"
    "fixture/RegisterRDSDBInstance.yaml"

testDeleteUserProfile :: DeleteUserProfile -> TestTree
testDeleteUserProfile = req
    "DeleteUserProfile"
    "fixture/DeleteUserProfile.yaml"

testUpdateUserProfile :: UpdateUserProfile -> TestTree
testUpdateUserProfile = req
    "UpdateUserProfile"
    "fixture/UpdateUserProfile.yaml"

testDescribeServiceErrors :: DescribeServiceErrors -> TestTree
testDescribeServiceErrors = req
    "DescribeServiceErrors"
    "fixture/DescribeServiceErrors.yaml"

testStartStack :: StartStack -> TestTree
testStartStack = req
    "StartStack"
    "fixture/StartStack.yaml"

testCreateUserProfile :: CreateUserProfile -> TestTree
testCreateUserProfile = req
    "CreateUserProfile"
    "fixture/CreateUserProfile.yaml"

testDescribeCommands :: DescribeCommands -> TestTree
testDescribeCommands = req
    "DescribeCommands"
    "fixture/DescribeCommands.yaml"

testDescribeEcsClusters :: DescribeEcsClusters -> TestTree
testDescribeEcsClusters = req
    "DescribeEcsClusters"
    "fixture/DescribeEcsClusters.yaml"

testDescribeElasticLoadBalancers :: DescribeElasticLoadBalancers -> TestTree
testDescribeElasticLoadBalancers = req
    "DescribeElasticLoadBalancers"
    "fixture/DescribeElasticLoadBalancers.yaml"

testDeregisterInstance :: DeregisterInstance -> TestTree
testDeregisterInstance = req
    "DeregisterInstance"
    "fixture/DeregisterInstance.yaml"

testDescribeRAIdArrays :: DescribeRAIdArrays -> TestTree
testDescribeRAIdArrays = req
    "DescribeRAIdArrays"
    "fixture/DescribeRAIdArrays.yaml"

testSetPermission :: SetPermission -> TestTree
testSetPermission = req
    "SetPermission"
    "fixture/SetPermission.yaml"

testUpdateVolume :: UpdateVolume -> TestTree
testUpdateVolume = req
    "UpdateVolume"
    "fixture/UpdateVolume.yaml"

testAssignVolume :: AssignVolume -> TestTree
testAssignVolume = req
    "AssignVolume"
    "fixture/AssignVolume.yaml"

testStartInstance :: StartInstance -> TestTree
testStartInstance = req
    "StartInstance"
    "fixture/StartInstance.yaml"

-- Responses

testDescribeRDSDBInstancesResponse :: DescribeRDSDBInstancesResponse -> TestTree
testDescribeRDSDBInstancesResponse = res
    "DescribeRDSDBInstancesResponse"
    "fixture/DescribeRDSDBInstancesResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeRDSDBInstances)

testDeleteStackResponse :: DeleteStackResponse -> TestTree
testDeleteStackResponse = res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    opsWorks
    (Proxy :: Proxy DeleteStack)

testUpdateStackResponse :: UpdateStackResponse -> TestTree
testUpdateStackResponse = res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateStack)

testCreateLayerResponse :: CreateLayerResponse -> TestTree
testCreateLayerResponse = res
    "CreateLayerResponse"
    "fixture/CreateLayerResponse.proto"
    opsWorks
    (Proxy :: Proxy CreateLayer)

testSetLoadBasedAutoScalingResponse :: SetLoadBasedAutoScalingResponse -> TestTree
testSetLoadBasedAutoScalingResponse = res
    "SetLoadBasedAutoScalingResponse"
    "fixture/SetLoadBasedAutoScalingResponse.proto"
    opsWorks
    (Proxy :: Proxy SetLoadBasedAutoScaling)

testUnassignVolumeResponse :: UnassignVolumeResponse -> TestTree
testUnassignVolumeResponse = res
    "UnassignVolumeResponse"
    "fixture/UnassignVolumeResponse.proto"
    opsWorks
    (Proxy :: Proxy UnassignVolume)

testDeregisterRDSDBInstanceResponse :: DeregisterRDSDBInstanceResponse -> TestTree
testDeregisterRDSDBInstanceResponse = res
    "DeregisterRDSDBInstanceResponse"
    "fixture/DeregisterRDSDBInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy DeregisterRDSDBInstance)

testCreateInstanceResponse :: CreateInstanceResponse -> TestTree
testCreateInstanceResponse = res
    "CreateInstanceResponse"
    "fixture/CreateInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy CreateInstance)

testRegisterElasticIPResponse :: RegisterElasticIPResponse -> TestTree
testRegisterElasticIPResponse = res
    "RegisterElasticIPResponse"
    "fixture/RegisterElasticIPResponse.proto"
    opsWorks
    (Proxy :: Proxy RegisterElasticIP)

testDescribeAgentVersionsResponse :: DescribeAgentVersionsResponse -> TestTree
testDescribeAgentVersionsResponse = res
    "DescribeAgentVersionsResponse"
    "fixture/DescribeAgentVersionsResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeAgentVersions)

testDescribeLayersResponse :: DescribeLayersResponse -> TestTree
testDescribeLayersResponse = res
    "DescribeLayersResponse"
    "fixture/DescribeLayersResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeLayers)

testCreateDeploymentResponse :: CreateDeploymentResponse -> TestTree
testCreateDeploymentResponse = res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    opsWorks
    (Proxy :: Proxy CreateDeployment)

testDeleteAppResponse :: DeleteAppResponse -> TestTree
testDeleteAppResponse = res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    opsWorks
    (Proxy :: Proxy DeleteApp)

testUpdateAppResponse :: UpdateAppResponse -> TestTree
testUpdateAppResponse = res
    "UpdateAppResponse"
    "fixture/UpdateAppResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateApp)

testDeleteInstanceResponse :: DeleteInstanceResponse -> TestTree
testDeleteInstanceResponse = res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy DeleteInstance)

testUpdateInstanceResponse :: UpdateInstanceResponse -> TestTree
testUpdateInstanceResponse = res
    "UpdateInstanceResponse"
    "fixture/UpdateInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateInstance)

testDescribeStacksResponse :: DescribeStacksResponse -> TestTree
testDescribeStacksResponse = res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeStacks)

testDeregisterVolumeResponse :: DeregisterVolumeResponse -> TestTree
testDeregisterVolumeResponse = res
    "DeregisterVolumeResponse"
    "fixture/DeregisterVolumeResponse.proto"
    opsWorks
    (Proxy :: Proxy DeregisterVolume)

testAssignInstanceResponse :: AssignInstanceResponse -> TestTree
testAssignInstanceResponse = res
    "AssignInstanceResponse"
    "fixture/AssignInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy AssignInstance)

testRebootInstanceResponse :: RebootInstanceResponse -> TestTree
testRebootInstanceResponse = res
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy RebootInstance)

testDescribeTimeBasedAutoScalingResponse :: DescribeTimeBasedAutoScalingResponse -> TestTree
testDescribeTimeBasedAutoScalingResponse = res
    "DescribeTimeBasedAutoScalingResponse"
    "fixture/DescribeTimeBasedAutoScalingResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeTimeBasedAutoScaling)

testUpdateRDSDBInstanceResponse :: UpdateRDSDBInstanceResponse -> TestTree
testUpdateRDSDBInstanceResponse = res
    "UpdateRDSDBInstanceResponse"
    "fixture/UpdateRDSDBInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateRDSDBInstance)

testStopStackResponse :: StopStackResponse -> TestTree
testStopStackResponse = res
    "StopStackResponse"
    "fixture/StopStackResponse.proto"
    opsWorks
    (Proxy :: Proxy StopStack)

testDescribeVolumesResponse :: DescribeVolumesResponse -> TestTree
testDescribeVolumesResponse = res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeVolumes)

testDisassociateElasticIPResponse :: DisassociateElasticIPResponse -> TestTree
testDisassociateElasticIPResponse = res
    "DisassociateElasticIPResponse"
    "fixture/DisassociateElasticIPResponse.proto"
    opsWorks
    (Proxy :: Proxy DisassociateElasticIP)

testRegisterEcsClusterResponse :: RegisterEcsClusterResponse -> TestTree
testRegisterEcsClusterResponse = res
    "RegisterEcsClusterResponse"
    "fixture/RegisterEcsClusterResponse.proto"
    opsWorks
    (Proxy :: Proxy RegisterEcsCluster)

testStopInstanceResponse :: StopInstanceResponse -> TestTree
testStopInstanceResponse = res
    "StopInstanceResponse"
    "fixture/StopInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy StopInstance)

testRegisterVolumeResponse :: RegisterVolumeResponse -> TestTree
testRegisterVolumeResponse = res
    "RegisterVolumeResponse"
    "fixture/RegisterVolumeResponse.proto"
    opsWorks
    (Proxy :: Proxy RegisterVolume)

testSetTimeBasedAutoScalingResponse :: SetTimeBasedAutoScalingResponse -> TestTree
testSetTimeBasedAutoScalingResponse = res
    "SetTimeBasedAutoScalingResponse"
    "fixture/SetTimeBasedAutoScalingResponse.proto"
    opsWorks
    (Proxy :: Proxy SetTimeBasedAutoScaling)

testDeregisterElasticIPResponse :: DeregisterElasticIPResponse -> TestTree
testDeregisterElasticIPResponse = res
    "DeregisterElasticIPResponse"
    "fixture/DeregisterElasticIPResponse.proto"
    opsWorks
    (Proxy :: Proxy DeregisterElasticIP)

testAttachElasticLoadBalancerResponse :: AttachElasticLoadBalancerResponse -> TestTree
testAttachElasticLoadBalancerResponse = res
    "AttachElasticLoadBalancerResponse"
    "fixture/AttachElasticLoadBalancerResponse.proto"
    opsWorks
    (Proxy :: Proxy AttachElasticLoadBalancer)

testDescribeUserProfilesResponse :: DescribeUserProfilesResponse -> TestTree
testDescribeUserProfilesResponse = res
    "DescribeUserProfilesResponse"
    "fixture/DescribeUserProfilesResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeUserProfiles)

testDescribeStackSummaryResponse :: DescribeStackSummaryResponse -> TestTree
testDescribeStackSummaryResponse = res
    "DescribeStackSummaryResponse"
    "fixture/DescribeStackSummaryResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeStackSummary)

testDeregisterEcsClusterResponse :: DeregisterEcsClusterResponse -> TestTree
testDeregisterEcsClusterResponse = res
    "DeregisterEcsClusterResponse"
    "fixture/DeregisterEcsClusterResponse.proto"
    opsWorks
    (Proxy :: Proxy DeregisterEcsCluster)

testDescribeAppsResponse :: DescribeAppsResponse -> TestTree
testDescribeAppsResponse = res
    "DescribeAppsResponse"
    "fixture/DescribeAppsResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeApps)

testUpdateMyUserProfileResponse :: UpdateMyUserProfileResponse -> TestTree
testUpdateMyUserProfileResponse = res
    "UpdateMyUserProfileResponse"
    "fixture/UpdateMyUserProfileResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateMyUserProfile)

testDescribeInstancesResponse :: DescribeInstancesResponse -> TestTree
testDescribeInstancesResponse = res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeInstances)

testDescribeDeploymentsResponse :: DescribeDeploymentsResponse -> TestTree
testDescribeDeploymentsResponse = res
    "DescribeDeploymentsResponse"
    "fixture/DescribeDeploymentsResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeDeployments)

testCreateStackResponse :: CreateStackResponse -> TestTree
testCreateStackResponse = res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    opsWorks
    (Proxy :: Proxy CreateStack)

testGrantAccessResponse :: GrantAccessResponse -> TestTree
testGrantAccessResponse = res
    "GrantAccessResponse"
    "fixture/GrantAccessResponse.proto"
    opsWorks
    (Proxy :: Proxy GrantAccess)

testDescribeElasticIPsResponse :: DescribeElasticIPsResponse -> TestTree
testDescribeElasticIPsResponse = res
    "DescribeElasticIPsResponse"
    "fixture/DescribeElasticIPsResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeElasticIPs)

testDeleteLayerResponse :: DeleteLayerResponse -> TestTree
testDeleteLayerResponse = res
    "DeleteLayerResponse"
    "fixture/DeleteLayerResponse.proto"
    opsWorks
    (Proxy :: Proxy DeleteLayer)

testUpdateLayerResponse :: UpdateLayerResponse -> TestTree
testUpdateLayerResponse = res
    "UpdateLayerResponse"
    "fixture/UpdateLayerResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateLayer)

testCloneStackResponse :: CloneStackResponse -> TestTree
testCloneStackResponse = res
    "CloneStackResponse"
    "fixture/CloneStackResponse.proto"
    opsWorks
    (Proxy :: Proxy CloneStack)

testGetHostnameSuggestionResponse :: GetHostnameSuggestionResponse -> TestTree
testGetHostnameSuggestionResponse = res
    "GetHostnameSuggestionResponse"
    "fixture/GetHostnameSuggestionResponse.proto"
    opsWorks
    (Proxy :: Proxy GetHostnameSuggestion)

testCreateAppResponse :: CreateAppResponse -> TestTree
testCreateAppResponse = res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    opsWorks
    (Proxy :: Proxy CreateApp)

testDescribePermissionsResponse :: DescribePermissionsResponse -> TestTree
testDescribePermissionsResponse = res
    "DescribePermissionsResponse"
    "fixture/DescribePermissionsResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribePermissions)

testUpdateElasticIPResponse :: UpdateElasticIPResponse -> TestTree
testUpdateElasticIPResponse = res
    "UpdateElasticIPResponse"
    "fixture/UpdateElasticIPResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateElasticIP)

testDescribeLoadBasedAutoScalingResponse :: DescribeLoadBasedAutoScalingResponse -> TestTree
testDescribeLoadBasedAutoScalingResponse = res
    "DescribeLoadBasedAutoScalingResponse"
    "fixture/DescribeLoadBasedAutoScalingResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeLoadBasedAutoScaling)

testRegisterInstanceResponse :: RegisterInstanceResponse -> TestTree
testRegisterInstanceResponse = res
    "RegisterInstanceResponse"
    "fixture/RegisterInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy RegisterInstance)

testAssociateElasticIPResponse :: AssociateElasticIPResponse -> TestTree
testAssociateElasticIPResponse = res
    "AssociateElasticIPResponse"
    "fixture/AssociateElasticIPResponse.proto"
    opsWorks
    (Proxy :: Proxy AssociateElasticIP)

testDetachElasticLoadBalancerResponse :: DetachElasticLoadBalancerResponse -> TestTree
testDetachElasticLoadBalancerResponse = res
    "DetachElasticLoadBalancerResponse"
    "fixture/DetachElasticLoadBalancerResponse.proto"
    opsWorks
    (Proxy :: Proxy DetachElasticLoadBalancer)

testDescribeStackProvisioningParametersResponse :: DescribeStackProvisioningParametersResponse -> TestTree
testDescribeStackProvisioningParametersResponse = res
    "DescribeStackProvisioningParametersResponse"
    "fixture/DescribeStackProvisioningParametersResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeStackProvisioningParameters)

testDescribeMyUserProfileResponse :: DescribeMyUserProfileResponse -> TestTree
testDescribeMyUserProfileResponse = res
    "DescribeMyUserProfileResponse"
    "fixture/DescribeMyUserProfileResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeMyUserProfile)

testUnassignInstanceResponse :: UnassignInstanceResponse -> TestTree
testUnassignInstanceResponse = res
    "UnassignInstanceResponse"
    "fixture/UnassignInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy UnassignInstance)

testRegisterRDSDBInstanceResponse :: RegisterRDSDBInstanceResponse -> TestTree
testRegisterRDSDBInstanceResponse = res
    "RegisterRDSDBInstanceResponse"
    "fixture/RegisterRDSDBInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy RegisterRDSDBInstance)

testDeleteUserProfileResponse :: DeleteUserProfileResponse -> TestTree
testDeleteUserProfileResponse = res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse.proto"
    opsWorks
    (Proxy :: Proxy DeleteUserProfile)

testUpdateUserProfileResponse :: UpdateUserProfileResponse -> TestTree
testUpdateUserProfileResponse = res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateUserProfile)

testDescribeServiceErrorsResponse :: DescribeServiceErrorsResponse -> TestTree
testDescribeServiceErrorsResponse = res
    "DescribeServiceErrorsResponse"
    "fixture/DescribeServiceErrorsResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeServiceErrors)

testStartStackResponse :: StartStackResponse -> TestTree
testStartStackResponse = res
    "StartStackResponse"
    "fixture/StartStackResponse.proto"
    opsWorks
    (Proxy :: Proxy StartStack)

testCreateUserProfileResponse :: CreateUserProfileResponse -> TestTree
testCreateUserProfileResponse = res
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse.proto"
    opsWorks
    (Proxy :: Proxy CreateUserProfile)

testDescribeCommandsResponse :: DescribeCommandsResponse -> TestTree
testDescribeCommandsResponse = res
    "DescribeCommandsResponse"
    "fixture/DescribeCommandsResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeCommands)

testDescribeEcsClustersResponse :: DescribeEcsClustersResponse -> TestTree
testDescribeEcsClustersResponse = res
    "DescribeEcsClustersResponse"
    "fixture/DescribeEcsClustersResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeEcsClusters)

testDescribeElasticLoadBalancersResponse :: DescribeElasticLoadBalancersResponse -> TestTree
testDescribeElasticLoadBalancersResponse = res
    "DescribeElasticLoadBalancersResponse"
    "fixture/DescribeElasticLoadBalancersResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeElasticLoadBalancers)

testDeregisterInstanceResponse :: DeregisterInstanceResponse -> TestTree
testDeregisterInstanceResponse = res
    "DeregisterInstanceResponse"
    "fixture/DeregisterInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy DeregisterInstance)

testDescribeRAIdArraysResponse :: DescribeRAIdArraysResponse -> TestTree
testDescribeRAIdArraysResponse = res
    "DescribeRAIdArraysResponse"
    "fixture/DescribeRAIdArraysResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeRAIdArrays)

testSetPermissionResponse :: SetPermissionResponse -> TestTree
testSetPermissionResponse = res
    "SetPermissionResponse"
    "fixture/SetPermissionResponse.proto"
    opsWorks
    (Proxy :: Proxy SetPermission)

testUpdateVolumeResponse :: UpdateVolumeResponse -> TestTree
testUpdateVolumeResponse = res
    "UpdateVolumeResponse"
    "fixture/UpdateVolumeResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateVolume)

testAssignVolumeResponse :: AssignVolumeResponse -> TestTree
testAssignVolumeResponse = res
    "AssignVolumeResponse"
    "fixture/AssignVolumeResponse.proto"
    opsWorks
    (Proxy :: Proxy AssignVolume)

testStartInstanceResponse :: StartInstanceResponse -> TestTree
testStartInstanceResponse = res
    "StartInstanceResponse"
    "fixture/StartInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy StartInstance)
