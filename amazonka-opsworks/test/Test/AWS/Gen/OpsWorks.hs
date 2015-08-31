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
--         , testDeregisterRDSDBInstance $
--             deregisterRDSDBInstance
--
--         , testUnassignVolume $
--             unassignVolume
--
--         , testCreateInstance $
--             createInstance
--
--         , testDescribeLayers $
--             describeLayers
--
--         , testRegisterElasticIP $
--             registerElasticIP
--
--         , testDescribeAgentVersions $
--             describeAgentVersions
--
--         , testCreateDeployment $
--             createDeployment
--
--         , testAssignInstance $
--             assignInstance
--
--         , testDescribeStacks $
--             describeStacks
--
--         , testDeleteInstance $
--             deleteInstance
--
--         , testUpdateInstance $
--             updateInstance
--
--         , testDeregisterVolume $
--             deregisterVolume
--
--         , testRebootInstance $
--             rebootInstance
--
--         , testDeleteApp $
--             deleteApp
--
--         , testUpdateApp $
--             updateApp
--
--         , testUpdateRDSDBInstance $
--             updateRDSDBInstance
--
--         , testDescribeTimeBasedAutoScaling $
--             describeTimeBasedAutoScaling
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
--         , testDescribeUserProfiles $
--             describeUserProfiles
--
--         , testAttachElasticLoadBalancer $
--             attachElasticLoadBalancer
--
--         , testDeregisterElasticIP $
--             deregisterElasticIP
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
--         , testDescribeStackSummary $
--             describeStackSummary
--
--         , testDescribeInstances $
--             describeInstances
--
--         , testDescribeDeployments $
--             describeDeployments
--
--         , testDescribeElasticIPs $
--             describeElasticIPs
--
--         , testGrantAccess $
--             grantAccess
--
--         , testDeleteLayer $
--             deleteLayer
--
--         , testUpdateLayer $
--             updateLayer
--
--         , testCreateStack $
--             createStack
--
--         , testUpdateElasticIP $
--             updateElasticIP
--
--         , testCreateApp $
--             createApp
--
--         , testGetHostnameSuggestion $
--             getHostnameSuggestion
--
--         , testCloneStack $
--             cloneStack
--
--         , testDescribePermissions $
--             describePermissions
--
--         , testDetachElasticLoadBalancer $
--             detachElasticLoadBalancer
--
--         , testRegisterInstance $
--             registerInstance
--
--         , testAssociateElasticIP $
--             associateElasticIP
--
--         , testDescribeLoadBasedAutoScaling $
--             describeLoadBasedAutoScaling
--
--         , testDescribeStackProvisioningParameters $
--             describeStackProvisioningParameters
--
--         , testUnassignInstance $
--             unassignInstance
--
--         , testDescribeMyUserProfile $
--             describeMyUserProfile
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
--         , testRegisterRDSDBInstance $
--             registerRDSDBInstance
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
--         , testAssignVolume $
--             assignVolume
--
--         , testDescribeElasticLoadBalancers $
--             describeElasticLoadBalancers
--
--         , testSetPermission $
--             setPermission
--
--         , testDeregisterInstance $
--             deregisterInstance
--
--         , testDescribeEcsClusters $
--             describeEcsClusters
--
--         , testDescribeRAIdArrays $
--             describeRAIdArrays
--
--         , testUpdateVolume $
--             updateVolume
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
--         , testDeregisterRDSDBInstanceResponse $
--             deregisterRDSDBInstanceResponse
--
--         , testUnassignVolumeResponse $
--             unassignVolumeResponse
--
--         , testCreateInstanceResponse $
--             createInstanceResponse
--
--         , testDescribeLayersResponse $
--             describeLayersResponse
--
--         , testRegisterElasticIPResponse $
--             registerElasticIPResponse
--
--         , testDescribeAgentVersionsResponse $
--             describeAgentVersionsResponse
--
--         , testCreateDeploymentResponse $
--             createDeploymentResponse
--
--         , testAssignInstanceResponse $
--             assignInstanceResponse
--
--         , testDescribeStacksResponse $
--             describeStacksResponse
--
--         , testDeleteInstanceResponse $
--             deleteInstanceResponse
--
--         , testUpdateInstanceResponse $
--             updateInstanceResponse
--
--         , testDeregisterVolumeResponse $
--             deregisterVolumeResponse
--
--         , testRebootInstanceResponse $
--             rebootInstanceResponse
--
--         , testDeleteAppResponse $
--             deleteAppResponse
--
--         , testUpdateAppResponse $
--             updateAppResponse
--
--         , testUpdateRDSDBInstanceResponse $
--             updateRDSDBInstanceResponse
--
--         , testDescribeTimeBasedAutoScalingResponse $
--             describeTimeBasedAutoScalingResponse
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
--         , testDescribeUserProfilesResponse $
--             describeUserProfilesResponse
--
--         , testAttachElasticLoadBalancerResponse $
--             attachElasticLoadBalancerResponse
--
--         , testDeregisterElasticIPResponse $
--             deregisterElasticIPResponse
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
--         , testDescribeStackSummaryResponse $
--             describeStackSummaryResponse
--
--         , testDescribeInstancesResponse $
--             describeInstancesResponse
--
--         , testDescribeDeploymentsResponse $
--             describeDeploymentsResponse
--
--         , testDescribeElasticIPsResponse $
--             describeElasticIPsResponse
--
--         , testGrantAccessResponse $
--             grantAccessResponse
--
--         , testDeleteLayerResponse $
--             deleteLayerResponse
--
--         , testUpdateLayerResponse $
--             updateLayerResponse
--
--         , testCreateStackResponse $
--             createStackResponse
--
--         , testUpdateElasticIPResponse $
--             updateElasticIPResponse
--
--         , testCreateAppResponse $
--             createAppResponse
--
--         , testGetHostnameSuggestionResponse $
--             getHostnameSuggestionResponse
--
--         , testCloneStackResponse $
--             cloneStackResponse
--
--         , testDescribePermissionsResponse $
--             describePermissionsResponse
--
--         , testDetachElasticLoadBalancerResponse $
--             detachElasticLoadBalancerResponse
--
--         , testRegisterInstanceResponse $
--             registerInstanceResponse
--
--         , testAssociateElasticIPResponse $
--             associateElasticIPResponse
--
--         , testDescribeLoadBasedAutoScalingResponse $
--             describeLoadBasedAutoScalingResponse
--
--         , testDescribeStackProvisioningParametersResponse $
--             describeStackProvisioningParametersResponse
--
--         , testUnassignInstanceResponse $
--             unassignInstanceResponse
--
--         , testDescribeMyUserProfileResponse $
--             describeMyUserProfileResponse
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
--         , testRegisterRDSDBInstanceResponse $
--             registerRDSDBInstanceResponse
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
--         , testAssignVolumeResponse $
--             assignVolumeResponse
--
--         , testDescribeElasticLoadBalancersResponse $
--             describeElasticLoadBalancersResponse
--
--         , testSetPermissionResponse $
--             setPermissionResponse
--
--         , testDeregisterInstanceResponse $
--             deregisterInstanceResponse
--
--         , testDescribeEcsClustersResponse $
--             describeEcsClustersResponse
--
--         , testDescribeRAIdArraysResponse $
--             describeRAIdArraysResponse
--
--         , testUpdateVolumeResponse $
--             updateVolumeResponse
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

testDeregisterRDSDBInstance :: DeregisterRDSDBInstance -> TestTree
testDeregisterRDSDBInstance = req
    "DeregisterRDSDBInstance"
    "fixture/DeregisterRDSDBInstance.yaml"

testUnassignVolume :: UnassignVolume -> TestTree
testUnassignVolume = req
    "UnassignVolume"
    "fixture/UnassignVolume.yaml"

testCreateInstance :: CreateInstance -> TestTree
testCreateInstance = req
    "CreateInstance"
    "fixture/CreateInstance.yaml"

testDescribeLayers :: DescribeLayers -> TestTree
testDescribeLayers = req
    "DescribeLayers"
    "fixture/DescribeLayers.yaml"

testRegisterElasticIP :: RegisterElasticIP -> TestTree
testRegisterElasticIP = req
    "RegisterElasticIP"
    "fixture/RegisterElasticIP.yaml"

testDescribeAgentVersions :: DescribeAgentVersions -> TestTree
testDescribeAgentVersions = req
    "DescribeAgentVersions"
    "fixture/DescribeAgentVersions.yaml"

testCreateDeployment :: CreateDeployment -> TestTree
testCreateDeployment = req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

testAssignInstance :: AssignInstance -> TestTree
testAssignInstance = req
    "AssignInstance"
    "fixture/AssignInstance.yaml"

testDescribeStacks :: DescribeStacks -> TestTree
testDescribeStacks = req
    "DescribeStacks"
    "fixture/DescribeStacks.yaml"

testDeleteInstance :: DeleteInstance -> TestTree
testDeleteInstance = req
    "DeleteInstance"
    "fixture/DeleteInstance.yaml"

testUpdateInstance :: UpdateInstance -> TestTree
testUpdateInstance = req
    "UpdateInstance"
    "fixture/UpdateInstance.yaml"

testDeregisterVolume :: DeregisterVolume -> TestTree
testDeregisterVolume = req
    "DeregisterVolume"
    "fixture/DeregisterVolume.yaml"

testRebootInstance :: RebootInstance -> TestTree
testRebootInstance = req
    "RebootInstance"
    "fixture/RebootInstance.yaml"

testDeleteApp :: DeleteApp -> TestTree
testDeleteApp = req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

testUpdateApp :: UpdateApp -> TestTree
testUpdateApp = req
    "UpdateApp"
    "fixture/UpdateApp.yaml"

testUpdateRDSDBInstance :: UpdateRDSDBInstance -> TestTree
testUpdateRDSDBInstance = req
    "UpdateRDSDBInstance"
    "fixture/UpdateRDSDBInstance.yaml"

testDescribeTimeBasedAutoScaling :: DescribeTimeBasedAutoScaling -> TestTree
testDescribeTimeBasedAutoScaling = req
    "DescribeTimeBasedAutoScaling"
    "fixture/DescribeTimeBasedAutoScaling.yaml"

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

testDescribeUserProfiles :: DescribeUserProfiles -> TestTree
testDescribeUserProfiles = req
    "DescribeUserProfiles"
    "fixture/DescribeUserProfiles.yaml"

testAttachElasticLoadBalancer :: AttachElasticLoadBalancer -> TestTree
testAttachElasticLoadBalancer = req
    "AttachElasticLoadBalancer"
    "fixture/AttachElasticLoadBalancer.yaml"

testDeregisterElasticIP :: DeregisterElasticIP -> TestTree
testDeregisterElasticIP = req
    "DeregisterElasticIP"
    "fixture/DeregisterElasticIP.yaml"

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

testDescribeStackSummary :: DescribeStackSummary -> TestTree
testDescribeStackSummary = req
    "DescribeStackSummary"
    "fixture/DescribeStackSummary.yaml"

testDescribeInstances :: DescribeInstances -> TestTree
testDescribeInstances = req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

testDescribeDeployments :: DescribeDeployments -> TestTree
testDescribeDeployments = req
    "DescribeDeployments"
    "fixture/DescribeDeployments.yaml"

testDescribeElasticIPs :: DescribeElasticIPs -> TestTree
testDescribeElasticIPs = req
    "DescribeElasticIPs"
    "fixture/DescribeElasticIPs.yaml"

testGrantAccess :: GrantAccess -> TestTree
testGrantAccess = req
    "GrantAccess"
    "fixture/GrantAccess.yaml"

testDeleteLayer :: DeleteLayer -> TestTree
testDeleteLayer = req
    "DeleteLayer"
    "fixture/DeleteLayer.yaml"

testUpdateLayer :: UpdateLayer -> TestTree
testUpdateLayer = req
    "UpdateLayer"
    "fixture/UpdateLayer.yaml"

testCreateStack :: CreateStack -> TestTree
testCreateStack = req
    "CreateStack"
    "fixture/CreateStack.yaml"

testUpdateElasticIP :: UpdateElasticIP -> TestTree
testUpdateElasticIP = req
    "UpdateElasticIP"
    "fixture/UpdateElasticIP.yaml"

testCreateApp :: CreateApp -> TestTree
testCreateApp = req
    "CreateApp"
    "fixture/CreateApp.yaml"

testGetHostnameSuggestion :: GetHostnameSuggestion -> TestTree
testGetHostnameSuggestion = req
    "GetHostnameSuggestion"
    "fixture/GetHostnameSuggestion.yaml"

testCloneStack :: CloneStack -> TestTree
testCloneStack = req
    "CloneStack"
    "fixture/CloneStack.yaml"

testDescribePermissions :: DescribePermissions -> TestTree
testDescribePermissions = req
    "DescribePermissions"
    "fixture/DescribePermissions.yaml"

testDetachElasticLoadBalancer :: DetachElasticLoadBalancer -> TestTree
testDetachElasticLoadBalancer = req
    "DetachElasticLoadBalancer"
    "fixture/DetachElasticLoadBalancer.yaml"

testRegisterInstance :: RegisterInstance -> TestTree
testRegisterInstance = req
    "RegisterInstance"
    "fixture/RegisterInstance.yaml"

testAssociateElasticIP :: AssociateElasticIP -> TestTree
testAssociateElasticIP = req
    "AssociateElasticIP"
    "fixture/AssociateElasticIP.yaml"

testDescribeLoadBasedAutoScaling :: DescribeLoadBasedAutoScaling -> TestTree
testDescribeLoadBasedAutoScaling = req
    "DescribeLoadBasedAutoScaling"
    "fixture/DescribeLoadBasedAutoScaling.yaml"

testDescribeStackProvisioningParameters :: DescribeStackProvisioningParameters -> TestTree
testDescribeStackProvisioningParameters = req
    "DescribeStackProvisioningParameters"
    "fixture/DescribeStackProvisioningParameters.yaml"

testUnassignInstance :: UnassignInstance -> TestTree
testUnassignInstance = req
    "UnassignInstance"
    "fixture/UnassignInstance.yaml"

testDescribeMyUserProfile :: DescribeMyUserProfile -> TestTree
testDescribeMyUserProfile = req
    "DescribeMyUserProfile"
    "fixture/DescribeMyUserProfile.yaml"

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

testRegisterRDSDBInstance :: RegisterRDSDBInstance -> TestTree
testRegisterRDSDBInstance = req
    "RegisterRDSDBInstance"
    "fixture/RegisterRDSDBInstance.yaml"

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

testAssignVolume :: AssignVolume -> TestTree
testAssignVolume = req
    "AssignVolume"
    "fixture/AssignVolume.yaml"

testDescribeElasticLoadBalancers :: DescribeElasticLoadBalancers -> TestTree
testDescribeElasticLoadBalancers = req
    "DescribeElasticLoadBalancers"
    "fixture/DescribeElasticLoadBalancers.yaml"

testSetPermission :: SetPermission -> TestTree
testSetPermission = req
    "SetPermission"
    "fixture/SetPermission.yaml"

testDeregisterInstance :: DeregisterInstance -> TestTree
testDeregisterInstance = req
    "DeregisterInstance"
    "fixture/DeregisterInstance.yaml"

testDescribeEcsClusters :: DescribeEcsClusters -> TestTree
testDescribeEcsClusters = req
    "DescribeEcsClusters"
    "fixture/DescribeEcsClusters.yaml"

testDescribeRAIdArrays :: DescribeRAIdArrays -> TestTree
testDescribeRAIdArrays = req
    "DescribeRAIdArrays"
    "fixture/DescribeRAIdArrays.yaml"

testUpdateVolume :: UpdateVolume -> TestTree
testUpdateVolume = req
    "UpdateVolume"
    "fixture/UpdateVolume.yaml"

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

testDeregisterRDSDBInstanceResponse :: DeregisterRDSDBInstanceResponse -> TestTree
testDeregisterRDSDBInstanceResponse = res
    "DeregisterRDSDBInstanceResponse"
    "fixture/DeregisterRDSDBInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy DeregisterRDSDBInstance)

testUnassignVolumeResponse :: UnassignVolumeResponse -> TestTree
testUnassignVolumeResponse = res
    "UnassignVolumeResponse"
    "fixture/UnassignVolumeResponse.proto"
    opsWorks
    (Proxy :: Proxy UnassignVolume)

testCreateInstanceResponse :: CreateInstanceResponse -> TestTree
testCreateInstanceResponse = res
    "CreateInstanceResponse"
    "fixture/CreateInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy CreateInstance)

testDescribeLayersResponse :: DescribeLayersResponse -> TestTree
testDescribeLayersResponse = res
    "DescribeLayersResponse"
    "fixture/DescribeLayersResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeLayers)

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

testCreateDeploymentResponse :: CreateDeploymentResponse -> TestTree
testCreateDeploymentResponse = res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    opsWorks
    (Proxy :: Proxy CreateDeployment)

testAssignInstanceResponse :: AssignInstanceResponse -> TestTree
testAssignInstanceResponse = res
    "AssignInstanceResponse"
    "fixture/AssignInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy AssignInstance)

testDescribeStacksResponse :: DescribeStacksResponse -> TestTree
testDescribeStacksResponse = res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeStacks)

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

testDeregisterVolumeResponse :: DeregisterVolumeResponse -> TestTree
testDeregisterVolumeResponse = res
    "DeregisterVolumeResponse"
    "fixture/DeregisterVolumeResponse.proto"
    opsWorks
    (Proxy :: Proxy DeregisterVolume)

testRebootInstanceResponse :: RebootInstanceResponse -> TestTree
testRebootInstanceResponse = res
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy RebootInstance)

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

testUpdateRDSDBInstanceResponse :: UpdateRDSDBInstanceResponse -> TestTree
testUpdateRDSDBInstanceResponse = res
    "UpdateRDSDBInstanceResponse"
    "fixture/UpdateRDSDBInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateRDSDBInstance)

testDescribeTimeBasedAutoScalingResponse :: DescribeTimeBasedAutoScalingResponse -> TestTree
testDescribeTimeBasedAutoScalingResponse = res
    "DescribeTimeBasedAutoScalingResponse"
    "fixture/DescribeTimeBasedAutoScalingResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeTimeBasedAutoScaling)

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

testDescribeUserProfilesResponse :: DescribeUserProfilesResponse -> TestTree
testDescribeUserProfilesResponse = res
    "DescribeUserProfilesResponse"
    "fixture/DescribeUserProfilesResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeUserProfiles)

testAttachElasticLoadBalancerResponse :: AttachElasticLoadBalancerResponse -> TestTree
testAttachElasticLoadBalancerResponse = res
    "AttachElasticLoadBalancerResponse"
    "fixture/AttachElasticLoadBalancerResponse.proto"
    opsWorks
    (Proxy :: Proxy AttachElasticLoadBalancer)

testDeregisterElasticIPResponse :: DeregisterElasticIPResponse -> TestTree
testDeregisterElasticIPResponse = res
    "DeregisterElasticIPResponse"
    "fixture/DeregisterElasticIPResponse.proto"
    opsWorks
    (Proxy :: Proxy DeregisterElasticIP)

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

testDescribeStackSummaryResponse :: DescribeStackSummaryResponse -> TestTree
testDescribeStackSummaryResponse = res
    "DescribeStackSummaryResponse"
    "fixture/DescribeStackSummaryResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeStackSummary)

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

testDescribeElasticIPsResponse :: DescribeElasticIPsResponse -> TestTree
testDescribeElasticIPsResponse = res
    "DescribeElasticIPsResponse"
    "fixture/DescribeElasticIPsResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeElasticIPs)

testGrantAccessResponse :: GrantAccessResponse -> TestTree
testGrantAccessResponse = res
    "GrantAccessResponse"
    "fixture/GrantAccessResponse.proto"
    opsWorks
    (Proxy :: Proxy GrantAccess)

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

testCreateStackResponse :: CreateStackResponse -> TestTree
testCreateStackResponse = res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    opsWorks
    (Proxy :: Proxy CreateStack)

testUpdateElasticIPResponse :: UpdateElasticIPResponse -> TestTree
testUpdateElasticIPResponse = res
    "UpdateElasticIPResponse"
    "fixture/UpdateElasticIPResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateElasticIP)

testCreateAppResponse :: CreateAppResponse -> TestTree
testCreateAppResponse = res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    opsWorks
    (Proxy :: Proxy CreateApp)

testGetHostnameSuggestionResponse :: GetHostnameSuggestionResponse -> TestTree
testGetHostnameSuggestionResponse = res
    "GetHostnameSuggestionResponse"
    "fixture/GetHostnameSuggestionResponse.proto"
    opsWorks
    (Proxy :: Proxy GetHostnameSuggestion)

testCloneStackResponse :: CloneStackResponse -> TestTree
testCloneStackResponse = res
    "CloneStackResponse"
    "fixture/CloneStackResponse.proto"
    opsWorks
    (Proxy :: Proxy CloneStack)

testDescribePermissionsResponse :: DescribePermissionsResponse -> TestTree
testDescribePermissionsResponse = res
    "DescribePermissionsResponse"
    "fixture/DescribePermissionsResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribePermissions)

testDetachElasticLoadBalancerResponse :: DetachElasticLoadBalancerResponse -> TestTree
testDetachElasticLoadBalancerResponse = res
    "DetachElasticLoadBalancerResponse"
    "fixture/DetachElasticLoadBalancerResponse.proto"
    opsWorks
    (Proxy :: Proxy DetachElasticLoadBalancer)

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

testDescribeLoadBasedAutoScalingResponse :: DescribeLoadBasedAutoScalingResponse -> TestTree
testDescribeLoadBasedAutoScalingResponse = res
    "DescribeLoadBasedAutoScalingResponse"
    "fixture/DescribeLoadBasedAutoScalingResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeLoadBasedAutoScaling)

testDescribeStackProvisioningParametersResponse :: DescribeStackProvisioningParametersResponse -> TestTree
testDescribeStackProvisioningParametersResponse = res
    "DescribeStackProvisioningParametersResponse"
    "fixture/DescribeStackProvisioningParametersResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeStackProvisioningParameters)

testUnassignInstanceResponse :: UnassignInstanceResponse -> TestTree
testUnassignInstanceResponse = res
    "UnassignInstanceResponse"
    "fixture/UnassignInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy UnassignInstance)

testDescribeMyUserProfileResponse :: DescribeMyUserProfileResponse -> TestTree
testDescribeMyUserProfileResponse = res
    "DescribeMyUserProfileResponse"
    "fixture/DescribeMyUserProfileResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeMyUserProfile)

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

testRegisterRDSDBInstanceResponse :: RegisterRDSDBInstanceResponse -> TestTree
testRegisterRDSDBInstanceResponse = res
    "RegisterRDSDBInstanceResponse"
    "fixture/RegisterRDSDBInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy RegisterRDSDBInstance)

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

testAssignVolumeResponse :: AssignVolumeResponse -> TestTree
testAssignVolumeResponse = res
    "AssignVolumeResponse"
    "fixture/AssignVolumeResponse.proto"
    opsWorks
    (Proxy :: Proxy AssignVolume)

testDescribeElasticLoadBalancersResponse :: DescribeElasticLoadBalancersResponse -> TestTree
testDescribeElasticLoadBalancersResponse = res
    "DescribeElasticLoadBalancersResponse"
    "fixture/DescribeElasticLoadBalancersResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeElasticLoadBalancers)

testSetPermissionResponse :: SetPermissionResponse -> TestTree
testSetPermissionResponse = res
    "SetPermissionResponse"
    "fixture/SetPermissionResponse.proto"
    opsWorks
    (Proxy :: Proxy SetPermission)

testDeregisterInstanceResponse :: DeregisterInstanceResponse -> TestTree
testDeregisterInstanceResponse = res
    "DeregisterInstanceResponse"
    "fixture/DeregisterInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy DeregisterInstance)

testDescribeEcsClustersResponse :: DescribeEcsClustersResponse -> TestTree
testDescribeEcsClustersResponse = res
    "DescribeEcsClustersResponse"
    "fixture/DescribeEcsClustersResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeEcsClusters)

testDescribeRAIdArraysResponse :: DescribeRAIdArraysResponse -> TestTree
testDescribeRAIdArraysResponse = res
    "DescribeRAIdArraysResponse"
    "fixture/DescribeRAIdArraysResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeRAIdArrays)

testUpdateVolumeResponse :: UpdateVolumeResponse -> TestTree
testUpdateVolumeResponse = res
    "UpdateVolumeResponse"
    "fixture/UpdateVolumeResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateVolume)

testStartInstanceResponse :: StartInstanceResponse -> TestTree
testStartInstanceResponse = res
    "StartInstanceResponse"
    "fixture/StartInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy StartInstance)
