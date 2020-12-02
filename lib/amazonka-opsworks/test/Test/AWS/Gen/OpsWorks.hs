{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.OpsWorks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.OpsWorks where

import Data.Proxy
import Network.AWS.OpsWorks
import Test.AWS.Fixture
import Test.AWS.OpsWorks.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeRDSDBInstances $
--             describeRDSDBInstances
--
--         , requestDeleteStack $
--             deleteStack
--
--         , requestUpdateStack $
--             updateStack
--
--         , requestCreateLayer $
--             createLayer
--
--         , requestSetLoadBasedAutoScaling $
--             setLoadBasedAutoScaling
--
--         , requestDeregisterRDSDBInstance $
--             deregisterRDSDBInstance
--
--         , requestUnassignVolume $
--             unassignVolume
--
--         , requestCreateInstance $
--             createInstance
--
--         , requestDescribeLayers $
--             describeLayers
--
--         , requestRegisterElasticIP $
--             registerElasticIP
--
--         , requestDescribeAgentVersions $
--             describeAgentVersions
--
--         , requestCreateDeployment $
--             createDeployment
--
--         , requestAssignInstance $
--             assignInstance
--
--         , requestDescribeStacks $
--             describeStacks
--
--         , requestDeleteInstance $
--             deleteInstance
--
--         , requestUpdateInstance $
--             updateInstance
--
--         , requestDeregisterVolume $
--             deregisterVolume
--
--         , requestRebootInstance $
--             rebootInstance
--
--         , requestDeleteApp $
--             deleteApp
--
--         , requestUpdateApp $
--             updateApp
--
--         , requestUpdateRDSDBInstance $
--             updateRDSDBInstance
--
--         , requestDescribeTimeBasedAutoScaling $
--             describeTimeBasedAutoScaling
--
--         , requestStopStack $
--             stopStack
--
--         , requestDescribeVolumes $
--             describeVolumes
--
--         , requestDisassociateElasticIP $
--             disassociateElasticIP
--
--         , requestRegisterEcsCluster $
--             registerEcsCluster
--
--         , requestStopInstance $
--             stopInstance
--
--         , requestRegisterVolume $
--             registerVolume
--
--         , requestSetTimeBasedAutoScaling $
--             setTimeBasedAutoScaling
--
--         , requestDescribeUserProfiles $
--             describeUserProfiles
--
--         , requestAttachElasticLoadBalancer $
--             attachElasticLoadBalancer
--
--         , requestDeregisterElasticIP $
--             deregisterElasticIP
--
--         , requestDeregisterEcsCluster $
--             deregisterEcsCluster
--
--         , requestDescribeApps $
--             describeApps
--
--         , requestUpdateMyUserProfile $
--             updateMyUserProfile
--
--         , requestDescribeStackSummary $
--             describeStackSummary
--
--         , requestDescribeInstances $
--             describeInstances
--
--         , requestDescribeDeployments $
--             describeDeployments
--
--         , requestDescribeElasticIPs $
--             describeElasticIPs
--
--         , requestGrantAccess $
--             grantAccess
--
--         , requestDeleteLayer $
--             deleteLayer
--
--         , requestUpdateLayer $
--             updateLayer
--
--         , requestCreateStack $
--             createStack
--
--         , requestUpdateElasticIP $
--             updateElasticIP
--
--         , requestCreateApp $
--             createApp
--
--         , requestGetHostnameSuggestion $
--             getHostnameSuggestion
--
--         , requestCloneStack $
--             cloneStack
--
--         , requestDescribePermissions $
--             describePermissions
--
--         , requestDetachElasticLoadBalancer $
--             detachElasticLoadBalancer
--
--         , requestRegisterInstance $
--             registerInstance
--
--         , requestAssociateElasticIP $
--             associateElasticIP
--
--         , requestDescribeLoadBasedAutoScaling $
--             describeLoadBasedAutoScaling
--
--         , requestDescribeStackProvisioningParameters $
--             describeStackProvisioningParameters
--
--         , requestTagResource $
--             tagResource
--
--         , requestListTags $
--             listTags
--
--         , requestUnassignInstance $
--             unassignInstance
--
--         , requestDescribeMyUserProfile $
--             describeMyUserProfile
--
--         , requestDeleteUserProfile $
--             deleteUserProfile
--
--         , requestUpdateUserProfile $
--             updateUserProfile
--
--         , requestDescribeServiceErrors $
--             describeServiceErrors
--
--         , requestRegisterRDSDBInstance $
--             registerRDSDBInstance
--
--         , requestUntagResource $
--             untagResource
--
--         , requestStartStack $
--             startStack
--
--         , requestCreateUserProfile $
--             createUserProfile
--
--         , requestDescribeOperatingSystems $
--             describeOperatingSystems
--
--         , requestDescribeCommands $
--             describeCommands
--
--         , requestAssignVolume $
--             assignVolume
--
--         , requestDescribeElasticLoadBalancers $
--             describeElasticLoadBalancers
--
--         , requestSetPermission $
--             setPermission
--
--         , requestDeregisterInstance $
--             deregisterInstance
--
--         , requestDescribeEcsClusters $
--             describeEcsClusters
--
--         , requestDescribeRAIdArrays $
--             describeRAIdArrays
--
--         , requestUpdateVolume $
--             updateVolume
--
--         , requestStartInstance $
--             startInstance
--
--           ]

--     , testGroup "response"
--         [ responseDescribeRDSDBInstances $
--             describeRDSDBInstancesResponse
--
--         , responseDeleteStack $
--             deleteStackResponse
--
--         , responseUpdateStack $
--             updateStackResponse
--
--         , responseCreateLayer $
--             createLayerResponse
--
--         , responseSetLoadBasedAutoScaling $
--             setLoadBasedAutoScalingResponse
--
--         , responseDeregisterRDSDBInstance $
--             deregisterRDSDBInstanceResponse
--
--         , responseUnassignVolume $
--             unassignVolumeResponse
--
--         , responseCreateInstance $
--             createInstanceResponse
--
--         , responseDescribeLayers $
--             describeLayersResponse
--
--         , responseRegisterElasticIP $
--             registerElasticIPResponse
--
--         , responseDescribeAgentVersions $
--             describeAgentVersionsResponse
--
--         , responseCreateDeployment $
--             createDeploymentResponse
--
--         , responseAssignInstance $
--             assignInstanceResponse
--
--         , responseDescribeStacks $
--             describeStacksResponse
--
--         , responseDeleteInstance $
--             deleteInstanceResponse
--
--         , responseUpdateInstance $
--             updateInstanceResponse
--
--         , responseDeregisterVolume $
--             deregisterVolumeResponse
--
--         , responseRebootInstance $
--             rebootInstanceResponse
--
--         , responseDeleteApp $
--             deleteAppResponse
--
--         , responseUpdateApp $
--             updateAppResponse
--
--         , responseUpdateRDSDBInstance $
--             updateRDSDBInstanceResponse
--
--         , responseDescribeTimeBasedAutoScaling $
--             describeTimeBasedAutoScalingResponse
--
--         , responseStopStack $
--             stopStackResponse
--
--         , responseDescribeVolumes $
--             describeVolumesResponse
--
--         , responseDisassociateElasticIP $
--             disassociateElasticIPResponse
--
--         , responseRegisterEcsCluster $
--             registerEcsClusterResponse
--
--         , responseStopInstance $
--             stopInstanceResponse
--
--         , responseRegisterVolume $
--             registerVolumeResponse
--
--         , responseSetTimeBasedAutoScaling $
--             setTimeBasedAutoScalingResponse
--
--         , responseDescribeUserProfiles $
--             describeUserProfilesResponse
--
--         , responseAttachElasticLoadBalancer $
--             attachElasticLoadBalancerResponse
--
--         , responseDeregisterElasticIP $
--             deregisterElasticIPResponse
--
--         , responseDeregisterEcsCluster $
--             deregisterEcsClusterResponse
--
--         , responseDescribeApps $
--             describeAppsResponse
--
--         , responseUpdateMyUserProfile $
--             updateMyUserProfileResponse
--
--         , responseDescribeStackSummary $
--             describeStackSummaryResponse
--
--         , responseDescribeInstances $
--             describeInstancesResponse
--
--         , responseDescribeDeployments $
--             describeDeploymentsResponse
--
--         , responseDescribeElasticIPs $
--             describeElasticIPsResponse
--
--         , responseGrantAccess $
--             grantAccessResponse
--
--         , responseDeleteLayer $
--             deleteLayerResponse
--
--         , responseUpdateLayer $
--             updateLayerResponse
--
--         , responseCreateStack $
--             createStackResponse
--
--         , responseUpdateElasticIP $
--             updateElasticIPResponse
--
--         , responseCreateApp $
--             createAppResponse
--
--         , responseGetHostnameSuggestion $
--             getHostnameSuggestionResponse
--
--         , responseCloneStack $
--             cloneStackResponse
--
--         , responseDescribePermissions $
--             describePermissionsResponse
--
--         , responseDetachElasticLoadBalancer $
--             detachElasticLoadBalancerResponse
--
--         , responseRegisterInstance $
--             registerInstanceResponse
--
--         , responseAssociateElasticIP $
--             associateElasticIPResponse
--
--         , responseDescribeLoadBasedAutoScaling $
--             describeLoadBasedAutoScalingResponse
--
--         , responseDescribeStackProvisioningParameters $
--             describeStackProvisioningParametersResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseListTags $
--             listTagsResponse
--
--         , responseUnassignInstance $
--             unassignInstanceResponse
--
--         , responseDescribeMyUserProfile $
--             describeMyUserProfileResponse
--
--         , responseDeleteUserProfile $
--             deleteUserProfileResponse
--
--         , responseUpdateUserProfile $
--             updateUserProfileResponse
--
--         , responseDescribeServiceErrors $
--             describeServiceErrorsResponse
--
--         , responseRegisterRDSDBInstance $
--             registerRDSDBInstanceResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseStartStack $
--             startStackResponse
--
--         , responseCreateUserProfile $
--             createUserProfileResponse
--
--         , responseDescribeOperatingSystems $
--             describeOperatingSystemsResponse
--
--         , responseDescribeCommands $
--             describeCommandsResponse
--
--         , responseAssignVolume $
--             assignVolumeResponse
--
--         , responseDescribeElasticLoadBalancers $
--             describeElasticLoadBalancersResponse
--
--         , responseSetPermission $
--             setPermissionResponse
--
--         , responseDeregisterInstance $
--             deregisterInstanceResponse
--
--         , responseDescribeEcsClusters $
--             describeEcsClustersResponse
--
--         , responseDescribeRAIdArrays $
--             describeRAIdArraysResponse
--
--         , responseUpdateVolume $
--             updateVolumeResponse
--
--         , responseStartInstance $
--             startInstanceResponse
--
--           ]
--     ]

-- Requests

requestDescribeRDSDBInstances :: DescribeRDSDBInstances -> TestTree
requestDescribeRDSDBInstances = req
    "DescribeRDSDBInstances"
    "fixture/DescribeRDSDBInstances.yaml"

requestDeleteStack :: DeleteStack -> TestTree
requestDeleteStack = req
    "DeleteStack"
    "fixture/DeleteStack.yaml"

requestUpdateStack :: UpdateStack -> TestTree
requestUpdateStack = req
    "UpdateStack"
    "fixture/UpdateStack.yaml"

requestCreateLayer :: CreateLayer -> TestTree
requestCreateLayer = req
    "CreateLayer"
    "fixture/CreateLayer.yaml"

requestSetLoadBasedAutoScaling :: SetLoadBasedAutoScaling -> TestTree
requestSetLoadBasedAutoScaling = req
    "SetLoadBasedAutoScaling"
    "fixture/SetLoadBasedAutoScaling.yaml"

requestDeregisterRDSDBInstance :: DeregisterRDSDBInstance -> TestTree
requestDeregisterRDSDBInstance = req
    "DeregisterRDSDBInstance"
    "fixture/DeregisterRDSDBInstance.yaml"

requestUnassignVolume :: UnassignVolume -> TestTree
requestUnassignVolume = req
    "UnassignVolume"
    "fixture/UnassignVolume.yaml"

requestCreateInstance :: CreateInstance -> TestTree
requestCreateInstance = req
    "CreateInstance"
    "fixture/CreateInstance.yaml"

requestDescribeLayers :: DescribeLayers -> TestTree
requestDescribeLayers = req
    "DescribeLayers"
    "fixture/DescribeLayers.yaml"

requestRegisterElasticIP :: RegisterElasticIP -> TestTree
requestRegisterElasticIP = req
    "RegisterElasticIP"
    "fixture/RegisterElasticIP.yaml"

requestDescribeAgentVersions :: DescribeAgentVersions -> TestTree
requestDescribeAgentVersions = req
    "DescribeAgentVersions"
    "fixture/DescribeAgentVersions.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment = req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestAssignInstance :: AssignInstance -> TestTree
requestAssignInstance = req
    "AssignInstance"
    "fixture/AssignInstance.yaml"

requestDescribeStacks :: DescribeStacks -> TestTree
requestDescribeStacks = req
    "DescribeStacks"
    "fixture/DescribeStacks.yaml"

requestDeleteInstance :: DeleteInstance -> TestTree
requestDeleteInstance = req
    "DeleteInstance"
    "fixture/DeleteInstance.yaml"

requestUpdateInstance :: UpdateInstance -> TestTree
requestUpdateInstance = req
    "UpdateInstance"
    "fixture/UpdateInstance.yaml"

requestDeregisterVolume :: DeregisterVolume -> TestTree
requestDeregisterVolume = req
    "DeregisterVolume"
    "fixture/DeregisterVolume.yaml"

requestRebootInstance :: RebootInstance -> TestTree
requestRebootInstance = req
    "RebootInstance"
    "fixture/RebootInstance.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp = req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestUpdateApp :: UpdateApp -> TestTree
requestUpdateApp = req
    "UpdateApp"
    "fixture/UpdateApp.yaml"

requestUpdateRDSDBInstance :: UpdateRDSDBInstance -> TestTree
requestUpdateRDSDBInstance = req
    "UpdateRDSDBInstance"
    "fixture/UpdateRDSDBInstance.yaml"

requestDescribeTimeBasedAutoScaling :: DescribeTimeBasedAutoScaling -> TestTree
requestDescribeTimeBasedAutoScaling = req
    "DescribeTimeBasedAutoScaling"
    "fixture/DescribeTimeBasedAutoScaling.yaml"

requestStopStack :: StopStack -> TestTree
requestStopStack = req
    "StopStack"
    "fixture/StopStack.yaml"

requestDescribeVolumes :: DescribeVolumes -> TestTree
requestDescribeVolumes = req
    "DescribeVolumes"
    "fixture/DescribeVolumes.yaml"

requestDisassociateElasticIP :: DisassociateElasticIP -> TestTree
requestDisassociateElasticIP = req
    "DisassociateElasticIP"
    "fixture/DisassociateElasticIP.yaml"

requestRegisterEcsCluster :: RegisterEcsCluster -> TestTree
requestRegisterEcsCluster = req
    "RegisterEcsCluster"
    "fixture/RegisterEcsCluster.yaml"

requestStopInstance :: StopInstance -> TestTree
requestStopInstance = req
    "StopInstance"
    "fixture/StopInstance.yaml"

requestRegisterVolume :: RegisterVolume -> TestTree
requestRegisterVolume = req
    "RegisterVolume"
    "fixture/RegisterVolume.yaml"

requestSetTimeBasedAutoScaling :: SetTimeBasedAutoScaling -> TestTree
requestSetTimeBasedAutoScaling = req
    "SetTimeBasedAutoScaling"
    "fixture/SetTimeBasedAutoScaling.yaml"

requestDescribeUserProfiles :: DescribeUserProfiles -> TestTree
requestDescribeUserProfiles = req
    "DescribeUserProfiles"
    "fixture/DescribeUserProfiles.yaml"

requestAttachElasticLoadBalancer :: AttachElasticLoadBalancer -> TestTree
requestAttachElasticLoadBalancer = req
    "AttachElasticLoadBalancer"
    "fixture/AttachElasticLoadBalancer.yaml"

requestDeregisterElasticIP :: DeregisterElasticIP -> TestTree
requestDeregisterElasticIP = req
    "DeregisterElasticIP"
    "fixture/DeregisterElasticIP.yaml"

requestDeregisterEcsCluster :: DeregisterEcsCluster -> TestTree
requestDeregisterEcsCluster = req
    "DeregisterEcsCluster"
    "fixture/DeregisterEcsCluster.yaml"

requestDescribeApps :: DescribeApps -> TestTree
requestDescribeApps = req
    "DescribeApps"
    "fixture/DescribeApps.yaml"

requestUpdateMyUserProfile :: UpdateMyUserProfile -> TestTree
requestUpdateMyUserProfile = req
    "UpdateMyUserProfile"
    "fixture/UpdateMyUserProfile.yaml"

requestDescribeStackSummary :: DescribeStackSummary -> TestTree
requestDescribeStackSummary = req
    "DescribeStackSummary"
    "fixture/DescribeStackSummary.yaml"

requestDescribeInstances :: DescribeInstances -> TestTree
requestDescribeInstances = req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

requestDescribeDeployments :: DescribeDeployments -> TestTree
requestDescribeDeployments = req
    "DescribeDeployments"
    "fixture/DescribeDeployments.yaml"

requestDescribeElasticIPs :: DescribeElasticIPs -> TestTree
requestDescribeElasticIPs = req
    "DescribeElasticIPs"
    "fixture/DescribeElasticIPs.yaml"

requestGrantAccess :: GrantAccess -> TestTree
requestGrantAccess = req
    "GrantAccess"
    "fixture/GrantAccess.yaml"

requestDeleteLayer :: DeleteLayer -> TestTree
requestDeleteLayer = req
    "DeleteLayer"
    "fixture/DeleteLayer.yaml"

requestUpdateLayer :: UpdateLayer -> TestTree
requestUpdateLayer = req
    "UpdateLayer"
    "fixture/UpdateLayer.yaml"

requestCreateStack :: CreateStack -> TestTree
requestCreateStack = req
    "CreateStack"
    "fixture/CreateStack.yaml"

requestUpdateElasticIP :: UpdateElasticIP -> TestTree
requestUpdateElasticIP = req
    "UpdateElasticIP"
    "fixture/UpdateElasticIP.yaml"

requestCreateApp :: CreateApp -> TestTree
requestCreateApp = req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestGetHostnameSuggestion :: GetHostnameSuggestion -> TestTree
requestGetHostnameSuggestion = req
    "GetHostnameSuggestion"
    "fixture/GetHostnameSuggestion.yaml"

requestCloneStack :: CloneStack -> TestTree
requestCloneStack = req
    "CloneStack"
    "fixture/CloneStack.yaml"

requestDescribePermissions :: DescribePermissions -> TestTree
requestDescribePermissions = req
    "DescribePermissions"
    "fixture/DescribePermissions.yaml"

requestDetachElasticLoadBalancer :: DetachElasticLoadBalancer -> TestTree
requestDetachElasticLoadBalancer = req
    "DetachElasticLoadBalancer"
    "fixture/DetachElasticLoadBalancer.yaml"

requestRegisterInstance :: RegisterInstance -> TestTree
requestRegisterInstance = req
    "RegisterInstance"
    "fixture/RegisterInstance.yaml"

requestAssociateElasticIP :: AssociateElasticIP -> TestTree
requestAssociateElasticIP = req
    "AssociateElasticIP"
    "fixture/AssociateElasticIP.yaml"

requestDescribeLoadBasedAutoScaling :: DescribeLoadBasedAutoScaling -> TestTree
requestDescribeLoadBasedAutoScaling = req
    "DescribeLoadBasedAutoScaling"
    "fixture/DescribeLoadBasedAutoScaling.yaml"

requestDescribeStackProvisioningParameters :: DescribeStackProvisioningParameters -> TestTree
requestDescribeStackProvisioningParameters = req
    "DescribeStackProvisioningParameters"
    "fixture/DescribeStackProvisioningParameters.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestListTags :: ListTags -> TestTree
requestListTags = req
    "ListTags"
    "fixture/ListTags.yaml"

requestUnassignInstance :: UnassignInstance -> TestTree
requestUnassignInstance = req
    "UnassignInstance"
    "fixture/UnassignInstance.yaml"

requestDescribeMyUserProfile :: DescribeMyUserProfile -> TestTree
requestDescribeMyUserProfile = req
    "DescribeMyUserProfile"
    "fixture/DescribeMyUserProfile.yaml"

requestDeleteUserProfile :: DeleteUserProfile -> TestTree
requestDeleteUserProfile = req
    "DeleteUserProfile"
    "fixture/DeleteUserProfile.yaml"

requestUpdateUserProfile :: UpdateUserProfile -> TestTree
requestUpdateUserProfile = req
    "UpdateUserProfile"
    "fixture/UpdateUserProfile.yaml"

requestDescribeServiceErrors :: DescribeServiceErrors -> TestTree
requestDescribeServiceErrors = req
    "DescribeServiceErrors"
    "fixture/DescribeServiceErrors.yaml"

requestRegisterRDSDBInstance :: RegisterRDSDBInstance -> TestTree
requestRegisterRDSDBInstance = req
    "RegisterRDSDBInstance"
    "fixture/RegisterRDSDBInstance.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestStartStack :: StartStack -> TestTree
requestStartStack = req
    "StartStack"
    "fixture/StartStack.yaml"

requestCreateUserProfile :: CreateUserProfile -> TestTree
requestCreateUserProfile = req
    "CreateUserProfile"
    "fixture/CreateUserProfile.yaml"

requestDescribeOperatingSystems :: DescribeOperatingSystems -> TestTree
requestDescribeOperatingSystems = req
    "DescribeOperatingSystems"
    "fixture/DescribeOperatingSystems.yaml"

requestDescribeCommands :: DescribeCommands -> TestTree
requestDescribeCommands = req
    "DescribeCommands"
    "fixture/DescribeCommands.yaml"

requestAssignVolume :: AssignVolume -> TestTree
requestAssignVolume = req
    "AssignVolume"
    "fixture/AssignVolume.yaml"

requestDescribeElasticLoadBalancers :: DescribeElasticLoadBalancers -> TestTree
requestDescribeElasticLoadBalancers = req
    "DescribeElasticLoadBalancers"
    "fixture/DescribeElasticLoadBalancers.yaml"

requestSetPermission :: SetPermission -> TestTree
requestSetPermission = req
    "SetPermission"
    "fixture/SetPermission.yaml"

requestDeregisterInstance :: DeregisterInstance -> TestTree
requestDeregisterInstance = req
    "DeregisterInstance"
    "fixture/DeregisterInstance.yaml"

requestDescribeEcsClusters :: DescribeEcsClusters -> TestTree
requestDescribeEcsClusters = req
    "DescribeEcsClusters"
    "fixture/DescribeEcsClusters.yaml"

requestDescribeRAIdArrays :: DescribeRAIdArrays -> TestTree
requestDescribeRAIdArrays = req
    "DescribeRAIdArrays"
    "fixture/DescribeRAIdArrays.yaml"

requestUpdateVolume :: UpdateVolume -> TestTree
requestUpdateVolume = req
    "UpdateVolume"
    "fixture/UpdateVolume.yaml"

requestStartInstance :: StartInstance -> TestTree
requestStartInstance = req
    "StartInstance"
    "fixture/StartInstance.yaml"

-- Responses

responseDescribeRDSDBInstances :: DescribeRDSDBInstancesResponse -> TestTree
responseDescribeRDSDBInstances = res
    "DescribeRDSDBInstancesResponse"
    "fixture/DescribeRDSDBInstancesResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeRDSDBInstances)

responseDeleteStack :: DeleteStackResponse -> TestTree
responseDeleteStack = res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    opsWorks
    (Proxy :: Proxy DeleteStack)

responseUpdateStack :: UpdateStackResponse -> TestTree
responseUpdateStack = res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateStack)

responseCreateLayer :: CreateLayerResponse -> TestTree
responseCreateLayer = res
    "CreateLayerResponse"
    "fixture/CreateLayerResponse.proto"
    opsWorks
    (Proxy :: Proxy CreateLayer)

responseSetLoadBasedAutoScaling :: SetLoadBasedAutoScalingResponse -> TestTree
responseSetLoadBasedAutoScaling = res
    "SetLoadBasedAutoScalingResponse"
    "fixture/SetLoadBasedAutoScalingResponse.proto"
    opsWorks
    (Proxy :: Proxy SetLoadBasedAutoScaling)

responseDeregisterRDSDBInstance :: DeregisterRDSDBInstanceResponse -> TestTree
responseDeregisterRDSDBInstance = res
    "DeregisterRDSDBInstanceResponse"
    "fixture/DeregisterRDSDBInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy DeregisterRDSDBInstance)

responseUnassignVolume :: UnassignVolumeResponse -> TestTree
responseUnassignVolume = res
    "UnassignVolumeResponse"
    "fixture/UnassignVolumeResponse.proto"
    opsWorks
    (Proxy :: Proxy UnassignVolume)

responseCreateInstance :: CreateInstanceResponse -> TestTree
responseCreateInstance = res
    "CreateInstanceResponse"
    "fixture/CreateInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy CreateInstance)

responseDescribeLayers :: DescribeLayersResponse -> TestTree
responseDescribeLayers = res
    "DescribeLayersResponse"
    "fixture/DescribeLayersResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeLayers)

responseRegisterElasticIP :: RegisterElasticIPResponse -> TestTree
responseRegisterElasticIP = res
    "RegisterElasticIPResponse"
    "fixture/RegisterElasticIPResponse.proto"
    opsWorks
    (Proxy :: Proxy RegisterElasticIP)

responseDescribeAgentVersions :: DescribeAgentVersionsResponse -> TestTree
responseDescribeAgentVersions = res
    "DescribeAgentVersionsResponse"
    "fixture/DescribeAgentVersionsResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeAgentVersions)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment = res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    opsWorks
    (Proxy :: Proxy CreateDeployment)

responseAssignInstance :: AssignInstanceResponse -> TestTree
responseAssignInstance = res
    "AssignInstanceResponse"
    "fixture/AssignInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy AssignInstance)

responseDescribeStacks :: DescribeStacksResponse -> TestTree
responseDescribeStacks = res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeStacks)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance = res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy DeleteInstance)

responseUpdateInstance :: UpdateInstanceResponse -> TestTree
responseUpdateInstance = res
    "UpdateInstanceResponse"
    "fixture/UpdateInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateInstance)

responseDeregisterVolume :: DeregisterVolumeResponse -> TestTree
responseDeregisterVolume = res
    "DeregisterVolumeResponse"
    "fixture/DeregisterVolumeResponse.proto"
    opsWorks
    (Proxy :: Proxy DeregisterVolume)

responseRebootInstance :: RebootInstanceResponse -> TestTree
responseRebootInstance = res
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy RebootInstance)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp = res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    opsWorks
    (Proxy :: Proxy DeleteApp)

responseUpdateApp :: UpdateAppResponse -> TestTree
responseUpdateApp = res
    "UpdateAppResponse"
    "fixture/UpdateAppResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateApp)

responseUpdateRDSDBInstance :: UpdateRDSDBInstanceResponse -> TestTree
responseUpdateRDSDBInstance = res
    "UpdateRDSDBInstanceResponse"
    "fixture/UpdateRDSDBInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateRDSDBInstance)

responseDescribeTimeBasedAutoScaling :: DescribeTimeBasedAutoScalingResponse -> TestTree
responseDescribeTimeBasedAutoScaling = res
    "DescribeTimeBasedAutoScalingResponse"
    "fixture/DescribeTimeBasedAutoScalingResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeTimeBasedAutoScaling)

responseStopStack :: StopStackResponse -> TestTree
responseStopStack = res
    "StopStackResponse"
    "fixture/StopStackResponse.proto"
    opsWorks
    (Proxy :: Proxy StopStack)

responseDescribeVolumes :: DescribeVolumesResponse -> TestTree
responseDescribeVolumes = res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeVolumes)

responseDisassociateElasticIP :: DisassociateElasticIPResponse -> TestTree
responseDisassociateElasticIP = res
    "DisassociateElasticIPResponse"
    "fixture/DisassociateElasticIPResponse.proto"
    opsWorks
    (Proxy :: Proxy DisassociateElasticIP)

responseRegisterEcsCluster :: RegisterEcsClusterResponse -> TestTree
responseRegisterEcsCluster = res
    "RegisterEcsClusterResponse"
    "fixture/RegisterEcsClusterResponse.proto"
    opsWorks
    (Proxy :: Proxy RegisterEcsCluster)

responseStopInstance :: StopInstanceResponse -> TestTree
responseStopInstance = res
    "StopInstanceResponse"
    "fixture/StopInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy StopInstance)

responseRegisterVolume :: RegisterVolumeResponse -> TestTree
responseRegisterVolume = res
    "RegisterVolumeResponse"
    "fixture/RegisterVolumeResponse.proto"
    opsWorks
    (Proxy :: Proxy RegisterVolume)

responseSetTimeBasedAutoScaling :: SetTimeBasedAutoScalingResponse -> TestTree
responseSetTimeBasedAutoScaling = res
    "SetTimeBasedAutoScalingResponse"
    "fixture/SetTimeBasedAutoScalingResponse.proto"
    opsWorks
    (Proxy :: Proxy SetTimeBasedAutoScaling)

responseDescribeUserProfiles :: DescribeUserProfilesResponse -> TestTree
responseDescribeUserProfiles = res
    "DescribeUserProfilesResponse"
    "fixture/DescribeUserProfilesResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeUserProfiles)

responseAttachElasticLoadBalancer :: AttachElasticLoadBalancerResponse -> TestTree
responseAttachElasticLoadBalancer = res
    "AttachElasticLoadBalancerResponse"
    "fixture/AttachElasticLoadBalancerResponse.proto"
    opsWorks
    (Proxy :: Proxy AttachElasticLoadBalancer)

responseDeregisterElasticIP :: DeregisterElasticIPResponse -> TestTree
responseDeregisterElasticIP = res
    "DeregisterElasticIPResponse"
    "fixture/DeregisterElasticIPResponse.proto"
    opsWorks
    (Proxy :: Proxy DeregisterElasticIP)

responseDeregisterEcsCluster :: DeregisterEcsClusterResponse -> TestTree
responseDeregisterEcsCluster = res
    "DeregisterEcsClusterResponse"
    "fixture/DeregisterEcsClusterResponse.proto"
    opsWorks
    (Proxy :: Proxy DeregisterEcsCluster)

responseDescribeApps :: DescribeAppsResponse -> TestTree
responseDescribeApps = res
    "DescribeAppsResponse"
    "fixture/DescribeAppsResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeApps)

responseUpdateMyUserProfile :: UpdateMyUserProfileResponse -> TestTree
responseUpdateMyUserProfile = res
    "UpdateMyUserProfileResponse"
    "fixture/UpdateMyUserProfileResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateMyUserProfile)

responseDescribeStackSummary :: DescribeStackSummaryResponse -> TestTree
responseDescribeStackSummary = res
    "DescribeStackSummaryResponse"
    "fixture/DescribeStackSummaryResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeStackSummary)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances = res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeInstances)

responseDescribeDeployments :: DescribeDeploymentsResponse -> TestTree
responseDescribeDeployments = res
    "DescribeDeploymentsResponse"
    "fixture/DescribeDeploymentsResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeDeployments)

responseDescribeElasticIPs :: DescribeElasticIPsResponse -> TestTree
responseDescribeElasticIPs = res
    "DescribeElasticIPsResponse"
    "fixture/DescribeElasticIPsResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeElasticIPs)

responseGrantAccess :: GrantAccessResponse -> TestTree
responseGrantAccess = res
    "GrantAccessResponse"
    "fixture/GrantAccessResponse.proto"
    opsWorks
    (Proxy :: Proxy GrantAccess)

responseDeleteLayer :: DeleteLayerResponse -> TestTree
responseDeleteLayer = res
    "DeleteLayerResponse"
    "fixture/DeleteLayerResponse.proto"
    opsWorks
    (Proxy :: Proxy DeleteLayer)

responseUpdateLayer :: UpdateLayerResponse -> TestTree
responseUpdateLayer = res
    "UpdateLayerResponse"
    "fixture/UpdateLayerResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateLayer)

responseCreateStack :: CreateStackResponse -> TestTree
responseCreateStack = res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    opsWorks
    (Proxy :: Proxy CreateStack)

responseUpdateElasticIP :: UpdateElasticIPResponse -> TestTree
responseUpdateElasticIP = res
    "UpdateElasticIPResponse"
    "fixture/UpdateElasticIPResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateElasticIP)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp = res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    opsWorks
    (Proxy :: Proxy CreateApp)

responseGetHostnameSuggestion :: GetHostnameSuggestionResponse -> TestTree
responseGetHostnameSuggestion = res
    "GetHostnameSuggestionResponse"
    "fixture/GetHostnameSuggestionResponse.proto"
    opsWorks
    (Proxy :: Proxy GetHostnameSuggestion)

responseCloneStack :: CloneStackResponse -> TestTree
responseCloneStack = res
    "CloneStackResponse"
    "fixture/CloneStackResponse.proto"
    opsWorks
    (Proxy :: Proxy CloneStack)

responseDescribePermissions :: DescribePermissionsResponse -> TestTree
responseDescribePermissions = res
    "DescribePermissionsResponse"
    "fixture/DescribePermissionsResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribePermissions)

responseDetachElasticLoadBalancer :: DetachElasticLoadBalancerResponse -> TestTree
responseDetachElasticLoadBalancer = res
    "DetachElasticLoadBalancerResponse"
    "fixture/DetachElasticLoadBalancerResponse.proto"
    opsWorks
    (Proxy :: Proxy DetachElasticLoadBalancer)

responseRegisterInstance :: RegisterInstanceResponse -> TestTree
responseRegisterInstance = res
    "RegisterInstanceResponse"
    "fixture/RegisterInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy RegisterInstance)

responseAssociateElasticIP :: AssociateElasticIPResponse -> TestTree
responseAssociateElasticIP = res
    "AssociateElasticIPResponse"
    "fixture/AssociateElasticIPResponse.proto"
    opsWorks
    (Proxy :: Proxy AssociateElasticIP)

responseDescribeLoadBasedAutoScaling :: DescribeLoadBasedAutoScalingResponse -> TestTree
responseDescribeLoadBasedAutoScaling = res
    "DescribeLoadBasedAutoScalingResponse"
    "fixture/DescribeLoadBasedAutoScalingResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeLoadBasedAutoScaling)

responseDescribeStackProvisioningParameters :: DescribeStackProvisioningParametersResponse -> TestTree
responseDescribeStackProvisioningParameters = res
    "DescribeStackProvisioningParametersResponse"
    "fixture/DescribeStackProvisioningParametersResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeStackProvisioningParameters)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    opsWorks
    (Proxy :: Proxy TagResource)

responseListTags :: ListTagsResponse -> TestTree
responseListTags = res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    opsWorks
    (Proxy :: Proxy ListTags)

responseUnassignInstance :: UnassignInstanceResponse -> TestTree
responseUnassignInstance = res
    "UnassignInstanceResponse"
    "fixture/UnassignInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy UnassignInstance)

responseDescribeMyUserProfile :: DescribeMyUserProfileResponse -> TestTree
responseDescribeMyUserProfile = res
    "DescribeMyUserProfileResponse"
    "fixture/DescribeMyUserProfileResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeMyUserProfile)

responseDeleteUserProfile :: DeleteUserProfileResponse -> TestTree
responseDeleteUserProfile = res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse.proto"
    opsWorks
    (Proxy :: Proxy DeleteUserProfile)

responseUpdateUserProfile :: UpdateUserProfileResponse -> TestTree
responseUpdateUserProfile = res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateUserProfile)

responseDescribeServiceErrors :: DescribeServiceErrorsResponse -> TestTree
responseDescribeServiceErrors = res
    "DescribeServiceErrorsResponse"
    "fixture/DescribeServiceErrorsResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeServiceErrors)

responseRegisterRDSDBInstance :: RegisterRDSDBInstanceResponse -> TestTree
responseRegisterRDSDBInstance = res
    "RegisterRDSDBInstanceResponse"
    "fixture/RegisterRDSDBInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy RegisterRDSDBInstance)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    opsWorks
    (Proxy :: Proxy UntagResource)

responseStartStack :: StartStackResponse -> TestTree
responseStartStack = res
    "StartStackResponse"
    "fixture/StartStackResponse.proto"
    opsWorks
    (Proxy :: Proxy StartStack)

responseCreateUserProfile :: CreateUserProfileResponse -> TestTree
responseCreateUserProfile = res
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse.proto"
    opsWorks
    (Proxy :: Proxy CreateUserProfile)

responseDescribeOperatingSystems :: DescribeOperatingSystemsResponse -> TestTree
responseDescribeOperatingSystems = res
    "DescribeOperatingSystemsResponse"
    "fixture/DescribeOperatingSystemsResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeOperatingSystems)

responseDescribeCommands :: DescribeCommandsResponse -> TestTree
responseDescribeCommands = res
    "DescribeCommandsResponse"
    "fixture/DescribeCommandsResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeCommands)

responseAssignVolume :: AssignVolumeResponse -> TestTree
responseAssignVolume = res
    "AssignVolumeResponse"
    "fixture/AssignVolumeResponse.proto"
    opsWorks
    (Proxy :: Proxy AssignVolume)

responseDescribeElasticLoadBalancers :: DescribeElasticLoadBalancersResponse -> TestTree
responseDescribeElasticLoadBalancers = res
    "DescribeElasticLoadBalancersResponse"
    "fixture/DescribeElasticLoadBalancersResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeElasticLoadBalancers)

responseSetPermission :: SetPermissionResponse -> TestTree
responseSetPermission = res
    "SetPermissionResponse"
    "fixture/SetPermissionResponse.proto"
    opsWorks
    (Proxy :: Proxy SetPermission)

responseDeregisterInstance :: DeregisterInstanceResponse -> TestTree
responseDeregisterInstance = res
    "DeregisterInstanceResponse"
    "fixture/DeregisterInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy DeregisterInstance)

responseDescribeEcsClusters :: DescribeEcsClustersResponse -> TestTree
responseDescribeEcsClusters = res
    "DescribeEcsClustersResponse"
    "fixture/DescribeEcsClustersResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeEcsClusters)

responseDescribeRAIdArrays :: DescribeRAIdArraysResponse -> TestTree
responseDescribeRAIdArrays = res
    "DescribeRAIdArraysResponse"
    "fixture/DescribeRAIdArraysResponse.proto"
    opsWorks
    (Proxy :: Proxy DescribeRAIdArrays)

responseUpdateVolume :: UpdateVolumeResponse -> TestTree
responseUpdateVolume = res
    "UpdateVolumeResponse"
    "fixture/UpdateVolumeResponse.proto"
    opsWorks
    (Proxy :: Proxy UpdateVolume)

responseStartInstance :: StartInstanceResponse -> TestTree
responseStartInstance = res
    "StartInstanceResponse"
    "fixture/StartInstanceResponse.proto"
    opsWorks
    (Proxy :: Proxy StartInstance)
