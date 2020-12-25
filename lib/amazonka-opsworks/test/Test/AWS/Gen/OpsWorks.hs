{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.OpsWorks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestDescribeRdsDbInstances $
--             mkDescribeRdsDbInstances
--
--         , requestDeleteStack $
--             mkDeleteStack
--
--         , requestUpdateStack $
--             mkUpdateStack
--
--         , requestCreateLayer $
--             mkCreateLayer
--
--         , requestSetLoadBasedAutoScaling $
--             mkSetLoadBasedAutoScaling
--
--         , requestDeregisterRdsDbInstance $
--             mkDeregisterRdsDbInstance
--
--         , requestUnassignVolume $
--             mkUnassignVolume
--
--         , requestCreateInstance $
--             mkCreateInstance
--
--         , requestDescribeLayers $
--             mkDescribeLayers
--
--         , requestRegisterElasticIp $
--             mkRegisterElasticIp
--
--         , requestDescribeAgentVersions $
--             mkDescribeAgentVersions
--
--         , requestCreateDeployment $
--             mkCreateDeployment
--
--         , requestAssignInstance $
--             mkAssignInstance
--
--         , requestDescribeStacks $
--             mkDescribeStacks
--
--         , requestDeleteInstance $
--             mkDeleteInstance
--
--         , requestUpdateInstance $
--             mkUpdateInstance
--
--         , requestDeregisterVolume $
--             mkDeregisterVolume
--
--         , requestRebootInstance $
--             mkRebootInstance
--
--         , requestDeleteApp $
--             mkDeleteApp
--
--         , requestUpdateApp $
--             mkUpdateApp
--
--         , requestUpdateRdsDbInstance $
--             mkUpdateRdsDbInstance
--
--         , requestDescribeTimeBasedAutoScaling $
--             mkDescribeTimeBasedAutoScaling
--
--         , requestStopStack $
--             mkStopStack
--
--         , requestDescribeVolumes $
--             mkDescribeVolumes
--
--         , requestDisassociateElasticIp $
--             mkDisassociateElasticIp
--
--         , requestRegisterEcsCluster $
--             mkRegisterEcsCluster
--
--         , requestStopInstance $
--             mkStopInstance
--
--         , requestRegisterVolume $
--             mkRegisterVolume
--
--         , requestSetTimeBasedAutoScaling $
--             mkSetTimeBasedAutoScaling
--
--         , requestDescribeUserProfiles $
--             mkDescribeUserProfiles
--
--         , requestAttachElasticLoadBalancer $
--             mkAttachElasticLoadBalancer
--
--         , requestDeregisterElasticIp $
--             mkDeregisterElasticIp
--
--         , requestDeregisterEcsCluster $
--             mkDeregisterEcsCluster
--
--         , requestDescribeApps $
--             mkDescribeApps
--
--         , requestUpdateMyUserProfile $
--             mkUpdateMyUserProfile
--
--         , requestDescribeStackSummary $
--             mkDescribeStackSummary
--
--         , requestDescribeInstances $
--             mkDescribeInstances
--
--         , requestDescribeDeployments $
--             mkDescribeDeployments
--
--         , requestDescribeElasticIps $
--             mkDescribeElasticIps
--
--         , requestGrantAccess $
--             mkGrantAccess
--
--         , requestDeleteLayer $
--             mkDeleteLayer
--
--         , requestUpdateLayer $
--             mkUpdateLayer
--
--         , requestCreateStack $
--             mkCreateStack
--
--         , requestUpdateElasticIp $
--             mkUpdateElasticIp
--
--         , requestCreateApp $
--             mkCreateApp
--
--         , requestGetHostnameSuggestion $
--             mkGetHostnameSuggestion
--
--         , requestCloneStack $
--             mkCloneStack
--
--         , requestDescribePermissions $
--             mkDescribePermissions
--
--         , requestDetachElasticLoadBalancer $
--             mkDetachElasticLoadBalancer
--
--         , requestRegisterInstance $
--             mkRegisterInstance
--
--         , requestAssociateElasticIp $
--             mkAssociateElasticIp
--
--         , requestDescribeLoadBasedAutoScaling $
--             mkDescribeLoadBasedAutoScaling
--
--         , requestDescribeStackProvisioningParameters $
--             mkDescribeStackProvisioningParameters
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestListTags $
--             mkListTags
--
--         , requestUnassignInstance $
--             mkUnassignInstance
--
--         , requestDescribeMyUserProfile $
--             mkDescribeMyUserProfile
--
--         , requestDeleteUserProfile $
--             mkDeleteUserProfile
--
--         , requestUpdateUserProfile $
--             mkUpdateUserProfile
--
--         , requestDescribeServiceErrors $
--             mkDescribeServiceErrors
--
--         , requestRegisterRdsDbInstance $
--             mkRegisterRdsDbInstance
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestStartStack $
--             mkStartStack
--
--         , requestCreateUserProfile $
--             mkCreateUserProfile
--
--         , requestDescribeOperatingSystems $
--             mkDescribeOperatingSystems
--
--         , requestDescribeCommands $
--             mkDescribeCommands
--
--         , requestAssignVolume $
--             mkAssignVolume
--
--         , requestDescribeElasticLoadBalancers $
--             mkDescribeElasticLoadBalancers
--
--         , requestSetPermission $
--             mkSetPermission
--
--         , requestDeregisterInstance $
--             mkDeregisterInstance
--
--         , requestDescribeEcsClusters $
--             mkDescribeEcsClusters
--
--         , requestDescribeRaidArrays $
--             mkDescribeRaidArrays
--
--         , requestUpdateVolume $
--             mkUpdateVolume
--
--         , requestStartInstance $
--             mkStartInstance
--
--           ]

--     , testGroup "response"
--         [ responseDescribeRdsDbInstances $
--             mkDescribeRdsDbInstancesResponse
--
--         , responseDeleteStack $
--             mkDeleteStackResponse
--
--         , responseUpdateStack $
--             mkUpdateStackResponse
--
--         , responseCreateLayer $
--             mkCreateLayerResponse
--
--         , responseSetLoadBasedAutoScaling $
--             mkSetLoadBasedAutoScalingResponse
--
--         , responseDeregisterRdsDbInstance $
--             mkDeregisterRdsDbInstanceResponse
--
--         , responseUnassignVolume $
--             mkUnassignVolumeResponse
--
--         , responseCreateInstance $
--             mkCreateInstanceResponse
--
--         , responseDescribeLayers $
--             mkDescribeLayersResponse
--
--         , responseRegisterElasticIp $
--             mkRegisterElasticIpResponse
--
--         , responseDescribeAgentVersions $
--             mkDescribeAgentVersionsResponse
--
--         , responseCreateDeployment $
--             mkCreateDeploymentResponse
--
--         , responseAssignInstance $
--             mkAssignInstanceResponse
--
--         , responseDescribeStacks $
--             mkDescribeStacksResponse
--
--         , responseDeleteInstance $
--             mkDeleteInstanceResponse
--
--         , responseUpdateInstance $
--             mkUpdateInstanceResponse
--
--         , responseDeregisterVolume $
--             mkDeregisterVolumeResponse
--
--         , responseRebootInstance $
--             mkRebootInstanceResponse
--
--         , responseDeleteApp $
--             mkDeleteAppResponse
--
--         , responseUpdateApp $
--             mkUpdateAppResponse
--
--         , responseUpdateRdsDbInstance $
--             mkUpdateRdsDbInstanceResponse
--
--         , responseDescribeTimeBasedAutoScaling $
--             mkDescribeTimeBasedAutoScalingResponse
--
--         , responseStopStack $
--             mkStopStackResponse
--
--         , responseDescribeVolumes $
--             mkDescribeVolumesResponse
--
--         , responseDisassociateElasticIp $
--             mkDisassociateElasticIpResponse
--
--         , responseRegisterEcsCluster $
--             mkRegisterEcsClusterResponse
--
--         , responseStopInstance $
--             mkStopInstanceResponse
--
--         , responseRegisterVolume $
--             mkRegisterVolumeResponse
--
--         , responseSetTimeBasedAutoScaling $
--             mkSetTimeBasedAutoScalingResponse
--
--         , responseDescribeUserProfiles $
--             mkDescribeUserProfilesResponse
--
--         , responseAttachElasticLoadBalancer $
--             mkAttachElasticLoadBalancerResponse
--
--         , responseDeregisterElasticIp $
--             mkDeregisterElasticIpResponse
--
--         , responseDeregisterEcsCluster $
--             mkDeregisterEcsClusterResponse
--
--         , responseDescribeApps $
--             mkDescribeAppsResponse
--
--         , responseUpdateMyUserProfile $
--             mkUpdateMyUserProfileResponse
--
--         , responseDescribeStackSummary $
--             mkDescribeStackSummaryResponse
--
--         , responseDescribeInstances $
--             mkDescribeInstancesResponse
--
--         , responseDescribeDeployments $
--             mkDescribeDeploymentsResponse
--
--         , responseDescribeElasticIps $
--             mkDescribeElasticIpsResponse
--
--         , responseGrantAccess $
--             mkGrantAccessResponse
--
--         , responseDeleteLayer $
--             mkDeleteLayerResponse
--
--         , responseUpdateLayer $
--             mkUpdateLayerResponse
--
--         , responseCreateStack $
--             mkCreateStackResponse
--
--         , responseUpdateElasticIp $
--             mkUpdateElasticIpResponse
--
--         , responseCreateApp $
--             mkCreateAppResponse
--
--         , responseGetHostnameSuggestion $
--             mkGetHostnameSuggestionResponse
--
--         , responseCloneStack $
--             mkCloneStackResponse
--
--         , responseDescribePermissions $
--             mkDescribePermissionsResponse
--
--         , responseDetachElasticLoadBalancer $
--             mkDetachElasticLoadBalancerResponse
--
--         , responseRegisterInstance $
--             mkRegisterInstanceResponse
--
--         , responseAssociateElasticIp $
--             mkAssociateElasticIpResponse
--
--         , responseDescribeLoadBasedAutoScaling $
--             mkDescribeLoadBasedAutoScalingResponse
--
--         , responseDescribeStackProvisioningParameters $
--             mkDescribeStackProvisioningParametersResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseListTags $
--             mkListTagsResponse
--
--         , responseUnassignInstance $
--             mkUnassignInstanceResponse
--
--         , responseDescribeMyUserProfile $
--             mkDescribeMyUserProfileResponse
--
--         , responseDeleteUserProfile $
--             mkDeleteUserProfileResponse
--
--         , responseUpdateUserProfile $
--             mkUpdateUserProfileResponse
--
--         , responseDescribeServiceErrors $
--             mkDescribeServiceErrorsResponse
--
--         , responseRegisterRdsDbInstance $
--             mkRegisterRdsDbInstanceResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseStartStack $
--             mkStartStackResponse
--
--         , responseCreateUserProfile $
--             mkCreateUserProfileResponse
--
--         , responseDescribeOperatingSystems $
--             mkDescribeOperatingSystemsResponse
--
--         , responseDescribeCommands $
--             mkDescribeCommandsResponse
--
--         , responseAssignVolume $
--             mkAssignVolumeResponse
--
--         , responseDescribeElasticLoadBalancers $
--             mkDescribeElasticLoadBalancersResponse
--
--         , responseSetPermission $
--             mkSetPermissionResponse
--
--         , responseDeregisterInstance $
--             mkDeregisterInstanceResponse
--
--         , responseDescribeEcsClusters $
--             mkDescribeEcsClustersResponse
--
--         , responseDescribeRaidArrays $
--             mkDescribeRaidArraysResponse
--
--         , responseUpdateVolume $
--             mkUpdateVolumeResponse
--
--         , responseStartInstance $
--             mkStartInstanceResponse
--
--           ]
--     ]

-- Requests

requestDescribeRdsDbInstances :: DescribeRdsDbInstances -> TestTree
requestDescribeRdsDbInstances =
  req
    "DescribeRdsDbInstances"
    "fixture/DescribeRdsDbInstances.yaml"

requestDeleteStack :: DeleteStack -> TestTree
requestDeleteStack =
  req
    "DeleteStack"
    "fixture/DeleteStack.yaml"

requestUpdateStack :: UpdateStack -> TestTree
requestUpdateStack =
  req
    "UpdateStack"
    "fixture/UpdateStack.yaml"

requestCreateLayer :: CreateLayer -> TestTree
requestCreateLayer =
  req
    "CreateLayer"
    "fixture/CreateLayer.yaml"

requestSetLoadBasedAutoScaling :: SetLoadBasedAutoScaling -> TestTree
requestSetLoadBasedAutoScaling =
  req
    "SetLoadBasedAutoScaling"
    "fixture/SetLoadBasedAutoScaling.yaml"

requestDeregisterRdsDbInstance :: DeregisterRdsDbInstance -> TestTree
requestDeregisterRdsDbInstance =
  req
    "DeregisterRdsDbInstance"
    "fixture/DeregisterRdsDbInstance.yaml"

requestUnassignVolume :: UnassignVolume -> TestTree
requestUnassignVolume =
  req
    "UnassignVolume"
    "fixture/UnassignVolume.yaml"

requestCreateInstance :: CreateInstance -> TestTree
requestCreateInstance =
  req
    "CreateInstance"
    "fixture/CreateInstance.yaml"

requestDescribeLayers :: DescribeLayers -> TestTree
requestDescribeLayers =
  req
    "DescribeLayers"
    "fixture/DescribeLayers.yaml"

requestRegisterElasticIp :: RegisterElasticIp -> TestTree
requestRegisterElasticIp =
  req
    "RegisterElasticIp"
    "fixture/RegisterElasticIp.yaml"

requestDescribeAgentVersions :: DescribeAgentVersions -> TestTree
requestDescribeAgentVersions =
  req
    "DescribeAgentVersions"
    "fixture/DescribeAgentVersions.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestAssignInstance :: AssignInstance -> TestTree
requestAssignInstance =
  req
    "AssignInstance"
    "fixture/AssignInstance.yaml"

requestDescribeStacks :: DescribeStacks -> TestTree
requestDescribeStacks =
  req
    "DescribeStacks"
    "fixture/DescribeStacks.yaml"

requestDeleteInstance :: DeleteInstance -> TestTree
requestDeleteInstance =
  req
    "DeleteInstance"
    "fixture/DeleteInstance.yaml"

requestUpdateInstance :: UpdateInstance -> TestTree
requestUpdateInstance =
  req
    "UpdateInstance"
    "fixture/UpdateInstance.yaml"

requestDeregisterVolume :: DeregisterVolume -> TestTree
requestDeregisterVolume =
  req
    "DeregisterVolume"
    "fixture/DeregisterVolume.yaml"

requestRebootInstance :: RebootInstance -> TestTree
requestRebootInstance =
  req
    "RebootInstance"
    "fixture/RebootInstance.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestUpdateApp :: UpdateApp -> TestTree
requestUpdateApp =
  req
    "UpdateApp"
    "fixture/UpdateApp.yaml"

requestUpdateRdsDbInstance :: UpdateRdsDbInstance -> TestTree
requestUpdateRdsDbInstance =
  req
    "UpdateRdsDbInstance"
    "fixture/UpdateRdsDbInstance.yaml"

requestDescribeTimeBasedAutoScaling :: DescribeTimeBasedAutoScaling -> TestTree
requestDescribeTimeBasedAutoScaling =
  req
    "DescribeTimeBasedAutoScaling"
    "fixture/DescribeTimeBasedAutoScaling.yaml"

requestStopStack :: StopStack -> TestTree
requestStopStack =
  req
    "StopStack"
    "fixture/StopStack.yaml"

requestDescribeVolumes :: DescribeVolumes -> TestTree
requestDescribeVolumes =
  req
    "DescribeVolumes"
    "fixture/DescribeVolumes.yaml"

requestDisassociateElasticIp :: DisassociateElasticIp -> TestTree
requestDisassociateElasticIp =
  req
    "DisassociateElasticIp"
    "fixture/DisassociateElasticIp.yaml"

requestRegisterEcsCluster :: RegisterEcsCluster -> TestTree
requestRegisterEcsCluster =
  req
    "RegisterEcsCluster"
    "fixture/RegisterEcsCluster.yaml"

requestStopInstance :: StopInstance -> TestTree
requestStopInstance =
  req
    "StopInstance"
    "fixture/StopInstance.yaml"

requestRegisterVolume :: RegisterVolume -> TestTree
requestRegisterVolume =
  req
    "RegisterVolume"
    "fixture/RegisterVolume.yaml"

requestSetTimeBasedAutoScaling :: SetTimeBasedAutoScaling -> TestTree
requestSetTimeBasedAutoScaling =
  req
    "SetTimeBasedAutoScaling"
    "fixture/SetTimeBasedAutoScaling.yaml"

requestDescribeUserProfiles :: DescribeUserProfiles -> TestTree
requestDescribeUserProfiles =
  req
    "DescribeUserProfiles"
    "fixture/DescribeUserProfiles.yaml"

requestAttachElasticLoadBalancer :: AttachElasticLoadBalancer -> TestTree
requestAttachElasticLoadBalancer =
  req
    "AttachElasticLoadBalancer"
    "fixture/AttachElasticLoadBalancer.yaml"

requestDeregisterElasticIp :: DeregisterElasticIp -> TestTree
requestDeregisterElasticIp =
  req
    "DeregisterElasticIp"
    "fixture/DeregisterElasticIp.yaml"

requestDeregisterEcsCluster :: DeregisterEcsCluster -> TestTree
requestDeregisterEcsCluster =
  req
    "DeregisterEcsCluster"
    "fixture/DeregisterEcsCluster.yaml"

requestDescribeApps :: DescribeApps -> TestTree
requestDescribeApps =
  req
    "DescribeApps"
    "fixture/DescribeApps.yaml"

requestUpdateMyUserProfile :: UpdateMyUserProfile -> TestTree
requestUpdateMyUserProfile =
  req
    "UpdateMyUserProfile"
    "fixture/UpdateMyUserProfile.yaml"

requestDescribeStackSummary :: DescribeStackSummary -> TestTree
requestDescribeStackSummary =
  req
    "DescribeStackSummary"
    "fixture/DescribeStackSummary.yaml"

requestDescribeInstances :: DescribeInstances -> TestTree
requestDescribeInstances =
  req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

requestDescribeDeployments :: DescribeDeployments -> TestTree
requestDescribeDeployments =
  req
    "DescribeDeployments"
    "fixture/DescribeDeployments.yaml"

requestDescribeElasticIps :: DescribeElasticIps -> TestTree
requestDescribeElasticIps =
  req
    "DescribeElasticIps"
    "fixture/DescribeElasticIps.yaml"

requestGrantAccess :: GrantAccess -> TestTree
requestGrantAccess =
  req
    "GrantAccess"
    "fixture/GrantAccess.yaml"

requestDeleteLayer :: DeleteLayer -> TestTree
requestDeleteLayer =
  req
    "DeleteLayer"
    "fixture/DeleteLayer.yaml"

requestUpdateLayer :: UpdateLayer -> TestTree
requestUpdateLayer =
  req
    "UpdateLayer"
    "fixture/UpdateLayer.yaml"

requestCreateStack :: CreateStack -> TestTree
requestCreateStack =
  req
    "CreateStack"
    "fixture/CreateStack.yaml"

requestUpdateElasticIp :: UpdateElasticIp -> TestTree
requestUpdateElasticIp =
  req
    "UpdateElasticIp"
    "fixture/UpdateElasticIp.yaml"

requestCreateApp :: CreateApp -> TestTree
requestCreateApp =
  req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestGetHostnameSuggestion :: GetHostnameSuggestion -> TestTree
requestGetHostnameSuggestion =
  req
    "GetHostnameSuggestion"
    "fixture/GetHostnameSuggestion.yaml"

requestCloneStack :: CloneStack -> TestTree
requestCloneStack =
  req
    "CloneStack"
    "fixture/CloneStack.yaml"

requestDescribePermissions :: DescribePermissions -> TestTree
requestDescribePermissions =
  req
    "DescribePermissions"
    "fixture/DescribePermissions.yaml"

requestDetachElasticLoadBalancer :: DetachElasticLoadBalancer -> TestTree
requestDetachElasticLoadBalancer =
  req
    "DetachElasticLoadBalancer"
    "fixture/DetachElasticLoadBalancer.yaml"

requestRegisterInstance :: RegisterInstance -> TestTree
requestRegisterInstance =
  req
    "RegisterInstance"
    "fixture/RegisterInstance.yaml"

requestAssociateElasticIp :: AssociateElasticIp -> TestTree
requestAssociateElasticIp =
  req
    "AssociateElasticIp"
    "fixture/AssociateElasticIp.yaml"

requestDescribeLoadBasedAutoScaling :: DescribeLoadBasedAutoScaling -> TestTree
requestDescribeLoadBasedAutoScaling =
  req
    "DescribeLoadBasedAutoScaling"
    "fixture/DescribeLoadBasedAutoScaling.yaml"

requestDescribeStackProvisioningParameters :: DescribeStackProvisioningParameters -> TestTree
requestDescribeStackProvisioningParameters =
  req
    "DescribeStackProvisioningParameters"
    "fixture/DescribeStackProvisioningParameters.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestUnassignInstance :: UnassignInstance -> TestTree
requestUnassignInstance =
  req
    "UnassignInstance"
    "fixture/UnassignInstance.yaml"

requestDescribeMyUserProfile :: DescribeMyUserProfile -> TestTree
requestDescribeMyUserProfile =
  req
    "DescribeMyUserProfile"
    "fixture/DescribeMyUserProfile.yaml"

requestDeleteUserProfile :: DeleteUserProfile -> TestTree
requestDeleteUserProfile =
  req
    "DeleteUserProfile"
    "fixture/DeleteUserProfile.yaml"

requestUpdateUserProfile :: UpdateUserProfile -> TestTree
requestUpdateUserProfile =
  req
    "UpdateUserProfile"
    "fixture/UpdateUserProfile.yaml"

requestDescribeServiceErrors :: DescribeServiceErrors -> TestTree
requestDescribeServiceErrors =
  req
    "DescribeServiceErrors"
    "fixture/DescribeServiceErrors.yaml"

requestRegisterRdsDbInstance :: RegisterRdsDbInstance -> TestTree
requestRegisterRdsDbInstance =
  req
    "RegisterRdsDbInstance"
    "fixture/RegisterRdsDbInstance.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestStartStack :: StartStack -> TestTree
requestStartStack =
  req
    "StartStack"
    "fixture/StartStack.yaml"

requestCreateUserProfile :: CreateUserProfile -> TestTree
requestCreateUserProfile =
  req
    "CreateUserProfile"
    "fixture/CreateUserProfile.yaml"

requestDescribeOperatingSystems :: DescribeOperatingSystems -> TestTree
requestDescribeOperatingSystems =
  req
    "DescribeOperatingSystems"
    "fixture/DescribeOperatingSystems.yaml"

requestDescribeCommands :: DescribeCommands -> TestTree
requestDescribeCommands =
  req
    "DescribeCommands"
    "fixture/DescribeCommands.yaml"

requestAssignVolume :: AssignVolume -> TestTree
requestAssignVolume =
  req
    "AssignVolume"
    "fixture/AssignVolume.yaml"

requestDescribeElasticLoadBalancers :: DescribeElasticLoadBalancers -> TestTree
requestDescribeElasticLoadBalancers =
  req
    "DescribeElasticLoadBalancers"
    "fixture/DescribeElasticLoadBalancers.yaml"

requestSetPermission :: SetPermission -> TestTree
requestSetPermission =
  req
    "SetPermission"
    "fixture/SetPermission.yaml"

requestDeregisterInstance :: DeregisterInstance -> TestTree
requestDeregisterInstance =
  req
    "DeregisterInstance"
    "fixture/DeregisterInstance.yaml"

requestDescribeEcsClusters :: DescribeEcsClusters -> TestTree
requestDescribeEcsClusters =
  req
    "DescribeEcsClusters"
    "fixture/DescribeEcsClusters.yaml"

requestDescribeRaidArrays :: DescribeRaidArrays -> TestTree
requestDescribeRaidArrays =
  req
    "DescribeRaidArrays"
    "fixture/DescribeRaidArrays.yaml"

requestUpdateVolume :: UpdateVolume -> TestTree
requestUpdateVolume =
  req
    "UpdateVolume"
    "fixture/UpdateVolume.yaml"

requestStartInstance :: StartInstance -> TestTree
requestStartInstance =
  req
    "StartInstance"
    "fixture/StartInstance.yaml"

-- Responses

responseDescribeRdsDbInstances :: DescribeRdsDbInstancesResponse -> TestTree
responseDescribeRdsDbInstances =
  res
    "DescribeRdsDbInstancesResponse"
    "fixture/DescribeRdsDbInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeRdsDbInstances)

responseDeleteStack :: DeleteStackResponse -> TestTree
responseDeleteStack =
  res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteStack)

responseUpdateStack :: UpdateStackResponse -> TestTree
responseUpdateStack =
  res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateStack)

responseCreateLayer :: CreateLayerResponse -> TestTree
responseCreateLayer =
  res
    "CreateLayerResponse"
    "fixture/CreateLayerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateLayer)

responseSetLoadBasedAutoScaling :: SetLoadBasedAutoScalingResponse -> TestTree
responseSetLoadBasedAutoScaling =
  res
    "SetLoadBasedAutoScalingResponse"
    "fixture/SetLoadBasedAutoScalingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetLoadBasedAutoScaling)

responseDeregisterRdsDbInstance :: DeregisterRdsDbInstanceResponse -> TestTree
responseDeregisterRdsDbInstance =
  res
    "DeregisterRdsDbInstanceResponse"
    "fixture/DeregisterRdsDbInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeregisterRdsDbInstance)

responseUnassignVolume :: UnassignVolumeResponse -> TestTree
responseUnassignVolume =
  res
    "UnassignVolumeResponse"
    "fixture/UnassignVolumeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UnassignVolume)

responseCreateInstance :: CreateInstanceResponse -> TestTree
responseCreateInstance =
  res
    "CreateInstanceResponse"
    "fixture/CreateInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateInstance)

responseDescribeLayers :: DescribeLayersResponse -> TestTree
responseDescribeLayers =
  res
    "DescribeLayersResponse"
    "fixture/DescribeLayersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLayers)

responseRegisterElasticIp :: RegisterElasticIpResponse -> TestTree
responseRegisterElasticIp =
  res
    "RegisterElasticIpResponse"
    "fixture/RegisterElasticIpResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterElasticIp)

responseDescribeAgentVersions :: DescribeAgentVersionsResponse -> TestTree
responseDescribeAgentVersions =
  res
    "DescribeAgentVersionsResponse"
    "fixture/DescribeAgentVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAgentVersions)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDeployment)

responseAssignInstance :: AssignInstanceResponse -> TestTree
responseAssignInstance =
  res
    "AssignInstanceResponse"
    "fixture/AssignInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssignInstance)

responseDescribeStacks :: DescribeStacksResponse -> TestTree
responseDescribeStacks =
  res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStacks)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance =
  res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteInstance)

responseUpdateInstance :: UpdateInstanceResponse -> TestTree
responseUpdateInstance =
  res
    "UpdateInstanceResponse"
    "fixture/UpdateInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateInstance)

responseDeregisterVolume :: DeregisterVolumeResponse -> TestTree
responseDeregisterVolume =
  res
    "DeregisterVolumeResponse"
    "fixture/DeregisterVolumeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeregisterVolume)

responseRebootInstance :: RebootInstanceResponse -> TestTree
responseRebootInstance =
  res
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RebootInstance)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteApp)

responseUpdateApp :: UpdateAppResponse -> TestTree
responseUpdateApp =
  res
    "UpdateAppResponse"
    "fixture/UpdateAppResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateApp)

responseUpdateRdsDbInstance :: UpdateRdsDbInstanceResponse -> TestTree
responseUpdateRdsDbInstance =
  res
    "UpdateRdsDbInstanceResponse"
    "fixture/UpdateRdsDbInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRdsDbInstance)

responseDescribeTimeBasedAutoScaling :: DescribeTimeBasedAutoScalingResponse -> TestTree
responseDescribeTimeBasedAutoScaling =
  res
    "DescribeTimeBasedAutoScalingResponse"
    "fixture/DescribeTimeBasedAutoScalingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTimeBasedAutoScaling)

responseStopStack :: StopStackResponse -> TestTree
responseStopStack =
  res
    "StopStackResponse"
    "fixture/StopStackResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopStack)

responseDescribeVolumes :: DescribeVolumesResponse -> TestTree
responseDescribeVolumes =
  res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeVolumes)

responseDisassociateElasticIp :: DisassociateElasticIpResponse -> TestTree
responseDisassociateElasticIp =
  res
    "DisassociateElasticIpResponse"
    "fixture/DisassociateElasticIpResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateElasticIp)

responseRegisterEcsCluster :: RegisterEcsClusterResponse -> TestTree
responseRegisterEcsCluster =
  res
    "RegisterEcsClusterResponse"
    "fixture/RegisterEcsClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterEcsCluster)

responseStopInstance :: StopInstanceResponse -> TestTree
responseStopInstance =
  res
    "StopInstanceResponse"
    "fixture/StopInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopInstance)

responseRegisterVolume :: RegisterVolumeResponse -> TestTree
responseRegisterVolume =
  res
    "RegisterVolumeResponse"
    "fixture/RegisterVolumeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterVolume)

responseSetTimeBasedAutoScaling :: SetTimeBasedAutoScalingResponse -> TestTree
responseSetTimeBasedAutoScaling =
  res
    "SetTimeBasedAutoScalingResponse"
    "fixture/SetTimeBasedAutoScalingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetTimeBasedAutoScaling)

responseDescribeUserProfiles :: DescribeUserProfilesResponse -> TestTree
responseDescribeUserProfiles =
  res
    "DescribeUserProfilesResponse"
    "fixture/DescribeUserProfilesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeUserProfiles)

responseAttachElasticLoadBalancer :: AttachElasticLoadBalancerResponse -> TestTree
responseAttachElasticLoadBalancer =
  res
    "AttachElasticLoadBalancerResponse"
    "fixture/AttachElasticLoadBalancerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachElasticLoadBalancer)

responseDeregisterElasticIp :: DeregisterElasticIpResponse -> TestTree
responseDeregisterElasticIp =
  res
    "DeregisterElasticIpResponse"
    "fixture/DeregisterElasticIpResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeregisterElasticIp)

responseDeregisterEcsCluster :: DeregisterEcsClusterResponse -> TestTree
responseDeregisterEcsCluster =
  res
    "DeregisterEcsClusterResponse"
    "fixture/DeregisterEcsClusterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeregisterEcsCluster)

responseDescribeApps :: DescribeAppsResponse -> TestTree
responseDescribeApps =
  res
    "DescribeAppsResponse"
    "fixture/DescribeAppsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeApps)

responseUpdateMyUserProfile :: UpdateMyUserProfileResponse -> TestTree
responseUpdateMyUserProfile =
  res
    "UpdateMyUserProfileResponse"
    "fixture/UpdateMyUserProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateMyUserProfile)

responseDescribeStackSummary :: DescribeStackSummaryResponse -> TestTree
responseDescribeStackSummary =
  res
    "DescribeStackSummaryResponse"
    "fixture/DescribeStackSummaryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStackSummary)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances =
  res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeInstances)

responseDescribeDeployments :: DescribeDeploymentsResponse -> TestTree
responseDescribeDeployments =
  res
    "DescribeDeploymentsResponse"
    "fixture/DescribeDeploymentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDeployments)

responseDescribeElasticIps :: DescribeElasticIpsResponse -> TestTree
responseDescribeElasticIps =
  res
    "DescribeElasticIpsResponse"
    "fixture/DescribeElasticIpsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeElasticIps)

responseGrantAccess :: GrantAccessResponse -> TestTree
responseGrantAccess =
  res
    "GrantAccessResponse"
    "fixture/GrantAccessResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GrantAccess)

responseDeleteLayer :: DeleteLayerResponse -> TestTree
responseDeleteLayer =
  res
    "DeleteLayerResponse"
    "fixture/DeleteLayerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLayer)

responseUpdateLayer :: UpdateLayerResponse -> TestTree
responseUpdateLayer =
  res
    "UpdateLayerResponse"
    "fixture/UpdateLayerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateLayer)

responseCreateStack :: CreateStackResponse -> TestTree
responseCreateStack =
  res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateStack)

responseUpdateElasticIp :: UpdateElasticIpResponse -> TestTree
responseUpdateElasticIp =
  res
    "UpdateElasticIpResponse"
    "fixture/UpdateElasticIpResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateElasticIp)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateApp)

responseGetHostnameSuggestion :: GetHostnameSuggestionResponse -> TestTree
responseGetHostnameSuggestion =
  res
    "GetHostnameSuggestionResponse"
    "fixture/GetHostnameSuggestionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetHostnameSuggestion)

responseCloneStack :: CloneStackResponse -> TestTree
responseCloneStack =
  res
    "CloneStackResponse"
    "fixture/CloneStackResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CloneStack)

responseDescribePermissions :: DescribePermissionsResponse -> TestTree
responseDescribePermissions =
  res
    "DescribePermissionsResponse"
    "fixture/DescribePermissionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribePermissions)

responseDetachElasticLoadBalancer :: DetachElasticLoadBalancerResponse -> TestTree
responseDetachElasticLoadBalancer =
  res
    "DetachElasticLoadBalancerResponse"
    "fixture/DetachElasticLoadBalancerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachElasticLoadBalancer)

responseRegisterInstance :: RegisterInstanceResponse -> TestTree
responseRegisterInstance =
  res
    "RegisterInstanceResponse"
    "fixture/RegisterInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterInstance)

responseAssociateElasticIp :: AssociateElasticIpResponse -> TestTree
responseAssociateElasticIp =
  res
    "AssociateElasticIpResponse"
    "fixture/AssociateElasticIpResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateElasticIp)

responseDescribeLoadBasedAutoScaling :: DescribeLoadBasedAutoScalingResponse -> TestTree
responseDescribeLoadBasedAutoScaling =
  res
    "DescribeLoadBasedAutoScalingResponse"
    "fixture/DescribeLoadBasedAutoScalingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeLoadBasedAutoScaling)

responseDescribeStackProvisioningParameters :: DescribeStackProvisioningParametersResponse -> TestTree
responseDescribeStackProvisioningParameters =
  res
    "DescribeStackProvisioningParametersResponse"
    "fixture/DescribeStackProvisioningParametersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStackProvisioningParameters)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTags)

responseUnassignInstance :: UnassignInstanceResponse -> TestTree
responseUnassignInstance =
  res
    "UnassignInstanceResponse"
    "fixture/UnassignInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UnassignInstance)

responseDescribeMyUserProfile :: DescribeMyUserProfileResponse -> TestTree
responseDescribeMyUserProfile =
  res
    "DescribeMyUserProfileResponse"
    "fixture/DescribeMyUserProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeMyUserProfile)

responseDeleteUserProfile :: DeleteUserProfileResponse -> TestTree
responseDeleteUserProfile =
  res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteUserProfile)

responseUpdateUserProfile :: UpdateUserProfileResponse -> TestTree
responseUpdateUserProfile =
  res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateUserProfile)

responseDescribeServiceErrors :: DescribeServiceErrorsResponse -> TestTree
responseDescribeServiceErrors =
  res
    "DescribeServiceErrorsResponse"
    "fixture/DescribeServiceErrorsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeServiceErrors)

responseRegisterRdsDbInstance :: RegisterRdsDbInstanceResponse -> TestTree
responseRegisterRdsDbInstance =
  res
    "RegisterRdsDbInstanceResponse"
    "fixture/RegisterRdsDbInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterRdsDbInstance)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseStartStack :: StartStackResponse -> TestTree
responseStartStack =
  res
    "StartStackResponse"
    "fixture/StartStackResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartStack)

responseCreateUserProfile :: CreateUserProfileResponse -> TestTree
responseCreateUserProfile =
  res
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateUserProfile)

responseDescribeOperatingSystems :: DescribeOperatingSystemsResponse -> TestTree
responseDescribeOperatingSystems =
  res
    "DescribeOperatingSystemsResponse"
    "fixture/DescribeOperatingSystemsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeOperatingSystems)

responseDescribeCommands :: DescribeCommandsResponse -> TestTree
responseDescribeCommands =
  res
    "DescribeCommandsResponse"
    "fixture/DescribeCommandsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCommands)

responseAssignVolume :: AssignVolumeResponse -> TestTree
responseAssignVolume =
  res
    "AssignVolumeResponse"
    "fixture/AssignVolumeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssignVolume)

responseDescribeElasticLoadBalancers :: DescribeElasticLoadBalancersResponse -> TestTree
responseDescribeElasticLoadBalancers =
  res
    "DescribeElasticLoadBalancersResponse"
    "fixture/DescribeElasticLoadBalancersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeElasticLoadBalancers)

responseSetPermission :: SetPermissionResponse -> TestTree
responseSetPermission =
  res
    "SetPermissionResponse"
    "fixture/SetPermissionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetPermission)

responseDeregisterInstance :: DeregisterInstanceResponse -> TestTree
responseDeregisterInstance =
  res
    "DeregisterInstanceResponse"
    "fixture/DeregisterInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeregisterInstance)

responseDescribeEcsClusters :: DescribeEcsClustersResponse -> TestTree
responseDescribeEcsClusters =
  res
    "DescribeEcsClustersResponse"
    "fixture/DescribeEcsClustersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEcsClusters)

responseDescribeRaidArrays :: DescribeRaidArraysResponse -> TestTree
responseDescribeRaidArrays =
  res
    "DescribeRaidArraysResponse"
    "fixture/DescribeRaidArraysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeRaidArrays)

responseUpdateVolume :: UpdateVolumeResponse -> TestTree
responseUpdateVolume =
  res
    "UpdateVolumeResponse"
    "fixture/UpdateVolumeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateVolume)

responseStartInstance :: StartInstanceResponse -> TestTree
responseStartInstance =
  res
    "StartInstanceResponse"
    "fixture/StartInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartInstance)
