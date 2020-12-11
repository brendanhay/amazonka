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
--         [ requestDescribeRDSDBInstances $
--             mkDescribeRDSDBInstances
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
--         , requestDeregisterRDSDBInstance $
--             mkDeregisterRDSDBInstance
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
--         , requestRegisterElasticIP $
--             mkRegisterElasticIP
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
--         , requestUpdateRDSDBInstance $
--             mkUpdateRDSDBInstance
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
--         , requestDisassociateElasticIP $
--             mkDisassociateElasticIP
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
--         , requestDeregisterElasticIP $
--             mkDeregisterElasticIP
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
--         , requestDescribeElasticIPs $
--             mkDescribeElasticIPs
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
--         , requestUpdateElasticIP $
--             mkUpdateElasticIP
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
--         , requestAssociateElasticIP $
--             mkAssociateElasticIP
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
--         , requestRegisterRDSDBInstance $
--             mkRegisterRDSDBInstance
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
--         , requestDescribeRAIDArrays $
--             mkDescribeRAIDArrays
--
--         , requestUpdateVolume $
--             mkUpdateVolume
--
--         , requestStartInstance $
--             mkStartInstance
--
--           ]

--     , testGroup "response"
--         [ responseDescribeRDSDBInstances $
--             mkDescribeRDSDBInstancesResponse
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
--         , responseDeregisterRDSDBInstance $
--             mkDeregisterRDSDBInstanceResponse
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
--         , responseRegisterElasticIP $
--             mkRegisterElasticIPResponse
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
--         , responseUpdateRDSDBInstance $
--             mkUpdateRDSDBInstanceResponse
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
--         , responseDisassociateElasticIP $
--             mkDisassociateElasticIPResponse
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
--         , responseDeregisterElasticIP $
--             mkDeregisterElasticIPResponse
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
--         , responseDescribeElasticIPs $
--             mkDescribeElasticIPsResponse
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
--         , responseUpdateElasticIP $
--             mkUpdateElasticIPResponse
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
--         , responseAssociateElasticIP $
--             mkAssociateElasticIPResponse
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
--         , responseRegisterRDSDBInstance $
--             mkRegisterRDSDBInstanceResponse
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
--         , responseDescribeRAIDArrays $
--             mkDescribeRAIdArraysResponse
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

requestDescribeRDSDBInstances :: DescribeRDSDBInstances -> TestTree
requestDescribeRDSDBInstances =
  req
    "DescribeRDSDBInstances"
    "fixture/DescribeRDSDBInstances.yaml"

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

requestDeregisterRDSDBInstance :: DeregisterRDSDBInstance -> TestTree
requestDeregisterRDSDBInstance =
  req
    "DeregisterRDSDBInstance"
    "fixture/DeregisterRDSDBInstance.yaml"

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

requestRegisterElasticIP :: RegisterElasticIP -> TestTree
requestRegisterElasticIP =
  req
    "RegisterElasticIP"
    "fixture/RegisterElasticIP.yaml"

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

requestUpdateRDSDBInstance :: UpdateRDSDBInstance -> TestTree
requestUpdateRDSDBInstance =
  req
    "UpdateRDSDBInstance"
    "fixture/UpdateRDSDBInstance.yaml"

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

requestDisassociateElasticIP :: DisassociateElasticIP -> TestTree
requestDisassociateElasticIP =
  req
    "DisassociateElasticIP"
    "fixture/DisassociateElasticIP.yaml"

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

requestDeregisterElasticIP :: DeregisterElasticIP -> TestTree
requestDeregisterElasticIP =
  req
    "DeregisterElasticIP"
    "fixture/DeregisterElasticIP.yaml"

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

requestDescribeElasticIPs :: DescribeElasticIPs -> TestTree
requestDescribeElasticIPs =
  req
    "DescribeElasticIPs"
    "fixture/DescribeElasticIPs.yaml"

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

requestUpdateElasticIP :: UpdateElasticIP -> TestTree
requestUpdateElasticIP =
  req
    "UpdateElasticIP"
    "fixture/UpdateElasticIP.yaml"

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

requestAssociateElasticIP :: AssociateElasticIP -> TestTree
requestAssociateElasticIP =
  req
    "AssociateElasticIP"
    "fixture/AssociateElasticIP.yaml"

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

requestRegisterRDSDBInstance :: RegisterRDSDBInstance -> TestTree
requestRegisterRDSDBInstance =
  req
    "RegisterRDSDBInstance"
    "fixture/RegisterRDSDBInstance.yaml"

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

requestDescribeRAIDArrays :: DescribeRAIDArrays -> TestTree
requestDescribeRAIDArrays =
  req
    "DescribeRAIDArrays"
    "fixture/DescribeRAIDArrays.yaml"

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

responseDescribeRDSDBInstances :: DescribeRDSDBInstancesResponse -> TestTree
responseDescribeRDSDBInstances =
  res
    "DescribeRDSDBInstancesResponse"
    "fixture/DescribeRDSDBInstancesResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeRDSDBInstances)

responseDeleteStack :: DeleteStackResponse -> TestTree
responseDeleteStack =
  res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    opsWorksService
    (Proxy :: Proxy DeleteStack)

responseUpdateStack :: UpdateStackResponse -> TestTree
responseUpdateStack =
  res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    opsWorksService
    (Proxy :: Proxy UpdateStack)

responseCreateLayer :: CreateLayerResponse -> TestTree
responseCreateLayer =
  res
    "CreateLayerResponse"
    "fixture/CreateLayerResponse.proto"
    opsWorksService
    (Proxy :: Proxy CreateLayer)

responseSetLoadBasedAutoScaling :: SetLoadBasedAutoScalingResponse -> TestTree
responseSetLoadBasedAutoScaling =
  res
    "SetLoadBasedAutoScalingResponse"
    "fixture/SetLoadBasedAutoScalingResponse.proto"
    opsWorksService
    (Proxy :: Proxy SetLoadBasedAutoScaling)

responseDeregisterRDSDBInstance :: DeregisterRDSDBInstanceResponse -> TestTree
responseDeregisterRDSDBInstance =
  res
    "DeregisterRDSDBInstanceResponse"
    "fixture/DeregisterRDSDBInstanceResponse.proto"
    opsWorksService
    (Proxy :: Proxy DeregisterRDSDBInstance)

responseUnassignVolume :: UnassignVolumeResponse -> TestTree
responseUnassignVolume =
  res
    "UnassignVolumeResponse"
    "fixture/UnassignVolumeResponse.proto"
    opsWorksService
    (Proxy :: Proxy UnassignVolume)

responseCreateInstance :: CreateInstanceResponse -> TestTree
responseCreateInstance =
  res
    "CreateInstanceResponse"
    "fixture/CreateInstanceResponse.proto"
    opsWorksService
    (Proxy :: Proxy CreateInstance)

responseDescribeLayers :: DescribeLayersResponse -> TestTree
responseDescribeLayers =
  res
    "DescribeLayersResponse"
    "fixture/DescribeLayersResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeLayers)

responseRegisterElasticIP :: RegisterElasticIPResponse -> TestTree
responseRegisterElasticIP =
  res
    "RegisterElasticIPResponse"
    "fixture/RegisterElasticIPResponse.proto"
    opsWorksService
    (Proxy :: Proxy RegisterElasticIP)

responseDescribeAgentVersions :: DescribeAgentVersionsResponse -> TestTree
responseDescribeAgentVersions =
  res
    "DescribeAgentVersionsResponse"
    "fixture/DescribeAgentVersionsResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeAgentVersions)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    opsWorksService
    (Proxy :: Proxy CreateDeployment)

responseAssignInstance :: AssignInstanceResponse -> TestTree
responseAssignInstance =
  res
    "AssignInstanceResponse"
    "fixture/AssignInstanceResponse.proto"
    opsWorksService
    (Proxy :: Proxy AssignInstance)

responseDescribeStacks :: DescribeStacksResponse -> TestTree
responseDescribeStacks =
  res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeStacks)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance =
  res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    opsWorksService
    (Proxy :: Proxy DeleteInstance)

responseUpdateInstance :: UpdateInstanceResponse -> TestTree
responseUpdateInstance =
  res
    "UpdateInstanceResponse"
    "fixture/UpdateInstanceResponse.proto"
    opsWorksService
    (Proxy :: Proxy UpdateInstance)

responseDeregisterVolume :: DeregisterVolumeResponse -> TestTree
responseDeregisterVolume =
  res
    "DeregisterVolumeResponse"
    "fixture/DeregisterVolumeResponse.proto"
    opsWorksService
    (Proxy :: Proxy DeregisterVolume)

responseRebootInstance :: RebootInstanceResponse -> TestTree
responseRebootInstance =
  res
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse.proto"
    opsWorksService
    (Proxy :: Proxy RebootInstance)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    opsWorksService
    (Proxy :: Proxy DeleteApp)

responseUpdateApp :: UpdateAppResponse -> TestTree
responseUpdateApp =
  res
    "UpdateAppResponse"
    "fixture/UpdateAppResponse.proto"
    opsWorksService
    (Proxy :: Proxy UpdateApp)

responseUpdateRDSDBInstance :: UpdateRDSDBInstanceResponse -> TestTree
responseUpdateRDSDBInstance =
  res
    "UpdateRDSDBInstanceResponse"
    "fixture/UpdateRDSDBInstanceResponse.proto"
    opsWorksService
    (Proxy :: Proxy UpdateRDSDBInstance)

responseDescribeTimeBasedAutoScaling :: DescribeTimeBasedAutoScalingResponse -> TestTree
responseDescribeTimeBasedAutoScaling =
  res
    "DescribeTimeBasedAutoScalingResponse"
    "fixture/DescribeTimeBasedAutoScalingResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeTimeBasedAutoScaling)

responseStopStack :: StopStackResponse -> TestTree
responseStopStack =
  res
    "StopStackResponse"
    "fixture/StopStackResponse.proto"
    opsWorksService
    (Proxy :: Proxy StopStack)

responseDescribeVolumes :: DescribeVolumesResponse -> TestTree
responseDescribeVolumes =
  res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeVolumes)

responseDisassociateElasticIP :: DisassociateElasticIPResponse -> TestTree
responseDisassociateElasticIP =
  res
    "DisassociateElasticIPResponse"
    "fixture/DisassociateElasticIPResponse.proto"
    opsWorksService
    (Proxy :: Proxy DisassociateElasticIP)

responseRegisterEcsCluster :: RegisterEcsClusterResponse -> TestTree
responseRegisterEcsCluster =
  res
    "RegisterEcsClusterResponse"
    "fixture/RegisterEcsClusterResponse.proto"
    opsWorksService
    (Proxy :: Proxy RegisterEcsCluster)

responseStopInstance :: StopInstanceResponse -> TestTree
responseStopInstance =
  res
    "StopInstanceResponse"
    "fixture/StopInstanceResponse.proto"
    opsWorksService
    (Proxy :: Proxy StopInstance)

responseRegisterVolume :: RegisterVolumeResponse -> TestTree
responseRegisterVolume =
  res
    "RegisterVolumeResponse"
    "fixture/RegisterVolumeResponse.proto"
    opsWorksService
    (Proxy :: Proxy RegisterVolume)

responseSetTimeBasedAutoScaling :: SetTimeBasedAutoScalingResponse -> TestTree
responseSetTimeBasedAutoScaling =
  res
    "SetTimeBasedAutoScalingResponse"
    "fixture/SetTimeBasedAutoScalingResponse.proto"
    opsWorksService
    (Proxy :: Proxy SetTimeBasedAutoScaling)

responseDescribeUserProfiles :: DescribeUserProfilesResponse -> TestTree
responseDescribeUserProfiles =
  res
    "DescribeUserProfilesResponse"
    "fixture/DescribeUserProfilesResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeUserProfiles)

responseAttachElasticLoadBalancer :: AttachElasticLoadBalancerResponse -> TestTree
responseAttachElasticLoadBalancer =
  res
    "AttachElasticLoadBalancerResponse"
    "fixture/AttachElasticLoadBalancerResponse.proto"
    opsWorksService
    (Proxy :: Proxy AttachElasticLoadBalancer)

responseDeregisterElasticIP :: DeregisterElasticIPResponse -> TestTree
responseDeregisterElasticIP =
  res
    "DeregisterElasticIPResponse"
    "fixture/DeregisterElasticIPResponse.proto"
    opsWorksService
    (Proxy :: Proxy DeregisterElasticIP)

responseDeregisterEcsCluster :: DeregisterEcsClusterResponse -> TestTree
responseDeregisterEcsCluster =
  res
    "DeregisterEcsClusterResponse"
    "fixture/DeregisterEcsClusterResponse.proto"
    opsWorksService
    (Proxy :: Proxy DeregisterEcsCluster)

responseDescribeApps :: DescribeAppsResponse -> TestTree
responseDescribeApps =
  res
    "DescribeAppsResponse"
    "fixture/DescribeAppsResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeApps)

responseUpdateMyUserProfile :: UpdateMyUserProfileResponse -> TestTree
responseUpdateMyUserProfile =
  res
    "UpdateMyUserProfileResponse"
    "fixture/UpdateMyUserProfileResponse.proto"
    opsWorksService
    (Proxy :: Proxy UpdateMyUserProfile)

responseDescribeStackSummary :: DescribeStackSummaryResponse -> TestTree
responseDescribeStackSummary =
  res
    "DescribeStackSummaryResponse"
    "fixture/DescribeStackSummaryResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeStackSummary)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances =
  res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeInstances)

responseDescribeDeployments :: DescribeDeploymentsResponse -> TestTree
responseDescribeDeployments =
  res
    "DescribeDeploymentsResponse"
    "fixture/DescribeDeploymentsResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeDeployments)

responseDescribeElasticIPs :: DescribeElasticIPsResponse -> TestTree
responseDescribeElasticIPs =
  res
    "DescribeElasticIPsResponse"
    "fixture/DescribeElasticIPsResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeElasticIPs)

responseGrantAccess :: GrantAccessResponse -> TestTree
responseGrantAccess =
  res
    "GrantAccessResponse"
    "fixture/GrantAccessResponse.proto"
    opsWorksService
    (Proxy :: Proxy GrantAccess)

responseDeleteLayer :: DeleteLayerResponse -> TestTree
responseDeleteLayer =
  res
    "DeleteLayerResponse"
    "fixture/DeleteLayerResponse.proto"
    opsWorksService
    (Proxy :: Proxy DeleteLayer)

responseUpdateLayer :: UpdateLayerResponse -> TestTree
responseUpdateLayer =
  res
    "UpdateLayerResponse"
    "fixture/UpdateLayerResponse.proto"
    opsWorksService
    (Proxy :: Proxy UpdateLayer)

responseCreateStack :: CreateStackResponse -> TestTree
responseCreateStack =
  res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    opsWorksService
    (Proxy :: Proxy CreateStack)

responseUpdateElasticIP :: UpdateElasticIPResponse -> TestTree
responseUpdateElasticIP =
  res
    "UpdateElasticIPResponse"
    "fixture/UpdateElasticIPResponse.proto"
    opsWorksService
    (Proxy :: Proxy UpdateElasticIP)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    opsWorksService
    (Proxy :: Proxy CreateApp)

responseGetHostnameSuggestion :: GetHostnameSuggestionResponse -> TestTree
responseGetHostnameSuggestion =
  res
    "GetHostnameSuggestionResponse"
    "fixture/GetHostnameSuggestionResponse.proto"
    opsWorksService
    (Proxy :: Proxy GetHostnameSuggestion)

responseCloneStack :: CloneStackResponse -> TestTree
responseCloneStack =
  res
    "CloneStackResponse"
    "fixture/CloneStackResponse.proto"
    opsWorksService
    (Proxy :: Proxy CloneStack)

responseDescribePermissions :: DescribePermissionsResponse -> TestTree
responseDescribePermissions =
  res
    "DescribePermissionsResponse"
    "fixture/DescribePermissionsResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribePermissions)

responseDetachElasticLoadBalancer :: DetachElasticLoadBalancerResponse -> TestTree
responseDetachElasticLoadBalancer =
  res
    "DetachElasticLoadBalancerResponse"
    "fixture/DetachElasticLoadBalancerResponse.proto"
    opsWorksService
    (Proxy :: Proxy DetachElasticLoadBalancer)

responseRegisterInstance :: RegisterInstanceResponse -> TestTree
responseRegisterInstance =
  res
    "RegisterInstanceResponse"
    "fixture/RegisterInstanceResponse.proto"
    opsWorksService
    (Proxy :: Proxy RegisterInstance)

responseAssociateElasticIP :: AssociateElasticIPResponse -> TestTree
responseAssociateElasticIP =
  res
    "AssociateElasticIPResponse"
    "fixture/AssociateElasticIPResponse.proto"
    opsWorksService
    (Proxy :: Proxy AssociateElasticIP)

responseDescribeLoadBasedAutoScaling :: DescribeLoadBasedAutoScalingResponse -> TestTree
responseDescribeLoadBasedAutoScaling =
  res
    "DescribeLoadBasedAutoScalingResponse"
    "fixture/DescribeLoadBasedAutoScalingResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeLoadBasedAutoScaling)

responseDescribeStackProvisioningParameters :: DescribeStackProvisioningParametersResponse -> TestTree
responseDescribeStackProvisioningParameters =
  res
    "DescribeStackProvisioningParametersResponse"
    "fixture/DescribeStackProvisioningParametersResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeStackProvisioningParameters)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    opsWorksService
    (Proxy :: Proxy TagResource)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    opsWorksService
    (Proxy :: Proxy ListTags)

responseUnassignInstance :: UnassignInstanceResponse -> TestTree
responseUnassignInstance =
  res
    "UnassignInstanceResponse"
    "fixture/UnassignInstanceResponse.proto"
    opsWorksService
    (Proxy :: Proxy UnassignInstance)

responseDescribeMyUserProfile :: DescribeMyUserProfileResponse -> TestTree
responseDescribeMyUserProfile =
  res
    "DescribeMyUserProfileResponse"
    "fixture/DescribeMyUserProfileResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeMyUserProfile)

responseDeleteUserProfile :: DeleteUserProfileResponse -> TestTree
responseDeleteUserProfile =
  res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse.proto"
    opsWorksService
    (Proxy :: Proxy DeleteUserProfile)

responseUpdateUserProfile :: UpdateUserProfileResponse -> TestTree
responseUpdateUserProfile =
  res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse.proto"
    opsWorksService
    (Proxy :: Proxy UpdateUserProfile)

responseDescribeServiceErrors :: DescribeServiceErrorsResponse -> TestTree
responseDescribeServiceErrors =
  res
    "DescribeServiceErrorsResponse"
    "fixture/DescribeServiceErrorsResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeServiceErrors)

responseRegisterRDSDBInstance :: RegisterRDSDBInstanceResponse -> TestTree
responseRegisterRDSDBInstance =
  res
    "RegisterRDSDBInstanceResponse"
    "fixture/RegisterRDSDBInstanceResponse.proto"
    opsWorksService
    (Proxy :: Proxy RegisterRDSDBInstance)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    opsWorksService
    (Proxy :: Proxy UntagResource)

responseStartStack :: StartStackResponse -> TestTree
responseStartStack =
  res
    "StartStackResponse"
    "fixture/StartStackResponse.proto"
    opsWorksService
    (Proxy :: Proxy StartStack)

responseCreateUserProfile :: CreateUserProfileResponse -> TestTree
responseCreateUserProfile =
  res
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse.proto"
    opsWorksService
    (Proxy :: Proxy CreateUserProfile)

responseDescribeOperatingSystems :: DescribeOperatingSystemsResponse -> TestTree
responseDescribeOperatingSystems =
  res
    "DescribeOperatingSystemsResponse"
    "fixture/DescribeOperatingSystemsResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeOperatingSystems)

responseDescribeCommands :: DescribeCommandsResponse -> TestTree
responseDescribeCommands =
  res
    "DescribeCommandsResponse"
    "fixture/DescribeCommandsResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeCommands)

responseAssignVolume :: AssignVolumeResponse -> TestTree
responseAssignVolume =
  res
    "AssignVolumeResponse"
    "fixture/AssignVolumeResponse.proto"
    opsWorksService
    (Proxy :: Proxy AssignVolume)

responseDescribeElasticLoadBalancers :: DescribeElasticLoadBalancersResponse -> TestTree
responseDescribeElasticLoadBalancers =
  res
    "DescribeElasticLoadBalancersResponse"
    "fixture/DescribeElasticLoadBalancersResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeElasticLoadBalancers)

responseSetPermission :: SetPermissionResponse -> TestTree
responseSetPermission =
  res
    "SetPermissionResponse"
    "fixture/SetPermissionResponse.proto"
    opsWorksService
    (Proxy :: Proxy SetPermission)

responseDeregisterInstance :: DeregisterInstanceResponse -> TestTree
responseDeregisterInstance =
  res
    "DeregisterInstanceResponse"
    "fixture/DeregisterInstanceResponse.proto"
    opsWorksService
    (Proxy :: Proxy DeregisterInstance)

responseDescribeEcsClusters :: DescribeEcsClustersResponse -> TestTree
responseDescribeEcsClusters =
  res
    "DescribeEcsClustersResponse"
    "fixture/DescribeEcsClustersResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeEcsClusters)

responseDescribeRAIDArrays :: DescribeRAIdArraysResponse -> TestTree
responseDescribeRAIDArrays =
  res
    "DescribeRAIDArraysResponse"
    "fixture/DescribeRAIDArraysResponse.proto"
    opsWorksService
    (Proxy :: Proxy DescribeRAIDArrays)

responseUpdateVolume :: UpdateVolumeResponse -> TestTree
responseUpdateVolume =
  res
    "UpdateVolumeResponse"
    "fixture/UpdateVolumeResponse.proto"
    opsWorksService
    (Proxy :: Proxy UpdateVolume)

responseStartInstance :: StartInstanceResponse -> TestTree
responseStartInstance =
  res
    "StartInstanceResponse"
    "fixture/StartInstanceResponse.proto"
    opsWorksService
    (Proxy :: Proxy StartInstance)
