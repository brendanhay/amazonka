{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.OpsWorks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.OpsWorks where

import Amazonka.OpsWorks
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.OpsWorks.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeRdsDbInstances $
--             newDescribeRdsDbInstances
--
--         , requestDeleteStack $
--             newDeleteStack
--
--         , requestUpdateStack $
--             newUpdateStack
--
--         , requestCreateLayer $
--             newCreateLayer
--
--         , requestSetLoadBasedAutoScaling $
--             newSetLoadBasedAutoScaling
--
--         , requestDeregisterRdsDbInstance $
--             newDeregisterRdsDbInstance
--
--         , requestUnassignVolume $
--             newUnassignVolume
--
--         , requestCreateInstance $
--             newCreateInstance
--
--         , requestDescribeLayers $
--             newDescribeLayers
--
--         , requestRegisterElasticIp $
--             newRegisterElasticIp
--
--         , requestDescribeAgentVersions $
--             newDescribeAgentVersions
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestAssignInstance $
--             newAssignInstance
--
--         , requestDescribeStacks $
--             newDescribeStacks
--
--         , requestDeleteInstance $
--             newDeleteInstance
--
--         , requestUpdateInstance $
--             newUpdateInstance
--
--         , requestDeregisterVolume $
--             newDeregisterVolume
--
--         , requestRebootInstance $
--             newRebootInstance
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestUpdateApp $
--             newUpdateApp
--
--         , requestUpdateRdsDbInstance $
--             newUpdateRdsDbInstance
--
--         , requestDescribeTimeBasedAutoScaling $
--             newDescribeTimeBasedAutoScaling
--
--         , requestStopStack $
--             newStopStack
--
--         , requestDescribeVolumes $
--             newDescribeVolumes
--
--         , requestDisassociateElasticIp $
--             newDisassociateElasticIp
--
--         , requestRegisterEcsCluster $
--             newRegisterEcsCluster
--
--         , requestStopInstance $
--             newStopInstance
--
--         , requestRegisterVolume $
--             newRegisterVolume
--
--         , requestSetTimeBasedAutoScaling $
--             newSetTimeBasedAutoScaling
--
--         , requestDescribeUserProfiles $
--             newDescribeUserProfiles
--
--         , requestAttachElasticLoadBalancer $
--             newAttachElasticLoadBalancer
--
--         , requestDeregisterElasticIp $
--             newDeregisterElasticIp
--
--         , requestDeregisterEcsCluster $
--             newDeregisterEcsCluster
--
--         , requestDescribeApps $
--             newDescribeApps
--
--         , requestUpdateMyUserProfile $
--             newUpdateMyUserProfile
--
--         , requestDescribeStackSummary $
--             newDescribeStackSummary
--
--         , requestDescribeInstances $
--             newDescribeInstances
--
--         , requestDescribeDeployments $
--             newDescribeDeployments
--
--         , requestDescribeElasticIps $
--             newDescribeElasticIps
--
--         , requestGrantAccess $
--             newGrantAccess
--
--         , requestDeleteLayer $
--             newDeleteLayer
--
--         , requestUpdateLayer $
--             newUpdateLayer
--
--         , requestCreateStack $
--             newCreateStack
--
--         , requestUpdateElasticIp $
--             newUpdateElasticIp
--
--         , requestCreateApp $
--             newCreateApp
--
--         , requestGetHostnameSuggestion $
--             newGetHostnameSuggestion
--
--         , requestCloneStack $
--             newCloneStack
--
--         , requestDescribePermissions $
--             newDescribePermissions
--
--         , requestDetachElasticLoadBalancer $
--             newDetachElasticLoadBalancer
--
--         , requestRegisterInstance $
--             newRegisterInstance
--
--         , requestAssociateElasticIp $
--             newAssociateElasticIp
--
--         , requestDescribeLoadBasedAutoScaling $
--             newDescribeLoadBasedAutoScaling
--
--         , requestDescribeStackProvisioningParameters $
--             newDescribeStackProvisioningParameters
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListTags $
--             newListTags
--
--         , requestUnassignInstance $
--             newUnassignInstance
--
--         , requestDescribeMyUserProfile $
--             newDescribeMyUserProfile
--
--         , requestDeleteUserProfile $
--             newDeleteUserProfile
--
--         , requestUpdateUserProfile $
--             newUpdateUserProfile
--
--         , requestDescribeServiceErrors $
--             newDescribeServiceErrors
--
--         , requestRegisterRdsDbInstance $
--             newRegisterRdsDbInstance
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestStartStack $
--             newStartStack
--
--         , requestCreateUserProfile $
--             newCreateUserProfile
--
--         , requestDescribeOperatingSystems $
--             newDescribeOperatingSystems
--
--         , requestDescribeCommands $
--             newDescribeCommands
--
--         , requestAssignVolume $
--             newAssignVolume
--
--         , requestDescribeElasticLoadBalancers $
--             newDescribeElasticLoadBalancers
--
--         , requestSetPermission $
--             newSetPermission
--
--         , requestDeregisterInstance $
--             newDeregisterInstance
--
--         , requestDescribeEcsClusters $
--             newDescribeEcsClusters
--
--         , requestDescribeRaidArrays $
--             newDescribeRaidArrays
--
--         , requestUpdateVolume $
--             newUpdateVolume
--
--         , requestStartInstance $
--             newStartInstance
--
--           ]

--     , testGroup "response"
--         [ responseDescribeRdsDbInstances $
--             newDescribeRdsDbInstancesResponse
--
--         , responseDeleteStack $
--             newDeleteStackResponse
--
--         , responseUpdateStack $
--             newUpdateStackResponse
--
--         , responseCreateLayer $
--             newCreateLayerResponse
--
--         , responseSetLoadBasedAutoScaling $
--             newSetLoadBasedAutoScalingResponse
--
--         , responseDeregisterRdsDbInstance $
--             newDeregisterRdsDbInstanceResponse
--
--         , responseUnassignVolume $
--             newUnassignVolumeResponse
--
--         , responseCreateInstance $
--             newCreateInstanceResponse
--
--         , responseDescribeLayers $
--             newDescribeLayersResponse
--
--         , responseRegisterElasticIp $
--             newRegisterElasticIpResponse
--
--         , responseDescribeAgentVersions $
--             newDescribeAgentVersionsResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseAssignInstance $
--             newAssignInstanceResponse
--
--         , responseDescribeStacks $
--             newDescribeStacksResponse
--
--         , responseDeleteInstance $
--             newDeleteInstanceResponse
--
--         , responseUpdateInstance $
--             newUpdateInstanceResponse
--
--         , responseDeregisterVolume $
--             newDeregisterVolumeResponse
--
--         , responseRebootInstance $
--             newRebootInstanceResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseUpdateApp $
--             newUpdateAppResponse
--
--         , responseUpdateRdsDbInstance $
--             newUpdateRdsDbInstanceResponse
--
--         , responseDescribeTimeBasedAutoScaling $
--             newDescribeTimeBasedAutoScalingResponse
--
--         , responseStopStack $
--             newStopStackResponse
--
--         , responseDescribeVolumes $
--             newDescribeVolumesResponse
--
--         , responseDisassociateElasticIp $
--             newDisassociateElasticIpResponse
--
--         , responseRegisterEcsCluster $
--             newRegisterEcsClusterResponse
--
--         , responseStopInstance $
--             newStopInstanceResponse
--
--         , responseRegisterVolume $
--             newRegisterVolumeResponse
--
--         , responseSetTimeBasedAutoScaling $
--             newSetTimeBasedAutoScalingResponse
--
--         , responseDescribeUserProfiles $
--             newDescribeUserProfilesResponse
--
--         , responseAttachElasticLoadBalancer $
--             newAttachElasticLoadBalancerResponse
--
--         , responseDeregisterElasticIp $
--             newDeregisterElasticIpResponse
--
--         , responseDeregisterEcsCluster $
--             newDeregisterEcsClusterResponse
--
--         , responseDescribeApps $
--             newDescribeAppsResponse
--
--         , responseUpdateMyUserProfile $
--             newUpdateMyUserProfileResponse
--
--         , responseDescribeStackSummary $
--             newDescribeStackSummaryResponse
--
--         , responseDescribeInstances $
--             newDescribeInstancesResponse
--
--         , responseDescribeDeployments $
--             newDescribeDeploymentsResponse
--
--         , responseDescribeElasticIps $
--             newDescribeElasticIpsResponse
--
--         , responseGrantAccess $
--             newGrantAccessResponse
--
--         , responseDeleteLayer $
--             newDeleteLayerResponse
--
--         , responseUpdateLayer $
--             newUpdateLayerResponse
--
--         , responseCreateStack $
--             newCreateStackResponse
--
--         , responseUpdateElasticIp $
--             newUpdateElasticIpResponse
--
--         , responseCreateApp $
--             newCreateAppResponse
--
--         , responseGetHostnameSuggestion $
--             newGetHostnameSuggestionResponse
--
--         , responseCloneStack $
--             newCloneStackResponse
--
--         , responseDescribePermissions $
--             newDescribePermissionsResponse
--
--         , responseDetachElasticLoadBalancer $
--             newDetachElasticLoadBalancerResponse
--
--         , responseRegisterInstance $
--             newRegisterInstanceResponse
--
--         , responseAssociateElasticIp $
--             newAssociateElasticIpResponse
--
--         , responseDescribeLoadBasedAutoScaling $
--             newDescribeLoadBasedAutoScalingResponse
--
--         , responseDescribeStackProvisioningParameters $
--             newDescribeStackProvisioningParametersResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseUnassignInstance $
--             newUnassignInstanceResponse
--
--         , responseDescribeMyUserProfile $
--             newDescribeMyUserProfileResponse
--
--         , responseDeleteUserProfile $
--             newDeleteUserProfileResponse
--
--         , responseUpdateUserProfile $
--             newUpdateUserProfileResponse
--
--         , responseDescribeServiceErrors $
--             newDescribeServiceErrorsResponse
--
--         , responseRegisterRdsDbInstance $
--             newRegisterRdsDbInstanceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseStartStack $
--             newStartStackResponse
--
--         , responseCreateUserProfile $
--             newCreateUserProfileResponse
--
--         , responseDescribeOperatingSystems $
--             newDescribeOperatingSystemsResponse
--
--         , responseDescribeCommands $
--             newDescribeCommandsResponse
--
--         , responseAssignVolume $
--             newAssignVolumeResponse
--
--         , responseDescribeElasticLoadBalancers $
--             newDescribeElasticLoadBalancersResponse
--
--         , responseSetPermission $
--             newSetPermissionResponse
--
--         , responseDeregisterInstance $
--             newDeregisterInstanceResponse
--
--         , responseDescribeEcsClusters $
--             newDescribeEcsClustersResponse
--
--         , responseDescribeRaidArrays $
--             newDescribeRaidArraysResponse
--
--         , responseUpdateVolume $
--             newUpdateVolumeResponse
--
--         , responseStartInstance $
--             newStartInstanceResponse
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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRdsDbInstances)

responseDeleteStack :: DeleteStackResponse -> TestTree
responseDeleteStack =
  res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStack)

responseUpdateStack :: UpdateStackResponse -> TestTree
responseUpdateStack =
  res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStack)

responseCreateLayer :: CreateLayerResponse -> TestTree
responseCreateLayer =
  res
    "CreateLayerResponse"
    "fixture/CreateLayerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLayer)

responseSetLoadBasedAutoScaling :: SetLoadBasedAutoScalingResponse -> TestTree
responseSetLoadBasedAutoScaling =
  res
    "SetLoadBasedAutoScalingResponse"
    "fixture/SetLoadBasedAutoScalingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetLoadBasedAutoScaling)

responseDeregisterRdsDbInstance :: DeregisterRdsDbInstanceResponse -> TestTree
responseDeregisterRdsDbInstance =
  res
    "DeregisterRdsDbInstanceResponse"
    "fixture/DeregisterRdsDbInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterRdsDbInstance)

responseUnassignVolume :: UnassignVolumeResponse -> TestTree
responseUnassignVolume =
  res
    "UnassignVolumeResponse"
    "fixture/UnassignVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnassignVolume)

responseCreateInstance :: CreateInstanceResponse -> TestTree
responseCreateInstance =
  res
    "CreateInstanceResponse"
    "fixture/CreateInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstance)

responseDescribeLayers :: DescribeLayersResponse -> TestTree
responseDescribeLayers =
  res
    "DescribeLayersResponse"
    "fixture/DescribeLayersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLayers)

responseRegisterElasticIp :: RegisterElasticIpResponse -> TestTree
responseRegisterElasticIp =
  res
    "RegisterElasticIpResponse"
    "fixture/RegisterElasticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterElasticIp)

responseDescribeAgentVersions :: DescribeAgentVersionsResponse -> TestTree
responseDescribeAgentVersions =
  res
    "DescribeAgentVersionsResponse"
    "fixture/DescribeAgentVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAgentVersions)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeployment)

responseAssignInstance :: AssignInstanceResponse -> TestTree
responseAssignInstance =
  res
    "AssignInstanceResponse"
    "fixture/AssignInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssignInstance)

responseDescribeStacks :: DescribeStacksResponse -> TestTree
responseDescribeStacks =
  res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStacks)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance =
  res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInstance)

responseUpdateInstance :: UpdateInstanceResponse -> TestTree
responseUpdateInstance =
  res
    "UpdateInstanceResponse"
    "fixture/UpdateInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInstance)

responseDeregisterVolume :: DeregisterVolumeResponse -> TestTree
responseDeregisterVolume =
  res
    "DeregisterVolumeResponse"
    "fixture/DeregisterVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterVolume)

responseRebootInstance :: RebootInstanceResponse -> TestTree
responseRebootInstance =
  res
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootInstance)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApp)

responseUpdateApp :: UpdateAppResponse -> TestTree
responseUpdateApp =
  res
    "UpdateAppResponse"
    "fixture/UpdateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApp)

responseUpdateRdsDbInstance :: UpdateRdsDbInstanceResponse -> TestTree
responseUpdateRdsDbInstance =
  res
    "UpdateRdsDbInstanceResponse"
    "fixture/UpdateRdsDbInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRdsDbInstance)

responseDescribeTimeBasedAutoScaling :: DescribeTimeBasedAutoScalingResponse -> TestTree
responseDescribeTimeBasedAutoScaling =
  res
    "DescribeTimeBasedAutoScalingResponse"
    "fixture/DescribeTimeBasedAutoScalingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTimeBasedAutoScaling)

responseStopStack :: StopStackResponse -> TestTree
responseStopStack =
  res
    "StopStackResponse"
    "fixture/StopStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopStack)

responseDescribeVolumes :: DescribeVolumesResponse -> TestTree
responseDescribeVolumes =
  res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVolumes)

responseDisassociateElasticIp :: DisassociateElasticIpResponse -> TestTree
responseDisassociateElasticIp =
  res
    "DisassociateElasticIpResponse"
    "fixture/DisassociateElasticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateElasticIp)

responseRegisterEcsCluster :: RegisterEcsClusterResponse -> TestTree
responseRegisterEcsCluster =
  res
    "RegisterEcsClusterResponse"
    "fixture/RegisterEcsClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterEcsCluster)

responseStopInstance :: StopInstanceResponse -> TestTree
responseStopInstance =
  res
    "StopInstanceResponse"
    "fixture/StopInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopInstance)

responseRegisterVolume :: RegisterVolumeResponse -> TestTree
responseRegisterVolume =
  res
    "RegisterVolumeResponse"
    "fixture/RegisterVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterVolume)

responseSetTimeBasedAutoScaling :: SetTimeBasedAutoScalingResponse -> TestTree
responseSetTimeBasedAutoScaling =
  res
    "SetTimeBasedAutoScalingResponse"
    "fixture/SetTimeBasedAutoScalingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetTimeBasedAutoScaling)

responseDescribeUserProfiles :: DescribeUserProfilesResponse -> TestTree
responseDescribeUserProfiles =
  res
    "DescribeUserProfilesResponse"
    "fixture/DescribeUserProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserProfiles)

responseAttachElasticLoadBalancer :: AttachElasticLoadBalancerResponse -> TestTree
responseAttachElasticLoadBalancer =
  res
    "AttachElasticLoadBalancerResponse"
    "fixture/AttachElasticLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachElasticLoadBalancer)

responseDeregisterElasticIp :: DeregisterElasticIpResponse -> TestTree
responseDeregisterElasticIp =
  res
    "DeregisterElasticIpResponse"
    "fixture/DeregisterElasticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterElasticIp)

responseDeregisterEcsCluster :: DeregisterEcsClusterResponse -> TestTree
responseDeregisterEcsCluster =
  res
    "DeregisterEcsClusterResponse"
    "fixture/DeregisterEcsClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterEcsCluster)

responseDescribeApps :: DescribeAppsResponse -> TestTree
responseDescribeApps =
  res
    "DescribeAppsResponse"
    "fixture/DescribeAppsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApps)

responseUpdateMyUserProfile :: UpdateMyUserProfileResponse -> TestTree
responseUpdateMyUserProfile =
  res
    "UpdateMyUserProfileResponse"
    "fixture/UpdateMyUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMyUserProfile)

responseDescribeStackSummary :: DescribeStackSummaryResponse -> TestTree
responseDescribeStackSummary =
  res
    "DescribeStackSummaryResponse"
    "fixture/DescribeStackSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackSummary)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances =
  res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstances)

responseDescribeDeployments :: DescribeDeploymentsResponse -> TestTree
responseDescribeDeployments =
  res
    "DescribeDeploymentsResponse"
    "fixture/DescribeDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDeployments)

responseDescribeElasticIps :: DescribeElasticIpsResponse -> TestTree
responseDescribeElasticIps =
  res
    "DescribeElasticIpsResponse"
    "fixture/DescribeElasticIpsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeElasticIps)

responseGrantAccess :: GrantAccessResponse -> TestTree
responseGrantAccess =
  res
    "GrantAccessResponse"
    "fixture/GrantAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GrantAccess)

responseDeleteLayer :: DeleteLayerResponse -> TestTree
responseDeleteLayer =
  res
    "DeleteLayerResponse"
    "fixture/DeleteLayerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLayer)

responseUpdateLayer :: UpdateLayerResponse -> TestTree
responseUpdateLayer =
  res
    "UpdateLayerResponse"
    "fixture/UpdateLayerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLayer)

responseCreateStack :: CreateStackResponse -> TestTree
responseCreateStack =
  res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStack)

responseUpdateElasticIp :: UpdateElasticIpResponse -> TestTree
responseUpdateElasticIp =
  res
    "UpdateElasticIpResponse"
    "fixture/UpdateElasticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateElasticIp)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApp)

responseGetHostnameSuggestion :: GetHostnameSuggestionResponse -> TestTree
responseGetHostnameSuggestion =
  res
    "GetHostnameSuggestionResponse"
    "fixture/GetHostnameSuggestionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHostnameSuggestion)

responseCloneStack :: CloneStackResponse -> TestTree
responseCloneStack =
  res
    "CloneStackResponse"
    "fixture/CloneStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CloneStack)

responseDescribePermissions :: DescribePermissionsResponse -> TestTree
responseDescribePermissions =
  res
    "DescribePermissionsResponse"
    "fixture/DescribePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePermissions)

responseDetachElasticLoadBalancer :: DetachElasticLoadBalancerResponse -> TestTree
responseDetachElasticLoadBalancer =
  res
    "DetachElasticLoadBalancerResponse"
    "fixture/DetachElasticLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachElasticLoadBalancer)

responseRegisterInstance :: RegisterInstanceResponse -> TestTree
responseRegisterInstance =
  res
    "RegisterInstanceResponse"
    "fixture/RegisterInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterInstance)

responseAssociateElasticIp :: AssociateElasticIpResponse -> TestTree
responseAssociateElasticIp =
  res
    "AssociateElasticIpResponse"
    "fixture/AssociateElasticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateElasticIp)

responseDescribeLoadBasedAutoScaling :: DescribeLoadBasedAutoScalingResponse -> TestTree
responseDescribeLoadBasedAutoScaling =
  res
    "DescribeLoadBasedAutoScalingResponse"
    "fixture/DescribeLoadBasedAutoScalingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoadBasedAutoScaling)

responseDescribeStackProvisioningParameters :: DescribeStackProvisioningParametersResponse -> TestTree
responseDescribeStackProvisioningParameters =
  res
    "DescribeStackProvisioningParametersResponse"
    "fixture/DescribeStackProvisioningParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackProvisioningParameters)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responseUnassignInstance :: UnassignInstanceResponse -> TestTree
responseUnassignInstance =
  res
    "UnassignInstanceResponse"
    "fixture/UnassignInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnassignInstance)

responseDescribeMyUserProfile :: DescribeMyUserProfileResponse -> TestTree
responseDescribeMyUserProfile =
  res
    "DescribeMyUserProfileResponse"
    "fixture/DescribeMyUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMyUserProfile)

responseDeleteUserProfile :: DeleteUserProfileResponse -> TestTree
responseDeleteUserProfile =
  res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserProfile)

responseUpdateUserProfile :: UpdateUserProfileResponse -> TestTree
responseUpdateUserProfile =
  res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserProfile)

responseDescribeServiceErrors :: DescribeServiceErrorsResponse -> TestTree
responseDescribeServiceErrors =
  res
    "DescribeServiceErrorsResponse"
    "fixture/DescribeServiceErrorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServiceErrors)

responseRegisterRdsDbInstance :: RegisterRdsDbInstanceResponse -> TestTree
responseRegisterRdsDbInstance =
  res
    "RegisterRdsDbInstanceResponse"
    "fixture/RegisterRdsDbInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterRdsDbInstance)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseStartStack :: StartStackResponse -> TestTree
responseStartStack =
  res
    "StartStackResponse"
    "fixture/StartStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartStack)

responseCreateUserProfile :: CreateUserProfileResponse -> TestTree
responseCreateUserProfile =
  res
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserProfile)

responseDescribeOperatingSystems :: DescribeOperatingSystemsResponse -> TestTree
responseDescribeOperatingSystems =
  res
    "DescribeOperatingSystemsResponse"
    "fixture/DescribeOperatingSystemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOperatingSystems)

responseDescribeCommands :: DescribeCommandsResponse -> TestTree
responseDescribeCommands =
  res
    "DescribeCommandsResponse"
    "fixture/DescribeCommandsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCommands)

responseAssignVolume :: AssignVolumeResponse -> TestTree
responseAssignVolume =
  res
    "AssignVolumeResponse"
    "fixture/AssignVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssignVolume)

responseDescribeElasticLoadBalancers :: DescribeElasticLoadBalancersResponse -> TestTree
responseDescribeElasticLoadBalancers =
  res
    "DescribeElasticLoadBalancersResponse"
    "fixture/DescribeElasticLoadBalancersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeElasticLoadBalancers)

responseSetPermission :: SetPermissionResponse -> TestTree
responseSetPermission =
  res
    "SetPermissionResponse"
    "fixture/SetPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetPermission)

responseDeregisterInstance :: DeregisterInstanceResponse -> TestTree
responseDeregisterInstance =
  res
    "DeregisterInstanceResponse"
    "fixture/DeregisterInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterInstance)

responseDescribeEcsClusters :: DescribeEcsClustersResponse -> TestTree
responseDescribeEcsClusters =
  res
    "DescribeEcsClustersResponse"
    "fixture/DescribeEcsClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEcsClusters)

responseDescribeRaidArrays :: DescribeRaidArraysResponse -> TestTree
responseDescribeRaidArrays =
  res
    "DescribeRaidArraysResponse"
    "fixture/DescribeRaidArraysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRaidArrays)

responseUpdateVolume :: UpdateVolumeResponse -> TestTree
responseUpdateVolume =
  res
    "UpdateVolumeResponse"
    "fixture/UpdateVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVolume)

responseStartInstance :: StartInstanceResponse -> TestTree
responseStartInstance =
  res
    "StartInstanceResponse"
    "fixture/StartInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartInstance)
