{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.OpsWorks
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestDescribeDeployments $
--             newDescribeDeployments
--
--         , requestUpdateMyUserProfile $
--             newUpdateMyUserProfile
--
--         , requestDescribeInstances $
--             newDescribeInstances
--
--         , requestSetTimeBasedAutoScaling $
--             newSetTimeBasedAutoScaling
--
--         , requestAttachElasticLoadBalancer $
--             newAttachElasticLoadBalancer
--
--         , requestDescribeRdsDbInstances $
--             newDescribeRdsDbInstances
--
--         , requestDeregisterElasticIp $
--             newDeregisterElasticIp
--
--         , requestSetPermission $
--             newSetPermission
--
--         , requestRegisterVolume $
--             newRegisterVolume
--
--         , requestStartInstance $
--             newStartInstance
--
--         , requestDescribeEcsClusters $
--             newDescribeEcsClusters
--
--         , requestStopInstance $
--             newStopInstance
--
--         , requestDisassociateElasticIp $
--             newDisassociateElasticIp
--
--         , requestDescribeOperatingSystems $
--             newDescribeOperatingSystems
--
--         , requestStopStack $
--             newStopStack
--
--         , requestDescribeVolumes $
--             newDescribeVolumes
--
--         , requestStartStack $
--             newStartStack
--
--         , requestUpdateUserProfile $
--             newUpdateUserProfile
--
--         , requestDescribeTimeBasedAutoScaling $
--             newDescribeTimeBasedAutoScaling
--
--         , requestDescribeServiceErrors $
--             newDescribeServiceErrors
--
--         , requestRegisterRdsDbInstance $
--             newRegisterRdsDbInstance
--
--         , requestDeleteUserProfile $
--             newDeleteUserProfile
--
--         , requestDescribeMyUserProfile $
--             newDescribeMyUserProfile
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateInstance $
--             newUpdateInstance
--
--         , requestAssignInstance $
--             newAssignInstance
--
--         , requestTagResource $
--             newTagResource
--
--         , requestRebootInstance $
--             newRebootInstance
--
--         , requestDeregisterVolume $
--             newDeregisterVolume
--
--         , requestDescribeStackProvisioningParameters $
--             newDescribeStackProvisioningParameters
--
--         , requestDeleteInstance $
--             newDeleteInstance
--
--         , requestDescribeStacks $
--             newDescribeStacks
--
--         , requestDetachElasticLoadBalancer $
--             newDetachElasticLoadBalancer
--
--         , requestRegisterElasticIp $
--             newRegisterElasticIp
--
--         , requestCloneStack $
--             newCloneStack
--
--         , requestDescribeAgentVersions $
--             newDescribeAgentVersions
--
--         , requestUpdateLayer $
--             newUpdateLayer
--
--         , requestCreateStack $
--             newCreateStack
--
--         , requestDeleteLayer $
--             newDeleteLayer
--
--         , requestUnassignVolume $
--             newUnassignVolume
--
--         , requestGrantAccess $
--             newGrantAccess
--
--         , requestCreateLayer $
--             newCreateLayer
--
--         , requestDeleteStack $
--             newDeleteStack
--
--         , requestUpdateStack $
--             newUpdateStack
--
--         , requestSetLoadBasedAutoScaling $
--             newSetLoadBasedAutoScaling
--
--         , requestDescribeStackSummary $
--             newDescribeStackSummary
--
--         , requestDescribeApps $
--             newDescribeApps
--
--         , requestDeregisterEcsCluster $
--             newDeregisterEcsCluster
--
--         , requestDescribeUserProfiles $
--             newDescribeUserProfiles
--
--         , requestDescribeElasticLoadBalancers $
--             newDescribeElasticLoadBalancers
--
--         , requestDescribeRaidArrays $
--             newDescribeRaidArrays
--
--         , requestDescribeCommands $
--             newDescribeCommands
--
--         , requestDeregisterInstance $
--             newDeregisterInstance
--
--         , requestUpdateVolume $
--             newUpdateVolume
--
--         , requestAssignVolume $
--             newAssignVolume
--
--         , requestRegisterEcsCluster $
--             newRegisterEcsCluster
--
--         , requestCreateUserProfile $
--             newCreateUserProfile
--
--         , requestListTags $
--             newListTags
--
--         , requestUnassignInstance $
--             newUnassignInstance
--
--         , requestUpdateRdsDbInstance $
--             newUpdateRdsDbInstance
--
--         , requestRegisterInstance $
--             newRegisterInstance
--
--         , requestDescribeLoadBasedAutoScaling $
--             newDescribeLoadBasedAutoScaling
--
--         , requestAssociateElasticIp $
--             newAssociateElasticIp
--
--         , requestUpdateApp $
--             newUpdateApp
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestGetHostnameSuggestion $
--             newGetHostnameSuggestion
--
--         , requestCreateInstance $
--             newCreateInstance
--
--         , requestDescribeLayers $
--             newDescribeLayers
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestCreateApp $
--             newCreateApp
--
--         , requestDescribePermissions $
--             newDescribePermissions
--
--         , requestUpdateElasticIp $
--             newUpdateElasticIp
--
--         , requestDescribeElasticIps $
--             newDescribeElasticIps
--
--         , requestDeregisterRdsDbInstance $
--             newDeregisterRdsDbInstance
--
--           ]

--     , testGroup "response"
--         [ responseDescribeDeployments $
--             newDescribeDeploymentsResponse
--
--         , responseUpdateMyUserProfile $
--             newUpdateMyUserProfileResponse
--
--         , responseDescribeInstances $
--             newDescribeInstancesResponse
--
--         , responseSetTimeBasedAutoScaling $
--             newSetTimeBasedAutoScalingResponse
--
--         , responseAttachElasticLoadBalancer $
--             newAttachElasticLoadBalancerResponse
--
--         , responseDescribeRdsDbInstances $
--             newDescribeRdsDbInstancesResponse
--
--         , responseDeregisterElasticIp $
--             newDeregisterElasticIpResponse
--
--         , responseSetPermission $
--             newSetPermissionResponse
--
--         , responseRegisterVolume $
--             newRegisterVolumeResponse
--
--         , responseStartInstance $
--             newStartInstanceResponse
--
--         , responseDescribeEcsClusters $
--             newDescribeEcsClustersResponse
--
--         , responseStopInstance $
--             newStopInstanceResponse
--
--         , responseDisassociateElasticIp $
--             newDisassociateElasticIpResponse
--
--         , responseDescribeOperatingSystems $
--             newDescribeOperatingSystemsResponse
--
--         , responseStopStack $
--             newStopStackResponse
--
--         , responseDescribeVolumes $
--             newDescribeVolumesResponse
--
--         , responseStartStack $
--             newStartStackResponse
--
--         , responseUpdateUserProfile $
--             newUpdateUserProfileResponse
--
--         , responseDescribeTimeBasedAutoScaling $
--             newDescribeTimeBasedAutoScalingResponse
--
--         , responseDescribeServiceErrors $
--             newDescribeServiceErrorsResponse
--
--         , responseRegisterRdsDbInstance $
--             newRegisterRdsDbInstanceResponse
--
--         , responseDeleteUserProfile $
--             newDeleteUserProfileResponse
--
--         , responseDescribeMyUserProfile $
--             newDescribeMyUserProfileResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateInstance $
--             newUpdateInstanceResponse
--
--         , responseAssignInstance $
--             newAssignInstanceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseRebootInstance $
--             newRebootInstanceResponse
--
--         , responseDeregisterVolume $
--             newDeregisterVolumeResponse
--
--         , responseDescribeStackProvisioningParameters $
--             newDescribeStackProvisioningParametersResponse
--
--         , responseDeleteInstance $
--             newDeleteInstanceResponse
--
--         , responseDescribeStacks $
--             newDescribeStacksResponse
--
--         , responseDetachElasticLoadBalancer $
--             newDetachElasticLoadBalancerResponse
--
--         , responseRegisterElasticIp $
--             newRegisterElasticIpResponse
--
--         , responseCloneStack $
--             newCloneStackResponse
--
--         , responseDescribeAgentVersions $
--             newDescribeAgentVersionsResponse
--
--         , responseUpdateLayer $
--             newUpdateLayerResponse
--
--         , responseCreateStack $
--             newCreateStackResponse
--
--         , responseDeleteLayer $
--             newDeleteLayerResponse
--
--         , responseUnassignVolume $
--             newUnassignVolumeResponse
--
--         , responseGrantAccess $
--             newGrantAccessResponse
--
--         , responseCreateLayer $
--             newCreateLayerResponse
--
--         , responseDeleteStack $
--             newDeleteStackResponse
--
--         , responseUpdateStack $
--             newUpdateStackResponse
--
--         , responseSetLoadBasedAutoScaling $
--             newSetLoadBasedAutoScalingResponse
--
--         , responseDescribeStackSummary $
--             newDescribeStackSummaryResponse
--
--         , responseDescribeApps $
--             newDescribeAppsResponse
--
--         , responseDeregisterEcsCluster $
--             newDeregisterEcsClusterResponse
--
--         , responseDescribeUserProfiles $
--             newDescribeUserProfilesResponse
--
--         , responseDescribeElasticLoadBalancers $
--             newDescribeElasticLoadBalancersResponse
--
--         , responseDescribeRaidArrays $
--             newDescribeRaidArraysResponse
--
--         , responseDescribeCommands $
--             newDescribeCommandsResponse
--
--         , responseDeregisterInstance $
--             newDeregisterInstanceResponse
--
--         , responseUpdateVolume $
--             newUpdateVolumeResponse
--
--         , responseAssignVolume $
--             newAssignVolumeResponse
--
--         , responseRegisterEcsCluster $
--             newRegisterEcsClusterResponse
--
--         , responseCreateUserProfile $
--             newCreateUserProfileResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseUnassignInstance $
--             newUnassignInstanceResponse
--
--         , responseUpdateRdsDbInstance $
--             newUpdateRdsDbInstanceResponse
--
--         , responseRegisterInstance $
--             newRegisterInstanceResponse
--
--         , responseDescribeLoadBasedAutoScaling $
--             newDescribeLoadBasedAutoScalingResponse
--
--         , responseAssociateElasticIp $
--             newAssociateElasticIpResponse
--
--         , responseUpdateApp $
--             newUpdateAppResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseGetHostnameSuggestion $
--             newGetHostnameSuggestionResponse
--
--         , responseCreateInstance $
--             newCreateInstanceResponse
--
--         , responseDescribeLayers $
--             newDescribeLayersResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseCreateApp $
--             newCreateAppResponse
--
--         , responseDescribePermissions $
--             newDescribePermissionsResponse
--
--         , responseUpdateElasticIp $
--             newUpdateElasticIpResponse
--
--         , responseDescribeElasticIps $
--             newDescribeElasticIpsResponse
--
--         , responseDeregisterRdsDbInstance $
--             newDeregisterRdsDbInstanceResponse
--
--           ]
--     ]

-- Requests

requestDescribeDeployments :: DescribeDeployments -> TestTree
requestDescribeDeployments =
  req
    "DescribeDeployments"
    "fixture/DescribeDeployments.yaml"

requestUpdateMyUserProfile :: UpdateMyUserProfile -> TestTree
requestUpdateMyUserProfile =
  req
    "UpdateMyUserProfile"
    "fixture/UpdateMyUserProfile.yaml"

requestDescribeInstances :: DescribeInstances -> TestTree
requestDescribeInstances =
  req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

requestSetTimeBasedAutoScaling :: SetTimeBasedAutoScaling -> TestTree
requestSetTimeBasedAutoScaling =
  req
    "SetTimeBasedAutoScaling"
    "fixture/SetTimeBasedAutoScaling.yaml"

requestAttachElasticLoadBalancer :: AttachElasticLoadBalancer -> TestTree
requestAttachElasticLoadBalancer =
  req
    "AttachElasticLoadBalancer"
    "fixture/AttachElasticLoadBalancer.yaml"

requestDescribeRdsDbInstances :: DescribeRdsDbInstances -> TestTree
requestDescribeRdsDbInstances =
  req
    "DescribeRdsDbInstances"
    "fixture/DescribeRdsDbInstances.yaml"

requestDeregisterElasticIp :: DeregisterElasticIp -> TestTree
requestDeregisterElasticIp =
  req
    "DeregisterElasticIp"
    "fixture/DeregisterElasticIp.yaml"

requestSetPermission :: SetPermission -> TestTree
requestSetPermission =
  req
    "SetPermission"
    "fixture/SetPermission.yaml"

requestRegisterVolume :: RegisterVolume -> TestTree
requestRegisterVolume =
  req
    "RegisterVolume"
    "fixture/RegisterVolume.yaml"

requestStartInstance :: StartInstance -> TestTree
requestStartInstance =
  req
    "StartInstance"
    "fixture/StartInstance.yaml"

requestDescribeEcsClusters :: DescribeEcsClusters -> TestTree
requestDescribeEcsClusters =
  req
    "DescribeEcsClusters"
    "fixture/DescribeEcsClusters.yaml"

requestStopInstance :: StopInstance -> TestTree
requestStopInstance =
  req
    "StopInstance"
    "fixture/StopInstance.yaml"

requestDisassociateElasticIp :: DisassociateElasticIp -> TestTree
requestDisassociateElasticIp =
  req
    "DisassociateElasticIp"
    "fixture/DisassociateElasticIp.yaml"

requestDescribeOperatingSystems :: DescribeOperatingSystems -> TestTree
requestDescribeOperatingSystems =
  req
    "DescribeOperatingSystems"
    "fixture/DescribeOperatingSystems.yaml"

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

requestStartStack :: StartStack -> TestTree
requestStartStack =
  req
    "StartStack"
    "fixture/StartStack.yaml"

requestUpdateUserProfile :: UpdateUserProfile -> TestTree
requestUpdateUserProfile =
  req
    "UpdateUserProfile"
    "fixture/UpdateUserProfile.yaml"

requestDescribeTimeBasedAutoScaling :: DescribeTimeBasedAutoScaling -> TestTree
requestDescribeTimeBasedAutoScaling =
  req
    "DescribeTimeBasedAutoScaling"
    "fixture/DescribeTimeBasedAutoScaling.yaml"

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

requestDeleteUserProfile :: DeleteUserProfile -> TestTree
requestDeleteUserProfile =
  req
    "DeleteUserProfile"
    "fixture/DeleteUserProfile.yaml"

requestDescribeMyUserProfile :: DescribeMyUserProfile -> TestTree
requestDescribeMyUserProfile =
  req
    "DescribeMyUserProfile"
    "fixture/DescribeMyUserProfile.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateInstance :: UpdateInstance -> TestTree
requestUpdateInstance =
  req
    "UpdateInstance"
    "fixture/UpdateInstance.yaml"

requestAssignInstance :: AssignInstance -> TestTree
requestAssignInstance =
  req
    "AssignInstance"
    "fixture/AssignInstance.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestRebootInstance :: RebootInstance -> TestTree
requestRebootInstance =
  req
    "RebootInstance"
    "fixture/RebootInstance.yaml"

requestDeregisterVolume :: DeregisterVolume -> TestTree
requestDeregisterVolume =
  req
    "DeregisterVolume"
    "fixture/DeregisterVolume.yaml"

requestDescribeStackProvisioningParameters :: DescribeStackProvisioningParameters -> TestTree
requestDescribeStackProvisioningParameters =
  req
    "DescribeStackProvisioningParameters"
    "fixture/DescribeStackProvisioningParameters.yaml"

requestDeleteInstance :: DeleteInstance -> TestTree
requestDeleteInstance =
  req
    "DeleteInstance"
    "fixture/DeleteInstance.yaml"

requestDescribeStacks :: DescribeStacks -> TestTree
requestDescribeStacks =
  req
    "DescribeStacks"
    "fixture/DescribeStacks.yaml"

requestDetachElasticLoadBalancer :: DetachElasticLoadBalancer -> TestTree
requestDetachElasticLoadBalancer =
  req
    "DetachElasticLoadBalancer"
    "fixture/DetachElasticLoadBalancer.yaml"

requestRegisterElasticIp :: RegisterElasticIp -> TestTree
requestRegisterElasticIp =
  req
    "RegisterElasticIp"
    "fixture/RegisterElasticIp.yaml"

requestCloneStack :: CloneStack -> TestTree
requestCloneStack =
  req
    "CloneStack"
    "fixture/CloneStack.yaml"

requestDescribeAgentVersions :: DescribeAgentVersions -> TestTree
requestDescribeAgentVersions =
  req
    "DescribeAgentVersions"
    "fixture/DescribeAgentVersions.yaml"

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

requestDeleteLayer :: DeleteLayer -> TestTree
requestDeleteLayer =
  req
    "DeleteLayer"
    "fixture/DeleteLayer.yaml"

requestUnassignVolume :: UnassignVolume -> TestTree
requestUnassignVolume =
  req
    "UnassignVolume"
    "fixture/UnassignVolume.yaml"

requestGrantAccess :: GrantAccess -> TestTree
requestGrantAccess =
  req
    "GrantAccess"
    "fixture/GrantAccess.yaml"

requestCreateLayer :: CreateLayer -> TestTree
requestCreateLayer =
  req
    "CreateLayer"
    "fixture/CreateLayer.yaml"

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

requestSetLoadBasedAutoScaling :: SetLoadBasedAutoScaling -> TestTree
requestSetLoadBasedAutoScaling =
  req
    "SetLoadBasedAutoScaling"
    "fixture/SetLoadBasedAutoScaling.yaml"

requestDescribeStackSummary :: DescribeStackSummary -> TestTree
requestDescribeStackSummary =
  req
    "DescribeStackSummary"
    "fixture/DescribeStackSummary.yaml"

requestDescribeApps :: DescribeApps -> TestTree
requestDescribeApps =
  req
    "DescribeApps"
    "fixture/DescribeApps.yaml"

requestDeregisterEcsCluster :: DeregisterEcsCluster -> TestTree
requestDeregisterEcsCluster =
  req
    "DeregisterEcsCluster"
    "fixture/DeregisterEcsCluster.yaml"

requestDescribeUserProfiles :: DescribeUserProfiles -> TestTree
requestDescribeUserProfiles =
  req
    "DescribeUserProfiles"
    "fixture/DescribeUserProfiles.yaml"

requestDescribeElasticLoadBalancers :: DescribeElasticLoadBalancers -> TestTree
requestDescribeElasticLoadBalancers =
  req
    "DescribeElasticLoadBalancers"
    "fixture/DescribeElasticLoadBalancers.yaml"

requestDescribeRaidArrays :: DescribeRaidArrays -> TestTree
requestDescribeRaidArrays =
  req
    "DescribeRaidArrays"
    "fixture/DescribeRaidArrays.yaml"

requestDescribeCommands :: DescribeCommands -> TestTree
requestDescribeCommands =
  req
    "DescribeCommands"
    "fixture/DescribeCommands.yaml"

requestDeregisterInstance :: DeregisterInstance -> TestTree
requestDeregisterInstance =
  req
    "DeregisterInstance"
    "fixture/DeregisterInstance.yaml"

requestUpdateVolume :: UpdateVolume -> TestTree
requestUpdateVolume =
  req
    "UpdateVolume"
    "fixture/UpdateVolume.yaml"

requestAssignVolume :: AssignVolume -> TestTree
requestAssignVolume =
  req
    "AssignVolume"
    "fixture/AssignVolume.yaml"

requestRegisterEcsCluster :: RegisterEcsCluster -> TestTree
requestRegisterEcsCluster =
  req
    "RegisterEcsCluster"
    "fixture/RegisterEcsCluster.yaml"

requestCreateUserProfile :: CreateUserProfile -> TestTree
requestCreateUserProfile =
  req
    "CreateUserProfile"
    "fixture/CreateUserProfile.yaml"

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

requestUpdateRdsDbInstance :: UpdateRdsDbInstance -> TestTree
requestUpdateRdsDbInstance =
  req
    "UpdateRdsDbInstance"
    "fixture/UpdateRdsDbInstance.yaml"

requestRegisterInstance :: RegisterInstance -> TestTree
requestRegisterInstance =
  req
    "RegisterInstance"
    "fixture/RegisterInstance.yaml"

requestDescribeLoadBasedAutoScaling :: DescribeLoadBasedAutoScaling -> TestTree
requestDescribeLoadBasedAutoScaling =
  req
    "DescribeLoadBasedAutoScaling"
    "fixture/DescribeLoadBasedAutoScaling.yaml"

requestAssociateElasticIp :: AssociateElasticIp -> TestTree
requestAssociateElasticIp =
  req
    "AssociateElasticIp"
    "fixture/AssociateElasticIp.yaml"

requestUpdateApp :: UpdateApp -> TestTree
requestUpdateApp =
  req
    "UpdateApp"
    "fixture/UpdateApp.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestGetHostnameSuggestion :: GetHostnameSuggestion -> TestTree
requestGetHostnameSuggestion =
  req
    "GetHostnameSuggestion"
    "fixture/GetHostnameSuggestion.yaml"

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

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestCreateApp :: CreateApp -> TestTree
requestCreateApp =
  req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestDescribePermissions :: DescribePermissions -> TestTree
requestDescribePermissions =
  req
    "DescribePermissions"
    "fixture/DescribePermissions.yaml"

requestUpdateElasticIp :: UpdateElasticIp -> TestTree
requestUpdateElasticIp =
  req
    "UpdateElasticIp"
    "fixture/UpdateElasticIp.yaml"

requestDescribeElasticIps :: DescribeElasticIps -> TestTree
requestDescribeElasticIps =
  req
    "DescribeElasticIps"
    "fixture/DescribeElasticIps.yaml"

requestDeregisterRdsDbInstance :: DeregisterRdsDbInstance -> TestTree
requestDeregisterRdsDbInstance =
  req
    "DeregisterRdsDbInstance"
    "fixture/DeregisterRdsDbInstance.yaml"

-- Responses

responseDescribeDeployments :: DescribeDeploymentsResponse -> TestTree
responseDescribeDeployments =
  res
    "DescribeDeploymentsResponse"
    "fixture/DescribeDeploymentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDeployments)

responseUpdateMyUserProfile :: UpdateMyUserProfileResponse -> TestTree
responseUpdateMyUserProfile =
  res
    "UpdateMyUserProfileResponse"
    "fixture/UpdateMyUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMyUserProfile)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances =
  res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstances)

responseSetTimeBasedAutoScaling :: SetTimeBasedAutoScalingResponse -> TestTree
responseSetTimeBasedAutoScaling =
  res
    "SetTimeBasedAutoScalingResponse"
    "fixture/SetTimeBasedAutoScalingResponse.proto"
    defaultService
    (Proxy :: Proxy SetTimeBasedAutoScaling)

responseAttachElasticLoadBalancer :: AttachElasticLoadBalancerResponse -> TestTree
responseAttachElasticLoadBalancer =
  res
    "AttachElasticLoadBalancerResponse"
    "fixture/AttachElasticLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy AttachElasticLoadBalancer)

responseDescribeRdsDbInstances :: DescribeRdsDbInstancesResponse -> TestTree
responseDescribeRdsDbInstances =
  res
    "DescribeRdsDbInstancesResponse"
    "fixture/DescribeRdsDbInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRdsDbInstances)

responseDeregisterElasticIp :: DeregisterElasticIpResponse -> TestTree
responseDeregisterElasticIp =
  res
    "DeregisterElasticIpResponse"
    "fixture/DeregisterElasticIpResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterElasticIp)

responseSetPermission :: SetPermissionResponse -> TestTree
responseSetPermission =
  res
    "SetPermissionResponse"
    "fixture/SetPermissionResponse.proto"
    defaultService
    (Proxy :: Proxy SetPermission)

responseRegisterVolume :: RegisterVolumeResponse -> TestTree
responseRegisterVolume =
  res
    "RegisterVolumeResponse"
    "fixture/RegisterVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterVolume)

responseStartInstance :: StartInstanceResponse -> TestTree
responseStartInstance =
  res
    "StartInstanceResponse"
    "fixture/StartInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy StartInstance)

responseDescribeEcsClusters :: DescribeEcsClustersResponse -> TestTree
responseDescribeEcsClusters =
  res
    "DescribeEcsClustersResponse"
    "fixture/DescribeEcsClustersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEcsClusters)

responseStopInstance :: StopInstanceResponse -> TestTree
responseStopInstance =
  res
    "StopInstanceResponse"
    "fixture/StopInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy StopInstance)

responseDisassociateElasticIp :: DisassociateElasticIpResponse -> TestTree
responseDisassociateElasticIp =
  res
    "DisassociateElasticIpResponse"
    "fixture/DisassociateElasticIpResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateElasticIp)

responseDescribeOperatingSystems :: DescribeOperatingSystemsResponse -> TestTree
responseDescribeOperatingSystems =
  res
    "DescribeOperatingSystemsResponse"
    "fixture/DescribeOperatingSystemsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOperatingSystems)

responseStopStack :: StopStackResponse -> TestTree
responseStopStack =
  res
    "StopStackResponse"
    "fixture/StopStackResponse.proto"
    defaultService
    (Proxy :: Proxy StopStack)

responseDescribeVolumes :: DescribeVolumesResponse -> TestTree
responseDescribeVolumes =
  res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVolumes)

responseStartStack :: StartStackResponse -> TestTree
responseStartStack =
  res
    "StartStackResponse"
    "fixture/StartStackResponse.proto"
    defaultService
    (Proxy :: Proxy StartStack)

responseUpdateUserProfile :: UpdateUserProfileResponse -> TestTree
responseUpdateUserProfile =
  res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserProfile)

responseDescribeTimeBasedAutoScaling :: DescribeTimeBasedAutoScalingResponse -> TestTree
responseDescribeTimeBasedAutoScaling =
  res
    "DescribeTimeBasedAutoScalingResponse"
    "fixture/DescribeTimeBasedAutoScalingResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTimeBasedAutoScaling)

responseDescribeServiceErrors :: DescribeServiceErrorsResponse -> TestTree
responseDescribeServiceErrors =
  res
    "DescribeServiceErrorsResponse"
    "fixture/DescribeServiceErrorsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeServiceErrors)

responseRegisterRdsDbInstance :: RegisterRdsDbInstanceResponse -> TestTree
responseRegisterRdsDbInstance =
  res
    "RegisterRdsDbInstanceResponse"
    "fixture/RegisterRdsDbInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterRdsDbInstance)

responseDeleteUserProfile :: DeleteUserProfileResponse -> TestTree
responseDeleteUserProfile =
  res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserProfile)

responseDescribeMyUserProfile :: DescribeMyUserProfileResponse -> TestTree
responseDescribeMyUserProfile =
  res
    "DescribeMyUserProfileResponse"
    "fixture/DescribeMyUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMyUserProfile)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseUpdateInstance :: UpdateInstanceResponse -> TestTree
responseUpdateInstance =
  res
    "UpdateInstanceResponse"
    "fixture/UpdateInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateInstance)

responseAssignInstance :: AssignInstanceResponse -> TestTree
responseAssignInstance =
  res
    "AssignInstanceResponse"
    "fixture/AssignInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy AssignInstance)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseRebootInstance :: RebootInstanceResponse -> TestTree
responseRebootInstance =
  res
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy RebootInstance)

responseDeregisterVolume :: DeregisterVolumeResponse -> TestTree
responseDeregisterVolume =
  res
    "DeregisterVolumeResponse"
    "fixture/DeregisterVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterVolume)

responseDescribeStackProvisioningParameters :: DescribeStackProvisioningParametersResponse -> TestTree
responseDescribeStackProvisioningParameters =
  res
    "DescribeStackProvisioningParametersResponse"
    "fixture/DescribeStackProvisioningParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStackProvisioningParameters)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance =
  res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInstance)

responseDescribeStacks :: DescribeStacksResponse -> TestTree
responseDescribeStacks =
  res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStacks)

responseDetachElasticLoadBalancer :: DetachElasticLoadBalancerResponse -> TestTree
responseDetachElasticLoadBalancer =
  res
    "DetachElasticLoadBalancerResponse"
    "fixture/DetachElasticLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy DetachElasticLoadBalancer)

responseRegisterElasticIp :: RegisterElasticIpResponse -> TestTree
responseRegisterElasticIp =
  res
    "RegisterElasticIpResponse"
    "fixture/RegisterElasticIpResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterElasticIp)

responseCloneStack :: CloneStackResponse -> TestTree
responseCloneStack =
  res
    "CloneStackResponse"
    "fixture/CloneStackResponse.proto"
    defaultService
    (Proxy :: Proxy CloneStack)

responseDescribeAgentVersions :: DescribeAgentVersionsResponse -> TestTree
responseDescribeAgentVersions =
  res
    "DescribeAgentVersionsResponse"
    "fixture/DescribeAgentVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAgentVersions)

responseUpdateLayer :: UpdateLayerResponse -> TestTree
responseUpdateLayer =
  res
    "UpdateLayerResponse"
    "fixture/UpdateLayerResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateLayer)

responseCreateStack :: CreateStackResponse -> TestTree
responseCreateStack =
  res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStack)

responseDeleteLayer :: DeleteLayerResponse -> TestTree
responseDeleteLayer =
  res
    "DeleteLayerResponse"
    "fixture/DeleteLayerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLayer)

responseUnassignVolume :: UnassignVolumeResponse -> TestTree
responseUnassignVolume =
  res
    "UnassignVolumeResponse"
    "fixture/UnassignVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy UnassignVolume)

responseGrantAccess :: GrantAccessResponse -> TestTree
responseGrantAccess =
  res
    "GrantAccessResponse"
    "fixture/GrantAccessResponse.proto"
    defaultService
    (Proxy :: Proxy GrantAccess)

responseCreateLayer :: CreateLayerResponse -> TestTree
responseCreateLayer =
  res
    "CreateLayerResponse"
    "fixture/CreateLayerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLayer)

responseDeleteStack :: DeleteStackResponse -> TestTree
responseDeleteStack =
  res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStack)

responseUpdateStack :: UpdateStackResponse -> TestTree
responseUpdateStack =
  res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStack)

responseSetLoadBasedAutoScaling :: SetLoadBasedAutoScalingResponse -> TestTree
responseSetLoadBasedAutoScaling =
  res
    "SetLoadBasedAutoScalingResponse"
    "fixture/SetLoadBasedAutoScalingResponse.proto"
    defaultService
    (Proxy :: Proxy SetLoadBasedAutoScaling)

responseDescribeStackSummary :: DescribeStackSummaryResponse -> TestTree
responseDescribeStackSummary =
  res
    "DescribeStackSummaryResponse"
    "fixture/DescribeStackSummaryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStackSummary)

responseDescribeApps :: DescribeAppsResponse -> TestTree
responseDescribeApps =
  res
    "DescribeAppsResponse"
    "fixture/DescribeAppsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeApps)

responseDeregisterEcsCluster :: DeregisterEcsClusterResponse -> TestTree
responseDeregisterEcsCluster =
  res
    "DeregisterEcsClusterResponse"
    "fixture/DeregisterEcsClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterEcsCluster)

responseDescribeUserProfiles :: DescribeUserProfilesResponse -> TestTree
responseDescribeUserProfiles =
  res
    "DescribeUserProfilesResponse"
    "fixture/DescribeUserProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserProfiles)

responseDescribeElasticLoadBalancers :: DescribeElasticLoadBalancersResponse -> TestTree
responseDescribeElasticLoadBalancers =
  res
    "DescribeElasticLoadBalancersResponse"
    "fixture/DescribeElasticLoadBalancersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeElasticLoadBalancers)

responseDescribeRaidArrays :: DescribeRaidArraysResponse -> TestTree
responseDescribeRaidArrays =
  res
    "DescribeRaidArraysResponse"
    "fixture/DescribeRaidArraysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRaidArrays)

responseDescribeCommands :: DescribeCommandsResponse -> TestTree
responseDescribeCommands =
  res
    "DescribeCommandsResponse"
    "fixture/DescribeCommandsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCommands)

responseDeregisterInstance :: DeregisterInstanceResponse -> TestTree
responseDeregisterInstance =
  res
    "DeregisterInstanceResponse"
    "fixture/DeregisterInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterInstance)

responseUpdateVolume :: UpdateVolumeResponse -> TestTree
responseUpdateVolume =
  res
    "UpdateVolumeResponse"
    "fixture/UpdateVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateVolume)

responseAssignVolume :: AssignVolumeResponse -> TestTree
responseAssignVolume =
  res
    "AssignVolumeResponse"
    "fixture/AssignVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy AssignVolume)

responseRegisterEcsCluster :: RegisterEcsClusterResponse -> TestTree
responseRegisterEcsCluster =
  res
    "RegisterEcsClusterResponse"
    "fixture/RegisterEcsClusterResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterEcsCluster)

responseCreateUserProfile :: CreateUserProfileResponse -> TestTree
responseCreateUserProfile =
  res
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUserProfile)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTags)

responseUnassignInstance :: UnassignInstanceResponse -> TestTree
responseUnassignInstance =
  res
    "UnassignInstanceResponse"
    "fixture/UnassignInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy UnassignInstance)

responseUpdateRdsDbInstance :: UpdateRdsDbInstanceResponse -> TestTree
responseUpdateRdsDbInstance =
  res
    "UpdateRdsDbInstanceResponse"
    "fixture/UpdateRdsDbInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRdsDbInstance)

responseRegisterInstance :: RegisterInstanceResponse -> TestTree
responseRegisterInstance =
  res
    "RegisterInstanceResponse"
    "fixture/RegisterInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterInstance)

responseDescribeLoadBasedAutoScaling :: DescribeLoadBasedAutoScalingResponse -> TestTree
responseDescribeLoadBasedAutoScaling =
  res
    "DescribeLoadBasedAutoScalingResponse"
    "fixture/DescribeLoadBasedAutoScalingResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoadBasedAutoScaling)

responseAssociateElasticIp :: AssociateElasticIpResponse -> TestTree
responseAssociateElasticIp =
  res
    "AssociateElasticIpResponse"
    "fixture/AssociateElasticIpResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateElasticIp)

responseUpdateApp :: UpdateAppResponse -> TestTree
responseUpdateApp =
  res
    "UpdateAppResponse"
    "fixture/UpdateAppResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApp)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApp)

responseGetHostnameSuggestion :: GetHostnameSuggestionResponse -> TestTree
responseGetHostnameSuggestion =
  res
    "GetHostnameSuggestionResponse"
    "fixture/GetHostnameSuggestionResponse.proto"
    defaultService
    (Proxy :: Proxy GetHostnameSuggestion)

responseCreateInstance :: CreateInstanceResponse -> TestTree
responseCreateInstance =
  res
    "CreateInstanceResponse"
    "fixture/CreateInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInstance)

responseDescribeLayers :: DescribeLayersResponse -> TestTree
responseDescribeLayers =
  res
    "DescribeLayersResponse"
    "fixture/DescribeLayersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLayers)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeployment)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApp)

responseDescribePermissions :: DescribePermissionsResponse -> TestTree
responseDescribePermissions =
  res
    "DescribePermissionsResponse"
    "fixture/DescribePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePermissions)

responseUpdateElasticIp :: UpdateElasticIpResponse -> TestTree
responseUpdateElasticIp =
  res
    "UpdateElasticIpResponse"
    "fixture/UpdateElasticIpResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateElasticIp)

responseDescribeElasticIps :: DescribeElasticIpsResponse -> TestTree
responseDescribeElasticIps =
  res
    "DescribeElasticIpsResponse"
    "fixture/DescribeElasticIpsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeElasticIps)

responseDeregisterRdsDbInstance :: DeregisterRdsDbInstanceResponse -> TestTree
responseDeregisterRdsDbInstance =
  res
    "DeregisterRdsDbInstanceResponse"
    "fixture/DeregisterRdsDbInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterRdsDbInstance)
