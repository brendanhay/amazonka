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
--         [ requestDescribeInstances $
--             newDescribeInstances
--
--         , requestDescribeDeployments $
--             newDescribeDeployments
--
--         , requestUpdateMyUserProfile $
--             newUpdateMyUserProfile
--
--         , requestDeregisterElasticIp $
--             newDeregisterElasticIp
--
--         , requestSetTimeBasedAutoScaling $
--             newSetTimeBasedAutoScaling
--
--         , requestDescribeRdsDbInstances $
--             newDescribeRdsDbInstances
--
--         , requestAttachElasticLoadBalancer $
--             newAttachElasticLoadBalancer
--
--         , requestStartInstance $
--             newStartInstance
--
--         , requestSetPermission $
--             newSetPermission
--
--         , requestRegisterVolume $
--             newRegisterVolume
--
--         , requestStopInstance $
--             newStopInstance
--
--         , requestDescribeEcsClusters $
--             newDescribeEcsClusters
--
--         , requestDescribeVolumes $
--             newDescribeVolumes
--
--         , requestDescribeOperatingSystems $
--             newDescribeOperatingSystems
--
--         , requestDisassociateElasticIp $
--             newDisassociateElasticIp
--
--         , requestStartStack $
--             newStartStack
--
--         , requestStopStack $
--             newStopStack
--
--         , requestRegisterRdsDbInstance $
--             newRegisterRdsDbInstance
--
--         , requestDescribeServiceErrors $
--             newDescribeServiceErrors
--
--         , requestDescribeTimeBasedAutoScaling $
--             newDescribeTimeBasedAutoScaling
--
--         , requestUpdateUserProfile $
--             newUpdateUserProfile
--
--         , requestDescribeMyUserProfile $
--             newDescribeMyUserProfile
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteUserProfile $
--             newDeleteUserProfile
--
--         , requestAssignInstance $
--             newAssignInstance
--
--         , requestDetachElasticLoadBalancer $
--             newDetachElasticLoadBalancer
--
--         , requestDescribeStackProvisioningParameters $
--             newDescribeStackProvisioningParameters
--
--         , requestDeregisterVolume $
--             newDeregisterVolume
--
--         , requestDescribeStacks $
--             newDescribeStacks
--
--         , requestDeleteInstance $
--             newDeleteInstance
--
--         , requestRebootInstance $
--             newRebootInstance
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUpdateInstance $
--             newUpdateInstance
--
--         , requestCloneStack $
--             newCloneStack
--
--         , requestRegisterElasticIp $
--             newRegisterElasticIp
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
--         , requestUnassignVolume $
--             newUnassignVolume
--
--         , requestGrantAccess $
--             newGrantAccess
--
--         , requestDeleteLayer $
--             newDeleteLayer
--
--         , requestDescribeApps $
--             newDescribeApps
--
--         , requestDeregisterEcsCluster $
--             newDeregisterEcsCluster
--
--         , requestDescribeStackSummary $
--             newDescribeStackSummary
--
--         , requestDeleteStack $
--             newDeleteStack
--
--         , requestSetLoadBasedAutoScaling $
--             newSetLoadBasedAutoScaling
--
--         , requestCreateLayer $
--             newCreateLayer
--
--         , requestUpdateStack $
--             newUpdateStack
--
--         , requestDescribeUserProfiles $
--             newDescribeUserProfiles
--
--         , requestDescribeElasticLoadBalancers $
--             newDescribeElasticLoadBalancers
--
--         , requestDescribeCommands $
--             newDescribeCommands
--
--         , requestUpdateVolume $
--             newUpdateVolume
--
--         , requestAssignVolume $
--             newAssignVolume
--
--         , requestDescribeRaidArrays $
--             newDescribeRaidArrays
--
--         , requestDeregisterInstance $
--             newDeregisterInstance
--
--         , requestRegisterEcsCluster $
--             newRegisterEcsCluster
--
--         , requestCreateUserProfile $
--             newCreateUserProfile
--
--         , requestUpdateRdsDbInstance $
--             newUpdateRdsDbInstance
--
--         , requestUnassignInstance $
--             newUnassignInstance
--
--         , requestListTags $
--             newListTags
--
--         , requestDescribeLoadBasedAutoScaling $
--             newDescribeLoadBasedAutoScaling
--
--         , requestRegisterInstance $
--             newRegisterInstance
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestUpdateApp $
--             newUpdateApp
--
--         , requestAssociateElasticIp $
--             newAssociateElasticIp
--
--         , requestUpdateElasticIp $
--             newUpdateElasticIp
--
--         , requestDescribePermissions $
--             newDescribePermissions
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
--         , requestCreateApp $
--             newCreateApp
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestDeregisterRdsDbInstance $
--             newDeregisterRdsDbInstance
--
--         , requestDescribeElasticIps $
--             newDescribeElasticIps
--
--           ]

--     , testGroup "response"
--         [ responseDescribeInstances $
--             newDescribeInstancesResponse
--
--         , responseDescribeDeployments $
--             newDescribeDeploymentsResponse
--
--         , responseUpdateMyUserProfile $
--             newUpdateMyUserProfileResponse
--
--         , responseDeregisterElasticIp $
--             newDeregisterElasticIpResponse
--
--         , responseSetTimeBasedAutoScaling $
--             newSetTimeBasedAutoScalingResponse
--
--         , responseDescribeRdsDbInstances $
--             newDescribeRdsDbInstancesResponse
--
--         , responseAttachElasticLoadBalancer $
--             newAttachElasticLoadBalancerResponse
--
--         , responseStartInstance $
--             newStartInstanceResponse
--
--         , responseSetPermission $
--             newSetPermissionResponse
--
--         , responseRegisterVolume $
--             newRegisterVolumeResponse
--
--         , responseStopInstance $
--             newStopInstanceResponse
--
--         , responseDescribeEcsClusters $
--             newDescribeEcsClustersResponse
--
--         , responseDescribeVolumes $
--             newDescribeVolumesResponse
--
--         , responseDescribeOperatingSystems $
--             newDescribeOperatingSystemsResponse
--
--         , responseDisassociateElasticIp $
--             newDisassociateElasticIpResponse
--
--         , responseStartStack $
--             newStartStackResponse
--
--         , responseStopStack $
--             newStopStackResponse
--
--         , responseRegisterRdsDbInstance $
--             newRegisterRdsDbInstanceResponse
--
--         , responseDescribeServiceErrors $
--             newDescribeServiceErrorsResponse
--
--         , responseDescribeTimeBasedAutoScaling $
--             newDescribeTimeBasedAutoScalingResponse
--
--         , responseUpdateUserProfile $
--             newUpdateUserProfileResponse
--
--         , responseDescribeMyUserProfile $
--             newDescribeMyUserProfileResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteUserProfile $
--             newDeleteUserProfileResponse
--
--         , responseAssignInstance $
--             newAssignInstanceResponse
--
--         , responseDetachElasticLoadBalancer $
--             newDetachElasticLoadBalancerResponse
--
--         , responseDescribeStackProvisioningParameters $
--             newDescribeStackProvisioningParametersResponse
--
--         , responseDeregisterVolume $
--             newDeregisterVolumeResponse
--
--         , responseDescribeStacks $
--             newDescribeStacksResponse
--
--         , responseDeleteInstance $
--             newDeleteInstanceResponse
--
--         , responseRebootInstance $
--             newRebootInstanceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUpdateInstance $
--             newUpdateInstanceResponse
--
--         , responseCloneStack $
--             newCloneStackResponse
--
--         , responseRegisterElasticIp $
--             newRegisterElasticIpResponse
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
--         , responseUnassignVolume $
--             newUnassignVolumeResponse
--
--         , responseGrantAccess $
--             newGrantAccessResponse
--
--         , responseDeleteLayer $
--             newDeleteLayerResponse
--
--         , responseDescribeApps $
--             newDescribeAppsResponse
--
--         , responseDeregisterEcsCluster $
--             newDeregisterEcsClusterResponse
--
--         , responseDescribeStackSummary $
--             newDescribeStackSummaryResponse
--
--         , responseDeleteStack $
--             newDeleteStackResponse
--
--         , responseSetLoadBasedAutoScaling $
--             newSetLoadBasedAutoScalingResponse
--
--         , responseCreateLayer $
--             newCreateLayerResponse
--
--         , responseUpdateStack $
--             newUpdateStackResponse
--
--         , responseDescribeUserProfiles $
--             newDescribeUserProfilesResponse
--
--         , responseDescribeElasticLoadBalancers $
--             newDescribeElasticLoadBalancersResponse
--
--         , responseDescribeCommands $
--             newDescribeCommandsResponse
--
--         , responseUpdateVolume $
--             newUpdateVolumeResponse
--
--         , responseAssignVolume $
--             newAssignVolumeResponse
--
--         , responseDescribeRaidArrays $
--             newDescribeRaidArraysResponse
--
--         , responseDeregisterInstance $
--             newDeregisterInstanceResponse
--
--         , responseRegisterEcsCluster $
--             newRegisterEcsClusterResponse
--
--         , responseCreateUserProfile $
--             newCreateUserProfileResponse
--
--         , responseUpdateRdsDbInstance $
--             newUpdateRdsDbInstanceResponse
--
--         , responseUnassignInstance $
--             newUnassignInstanceResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseDescribeLoadBasedAutoScaling $
--             newDescribeLoadBasedAutoScalingResponse
--
--         , responseRegisterInstance $
--             newRegisterInstanceResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseUpdateApp $
--             newUpdateAppResponse
--
--         , responseAssociateElasticIp $
--             newAssociateElasticIpResponse
--
--         , responseUpdateElasticIp $
--             newUpdateElasticIpResponse
--
--         , responseDescribePermissions $
--             newDescribePermissionsResponse
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
--         , responseCreateApp $
--             newCreateAppResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseDeregisterRdsDbInstance $
--             newDeregisterRdsDbInstanceResponse
--
--         , responseDescribeElasticIps $
--             newDescribeElasticIpsResponse
--
--           ]
--     ]

-- Requests

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

requestUpdateMyUserProfile :: UpdateMyUserProfile -> TestTree
requestUpdateMyUserProfile =
  req
    "UpdateMyUserProfile"
    "fixture/UpdateMyUserProfile.yaml"

requestDeregisterElasticIp :: DeregisterElasticIp -> TestTree
requestDeregisterElasticIp =
  req
    "DeregisterElasticIp"
    "fixture/DeregisterElasticIp.yaml"

requestSetTimeBasedAutoScaling :: SetTimeBasedAutoScaling -> TestTree
requestSetTimeBasedAutoScaling =
  req
    "SetTimeBasedAutoScaling"
    "fixture/SetTimeBasedAutoScaling.yaml"

requestDescribeRdsDbInstances :: DescribeRdsDbInstances -> TestTree
requestDescribeRdsDbInstances =
  req
    "DescribeRdsDbInstances"
    "fixture/DescribeRdsDbInstances.yaml"

requestAttachElasticLoadBalancer :: AttachElasticLoadBalancer -> TestTree
requestAttachElasticLoadBalancer =
  req
    "AttachElasticLoadBalancer"
    "fixture/AttachElasticLoadBalancer.yaml"

requestStartInstance :: StartInstance -> TestTree
requestStartInstance =
  req
    "StartInstance"
    "fixture/StartInstance.yaml"

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

requestStopInstance :: StopInstance -> TestTree
requestStopInstance =
  req
    "StopInstance"
    "fixture/StopInstance.yaml"

requestDescribeEcsClusters :: DescribeEcsClusters -> TestTree
requestDescribeEcsClusters =
  req
    "DescribeEcsClusters"
    "fixture/DescribeEcsClusters.yaml"

requestDescribeVolumes :: DescribeVolumes -> TestTree
requestDescribeVolumes =
  req
    "DescribeVolumes"
    "fixture/DescribeVolumes.yaml"

requestDescribeOperatingSystems :: DescribeOperatingSystems -> TestTree
requestDescribeOperatingSystems =
  req
    "DescribeOperatingSystems"
    "fixture/DescribeOperatingSystems.yaml"

requestDisassociateElasticIp :: DisassociateElasticIp -> TestTree
requestDisassociateElasticIp =
  req
    "DisassociateElasticIp"
    "fixture/DisassociateElasticIp.yaml"

requestStartStack :: StartStack -> TestTree
requestStartStack =
  req
    "StartStack"
    "fixture/StartStack.yaml"

requestStopStack :: StopStack -> TestTree
requestStopStack =
  req
    "StopStack"
    "fixture/StopStack.yaml"

requestRegisterRdsDbInstance :: RegisterRdsDbInstance -> TestTree
requestRegisterRdsDbInstance =
  req
    "RegisterRdsDbInstance"
    "fixture/RegisterRdsDbInstance.yaml"

requestDescribeServiceErrors :: DescribeServiceErrors -> TestTree
requestDescribeServiceErrors =
  req
    "DescribeServiceErrors"
    "fixture/DescribeServiceErrors.yaml"

requestDescribeTimeBasedAutoScaling :: DescribeTimeBasedAutoScaling -> TestTree
requestDescribeTimeBasedAutoScaling =
  req
    "DescribeTimeBasedAutoScaling"
    "fixture/DescribeTimeBasedAutoScaling.yaml"

requestUpdateUserProfile :: UpdateUserProfile -> TestTree
requestUpdateUserProfile =
  req
    "UpdateUserProfile"
    "fixture/UpdateUserProfile.yaml"

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

requestDeleteUserProfile :: DeleteUserProfile -> TestTree
requestDeleteUserProfile =
  req
    "DeleteUserProfile"
    "fixture/DeleteUserProfile.yaml"

requestAssignInstance :: AssignInstance -> TestTree
requestAssignInstance =
  req
    "AssignInstance"
    "fixture/AssignInstance.yaml"

requestDetachElasticLoadBalancer :: DetachElasticLoadBalancer -> TestTree
requestDetachElasticLoadBalancer =
  req
    "DetachElasticLoadBalancer"
    "fixture/DetachElasticLoadBalancer.yaml"

requestDescribeStackProvisioningParameters :: DescribeStackProvisioningParameters -> TestTree
requestDescribeStackProvisioningParameters =
  req
    "DescribeStackProvisioningParameters"
    "fixture/DescribeStackProvisioningParameters.yaml"

requestDeregisterVolume :: DeregisterVolume -> TestTree
requestDeregisterVolume =
  req
    "DeregisterVolume"
    "fixture/DeregisterVolume.yaml"

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

requestRebootInstance :: RebootInstance -> TestTree
requestRebootInstance =
  req
    "RebootInstance"
    "fixture/RebootInstance.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUpdateInstance :: UpdateInstance -> TestTree
requestUpdateInstance =
  req
    "UpdateInstance"
    "fixture/UpdateInstance.yaml"

requestCloneStack :: CloneStack -> TestTree
requestCloneStack =
  req
    "CloneStack"
    "fixture/CloneStack.yaml"

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

requestDeleteLayer :: DeleteLayer -> TestTree
requestDeleteLayer =
  req
    "DeleteLayer"
    "fixture/DeleteLayer.yaml"

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

requestDescribeStackSummary :: DescribeStackSummary -> TestTree
requestDescribeStackSummary =
  req
    "DescribeStackSummary"
    "fixture/DescribeStackSummary.yaml"

requestDeleteStack :: DeleteStack -> TestTree
requestDeleteStack =
  req
    "DeleteStack"
    "fixture/DeleteStack.yaml"

requestSetLoadBasedAutoScaling :: SetLoadBasedAutoScaling -> TestTree
requestSetLoadBasedAutoScaling =
  req
    "SetLoadBasedAutoScaling"
    "fixture/SetLoadBasedAutoScaling.yaml"

requestCreateLayer :: CreateLayer -> TestTree
requestCreateLayer =
  req
    "CreateLayer"
    "fixture/CreateLayer.yaml"

requestUpdateStack :: UpdateStack -> TestTree
requestUpdateStack =
  req
    "UpdateStack"
    "fixture/UpdateStack.yaml"

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

requestDescribeCommands :: DescribeCommands -> TestTree
requestDescribeCommands =
  req
    "DescribeCommands"
    "fixture/DescribeCommands.yaml"

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

requestDescribeRaidArrays :: DescribeRaidArrays -> TestTree
requestDescribeRaidArrays =
  req
    "DescribeRaidArrays"
    "fixture/DescribeRaidArrays.yaml"

requestDeregisterInstance :: DeregisterInstance -> TestTree
requestDeregisterInstance =
  req
    "DeregisterInstance"
    "fixture/DeregisterInstance.yaml"

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

requestUpdateRdsDbInstance :: UpdateRdsDbInstance -> TestTree
requestUpdateRdsDbInstance =
  req
    "UpdateRdsDbInstance"
    "fixture/UpdateRdsDbInstance.yaml"

requestUnassignInstance :: UnassignInstance -> TestTree
requestUnassignInstance =
  req
    "UnassignInstance"
    "fixture/UnassignInstance.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestDescribeLoadBasedAutoScaling :: DescribeLoadBasedAutoScaling -> TestTree
requestDescribeLoadBasedAutoScaling =
  req
    "DescribeLoadBasedAutoScaling"
    "fixture/DescribeLoadBasedAutoScaling.yaml"

requestRegisterInstance :: RegisterInstance -> TestTree
requestRegisterInstance =
  req
    "RegisterInstance"
    "fixture/RegisterInstance.yaml"

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

requestAssociateElasticIp :: AssociateElasticIp -> TestTree
requestAssociateElasticIp =
  req
    "AssociateElasticIp"
    "fixture/AssociateElasticIp.yaml"

requestUpdateElasticIp :: UpdateElasticIp -> TestTree
requestUpdateElasticIp =
  req
    "UpdateElasticIp"
    "fixture/UpdateElasticIp.yaml"

requestDescribePermissions :: DescribePermissions -> TestTree
requestDescribePermissions =
  req
    "DescribePermissions"
    "fixture/DescribePermissions.yaml"

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

requestCreateApp :: CreateApp -> TestTree
requestCreateApp =
  req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestDeregisterRdsDbInstance :: DeregisterRdsDbInstance -> TestTree
requestDeregisterRdsDbInstance =
  req
    "DeregisterRdsDbInstance"
    "fixture/DeregisterRdsDbInstance.yaml"

requestDescribeElasticIps :: DescribeElasticIps -> TestTree
requestDescribeElasticIps =
  req
    "DescribeElasticIps"
    "fixture/DescribeElasticIps.yaml"

-- Responses

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances =
  res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstances)

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

responseDeregisterElasticIp :: DeregisterElasticIpResponse -> TestTree
responseDeregisterElasticIp =
  res
    "DeregisterElasticIpResponse"
    "fixture/DeregisterElasticIpResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterElasticIp)

responseSetTimeBasedAutoScaling :: SetTimeBasedAutoScalingResponse -> TestTree
responseSetTimeBasedAutoScaling =
  res
    "SetTimeBasedAutoScalingResponse"
    "fixture/SetTimeBasedAutoScalingResponse.proto"
    defaultService
    (Proxy :: Proxy SetTimeBasedAutoScaling)

responseDescribeRdsDbInstances :: DescribeRdsDbInstancesResponse -> TestTree
responseDescribeRdsDbInstances =
  res
    "DescribeRdsDbInstancesResponse"
    "fixture/DescribeRdsDbInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRdsDbInstances)

responseAttachElasticLoadBalancer :: AttachElasticLoadBalancerResponse -> TestTree
responseAttachElasticLoadBalancer =
  res
    "AttachElasticLoadBalancerResponse"
    "fixture/AttachElasticLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy AttachElasticLoadBalancer)

responseStartInstance :: StartInstanceResponse -> TestTree
responseStartInstance =
  res
    "StartInstanceResponse"
    "fixture/StartInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy StartInstance)

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

responseStopInstance :: StopInstanceResponse -> TestTree
responseStopInstance =
  res
    "StopInstanceResponse"
    "fixture/StopInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy StopInstance)

responseDescribeEcsClusters :: DescribeEcsClustersResponse -> TestTree
responseDescribeEcsClusters =
  res
    "DescribeEcsClustersResponse"
    "fixture/DescribeEcsClustersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEcsClusters)

responseDescribeVolumes :: DescribeVolumesResponse -> TestTree
responseDescribeVolumes =
  res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVolumes)

responseDescribeOperatingSystems :: DescribeOperatingSystemsResponse -> TestTree
responseDescribeOperatingSystems =
  res
    "DescribeOperatingSystemsResponse"
    "fixture/DescribeOperatingSystemsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOperatingSystems)

responseDisassociateElasticIp :: DisassociateElasticIpResponse -> TestTree
responseDisassociateElasticIp =
  res
    "DisassociateElasticIpResponse"
    "fixture/DisassociateElasticIpResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateElasticIp)

responseStartStack :: StartStackResponse -> TestTree
responseStartStack =
  res
    "StartStackResponse"
    "fixture/StartStackResponse.proto"
    defaultService
    (Proxy :: Proxy StartStack)

responseStopStack :: StopStackResponse -> TestTree
responseStopStack =
  res
    "StopStackResponse"
    "fixture/StopStackResponse.proto"
    defaultService
    (Proxy :: Proxy StopStack)

responseRegisterRdsDbInstance :: RegisterRdsDbInstanceResponse -> TestTree
responseRegisterRdsDbInstance =
  res
    "RegisterRdsDbInstanceResponse"
    "fixture/RegisterRdsDbInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterRdsDbInstance)

responseDescribeServiceErrors :: DescribeServiceErrorsResponse -> TestTree
responseDescribeServiceErrors =
  res
    "DescribeServiceErrorsResponse"
    "fixture/DescribeServiceErrorsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeServiceErrors)

responseDescribeTimeBasedAutoScaling :: DescribeTimeBasedAutoScalingResponse -> TestTree
responseDescribeTimeBasedAutoScaling =
  res
    "DescribeTimeBasedAutoScalingResponse"
    "fixture/DescribeTimeBasedAutoScalingResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTimeBasedAutoScaling)

responseUpdateUserProfile :: UpdateUserProfileResponse -> TestTree
responseUpdateUserProfile =
  res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserProfile)

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

responseDeleteUserProfile :: DeleteUserProfileResponse -> TestTree
responseDeleteUserProfile =
  res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserProfile)

responseAssignInstance :: AssignInstanceResponse -> TestTree
responseAssignInstance =
  res
    "AssignInstanceResponse"
    "fixture/AssignInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy AssignInstance)

responseDetachElasticLoadBalancer :: DetachElasticLoadBalancerResponse -> TestTree
responseDetachElasticLoadBalancer =
  res
    "DetachElasticLoadBalancerResponse"
    "fixture/DetachElasticLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy DetachElasticLoadBalancer)

responseDescribeStackProvisioningParameters :: DescribeStackProvisioningParametersResponse -> TestTree
responseDescribeStackProvisioningParameters =
  res
    "DescribeStackProvisioningParametersResponse"
    "fixture/DescribeStackProvisioningParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStackProvisioningParameters)

responseDeregisterVolume :: DeregisterVolumeResponse -> TestTree
responseDeregisterVolume =
  res
    "DeregisterVolumeResponse"
    "fixture/DeregisterVolumeResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterVolume)

responseDescribeStacks :: DescribeStacksResponse -> TestTree
responseDescribeStacks =
  res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStacks)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance =
  res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInstance)

responseRebootInstance :: RebootInstanceResponse -> TestTree
responseRebootInstance =
  res
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy RebootInstance)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseUpdateInstance :: UpdateInstanceResponse -> TestTree
responseUpdateInstance =
  res
    "UpdateInstanceResponse"
    "fixture/UpdateInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateInstance)

responseCloneStack :: CloneStackResponse -> TestTree
responseCloneStack =
  res
    "CloneStackResponse"
    "fixture/CloneStackResponse.proto"
    defaultService
    (Proxy :: Proxy CloneStack)

responseRegisterElasticIp :: RegisterElasticIpResponse -> TestTree
responseRegisterElasticIp =
  res
    "RegisterElasticIpResponse"
    "fixture/RegisterElasticIpResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterElasticIp)

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

responseDeleteLayer :: DeleteLayerResponse -> TestTree
responseDeleteLayer =
  res
    "DeleteLayerResponse"
    "fixture/DeleteLayerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLayer)

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

responseDescribeStackSummary :: DescribeStackSummaryResponse -> TestTree
responseDescribeStackSummary =
  res
    "DescribeStackSummaryResponse"
    "fixture/DescribeStackSummaryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStackSummary)

responseDeleteStack :: DeleteStackResponse -> TestTree
responseDeleteStack =
  res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStack)

responseSetLoadBasedAutoScaling :: SetLoadBasedAutoScalingResponse -> TestTree
responseSetLoadBasedAutoScaling =
  res
    "SetLoadBasedAutoScalingResponse"
    "fixture/SetLoadBasedAutoScalingResponse.proto"
    defaultService
    (Proxy :: Proxy SetLoadBasedAutoScaling)

responseCreateLayer :: CreateLayerResponse -> TestTree
responseCreateLayer =
  res
    "CreateLayerResponse"
    "fixture/CreateLayerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLayer)

responseUpdateStack :: UpdateStackResponse -> TestTree
responseUpdateStack =
  res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStack)

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

responseDescribeCommands :: DescribeCommandsResponse -> TestTree
responseDescribeCommands =
  res
    "DescribeCommandsResponse"
    "fixture/DescribeCommandsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCommands)

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

responseDescribeRaidArrays :: DescribeRaidArraysResponse -> TestTree
responseDescribeRaidArrays =
  res
    "DescribeRaidArraysResponse"
    "fixture/DescribeRaidArraysResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRaidArrays)

responseDeregisterInstance :: DeregisterInstanceResponse -> TestTree
responseDeregisterInstance =
  res
    "DeregisterInstanceResponse"
    "fixture/DeregisterInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterInstance)

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

responseUpdateRdsDbInstance :: UpdateRdsDbInstanceResponse -> TestTree
responseUpdateRdsDbInstance =
  res
    "UpdateRdsDbInstanceResponse"
    "fixture/UpdateRdsDbInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRdsDbInstance)

responseUnassignInstance :: UnassignInstanceResponse -> TestTree
responseUnassignInstance =
  res
    "UnassignInstanceResponse"
    "fixture/UnassignInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy UnassignInstance)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTags)

responseDescribeLoadBasedAutoScaling :: DescribeLoadBasedAutoScalingResponse -> TestTree
responseDescribeLoadBasedAutoScaling =
  res
    "DescribeLoadBasedAutoScalingResponse"
    "fixture/DescribeLoadBasedAutoScalingResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoadBasedAutoScaling)

responseRegisterInstance :: RegisterInstanceResponse -> TestTree
responseRegisterInstance =
  res
    "RegisterInstanceResponse"
    "fixture/RegisterInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterInstance)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApp)

responseUpdateApp :: UpdateAppResponse -> TestTree
responseUpdateApp =
  res
    "UpdateAppResponse"
    "fixture/UpdateAppResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApp)

responseAssociateElasticIp :: AssociateElasticIpResponse -> TestTree
responseAssociateElasticIp =
  res
    "AssociateElasticIpResponse"
    "fixture/AssociateElasticIpResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateElasticIp)

responseUpdateElasticIp :: UpdateElasticIpResponse -> TestTree
responseUpdateElasticIp =
  res
    "UpdateElasticIpResponse"
    "fixture/UpdateElasticIpResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateElasticIp)

responseDescribePermissions :: DescribePermissionsResponse -> TestTree
responseDescribePermissions =
  res
    "DescribePermissionsResponse"
    "fixture/DescribePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePermissions)

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

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApp)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeployment)

responseDeregisterRdsDbInstance :: DeregisterRdsDbInstanceResponse -> TestTree
responseDeregisterRdsDbInstance =
  res
    "DeregisterRdsDbInstanceResponse"
    "fixture/DeregisterRdsDbInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterRdsDbInstance)

responseDescribeElasticIps :: DescribeElasticIpsResponse -> TestTree
responseDescribeElasticIps =
  res
    "DescribeElasticIpsResponse"
    "fixture/DescribeElasticIpsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeElasticIps)
