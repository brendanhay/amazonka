{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.OpsWorks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         [ requestAssignInstance $
--             newAssignInstance
--
--         , requestAssignVolume $
--             newAssignVolume
--
--         , requestAssociateElasticIp $
--             newAssociateElasticIp
--
--         , requestAttachElasticLoadBalancer $
--             newAttachElasticLoadBalancer
--
--         , requestCloneStack $
--             newCloneStack
--
--         , requestCreateApp $
--             newCreateApp
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestCreateInstance $
--             newCreateInstance
--
--         , requestCreateLayer $
--             newCreateLayer
--
--         , requestCreateStack $
--             newCreateStack
--
--         , requestCreateUserProfile $
--             newCreateUserProfile
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestDeleteInstance $
--             newDeleteInstance
--
--         , requestDeleteLayer $
--             newDeleteLayer
--
--         , requestDeleteStack $
--             newDeleteStack
--
--         , requestDeleteUserProfile $
--             newDeleteUserProfile
--
--         , requestDeregisterEcsCluster $
--             newDeregisterEcsCluster
--
--         , requestDeregisterElasticIp $
--             newDeregisterElasticIp
--
--         , requestDeregisterInstance $
--             newDeregisterInstance
--
--         , requestDeregisterRdsDbInstance $
--             newDeregisterRdsDbInstance
--
--         , requestDeregisterVolume $
--             newDeregisterVolume
--
--         , requestDescribeAgentVersions $
--             newDescribeAgentVersions
--
--         , requestDescribeApps $
--             newDescribeApps
--
--         , requestDescribeCommands $
--             newDescribeCommands
--
--         , requestDescribeDeployments $
--             newDescribeDeployments
--
--         , requestDescribeEcsClusters $
--             newDescribeEcsClusters
--
--         , requestDescribeElasticIps $
--             newDescribeElasticIps
--
--         , requestDescribeElasticLoadBalancers $
--             newDescribeElasticLoadBalancers
--
--         , requestDescribeInstances $
--             newDescribeInstances
--
--         , requestDescribeLayers $
--             newDescribeLayers
--
--         , requestDescribeLoadBasedAutoScaling $
--             newDescribeLoadBasedAutoScaling
--
--         , requestDescribeMyUserProfile $
--             newDescribeMyUserProfile
--
--         , requestDescribeOperatingSystems $
--             newDescribeOperatingSystems
--
--         , requestDescribePermissions $
--             newDescribePermissions
--
--         , requestDescribeRaidArrays $
--             newDescribeRaidArrays
--
--         , requestDescribeRdsDbInstances $
--             newDescribeRdsDbInstances
--
--         , requestDescribeServiceErrors $
--             newDescribeServiceErrors
--
--         , requestDescribeStackProvisioningParameters $
--             newDescribeStackProvisioningParameters
--
--         , requestDescribeStackSummary $
--             newDescribeStackSummary
--
--         , requestDescribeStacks $
--             newDescribeStacks
--
--         , requestDescribeTimeBasedAutoScaling $
--             newDescribeTimeBasedAutoScaling
--
--         , requestDescribeUserProfiles $
--             newDescribeUserProfiles
--
--         , requestDescribeVolumes $
--             newDescribeVolumes
--
--         , requestDetachElasticLoadBalancer $
--             newDetachElasticLoadBalancer
--
--         , requestDisassociateElasticIp $
--             newDisassociateElasticIp
--
--         , requestGetHostnameSuggestion $
--             newGetHostnameSuggestion
--
--         , requestGrantAccess $
--             newGrantAccess
--
--         , requestListTags $
--             newListTags
--
--         , requestRebootInstance $
--             newRebootInstance
--
--         , requestRegisterEcsCluster $
--             newRegisterEcsCluster
--
--         , requestRegisterElasticIp $
--             newRegisterElasticIp
--
--         , requestRegisterInstance $
--             newRegisterInstance
--
--         , requestRegisterRdsDbInstance $
--             newRegisterRdsDbInstance
--
--         , requestRegisterVolume $
--             newRegisterVolume
--
--         , requestSetLoadBasedAutoScaling $
--             newSetLoadBasedAutoScaling
--
--         , requestSetPermission $
--             newSetPermission
--
--         , requestSetTimeBasedAutoScaling $
--             newSetTimeBasedAutoScaling
--
--         , requestStartInstance $
--             newStartInstance
--
--         , requestStartStack $
--             newStartStack
--
--         , requestStopInstance $
--             newStopInstance
--
--         , requestStopStack $
--             newStopStack
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUnassignInstance $
--             newUnassignInstance
--
--         , requestUnassignVolume $
--             newUnassignVolume
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateApp $
--             newUpdateApp
--
--         , requestUpdateElasticIp $
--             newUpdateElasticIp
--
--         , requestUpdateInstance $
--             newUpdateInstance
--
--         , requestUpdateLayer $
--             newUpdateLayer
--
--         , requestUpdateMyUserProfile $
--             newUpdateMyUserProfile
--
--         , requestUpdateRdsDbInstance $
--             newUpdateRdsDbInstance
--
--         , requestUpdateStack $
--             newUpdateStack
--
--         , requestUpdateUserProfile $
--             newUpdateUserProfile
--
--         , requestUpdateVolume $
--             newUpdateVolume
--
--           ]

--     , testGroup "response"
--         [ responseAssignInstance $
--             newAssignInstanceResponse
--
--         , responseAssignVolume $
--             newAssignVolumeResponse
--
--         , responseAssociateElasticIp $
--             newAssociateElasticIpResponse
--
--         , responseAttachElasticLoadBalancer $
--             newAttachElasticLoadBalancerResponse
--
--         , responseCloneStack $
--             newCloneStackResponse
--
--         , responseCreateApp $
--             newCreateAppResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseCreateInstance $
--             newCreateInstanceResponse
--
--         , responseCreateLayer $
--             newCreateLayerResponse
--
--         , responseCreateStack $
--             newCreateStackResponse
--
--         , responseCreateUserProfile $
--             newCreateUserProfileResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseDeleteInstance $
--             newDeleteInstanceResponse
--
--         , responseDeleteLayer $
--             newDeleteLayerResponse
--
--         , responseDeleteStack $
--             newDeleteStackResponse
--
--         , responseDeleteUserProfile $
--             newDeleteUserProfileResponse
--
--         , responseDeregisterEcsCluster $
--             newDeregisterEcsClusterResponse
--
--         , responseDeregisterElasticIp $
--             newDeregisterElasticIpResponse
--
--         , responseDeregisterInstance $
--             newDeregisterInstanceResponse
--
--         , responseDeregisterRdsDbInstance $
--             newDeregisterRdsDbInstanceResponse
--
--         , responseDeregisterVolume $
--             newDeregisterVolumeResponse
--
--         , responseDescribeAgentVersions $
--             newDescribeAgentVersionsResponse
--
--         , responseDescribeApps $
--             newDescribeAppsResponse
--
--         , responseDescribeCommands $
--             newDescribeCommandsResponse
--
--         , responseDescribeDeployments $
--             newDescribeDeploymentsResponse
--
--         , responseDescribeEcsClusters $
--             newDescribeEcsClustersResponse
--
--         , responseDescribeElasticIps $
--             newDescribeElasticIpsResponse
--
--         , responseDescribeElasticLoadBalancers $
--             newDescribeElasticLoadBalancersResponse
--
--         , responseDescribeInstances $
--             newDescribeInstancesResponse
--
--         , responseDescribeLayers $
--             newDescribeLayersResponse
--
--         , responseDescribeLoadBasedAutoScaling $
--             newDescribeLoadBasedAutoScalingResponse
--
--         , responseDescribeMyUserProfile $
--             newDescribeMyUserProfileResponse
--
--         , responseDescribeOperatingSystems $
--             newDescribeOperatingSystemsResponse
--
--         , responseDescribePermissions $
--             newDescribePermissionsResponse
--
--         , responseDescribeRaidArrays $
--             newDescribeRaidArraysResponse
--
--         , responseDescribeRdsDbInstances $
--             newDescribeRdsDbInstancesResponse
--
--         , responseDescribeServiceErrors $
--             newDescribeServiceErrorsResponse
--
--         , responseDescribeStackProvisioningParameters $
--             newDescribeStackProvisioningParametersResponse
--
--         , responseDescribeStackSummary $
--             newDescribeStackSummaryResponse
--
--         , responseDescribeStacks $
--             newDescribeStacksResponse
--
--         , responseDescribeTimeBasedAutoScaling $
--             newDescribeTimeBasedAutoScalingResponse
--
--         , responseDescribeUserProfiles $
--             newDescribeUserProfilesResponse
--
--         , responseDescribeVolumes $
--             newDescribeVolumesResponse
--
--         , responseDetachElasticLoadBalancer $
--             newDetachElasticLoadBalancerResponse
--
--         , responseDisassociateElasticIp $
--             newDisassociateElasticIpResponse
--
--         , responseGetHostnameSuggestion $
--             newGetHostnameSuggestionResponse
--
--         , responseGrantAccess $
--             newGrantAccessResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseRebootInstance $
--             newRebootInstanceResponse
--
--         , responseRegisterEcsCluster $
--             newRegisterEcsClusterResponse
--
--         , responseRegisterElasticIp $
--             newRegisterElasticIpResponse
--
--         , responseRegisterInstance $
--             newRegisterInstanceResponse
--
--         , responseRegisterRdsDbInstance $
--             newRegisterRdsDbInstanceResponse
--
--         , responseRegisterVolume $
--             newRegisterVolumeResponse
--
--         , responseSetLoadBasedAutoScaling $
--             newSetLoadBasedAutoScalingResponse
--
--         , responseSetPermission $
--             newSetPermissionResponse
--
--         , responseSetTimeBasedAutoScaling $
--             newSetTimeBasedAutoScalingResponse
--
--         , responseStartInstance $
--             newStartInstanceResponse
--
--         , responseStartStack $
--             newStartStackResponse
--
--         , responseStopInstance $
--             newStopInstanceResponse
--
--         , responseStopStack $
--             newStopStackResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUnassignInstance $
--             newUnassignInstanceResponse
--
--         , responseUnassignVolume $
--             newUnassignVolumeResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApp $
--             newUpdateAppResponse
--
--         , responseUpdateElasticIp $
--             newUpdateElasticIpResponse
--
--         , responseUpdateInstance $
--             newUpdateInstanceResponse
--
--         , responseUpdateLayer $
--             newUpdateLayerResponse
--
--         , responseUpdateMyUserProfile $
--             newUpdateMyUserProfileResponse
--
--         , responseUpdateRdsDbInstance $
--             newUpdateRdsDbInstanceResponse
--
--         , responseUpdateStack $
--             newUpdateStackResponse
--
--         , responseUpdateUserProfile $
--             newUpdateUserProfileResponse
--
--         , responseUpdateVolume $
--             newUpdateVolumeResponse
--
--           ]
--     ]

-- Requests

requestAssignInstance :: AssignInstance -> TestTree
requestAssignInstance =
  req
    "AssignInstance"
    "fixture/AssignInstance.yaml"

requestAssignVolume :: AssignVolume -> TestTree
requestAssignVolume =
  req
    "AssignVolume"
    "fixture/AssignVolume.yaml"

requestAssociateElasticIp :: AssociateElasticIp -> TestTree
requestAssociateElasticIp =
  req
    "AssociateElasticIp"
    "fixture/AssociateElasticIp.yaml"

requestAttachElasticLoadBalancer :: AttachElasticLoadBalancer -> TestTree
requestAttachElasticLoadBalancer =
  req
    "AttachElasticLoadBalancer"
    "fixture/AttachElasticLoadBalancer.yaml"

requestCloneStack :: CloneStack -> TestTree
requestCloneStack =
  req
    "CloneStack"
    "fixture/CloneStack.yaml"

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

requestCreateInstance :: CreateInstance -> TestTree
requestCreateInstance =
  req
    "CreateInstance"
    "fixture/CreateInstance.yaml"

requestCreateLayer :: CreateLayer -> TestTree
requestCreateLayer =
  req
    "CreateLayer"
    "fixture/CreateLayer.yaml"

requestCreateStack :: CreateStack -> TestTree
requestCreateStack =
  req
    "CreateStack"
    "fixture/CreateStack.yaml"

requestCreateUserProfile :: CreateUserProfile -> TestTree
requestCreateUserProfile =
  req
    "CreateUserProfile"
    "fixture/CreateUserProfile.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestDeleteInstance :: DeleteInstance -> TestTree
requestDeleteInstance =
  req
    "DeleteInstance"
    "fixture/DeleteInstance.yaml"

requestDeleteLayer :: DeleteLayer -> TestTree
requestDeleteLayer =
  req
    "DeleteLayer"
    "fixture/DeleteLayer.yaml"

requestDeleteStack :: DeleteStack -> TestTree
requestDeleteStack =
  req
    "DeleteStack"
    "fixture/DeleteStack.yaml"

requestDeleteUserProfile :: DeleteUserProfile -> TestTree
requestDeleteUserProfile =
  req
    "DeleteUserProfile"
    "fixture/DeleteUserProfile.yaml"

requestDeregisterEcsCluster :: DeregisterEcsCluster -> TestTree
requestDeregisterEcsCluster =
  req
    "DeregisterEcsCluster"
    "fixture/DeregisterEcsCluster.yaml"

requestDeregisterElasticIp :: DeregisterElasticIp -> TestTree
requestDeregisterElasticIp =
  req
    "DeregisterElasticIp"
    "fixture/DeregisterElasticIp.yaml"

requestDeregisterInstance :: DeregisterInstance -> TestTree
requestDeregisterInstance =
  req
    "DeregisterInstance"
    "fixture/DeregisterInstance.yaml"

requestDeregisterRdsDbInstance :: DeregisterRdsDbInstance -> TestTree
requestDeregisterRdsDbInstance =
  req
    "DeregisterRdsDbInstance"
    "fixture/DeregisterRdsDbInstance.yaml"

requestDeregisterVolume :: DeregisterVolume -> TestTree
requestDeregisterVolume =
  req
    "DeregisterVolume"
    "fixture/DeregisterVolume.yaml"

requestDescribeAgentVersions :: DescribeAgentVersions -> TestTree
requestDescribeAgentVersions =
  req
    "DescribeAgentVersions"
    "fixture/DescribeAgentVersions.yaml"

requestDescribeApps :: DescribeApps -> TestTree
requestDescribeApps =
  req
    "DescribeApps"
    "fixture/DescribeApps.yaml"

requestDescribeCommands :: DescribeCommands -> TestTree
requestDescribeCommands =
  req
    "DescribeCommands"
    "fixture/DescribeCommands.yaml"

requestDescribeDeployments :: DescribeDeployments -> TestTree
requestDescribeDeployments =
  req
    "DescribeDeployments"
    "fixture/DescribeDeployments.yaml"

requestDescribeEcsClusters :: DescribeEcsClusters -> TestTree
requestDescribeEcsClusters =
  req
    "DescribeEcsClusters"
    "fixture/DescribeEcsClusters.yaml"

requestDescribeElasticIps :: DescribeElasticIps -> TestTree
requestDescribeElasticIps =
  req
    "DescribeElasticIps"
    "fixture/DescribeElasticIps.yaml"

requestDescribeElasticLoadBalancers :: DescribeElasticLoadBalancers -> TestTree
requestDescribeElasticLoadBalancers =
  req
    "DescribeElasticLoadBalancers"
    "fixture/DescribeElasticLoadBalancers.yaml"

requestDescribeInstances :: DescribeInstances -> TestTree
requestDescribeInstances =
  req
    "DescribeInstances"
    "fixture/DescribeInstances.yaml"

requestDescribeLayers :: DescribeLayers -> TestTree
requestDescribeLayers =
  req
    "DescribeLayers"
    "fixture/DescribeLayers.yaml"

requestDescribeLoadBasedAutoScaling :: DescribeLoadBasedAutoScaling -> TestTree
requestDescribeLoadBasedAutoScaling =
  req
    "DescribeLoadBasedAutoScaling"
    "fixture/DescribeLoadBasedAutoScaling.yaml"

requestDescribeMyUserProfile :: DescribeMyUserProfile -> TestTree
requestDescribeMyUserProfile =
  req
    "DescribeMyUserProfile"
    "fixture/DescribeMyUserProfile.yaml"

requestDescribeOperatingSystems :: DescribeOperatingSystems -> TestTree
requestDescribeOperatingSystems =
  req
    "DescribeOperatingSystems"
    "fixture/DescribeOperatingSystems.yaml"

requestDescribePermissions :: DescribePermissions -> TestTree
requestDescribePermissions =
  req
    "DescribePermissions"
    "fixture/DescribePermissions.yaml"

requestDescribeRaidArrays :: DescribeRaidArrays -> TestTree
requestDescribeRaidArrays =
  req
    "DescribeRaidArrays"
    "fixture/DescribeRaidArrays.yaml"

requestDescribeRdsDbInstances :: DescribeRdsDbInstances -> TestTree
requestDescribeRdsDbInstances =
  req
    "DescribeRdsDbInstances"
    "fixture/DescribeRdsDbInstances.yaml"

requestDescribeServiceErrors :: DescribeServiceErrors -> TestTree
requestDescribeServiceErrors =
  req
    "DescribeServiceErrors"
    "fixture/DescribeServiceErrors.yaml"

requestDescribeStackProvisioningParameters :: DescribeStackProvisioningParameters -> TestTree
requestDescribeStackProvisioningParameters =
  req
    "DescribeStackProvisioningParameters"
    "fixture/DescribeStackProvisioningParameters.yaml"

requestDescribeStackSummary :: DescribeStackSummary -> TestTree
requestDescribeStackSummary =
  req
    "DescribeStackSummary"
    "fixture/DescribeStackSummary.yaml"

requestDescribeStacks :: DescribeStacks -> TestTree
requestDescribeStacks =
  req
    "DescribeStacks"
    "fixture/DescribeStacks.yaml"

requestDescribeTimeBasedAutoScaling :: DescribeTimeBasedAutoScaling -> TestTree
requestDescribeTimeBasedAutoScaling =
  req
    "DescribeTimeBasedAutoScaling"
    "fixture/DescribeTimeBasedAutoScaling.yaml"

requestDescribeUserProfiles :: DescribeUserProfiles -> TestTree
requestDescribeUserProfiles =
  req
    "DescribeUserProfiles"
    "fixture/DescribeUserProfiles.yaml"

requestDescribeVolumes :: DescribeVolumes -> TestTree
requestDescribeVolumes =
  req
    "DescribeVolumes"
    "fixture/DescribeVolumes.yaml"

requestDetachElasticLoadBalancer :: DetachElasticLoadBalancer -> TestTree
requestDetachElasticLoadBalancer =
  req
    "DetachElasticLoadBalancer"
    "fixture/DetachElasticLoadBalancer.yaml"

requestDisassociateElasticIp :: DisassociateElasticIp -> TestTree
requestDisassociateElasticIp =
  req
    "DisassociateElasticIp"
    "fixture/DisassociateElasticIp.yaml"

requestGetHostnameSuggestion :: GetHostnameSuggestion -> TestTree
requestGetHostnameSuggestion =
  req
    "GetHostnameSuggestion"
    "fixture/GetHostnameSuggestion.yaml"

requestGrantAccess :: GrantAccess -> TestTree
requestGrantAccess =
  req
    "GrantAccess"
    "fixture/GrantAccess.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestRebootInstance :: RebootInstance -> TestTree
requestRebootInstance =
  req
    "RebootInstance"
    "fixture/RebootInstance.yaml"

requestRegisterEcsCluster :: RegisterEcsCluster -> TestTree
requestRegisterEcsCluster =
  req
    "RegisterEcsCluster"
    "fixture/RegisterEcsCluster.yaml"

requestRegisterElasticIp :: RegisterElasticIp -> TestTree
requestRegisterElasticIp =
  req
    "RegisterElasticIp"
    "fixture/RegisterElasticIp.yaml"

requestRegisterInstance :: RegisterInstance -> TestTree
requestRegisterInstance =
  req
    "RegisterInstance"
    "fixture/RegisterInstance.yaml"

requestRegisterRdsDbInstance :: RegisterRdsDbInstance -> TestTree
requestRegisterRdsDbInstance =
  req
    "RegisterRdsDbInstance"
    "fixture/RegisterRdsDbInstance.yaml"

requestRegisterVolume :: RegisterVolume -> TestTree
requestRegisterVolume =
  req
    "RegisterVolume"
    "fixture/RegisterVolume.yaml"

requestSetLoadBasedAutoScaling :: SetLoadBasedAutoScaling -> TestTree
requestSetLoadBasedAutoScaling =
  req
    "SetLoadBasedAutoScaling"
    "fixture/SetLoadBasedAutoScaling.yaml"

requestSetPermission :: SetPermission -> TestTree
requestSetPermission =
  req
    "SetPermission"
    "fixture/SetPermission.yaml"

requestSetTimeBasedAutoScaling :: SetTimeBasedAutoScaling -> TestTree
requestSetTimeBasedAutoScaling =
  req
    "SetTimeBasedAutoScaling"
    "fixture/SetTimeBasedAutoScaling.yaml"

requestStartInstance :: StartInstance -> TestTree
requestStartInstance =
  req
    "StartInstance"
    "fixture/StartInstance.yaml"

requestStartStack :: StartStack -> TestTree
requestStartStack =
  req
    "StartStack"
    "fixture/StartStack.yaml"

requestStopInstance :: StopInstance -> TestTree
requestStopInstance =
  req
    "StopInstance"
    "fixture/StopInstance.yaml"

requestStopStack :: StopStack -> TestTree
requestStopStack =
  req
    "StopStack"
    "fixture/StopStack.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUnassignInstance :: UnassignInstance -> TestTree
requestUnassignInstance =
  req
    "UnassignInstance"
    "fixture/UnassignInstance.yaml"

requestUnassignVolume :: UnassignVolume -> TestTree
requestUnassignVolume =
  req
    "UnassignVolume"
    "fixture/UnassignVolume.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateApp :: UpdateApp -> TestTree
requestUpdateApp =
  req
    "UpdateApp"
    "fixture/UpdateApp.yaml"

requestUpdateElasticIp :: UpdateElasticIp -> TestTree
requestUpdateElasticIp =
  req
    "UpdateElasticIp"
    "fixture/UpdateElasticIp.yaml"

requestUpdateInstance :: UpdateInstance -> TestTree
requestUpdateInstance =
  req
    "UpdateInstance"
    "fixture/UpdateInstance.yaml"

requestUpdateLayer :: UpdateLayer -> TestTree
requestUpdateLayer =
  req
    "UpdateLayer"
    "fixture/UpdateLayer.yaml"

requestUpdateMyUserProfile :: UpdateMyUserProfile -> TestTree
requestUpdateMyUserProfile =
  req
    "UpdateMyUserProfile"
    "fixture/UpdateMyUserProfile.yaml"

requestUpdateRdsDbInstance :: UpdateRdsDbInstance -> TestTree
requestUpdateRdsDbInstance =
  req
    "UpdateRdsDbInstance"
    "fixture/UpdateRdsDbInstance.yaml"

requestUpdateStack :: UpdateStack -> TestTree
requestUpdateStack =
  req
    "UpdateStack"
    "fixture/UpdateStack.yaml"

requestUpdateUserProfile :: UpdateUserProfile -> TestTree
requestUpdateUserProfile =
  req
    "UpdateUserProfile"
    "fixture/UpdateUserProfile.yaml"

requestUpdateVolume :: UpdateVolume -> TestTree
requestUpdateVolume =
  req
    "UpdateVolume"
    "fixture/UpdateVolume.yaml"

-- Responses

responseAssignInstance :: AssignInstanceResponse -> TestTree
responseAssignInstance =
  res
    "AssignInstanceResponse"
    "fixture/AssignInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssignInstance)

responseAssignVolume :: AssignVolumeResponse -> TestTree
responseAssignVolume =
  res
    "AssignVolumeResponse"
    "fixture/AssignVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssignVolume)

responseAssociateElasticIp :: AssociateElasticIpResponse -> TestTree
responseAssociateElasticIp =
  res
    "AssociateElasticIpResponse"
    "fixture/AssociateElasticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateElasticIp)

responseAttachElasticLoadBalancer :: AttachElasticLoadBalancerResponse -> TestTree
responseAttachElasticLoadBalancer =
  res
    "AttachElasticLoadBalancerResponse"
    "fixture/AttachElasticLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachElasticLoadBalancer)

responseCloneStack :: CloneStackResponse -> TestTree
responseCloneStack =
  res
    "CloneStackResponse"
    "fixture/CloneStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CloneStack)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApp)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeployment)

responseCreateInstance :: CreateInstanceResponse -> TestTree
responseCreateInstance =
  res
    "CreateInstanceResponse"
    "fixture/CreateInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstance)

responseCreateLayer :: CreateLayerResponse -> TestTree
responseCreateLayer =
  res
    "CreateLayerResponse"
    "fixture/CreateLayerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLayer)

responseCreateStack :: CreateStackResponse -> TestTree
responseCreateStack =
  res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStack)

responseCreateUserProfile :: CreateUserProfileResponse -> TestTree
responseCreateUserProfile =
  res
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserProfile)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApp)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance =
  res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInstance)

responseDeleteLayer :: DeleteLayerResponse -> TestTree
responseDeleteLayer =
  res
    "DeleteLayerResponse"
    "fixture/DeleteLayerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLayer)

responseDeleteStack :: DeleteStackResponse -> TestTree
responseDeleteStack =
  res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStack)

responseDeleteUserProfile :: DeleteUserProfileResponse -> TestTree
responseDeleteUserProfile =
  res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserProfile)

responseDeregisterEcsCluster :: DeregisterEcsClusterResponse -> TestTree
responseDeregisterEcsCluster =
  res
    "DeregisterEcsClusterResponse"
    "fixture/DeregisterEcsClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterEcsCluster)

responseDeregisterElasticIp :: DeregisterElasticIpResponse -> TestTree
responseDeregisterElasticIp =
  res
    "DeregisterElasticIpResponse"
    "fixture/DeregisterElasticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterElasticIp)

responseDeregisterInstance :: DeregisterInstanceResponse -> TestTree
responseDeregisterInstance =
  res
    "DeregisterInstanceResponse"
    "fixture/DeregisterInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterInstance)

responseDeregisterRdsDbInstance :: DeregisterRdsDbInstanceResponse -> TestTree
responseDeregisterRdsDbInstance =
  res
    "DeregisterRdsDbInstanceResponse"
    "fixture/DeregisterRdsDbInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterRdsDbInstance)

responseDeregisterVolume :: DeregisterVolumeResponse -> TestTree
responseDeregisterVolume =
  res
    "DeregisterVolumeResponse"
    "fixture/DeregisterVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterVolume)

responseDescribeAgentVersions :: DescribeAgentVersionsResponse -> TestTree
responseDescribeAgentVersions =
  res
    "DescribeAgentVersionsResponse"
    "fixture/DescribeAgentVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAgentVersions)

responseDescribeApps :: DescribeAppsResponse -> TestTree
responseDescribeApps =
  res
    "DescribeAppsResponse"
    "fixture/DescribeAppsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApps)

responseDescribeCommands :: DescribeCommandsResponse -> TestTree
responseDescribeCommands =
  res
    "DescribeCommandsResponse"
    "fixture/DescribeCommandsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCommands)

responseDescribeDeployments :: DescribeDeploymentsResponse -> TestTree
responseDescribeDeployments =
  res
    "DescribeDeploymentsResponse"
    "fixture/DescribeDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDeployments)

responseDescribeEcsClusters :: DescribeEcsClustersResponse -> TestTree
responseDescribeEcsClusters =
  res
    "DescribeEcsClustersResponse"
    "fixture/DescribeEcsClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEcsClusters)

responseDescribeElasticIps :: DescribeElasticIpsResponse -> TestTree
responseDescribeElasticIps =
  res
    "DescribeElasticIpsResponse"
    "fixture/DescribeElasticIpsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeElasticIps)

responseDescribeElasticLoadBalancers :: DescribeElasticLoadBalancersResponse -> TestTree
responseDescribeElasticLoadBalancers =
  res
    "DescribeElasticLoadBalancersResponse"
    "fixture/DescribeElasticLoadBalancersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeElasticLoadBalancers)

responseDescribeInstances :: DescribeInstancesResponse -> TestTree
responseDescribeInstances =
  res
    "DescribeInstancesResponse"
    "fixture/DescribeInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstances)

responseDescribeLayers :: DescribeLayersResponse -> TestTree
responseDescribeLayers =
  res
    "DescribeLayersResponse"
    "fixture/DescribeLayersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLayers)

responseDescribeLoadBasedAutoScaling :: DescribeLoadBasedAutoScalingResponse -> TestTree
responseDescribeLoadBasedAutoScaling =
  res
    "DescribeLoadBasedAutoScalingResponse"
    "fixture/DescribeLoadBasedAutoScalingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoadBasedAutoScaling)

responseDescribeMyUserProfile :: DescribeMyUserProfileResponse -> TestTree
responseDescribeMyUserProfile =
  res
    "DescribeMyUserProfileResponse"
    "fixture/DescribeMyUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMyUserProfile)

responseDescribeOperatingSystems :: DescribeOperatingSystemsResponse -> TestTree
responseDescribeOperatingSystems =
  res
    "DescribeOperatingSystemsResponse"
    "fixture/DescribeOperatingSystemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOperatingSystems)

responseDescribePermissions :: DescribePermissionsResponse -> TestTree
responseDescribePermissions =
  res
    "DescribePermissionsResponse"
    "fixture/DescribePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePermissions)

responseDescribeRaidArrays :: DescribeRaidArraysResponse -> TestTree
responseDescribeRaidArrays =
  res
    "DescribeRaidArraysResponse"
    "fixture/DescribeRaidArraysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRaidArrays)

responseDescribeRdsDbInstances :: DescribeRdsDbInstancesResponse -> TestTree
responseDescribeRdsDbInstances =
  res
    "DescribeRdsDbInstancesResponse"
    "fixture/DescribeRdsDbInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRdsDbInstances)

responseDescribeServiceErrors :: DescribeServiceErrorsResponse -> TestTree
responseDescribeServiceErrors =
  res
    "DescribeServiceErrorsResponse"
    "fixture/DescribeServiceErrorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServiceErrors)

responseDescribeStackProvisioningParameters :: DescribeStackProvisioningParametersResponse -> TestTree
responseDescribeStackProvisioningParameters =
  res
    "DescribeStackProvisioningParametersResponse"
    "fixture/DescribeStackProvisioningParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackProvisioningParameters)

responseDescribeStackSummary :: DescribeStackSummaryResponse -> TestTree
responseDescribeStackSummary =
  res
    "DescribeStackSummaryResponse"
    "fixture/DescribeStackSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStackSummary)

responseDescribeStacks :: DescribeStacksResponse -> TestTree
responseDescribeStacks =
  res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStacks)

responseDescribeTimeBasedAutoScaling :: DescribeTimeBasedAutoScalingResponse -> TestTree
responseDescribeTimeBasedAutoScaling =
  res
    "DescribeTimeBasedAutoScalingResponse"
    "fixture/DescribeTimeBasedAutoScalingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTimeBasedAutoScaling)

responseDescribeUserProfiles :: DescribeUserProfilesResponse -> TestTree
responseDescribeUserProfiles =
  res
    "DescribeUserProfilesResponse"
    "fixture/DescribeUserProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserProfiles)

responseDescribeVolumes :: DescribeVolumesResponse -> TestTree
responseDescribeVolumes =
  res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVolumes)

responseDetachElasticLoadBalancer :: DetachElasticLoadBalancerResponse -> TestTree
responseDetachElasticLoadBalancer =
  res
    "DetachElasticLoadBalancerResponse"
    "fixture/DetachElasticLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachElasticLoadBalancer)

responseDisassociateElasticIp :: DisassociateElasticIpResponse -> TestTree
responseDisassociateElasticIp =
  res
    "DisassociateElasticIpResponse"
    "fixture/DisassociateElasticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateElasticIp)

responseGetHostnameSuggestion :: GetHostnameSuggestionResponse -> TestTree
responseGetHostnameSuggestion =
  res
    "GetHostnameSuggestionResponse"
    "fixture/GetHostnameSuggestionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHostnameSuggestion)

responseGrantAccess :: GrantAccessResponse -> TestTree
responseGrantAccess =
  res
    "GrantAccessResponse"
    "fixture/GrantAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GrantAccess)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responseRebootInstance :: RebootInstanceResponse -> TestTree
responseRebootInstance =
  res
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootInstance)

responseRegisterEcsCluster :: RegisterEcsClusterResponse -> TestTree
responseRegisterEcsCluster =
  res
    "RegisterEcsClusterResponse"
    "fixture/RegisterEcsClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterEcsCluster)

responseRegisterElasticIp :: RegisterElasticIpResponse -> TestTree
responseRegisterElasticIp =
  res
    "RegisterElasticIpResponse"
    "fixture/RegisterElasticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterElasticIp)

responseRegisterInstance :: RegisterInstanceResponse -> TestTree
responseRegisterInstance =
  res
    "RegisterInstanceResponse"
    "fixture/RegisterInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterInstance)

responseRegisterRdsDbInstance :: RegisterRdsDbInstanceResponse -> TestTree
responseRegisterRdsDbInstance =
  res
    "RegisterRdsDbInstanceResponse"
    "fixture/RegisterRdsDbInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterRdsDbInstance)

responseRegisterVolume :: RegisterVolumeResponse -> TestTree
responseRegisterVolume =
  res
    "RegisterVolumeResponse"
    "fixture/RegisterVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterVolume)

responseSetLoadBasedAutoScaling :: SetLoadBasedAutoScalingResponse -> TestTree
responseSetLoadBasedAutoScaling =
  res
    "SetLoadBasedAutoScalingResponse"
    "fixture/SetLoadBasedAutoScalingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetLoadBasedAutoScaling)

responseSetPermission :: SetPermissionResponse -> TestTree
responseSetPermission =
  res
    "SetPermissionResponse"
    "fixture/SetPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetPermission)

responseSetTimeBasedAutoScaling :: SetTimeBasedAutoScalingResponse -> TestTree
responseSetTimeBasedAutoScaling =
  res
    "SetTimeBasedAutoScalingResponse"
    "fixture/SetTimeBasedAutoScalingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetTimeBasedAutoScaling)

responseStartInstance :: StartInstanceResponse -> TestTree
responseStartInstance =
  res
    "StartInstanceResponse"
    "fixture/StartInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartInstance)

responseStartStack :: StartStackResponse -> TestTree
responseStartStack =
  res
    "StartStackResponse"
    "fixture/StartStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartStack)

responseStopInstance :: StopInstanceResponse -> TestTree
responseStopInstance =
  res
    "StopInstanceResponse"
    "fixture/StopInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopInstance)

responseStopStack :: StopStackResponse -> TestTree
responseStopStack =
  res
    "StopStackResponse"
    "fixture/StopStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopStack)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUnassignInstance :: UnassignInstanceResponse -> TestTree
responseUnassignInstance =
  res
    "UnassignInstanceResponse"
    "fixture/UnassignInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnassignInstance)

responseUnassignVolume :: UnassignVolumeResponse -> TestTree
responseUnassignVolume =
  res
    "UnassignVolumeResponse"
    "fixture/UnassignVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnassignVolume)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateApp :: UpdateAppResponse -> TestTree
responseUpdateApp =
  res
    "UpdateAppResponse"
    "fixture/UpdateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApp)

responseUpdateElasticIp :: UpdateElasticIpResponse -> TestTree
responseUpdateElasticIp =
  res
    "UpdateElasticIpResponse"
    "fixture/UpdateElasticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateElasticIp)

responseUpdateInstance :: UpdateInstanceResponse -> TestTree
responseUpdateInstance =
  res
    "UpdateInstanceResponse"
    "fixture/UpdateInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInstance)

responseUpdateLayer :: UpdateLayerResponse -> TestTree
responseUpdateLayer =
  res
    "UpdateLayerResponse"
    "fixture/UpdateLayerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLayer)

responseUpdateMyUserProfile :: UpdateMyUserProfileResponse -> TestTree
responseUpdateMyUserProfile =
  res
    "UpdateMyUserProfileResponse"
    "fixture/UpdateMyUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMyUserProfile)

responseUpdateRdsDbInstance :: UpdateRdsDbInstanceResponse -> TestTree
responseUpdateRdsDbInstance =
  res
    "UpdateRdsDbInstanceResponse"
    "fixture/UpdateRdsDbInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRdsDbInstance)

responseUpdateStack :: UpdateStackResponse -> TestTree
responseUpdateStack =
  res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStack)

responseUpdateUserProfile :: UpdateUserProfileResponse -> TestTree
responseUpdateUserProfile =
  res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserProfile)

responseUpdateVolume :: UpdateVolumeResponse -> TestTree
responseUpdateVolume =
  res
    "UpdateVolumeResponse"
    "fixture/UpdateVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVolume)
