{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.EKS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.EKS where

import Amazonka.EKS
import qualified Data.Proxy as Proxy
import Test.Amazonka.EKS.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateEncryptionConfig $
--             newAssociateEncryptionConfig
--
--         , requestAssociateIdentityProviderConfig $
--             newAssociateIdentityProviderConfig
--
--         , requestCreateAddon $
--             newCreateAddon
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestCreateFargateProfile $
--             newCreateFargateProfile
--
--         , requestCreateNodegroup $
--             newCreateNodegroup
--
--         , requestDeleteAddon $
--             newDeleteAddon
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestDeleteFargateProfile $
--             newDeleteFargateProfile
--
--         , requestDeleteNodegroup $
--             newDeleteNodegroup
--
--         , requestDeregisterCluster $
--             newDeregisterCluster
--
--         , requestDescribeAddon $
--             newDescribeAddon
--
--         , requestDescribeAddonConfiguration $
--             newDescribeAddonConfiguration
--
--         , requestDescribeAddonVersions $
--             newDescribeAddonVersions
--
--         , requestDescribeCluster $
--             newDescribeCluster
--
--         , requestDescribeFargateProfile $
--             newDescribeFargateProfile
--
--         , requestDescribeIdentityProviderConfig $
--             newDescribeIdentityProviderConfig
--
--         , requestDescribeNodegroup $
--             newDescribeNodegroup
--
--         , requestDescribeUpdate $
--             newDescribeUpdate
--
--         , requestDisassociateIdentityProviderConfig $
--             newDisassociateIdentityProviderConfig
--
--         , requestListAddons $
--             newListAddons
--
--         , requestListClusters $
--             newListClusters
--
--         , requestListFargateProfiles $
--             newListFargateProfiles
--
--         , requestListIdentityProviderConfigs $
--             newListIdentityProviderConfigs
--
--         , requestListNodegroups $
--             newListNodegroups
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListUpdates $
--             newListUpdates
--
--         , requestRegisterCluster $
--             newRegisterCluster
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAddon $
--             newUpdateAddon
--
--         , requestUpdateClusterConfig $
--             newUpdateClusterConfig
--
--         , requestUpdateClusterVersion $
--             newUpdateClusterVersion
--
--         , requestUpdateNodegroupConfig $
--             newUpdateNodegroupConfig
--
--         , requestUpdateNodegroupVersion $
--             newUpdateNodegroupVersion
--
--           ]

--     , testGroup "response"
--         [ responseAssociateEncryptionConfig $
--             newAssociateEncryptionConfigResponse
--
--         , responseAssociateIdentityProviderConfig $
--             newAssociateIdentityProviderConfigResponse
--
--         , responseCreateAddon $
--             newCreateAddonResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseCreateFargateProfile $
--             newCreateFargateProfileResponse
--
--         , responseCreateNodegroup $
--             newCreateNodegroupResponse
--
--         , responseDeleteAddon $
--             newDeleteAddonResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseDeleteFargateProfile $
--             newDeleteFargateProfileResponse
--
--         , responseDeleteNodegroup $
--             newDeleteNodegroupResponse
--
--         , responseDeregisterCluster $
--             newDeregisterClusterResponse
--
--         , responseDescribeAddon $
--             newDescribeAddonResponse
--
--         , responseDescribeAddonConfiguration $
--             newDescribeAddonConfigurationResponse
--
--         , responseDescribeAddonVersions $
--             newDescribeAddonVersionsResponse
--
--         , responseDescribeCluster $
--             newDescribeClusterResponse
--
--         , responseDescribeFargateProfile $
--             newDescribeFargateProfileResponse
--
--         , responseDescribeIdentityProviderConfig $
--             newDescribeIdentityProviderConfigResponse
--
--         , responseDescribeNodegroup $
--             newDescribeNodegroupResponse
--
--         , responseDescribeUpdate $
--             newDescribeUpdateResponse
--
--         , responseDisassociateIdentityProviderConfig $
--             newDisassociateIdentityProviderConfigResponse
--
--         , responseListAddons $
--             newListAddonsResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseListFargateProfiles $
--             newListFargateProfilesResponse
--
--         , responseListIdentityProviderConfigs $
--             newListIdentityProviderConfigsResponse
--
--         , responseListNodegroups $
--             newListNodegroupsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListUpdates $
--             newListUpdatesResponse
--
--         , responseRegisterCluster $
--             newRegisterClusterResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAddon $
--             newUpdateAddonResponse
--
--         , responseUpdateClusterConfig $
--             newUpdateClusterConfigResponse
--
--         , responseUpdateClusterVersion $
--             newUpdateClusterVersionResponse
--
--         , responseUpdateNodegroupConfig $
--             newUpdateNodegroupConfigResponse
--
--         , responseUpdateNodegroupVersion $
--             newUpdateNodegroupVersionResponse
--
--           ]
--     ]

-- Requests

requestAssociateEncryptionConfig :: AssociateEncryptionConfig -> TestTree
requestAssociateEncryptionConfig =
  req
    "AssociateEncryptionConfig"
    "fixture/AssociateEncryptionConfig.yaml"

requestAssociateIdentityProviderConfig :: AssociateIdentityProviderConfig -> TestTree
requestAssociateIdentityProviderConfig =
  req
    "AssociateIdentityProviderConfig"
    "fixture/AssociateIdentityProviderConfig.yaml"

requestCreateAddon :: CreateAddon -> TestTree
requestCreateAddon =
  req
    "CreateAddon"
    "fixture/CreateAddon.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestCreateFargateProfile :: CreateFargateProfile -> TestTree
requestCreateFargateProfile =
  req
    "CreateFargateProfile"
    "fixture/CreateFargateProfile.yaml"

requestCreateNodegroup :: CreateNodegroup -> TestTree
requestCreateNodegroup =
  req
    "CreateNodegroup"
    "fixture/CreateNodegroup.yaml"

requestDeleteAddon :: DeleteAddon -> TestTree
requestDeleteAddon =
  req
    "DeleteAddon"
    "fixture/DeleteAddon.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestDeleteFargateProfile :: DeleteFargateProfile -> TestTree
requestDeleteFargateProfile =
  req
    "DeleteFargateProfile"
    "fixture/DeleteFargateProfile.yaml"

requestDeleteNodegroup :: DeleteNodegroup -> TestTree
requestDeleteNodegroup =
  req
    "DeleteNodegroup"
    "fixture/DeleteNodegroup.yaml"

requestDeregisterCluster :: DeregisterCluster -> TestTree
requestDeregisterCluster =
  req
    "DeregisterCluster"
    "fixture/DeregisterCluster.yaml"

requestDescribeAddon :: DescribeAddon -> TestTree
requestDescribeAddon =
  req
    "DescribeAddon"
    "fixture/DescribeAddon.yaml"

requestDescribeAddonConfiguration :: DescribeAddonConfiguration -> TestTree
requestDescribeAddonConfiguration =
  req
    "DescribeAddonConfiguration"
    "fixture/DescribeAddonConfiguration.yaml"

requestDescribeAddonVersions :: DescribeAddonVersions -> TestTree
requestDescribeAddonVersions =
  req
    "DescribeAddonVersions"
    "fixture/DescribeAddonVersions.yaml"

requestDescribeCluster :: DescribeCluster -> TestTree
requestDescribeCluster =
  req
    "DescribeCluster"
    "fixture/DescribeCluster.yaml"

requestDescribeFargateProfile :: DescribeFargateProfile -> TestTree
requestDescribeFargateProfile =
  req
    "DescribeFargateProfile"
    "fixture/DescribeFargateProfile.yaml"

requestDescribeIdentityProviderConfig :: DescribeIdentityProviderConfig -> TestTree
requestDescribeIdentityProviderConfig =
  req
    "DescribeIdentityProviderConfig"
    "fixture/DescribeIdentityProviderConfig.yaml"

requestDescribeNodegroup :: DescribeNodegroup -> TestTree
requestDescribeNodegroup =
  req
    "DescribeNodegroup"
    "fixture/DescribeNodegroup.yaml"

requestDescribeUpdate :: DescribeUpdate -> TestTree
requestDescribeUpdate =
  req
    "DescribeUpdate"
    "fixture/DescribeUpdate.yaml"

requestDisassociateIdentityProviderConfig :: DisassociateIdentityProviderConfig -> TestTree
requestDisassociateIdentityProviderConfig =
  req
    "DisassociateIdentityProviderConfig"
    "fixture/DisassociateIdentityProviderConfig.yaml"

requestListAddons :: ListAddons -> TestTree
requestListAddons =
  req
    "ListAddons"
    "fixture/ListAddons.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestListFargateProfiles :: ListFargateProfiles -> TestTree
requestListFargateProfiles =
  req
    "ListFargateProfiles"
    "fixture/ListFargateProfiles.yaml"

requestListIdentityProviderConfigs :: ListIdentityProviderConfigs -> TestTree
requestListIdentityProviderConfigs =
  req
    "ListIdentityProviderConfigs"
    "fixture/ListIdentityProviderConfigs.yaml"

requestListNodegroups :: ListNodegroups -> TestTree
requestListNodegroups =
  req
    "ListNodegroups"
    "fixture/ListNodegroups.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListUpdates :: ListUpdates -> TestTree
requestListUpdates =
  req
    "ListUpdates"
    "fixture/ListUpdates.yaml"

requestRegisterCluster :: RegisterCluster -> TestTree
requestRegisterCluster =
  req
    "RegisterCluster"
    "fixture/RegisterCluster.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateAddon :: UpdateAddon -> TestTree
requestUpdateAddon =
  req
    "UpdateAddon"
    "fixture/UpdateAddon.yaml"

requestUpdateClusterConfig :: UpdateClusterConfig -> TestTree
requestUpdateClusterConfig =
  req
    "UpdateClusterConfig"
    "fixture/UpdateClusterConfig.yaml"

requestUpdateClusterVersion :: UpdateClusterVersion -> TestTree
requestUpdateClusterVersion =
  req
    "UpdateClusterVersion"
    "fixture/UpdateClusterVersion.yaml"

requestUpdateNodegroupConfig :: UpdateNodegroupConfig -> TestTree
requestUpdateNodegroupConfig =
  req
    "UpdateNodegroupConfig"
    "fixture/UpdateNodegroupConfig.yaml"

requestUpdateNodegroupVersion :: UpdateNodegroupVersion -> TestTree
requestUpdateNodegroupVersion =
  req
    "UpdateNodegroupVersion"
    "fixture/UpdateNodegroupVersion.yaml"

-- Responses

responseAssociateEncryptionConfig :: AssociateEncryptionConfigResponse -> TestTree
responseAssociateEncryptionConfig =
  res
    "AssociateEncryptionConfigResponse"
    "fixture/AssociateEncryptionConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateEncryptionConfig)

responseAssociateIdentityProviderConfig :: AssociateIdentityProviderConfigResponse -> TestTree
responseAssociateIdentityProviderConfig =
  res
    "AssociateIdentityProviderConfigResponse"
    "fixture/AssociateIdentityProviderConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateIdentityProviderConfig)

responseCreateAddon :: CreateAddonResponse -> TestTree
responseCreateAddon =
  res
    "CreateAddonResponse"
    "fixture/CreateAddonResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAddon)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCluster)

responseCreateFargateProfile :: CreateFargateProfileResponse -> TestTree
responseCreateFargateProfile =
  res
    "CreateFargateProfileResponse"
    "fixture/CreateFargateProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFargateProfile)

responseCreateNodegroup :: CreateNodegroupResponse -> TestTree
responseCreateNodegroup =
  res
    "CreateNodegroupResponse"
    "fixture/CreateNodegroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNodegroup)

responseDeleteAddon :: DeleteAddonResponse -> TestTree
responseDeleteAddon =
  res
    "DeleteAddonResponse"
    "fixture/DeleteAddonResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAddon)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCluster)

responseDeleteFargateProfile :: DeleteFargateProfileResponse -> TestTree
responseDeleteFargateProfile =
  res
    "DeleteFargateProfileResponse"
    "fixture/DeleteFargateProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFargateProfile)

responseDeleteNodegroup :: DeleteNodegroupResponse -> TestTree
responseDeleteNodegroup =
  res
    "DeleteNodegroupResponse"
    "fixture/DeleteNodegroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNodegroup)

responseDeregisterCluster :: DeregisterClusterResponse -> TestTree
responseDeregisterCluster =
  res
    "DeregisterClusterResponse"
    "fixture/DeregisterClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterCluster)

responseDescribeAddon :: DescribeAddonResponse -> TestTree
responseDescribeAddon =
  res
    "DescribeAddonResponse"
    "fixture/DescribeAddonResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAddon)

responseDescribeAddonConfiguration :: DescribeAddonConfigurationResponse -> TestTree
responseDescribeAddonConfiguration =
  res
    "DescribeAddonConfigurationResponse"
    "fixture/DescribeAddonConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAddonConfiguration)

responseDescribeAddonVersions :: DescribeAddonVersionsResponse -> TestTree
responseDescribeAddonVersions =
  res
    "DescribeAddonVersionsResponse"
    "fixture/DescribeAddonVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAddonVersions)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster =
  res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCluster)

responseDescribeFargateProfile :: DescribeFargateProfileResponse -> TestTree
responseDescribeFargateProfile =
  res
    "DescribeFargateProfileResponse"
    "fixture/DescribeFargateProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFargateProfile)

responseDescribeIdentityProviderConfig :: DescribeIdentityProviderConfigResponse -> TestTree
responseDescribeIdentityProviderConfig =
  res
    "DescribeIdentityProviderConfigResponse"
    "fixture/DescribeIdentityProviderConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIdentityProviderConfig)

responseDescribeNodegroup :: DescribeNodegroupResponse -> TestTree
responseDescribeNodegroup =
  res
    "DescribeNodegroupResponse"
    "fixture/DescribeNodegroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNodegroup)

responseDescribeUpdate :: DescribeUpdateResponse -> TestTree
responseDescribeUpdate =
  res
    "DescribeUpdateResponse"
    "fixture/DescribeUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUpdate)

responseDisassociateIdentityProviderConfig :: DisassociateIdentityProviderConfigResponse -> TestTree
responseDisassociateIdentityProviderConfig =
  res
    "DisassociateIdentityProviderConfigResponse"
    "fixture/DisassociateIdentityProviderConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateIdentityProviderConfig)

responseListAddons :: ListAddonsResponse -> TestTree
responseListAddons =
  res
    "ListAddonsResponse"
    "fixture/ListAddonsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAddons)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClusters)

responseListFargateProfiles :: ListFargateProfilesResponse -> TestTree
responseListFargateProfiles =
  res
    "ListFargateProfilesResponse"
    "fixture/ListFargateProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFargateProfiles)

responseListIdentityProviderConfigs :: ListIdentityProviderConfigsResponse -> TestTree
responseListIdentityProviderConfigs =
  res
    "ListIdentityProviderConfigsResponse"
    "fixture/ListIdentityProviderConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIdentityProviderConfigs)

responseListNodegroups :: ListNodegroupsResponse -> TestTree
responseListNodegroups =
  res
    "ListNodegroupsResponse"
    "fixture/ListNodegroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNodegroups)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListUpdates :: ListUpdatesResponse -> TestTree
responseListUpdates =
  res
    "ListUpdatesResponse"
    "fixture/ListUpdatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUpdates)

responseRegisterCluster :: RegisterClusterResponse -> TestTree
responseRegisterCluster =
  res
    "RegisterClusterResponse"
    "fixture/RegisterClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterCluster)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateAddon :: UpdateAddonResponse -> TestTree
responseUpdateAddon =
  res
    "UpdateAddonResponse"
    "fixture/UpdateAddonResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAddon)

responseUpdateClusterConfig :: UpdateClusterConfigResponse -> TestTree
responseUpdateClusterConfig =
  res
    "UpdateClusterConfigResponse"
    "fixture/UpdateClusterConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateClusterConfig)

responseUpdateClusterVersion :: UpdateClusterVersionResponse -> TestTree
responseUpdateClusterVersion =
  res
    "UpdateClusterVersionResponse"
    "fixture/UpdateClusterVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateClusterVersion)

responseUpdateNodegroupConfig :: UpdateNodegroupConfigResponse -> TestTree
responseUpdateNodegroupConfig =
  res
    "UpdateNodegroupConfigResponse"
    "fixture/UpdateNodegroupConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNodegroupConfig)

responseUpdateNodegroupVersion :: UpdateNodegroupVersionResponse -> TestTree
responseUpdateNodegroupVersion =
  res
    "UpdateNodegroupVersionResponse"
    "fixture/UpdateNodegroupVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNodegroupVersion)
