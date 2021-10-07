{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EKS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.EKS where

import Data.Proxy
import Network.AWS.EKS
import Test.AWS.EKS.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeUpdate $
--             newDescribeUpdate
--
--         , requestDescribeFargateProfile $
--             newDescribeFargateProfile
--
--         , requestDescribeAddonVersions $
--             newDescribeAddonVersions
--
--         , requestListIdentityProviderConfigs $
--             newListIdentityProviderConfigs
--
--         , requestListAddons $
--             newListAddons
--
--         , requestAssociateIdentityProviderConfig $
--             newAssociateIdentityProviderConfig
--
--         , requestUpdateClusterVersion $
--             newUpdateClusterVersion
--
--         , requestDeleteAddon $
--             newDeleteAddon
--
--         , requestAssociateEncryptionConfig $
--             newAssociateEncryptionConfig
--
--         , requestUpdateAddon $
--             newUpdateAddon
--
--         , requestListNodegroups $
--             newListNodegroups
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestRegisterCluster $
--             newRegisterCluster
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateNodegroup $
--             newCreateNodegroup
--
--         , requestDescribeIdentityProviderConfig $
--             newDescribeIdentityProviderConfig
--
--         , requestDeleteFargateProfile $
--             newDeleteFargateProfile
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListFargateProfiles $
--             newListFargateProfiles
--
--         , requestListUpdates $
--             newListUpdates
--
--         , requestDisassociateIdentityProviderConfig $
--             newDisassociateIdentityProviderConfig
--
--         , requestDescribeCluster $
--             newDescribeCluster
--
--         , requestUpdateNodegroupVersion $
--             newUpdateNodegroupVersion
--
--         , requestCreateAddon $
--             newCreateAddon
--
--         , requestUpdateNodegroupConfig $
--             newUpdateNodegroupConfig
--
--         , requestUpdateClusterConfig $
--             newUpdateClusterConfig
--
--         , requestDeleteNodegroup $
--             newDeleteNodegroup
--
--         , requestListClusters $
--             newListClusters
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestCreateFargateProfile $
--             newCreateFargateProfile
--
--         , requestDescribeAddon $
--             newDescribeAddon
--
--         , requestDescribeNodegroup $
--             newDescribeNodegroup
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeregisterCluster $
--             newDeregisterCluster
--
--           ]

--     , testGroup "response"
--         [ responseDescribeUpdate $
--             newDescribeUpdateResponse
--
--         , responseDescribeFargateProfile $
--             newDescribeFargateProfileResponse
--
--         , responseDescribeAddonVersions $
--             newDescribeAddonVersionsResponse
--
--         , responseListIdentityProviderConfigs $
--             newListIdentityProviderConfigsResponse
--
--         , responseListAddons $
--             newListAddonsResponse
--
--         , responseAssociateIdentityProviderConfig $
--             newAssociateIdentityProviderConfigResponse
--
--         , responseUpdateClusterVersion $
--             newUpdateClusterVersionResponse
--
--         , responseDeleteAddon $
--             newDeleteAddonResponse
--
--         , responseAssociateEncryptionConfig $
--             newAssociateEncryptionConfigResponse
--
--         , responseUpdateAddon $
--             newUpdateAddonResponse
--
--         , responseListNodegroups $
--             newListNodegroupsResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseRegisterCluster $
--             newRegisterClusterResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateNodegroup $
--             newCreateNodegroupResponse
--
--         , responseDescribeIdentityProviderConfig $
--             newDescribeIdentityProviderConfigResponse
--
--         , responseDeleteFargateProfile $
--             newDeleteFargateProfileResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListFargateProfiles $
--             newListFargateProfilesResponse
--
--         , responseListUpdates $
--             newListUpdatesResponse
--
--         , responseDisassociateIdentityProviderConfig $
--             newDisassociateIdentityProviderConfigResponse
--
--         , responseDescribeCluster $
--             newDescribeClusterResponse
--
--         , responseUpdateNodegroupVersion $
--             newUpdateNodegroupVersionResponse
--
--         , responseCreateAddon $
--             newCreateAddonResponse
--
--         , responseUpdateNodegroupConfig $
--             newUpdateNodegroupConfigResponse
--
--         , responseUpdateClusterConfig $
--             newUpdateClusterConfigResponse
--
--         , responseDeleteNodegroup $
--             newDeleteNodegroupResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseCreateFargateProfile $
--             newCreateFargateProfileResponse
--
--         , responseDescribeAddon $
--             newDescribeAddonResponse
--
--         , responseDescribeNodegroup $
--             newDescribeNodegroupResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDeregisterCluster $
--             newDeregisterClusterResponse
--
--           ]
--     ]

-- Requests

requestDescribeUpdate :: DescribeUpdate -> TestTree
requestDescribeUpdate =
  req
    "DescribeUpdate"
    "fixture/DescribeUpdate.yaml"

requestDescribeFargateProfile :: DescribeFargateProfile -> TestTree
requestDescribeFargateProfile =
  req
    "DescribeFargateProfile"
    "fixture/DescribeFargateProfile.yaml"

requestDescribeAddonVersions :: DescribeAddonVersions -> TestTree
requestDescribeAddonVersions =
  req
    "DescribeAddonVersions"
    "fixture/DescribeAddonVersions.yaml"

requestListIdentityProviderConfigs :: ListIdentityProviderConfigs -> TestTree
requestListIdentityProviderConfigs =
  req
    "ListIdentityProviderConfigs"
    "fixture/ListIdentityProviderConfigs.yaml"

requestListAddons :: ListAddons -> TestTree
requestListAddons =
  req
    "ListAddons"
    "fixture/ListAddons.yaml"

requestAssociateIdentityProviderConfig :: AssociateIdentityProviderConfig -> TestTree
requestAssociateIdentityProviderConfig =
  req
    "AssociateIdentityProviderConfig"
    "fixture/AssociateIdentityProviderConfig.yaml"

requestUpdateClusterVersion :: UpdateClusterVersion -> TestTree
requestUpdateClusterVersion =
  req
    "UpdateClusterVersion"
    "fixture/UpdateClusterVersion.yaml"

requestDeleteAddon :: DeleteAddon -> TestTree
requestDeleteAddon =
  req
    "DeleteAddon"
    "fixture/DeleteAddon.yaml"

requestAssociateEncryptionConfig :: AssociateEncryptionConfig -> TestTree
requestAssociateEncryptionConfig =
  req
    "AssociateEncryptionConfig"
    "fixture/AssociateEncryptionConfig.yaml"

requestUpdateAddon :: UpdateAddon -> TestTree
requestUpdateAddon =
  req
    "UpdateAddon"
    "fixture/UpdateAddon.yaml"

requestListNodegroups :: ListNodegroups -> TestTree
requestListNodegroups =
  req
    "ListNodegroups"
    "fixture/ListNodegroups.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestRegisterCluster :: RegisterCluster -> TestTree
requestRegisterCluster =
  req
    "RegisterCluster"
    "fixture/RegisterCluster.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateNodegroup :: CreateNodegroup -> TestTree
requestCreateNodegroup =
  req
    "CreateNodegroup"
    "fixture/CreateNodegroup.yaml"

requestDescribeIdentityProviderConfig :: DescribeIdentityProviderConfig -> TestTree
requestDescribeIdentityProviderConfig =
  req
    "DescribeIdentityProviderConfig"
    "fixture/DescribeIdentityProviderConfig.yaml"

requestDeleteFargateProfile :: DeleteFargateProfile -> TestTree
requestDeleteFargateProfile =
  req
    "DeleteFargateProfile"
    "fixture/DeleteFargateProfile.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListFargateProfiles :: ListFargateProfiles -> TestTree
requestListFargateProfiles =
  req
    "ListFargateProfiles"
    "fixture/ListFargateProfiles.yaml"

requestListUpdates :: ListUpdates -> TestTree
requestListUpdates =
  req
    "ListUpdates"
    "fixture/ListUpdates.yaml"

requestDisassociateIdentityProviderConfig :: DisassociateIdentityProviderConfig -> TestTree
requestDisassociateIdentityProviderConfig =
  req
    "DisassociateIdentityProviderConfig"
    "fixture/DisassociateIdentityProviderConfig.yaml"

requestDescribeCluster :: DescribeCluster -> TestTree
requestDescribeCluster =
  req
    "DescribeCluster"
    "fixture/DescribeCluster.yaml"

requestUpdateNodegroupVersion :: UpdateNodegroupVersion -> TestTree
requestUpdateNodegroupVersion =
  req
    "UpdateNodegroupVersion"
    "fixture/UpdateNodegroupVersion.yaml"

requestCreateAddon :: CreateAddon -> TestTree
requestCreateAddon =
  req
    "CreateAddon"
    "fixture/CreateAddon.yaml"

requestUpdateNodegroupConfig :: UpdateNodegroupConfig -> TestTree
requestUpdateNodegroupConfig =
  req
    "UpdateNodegroupConfig"
    "fixture/UpdateNodegroupConfig.yaml"

requestUpdateClusterConfig :: UpdateClusterConfig -> TestTree
requestUpdateClusterConfig =
  req
    "UpdateClusterConfig"
    "fixture/UpdateClusterConfig.yaml"

requestDeleteNodegroup :: DeleteNodegroup -> TestTree
requestDeleteNodegroup =
  req
    "DeleteNodegroup"
    "fixture/DeleteNodegroup.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestCreateFargateProfile :: CreateFargateProfile -> TestTree
requestCreateFargateProfile =
  req
    "CreateFargateProfile"
    "fixture/CreateFargateProfile.yaml"

requestDescribeAddon :: DescribeAddon -> TestTree
requestDescribeAddon =
  req
    "DescribeAddon"
    "fixture/DescribeAddon.yaml"

requestDescribeNodegroup :: DescribeNodegroup -> TestTree
requestDescribeNodegroup =
  req
    "DescribeNodegroup"
    "fixture/DescribeNodegroup.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDeregisterCluster :: DeregisterCluster -> TestTree
requestDeregisterCluster =
  req
    "DeregisterCluster"
    "fixture/DeregisterCluster.yaml"

-- Responses

responseDescribeUpdate :: DescribeUpdateResponse -> TestTree
responseDescribeUpdate =
  res
    "DescribeUpdateResponse"
    "fixture/DescribeUpdateResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUpdate)

responseDescribeFargateProfile :: DescribeFargateProfileResponse -> TestTree
responseDescribeFargateProfile =
  res
    "DescribeFargateProfileResponse"
    "fixture/DescribeFargateProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFargateProfile)

responseDescribeAddonVersions :: DescribeAddonVersionsResponse -> TestTree
responseDescribeAddonVersions =
  res
    "DescribeAddonVersionsResponse"
    "fixture/DescribeAddonVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAddonVersions)

responseListIdentityProviderConfigs :: ListIdentityProviderConfigsResponse -> TestTree
responseListIdentityProviderConfigs =
  res
    "ListIdentityProviderConfigsResponse"
    "fixture/ListIdentityProviderConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListIdentityProviderConfigs)

responseListAddons :: ListAddonsResponse -> TestTree
responseListAddons =
  res
    "ListAddonsResponse"
    "fixture/ListAddonsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAddons)

responseAssociateIdentityProviderConfig :: AssociateIdentityProviderConfigResponse -> TestTree
responseAssociateIdentityProviderConfig =
  res
    "AssociateIdentityProviderConfigResponse"
    "fixture/AssociateIdentityProviderConfigResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateIdentityProviderConfig)

responseUpdateClusterVersion :: UpdateClusterVersionResponse -> TestTree
responseUpdateClusterVersion =
  res
    "UpdateClusterVersionResponse"
    "fixture/UpdateClusterVersionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateClusterVersion)

responseDeleteAddon :: DeleteAddonResponse -> TestTree
responseDeleteAddon =
  res
    "DeleteAddonResponse"
    "fixture/DeleteAddonResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAddon)

responseAssociateEncryptionConfig :: AssociateEncryptionConfigResponse -> TestTree
responseAssociateEncryptionConfig =
  res
    "AssociateEncryptionConfigResponse"
    "fixture/AssociateEncryptionConfigResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateEncryptionConfig)

responseUpdateAddon :: UpdateAddonResponse -> TestTree
responseUpdateAddon =
  res
    "UpdateAddonResponse"
    "fixture/UpdateAddonResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAddon)

responseListNodegroups :: ListNodegroupsResponse -> TestTree
responseListNodegroups =
  res
    "ListNodegroupsResponse"
    "fixture/ListNodegroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListNodegroups)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCluster)

responseRegisterCluster :: RegisterClusterResponse -> TestTree
responseRegisterCluster =
  res
    "RegisterClusterResponse"
    "fixture/RegisterClusterResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterCluster)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseCreateNodegroup :: CreateNodegroupResponse -> TestTree
responseCreateNodegroup =
  res
    "CreateNodegroupResponse"
    "fixture/CreateNodegroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNodegroup)

responseDescribeIdentityProviderConfig :: DescribeIdentityProviderConfigResponse -> TestTree
responseDescribeIdentityProviderConfig =
  res
    "DescribeIdentityProviderConfigResponse"
    "fixture/DescribeIdentityProviderConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIdentityProviderConfig)

responseDeleteFargateProfile :: DeleteFargateProfileResponse -> TestTree
responseDeleteFargateProfile =
  res
    "DeleteFargateProfileResponse"
    "fixture/DeleteFargateProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFargateProfile)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListFargateProfiles :: ListFargateProfilesResponse -> TestTree
responseListFargateProfiles =
  res
    "ListFargateProfilesResponse"
    "fixture/ListFargateProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListFargateProfiles)

responseListUpdates :: ListUpdatesResponse -> TestTree
responseListUpdates =
  res
    "ListUpdatesResponse"
    "fixture/ListUpdatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListUpdates)

responseDisassociateIdentityProviderConfig :: DisassociateIdentityProviderConfigResponse -> TestTree
responseDisassociateIdentityProviderConfig =
  res
    "DisassociateIdentityProviderConfigResponse"
    "fixture/DisassociateIdentityProviderConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateIdentityProviderConfig)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster =
  res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCluster)

responseUpdateNodegroupVersion :: UpdateNodegroupVersionResponse -> TestTree
responseUpdateNodegroupVersion =
  res
    "UpdateNodegroupVersionResponse"
    "fixture/UpdateNodegroupVersionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNodegroupVersion)

responseCreateAddon :: CreateAddonResponse -> TestTree
responseCreateAddon =
  res
    "CreateAddonResponse"
    "fixture/CreateAddonResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAddon)

responseUpdateNodegroupConfig :: UpdateNodegroupConfigResponse -> TestTree
responseUpdateNodegroupConfig =
  res
    "UpdateNodegroupConfigResponse"
    "fixture/UpdateNodegroupConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNodegroupConfig)

responseUpdateClusterConfig :: UpdateClusterConfigResponse -> TestTree
responseUpdateClusterConfig =
  res
    "UpdateClusterConfigResponse"
    "fixture/UpdateClusterConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateClusterConfig)

responseDeleteNodegroup :: DeleteNodegroupResponse -> TestTree
responseDeleteNodegroup =
  res
    "DeleteNodegroupResponse"
    "fixture/DeleteNodegroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNodegroup)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy :: Proxy ListClusters)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCluster)

responseCreateFargateProfile :: CreateFargateProfileResponse -> TestTree
responseCreateFargateProfile =
  res
    "CreateFargateProfileResponse"
    "fixture/CreateFargateProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFargateProfile)

responseDescribeAddon :: DescribeAddonResponse -> TestTree
responseDescribeAddon =
  res
    "DescribeAddonResponse"
    "fixture/DescribeAddonResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAddon)

responseDescribeNodegroup :: DescribeNodegroupResponse -> TestTree
responseDescribeNodegroup =
  res
    "DescribeNodegroupResponse"
    "fixture/DescribeNodegroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNodegroup)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDeregisterCluster :: DeregisterClusterResponse -> TestTree
responseDeregisterCluster =
  res
    "DeregisterClusterResponse"
    "fixture/DeregisterClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterCluster)
