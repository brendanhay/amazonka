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
--         [ requestDescribeAddonVersions $
--             newDescribeAddonVersions
--
--         , requestDescribeUpdate $
--             newDescribeUpdate
--
--         , requestListIdentityProviderConfigs $
--             newListIdentityProviderConfigs
--
--         , requestDescribeFargateProfile $
--             newDescribeFargateProfile
--
--         , requestUpdateAddon $
--             newUpdateAddon
--
--         , requestAssociateEncryptionConfig $
--             newAssociateEncryptionConfig
--
--         , requestListAddons $
--             newListAddons
--
--         , requestDeleteAddon $
--             newDeleteAddon
--
--         , requestAssociateIdentityProviderConfig $
--             newAssociateIdentityProviderConfig
--
--         , requestUpdateClusterVersion $
--             newUpdateClusterVersion
--
--         , requestListNodegroups $
--             newListNodegroups
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestCreateNodegroup $
--             newCreateNodegroup
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeIdentityProviderConfig $
--             newDescribeIdentityProviderConfig
--
--         , requestDeleteFargateProfile $
--             newDeleteFargateProfile
--
--         , requestListUpdates $
--             newListUpdates
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListFargateProfiles $
--             newListFargateProfiles
--
--         , requestDisassociateIdentityProviderConfig $
--             newDisassociateIdentityProviderConfig
--
--         , requestDescribeCluster $
--             newDescribeCluster
--
--         , requestCreateAddon $
--             newCreateAddon
--
--         , requestUpdateNodegroupConfig $
--             newUpdateNodegroupConfig
--
--         , requestUpdateNodegroupVersion $
--             newUpdateNodegroupVersion
--
--         , requestUpdateClusterConfig $
--             newUpdateClusterConfig
--
--         , requestDeleteNodegroup $
--             newDeleteNodegroup
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestListClusters $
--             newListClusters
--
--         , requestDescribeAddon $
--             newDescribeAddon
--
--         , requestCreateFargateProfile $
--             newCreateFargateProfile
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeNodegroup $
--             newDescribeNodegroup
--
--           ]

--     , testGroup "response"
--         [ responseDescribeAddonVersions $
--             newDescribeAddonVersionsResponse
--
--         , responseDescribeUpdate $
--             newDescribeUpdateResponse
--
--         , responseListIdentityProviderConfigs $
--             newListIdentityProviderConfigsResponse
--
--         , responseDescribeFargateProfile $
--             newDescribeFargateProfileResponse
--
--         , responseUpdateAddon $
--             newUpdateAddonResponse
--
--         , responseAssociateEncryptionConfig $
--             newAssociateEncryptionConfigResponse
--
--         , responseListAddons $
--             newListAddonsResponse
--
--         , responseDeleteAddon $
--             newDeleteAddonResponse
--
--         , responseAssociateIdentityProviderConfig $
--             newAssociateIdentityProviderConfigResponse
--
--         , responseUpdateClusterVersion $
--             newUpdateClusterVersionResponse
--
--         , responseListNodegroups $
--             newListNodegroupsResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseCreateNodegroup $
--             newCreateNodegroupResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeIdentityProviderConfig $
--             newDescribeIdentityProviderConfigResponse
--
--         , responseDeleteFargateProfile $
--             newDeleteFargateProfileResponse
--
--         , responseListUpdates $
--             newListUpdatesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListFargateProfiles $
--             newListFargateProfilesResponse
--
--         , responseDisassociateIdentityProviderConfig $
--             newDisassociateIdentityProviderConfigResponse
--
--         , responseDescribeCluster $
--             newDescribeClusterResponse
--
--         , responseCreateAddon $
--             newCreateAddonResponse
--
--         , responseUpdateNodegroupConfig $
--             newUpdateNodegroupConfigResponse
--
--         , responseUpdateNodegroupVersion $
--             newUpdateNodegroupVersionResponse
--
--         , responseUpdateClusterConfig $
--             newUpdateClusterConfigResponse
--
--         , responseDeleteNodegroup $
--             newDeleteNodegroupResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseDescribeAddon $
--             newDescribeAddonResponse
--
--         , responseCreateFargateProfile $
--             newCreateFargateProfileResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeNodegroup $
--             newDescribeNodegroupResponse
--
--           ]
--     ]

-- Requests

requestDescribeAddonVersions :: DescribeAddonVersions -> TestTree
requestDescribeAddonVersions =
  req
    "DescribeAddonVersions"
    "fixture/DescribeAddonVersions.yaml"

requestDescribeUpdate :: DescribeUpdate -> TestTree
requestDescribeUpdate =
  req
    "DescribeUpdate"
    "fixture/DescribeUpdate.yaml"

requestListIdentityProviderConfigs :: ListIdentityProviderConfigs -> TestTree
requestListIdentityProviderConfigs =
  req
    "ListIdentityProviderConfigs"
    "fixture/ListIdentityProviderConfigs.yaml"

requestDescribeFargateProfile :: DescribeFargateProfile -> TestTree
requestDescribeFargateProfile =
  req
    "DescribeFargateProfile"
    "fixture/DescribeFargateProfile.yaml"

requestUpdateAddon :: UpdateAddon -> TestTree
requestUpdateAddon =
  req
    "UpdateAddon"
    "fixture/UpdateAddon.yaml"

requestAssociateEncryptionConfig :: AssociateEncryptionConfig -> TestTree
requestAssociateEncryptionConfig =
  req
    "AssociateEncryptionConfig"
    "fixture/AssociateEncryptionConfig.yaml"

requestListAddons :: ListAddons -> TestTree
requestListAddons =
  req
    "ListAddons"
    "fixture/ListAddons.yaml"

requestDeleteAddon :: DeleteAddon -> TestTree
requestDeleteAddon =
  req
    "DeleteAddon"
    "fixture/DeleteAddon.yaml"

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

requestCreateNodegroup :: CreateNodegroup -> TestTree
requestCreateNodegroup =
  req
    "CreateNodegroup"
    "fixture/CreateNodegroup.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

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

requestListUpdates :: ListUpdates -> TestTree
requestListUpdates =
  req
    "ListUpdates"
    "fixture/ListUpdates.yaml"

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

requestUpdateNodegroupVersion :: UpdateNodegroupVersion -> TestTree
requestUpdateNodegroupVersion =
  req
    "UpdateNodegroupVersion"
    "fixture/UpdateNodegroupVersion.yaml"

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

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestDescribeAddon :: DescribeAddon -> TestTree
requestDescribeAddon =
  req
    "DescribeAddon"
    "fixture/DescribeAddon.yaml"

requestCreateFargateProfile :: CreateFargateProfile -> TestTree
requestCreateFargateProfile =
  req
    "CreateFargateProfile"
    "fixture/CreateFargateProfile.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeNodegroup :: DescribeNodegroup -> TestTree
requestDescribeNodegroup =
  req
    "DescribeNodegroup"
    "fixture/DescribeNodegroup.yaml"

-- Responses

responseDescribeAddonVersions :: DescribeAddonVersionsResponse -> TestTree
responseDescribeAddonVersions =
  res
    "DescribeAddonVersionsResponse"
    "fixture/DescribeAddonVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAddonVersions)

responseDescribeUpdate :: DescribeUpdateResponse -> TestTree
responseDescribeUpdate =
  res
    "DescribeUpdateResponse"
    "fixture/DescribeUpdateResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUpdate)

responseListIdentityProviderConfigs :: ListIdentityProviderConfigsResponse -> TestTree
responseListIdentityProviderConfigs =
  res
    "ListIdentityProviderConfigsResponse"
    "fixture/ListIdentityProviderConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListIdentityProviderConfigs)

responseDescribeFargateProfile :: DescribeFargateProfileResponse -> TestTree
responseDescribeFargateProfile =
  res
    "DescribeFargateProfileResponse"
    "fixture/DescribeFargateProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFargateProfile)

responseUpdateAddon :: UpdateAddonResponse -> TestTree
responseUpdateAddon =
  res
    "UpdateAddonResponse"
    "fixture/UpdateAddonResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAddon)

responseAssociateEncryptionConfig :: AssociateEncryptionConfigResponse -> TestTree
responseAssociateEncryptionConfig =
  res
    "AssociateEncryptionConfigResponse"
    "fixture/AssociateEncryptionConfigResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateEncryptionConfig)

responseListAddons :: ListAddonsResponse -> TestTree
responseListAddons =
  res
    "ListAddonsResponse"
    "fixture/ListAddonsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAddons)

responseDeleteAddon :: DeleteAddonResponse -> TestTree
responseDeleteAddon =
  res
    "DeleteAddonResponse"
    "fixture/DeleteAddonResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAddon)

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

responseCreateNodegroup :: CreateNodegroupResponse -> TestTree
responseCreateNodegroup =
  res
    "CreateNodegroupResponse"
    "fixture/CreateNodegroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNodegroup)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

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

responseListUpdates :: ListUpdatesResponse -> TestTree
responseListUpdates =
  res
    "ListUpdatesResponse"
    "fixture/ListUpdatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListUpdates)

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

responseUpdateNodegroupVersion :: UpdateNodegroupVersionResponse -> TestTree
responseUpdateNodegroupVersion =
  res
    "UpdateNodegroupVersionResponse"
    "fixture/UpdateNodegroupVersionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNodegroupVersion)

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

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCluster)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy :: Proxy ListClusters)

responseDescribeAddon :: DescribeAddonResponse -> TestTree
responseDescribeAddon =
  res
    "DescribeAddonResponse"
    "fixture/DescribeAddonResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAddon)

responseCreateFargateProfile :: CreateFargateProfileResponse -> TestTree
responseCreateFargateProfile =
  res
    "CreateFargateProfileResponse"
    "fixture/CreateFargateProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFargateProfile)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDescribeNodegroup :: DescribeNodegroupResponse -> TestTree
responseDescribeNodegroup =
  res
    "DescribeNodegroupResponse"
    "fixture/DescribeNodegroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNodegroup)
