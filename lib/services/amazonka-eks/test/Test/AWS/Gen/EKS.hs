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
--         [ requestCreateAddon $
--             newCreateAddon
--
--         , requestDescribeFargateProfile $
--             newDescribeFargateProfile
--
--         , requestDescribeUpdate $
--             newDescribeUpdate
--
--         , requestUpdateNodegroupConfig $
--             newUpdateNodegroupConfig
--
--         , requestDescribeCluster $
--             newDescribeCluster
--
--         , requestDeregisterCluster $
--             newDeregisterCluster
--
--         , requestDescribeNodegroup $
--             newDescribeNodegroup
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateFargateProfile $
--             newCreateFargateProfile
--
--         , requestDescribeIdentityProviderConfig $
--             newDescribeIdentityProviderConfig
--
--         , requestDeleteFargateProfile $
--             newDeleteFargateProfile
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestUpdateClusterConfig $
--             newUpdateClusterConfig
--
--         , requestListAddons $
--             newListAddons
--
--         , requestUpdateClusterVersion $
--             newUpdateClusterVersion
--
--         , requestDescribeAddonVersions $
--             newDescribeAddonVersions
--
--         , requestUpdateNodegroupVersion $
--             newUpdateNodegroupVersion
--
--         , requestListIdentityProviderConfigs $
--             newListIdentityProviderConfigs
--
--         , requestDisassociateIdentityProviderConfig $
--             newDisassociateIdentityProviderConfig
--
--         , requestDescribeAddon $
--             newDescribeAddon
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
--         , requestRegisterCluster $
--             newRegisterCluster
--
--         , requestListClusters $
--             newListClusters
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateNodegroup $
--             newCreateNodegroup
--
--         , requestListNodegroups $
--             newListNodegroups
--
--         , requestDeleteNodegroup $
--             newDeleteNodegroup
--
--         , requestAssociateEncryptionConfig $
--             newAssociateEncryptionConfig
--
--         , requestAssociateIdentityProviderConfig $
--             newAssociateIdentityProviderConfig
--
--         , requestDeleteAddon $
--             newDeleteAddon
--
--         , requestUpdateAddon $
--             newUpdateAddon
--
--           ]

--     , testGroup "response"
--         [ responseCreateAddon $
--             newCreateAddonResponse
--
--         , responseDescribeFargateProfile $
--             newDescribeFargateProfileResponse
--
--         , responseDescribeUpdate $
--             newDescribeUpdateResponse
--
--         , responseUpdateNodegroupConfig $
--             newUpdateNodegroupConfigResponse
--
--         , responseDescribeCluster $
--             newDescribeClusterResponse
--
--         , responseDeregisterCluster $
--             newDeregisterClusterResponse
--
--         , responseDescribeNodegroup $
--             newDescribeNodegroupResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateFargateProfile $
--             newCreateFargateProfileResponse
--
--         , responseDescribeIdentityProviderConfig $
--             newDescribeIdentityProviderConfigResponse
--
--         , responseDeleteFargateProfile $
--             newDeleteFargateProfileResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseUpdateClusterConfig $
--             newUpdateClusterConfigResponse
--
--         , responseListAddons $
--             newListAddonsResponse
--
--         , responseUpdateClusterVersion $
--             newUpdateClusterVersionResponse
--
--         , responseDescribeAddonVersions $
--             newDescribeAddonVersionsResponse
--
--         , responseUpdateNodegroupVersion $
--             newUpdateNodegroupVersionResponse
--
--         , responseListIdentityProviderConfigs $
--             newListIdentityProviderConfigsResponse
--
--         , responseDisassociateIdentityProviderConfig $
--             newDisassociateIdentityProviderConfigResponse
--
--         , responseDescribeAddon $
--             newDescribeAddonResponse
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
--         , responseRegisterCluster $
--             newRegisterClusterResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateNodegroup $
--             newCreateNodegroupResponse
--
--         , responseListNodegroups $
--             newListNodegroupsResponse
--
--         , responseDeleteNodegroup $
--             newDeleteNodegroupResponse
--
--         , responseAssociateEncryptionConfig $
--             newAssociateEncryptionConfigResponse
--
--         , responseAssociateIdentityProviderConfig $
--             newAssociateIdentityProviderConfigResponse
--
--         , responseDeleteAddon $
--             newDeleteAddonResponse
--
--         , responseUpdateAddon $
--             newUpdateAddonResponse
--
--           ]
--     ]

-- Requests

requestCreateAddon :: CreateAddon -> TestTree
requestCreateAddon =
  req
    "CreateAddon"
    "fixture/CreateAddon.yaml"

requestDescribeFargateProfile :: DescribeFargateProfile -> TestTree
requestDescribeFargateProfile =
  req
    "DescribeFargateProfile"
    "fixture/DescribeFargateProfile.yaml"

requestDescribeUpdate :: DescribeUpdate -> TestTree
requestDescribeUpdate =
  req
    "DescribeUpdate"
    "fixture/DescribeUpdate.yaml"

requestUpdateNodegroupConfig :: UpdateNodegroupConfig -> TestTree
requestUpdateNodegroupConfig =
  req
    "UpdateNodegroupConfig"
    "fixture/UpdateNodegroupConfig.yaml"

requestDescribeCluster :: DescribeCluster -> TestTree
requestDescribeCluster =
  req
    "DescribeCluster"
    "fixture/DescribeCluster.yaml"

requestDeregisterCluster :: DeregisterCluster -> TestTree
requestDeregisterCluster =
  req
    "DeregisterCluster"
    "fixture/DeregisterCluster.yaml"

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

requestCreateFargateProfile :: CreateFargateProfile -> TestTree
requestCreateFargateProfile =
  req
    "CreateFargateProfile"
    "fixture/CreateFargateProfile.yaml"

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

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestUpdateClusterConfig :: UpdateClusterConfig -> TestTree
requestUpdateClusterConfig =
  req
    "UpdateClusterConfig"
    "fixture/UpdateClusterConfig.yaml"

requestListAddons :: ListAddons -> TestTree
requestListAddons =
  req
    "ListAddons"
    "fixture/ListAddons.yaml"

requestUpdateClusterVersion :: UpdateClusterVersion -> TestTree
requestUpdateClusterVersion =
  req
    "UpdateClusterVersion"
    "fixture/UpdateClusterVersion.yaml"

requestDescribeAddonVersions :: DescribeAddonVersions -> TestTree
requestDescribeAddonVersions =
  req
    "DescribeAddonVersions"
    "fixture/DescribeAddonVersions.yaml"

requestUpdateNodegroupVersion :: UpdateNodegroupVersion -> TestTree
requestUpdateNodegroupVersion =
  req
    "UpdateNodegroupVersion"
    "fixture/UpdateNodegroupVersion.yaml"

requestListIdentityProviderConfigs :: ListIdentityProviderConfigs -> TestTree
requestListIdentityProviderConfigs =
  req
    "ListIdentityProviderConfigs"
    "fixture/ListIdentityProviderConfigs.yaml"

requestDisassociateIdentityProviderConfig :: DisassociateIdentityProviderConfig -> TestTree
requestDisassociateIdentityProviderConfig =
  req
    "DisassociateIdentityProviderConfig"
    "fixture/DisassociateIdentityProviderConfig.yaml"

requestDescribeAddon :: DescribeAddon -> TestTree
requestDescribeAddon =
  req
    "DescribeAddon"
    "fixture/DescribeAddon.yaml"

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

requestRegisterCluster :: RegisterCluster -> TestTree
requestRegisterCluster =
  req
    "RegisterCluster"
    "fixture/RegisterCluster.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

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

requestListNodegroups :: ListNodegroups -> TestTree
requestListNodegroups =
  req
    "ListNodegroups"
    "fixture/ListNodegroups.yaml"

requestDeleteNodegroup :: DeleteNodegroup -> TestTree
requestDeleteNodegroup =
  req
    "DeleteNodegroup"
    "fixture/DeleteNodegroup.yaml"

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

requestDeleteAddon :: DeleteAddon -> TestTree
requestDeleteAddon =
  req
    "DeleteAddon"
    "fixture/DeleteAddon.yaml"

requestUpdateAddon :: UpdateAddon -> TestTree
requestUpdateAddon =
  req
    "UpdateAddon"
    "fixture/UpdateAddon.yaml"

-- Responses

responseCreateAddon :: CreateAddonResponse -> TestTree
responseCreateAddon =
  res
    "CreateAddonResponse"
    "fixture/CreateAddonResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAddon)

responseDescribeFargateProfile :: DescribeFargateProfileResponse -> TestTree
responseDescribeFargateProfile =
  res
    "DescribeFargateProfileResponse"
    "fixture/DescribeFargateProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFargateProfile)

responseDescribeUpdate :: DescribeUpdateResponse -> TestTree
responseDescribeUpdate =
  res
    "DescribeUpdateResponse"
    "fixture/DescribeUpdateResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUpdate)

responseUpdateNodegroupConfig :: UpdateNodegroupConfigResponse -> TestTree
responseUpdateNodegroupConfig =
  res
    "UpdateNodegroupConfigResponse"
    "fixture/UpdateNodegroupConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNodegroupConfig)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster =
  res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCluster)

responseDeregisterCluster :: DeregisterClusterResponse -> TestTree
responseDeregisterCluster =
  res
    "DeregisterClusterResponse"
    "fixture/DeregisterClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterCluster)

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

responseCreateFargateProfile :: CreateFargateProfileResponse -> TestTree
responseCreateFargateProfile =
  res
    "CreateFargateProfileResponse"
    "fixture/CreateFargateProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFargateProfile)

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

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCluster)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCluster)

responseUpdateClusterConfig :: UpdateClusterConfigResponse -> TestTree
responseUpdateClusterConfig =
  res
    "UpdateClusterConfigResponse"
    "fixture/UpdateClusterConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateClusterConfig)

responseListAddons :: ListAddonsResponse -> TestTree
responseListAddons =
  res
    "ListAddonsResponse"
    "fixture/ListAddonsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAddons)

responseUpdateClusterVersion :: UpdateClusterVersionResponse -> TestTree
responseUpdateClusterVersion =
  res
    "UpdateClusterVersionResponse"
    "fixture/UpdateClusterVersionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateClusterVersion)

responseDescribeAddonVersions :: DescribeAddonVersionsResponse -> TestTree
responseDescribeAddonVersions =
  res
    "DescribeAddonVersionsResponse"
    "fixture/DescribeAddonVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAddonVersions)

responseUpdateNodegroupVersion :: UpdateNodegroupVersionResponse -> TestTree
responseUpdateNodegroupVersion =
  res
    "UpdateNodegroupVersionResponse"
    "fixture/UpdateNodegroupVersionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNodegroupVersion)

responseListIdentityProviderConfigs :: ListIdentityProviderConfigsResponse -> TestTree
responseListIdentityProviderConfigs =
  res
    "ListIdentityProviderConfigsResponse"
    "fixture/ListIdentityProviderConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListIdentityProviderConfigs)

responseDisassociateIdentityProviderConfig :: DisassociateIdentityProviderConfigResponse -> TestTree
responseDisassociateIdentityProviderConfig =
  res
    "DisassociateIdentityProviderConfigResponse"
    "fixture/DisassociateIdentityProviderConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateIdentityProviderConfig)

responseDescribeAddon :: DescribeAddonResponse -> TestTree
responseDescribeAddon =
  res
    "DescribeAddonResponse"
    "fixture/DescribeAddonResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAddon)

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

responseRegisterCluster :: RegisterClusterResponse -> TestTree
responseRegisterCluster =
  res
    "RegisterClusterResponse"
    "fixture/RegisterClusterResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterCluster)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy :: Proxy ListClusters)

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

responseListNodegroups :: ListNodegroupsResponse -> TestTree
responseListNodegroups =
  res
    "ListNodegroupsResponse"
    "fixture/ListNodegroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListNodegroups)

responseDeleteNodegroup :: DeleteNodegroupResponse -> TestTree
responseDeleteNodegroup =
  res
    "DeleteNodegroupResponse"
    "fixture/DeleteNodegroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNodegroup)

responseAssociateEncryptionConfig :: AssociateEncryptionConfigResponse -> TestTree
responseAssociateEncryptionConfig =
  res
    "AssociateEncryptionConfigResponse"
    "fixture/AssociateEncryptionConfigResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateEncryptionConfig)

responseAssociateIdentityProviderConfig :: AssociateIdentityProviderConfigResponse -> TestTree
responseAssociateIdentityProviderConfig =
  res
    "AssociateIdentityProviderConfigResponse"
    "fixture/AssociateIdentityProviderConfigResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateIdentityProviderConfig)

responseDeleteAddon :: DeleteAddonResponse -> TestTree
responseDeleteAddon =
  res
    "DeleteAddonResponse"
    "fixture/DeleteAddonResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAddon)

responseUpdateAddon :: UpdateAddonResponse -> TestTree
responseUpdateAddon =
  res
    "UpdateAddonResponse"
    "fixture/UpdateAddonResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAddon)
