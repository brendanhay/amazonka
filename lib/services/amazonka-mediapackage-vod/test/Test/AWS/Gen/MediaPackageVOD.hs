{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MediaPackageVOD
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.MediaPackageVOD where

import Data.Proxy
import Network.AWS.MediaPackageVOD
import Test.AWS.Fixture
import Test.AWS.MediaPackageVOD.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreatePackagingGroup $
--             newCreatePackagingGroup
--
--         , requestConfigureLogs $
--             newConfigureLogs
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribePackagingGroup $
--             newDescribePackagingGroup
--
--         , requestDescribeAsset $
--             newDescribeAsset
--
--         , requestDeletePackagingConfiguration $
--             newDeletePackagingConfiguration
--
--         , requestListPackagingGroups $
--             newListPackagingGroups
--
--         , requestDeleteAsset $
--             newDeleteAsset
--
--         , requestUpdatePackagingGroup $
--             newUpdatePackagingGroup
--
--         , requestDeletePackagingGroup $
--             newDeletePackagingGroup
--
--         , requestCreateAsset $
--             newCreateAsset
--
--         , requestDescribePackagingConfiguration $
--             newDescribePackagingConfiguration
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreatePackagingConfiguration $
--             newCreatePackagingConfiguration
--
--         , requestListPackagingConfigurations $
--             newListPackagingConfigurations
--
--         , requestListAssets $
--             newListAssets
--
--           ]

--     , testGroup "response"
--         [ responseCreatePackagingGroup $
--             newCreatePackagingGroupResponse
--
--         , responseConfigureLogs $
--             newConfigureLogsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribePackagingGroup $
--             newDescribePackagingGroupResponse
--
--         , responseDescribeAsset $
--             newDescribeAssetResponse
--
--         , responseDeletePackagingConfiguration $
--             newDeletePackagingConfigurationResponse
--
--         , responseListPackagingGroups $
--             newListPackagingGroupsResponse
--
--         , responseDeleteAsset $
--             newDeleteAssetResponse
--
--         , responseUpdatePackagingGroup $
--             newUpdatePackagingGroupResponse
--
--         , responseDeletePackagingGroup $
--             newDeletePackagingGroupResponse
--
--         , responseCreateAsset $
--             newCreateAssetResponse
--
--         , responseDescribePackagingConfiguration $
--             newDescribePackagingConfigurationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreatePackagingConfiguration $
--             newCreatePackagingConfigurationResponse
--
--         , responseListPackagingConfigurations $
--             newListPackagingConfigurationsResponse
--
--         , responseListAssets $
--             newListAssetsResponse
--
--           ]
--     ]

-- Requests

requestCreatePackagingGroup :: CreatePackagingGroup -> TestTree
requestCreatePackagingGroup =
  req
    "CreatePackagingGroup"
    "fixture/CreatePackagingGroup.yaml"

requestConfigureLogs :: ConfigureLogs -> TestTree
requestConfigureLogs =
  req
    "ConfigureLogs"
    "fixture/ConfigureLogs.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribePackagingGroup :: DescribePackagingGroup -> TestTree
requestDescribePackagingGroup =
  req
    "DescribePackagingGroup"
    "fixture/DescribePackagingGroup.yaml"

requestDescribeAsset :: DescribeAsset -> TestTree
requestDescribeAsset =
  req
    "DescribeAsset"
    "fixture/DescribeAsset.yaml"

requestDeletePackagingConfiguration :: DeletePackagingConfiguration -> TestTree
requestDeletePackagingConfiguration =
  req
    "DeletePackagingConfiguration"
    "fixture/DeletePackagingConfiguration.yaml"

requestListPackagingGroups :: ListPackagingGroups -> TestTree
requestListPackagingGroups =
  req
    "ListPackagingGroups"
    "fixture/ListPackagingGroups.yaml"

requestDeleteAsset :: DeleteAsset -> TestTree
requestDeleteAsset =
  req
    "DeleteAsset"
    "fixture/DeleteAsset.yaml"

requestUpdatePackagingGroup :: UpdatePackagingGroup -> TestTree
requestUpdatePackagingGroup =
  req
    "UpdatePackagingGroup"
    "fixture/UpdatePackagingGroup.yaml"

requestDeletePackagingGroup :: DeletePackagingGroup -> TestTree
requestDeletePackagingGroup =
  req
    "DeletePackagingGroup"
    "fixture/DeletePackagingGroup.yaml"

requestCreateAsset :: CreateAsset -> TestTree
requestCreateAsset =
  req
    "CreateAsset"
    "fixture/CreateAsset.yaml"

requestDescribePackagingConfiguration :: DescribePackagingConfiguration -> TestTree
requestDescribePackagingConfiguration =
  req
    "DescribePackagingConfiguration"
    "fixture/DescribePackagingConfiguration.yaml"

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

requestCreatePackagingConfiguration :: CreatePackagingConfiguration -> TestTree
requestCreatePackagingConfiguration =
  req
    "CreatePackagingConfiguration"
    "fixture/CreatePackagingConfiguration.yaml"

requestListPackagingConfigurations :: ListPackagingConfigurations -> TestTree
requestListPackagingConfigurations =
  req
    "ListPackagingConfigurations"
    "fixture/ListPackagingConfigurations.yaml"

requestListAssets :: ListAssets -> TestTree
requestListAssets =
  req
    "ListAssets"
    "fixture/ListAssets.yaml"

-- Responses

responseCreatePackagingGroup :: CreatePackagingGroupResponse -> TestTree
responseCreatePackagingGroup =
  res
    "CreatePackagingGroupResponse"
    "fixture/CreatePackagingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePackagingGroup)

responseConfigureLogs :: ConfigureLogsResponse -> TestTree
responseConfigureLogs =
  res
    "ConfigureLogsResponse"
    "fixture/ConfigureLogsResponse.proto"
    defaultService
    (Proxy :: Proxy ConfigureLogs)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDescribePackagingGroup :: DescribePackagingGroupResponse -> TestTree
responseDescribePackagingGroup =
  res
    "DescribePackagingGroupResponse"
    "fixture/DescribePackagingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePackagingGroup)

responseDescribeAsset :: DescribeAssetResponse -> TestTree
responseDescribeAsset =
  res
    "DescribeAssetResponse"
    "fixture/DescribeAssetResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAsset)

responseDeletePackagingConfiguration :: DeletePackagingConfigurationResponse -> TestTree
responseDeletePackagingConfiguration =
  res
    "DeletePackagingConfigurationResponse"
    "fixture/DeletePackagingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePackagingConfiguration)

responseListPackagingGroups :: ListPackagingGroupsResponse -> TestTree
responseListPackagingGroups =
  res
    "ListPackagingGroupsResponse"
    "fixture/ListPackagingGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPackagingGroups)

responseDeleteAsset :: DeleteAssetResponse -> TestTree
responseDeleteAsset =
  res
    "DeleteAssetResponse"
    "fixture/DeleteAssetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAsset)

responseUpdatePackagingGroup :: UpdatePackagingGroupResponse -> TestTree
responseUpdatePackagingGroup =
  res
    "UpdatePackagingGroupResponse"
    "fixture/UpdatePackagingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePackagingGroup)

responseDeletePackagingGroup :: DeletePackagingGroupResponse -> TestTree
responseDeletePackagingGroup =
  res
    "DeletePackagingGroupResponse"
    "fixture/DeletePackagingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePackagingGroup)

responseCreateAsset :: CreateAssetResponse -> TestTree
responseCreateAsset =
  res
    "CreateAssetResponse"
    "fixture/CreateAssetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAsset)

responseDescribePackagingConfiguration :: DescribePackagingConfigurationResponse -> TestTree
responseDescribePackagingConfiguration =
  res
    "DescribePackagingConfigurationResponse"
    "fixture/DescribePackagingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePackagingConfiguration)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseCreatePackagingConfiguration :: CreatePackagingConfigurationResponse -> TestTree
responseCreatePackagingConfiguration =
  res
    "CreatePackagingConfigurationResponse"
    "fixture/CreatePackagingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePackagingConfiguration)

responseListPackagingConfigurations :: ListPackagingConfigurationsResponse -> TestTree
responseListPackagingConfigurations =
  res
    "ListPackagingConfigurationsResponse"
    "fixture/ListPackagingConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPackagingConfigurations)

responseListAssets :: ListAssetsResponse -> TestTree
responseListAssets =
  res
    "ListAssetsResponse"
    "fixture/ListAssetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssets)
