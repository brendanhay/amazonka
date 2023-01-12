{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MediaPackageVOD
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MediaPackageVOD where

import Amazonka.MediaPackageVOD
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MediaPackageVOD.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestConfigureLogs $
--             newConfigureLogs
--
--         , requestCreateAsset $
--             newCreateAsset
--
--         , requestCreatePackagingConfiguration $
--             newCreatePackagingConfiguration
--
--         , requestCreatePackagingGroup $
--             newCreatePackagingGroup
--
--         , requestDeleteAsset $
--             newDeleteAsset
--
--         , requestDeletePackagingConfiguration $
--             newDeletePackagingConfiguration
--
--         , requestDeletePackagingGroup $
--             newDeletePackagingGroup
--
--         , requestDescribeAsset $
--             newDescribeAsset
--
--         , requestDescribePackagingConfiguration $
--             newDescribePackagingConfiguration
--
--         , requestDescribePackagingGroup $
--             newDescribePackagingGroup
--
--         , requestListAssets $
--             newListAssets
--
--         , requestListPackagingConfigurations $
--             newListPackagingConfigurations
--
--         , requestListPackagingGroups $
--             newListPackagingGroups
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdatePackagingGroup $
--             newUpdatePackagingGroup
--
--           ]

--     , testGroup "response"
--         [ responseConfigureLogs $
--             newConfigureLogsResponse
--
--         , responseCreateAsset $
--             newCreateAssetResponse
--
--         , responseCreatePackagingConfiguration $
--             newCreatePackagingConfigurationResponse
--
--         , responseCreatePackagingGroup $
--             newCreatePackagingGroupResponse
--
--         , responseDeleteAsset $
--             newDeleteAssetResponse
--
--         , responseDeletePackagingConfiguration $
--             newDeletePackagingConfigurationResponse
--
--         , responseDeletePackagingGroup $
--             newDeletePackagingGroupResponse
--
--         , responseDescribeAsset $
--             newDescribeAssetResponse
--
--         , responseDescribePackagingConfiguration $
--             newDescribePackagingConfigurationResponse
--
--         , responseDescribePackagingGroup $
--             newDescribePackagingGroupResponse
--
--         , responseListAssets $
--             newListAssetsResponse
--
--         , responseListPackagingConfigurations $
--             newListPackagingConfigurationsResponse
--
--         , responseListPackagingGroups $
--             newListPackagingGroupsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdatePackagingGroup $
--             newUpdatePackagingGroupResponse
--
--           ]
--     ]

-- Requests

requestConfigureLogs :: ConfigureLogs -> TestTree
requestConfigureLogs =
  req
    "ConfigureLogs"
    "fixture/ConfigureLogs.yaml"

requestCreateAsset :: CreateAsset -> TestTree
requestCreateAsset =
  req
    "CreateAsset"
    "fixture/CreateAsset.yaml"

requestCreatePackagingConfiguration :: CreatePackagingConfiguration -> TestTree
requestCreatePackagingConfiguration =
  req
    "CreatePackagingConfiguration"
    "fixture/CreatePackagingConfiguration.yaml"

requestCreatePackagingGroup :: CreatePackagingGroup -> TestTree
requestCreatePackagingGroup =
  req
    "CreatePackagingGroup"
    "fixture/CreatePackagingGroup.yaml"

requestDeleteAsset :: DeleteAsset -> TestTree
requestDeleteAsset =
  req
    "DeleteAsset"
    "fixture/DeleteAsset.yaml"

requestDeletePackagingConfiguration :: DeletePackagingConfiguration -> TestTree
requestDeletePackagingConfiguration =
  req
    "DeletePackagingConfiguration"
    "fixture/DeletePackagingConfiguration.yaml"

requestDeletePackagingGroup :: DeletePackagingGroup -> TestTree
requestDeletePackagingGroup =
  req
    "DeletePackagingGroup"
    "fixture/DeletePackagingGroup.yaml"

requestDescribeAsset :: DescribeAsset -> TestTree
requestDescribeAsset =
  req
    "DescribeAsset"
    "fixture/DescribeAsset.yaml"

requestDescribePackagingConfiguration :: DescribePackagingConfiguration -> TestTree
requestDescribePackagingConfiguration =
  req
    "DescribePackagingConfiguration"
    "fixture/DescribePackagingConfiguration.yaml"

requestDescribePackagingGroup :: DescribePackagingGroup -> TestTree
requestDescribePackagingGroup =
  req
    "DescribePackagingGroup"
    "fixture/DescribePackagingGroup.yaml"

requestListAssets :: ListAssets -> TestTree
requestListAssets =
  req
    "ListAssets"
    "fixture/ListAssets.yaml"

requestListPackagingConfigurations :: ListPackagingConfigurations -> TestTree
requestListPackagingConfigurations =
  req
    "ListPackagingConfigurations"
    "fixture/ListPackagingConfigurations.yaml"

requestListPackagingGroups :: ListPackagingGroups -> TestTree
requestListPackagingGroups =
  req
    "ListPackagingGroups"
    "fixture/ListPackagingGroups.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestUpdatePackagingGroup :: UpdatePackagingGroup -> TestTree
requestUpdatePackagingGroup =
  req
    "UpdatePackagingGroup"
    "fixture/UpdatePackagingGroup.yaml"

-- Responses

responseConfigureLogs :: ConfigureLogsResponse -> TestTree
responseConfigureLogs =
  res
    "ConfigureLogsResponse"
    "fixture/ConfigureLogsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfigureLogs)

responseCreateAsset :: CreateAssetResponse -> TestTree
responseCreateAsset =
  res
    "CreateAssetResponse"
    "fixture/CreateAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAsset)

responseCreatePackagingConfiguration :: CreatePackagingConfigurationResponse -> TestTree
responseCreatePackagingConfiguration =
  res
    "CreatePackagingConfigurationResponse"
    "fixture/CreatePackagingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePackagingConfiguration)

responseCreatePackagingGroup :: CreatePackagingGroupResponse -> TestTree
responseCreatePackagingGroup =
  res
    "CreatePackagingGroupResponse"
    "fixture/CreatePackagingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePackagingGroup)

responseDeleteAsset :: DeleteAssetResponse -> TestTree
responseDeleteAsset =
  res
    "DeleteAssetResponse"
    "fixture/DeleteAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAsset)

responseDeletePackagingConfiguration :: DeletePackagingConfigurationResponse -> TestTree
responseDeletePackagingConfiguration =
  res
    "DeletePackagingConfigurationResponse"
    "fixture/DeletePackagingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePackagingConfiguration)

responseDeletePackagingGroup :: DeletePackagingGroupResponse -> TestTree
responseDeletePackagingGroup =
  res
    "DeletePackagingGroupResponse"
    "fixture/DeletePackagingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePackagingGroup)

responseDescribeAsset :: DescribeAssetResponse -> TestTree
responseDescribeAsset =
  res
    "DescribeAssetResponse"
    "fixture/DescribeAssetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAsset)

responseDescribePackagingConfiguration :: DescribePackagingConfigurationResponse -> TestTree
responseDescribePackagingConfiguration =
  res
    "DescribePackagingConfigurationResponse"
    "fixture/DescribePackagingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePackagingConfiguration)

responseDescribePackagingGroup :: DescribePackagingGroupResponse -> TestTree
responseDescribePackagingGroup =
  res
    "DescribePackagingGroupResponse"
    "fixture/DescribePackagingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePackagingGroup)

responseListAssets :: ListAssetsResponse -> TestTree
responseListAssets =
  res
    "ListAssetsResponse"
    "fixture/ListAssetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssets)

responseListPackagingConfigurations :: ListPackagingConfigurationsResponse -> TestTree
responseListPackagingConfigurations =
  res
    "ListPackagingConfigurationsResponse"
    "fixture/ListPackagingConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPackagingConfigurations)

responseListPackagingGroups :: ListPackagingGroupsResponse -> TestTree
responseListPackagingGroups =
  res
    "ListPackagingGroupsResponse"
    "fixture/ListPackagingGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPackagingGroups)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

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

responseUpdatePackagingGroup :: UpdatePackagingGroupResponse -> TestTree
responseUpdatePackagingGroup =
  res
    "UpdatePackagingGroupResponse"
    "fixture/UpdatePackagingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePackagingGroup)
