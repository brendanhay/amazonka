{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ResourceGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.ResourceGroups where

import Data.Proxy
import Network.AWS.ResourceGroups
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.ResourceGroups.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestSearchResources $
--             searchResources
--
--         , requestGetTags $
--             getTags
--
--         , requestTag $
--             tag
--
--         , requestUngroupResources $
--             ungroupResources
--
--         , requestGroupResources $
--             groupResources
--
--         , requestUntag $
--             untag
--
--         , requestUpdateGroupQuery $
--             updateGroupQuery
--
--         , requestListGroupResources $
--             listGroupResources
--
--         , requestGetGroupQuery $
--             getGroupQuery
--
--         , requestCreateGroup $
--             createGroup
--
--         , requestDeleteGroup $
--             deleteGroup
--
--         , requestUpdateGroup $
--             updateGroup
--
--         , requestListGroups $
--             listGroups
--
--         , requestGetGroup $
--             getGroup
--
--         , requestGetGroupConfiguration $
--             getGroupConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseSearchResources $
--             searchResourcesResponse
--
--         , responseGetTags $
--             getTagsResponse
--
--         , responseTag $
--             tagResponse
--
--         , responseUngroupResources $
--             ungroupResourcesResponse
--
--         , responseGroupResources $
--             groupResourcesResponse
--
--         , responseUntag $
--             untagResponse
--
--         , responseUpdateGroupQuery $
--             updateGroupQueryResponse
--
--         , responseListGroupResources $
--             listGroupResourcesResponse
--
--         , responseGetGroupQuery $
--             getGroupQueryResponse
--
--         , responseCreateGroup $
--             createGroupResponse
--
--         , responseDeleteGroup $
--             deleteGroupResponse
--
--         , responseUpdateGroup $
--             updateGroupResponse
--
--         , responseListGroups $
--             listGroupsResponse
--
--         , responseGetGroup $
--             getGroupResponse
--
--         , responseGetGroupConfiguration $
--             getGroupConfigurationResponse
--
--           ]
--     ]

-- Requests

requestSearchResources :: SearchResources -> TestTree
requestSearchResources =
  req
    "SearchResources"
    "fixture/SearchResources.yaml"

requestGetTags :: GetTags -> TestTree
requestGetTags =
  req
    "GetTags"
    "fixture/GetTags.yaml"

requestTag :: Tag -> TestTree
requestTag =
  req
    "Tag"
    "fixture/Tag.yaml"

requestUngroupResources :: UngroupResources -> TestTree
requestUngroupResources =
  req
    "UngroupResources"
    "fixture/UngroupResources.yaml"

requestGroupResources :: GroupResources -> TestTree
requestGroupResources =
  req
    "GroupResources"
    "fixture/GroupResources.yaml"

requestUntag :: Untag -> TestTree
requestUntag =
  req
    "Untag"
    "fixture/Untag.yaml"

requestUpdateGroupQuery :: UpdateGroupQuery -> TestTree
requestUpdateGroupQuery =
  req
    "UpdateGroupQuery"
    "fixture/UpdateGroupQuery.yaml"

requestListGroupResources :: ListGroupResources -> TestTree
requestListGroupResources =
  req
    "ListGroupResources"
    "fixture/ListGroupResources.yaml"

requestGetGroupQuery :: GetGroupQuery -> TestTree
requestGetGroupQuery =
  req
    "GetGroupQuery"
    "fixture/GetGroupQuery.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestGetGroup :: GetGroup -> TestTree
requestGetGroup =
  req
    "GetGroup"
    "fixture/GetGroup.yaml"

requestGetGroupConfiguration :: GetGroupConfiguration -> TestTree
requestGetGroupConfiguration =
  req
    "GetGroupConfiguration"
    "fixture/GetGroupConfiguration.yaml"

-- Responses

responseSearchResources :: SearchResourcesResponse -> TestTree
responseSearchResources =
  res
    "SearchResourcesResponse"
    "fixture/SearchResourcesResponse.proto"
    resourceGroups
    (Proxy :: Proxy SearchResources)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    resourceGroups
    (Proxy :: Proxy GetTags)

responseTag :: TagResponse -> TestTree
responseTag =
  res
    "TagResponse"
    "fixture/TagResponse.proto"
    resourceGroups
    (Proxy :: Proxy Tag)

responseUngroupResources :: UngroupResourcesResponse -> TestTree
responseUngroupResources =
  res
    "UngroupResourcesResponse"
    "fixture/UngroupResourcesResponse.proto"
    resourceGroups
    (Proxy :: Proxy UngroupResources)

responseGroupResources :: GroupResourcesResponse -> TestTree
responseGroupResources =
  res
    "GroupResourcesResponse"
    "fixture/GroupResourcesResponse.proto"
    resourceGroups
    (Proxy :: Proxy GroupResources)

responseUntag :: UntagResponse -> TestTree
responseUntag =
  res
    "UntagResponse"
    "fixture/UntagResponse.proto"
    resourceGroups
    (Proxy :: Proxy Untag)

responseUpdateGroupQuery :: UpdateGroupQueryResponse -> TestTree
responseUpdateGroupQuery =
  res
    "UpdateGroupQueryResponse"
    "fixture/UpdateGroupQueryResponse.proto"
    resourceGroups
    (Proxy :: Proxy UpdateGroupQuery)

responseListGroupResources :: ListGroupResourcesResponse -> TestTree
responseListGroupResources =
  res
    "ListGroupResourcesResponse"
    "fixture/ListGroupResourcesResponse.proto"
    resourceGroups
    (Proxy :: Proxy ListGroupResources)

responseGetGroupQuery :: GetGroupQueryResponse -> TestTree
responseGetGroupQuery =
  res
    "GetGroupQueryResponse"
    "fixture/GetGroupQueryResponse.proto"
    resourceGroups
    (Proxy :: Proxy GetGroupQuery)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    resourceGroups
    (Proxy :: Proxy CreateGroup)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    resourceGroups
    (Proxy :: Proxy DeleteGroup)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    resourceGroups
    (Proxy :: Proxy UpdateGroup)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    resourceGroups
    (Proxy :: Proxy ListGroups)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    resourceGroups
    (Proxy :: Proxy GetGroup)

responseGetGroupConfiguration :: GetGroupConfigurationResponse -> TestTree
responseGetGroupConfiguration =
  res
    "GetGroupConfigurationResponse"
    "fixture/GetGroupConfigurationResponse.proto"
    resourceGroups
    (Proxy :: Proxy GetGroupConfiguration)
