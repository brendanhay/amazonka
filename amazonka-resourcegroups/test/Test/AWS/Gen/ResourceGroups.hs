{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ResourceGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestGetGroupConfiguration $
--             newGetGroupConfiguration
--
--         , requestListGroups $
--             newListGroups
--
--         , requestPutGroupConfiguration $
--             newPutGroupConfiguration
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestGetGroupQuery $
--             newGetGroupQuery
--
--         , requestGetTags $
--             newGetTags
--
--         , requestSearchResources $
--             newSearchResources
--
--         , requestListGroupResources $
--             newListGroupResources
--
--         , requestUpdateGroupQuery $
--             newUpdateGroupQuery
--
--         , requestGetGroup $
--             newGetGroup
--
--         , requestUntag $
--             newUntag
--
--         , requestUpdateGroup $
--             newUpdateGroup
--
--         , requestGroupResources $
--             newGroupResources
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestUngroupResources $
--             newUngroupResources
--
--         , requestTag $
--             newTag
--
--           ]

--     , testGroup "response"
--         [ responseGetGroupConfiguration $
--             newGetGroupConfigurationResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responsePutGroupConfiguration $
--             newPutGroupConfigurationResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseGetGroupQuery $
--             newGetGroupQueryResponse
--
--         , responseGetTags $
--             newGetTagsResponse
--
--         , responseSearchResources $
--             newSearchResourcesResponse
--
--         , responseListGroupResources $
--             newListGroupResourcesResponse
--
--         , responseUpdateGroupQuery $
--             newUpdateGroupQueryResponse
--
--         , responseGetGroup $
--             newGetGroupResponse
--
--         , responseUntag $
--             newUntagResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
--
--         , responseGroupResources $
--             newGroupResourcesResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseUngroupResources $
--             newUngroupResourcesResponse
--
--         , responseTag $
--             newTagResponse
--
--           ]
--     ]

-- Requests

requestGetGroupConfiguration :: GetGroupConfiguration -> TestTree
requestGetGroupConfiguration =
  req
    "GetGroupConfiguration"
    "fixture/GetGroupConfiguration.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestPutGroupConfiguration :: PutGroupConfiguration -> TestTree
requestPutGroupConfiguration =
  req
    "PutGroupConfiguration"
    "fixture/PutGroupConfiguration.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestGetGroupQuery :: GetGroupQuery -> TestTree
requestGetGroupQuery =
  req
    "GetGroupQuery"
    "fixture/GetGroupQuery.yaml"

requestGetTags :: GetTags -> TestTree
requestGetTags =
  req
    "GetTags"
    "fixture/GetTags.yaml"

requestSearchResources :: SearchResources -> TestTree
requestSearchResources =
  req
    "SearchResources"
    "fixture/SearchResources.yaml"

requestListGroupResources :: ListGroupResources -> TestTree
requestListGroupResources =
  req
    "ListGroupResources"
    "fixture/ListGroupResources.yaml"

requestUpdateGroupQuery :: UpdateGroupQuery -> TestTree
requestUpdateGroupQuery =
  req
    "UpdateGroupQuery"
    "fixture/UpdateGroupQuery.yaml"

requestGetGroup :: GetGroup -> TestTree
requestGetGroup =
  req
    "GetGroup"
    "fixture/GetGroup.yaml"

requestUntag :: Untag -> TestTree
requestUntag =
  req
    "Untag"
    "fixture/Untag.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestGroupResources :: GroupResources -> TestTree
requestGroupResources =
  req
    "GroupResources"
    "fixture/GroupResources.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestUngroupResources :: UngroupResources -> TestTree
requestUngroupResources =
  req
    "UngroupResources"
    "fixture/UngroupResources.yaml"

requestTag :: Tag -> TestTree
requestTag =
  req
    "Tag"
    "fixture/Tag.yaml"

-- Responses

responseGetGroupConfiguration :: GetGroupConfigurationResponse -> TestTree
responseGetGroupConfiguration =
  res
    "GetGroupConfigurationResponse"
    "fixture/GetGroupConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroupConfiguration)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroups)

responsePutGroupConfiguration :: PutGroupConfigurationResponse -> TestTree
responsePutGroupConfiguration =
  res
    "PutGroupConfigurationResponse"
    "fixture/PutGroupConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutGroupConfiguration)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGroup)

responseGetGroupQuery :: GetGroupQueryResponse -> TestTree
responseGetGroupQuery =
  res
    "GetGroupQueryResponse"
    "fixture/GetGroupQueryResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroupQuery)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTags)

responseSearchResources :: SearchResourcesResponse -> TestTree
responseSearchResources =
  res
    "SearchResourcesResponse"
    "fixture/SearchResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy SearchResources)

responseListGroupResources :: ListGroupResourcesResponse -> TestTree
responseListGroupResources =
  res
    "ListGroupResourcesResponse"
    "fixture/ListGroupResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroupResources)

responseUpdateGroupQuery :: UpdateGroupQueryResponse -> TestTree
responseUpdateGroupQuery =
  res
    "UpdateGroupQueryResponse"
    "fixture/UpdateGroupQueryResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGroupQuery)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroup)

responseUntag :: UntagResponse -> TestTree
responseUntag =
  res
    "UntagResponse"
    "fixture/UntagResponse.proto"
    defaultService
    (Proxy :: Proxy Untag)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGroup)

responseGroupResources :: GroupResourcesResponse -> TestTree
responseGroupResources =
  res
    "GroupResourcesResponse"
    "fixture/GroupResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy GroupResources)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGroup)

responseUngroupResources :: UngroupResourcesResponse -> TestTree
responseUngroupResources =
  res
    "UngroupResourcesResponse"
    "fixture/UngroupResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy UngroupResources)

responseTag :: TagResponse -> TestTree
responseTag =
  res
    "TagResponse"
    "fixture/TagResponse.proto"
    defaultService
    (Proxy :: Proxy Tag)
