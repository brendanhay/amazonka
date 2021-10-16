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
--         , requestPutGroupConfiguration $
--             newPutGroupConfiguration
--
--         , requestListGroups $
--             newListGroups
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestGetGroupQuery $
--             newGetGroupQuery
--
--         , requestSearchResources $
--             newSearchResources
--
--         , requestGetTags $
--             newGetTags
--
--         , requestUpdateGroupQuery $
--             newUpdateGroupQuery
--
--         , requestListGroupResources $
--             newListGroupResources
--
--         , requestUntag $
--             newUntag
--
--         , requestGetGroup $
--             newGetGroup
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestGroupResources $
--             newGroupResources
--
--         , requestUpdateGroup $
--             newUpdateGroup
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
--         , responsePutGroupConfiguration $
--             newPutGroupConfigurationResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseGetGroupQuery $
--             newGetGroupQueryResponse
--
--         , responseSearchResources $
--             newSearchResourcesResponse
--
--         , responseGetTags $
--             newGetTagsResponse
--
--         , responseUpdateGroupQuery $
--             newUpdateGroupQueryResponse
--
--         , responseListGroupResources $
--             newListGroupResourcesResponse
--
--         , responseUntag $
--             newUntagResponse
--
--         , responseGetGroup $
--             newGetGroupResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseGroupResources $
--             newGroupResourcesResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
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

requestPutGroupConfiguration :: PutGroupConfiguration -> TestTree
requestPutGroupConfiguration =
  req
    "PutGroupConfiguration"
    "fixture/PutGroupConfiguration.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

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

requestUntag :: Untag -> TestTree
requestUntag =
  req
    "Untag"
    "fixture/Untag.yaml"

requestGetGroup :: GetGroup -> TestTree
requestGetGroup =
  req
    "GetGroup"
    "fixture/GetGroup.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestGroupResources :: GroupResources -> TestTree
requestGroupResources =
  req
    "GroupResources"
    "fixture/GroupResources.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

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

responsePutGroupConfiguration :: PutGroupConfigurationResponse -> TestTree
responsePutGroupConfiguration =
  res
    "PutGroupConfigurationResponse"
    "fixture/PutGroupConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutGroupConfiguration)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroups)

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

responseSearchResources :: SearchResourcesResponse -> TestTree
responseSearchResources =
  res
    "SearchResourcesResponse"
    "fixture/SearchResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy SearchResources)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTags)

responseUpdateGroupQuery :: UpdateGroupQueryResponse -> TestTree
responseUpdateGroupQuery =
  res
    "UpdateGroupQueryResponse"
    "fixture/UpdateGroupQueryResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGroupQuery)

responseListGroupResources :: ListGroupResourcesResponse -> TestTree
responseListGroupResources =
  res
    "ListGroupResourcesResponse"
    "fixture/ListGroupResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroupResources)

responseUntag :: UntagResponse -> TestTree
responseUntag =
  res
    "UntagResponse"
    "fixture/UntagResponse.proto"
    defaultService
    (Proxy :: Proxy Untag)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroup)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGroup)

responseGroupResources :: GroupResourcesResponse -> TestTree
responseGroupResources =
  res
    "GroupResourcesResponse"
    "fixture/GroupResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy GroupResources)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGroup)

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
