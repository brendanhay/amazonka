{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ResourceGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ResourceGroups where

import Amazonka.ResourceGroups
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.ResourceGroups.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateGroup $
--             newCreateGroup
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestGetGroup $
--             newGetGroup
--
--         , requestGetGroupConfiguration $
--             newGetGroupConfiguration
--
--         , requestGetGroupQuery $
--             newGetGroupQuery
--
--         , requestGetTags $
--             newGetTags
--
--         , requestGroupResources $
--             newGroupResources
--
--         , requestListGroupResources $
--             newListGroupResources
--
--         , requestListGroups $
--             newListGroups
--
--         , requestPutGroupConfiguration $
--             newPutGroupConfiguration
--
--         , requestSearchResources $
--             newSearchResources
--
--         , requestTag $
--             newTag
--
--         , requestUngroupResources $
--             newUngroupResources
--
--         , requestUntag $
--             newUntag
--
--         , requestUpdateGroup $
--             newUpdateGroup
--
--         , requestUpdateGroupQuery $
--             newUpdateGroupQuery
--
--           ]

--     , testGroup "response"
--         [ responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseGetGroup $
--             newGetGroupResponse
--
--         , responseGetGroupConfiguration $
--             newGetGroupConfigurationResponse
--
--         , responseGetGroupQuery $
--             newGetGroupQueryResponse
--
--         , responseGetTags $
--             newGetTagsResponse
--
--         , responseGroupResources $
--             newGroupResourcesResponse
--
--         , responseListGroupResources $
--             newListGroupResourcesResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responsePutGroupConfiguration $
--             newPutGroupConfigurationResponse
--
--         , responseSearchResources $
--             newSearchResourcesResponse
--
--         , responseTag $
--             newTagResponse
--
--         , responseUngroupResources $
--             newUngroupResourcesResponse
--
--         , responseUntag $
--             newUntagResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
--
--         , responseUpdateGroupQuery $
--             newUpdateGroupQueryResponse
--
--           ]
--     ]

-- Requests

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

requestGroupResources :: GroupResources -> TestTree
requestGroupResources =
  req
    "GroupResources"
    "fixture/GroupResources.yaml"

requestListGroupResources :: ListGroupResources -> TestTree
requestListGroupResources =
  req
    "ListGroupResources"
    "fixture/ListGroupResources.yaml"

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

requestSearchResources :: SearchResources -> TestTree
requestSearchResources =
  req
    "SearchResources"
    "fixture/SearchResources.yaml"

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

requestUpdateGroupQuery :: UpdateGroupQuery -> TestTree
requestUpdateGroupQuery =
  req
    "UpdateGroupQuery"
    "fixture/UpdateGroupQuery.yaml"

-- Responses

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroup)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGroup)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroup)

responseGetGroupConfiguration :: GetGroupConfigurationResponse -> TestTree
responseGetGroupConfiguration =
  res
    "GetGroupConfigurationResponse"
    "fixture/GetGroupConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroupConfiguration)

responseGetGroupQuery :: GetGroupQueryResponse -> TestTree
responseGetGroupQuery =
  res
    "GetGroupQueryResponse"
    "fixture/GetGroupQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroupQuery)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTags)

responseGroupResources :: GroupResourcesResponse -> TestTree
responseGroupResources =
  res
    "GroupResourcesResponse"
    "fixture/GroupResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GroupResources)

responseListGroupResources :: ListGroupResourcesResponse -> TestTree
responseListGroupResources =
  res
    "ListGroupResourcesResponse"
    "fixture/ListGroupResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroupResources)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroups)

responsePutGroupConfiguration :: PutGroupConfigurationResponse -> TestTree
responsePutGroupConfiguration =
  res
    "PutGroupConfigurationResponse"
    "fixture/PutGroupConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutGroupConfiguration)

responseSearchResources :: SearchResourcesResponse -> TestTree
responseSearchResources =
  res
    "SearchResourcesResponse"
    "fixture/SearchResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchResources)

responseTag :: TagResponse -> TestTree
responseTag =
  res
    "TagResponse"
    "fixture/TagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Tag)

responseUngroupResources :: UngroupResourcesResponse -> TestTree
responseUngroupResources =
  res
    "UngroupResourcesResponse"
    "fixture/UngroupResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UngroupResources)

responseUntag :: UntagResponse -> TestTree
responseUntag =
  res
    "UntagResponse"
    "fixture/UntagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Untag)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGroup)

responseUpdateGroupQuery :: UpdateGroupQueryResponse -> TestTree
responseUpdateGroupQuery =
  res
    "UpdateGroupQueryResponse"
    "fixture/UpdateGroupQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGroupQuery)
