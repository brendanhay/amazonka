{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IdentityStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IdentityStore where

import Amazonka.IdentityStore
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IdentityStore.Internal
import Test.Amazonka.Prelude
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
--         , requestCreateGroupMembership $
--             newCreateGroupMembership
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestDeleteGroupMembership $
--             newDeleteGroupMembership
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDescribeGroup $
--             newDescribeGroup
--
--         , requestDescribeGroupMembership $
--             newDescribeGroupMembership
--
--         , requestDescribeUser $
--             newDescribeUser
--
--         , requestGetGroupId $
--             newGetGroupId
--
--         , requestGetGroupMembershipId $
--             newGetGroupMembershipId
--
--         , requestGetUserId $
--             newGetUserId
--
--         , requestIsMemberInGroups $
--             newIsMemberInGroups
--
--         , requestListGroupMemberships $
--             newListGroupMemberships
--
--         , requestListGroupMembershipsForMember $
--             newListGroupMembershipsForMember
--
--         , requestListGroups $
--             newListGroups
--
--         , requestListUsers $
--             newListUsers
--
--         , requestUpdateGroup $
--             newUpdateGroup
--
--         , requestUpdateUser $
--             newUpdateUser
--
--           ]

--     , testGroup "response"
--         [ responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseCreateGroupMembership $
--             newCreateGroupMembershipResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseDeleteGroupMembership $
--             newDeleteGroupMembershipResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDescribeGroup $
--             newDescribeGroupResponse
--
--         , responseDescribeGroupMembership $
--             newDescribeGroupMembershipResponse
--
--         , responseDescribeUser $
--             newDescribeUserResponse
--
--         , responseGetGroupId $
--             newGetGroupIdResponse
--
--         , responseGetGroupMembershipId $
--             newGetGroupMembershipIdResponse
--
--         , responseGetUserId $
--             newGetUserIdResponse
--
--         , responseIsMemberInGroups $
--             newIsMemberInGroupsResponse
--
--         , responseListGroupMemberships $
--             newListGroupMembershipsResponse
--
--         , responseListGroupMembershipsForMember $
--             newListGroupMembershipsForMemberResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--           ]
--     ]

-- Requests

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestCreateGroupMembership :: CreateGroupMembership -> TestTree
requestCreateGroupMembership =
  req
    "CreateGroupMembership"
    "fixture/CreateGroupMembership.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestDeleteGroupMembership :: DeleteGroupMembership -> TestTree
requestDeleteGroupMembership =
  req
    "DeleteGroupMembership"
    "fixture/DeleteGroupMembership.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestDescribeGroup :: DescribeGroup -> TestTree
requestDescribeGroup =
  req
    "DescribeGroup"
    "fixture/DescribeGroup.yaml"

requestDescribeGroupMembership :: DescribeGroupMembership -> TestTree
requestDescribeGroupMembership =
  req
    "DescribeGroupMembership"
    "fixture/DescribeGroupMembership.yaml"

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser =
  req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

requestGetGroupId :: GetGroupId -> TestTree
requestGetGroupId =
  req
    "GetGroupId"
    "fixture/GetGroupId.yaml"

requestGetGroupMembershipId :: GetGroupMembershipId -> TestTree
requestGetGroupMembershipId =
  req
    "GetGroupMembershipId"
    "fixture/GetGroupMembershipId.yaml"

requestGetUserId :: GetUserId -> TestTree
requestGetUserId =
  req
    "GetUserId"
    "fixture/GetUserId.yaml"

requestIsMemberInGroups :: IsMemberInGroups -> TestTree
requestIsMemberInGroups =
  req
    "IsMemberInGroups"
    "fixture/IsMemberInGroups.yaml"

requestListGroupMemberships :: ListGroupMemberships -> TestTree
requestListGroupMemberships =
  req
    "ListGroupMemberships"
    "fixture/ListGroupMemberships.yaml"

requestListGroupMembershipsForMember :: ListGroupMembershipsForMember -> TestTree
requestListGroupMembershipsForMember =
  req
    "ListGroupMembershipsForMember"
    "fixture/ListGroupMembershipsForMember.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

-- Responses

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroup)

responseCreateGroupMembership :: CreateGroupMembershipResponse -> TestTree
responseCreateGroupMembership =
  res
    "CreateGroupMembershipResponse"
    "fixture/CreateGroupMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroupMembership)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGroup)

responseDeleteGroupMembership :: DeleteGroupMembershipResponse -> TestTree
responseDeleteGroupMembership =
  res
    "DeleteGroupMembershipResponse"
    "fixture/DeleteGroupMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGroupMembership)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseDescribeGroup :: DescribeGroupResponse -> TestTree
responseDescribeGroup =
  res
    "DescribeGroupResponse"
    "fixture/DescribeGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGroup)

responseDescribeGroupMembership :: DescribeGroupMembershipResponse -> TestTree
responseDescribeGroupMembership =
  res
    "DescribeGroupMembershipResponse"
    "fixture/DescribeGroupMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGroupMembership)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser =
  res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUser)

responseGetGroupId :: GetGroupIdResponse -> TestTree
responseGetGroupId =
  res
    "GetGroupIdResponse"
    "fixture/GetGroupIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroupId)

responseGetGroupMembershipId :: GetGroupMembershipIdResponse -> TestTree
responseGetGroupMembershipId =
  res
    "GetGroupMembershipIdResponse"
    "fixture/GetGroupMembershipIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroupMembershipId)

responseGetUserId :: GetUserIdResponse -> TestTree
responseGetUserId =
  res
    "GetUserIdResponse"
    "fixture/GetUserIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUserId)

responseIsMemberInGroups :: IsMemberInGroupsResponse -> TestTree
responseIsMemberInGroups =
  res
    "IsMemberInGroupsResponse"
    "fixture/IsMemberInGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy IsMemberInGroups)

responseListGroupMemberships :: ListGroupMembershipsResponse -> TestTree
responseListGroupMemberships =
  res
    "ListGroupMembershipsResponse"
    "fixture/ListGroupMembershipsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroupMemberships)

responseListGroupMembershipsForMember :: ListGroupMembershipsForMemberResponse -> TestTree
responseListGroupMembershipsForMember =
  res
    "ListGroupMembershipsForMemberResponse"
    "fixture/ListGroupMembershipsForMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroupMembershipsForMember)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroups)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsers)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGroup)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUser)
