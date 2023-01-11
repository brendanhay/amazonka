{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IdentityStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-06-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The Identity Store service used by AWS IAM Identity Center (successor to
-- AWS Single Sign-On) provides a single place to retrieve all of your
-- identities (users and groups). For more information, see the
-- <https://docs.aws.amazon.com/singlesignon/latest/userguide/what-is.html IAM Identity Center User Guide>.
--
-- >  <note> <p>Although AWS Single Sign-On was renamed, the <code>sso</code> and <code>identitystore</code> API namespaces will continue to retain their original name for backward compatibility purposes. For more information, see <a href="https://docs.aws.amazon.com/singlesignon/latest/userguide/what-is.html#renamed">IAM Identity Center rename</a>.</p> </note> <p>This reference guide describes the identity store operations that you can call programatically and includes detailed information on data types and errors.</p>
module Amazonka.IdentityStore
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateGroup
    CreateGroup (CreateGroup'),
    newCreateGroup,
    CreateGroupResponse (CreateGroupResponse'),
    newCreateGroupResponse,

    -- ** CreateGroupMembership
    CreateGroupMembership (CreateGroupMembership'),
    newCreateGroupMembership,
    CreateGroupMembershipResponse (CreateGroupMembershipResponse'),
    newCreateGroupMembershipResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** DeleteGroup
    DeleteGroup (DeleteGroup'),
    newDeleteGroup,
    DeleteGroupResponse (DeleteGroupResponse'),
    newDeleteGroupResponse,

    -- ** DeleteGroupMembership
    DeleteGroupMembership (DeleteGroupMembership'),
    newDeleteGroupMembership,
    DeleteGroupMembershipResponse (DeleteGroupMembershipResponse'),
    newDeleteGroupMembershipResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** DescribeGroup
    DescribeGroup (DescribeGroup'),
    newDescribeGroup,
    DescribeGroupResponse (DescribeGroupResponse'),
    newDescribeGroupResponse,

    -- ** DescribeGroupMembership
    DescribeGroupMembership (DescribeGroupMembership'),
    newDescribeGroupMembership,
    DescribeGroupMembershipResponse (DescribeGroupMembershipResponse'),
    newDescribeGroupMembershipResponse,

    -- ** DescribeUser
    DescribeUser (DescribeUser'),
    newDescribeUser,
    DescribeUserResponse (DescribeUserResponse'),
    newDescribeUserResponse,

    -- ** GetGroupId
    GetGroupId (GetGroupId'),
    newGetGroupId,
    GetGroupIdResponse (GetGroupIdResponse'),
    newGetGroupIdResponse,

    -- ** GetGroupMembershipId
    GetGroupMembershipId (GetGroupMembershipId'),
    newGetGroupMembershipId,
    GetGroupMembershipIdResponse (GetGroupMembershipIdResponse'),
    newGetGroupMembershipIdResponse,

    -- ** GetUserId
    GetUserId (GetUserId'),
    newGetUserId,
    GetUserIdResponse (GetUserIdResponse'),
    newGetUserIdResponse,

    -- ** IsMemberInGroups
    IsMemberInGroups (IsMemberInGroups'),
    newIsMemberInGroups,
    IsMemberInGroupsResponse (IsMemberInGroupsResponse'),
    newIsMemberInGroupsResponse,

    -- ** ListGroupMemberships (Paginated)
    ListGroupMemberships (ListGroupMemberships'),
    newListGroupMemberships,
    ListGroupMembershipsResponse (ListGroupMembershipsResponse'),
    newListGroupMembershipsResponse,

    -- ** ListGroupMembershipsForMember (Paginated)
    ListGroupMembershipsForMember (ListGroupMembershipsForMember'),
    newListGroupMembershipsForMember,
    ListGroupMembershipsForMemberResponse (ListGroupMembershipsForMemberResponse'),
    newListGroupMembershipsForMemberResponse,

    -- ** ListGroups (Paginated)
    ListGroups (ListGroups'),
    newListGroups,
    ListGroupsResponse (ListGroupsResponse'),
    newListGroupsResponse,

    -- ** ListUsers (Paginated)
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** UpdateGroup
    UpdateGroup (UpdateGroup'),
    newUpdateGroup,
    UpdateGroupResponse (UpdateGroupResponse'),
    newUpdateGroupResponse,

    -- ** UpdateUser
    UpdateUser (UpdateUser'),
    newUpdateUser,
    UpdateUserResponse (UpdateUserResponse'),
    newUpdateUserResponse,

    -- * Types

    -- ** Address
    Address (Address'),
    newAddress,

    -- ** AlternateIdentifier
    AlternateIdentifier (AlternateIdentifier'),
    newAlternateIdentifier,

    -- ** AttributeOperation
    AttributeOperation (AttributeOperation'),
    newAttributeOperation,

    -- ** AttributeValue
    AttributeValue (AttributeValue'),
    newAttributeValue,

    -- ** Email
    Email (Email'),
    newEmail,

    -- ** ExternalId
    ExternalId (ExternalId'),
    newExternalId,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** Group
    Group (Group'),
    newGroup,

    -- ** GroupMembership
    GroupMembership (GroupMembership'),
    newGroupMembership,

    -- ** GroupMembershipExistenceResult
    GroupMembershipExistenceResult (GroupMembershipExistenceResult'),
    newGroupMembershipExistenceResult,

    -- ** MemberId
    MemberId (MemberId'),
    newMemberId,

    -- ** Name
    Name (Name'),
    newName,

    -- ** PhoneNumber
    PhoneNumber (PhoneNumber'),
    newPhoneNumber,

    -- ** UniqueAttribute
    UniqueAttribute (UniqueAttribute'),
    newUniqueAttribute,

    -- ** User
    User (User'),
    newUser,
  )
where

import Amazonka.IdentityStore.CreateGroup
import Amazonka.IdentityStore.CreateGroupMembership
import Amazonka.IdentityStore.CreateUser
import Amazonka.IdentityStore.DeleteGroup
import Amazonka.IdentityStore.DeleteGroupMembership
import Amazonka.IdentityStore.DeleteUser
import Amazonka.IdentityStore.DescribeGroup
import Amazonka.IdentityStore.DescribeGroupMembership
import Amazonka.IdentityStore.DescribeUser
import Amazonka.IdentityStore.GetGroupId
import Amazonka.IdentityStore.GetGroupMembershipId
import Amazonka.IdentityStore.GetUserId
import Amazonka.IdentityStore.IsMemberInGroups
import Amazonka.IdentityStore.Lens
import Amazonka.IdentityStore.ListGroupMemberships
import Amazonka.IdentityStore.ListGroupMembershipsForMember
import Amazonka.IdentityStore.ListGroups
import Amazonka.IdentityStore.ListUsers
import Amazonka.IdentityStore.Types
import Amazonka.IdentityStore.UpdateGroup
import Amazonka.IdentityStore.UpdateUser
import Amazonka.IdentityStore.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IdentityStore'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
