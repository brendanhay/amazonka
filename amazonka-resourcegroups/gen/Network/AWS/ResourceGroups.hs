{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Resource Groups
--
-- AWS Resource Groups lets you organize AWS resources such as Amazon EC2
-- instances, Amazon Relational Database Service databases, and Amazon S3
-- buckets into groups using criteria that you define as tags. A resource
-- group is a collection of resources that match the resource types
-- specified in a query, and share one or more tags or portions of tags.
-- You can create a group of resources based on their roles in your cloud
-- infrastructure, lifecycle stages, regions, application layers, or
-- virtually any criteria. Resource Groups enable you to automate
-- management tasks, such as those in AWS Systems Manager Automation
-- documents, on tag-related resources in AWS Systems Manager. Groups of
-- tagged resources also let you quickly view a custom console in AWS
-- Systems Manager that shows AWS Config compliance and other monitoring
-- data about member resources.
--
-- To create a resource group, build a resource query, and specify tags
-- that identify the criteria that members of the group have in common.
-- Tags are key-value pairs.
--
-- For more information about Resource Groups, see the
-- <https://docs.aws.amazon.com/ARG/latest/userguide/welcome.html AWS Resource Groups User Guide>.
--
-- AWS Resource Groups uses a REST-compliant API that you can use to
-- perform the following types of operations.
--
-- -   Create, Read, Update, and Delete (CRUD) operations on resource
--     groups and resource query entities
--
-- -   Applying, editing, and removing tags from resource groups
--
-- -   Resolving resource group member ARNs so they can be returned as
--     search results
--
-- -   Getting data about resources that are members of a group
--
-- -   Searching AWS resources based on a resource query
module Network.AWS.ResourceGroups
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotFoundException
    _NotFoundException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** MethodNotAllowedException
    _MethodNotAllowedException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetGroupConfiguration
    GetGroupConfiguration (GetGroupConfiguration'),
    newGetGroupConfiguration,
    GetGroupConfigurationResponse (GetGroupConfigurationResponse'),
    newGetGroupConfigurationResponse,

    -- ** ListGroups (Paginated)
    ListGroups (ListGroups'),
    newListGroups,
    ListGroupsResponse (ListGroupsResponse'),
    newListGroupsResponse,

    -- ** PutGroupConfiguration
    PutGroupConfiguration (PutGroupConfiguration'),
    newPutGroupConfiguration,
    PutGroupConfigurationResponse (PutGroupConfigurationResponse'),
    newPutGroupConfigurationResponse,

    -- ** CreateGroup
    CreateGroup (CreateGroup'),
    newCreateGroup,
    CreateGroupResponse (CreateGroupResponse'),
    newCreateGroupResponse,

    -- ** GetGroupQuery
    GetGroupQuery (GetGroupQuery'),
    newGetGroupQuery,
    GetGroupQueryResponse (GetGroupQueryResponse'),
    newGetGroupQueryResponse,

    -- ** GetTags
    GetTags (GetTags'),
    newGetTags,
    GetTagsResponse (GetTagsResponse'),
    newGetTagsResponse,

    -- ** SearchResources (Paginated)
    SearchResources (SearchResources'),
    newSearchResources,
    SearchResourcesResponse (SearchResourcesResponse'),
    newSearchResourcesResponse,

    -- ** ListGroupResources (Paginated)
    ListGroupResources (ListGroupResources'),
    newListGroupResources,
    ListGroupResourcesResponse (ListGroupResourcesResponse'),
    newListGroupResourcesResponse,

    -- ** UpdateGroupQuery
    UpdateGroupQuery (UpdateGroupQuery'),
    newUpdateGroupQuery,
    UpdateGroupQueryResponse (UpdateGroupQueryResponse'),
    newUpdateGroupQueryResponse,

    -- ** GetGroup
    GetGroup (GetGroup'),
    newGetGroup,
    GetGroupResponse (GetGroupResponse'),
    newGetGroupResponse,

    -- ** Untag
    Untag (Untag'),
    newUntag,
    UntagResponse (UntagResponse'),
    newUntagResponse,

    -- ** UpdateGroup
    UpdateGroup (UpdateGroup'),
    newUpdateGroup,
    UpdateGroupResponse (UpdateGroupResponse'),
    newUpdateGroupResponse,

    -- ** GroupResources
    GroupResources (GroupResources'),
    newGroupResources,
    GroupResourcesResponse (GroupResourcesResponse'),
    newGroupResourcesResponse,

    -- ** DeleteGroup
    DeleteGroup (DeleteGroup'),
    newDeleteGroup,
    DeleteGroupResponse (DeleteGroupResponse'),
    newDeleteGroupResponse,

    -- ** UngroupResources
    UngroupResources (UngroupResources'),
    newUngroupResources,
    UngroupResourcesResponse (UngroupResourcesResponse'),
    newUngroupResourcesResponse,

    -- ** Tag
    Tag (Tag'),
    newTag,
    TagResponse (TagResponse'),
    newTagResponse,

    -- * Types

    -- ** GroupConfigurationStatus
    GroupConfigurationStatus (..),

    -- ** GroupFilterName
    GroupFilterName (..),

    -- ** QueryErrorCode
    QueryErrorCode (..),

    -- ** QueryType
    QueryType (..),

    -- ** ResourceFilterName
    ResourceFilterName (..),

    -- ** ResourceStatusValue
    ResourceStatusValue (..),

    -- ** FailedResource
    FailedResource (FailedResource'),
    newFailedResource,

    -- ** Group
    Group (Group'),
    newGroup,

    -- ** GroupConfiguration
    GroupConfiguration (GroupConfiguration'),
    newGroupConfiguration,

    -- ** GroupConfigurationItem
    GroupConfigurationItem (GroupConfigurationItem'),
    newGroupConfigurationItem,

    -- ** GroupConfigurationParameter
    GroupConfigurationParameter (GroupConfigurationParameter'),
    newGroupConfigurationParameter,

    -- ** GroupFilter
    GroupFilter (GroupFilter'),
    newGroupFilter,

    -- ** GroupIdentifier
    GroupIdentifier (GroupIdentifier'),
    newGroupIdentifier,

    -- ** GroupQuery
    GroupQuery (GroupQuery'),
    newGroupQuery,

    -- ** ListGroupResourcesItem
    ListGroupResourcesItem (ListGroupResourcesItem'),
    newListGroupResourcesItem,

    -- ** PendingResource
    PendingResource (PendingResource'),
    newPendingResource,

    -- ** QueryError
    QueryError (QueryError'),
    newQueryError,

    -- ** ResourceFilter
    ResourceFilter (ResourceFilter'),
    newResourceFilter,

    -- ** ResourceIdentifier
    ResourceIdentifier (ResourceIdentifier'),
    newResourceIdentifier,

    -- ** ResourceQuery
    ResourceQuery (ResourceQuery'),
    newResourceQuery,

    -- ** ResourceStatus
    ResourceStatus (ResourceStatus'),
    newResourceStatus,
  )
where

import Network.AWS.ResourceGroups.CreateGroup
import Network.AWS.ResourceGroups.DeleteGroup
import Network.AWS.ResourceGroups.GetGroup
import Network.AWS.ResourceGroups.GetGroupConfiguration
import Network.AWS.ResourceGroups.GetGroupQuery
import Network.AWS.ResourceGroups.GetTags
import Network.AWS.ResourceGroups.GroupResources
import Network.AWS.ResourceGroups.Lens
import Network.AWS.ResourceGroups.ListGroupResources
import Network.AWS.ResourceGroups.ListGroups
import Network.AWS.ResourceGroups.PutGroupConfiguration
import Network.AWS.ResourceGroups.SearchResources
import Network.AWS.ResourceGroups.Tag
import Network.AWS.ResourceGroups.Types
import Network.AWS.ResourceGroups.UngroupResources
import Network.AWS.ResourceGroups.Untag
import Network.AWS.ResourceGroups.UpdateGroup
import Network.AWS.ResourceGroups.UpdateGroupQuery
import Network.AWS.ResourceGroups.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ResourceGroups'.

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
