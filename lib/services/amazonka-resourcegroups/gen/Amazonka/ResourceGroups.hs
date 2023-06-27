{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ResourceGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-11-27@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Resource Groups lets you organize Amazon Web Services resources such as
-- Amazon Elastic Compute Cloud instances, Amazon Relational Database
-- Service databases, and Amazon Simple Storage Service buckets into groups
-- using criteria that you define as tags. A resource group is a collection
-- of resources that match the resource types specified in a query, and
-- share one or more tags or portions of tags. You can create a group of
-- resources based on their roles in your cloud infrastructure, lifecycle
-- stages, regions, application layers, or virtually any criteria. Resource
-- Groups enable you to automate management tasks, such as those in Amazon
-- Web Services Systems Manager Automation documents, on tag-related
-- resources in Amazon Web Services Systems Manager. Groups of tagged
-- resources also let you quickly view a custom console in Amazon Web
-- Services Systems Manager that shows Config compliance and other
-- monitoring data about member resources.
--
-- To create a resource group, build a resource query, and specify tags
-- that identify the criteria that members of the group have in common.
-- Tags are key-value pairs.
--
-- For more information about Resource Groups, see the
-- <https://docs.aws.amazon.com/ARG/latest/userguide/welcome.html Resource Groups User Guide>.
--
-- Resource Groups uses a REST-compliant API that you can use to perform
-- the following types of operations.
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
-- -   Searching Amazon Web Services resources based on a resource query
module Amazonka.ResourceGroups
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** MethodNotAllowedException
    _MethodNotAllowedException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateGroup
    CreateGroup (CreateGroup'),
    newCreateGroup,
    CreateGroupResponse (CreateGroupResponse'),
    newCreateGroupResponse,

    -- ** DeleteGroup
    DeleteGroup (DeleteGroup'),
    newDeleteGroup,
    DeleteGroupResponse (DeleteGroupResponse'),
    newDeleteGroupResponse,

    -- ** GetAccountSettings
    GetAccountSettings (GetAccountSettings'),
    newGetAccountSettings,
    GetAccountSettingsResponse (GetAccountSettingsResponse'),
    newGetAccountSettingsResponse,

    -- ** GetGroup
    GetGroup (GetGroup'),
    newGetGroup,
    GetGroupResponse (GetGroupResponse'),
    newGetGroupResponse,

    -- ** GetGroupConfiguration
    GetGroupConfiguration (GetGroupConfiguration'),
    newGetGroupConfiguration,
    GetGroupConfigurationResponse (GetGroupConfigurationResponse'),
    newGetGroupConfigurationResponse,

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

    -- ** GroupResources
    GroupResources (GroupResources'),
    newGroupResources,
    GroupResourcesResponse (GroupResourcesResponse'),
    newGroupResourcesResponse,

    -- ** ListGroupResources (Paginated)
    ListGroupResources (ListGroupResources'),
    newListGroupResources,
    ListGroupResourcesResponse (ListGroupResourcesResponse'),
    newListGroupResourcesResponse,

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

    -- ** SearchResources (Paginated)
    SearchResources (SearchResources'),
    newSearchResources,
    SearchResourcesResponse (SearchResourcesResponse'),
    newSearchResourcesResponse,

    -- ** Tag
    Tag (Tag'),
    newTag,
    TagResponse (TagResponse'),
    newTagResponse,

    -- ** UngroupResources
    UngroupResources (UngroupResources'),
    newUngroupResources,
    UngroupResourcesResponse (UngroupResourcesResponse'),
    newUngroupResourcesResponse,

    -- ** Untag
    Untag (Untag'),
    newUntag,
    UntagResponse (UntagResponse'),
    newUntagResponse,

    -- ** UpdateAccountSettings
    UpdateAccountSettings (UpdateAccountSettings'),
    newUpdateAccountSettings,
    UpdateAccountSettingsResponse (UpdateAccountSettingsResponse'),
    newUpdateAccountSettingsResponse,

    -- ** UpdateGroup
    UpdateGroup (UpdateGroup'),
    newUpdateGroup,
    UpdateGroupResponse (UpdateGroupResponse'),
    newUpdateGroupResponse,

    -- ** UpdateGroupQuery
    UpdateGroupQuery (UpdateGroupQuery'),
    newUpdateGroupQuery,
    UpdateGroupQueryResponse (UpdateGroupQueryResponse'),
    newUpdateGroupQueryResponse,

    -- * Types

    -- ** GroupConfigurationStatus
    GroupConfigurationStatus (..),

    -- ** GroupFilterName
    GroupFilterName (..),

    -- ** GroupLifecycleEventsDesiredStatus
    GroupLifecycleEventsDesiredStatus (..),

    -- ** GroupLifecycleEventsStatus
    GroupLifecycleEventsStatus (..),

    -- ** QueryErrorCode
    QueryErrorCode (..),

    -- ** QueryType
    QueryType (..),

    -- ** ResourceFilterName
    ResourceFilterName (..),

    -- ** ResourceStatusValue
    ResourceStatusValue (..),

    -- ** AccountSettings
    AccountSettings (AccountSettings'),
    newAccountSettings,

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

import Amazonka.ResourceGroups.CreateGroup
import Amazonka.ResourceGroups.DeleteGroup
import Amazonka.ResourceGroups.GetAccountSettings
import Amazonka.ResourceGroups.GetGroup
import Amazonka.ResourceGroups.GetGroupConfiguration
import Amazonka.ResourceGroups.GetGroupQuery
import Amazonka.ResourceGroups.GetTags
import Amazonka.ResourceGroups.GroupResources
import Amazonka.ResourceGroups.Lens
import Amazonka.ResourceGroups.ListGroupResources
import Amazonka.ResourceGroups.ListGroups
import Amazonka.ResourceGroups.PutGroupConfiguration
import Amazonka.ResourceGroups.SearchResources
import Amazonka.ResourceGroups.Tag
import Amazonka.ResourceGroups.Types
import Amazonka.ResourceGroups.UngroupResources
import Amazonka.ResourceGroups.Untag
import Amazonka.ResourceGroups.UpdateAccountSettings
import Amazonka.ResourceGroups.UpdateGroup
import Amazonka.ResourceGroups.UpdateGroupQuery
import Amazonka.ResourceGroups.Waiters

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
