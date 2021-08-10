{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Lens
  ( -- * Operations

    -- ** GetGroupConfiguration
    getGroupConfiguration_group,
    getGroupConfigurationResponse_groupConfiguration,
    getGroupConfigurationResponse_httpStatus,

    -- ** ListGroups
    listGroups_nextToken,
    listGroups_maxResults,
    listGroups_filters,
    listGroupsResponse_groups,
    listGroupsResponse_nextToken,
    listGroupsResponse_groupIdentifiers,
    listGroupsResponse_httpStatus,

    -- ** PutGroupConfiguration
    putGroupConfiguration_configuration,
    putGroupConfiguration_group,
    putGroupConfigurationResponse_httpStatus,

    -- ** CreateGroup
    createGroup_configuration,
    createGroup_tags,
    createGroup_description,
    createGroup_resourceQuery,
    createGroup_name,
    createGroupResponse_groupConfiguration,
    createGroupResponse_group,
    createGroupResponse_tags,
    createGroupResponse_resourceQuery,
    createGroupResponse_httpStatus,

    -- ** GetGroupQuery
    getGroupQuery_groupName,
    getGroupQuery_group,
    getGroupQueryResponse_groupQuery,
    getGroupQueryResponse_httpStatus,

    -- ** GetTags
    getTags_arn,
    getTagsResponse_arn,
    getTagsResponse_tags,
    getTagsResponse_httpStatus,

    -- ** SearchResources
    searchResources_nextToken,
    searchResources_maxResults,
    searchResources_resourceQuery,
    searchResourcesResponse_nextToken,
    searchResourcesResponse_resourceIdentifiers,
    searchResourcesResponse_queryErrors,
    searchResourcesResponse_httpStatus,

    -- ** ListGroupResources
    listGroupResources_nextToken,
    listGroupResources_maxResults,
    listGroupResources_groupName,
    listGroupResources_group,
    listGroupResources_filters,
    listGroupResourcesResponse_nextToken,
    listGroupResourcesResponse_resourceIdentifiers,
    listGroupResourcesResponse_resources,
    listGroupResourcesResponse_queryErrors,
    listGroupResourcesResponse_httpStatus,

    -- ** UpdateGroupQuery
    updateGroupQuery_groupName,
    updateGroupQuery_group,
    updateGroupQuery_resourceQuery,
    updateGroupQueryResponse_groupQuery,
    updateGroupQueryResponse_httpStatus,

    -- ** GetGroup
    getGroup_groupName,
    getGroup_group,
    getGroupResponse_group,
    getGroupResponse_httpStatus,

    -- ** Untag
    untag_arn,
    untag_keys,
    untagResponse_arn,
    untagResponse_keys,
    untagResponse_httpStatus,

    -- ** UpdateGroup
    updateGroup_groupName,
    updateGroup_group,
    updateGroup_description,
    updateGroupResponse_group,
    updateGroupResponse_httpStatus,

    -- ** GroupResources
    groupResources_group,
    groupResources_resourceArns,
    groupResourcesResponse_succeeded,
    groupResourcesResponse_pending,
    groupResourcesResponse_failed,
    groupResourcesResponse_httpStatus,

    -- ** DeleteGroup
    deleteGroup_groupName,
    deleteGroup_group,
    deleteGroupResponse_group,
    deleteGroupResponse_httpStatus,

    -- ** UngroupResources
    ungroupResources_group,
    ungroupResources_resourceArns,
    ungroupResourcesResponse_succeeded,
    ungroupResourcesResponse_pending,
    ungroupResourcesResponse_failed,
    ungroupResourcesResponse_httpStatus,

    -- ** Tag
    tag_arn,
    tag_tags,
    tagResponse_arn,
    tagResponse_tags,
    tagResponse_httpStatus,

    -- * Types

    -- ** FailedResource
    failedResource_resourceArn,
    failedResource_errorMessage,
    failedResource_errorCode,

    -- ** Group
    group_description,
    group_groupArn,
    group_name,

    -- ** GroupConfiguration
    groupConfiguration_status,
    groupConfiguration_configuration,
    groupConfiguration_failureReason,
    groupConfiguration_proposedConfiguration,

    -- ** GroupConfigurationItem
    groupConfigurationItem_parameters,
    groupConfigurationItem_type,

    -- ** GroupConfigurationParameter
    groupConfigurationParameter_values,
    groupConfigurationParameter_name,

    -- ** GroupFilter
    groupFilter_name,
    groupFilter_values,

    -- ** GroupIdentifier
    groupIdentifier_groupName,
    groupIdentifier_groupArn,

    -- ** GroupQuery
    groupQuery_groupName,
    groupQuery_resourceQuery,

    -- ** ListGroupResourcesItem
    listGroupResourcesItem_status,
    listGroupResourcesItem_identifier,

    -- ** PendingResource
    pendingResource_resourceArn,

    -- ** QueryError
    queryError_message,
    queryError_errorCode,

    -- ** ResourceFilter
    resourceFilter_name,
    resourceFilter_values,

    -- ** ResourceIdentifier
    resourceIdentifier_resourceArn,
    resourceIdentifier_resourceType,

    -- ** ResourceQuery
    resourceQuery_type,
    resourceQuery_searchQuery,

    -- ** ResourceStatus
    resourceStatus_name,
  )
where

import Network.AWS.ResourceGroups.CreateGroup
import Network.AWS.ResourceGroups.DeleteGroup
import Network.AWS.ResourceGroups.GetGroup
import Network.AWS.ResourceGroups.GetGroupConfiguration
import Network.AWS.ResourceGroups.GetGroupQuery
import Network.AWS.ResourceGroups.GetTags
import Network.AWS.ResourceGroups.GroupResources
import Network.AWS.ResourceGroups.ListGroupResources
import Network.AWS.ResourceGroups.ListGroups
import Network.AWS.ResourceGroups.PutGroupConfiguration
import Network.AWS.ResourceGroups.SearchResources
import Network.AWS.ResourceGroups.Tag
import Network.AWS.ResourceGroups.Types.FailedResource
import Network.AWS.ResourceGroups.Types.Group
import Network.AWS.ResourceGroups.Types.GroupConfiguration
import Network.AWS.ResourceGroups.Types.GroupConfigurationItem
import Network.AWS.ResourceGroups.Types.GroupConfigurationParameter
import Network.AWS.ResourceGroups.Types.GroupFilter
import Network.AWS.ResourceGroups.Types.GroupIdentifier
import Network.AWS.ResourceGroups.Types.GroupQuery
import Network.AWS.ResourceGroups.Types.ListGroupResourcesItem
import Network.AWS.ResourceGroups.Types.PendingResource
import Network.AWS.ResourceGroups.Types.QueryError
import Network.AWS.ResourceGroups.Types.ResourceFilter
import Network.AWS.ResourceGroups.Types.ResourceIdentifier
import Network.AWS.ResourceGroups.Types.ResourceQuery
import Network.AWS.ResourceGroups.Types.ResourceStatus
import Network.AWS.ResourceGroups.UngroupResources
import Network.AWS.ResourceGroups.Untag
import Network.AWS.ResourceGroups.UpdateGroup
import Network.AWS.ResourceGroups.UpdateGroupQuery
