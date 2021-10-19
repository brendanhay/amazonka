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

    -- ** SearchResources
    searchResources_nextToken,
    searchResources_maxResults,
    searchResources_resourceQuery,
    searchResourcesResponse_queryErrors,
    searchResourcesResponse_nextToken,
    searchResourcesResponse_resourceIdentifiers,
    searchResourcesResponse_httpStatus,

    -- ** GetTags
    getTags_arn,
    getTagsResponse_arn,
    getTagsResponse_tags,
    getTagsResponse_httpStatus,

    -- ** Tag
    tag_arn,
    tag_tags,
    tagResponse_arn,
    tagResponse_tags,
    tagResponse_httpStatus,

    -- ** UngroupResources
    ungroupResources_group,
    ungroupResources_resourceArns,
    ungroupResourcesResponse_pending,
    ungroupResourcesResponse_succeeded,
    ungroupResourcesResponse_failed,
    ungroupResourcesResponse_httpStatus,

    -- ** GroupResources
    groupResources_group,
    groupResources_resourceArns,
    groupResourcesResponse_pending,
    groupResourcesResponse_succeeded,
    groupResourcesResponse_failed,
    groupResourcesResponse_httpStatus,

    -- ** PutGroupConfiguration
    putGroupConfiguration_group,
    putGroupConfiguration_configuration,
    putGroupConfigurationResponse_httpStatus,

    -- ** Untag
    untag_arn,
    untag_keys,
    untagResponse_arn,
    untagResponse_keys,
    untagResponse_httpStatus,

    -- ** UpdateGroupQuery
    updateGroupQuery_group,
    updateGroupQuery_groupName,
    updateGroupQuery_resourceQuery,
    updateGroupQueryResponse_groupQuery,
    updateGroupQueryResponse_httpStatus,

    -- ** ListGroupResources
    listGroupResources_group,
    listGroupResources_filters,
    listGroupResources_nextToken,
    listGroupResources_groupName,
    listGroupResources_maxResults,
    listGroupResourcesResponse_resources,
    listGroupResourcesResponse_queryErrors,
    listGroupResourcesResponse_nextToken,
    listGroupResourcesResponse_resourceIdentifiers,
    listGroupResourcesResponse_httpStatus,

    -- ** GetGroupQuery
    getGroupQuery_group,
    getGroupQuery_groupName,
    getGroupQueryResponse_groupQuery,
    getGroupQueryResponse_httpStatus,

    -- ** CreateGroup
    createGroup_resourceQuery,
    createGroup_configuration,
    createGroup_description,
    createGroup_tags,
    createGroup_name,
    createGroupResponse_group,
    createGroupResponse_groupConfiguration,
    createGroupResponse_resourceQuery,
    createGroupResponse_tags,
    createGroupResponse_httpStatus,

    -- ** DeleteGroup
    deleteGroup_group,
    deleteGroup_groupName,
    deleteGroupResponse_group,
    deleteGroupResponse_httpStatus,

    -- ** UpdateGroup
    updateGroup_group,
    updateGroup_groupName,
    updateGroup_description,
    updateGroupResponse_group,
    updateGroupResponse_httpStatus,

    -- ** ListGroups
    listGroups_filters,
    listGroups_nextToken,
    listGroups_maxResults,
    listGroupsResponse_groups,
    listGroupsResponse_nextToken,
    listGroupsResponse_groupIdentifiers,
    listGroupsResponse_httpStatus,

    -- ** GetGroup
    getGroup_group,
    getGroup_groupName,
    getGroupResponse_group,
    getGroupResponse_httpStatus,

    -- ** GetGroupConfiguration
    getGroupConfiguration_group,
    getGroupConfigurationResponse_groupConfiguration,
    getGroupConfigurationResponse_httpStatus,

    -- * Types

    -- ** FailedResource
    failedResource_resourceArn,
    failedResource_errorCode,
    failedResource_errorMessage,

    -- ** Group
    group_description,
    group_groupArn,
    group_name,

    -- ** GroupConfiguration
    groupConfiguration_status,
    groupConfiguration_failureReason,
    groupConfiguration_proposedConfiguration,
    groupConfiguration_configuration,

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
    groupIdentifier_groupArn,
    groupIdentifier_groupName,

    -- ** GroupQuery
    groupQuery_groupName,
    groupQuery_resourceQuery,

    -- ** ListGroupResourcesItem
    listGroupResourcesItem_status,
    listGroupResourcesItem_identifier,

    -- ** PendingResource
    pendingResource_resourceArn,

    -- ** QueryError
    queryError_errorCode,
    queryError_message,

    -- ** ResourceFilter
    resourceFilter_name,
    resourceFilter_values,

    -- ** ResourceIdentifier
    resourceIdentifier_resourceType,
    resourceIdentifier_resourceArn,

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
