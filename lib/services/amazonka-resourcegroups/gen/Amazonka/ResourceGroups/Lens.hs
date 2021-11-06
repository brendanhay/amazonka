{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ResourceGroups.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroups.Lens
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

import Amazonka.ResourceGroups.CreateGroup
import Amazonka.ResourceGroups.DeleteGroup
import Amazonka.ResourceGroups.GetGroup
import Amazonka.ResourceGroups.GetGroupConfiguration
import Amazonka.ResourceGroups.GetGroupQuery
import Amazonka.ResourceGroups.GetTags
import Amazonka.ResourceGroups.GroupResources
import Amazonka.ResourceGroups.ListGroupResources
import Amazonka.ResourceGroups.ListGroups
import Amazonka.ResourceGroups.PutGroupConfiguration
import Amazonka.ResourceGroups.SearchResources
import Amazonka.ResourceGroups.Tag
import Amazonka.ResourceGroups.Types.FailedResource
import Amazonka.ResourceGroups.Types.Group
import Amazonka.ResourceGroups.Types.GroupConfiguration
import Amazonka.ResourceGroups.Types.GroupConfigurationItem
import Amazonka.ResourceGroups.Types.GroupConfigurationParameter
import Amazonka.ResourceGroups.Types.GroupFilter
import Amazonka.ResourceGroups.Types.GroupIdentifier
import Amazonka.ResourceGroups.Types.GroupQuery
import Amazonka.ResourceGroups.Types.ListGroupResourcesItem
import Amazonka.ResourceGroups.Types.PendingResource
import Amazonka.ResourceGroups.Types.QueryError
import Amazonka.ResourceGroups.Types.ResourceFilter
import Amazonka.ResourceGroups.Types.ResourceIdentifier
import Amazonka.ResourceGroups.Types.ResourceQuery
import Amazonka.ResourceGroups.Types.ResourceStatus
import Amazonka.ResourceGroups.UngroupResources
import Amazonka.ResourceGroups.Untag
import Amazonka.ResourceGroups.UpdateGroup
import Amazonka.ResourceGroups.UpdateGroupQuery
