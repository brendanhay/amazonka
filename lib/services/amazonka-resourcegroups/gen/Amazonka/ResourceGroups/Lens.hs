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

    -- ** CreateGroup
    createGroup_tags,
    createGroup_configuration,
    createGroup_description,
    createGroup_resourceQuery,
    createGroup_name,
    createGroupResponse_tags,
    createGroupResponse_groupConfiguration,
    createGroupResponse_resourceQuery,
    createGroupResponse_group,
    createGroupResponse_httpStatus,

    -- ** DeleteGroup
    deleteGroup_groupName,
    deleteGroup_group,
    deleteGroupResponse_group,
    deleteGroupResponse_httpStatus,

    -- ** GetGroup
    getGroup_groupName,
    getGroup_group,
    getGroupResponse_group,
    getGroupResponse_httpStatus,

    -- ** GetGroupConfiguration
    getGroupConfiguration_group,
    getGroupConfigurationResponse_groupConfiguration,
    getGroupConfigurationResponse_httpStatus,

    -- ** GetGroupQuery
    getGroupQuery_groupName,
    getGroupQuery_group,
    getGroupQueryResponse_groupQuery,
    getGroupQueryResponse_httpStatus,

    -- ** GetTags
    getTags_arn,
    getTagsResponse_tags,
    getTagsResponse_arn,
    getTagsResponse_httpStatus,

    -- ** GroupResources
    groupResources_group,
    groupResources_resourceArns,
    groupResourcesResponse_failed,
    groupResourcesResponse_succeeded,
    groupResourcesResponse_pending,
    groupResourcesResponse_httpStatus,

    -- ** ListGroupResources
    listGroupResources_nextToken,
    listGroupResources_filters,
    listGroupResources_groupName,
    listGroupResources_maxResults,
    listGroupResources_group,
    listGroupResourcesResponse_resourceIdentifiers,
    listGroupResourcesResponse_nextToken,
    listGroupResourcesResponse_queryErrors,
    listGroupResourcesResponse_resources,
    listGroupResourcesResponse_httpStatus,

    -- ** ListGroups
    listGroups_nextToken,
    listGroups_filters,
    listGroups_maxResults,
    listGroupsResponse_nextToken,
    listGroupsResponse_groups,
    listGroupsResponse_groupIdentifiers,
    listGroupsResponse_httpStatus,

    -- ** PutGroupConfiguration
    putGroupConfiguration_configuration,
    putGroupConfiguration_group,
    putGroupConfigurationResponse_httpStatus,

    -- ** SearchResources
    searchResources_nextToken,
    searchResources_maxResults,
    searchResources_resourceQuery,
    searchResourcesResponse_resourceIdentifiers,
    searchResourcesResponse_nextToken,
    searchResourcesResponse_queryErrors,
    searchResourcesResponse_httpStatus,

    -- ** Tag
    tag_arn,
    tag_tags,
    tagResponse_tags,
    tagResponse_arn,
    tagResponse_httpStatus,

    -- ** UngroupResources
    ungroupResources_group,
    ungroupResources_resourceArns,
    ungroupResourcesResponse_failed,
    ungroupResourcesResponse_succeeded,
    ungroupResourcesResponse_pending,
    ungroupResourcesResponse_httpStatus,

    -- ** Untag
    untag_arn,
    untag_keys,
    untagResponse_arn,
    untagResponse_keys,
    untagResponse_httpStatus,

    -- ** UpdateGroup
    updateGroup_groupName,
    updateGroup_description,
    updateGroup_group,
    updateGroupResponse_group,
    updateGroupResponse_httpStatus,

    -- ** UpdateGroupQuery
    updateGroupQuery_groupName,
    updateGroupQuery_group,
    updateGroupQuery_resourceQuery,
    updateGroupQueryResponse_groupQuery,
    updateGroupQueryResponse_httpStatus,

    -- * Types

    -- ** FailedResource
    failedResource_errorMessage,
    failedResource_errorCode,
    failedResource_resourceArn,

    -- ** Group
    group_description,
    group_groupArn,
    group_name,

    -- ** GroupConfiguration
    groupConfiguration_proposedConfiguration,
    groupConfiguration_configuration,
    groupConfiguration_status,
    groupConfiguration_failureReason,

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
