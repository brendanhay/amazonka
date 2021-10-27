{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AMP.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AMP.Lens
  ( -- * Operations

    -- ** ListWorkspaces
    listWorkspaces_alias,
    listWorkspaces_nextToken,
    listWorkspaces_maxResults,
    listWorkspacesResponse_nextToken,
    listWorkspacesResponse_httpStatus,
    listWorkspacesResponse_workspaces,

    -- ** CreateAlertManagerDefinition
    createAlertManagerDefinition_clientToken,
    createAlertManagerDefinition_data,
    createAlertManagerDefinition_workspaceId,
    createAlertManagerDefinitionResponse_httpStatus,
    createAlertManagerDefinitionResponse_status,

    -- ** DeleteWorkspace
    deleteWorkspace_clientToken,
    deleteWorkspace_workspaceId,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateRuleGroupsNamespace
    createRuleGroupsNamespace_clientToken,
    createRuleGroupsNamespace_tags,
    createRuleGroupsNamespace_data,
    createRuleGroupsNamespace_name,
    createRuleGroupsNamespace_workspaceId,
    createRuleGroupsNamespaceResponse_tags,
    createRuleGroupsNamespaceResponse_httpStatus,
    createRuleGroupsNamespaceResponse_arn,
    createRuleGroupsNamespaceResponse_name,
    createRuleGroupsNamespaceResponse_status,

    -- ** DescribeAlertManagerDefinition
    describeAlertManagerDefinition_workspaceId,
    describeAlertManagerDefinitionResponse_httpStatus,
    describeAlertManagerDefinitionResponse_alertManagerDefinition,

    -- ** DescribeWorkspace
    describeWorkspace_workspaceId,
    describeWorkspaceResponse_httpStatus,
    describeWorkspaceResponse_workspace,

    -- ** PutAlertManagerDefinition
    putAlertManagerDefinition_clientToken,
    putAlertManagerDefinition_data,
    putAlertManagerDefinition_workspaceId,
    putAlertManagerDefinitionResponse_httpStatus,
    putAlertManagerDefinitionResponse_status,

    -- ** DeleteAlertManagerDefinition
    deleteAlertManagerDefinition_clientToken,
    deleteAlertManagerDefinition_workspaceId,

    -- ** DescribeRuleGroupsNamespace
    describeRuleGroupsNamespace_name,
    describeRuleGroupsNamespace_workspaceId,
    describeRuleGroupsNamespaceResponse_httpStatus,
    describeRuleGroupsNamespaceResponse_ruleGroupsNamespace,

    -- ** UpdateWorkspaceAlias
    updateWorkspaceAlias_clientToken,
    updateWorkspaceAlias_alias,
    updateWorkspaceAlias_workspaceId,

    -- ** DeleteRuleGroupsNamespace
    deleteRuleGroupsNamespace_clientToken,
    deleteRuleGroupsNamespace_name,
    deleteRuleGroupsNamespace_workspaceId,

    -- ** PutRuleGroupsNamespace
    putRuleGroupsNamespace_clientToken,
    putRuleGroupsNamespace_data,
    putRuleGroupsNamespace_name,
    putRuleGroupsNamespace_workspaceId,
    putRuleGroupsNamespaceResponse_tags,
    putRuleGroupsNamespaceResponse_httpStatus,
    putRuleGroupsNamespaceResponse_arn,
    putRuleGroupsNamespaceResponse_name,
    putRuleGroupsNamespaceResponse_status,

    -- ** ListRuleGroupsNamespaces
    listRuleGroupsNamespaces_nextToken,
    listRuleGroupsNamespaces_name,
    listRuleGroupsNamespaces_maxResults,
    listRuleGroupsNamespaces_workspaceId,
    listRuleGroupsNamespacesResponse_nextToken,
    listRuleGroupsNamespacesResponse_httpStatus,
    listRuleGroupsNamespacesResponse_ruleGroupsNamespaces,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** CreateWorkspace
    createWorkspace_clientToken,
    createWorkspace_alias,
    createWorkspace_tags,
    createWorkspaceResponse_tags,
    createWorkspaceResponse_httpStatus,
    createWorkspaceResponse_arn,
    createWorkspaceResponse_status,
    createWorkspaceResponse_workspaceId,

    -- * Types

    -- ** AlertManagerDefinitionDescription
    alertManagerDefinitionDescription_createdAt,
    alertManagerDefinitionDescription_data,
    alertManagerDefinitionDescription_modifiedAt,
    alertManagerDefinitionDescription_status,

    -- ** AlertManagerDefinitionStatus
    alertManagerDefinitionStatus_statusReason,
    alertManagerDefinitionStatus_statusCode,

    -- ** RuleGroupsNamespaceDescription
    ruleGroupsNamespaceDescription_tags,
    ruleGroupsNamespaceDescription_arn,
    ruleGroupsNamespaceDescription_createdAt,
    ruleGroupsNamespaceDescription_data,
    ruleGroupsNamespaceDescription_modifiedAt,
    ruleGroupsNamespaceDescription_name,
    ruleGroupsNamespaceDescription_status,

    -- ** RuleGroupsNamespaceStatus
    ruleGroupsNamespaceStatus_statusReason,
    ruleGroupsNamespaceStatus_statusCode,

    -- ** RuleGroupsNamespaceSummary
    ruleGroupsNamespaceSummary_tags,
    ruleGroupsNamespaceSummary_arn,
    ruleGroupsNamespaceSummary_createdAt,
    ruleGroupsNamespaceSummary_modifiedAt,
    ruleGroupsNamespaceSummary_name,
    ruleGroupsNamespaceSummary_status,

    -- ** WorkspaceDescription
    workspaceDescription_alias,
    workspaceDescription_prometheusEndpoint,
    workspaceDescription_tags,
    workspaceDescription_arn,
    workspaceDescription_createdAt,
    workspaceDescription_status,
    workspaceDescription_workspaceId,

    -- ** WorkspaceStatus
    workspaceStatus_statusCode,

    -- ** WorkspaceSummary
    workspaceSummary_alias,
    workspaceSummary_tags,
    workspaceSummary_arn,
    workspaceSummary_createdAt,
    workspaceSummary_status,
    workspaceSummary_workspaceId,
  )
where

import Network.AWS.AMP.CreateAlertManagerDefinition
import Network.AWS.AMP.CreateRuleGroupsNamespace
import Network.AWS.AMP.CreateWorkspace
import Network.AWS.AMP.DeleteAlertManagerDefinition
import Network.AWS.AMP.DeleteRuleGroupsNamespace
import Network.AWS.AMP.DeleteWorkspace
import Network.AWS.AMP.DescribeAlertManagerDefinition
import Network.AWS.AMP.DescribeRuleGroupsNamespace
import Network.AWS.AMP.DescribeWorkspace
import Network.AWS.AMP.ListRuleGroupsNamespaces
import Network.AWS.AMP.ListTagsForResource
import Network.AWS.AMP.ListWorkspaces
import Network.AWS.AMP.PutAlertManagerDefinition
import Network.AWS.AMP.PutRuleGroupsNamespace
import Network.AWS.AMP.TagResource
import Network.AWS.AMP.Types.AlertManagerDefinitionDescription
import Network.AWS.AMP.Types.AlertManagerDefinitionStatus
import Network.AWS.AMP.Types.RuleGroupsNamespaceDescription
import Network.AWS.AMP.Types.RuleGroupsNamespaceStatus
import Network.AWS.AMP.Types.RuleGroupsNamespaceSummary
import Network.AWS.AMP.Types.WorkspaceDescription
import Network.AWS.AMP.Types.WorkspaceStatus
import Network.AWS.AMP.Types.WorkspaceSummary
import Network.AWS.AMP.UntagResource
import Network.AWS.AMP.UpdateWorkspaceAlias
