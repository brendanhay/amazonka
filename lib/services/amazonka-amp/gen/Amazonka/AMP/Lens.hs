{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AMP.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AMP.Lens
  ( -- * Operations

    -- ** CreateAlertManagerDefinition
    createAlertManagerDefinition_clientToken,
    createAlertManagerDefinition_data,
    createAlertManagerDefinition_workspaceId,
    createAlertManagerDefinitionResponse_httpStatus,
    createAlertManagerDefinitionResponse_status,

    -- ** CreateLoggingConfiguration
    createLoggingConfiguration_clientToken,
    createLoggingConfiguration_logGroupArn,
    createLoggingConfiguration_workspaceId,
    createLoggingConfigurationResponse_httpStatus,
    createLoggingConfigurationResponse_status,

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

    -- ** CreateWorkspace
    createWorkspace_alias,
    createWorkspace_clientToken,
    createWorkspace_tags,
    createWorkspaceResponse_tags,
    createWorkspaceResponse_httpStatus,
    createWorkspaceResponse_arn,
    createWorkspaceResponse_status,
    createWorkspaceResponse_workspaceId,

    -- ** DeleteAlertManagerDefinition
    deleteAlertManagerDefinition_clientToken,
    deleteAlertManagerDefinition_workspaceId,

    -- ** DeleteLoggingConfiguration
    deleteLoggingConfiguration_clientToken,
    deleteLoggingConfiguration_workspaceId,

    -- ** DeleteRuleGroupsNamespace
    deleteRuleGroupsNamespace_clientToken,
    deleteRuleGroupsNamespace_name,
    deleteRuleGroupsNamespace_workspaceId,

    -- ** DeleteWorkspace
    deleteWorkspace_clientToken,
    deleteWorkspace_workspaceId,

    -- ** DescribeAlertManagerDefinition
    describeAlertManagerDefinition_workspaceId,
    describeAlertManagerDefinitionResponse_httpStatus,
    describeAlertManagerDefinitionResponse_alertManagerDefinition,

    -- ** DescribeLoggingConfiguration
    describeLoggingConfiguration_workspaceId,
    describeLoggingConfigurationResponse_httpStatus,
    describeLoggingConfigurationResponse_loggingConfiguration,

    -- ** DescribeRuleGroupsNamespace
    describeRuleGroupsNamespace_name,
    describeRuleGroupsNamespace_workspaceId,
    describeRuleGroupsNamespaceResponse_httpStatus,
    describeRuleGroupsNamespaceResponse_ruleGroupsNamespace,

    -- ** DescribeWorkspace
    describeWorkspace_workspaceId,
    describeWorkspaceResponse_httpStatus,
    describeWorkspaceResponse_workspace,

    -- ** ListRuleGroupsNamespaces
    listRuleGroupsNamespaces_maxResults,
    listRuleGroupsNamespaces_name,
    listRuleGroupsNamespaces_nextToken,
    listRuleGroupsNamespaces_workspaceId,
    listRuleGroupsNamespacesResponse_nextToken,
    listRuleGroupsNamespacesResponse_httpStatus,
    listRuleGroupsNamespacesResponse_ruleGroupsNamespaces,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWorkspaces
    listWorkspaces_alias,
    listWorkspaces_maxResults,
    listWorkspaces_nextToken,
    listWorkspacesResponse_nextToken,
    listWorkspacesResponse_httpStatus,
    listWorkspacesResponse_workspaces,

    -- ** PutAlertManagerDefinition
    putAlertManagerDefinition_clientToken,
    putAlertManagerDefinition_data,
    putAlertManagerDefinition_workspaceId,
    putAlertManagerDefinitionResponse_httpStatus,
    putAlertManagerDefinitionResponse_status,

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

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateLoggingConfiguration
    updateLoggingConfiguration_clientToken,
    updateLoggingConfiguration_logGroupArn,
    updateLoggingConfiguration_workspaceId,
    updateLoggingConfigurationResponse_httpStatus,
    updateLoggingConfigurationResponse_status,

    -- ** UpdateWorkspaceAlias
    updateWorkspaceAlias_alias,
    updateWorkspaceAlias_clientToken,
    updateWorkspaceAlias_workspaceId,

    -- * Types

    -- ** AlertManagerDefinitionDescription
    alertManagerDefinitionDescription_createdAt,
    alertManagerDefinitionDescription_data,
    alertManagerDefinitionDescription_modifiedAt,
    alertManagerDefinitionDescription_status,

    -- ** AlertManagerDefinitionStatus
    alertManagerDefinitionStatus_statusReason,
    alertManagerDefinitionStatus_statusCode,

    -- ** LoggingConfigurationMetadata
    loggingConfigurationMetadata_createdAt,
    loggingConfigurationMetadata_logGroupArn,
    loggingConfigurationMetadata_modifiedAt,
    loggingConfigurationMetadata_status,
    loggingConfigurationMetadata_workspace,

    -- ** LoggingConfigurationStatus
    loggingConfigurationStatus_statusReason,
    loggingConfigurationStatus_statusCode,

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

import Amazonka.AMP.CreateAlertManagerDefinition
import Amazonka.AMP.CreateLoggingConfiguration
import Amazonka.AMP.CreateRuleGroupsNamespace
import Amazonka.AMP.CreateWorkspace
import Amazonka.AMP.DeleteAlertManagerDefinition
import Amazonka.AMP.DeleteLoggingConfiguration
import Amazonka.AMP.DeleteRuleGroupsNamespace
import Amazonka.AMP.DeleteWorkspace
import Amazonka.AMP.DescribeAlertManagerDefinition
import Amazonka.AMP.DescribeLoggingConfiguration
import Amazonka.AMP.DescribeRuleGroupsNamespace
import Amazonka.AMP.DescribeWorkspace
import Amazonka.AMP.ListRuleGroupsNamespaces
import Amazonka.AMP.ListTagsForResource
import Amazonka.AMP.ListWorkspaces
import Amazonka.AMP.PutAlertManagerDefinition
import Amazonka.AMP.PutRuleGroupsNamespace
import Amazonka.AMP.TagResource
import Amazonka.AMP.Types.AlertManagerDefinitionDescription
import Amazonka.AMP.Types.AlertManagerDefinitionStatus
import Amazonka.AMP.Types.LoggingConfigurationMetadata
import Amazonka.AMP.Types.LoggingConfigurationStatus
import Amazonka.AMP.Types.RuleGroupsNamespaceDescription
import Amazonka.AMP.Types.RuleGroupsNamespaceStatus
import Amazonka.AMP.Types.RuleGroupsNamespaceSummary
import Amazonka.AMP.Types.WorkspaceDescription
import Amazonka.AMP.Types.WorkspaceStatus
import Amazonka.AMP.Types.WorkspaceSummary
import Amazonka.AMP.UntagResource
import Amazonka.AMP.UpdateLoggingConfiguration
import Amazonka.AMP.UpdateWorkspaceAlias
