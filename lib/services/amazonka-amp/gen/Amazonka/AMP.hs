{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.AMP
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-08-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Managed Service for Prometheus
module Amazonka.AMP
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

    -- ** WorkspaceActive
    newWorkspaceActive,

    -- ** WorkspaceDeleted
    newWorkspaceDeleted,

    -- * Operations
    -- $operations

    -- ** CreateAlertManagerDefinition
    CreateAlertManagerDefinition (CreateAlertManagerDefinition'),
    newCreateAlertManagerDefinition,
    CreateAlertManagerDefinitionResponse (CreateAlertManagerDefinitionResponse'),
    newCreateAlertManagerDefinitionResponse,

    -- ** CreateLoggingConfiguration
    CreateLoggingConfiguration (CreateLoggingConfiguration'),
    newCreateLoggingConfiguration,
    CreateLoggingConfigurationResponse (CreateLoggingConfigurationResponse'),
    newCreateLoggingConfigurationResponse,

    -- ** CreateRuleGroupsNamespace
    CreateRuleGroupsNamespace (CreateRuleGroupsNamespace'),
    newCreateRuleGroupsNamespace,
    CreateRuleGroupsNamespaceResponse (CreateRuleGroupsNamespaceResponse'),
    newCreateRuleGroupsNamespaceResponse,

    -- ** CreateWorkspace
    CreateWorkspace (CreateWorkspace'),
    newCreateWorkspace,
    CreateWorkspaceResponse (CreateWorkspaceResponse'),
    newCreateWorkspaceResponse,

    -- ** DeleteAlertManagerDefinition
    DeleteAlertManagerDefinition (DeleteAlertManagerDefinition'),
    newDeleteAlertManagerDefinition,
    DeleteAlertManagerDefinitionResponse (DeleteAlertManagerDefinitionResponse'),
    newDeleteAlertManagerDefinitionResponse,

    -- ** DeleteLoggingConfiguration
    DeleteLoggingConfiguration (DeleteLoggingConfiguration'),
    newDeleteLoggingConfiguration,
    DeleteLoggingConfigurationResponse (DeleteLoggingConfigurationResponse'),
    newDeleteLoggingConfigurationResponse,

    -- ** DeleteRuleGroupsNamespace
    DeleteRuleGroupsNamespace (DeleteRuleGroupsNamespace'),
    newDeleteRuleGroupsNamespace,
    DeleteRuleGroupsNamespaceResponse (DeleteRuleGroupsNamespaceResponse'),
    newDeleteRuleGroupsNamespaceResponse,

    -- ** DeleteWorkspace
    DeleteWorkspace (DeleteWorkspace'),
    newDeleteWorkspace,
    DeleteWorkspaceResponse (DeleteWorkspaceResponse'),
    newDeleteWorkspaceResponse,

    -- ** DescribeAlertManagerDefinition
    DescribeAlertManagerDefinition (DescribeAlertManagerDefinition'),
    newDescribeAlertManagerDefinition,
    DescribeAlertManagerDefinitionResponse (DescribeAlertManagerDefinitionResponse'),
    newDescribeAlertManagerDefinitionResponse,

    -- ** DescribeLoggingConfiguration
    DescribeLoggingConfiguration (DescribeLoggingConfiguration'),
    newDescribeLoggingConfiguration,
    DescribeLoggingConfigurationResponse (DescribeLoggingConfigurationResponse'),
    newDescribeLoggingConfigurationResponse,

    -- ** DescribeRuleGroupsNamespace
    DescribeRuleGroupsNamespace (DescribeRuleGroupsNamespace'),
    newDescribeRuleGroupsNamespace,
    DescribeRuleGroupsNamespaceResponse (DescribeRuleGroupsNamespaceResponse'),
    newDescribeRuleGroupsNamespaceResponse,

    -- ** DescribeWorkspace
    DescribeWorkspace (DescribeWorkspace'),
    newDescribeWorkspace,
    DescribeWorkspaceResponse (DescribeWorkspaceResponse'),
    newDescribeWorkspaceResponse,

    -- ** ListRuleGroupsNamespaces (Paginated)
    ListRuleGroupsNamespaces (ListRuleGroupsNamespaces'),
    newListRuleGroupsNamespaces,
    ListRuleGroupsNamespacesResponse (ListRuleGroupsNamespacesResponse'),
    newListRuleGroupsNamespacesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListWorkspaces (Paginated)
    ListWorkspaces (ListWorkspaces'),
    newListWorkspaces,
    ListWorkspacesResponse (ListWorkspacesResponse'),
    newListWorkspacesResponse,

    -- ** PutAlertManagerDefinition
    PutAlertManagerDefinition (PutAlertManagerDefinition'),
    newPutAlertManagerDefinition,
    PutAlertManagerDefinitionResponse (PutAlertManagerDefinitionResponse'),
    newPutAlertManagerDefinitionResponse,

    -- ** PutRuleGroupsNamespace
    PutRuleGroupsNamespace (PutRuleGroupsNamespace'),
    newPutRuleGroupsNamespace,
    PutRuleGroupsNamespaceResponse (PutRuleGroupsNamespaceResponse'),
    newPutRuleGroupsNamespaceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateLoggingConfiguration
    UpdateLoggingConfiguration (UpdateLoggingConfiguration'),
    newUpdateLoggingConfiguration,
    UpdateLoggingConfigurationResponse (UpdateLoggingConfigurationResponse'),
    newUpdateLoggingConfigurationResponse,

    -- ** UpdateWorkspaceAlias
    UpdateWorkspaceAlias (UpdateWorkspaceAlias'),
    newUpdateWorkspaceAlias,
    UpdateWorkspaceAliasResponse (UpdateWorkspaceAliasResponse'),
    newUpdateWorkspaceAliasResponse,

    -- * Types

    -- ** AlertManagerDefinitionStatusCode
    AlertManagerDefinitionStatusCode (..),

    -- ** LoggingConfigurationStatusCode
    LoggingConfigurationStatusCode (..),

    -- ** RuleGroupsNamespaceStatusCode
    RuleGroupsNamespaceStatusCode (..),

    -- ** WorkspaceStatusCode
    WorkspaceStatusCode (..),

    -- ** AlertManagerDefinitionDescription
    AlertManagerDefinitionDescription (AlertManagerDefinitionDescription'),
    newAlertManagerDefinitionDescription,

    -- ** AlertManagerDefinitionStatus
    AlertManagerDefinitionStatus (AlertManagerDefinitionStatus'),
    newAlertManagerDefinitionStatus,

    -- ** LoggingConfigurationMetadata
    LoggingConfigurationMetadata (LoggingConfigurationMetadata'),
    newLoggingConfigurationMetadata,

    -- ** LoggingConfigurationStatus
    LoggingConfigurationStatus (LoggingConfigurationStatus'),
    newLoggingConfigurationStatus,

    -- ** RuleGroupsNamespaceDescription
    RuleGroupsNamespaceDescription (RuleGroupsNamespaceDescription'),
    newRuleGroupsNamespaceDescription,

    -- ** RuleGroupsNamespaceStatus
    RuleGroupsNamespaceStatus (RuleGroupsNamespaceStatus'),
    newRuleGroupsNamespaceStatus,

    -- ** RuleGroupsNamespaceSummary
    RuleGroupsNamespaceSummary (RuleGroupsNamespaceSummary'),
    newRuleGroupsNamespaceSummary,

    -- ** WorkspaceDescription
    WorkspaceDescription (WorkspaceDescription'),
    newWorkspaceDescription,

    -- ** WorkspaceStatus
    WorkspaceStatus (WorkspaceStatus'),
    newWorkspaceStatus,

    -- ** WorkspaceSummary
    WorkspaceSummary (WorkspaceSummary'),
    newWorkspaceSummary,
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
import Amazonka.AMP.Lens
import Amazonka.AMP.ListRuleGroupsNamespaces
import Amazonka.AMP.ListTagsForResource
import Amazonka.AMP.ListWorkspaces
import Amazonka.AMP.PutAlertManagerDefinition
import Amazonka.AMP.PutRuleGroupsNamespace
import Amazonka.AMP.TagResource
import Amazonka.AMP.Types
import Amazonka.AMP.UntagResource
import Amazonka.AMP.UpdateLoggingConfiguration
import Amazonka.AMP.UpdateWorkspaceAlias
import Amazonka.AMP.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'AMP'.

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
