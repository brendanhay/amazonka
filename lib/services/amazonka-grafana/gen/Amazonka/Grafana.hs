{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Grafana
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-08-18@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Managed Grafana is a fully managed and secure data visualization
-- service that you can use to instantly query, correlate, and visualize
-- operational metrics, logs, and traces from multiple sources. Amazon
-- Managed Grafana makes it easy to deploy, operate, and scale Grafana, a
-- widely deployed data visualization tool that is popular for its
-- extensible data support.
--
-- With Amazon Managed Grafana, you create logically isolated Grafana
-- servers called /workspaces/. In a workspace, you can create Grafana
-- dashboards and visualizations to analyze your metrics, logs, and traces
-- without having to build, package, or deploy any hardware to run Grafana
-- servers.
module Amazonka.Grafana
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

    -- ** AssociateLicense
    AssociateLicense (AssociateLicense'),
    newAssociateLicense,
    AssociateLicenseResponse (AssociateLicenseResponse'),
    newAssociateLicenseResponse,

    -- ** CreateWorkspace
    CreateWorkspace (CreateWorkspace'),
    newCreateWorkspace,
    CreateWorkspaceResponse (CreateWorkspaceResponse'),
    newCreateWorkspaceResponse,

    -- ** CreateWorkspaceApiKey
    CreateWorkspaceApiKey (CreateWorkspaceApiKey'),
    newCreateWorkspaceApiKey,
    CreateWorkspaceApiKeyResponse (CreateWorkspaceApiKeyResponse'),
    newCreateWorkspaceApiKeyResponse,

    -- ** DeleteWorkspace
    DeleteWorkspace (DeleteWorkspace'),
    newDeleteWorkspace,
    DeleteWorkspaceResponse (DeleteWorkspaceResponse'),
    newDeleteWorkspaceResponse,

    -- ** DeleteWorkspaceApiKey
    DeleteWorkspaceApiKey (DeleteWorkspaceApiKey'),
    newDeleteWorkspaceApiKey,
    DeleteWorkspaceApiKeyResponse (DeleteWorkspaceApiKeyResponse'),
    newDeleteWorkspaceApiKeyResponse,

    -- ** DescribeWorkspace
    DescribeWorkspace (DescribeWorkspace'),
    newDescribeWorkspace,
    DescribeWorkspaceResponse (DescribeWorkspaceResponse'),
    newDescribeWorkspaceResponse,

    -- ** DescribeWorkspaceAuthentication
    DescribeWorkspaceAuthentication (DescribeWorkspaceAuthentication'),
    newDescribeWorkspaceAuthentication,
    DescribeWorkspaceAuthenticationResponse (DescribeWorkspaceAuthenticationResponse'),
    newDescribeWorkspaceAuthenticationResponse,

    -- ** DescribeWorkspaceConfiguration
    DescribeWorkspaceConfiguration (DescribeWorkspaceConfiguration'),
    newDescribeWorkspaceConfiguration,
    DescribeWorkspaceConfigurationResponse (DescribeWorkspaceConfigurationResponse'),
    newDescribeWorkspaceConfigurationResponse,

    -- ** DisassociateLicense
    DisassociateLicense (DisassociateLicense'),
    newDisassociateLicense,
    DisassociateLicenseResponse (DisassociateLicenseResponse'),
    newDisassociateLicenseResponse,

    -- ** ListPermissions (Paginated)
    ListPermissions (ListPermissions'),
    newListPermissions,
    ListPermissionsResponse (ListPermissionsResponse'),
    newListPermissionsResponse,

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

    -- ** UpdatePermissions
    UpdatePermissions (UpdatePermissions'),
    newUpdatePermissions,
    UpdatePermissionsResponse (UpdatePermissionsResponse'),
    newUpdatePermissionsResponse,

    -- ** UpdateWorkspace
    UpdateWorkspace (UpdateWorkspace'),
    newUpdateWorkspace,
    UpdateWorkspaceResponse (UpdateWorkspaceResponse'),
    newUpdateWorkspaceResponse,

    -- ** UpdateWorkspaceAuthentication
    UpdateWorkspaceAuthentication (UpdateWorkspaceAuthentication'),
    newUpdateWorkspaceAuthentication,
    UpdateWorkspaceAuthenticationResponse (UpdateWorkspaceAuthenticationResponse'),
    newUpdateWorkspaceAuthenticationResponse,

    -- ** UpdateWorkspaceConfiguration
    UpdateWorkspaceConfiguration (UpdateWorkspaceConfiguration'),
    newUpdateWorkspaceConfiguration,
    UpdateWorkspaceConfigurationResponse (UpdateWorkspaceConfigurationResponse'),
    newUpdateWorkspaceConfigurationResponse,

    -- * Types

    -- ** AccountAccessType
    AccountAccessType (..),

    -- ** AuthenticationProviderTypes
    AuthenticationProviderTypes (..),

    -- ** DataSourceType
    DataSourceType (..),

    -- ** LicenseType
    LicenseType (..),

    -- ** NotificationDestinationType
    NotificationDestinationType (..),

    -- ** PermissionType
    PermissionType (..),

    -- ** Role
    Role (..),

    -- ** SamlConfigurationStatus
    SamlConfigurationStatus (..),

    -- ** UpdateAction
    UpdateAction (..),

    -- ** UserType
    UserType (..),

    -- ** WorkspaceStatus
    WorkspaceStatus (..),

    -- ** AssertionAttributes
    AssertionAttributes (AssertionAttributes'),
    newAssertionAttributes,

    -- ** AuthenticationDescription
    AuthenticationDescription (AuthenticationDescription'),
    newAuthenticationDescription,

    -- ** AuthenticationSummary
    AuthenticationSummary (AuthenticationSummary'),
    newAuthenticationSummary,

    -- ** AwsSsoAuthentication
    AwsSsoAuthentication (AwsSsoAuthentication'),
    newAwsSsoAuthentication,

    -- ** IdpMetadata
    IdpMetadata (IdpMetadata'),
    newIdpMetadata,

    -- ** NetworkAccessConfiguration
    NetworkAccessConfiguration (NetworkAccessConfiguration'),
    newNetworkAccessConfiguration,

    -- ** PermissionEntry
    PermissionEntry (PermissionEntry'),
    newPermissionEntry,

    -- ** RoleValues
    RoleValues (RoleValues'),
    newRoleValues,

    -- ** SamlAuthentication
    SamlAuthentication (SamlAuthentication'),
    newSamlAuthentication,

    -- ** SamlConfiguration
    SamlConfiguration (SamlConfiguration'),
    newSamlConfiguration,

    -- ** UpdateError
    UpdateError (UpdateError'),
    newUpdateError,

    -- ** UpdateInstruction
    UpdateInstruction (UpdateInstruction'),
    newUpdateInstruction,

    -- ** User
    User (User'),
    newUser,

    -- ** VpcConfiguration
    VpcConfiguration (VpcConfiguration'),
    newVpcConfiguration,

    -- ** WorkspaceDescription
    WorkspaceDescription (WorkspaceDescription'),
    newWorkspaceDescription,

    -- ** WorkspaceSummary
    WorkspaceSummary (WorkspaceSummary'),
    newWorkspaceSummary,
  )
where

import Amazonka.Grafana.AssociateLicense
import Amazonka.Grafana.CreateWorkspace
import Amazonka.Grafana.CreateWorkspaceApiKey
import Amazonka.Grafana.DeleteWorkspace
import Amazonka.Grafana.DeleteWorkspaceApiKey
import Amazonka.Grafana.DescribeWorkspace
import Amazonka.Grafana.DescribeWorkspaceAuthentication
import Amazonka.Grafana.DescribeWorkspaceConfiguration
import Amazonka.Grafana.DisassociateLicense
import Amazonka.Grafana.Lens
import Amazonka.Grafana.ListPermissions
import Amazonka.Grafana.ListTagsForResource
import Amazonka.Grafana.ListWorkspaces
import Amazonka.Grafana.TagResource
import Amazonka.Grafana.Types
import Amazonka.Grafana.UntagResource
import Amazonka.Grafana.UpdatePermissions
import Amazonka.Grafana.UpdateWorkspace
import Amazonka.Grafana.UpdateWorkspaceAuthentication
import Amazonka.Grafana.UpdateWorkspaceConfiguration
import Amazonka.Grafana.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Grafana'.

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
