{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Grafana.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Grafana.Lens
  ( -- * Operations

    -- ** ListWorkspaces
    listWorkspaces_nextToken,
    listWorkspaces_maxResults,
    listWorkspacesResponse_nextToken,
    listWorkspacesResponse_httpStatus,
    listWorkspacesResponse_workspaces,

    -- ** DeleteWorkspace
    deleteWorkspace_workspaceId,
    deleteWorkspaceResponse_httpStatus,
    deleteWorkspaceResponse_workspace,

    -- ** UpdateWorkspace
    updateWorkspace_workspaceRoleArn,
    updateWorkspace_workspaceDataSources,
    updateWorkspace_permissionType,
    updateWorkspace_workspaceNotificationDestinations,
    updateWorkspace_workspaceName,
    updateWorkspace_accountAccessType,
    updateWorkspace_organizationRoleName,
    updateWorkspace_workspaceDescription,
    updateWorkspace_stackSetName,
    updateWorkspace_workspaceOrganizationalUnits,
    updateWorkspace_workspaceId,
    updateWorkspaceResponse_httpStatus,
    updateWorkspaceResponse_workspace,

    -- ** UpdateWorkspaceAuthentication
    updateWorkspaceAuthentication_samlConfiguration,
    updateWorkspaceAuthentication_authenticationProviders,
    updateWorkspaceAuthentication_workspaceId,
    updateWorkspaceAuthenticationResponse_httpStatus,
    updateWorkspaceAuthenticationResponse_authentication,

    -- ** DescribeWorkspaceAuthentication
    describeWorkspaceAuthentication_workspaceId,
    describeWorkspaceAuthenticationResponse_httpStatus,
    describeWorkspaceAuthenticationResponse_authentication,

    -- ** DescribeWorkspace
    describeWorkspace_workspaceId,
    describeWorkspaceResponse_httpStatus,
    describeWorkspaceResponse_workspace,

    -- ** AssociateLicense
    associateLicense_licenseType,
    associateLicense_workspaceId,
    associateLicenseResponse_httpStatus,
    associateLicenseResponse_workspace,

    -- ** ListPermissions
    listPermissions_userId,
    listPermissions_nextToken,
    listPermissions_groupId,
    listPermissions_maxResults,
    listPermissions_userType,
    listPermissions_workspaceId,
    listPermissionsResponse_nextToken,
    listPermissionsResponse_httpStatus,
    listPermissionsResponse_permissions,

    -- ** UpdatePermissions
    updatePermissions_updateInstructionBatch,
    updatePermissions_workspaceId,
    updatePermissionsResponse_httpStatus,
    updatePermissionsResponse_errors,

    -- ** DisassociateLicense
    disassociateLicense_licenseType,
    disassociateLicense_workspaceId,
    disassociateLicenseResponse_httpStatus,
    disassociateLicenseResponse_workspace,

    -- ** CreateWorkspace
    createWorkspace_workspaceRoleArn,
    createWorkspace_clientToken,
    createWorkspace_workspaceDataSources,
    createWorkspace_workspaceNotificationDestinations,
    createWorkspace_workspaceName,
    createWorkspace_organizationRoleName,
    createWorkspace_workspaceDescription,
    createWorkspace_stackSetName,
    createWorkspace_workspaceOrganizationalUnits,
    createWorkspace_accountAccessType,
    createWorkspace_authenticationProviders,
    createWorkspace_permissionType,
    createWorkspaceResponse_httpStatus,
    createWorkspaceResponse_workspace,

    -- * Types

    -- ** AssertionAttributes
    assertionAttributes_email,
    assertionAttributes_groups,
    assertionAttributes_org,
    assertionAttributes_role,
    assertionAttributes_name,
    assertionAttributes_login,

    -- ** AuthenticationDescription
    authenticationDescription_awsSso,
    authenticationDescription_saml,
    authenticationDescription_providers,

    -- ** AuthenticationSummary
    authenticationSummary_samlConfigurationStatus,
    authenticationSummary_providers,

    -- ** AwsSsoAuthentication
    awsSsoAuthentication_ssoClientId,

    -- ** IdpMetadata
    idpMetadata_url,
    idpMetadata_xml,

    -- ** PermissionEntry
    permissionEntry_role,
    permissionEntry_user,

    -- ** RoleValues
    roleValues_admin,
    roleValues_editor,

    -- ** SamlAuthentication
    samlAuthentication_configuration,
    samlAuthentication_status,

    -- ** SamlConfiguration
    samlConfiguration_loginValidityDuration,
    samlConfiguration_assertionAttributes,
    samlConfiguration_allowedOrganizations,
    samlConfiguration_roleValues,
    samlConfiguration_idpMetadata,

    -- ** UpdateError
    updateError_causedBy,
    updateError_code,
    updateError_message,

    -- ** UpdateInstruction
    updateInstruction_action,
    updateInstruction_role,
    updateInstruction_users,

    -- ** User
    user_id,
    user_type,

    -- ** WorkspaceDescription
    workspaceDescription_workspaceRoleArn,
    workspaceDescription_freeTrialExpiration,
    workspaceDescription_licenseType,
    workspaceDescription_permissionType,
    workspaceDescription_name,
    workspaceDescription_notificationDestinations,
    workspaceDescription_accountAccessType,
    workspaceDescription_licenseExpiration,
    workspaceDescription_organizationRoleName,
    workspaceDescription_stackSetName,
    workspaceDescription_organizationalUnits,
    workspaceDescription_description,
    workspaceDescription_freeTrialConsumed,
    workspaceDescription_authentication,
    workspaceDescription_created,
    workspaceDescription_dataSources,
    workspaceDescription_endpoint,
    workspaceDescription_grafanaVersion,
    workspaceDescription_id,
    workspaceDescription_modified,
    workspaceDescription_status,

    -- ** WorkspaceSummary
    workspaceSummary_name,
    workspaceSummary_notificationDestinations,
    workspaceSummary_description,
    workspaceSummary_authentication,
    workspaceSummary_created,
    workspaceSummary_endpoint,
    workspaceSummary_grafanaVersion,
    workspaceSummary_id,
    workspaceSummary_modified,
    workspaceSummary_status,
  )
where

import Network.AWS.Grafana.AssociateLicense
import Network.AWS.Grafana.CreateWorkspace
import Network.AWS.Grafana.DeleteWorkspace
import Network.AWS.Grafana.DescribeWorkspace
import Network.AWS.Grafana.DescribeWorkspaceAuthentication
import Network.AWS.Grafana.DisassociateLicense
import Network.AWS.Grafana.ListPermissions
import Network.AWS.Grafana.ListWorkspaces
import Network.AWS.Grafana.Types.AssertionAttributes
import Network.AWS.Grafana.Types.AuthenticationDescription
import Network.AWS.Grafana.Types.AuthenticationSummary
import Network.AWS.Grafana.Types.AwsSsoAuthentication
import Network.AWS.Grafana.Types.IdpMetadata
import Network.AWS.Grafana.Types.PermissionEntry
import Network.AWS.Grafana.Types.RoleValues
import Network.AWS.Grafana.Types.SamlAuthentication
import Network.AWS.Grafana.Types.SamlConfiguration
import Network.AWS.Grafana.Types.UpdateError
import Network.AWS.Grafana.Types.UpdateInstruction
import Network.AWS.Grafana.Types.User
import Network.AWS.Grafana.Types.WorkspaceDescription
import Network.AWS.Grafana.Types.WorkspaceSummary
import Network.AWS.Grafana.UpdatePermissions
import Network.AWS.Grafana.UpdateWorkspace
import Network.AWS.Grafana.UpdateWorkspaceAuthentication
