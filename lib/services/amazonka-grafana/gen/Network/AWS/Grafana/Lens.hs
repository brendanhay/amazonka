{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Grafana.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Lens
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

import Amazonka.Grafana.AssociateLicense
import Amazonka.Grafana.CreateWorkspace
import Amazonka.Grafana.DeleteWorkspace
import Amazonka.Grafana.DescribeWorkspace
import Amazonka.Grafana.DescribeWorkspaceAuthentication
import Amazonka.Grafana.DisassociateLicense
import Amazonka.Grafana.ListPermissions
import Amazonka.Grafana.ListWorkspaces
import Amazonka.Grafana.Types.AssertionAttributes
import Amazonka.Grafana.Types.AuthenticationDescription
import Amazonka.Grafana.Types.AuthenticationSummary
import Amazonka.Grafana.Types.AwsSsoAuthentication
import Amazonka.Grafana.Types.IdpMetadata
import Amazonka.Grafana.Types.PermissionEntry
import Amazonka.Grafana.Types.RoleValues
import Amazonka.Grafana.Types.SamlAuthentication
import Amazonka.Grafana.Types.SamlConfiguration
import Amazonka.Grafana.Types.UpdateError
import Amazonka.Grafana.Types.UpdateInstruction
import Amazonka.Grafana.Types.User
import Amazonka.Grafana.Types.WorkspaceDescription
import Amazonka.Grafana.Types.WorkspaceSummary
import Amazonka.Grafana.UpdatePermissions
import Amazonka.Grafana.UpdateWorkspace
import Amazonka.Grafana.UpdateWorkspaceAuthentication
