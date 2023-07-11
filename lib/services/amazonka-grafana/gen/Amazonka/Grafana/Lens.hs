{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Grafana.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Lens
  ( -- * Operations

    -- ** AssociateLicense
    associateLicense_licenseType,
    associateLicense_workspaceId,
    associateLicenseResponse_httpStatus,
    associateLicenseResponse_workspace,

    -- ** CreateWorkspace
    createWorkspace_clientToken,
    createWorkspace_configuration,
    createWorkspace_organizationRoleName,
    createWorkspace_stackSetName,
    createWorkspace_tags,
    createWorkspace_vpcConfiguration,
    createWorkspace_workspaceDataSources,
    createWorkspace_workspaceDescription,
    createWorkspace_workspaceName,
    createWorkspace_workspaceNotificationDestinations,
    createWorkspace_workspaceOrganizationalUnits,
    createWorkspace_workspaceRoleArn,
    createWorkspace_accountAccessType,
    createWorkspace_authenticationProviders,
    createWorkspace_permissionType,
    createWorkspaceResponse_httpStatus,
    createWorkspaceResponse_workspace,

    -- ** CreateWorkspaceApiKey
    createWorkspaceApiKey_keyName,
    createWorkspaceApiKey_keyRole,
    createWorkspaceApiKey_secondsToLive,
    createWorkspaceApiKey_workspaceId,
    createWorkspaceApiKeyResponse_httpStatus,
    createWorkspaceApiKeyResponse_key,
    createWorkspaceApiKeyResponse_keyName,
    createWorkspaceApiKeyResponse_workspaceId,

    -- ** DeleteWorkspace
    deleteWorkspace_workspaceId,
    deleteWorkspaceResponse_httpStatus,
    deleteWorkspaceResponse_workspace,

    -- ** DeleteWorkspaceApiKey
    deleteWorkspaceApiKey_keyName,
    deleteWorkspaceApiKey_workspaceId,
    deleteWorkspaceApiKeyResponse_httpStatus,
    deleteWorkspaceApiKeyResponse_keyName,
    deleteWorkspaceApiKeyResponse_workspaceId,

    -- ** DescribeWorkspace
    describeWorkspace_workspaceId,
    describeWorkspaceResponse_httpStatus,
    describeWorkspaceResponse_workspace,

    -- ** DescribeWorkspaceAuthentication
    describeWorkspaceAuthentication_workspaceId,
    describeWorkspaceAuthenticationResponse_httpStatus,
    describeWorkspaceAuthenticationResponse_authentication,

    -- ** DescribeWorkspaceConfiguration
    describeWorkspaceConfiguration_workspaceId,
    describeWorkspaceConfigurationResponse_httpStatus,
    describeWorkspaceConfigurationResponse_configuration,

    -- ** DisassociateLicense
    disassociateLicense_licenseType,
    disassociateLicense_workspaceId,
    disassociateLicenseResponse_httpStatus,
    disassociateLicenseResponse_workspace,

    -- ** ListPermissions
    listPermissions_groupId,
    listPermissions_maxResults,
    listPermissions_nextToken,
    listPermissions_userId,
    listPermissions_userType,
    listPermissions_workspaceId,
    listPermissionsResponse_nextToken,
    listPermissionsResponse_httpStatus,
    listPermissionsResponse_permissions,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWorkspaces
    listWorkspaces_maxResults,
    listWorkspaces_nextToken,
    listWorkspacesResponse_nextToken,
    listWorkspacesResponse_httpStatus,
    listWorkspacesResponse_workspaces,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdatePermissions
    updatePermissions_updateInstructionBatch,
    updatePermissions_workspaceId,
    updatePermissionsResponse_httpStatus,
    updatePermissionsResponse_errors,

    -- ** UpdateWorkspace
    updateWorkspace_accountAccessType,
    updateWorkspace_organizationRoleName,
    updateWorkspace_permissionType,
    updateWorkspace_removeVpcConfiguration,
    updateWorkspace_stackSetName,
    updateWorkspace_vpcConfiguration,
    updateWorkspace_workspaceDataSources,
    updateWorkspace_workspaceDescription,
    updateWorkspace_workspaceName,
    updateWorkspace_workspaceNotificationDestinations,
    updateWorkspace_workspaceOrganizationalUnits,
    updateWorkspace_workspaceRoleArn,
    updateWorkspace_workspaceId,
    updateWorkspaceResponse_httpStatus,
    updateWorkspaceResponse_workspace,

    -- ** UpdateWorkspaceAuthentication
    updateWorkspaceAuthentication_samlConfiguration,
    updateWorkspaceAuthentication_authenticationProviders,
    updateWorkspaceAuthentication_workspaceId,
    updateWorkspaceAuthenticationResponse_httpStatus,
    updateWorkspaceAuthenticationResponse_authentication,

    -- ** UpdateWorkspaceConfiguration
    updateWorkspaceConfiguration_configuration,
    updateWorkspaceConfiguration_workspaceId,
    updateWorkspaceConfigurationResponse_httpStatus,

    -- * Types

    -- ** AssertionAttributes
    assertionAttributes_email,
    assertionAttributes_groups,
    assertionAttributes_login,
    assertionAttributes_name,
    assertionAttributes_org,
    assertionAttributes_role,

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
    samlConfiguration_allowedOrganizations,
    samlConfiguration_assertionAttributes,
    samlConfiguration_loginValidityDuration,
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

    -- ** VpcConfiguration
    vpcConfiguration_securityGroupIds,
    vpcConfiguration_subnetIds,

    -- ** WorkspaceDescription
    workspaceDescription_accountAccessType,
    workspaceDescription_description,
    workspaceDescription_freeTrialConsumed,
    workspaceDescription_freeTrialExpiration,
    workspaceDescription_licenseExpiration,
    workspaceDescription_licenseType,
    workspaceDescription_name,
    workspaceDescription_notificationDestinations,
    workspaceDescription_organizationRoleName,
    workspaceDescription_organizationalUnits,
    workspaceDescription_permissionType,
    workspaceDescription_stackSetName,
    workspaceDescription_tags,
    workspaceDescription_vpcConfiguration,
    workspaceDescription_workspaceRoleArn,
    workspaceDescription_authentication,
    workspaceDescription_created,
    workspaceDescription_dataSources,
    workspaceDescription_endpoint,
    workspaceDescription_grafanaVersion,
    workspaceDescription_id,
    workspaceDescription_modified,
    workspaceDescription_status,

    -- ** WorkspaceSummary
    workspaceSummary_description,
    workspaceSummary_name,
    workspaceSummary_notificationDestinations,
    workspaceSummary_tags,
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
import Amazonka.Grafana.CreateWorkspaceApiKey
import Amazonka.Grafana.DeleteWorkspace
import Amazonka.Grafana.DeleteWorkspaceApiKey
import Amazonka.Grafana.DescribeWorkspace
import Amazonka.Grafana.DescribeWorkspaceAuthentication
import Amazonka.Grafana.DescribeWorkspaceConfiguration
import Amazonka.Grafana.DisassociateLicense
import Amazonka.Grafana.ListPermissions
import Amazonka.Grafana.ListTagsForResource
import Amazonka.Grafana.ListWorkspaces
import Amazonka.Grafana.TagResource
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
import Amazonka.Grafana.Types.VpcConfiguration
import Amazonka.Grafana.Types.WorkspaceDescription
import Amazonka.Grafana.Types.WorkspaceSummary
import Amazonka.Grafana.UntagResource
import Amazonka.Grafana.UpdatePermissions
import Amazonka.Grafana.UpdateWorkspace
import Amazonka.Grafana.UpdateWorkspaceAuthentication
import Amazonka.Grafana.UpdateWorkspaceConfiguration
