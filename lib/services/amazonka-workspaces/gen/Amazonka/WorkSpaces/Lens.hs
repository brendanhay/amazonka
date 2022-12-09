{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkSpaces.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Lens
  ( -- * Operations

    -- ** AssociateConnectionAlias
    associateConnectionAlias_aliasId,
    associateConnectionAlias_resourceId,
    associateConnectionAliasResponse_connectionIdentifier,
    associateConnectionAliasResponse_httpStatus,

    -- ** AssociateIpGroups
    associateIpGroups_directoryId,
    associateIpGroups_groupIds,
    associateIpGroupsResponse_httpStatus,

    -- ** AuthorizeIpRules
    authorizeIpRules_groupId,
    authorizeIpRules_userRules,
    authorizeIpRulesResponse_httpStatus,

    -- ** CopyWorkspaceImage
    copyWorkspaceImage_description,
    copyWorkspaceImage_tags,
    copyWorkspaceImage_name,
    copyWorkspaceImage_sourceImageId,
    copyWorkspaceImage_sourceRegion,
    copyWorkspaceImageResponse_imageId,
    copyWorkspaceImageResponse_httpStatus,

    -- ** CreateConnectClientAddIn
    createConnectClientAddIn_resourceId,
    createConnectClientAddIn_name,
    createConnectClientAddIn_url,
    createConnectClientAddInResponse_addInId,
    createConnectClientAddInResponse_httpStatus,

    -- ** CreateConnectionAlias
    createConnectionAlias_tags,
    createConnectionAlias_connectionString,
    createConnectionAliasResponse_aliasId,
    createConnectionAliasResponse_httpStatus,

    -- ** CreateIpGroup
    createIpGroup_groupDesc,
    createIpGroup_tags,
    createIpGroup_userRules,
    createIpGroup_groupName,
    createIpGroupResponse_groupId,
    createIpGroupResponse_httpStatus,

    -- ** CreateStandbyWorkspaces
    createStandbyWorkspaces_primaryRegion,
    createStandbyWorkspaces_standbyWorkspaces,
    createStandbyWorkspacesResponse_failedStandbyRequests,
    createStandbyWorkspacesResponse_pendingStandbyRequests,
    createStandbyWorkspacesResponse_httpStatus,

    -- ** CreateTags
    createTags_resourceId,
    createTags_tags,
    createTagsResponse_httpStatus,

    -- ** CreateUpdatedWorkspaceImage
    createUpdatedWorkspaceImage_tags,
    createUpdatedWorkspaceImage_name,
    createUpdatedWorkspaceImage_description,
    createUpdatedWorkspaceImage_sourceImageId,
    createUpdatedWorkspaceImageResponse_imageId,
    createUpdatedWorkspaceImageResponse_httpStatus,

    -- ** CreateWorkspaceBundle
    createWorkspaceBundle_rootStorage,
    createWorkspaceBundle_tags,
    createWorkspaceBundle_bundleName,
    createWorkspaceBundle_bundleDescription,
    createWorkspaceBundle_imageId,
    createWorkspaceBundle_computeType,
    createWorkspaceBundle_userStorage,
    createWorkspaceBundleResponse_workspaceBundle,
    createWorkspaceBundleResponse_httpStatus,

    -- ** CreateWorkspaceImage
    createWorkspaceImage_tags,
    createWorkspaceImage_name,
    createWorkspaceImage_description,
    createWorkspaceImage_workspaceId,
    createWorkspaceImageResponse_created,
    createWorkspaceImageResponse_description,
    createWorkspaceImageResponse_imageId,
    createWorkspaceImageResponse_name,
    createWorkspaceImageResponse_operatingSystem,
    createWorkspaceImageResponse_ownerAccountId,
    createWorkspaceImageResponse_requiredTenancy,
    createWorkspaceImageResponse_state,
    createWorkspaceImageResponse_httpStatus,

    -- ** CreateWorkspaces
    createWorkspaces_workspaces,
    createWorkspacesResponse_failedRequests,
    createWorkspacesResponse_pendingRequests,
    createWorkspacesResponse_httpStatus,

    -- ** DeleteClientBranding
    deleteClientBranding_resourceId,
    deleteClientBranding_platforms,
    deleteClientBrandingResponse_httpStatus,

    -- ** DeleteConnectClientAddIn
    deleteConnectClientAddIn_addInId,
    deleteConnectClientAddIn_resourceId,
    deleteConnectClientAddInResponse_httpStatus,

    -- ** DeleteConnectionAlias
    deleteConnectionAlias_aliasId,
    deleteConnectionAliasResponse_httpStatus,

    -- ** DeleteIpGroup
    deleteIpGroup_groupId,
    deleteIpGroupResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_resourceId,
    deleteTags_tagKeys,
    deleteTagsResponse_httpStatus,

    -- ** DeleteWorkspaceBundle
    deleteWorkspaceBundle_bundleId,
    deleteWorkspaceBundleResponse_httpStatus,

    -- ** DeleteWorkspaceImage
    deleteWorkspaceImage_imageId,
    deleteWorkspaceImageResponse_httpStatus,

    -- ** DeregisterWorkspaceDirectory
    deregisterWorkspaceDirectory_directoryId,
    deregisterWorkspaceDirectoryResponse_httpStatus,

    -- ** DescribeAccount
    describeAccountResponse_dedicatedTenancyManagementCidrRange,
    describeAccountResponse_dedicatedTenancySupport,
    describeAccountResponse_httpStatus,

    -- ** DescribeAccountModifications
    describeAccountModifications_nextToken,
    describeAccountModificationsResponse_accountModifications,
    describeAccountModificationsResponse_nextToken,
    describeAccountModificationsResponse_httpStatus,

    -- ** DescribeClientBranding
    describeClientBranding_resourceId,
    describeClientBrandingResponse_deviceTypeAndroid,
    describeClientBrandingResponse_deviceTypeIos,
    describeClientBrandingResponse_deviceTypeLinux,
    describeClientBrandingResponse_deviceTypeOsx,
    describeClientBrandingResponse_deviceTypeWeb,
    describeClientBrandingResponse_deviceTypeWindows,
    describeClientBrandingResponse_httpStatus,

    -- ** DescribeClientProperties
    describeClientProperties_resourceIds,
    describeClientPropertiesResponse_clientPropertiesList,
    describeClientPropertiesResponse_httpStatus,

    -- ** DescribeConnectClientAddIns
    describeConnectClientAddIns_maxResults,
    describeConnectClientAddIns_nextToken,
    describeConnectClientAddIns_resourceId,
    describeConnectClientAddInsResponse_addIns,
    describeConnectClientAddInsResponse_nextToken,
    describeConnectClientAddInsResponse_httpStatus,

    -- ** DescribeConnectionAliasPermissions
    describeConnectionAliasPermissions_maxResults,
    describeConnectionAliasPermissions_nextToken,
    describeConnectionAliasPermissions_aliasId,
    describeConnectionAliasPermissionsResponse_aliasId,
    describeConnectionAliasPermissionsResponse_connectionAliasPermissions,
    describeConnectionAliasPermissionsResponse_nextToken,
    describeConnectionAliasPermissionsResponse_httpStatus,

    -- ** DescribeConnectionAliases
    describeConnectionAliases_aliasIds,
    describeConnectionAliases_limit,
    describeConnectionAliases_nextToken,
    describeConnectionAliases_resourceId,
    describeConnectionAliasesResponse_connectionAliases,
    describeConnectionAliasesResponse_nextToken,
    describeConnectionAliasesResponse_httpStatus,

    -- ** DescribeIpGroups
    describeIpGroups_groupIds,
    describeIpGroups_maxResults,
    describeIpGroups_nextToken,
    describeIpGroupsResponse_nextToken,
    describeIpGroupsResponse_result,
    describeIpGroupsResponse_httpStatus,

    -- ** DescribeTags
    describeTags_resourceId,
    describeTagsResponse_tagList,
    describeTagsResponse_httpStatus,

    -- ** DescribeWorkspaceBundles
    describeWorkspaceBundles_bundleIds,
    describeWorkspaceBundles_nextToken,
    describeWorkspaceBundles_owner,
    describeWorkspaceBundlesResponse_bundles,
    describeWorkspaceBundlesResponse_nextToken,
    describeWorkspaceBundlesResponse_httpStatus,

    -- ** DescribeWorkspaceDirectories
    describeWorkspaceDirectories_directoryIds,
    describeWorkspaceDirectories_limit,
    describeWorkspaceDirectories_nextToken,
    describeWorkspaceDirectoriesResponse_directories,
    describeWorkspaceDirectoriesResponse_nextToken,
    describeWorkspaceDirectoriesResponse_httpStatus,

    -- ** DescribeWorkspaceImagePermissions
    describeWorkspaceImagePermissions_maxResults,
    describeWorkspaceImagePermissions_nextToken,
    describeWorkspaceImagePermissions_imageId,
    describeWorkspaceImagePermissionsResponse_imageId,
    describeWorkspaceImagePermissionsResponse_imagePermissions,
    describeWorkspaceImagePermissionsResponse_nextToken,
    describeWorkspaceImagePermissionsResponse_httpStatus,

    -- ** DescribeWorkspaceImages
    describeWorkspaceImages_imageIds,
    describeWorkspaceImages_imageType,
    describeWorkspaceImages_maxResults,
    describeWorkspaceImages_nextToken,
    describeWorkspaceImagesResponse_images,
    describeWorkspaceImagesResponse_nextToken,
    describeWorkspaceImagesResponse_httpStatus,

    -- ** DescribeWorkspaceSnapshots
    describeWorkspaceSnapshots_workspaceId,
    describeWorkspaceSnapshotsResponse_rebuildSnapshots,
    describeWorkspaceSnapshotsResponse_restoreSnapshots,
    describeWorkspaceSnapshotsResponse_httpStatus,

    -- ** DescribeWorkspaces
    describeWorkspaces_bundleId,
    describeWorkspaces_directoryId,
    describeWorkspaces_limit,
    describeWorkspaces_nextToken,
    describeWorkspaces_userName,
    describeWorkspaces_workspaceIds,
    describeWorkspacesResponse_nextToken,
    describeWorkspacesResponse_workspaces,
    describeWorkspacesResponse_httpStatus,

    -- ** DescribeWorkspacesConnectionStatus
    describeWorkspacesConnectionStatus_nextToken,
    describeWorkspacesConnectionStatus_workspaceIds,
    describeWorkspacesConnectionStatusResponse_nextToken,
    describeWorkspacesConnectionStatusResponse_workspacesConnectionStatus,
    describeWorkspacesConnectionStatusResponse_httpStatus,

    -- ** DisassociateConnectionAlias
    disassociateConnectionAlias_aliasId,
    disassociateConnectionAliasResponse_httpStatus,

    -- ** DisassociateIpGroups
    disassociateIpGroups_directoryId,
    disassociateIpGroups_groupIds,
    disassociateIpGroupsResponse_httpStatus,

    -- ** ImportClientBranding
    importClientBranding_deviceTypeAndroid,
    importClientBranding_deviceTypeIos,
    importClientBranding_deviceTypeLinux,
    importClientBranding_deviceTypeOsx,
    importClientBranding_deviceTypeWeb,
    importClientBranding_deviceTypeWindows,
    importClientBranding_resourceId,
    importClientBrandingResponse_deviceTypeAndroid,
    importClientBrandingResponse_deviceTypeIos,
    importClientBrandingResponse_deviceTypeLinux,
    importClientBrandingResponse_deviceTypeOsx,
    importClientBrandingResponse_deviceTypeWeb,
    importClientBrandingResponse_deviceTypeWindows,
    importClientBrandingResponse_httpStatus,

    -- ** ImportWorkspaceImage
    importWorkspaceImage_applications,
    importWorkspaceImage_tags,
    importWorkspaceImage_ec2ImageId,
    importWorkspaceImage_ingestionProcess,
    importWorkspaceImage_imageName,
    importWorkspaceImage_imageDescription,
    importWorkspaceImageResponse_imageId,
    importWorkspaceImageResponse_httpStatus,

    -- ** ListAvailableManagementCidrRanges
    listAvailableManagementCidrRanges_maxResults,
    listAvailableManagementCidrRanges_nextToken,
    listAvailableManagementCidrRanges_managementCidrRangeConstraint,
    listAvailableManagementCidrRangesResponse_managementCidrRanges,
    listAvailableManagementCidrRangesResponse_nextToken,
    listAvailableManagementCidrRangesResponse_httpStatus,

    -- ** MigrateWorkspace
    migrateWorkspace_sourceWorkspaceId,
    migrateWorkspace_bundleId,
    migrateWorkspaceResponse_sourceWorkspaceId,
    migrateWorkspaceResponse_targetWorkspaceId,
    migrateWorkspaceResponse_httpStatus,

    -- ** ModifyAccount
    modifyAccount_dedicatedTenancyManagementCidrRange,
    modifyAccount_dedicatedTenancySupport,
    modifyAccountResponse_httpStatus,

    -- ** ModifyCertificateBasedAuthProperties
    modifyCertificateBasedAuthProperties_certificateBasedAuthProperties,
    modifyCertificateBasedAuthProperties_propertiesToDelete,
    modifyCertificateBasedAuthProperties_resourceId,
    modifyCertificateBasedAuthPropertiesResponse_httpStatus,

    -- ** ModifyClientProperties
    modifyClientProperties_resourceId,
    modifyClientProperties_clientProperties,
    modifyClientPropertiesResponse_httpStatus,

    -- ** ModifySamlProperties
    modifySamlProperties_propertiesToDelete,
    modifySamlProperties_samlProperties,
    modifySamlProperties_resourceId,
    modifySamlPropertiesResponse_httpStatus,

    -- ** ModifySelfservicePermissions
    modifySelfservicePermissions_resourceId,
    modifySelfservicePermissions_selfservicePermissions,
    modifySelfservicePermissionsResponse_httpStatus,

    -- ** ModifyWorkspaceAccessProperties
    modifyWorkspaceAccessProperties_resourceId,
    modifyWorkspaceAccessProperties_workspaceAccessProperties,
    modifyWorkspaceAccessPropertiesResponse_httpStatus,

    -- ** ModifyWorkspaceCreationProperties
    modifyWorkspaceCreationProperties_resourceId,
    modifyWorkspaceCreationProperties_workspaceCreationProperties,
    modifyWorkspaceCreationPropertiesResponse_httpStatus,

    -- ** ModifyWorkspaceProperties
    modifyWorkspaceProperties_workspaceId,
    modifyWorkspaceProperties_workspaceProperties,
    modifyWorkspacePropertiesResponse_httpStatus,

    -- ** ModifyWorkspaceState
    modifyWorkspaceState_workspaceId,
    modifyWorkspaceState_workspaceState,
    modifyWorkspaceStateResponse_httpStatus,

    -- ** RebootWorkspaces
    rebootWorkspaces_rebootWorkspaceRequests,
    rebootWorkspacesResponse_failedRequests,
    rebootWorkspacesResponse_httpStatus,

    -- ** RebuildWorkspaces
    rebuildWorkspaces_rebuildWorkspaceRequests,
    rebuildWorkspacesResponse_failedRequests,
    rebuildWorkspacesResponse_httpStatus,

    -- ** RegisterWorkspaceDirectory
    registerWorkspaceDirectory_enableSelfService,
    registerWorkspaceDirectory_subnetIds,
    registerWorkspaceDirectory_tags,
    registerWorkspaceDirectory_tenancy,
    registerWorkspaceDirectory_directoryId,
    registerWorkspaceDirectory_enableWorkDocs,
    registerWorkspaceDirectoryResponse_httpStatus,

    -- ** RestoreWorkspace
    restoreWorkspace_workspaceId,
    restoreWorkspaceResponse_httpStatus,

    -- ** RevokeIpRules
    revokeIpRules_groupId,
    revokeIpRules_userRules,
    revokeIpRulesResponse_httpStatus,

    -- ** StartWorkspaces
    startWorkspaces_startWorkspaceRequests,
    startWorkspacesResponse_failedRequests,
    startWorkspacesResponse_httpStatus,

    -- ** StopWorkspaces
    stopWorkspaces_stopWorkspaceRequests,
    stopWorkspacesResponse_failedRequests,
    stopWorkspacesResponse_httpStatus,

    -- ** TerminateWorkspaces
    terminateWorkspaces_terminateWorkspaceRequests,
    terminateWorkspacesResponse_failedRequests,
    terminateWorkspacesResponse_httpStatus,

    -- ** UpdateConnectClientAddIn
    updateConnectClientAddIn_name,
    updateConnectClientAddIn_url,
    updateConnectClientAddIn_addInId,
    updateConnectClientAddIn_resourceId,
    updateConnectClientAddInResponse_httpStatus,

    -- ** UpdateConnectionAliasPermission
    updateConnectionAliasPermission_aliasId,
    updateConnectionAliasPermission_connectionAliasPermission,
    updateConnectionAliasPermissionResponse_httpStatus,

    -- ** UpdateRulesOfIpGroup
    updateRulesOfIpGroup_groupId,
    updateRulesOfIpGroup_userRules,
    updateRulesOfIpGroupResponse_httpStatus,

    -- ** UpdateWorkspaceBundle
    updateWorkspaceBundle_bundleId,
    updateWorkspaceBundle_imageId,
    updateWorkspaceBundleResponse_httpStatus,

    -- ** UpdateWorkspaceImagePermission
    updateWorkspaceImagePermission_imageId,
    updateWorkspaceImagePermission_allowCopyImage,
    updateWorkspaceImagePermission_sharedAccountId,
    updateWorkspaceImagePermissionResponse_httpStatus,

    -- * Types

    -- ** AccountModification
    accountModification_dedicatedTenancyManagementCidrRange,
    accountModification_dedicatedTenancySupport,
    accountModification_errorCode,
    accountModification_errorMessage,
    accountModification_modificationState,
    accountModification_startTime,

    -- ** CertificateBasedAuthProperties
    certificateBasedAuthProperties_certificateAuthorityArn,
    certificateBasedAuthProperties_status,

    -- ** ClientProperties
    clientProperties_logUploadEnabled,
    clientProperties_reconnectEnabled,

    -- ** ClientPropertiesResult
    clientPropertiesResult_clientProperties,
    clientPropertiesResult_resourceId,

    -- ** ComputeType
    computeType_name,

    -- ** ConnectClientAddIn
    connectClientAddIn_addInId,
    connectClientAddIn_name,
    connectClientAddIn_resourceId,
    connectClientAddIn_url,

    -- ** ConnectionAlias
    connectionAlias_aliasId,
    connectionAlias_associations,
    connectionAlias_connectionString,
    connectionAlias_ownerAccountId,
    connectionAlias_state,

    -- ** ConnectionAliasAssociation
    connectionAliasAssociation_associatedAccountId,
    connectionAliasAssociation_associationStatus,
    connectionAliasAssociation_connectionIdentifier,
    connectionAliasAssociation_resourceId,

    -- ** ConnectionAliasPermission
    connectionAliasPermission_sharedAccountId,
    connectionAliasPermission_allowAssociation,

    -- ** DefaultClientBrandingAttributes
    defaultClientBrandingAttributes_forgotPasswordLink,
    defaultClientBrandingAttributes_loginMessage,
    defaultClientBrandingAttributes_logoUrl,
    defaultClientBrandingAttributes_supportEmail,
    defaultClientBrandingAttributes_supportLink,

    -- ** DefaultImportClientBrandingAttributes
    defaultImportClientBrandingAttributes_forgotPasswordLink,
    defaultImportClientBrandingAttributes_loginMessage,
    defaultImportClientBrandingAttributes_logo,
    defaultImportClientBrandingAttributes_supportEmail,
    defaultImportClientBrandingAttributes_supportLink,

    -- ** DefaultWorkspaceCreationProperties
    defaultWorkspaceCreationProperties_customSecurityGroupId,
    defaultWorkspaceCreationProperties_defaultOu,
    defaultWorkspaceCreationProperties_enableInternetAccess,
    defaultWorkspaceCreationProperties_enableMaintenanceMode,
    defaultWorkspaceCreationProperties_enableWorkDocs,
    defaultWorkspaceCreationProperties_userEnabledAsLocalAdministrator,

    -- ** FailedCreateStandbyWorkspacesRequest
    failedCreateStandbyWorkspacesRequest_errorCode,
    failedCreateStandbyWorkspacesRequest_errorMessage,
    failedCreateStandbyWorkspacesRequest_standbyWorkspaceRequest,

    -- ** FailedCreateWorkspaceRequest
    failedCreateWorkspaceRequest_errorCode,
    failedCreateWorkspaceRequest_errorMessage,
    failedCreateWorkspaceRequest_workspaceRequest,

    -- ** FailedWorkspaceChangeRequest
    failedWorkspaceChangeRequest_errorCode,
    failedWorkspaceChangeRequest_errorMessage,
    failedWorkspaceChangeRequest_workspaceId,

    -- ** ImagePermission
    imagePermission_sharedAccountId,

    -- ** IosClientBrandingAttributes
    iosClientBrandingAttributes_forgotPasswordLink,
    iosClientBrandingAttributes_loginMessage,
    iosClientBrandingAttributes_logo2xUrl,
    iosClientBrandingAttributes_logo3xUrl,
    iosClientBrandingAttributes_logoUrl,
    iosClientBrandingAttributes_supportEmail,
    iosClientBrandingAttributes_supportLink,

    -- ** IosImportClientBrandingAttributes
    iosImportClientBrandingAttributes_forgotPasswordLink,
    iosImportClientBrandingAttributes_loginMessage,
    iosImportClientBrandingAttributes_logo,
    iosImportClientBrandingAttributes_logo2x,
    iosImportClientBrandingAttributes_logo3x,
    iosImportClientBrandingAttributes_supportEmail,
    iosImportClientBrandingAttributes_supportLink,

    -- ** IpRuleItem
    ipRuleItem_ipRule,
    ipRuleItem_ruleDesc,

    -- ** ModificationState
    modificationState_resource,
    modificationState_state,

    -- ** OperatingSystem
    operatingSystem_type,

    -- ** PendingCreateStandbyWorkspacesRequest
    pendingCreateStandbyWorkspacesRequest_directoryId,
    pendingCreateStandbyWorkspacesRequest_state,
    pendingCreateStandbyWorkspacesRequest_userName,
    pendingCreateStandbyWorkspacesRequest_workspaceId,

    -- ** RebootRequest
    rebootRequest_workspaceId,

    -- ** RebuildRequest
    rebuildRequest_workspaceId,

    -- ** RelatedWorkspaceProperties
    relatedWorkspaceProperties_region,
    relatedWorkspaceProperties_state,
    relatedWorkspaceProperties_type,
    relatedWorkspaceProperties_workspaceId,

    -- ** RootStorage
    rootStorage_capacity,

    -- ** SamlProperties
    samlProperties_relayStateParameterName,
    samlProperties_status,
    samlProperties_userAccessUrl,

    -- ** SelfservicePermissions
    selfservicePermissions_changeComputeType,
    selfservicePermissions_increaseVolumeSize,
    selfservicePermissions_rebuildWorkspace,
    selfservicePermissions_restartWorkspace,
    selfservicePermissions_switchRunningMode,

    -- ** Snapshot
    snapshot_snapshotTime,

    -- ** StandbyWorkspace
    standbyWorkspace_tags,
    standbyWorkspace_volumeEncryptionKey,
    standbyWorkspace_primaryWorkspaceId,
    standbyWorkspace_directoryId,

    -- ** StartRequest
    startRequest_workspaceId,

    -- ** StopRequest
    stopRequest_workspaceId,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TerminateRequest
    terminateRequest_workspaceId,

    -- ** UpdateResult
    updateResult_description,
    updateResult_updateAvailable,

    -- ** UserStorage
    userStorage_capacity,

    -- ** Workspace
    workspace_bundleId,
    workspace_computerName,
    workspace_directoryId,
    workspace_errorCode,
    workspace_errorMessage,
    workspace_ipAddress,
    workspace_modificationStates,
    workspace_relatedWorkspaces,
    workspace_rootVolumeEncryptionEnabled,
    workspace_state,
    workspace_subnetId,
    workspace_userName,
    workspace_userVolumeEncryptionEnabled,
    workspace_volumeEncryptionKey,
    workspace_workspaceId,
    workspace_workspaceProperties,

    -- ** WorkspaceAccessProperties
    workspaceAccessProperties_deviceTypeAndroid,
    workspaceAccessProperties_deviceTypeChromeOs,
    workspaceAccessProperties_deviceTypeIos,
    workspaceAccessProperties_deviceTypeLinux,
    workspaceAccessProperties_deviceTypeOsx,
    workspaceAccessProperties_deviceTypeWeb,
    workspaceAccessProperties_deviceTypeWindows,
    workspaceAccessProperties_deviceTypeZeroClient,

    -- ** WorkspaceBundle
    workspaceBundle_bundleId,
    workspaceBundle_bundleType,
    workspaceBundle_computeType,
    workspaceBundle_creationTime,
    workspaceBundle_description,
    workspaceBundle_imageId,
    workspaceBundle_lastUpdatedTime,
    workspaceBundle_name,
    workspaceBundle_owner,
    workspaceBundle_rootStorage,
    workspaceBundle_state,
    workspaceBundle_userStorage,

    -- ** WorkspaceConnectionStatus
    workspaceConnectionStatus_connectionState,
    workspaceConnectionStatus_connectionStateCheckTimestamp,
    workspaceConnectionStatus_lastKnownUserConnectionTimestamp,
    workspaceConnectionStatus_workspaceId,

    -- ** WorkspaceCreationProperties
    workspaceCreationProperties_customSecurityGroupId,
    workspaceCreationProperties_defaultOu,
    workspaceCreationProperties_enableInternetAccess,
    workspaceCreationProperties_enableMaintenanceMode,
    workspaceCreationProperties_enableWorkDocs,
    workspaceCreationProperties_userEnabledAsLocalAdministrator,

    -- ** WorkspaceDirectory
    workspaceDirectory_alias,
    workspaceDirectory_certificateBasedAuthProperties,
    workspaceDirectory_customerUserName,
    workspaceDirectory_directoryId,
    workspaceDirectory_directoryName,
    workspaceDirectory_directoryType,
    workspaceDirectory_dnsIpAddresses,
    workspaceDirectory_iamRoleId,
    workspaceDirectory_registrationCode,
    workspaceDirectory_samlProperties,
    workspaceDirectory_selfservicePermissions,
    workspaceDirectory_state,
    workspaceDirectory_subnetIds,
    workspaceDirectory_tenancy,
    workspaceDirectory_workspaceAccessProperties,
    workspaceDirectory_workspaceCreationProperties,
    workspaceDirectory_workspaceSecurityGroupId,
    workspaceDirectory_ipGroupIds,

    -- ** WorkspaceImage
    workspaceImage_created,
    workspaceImage_description,
    workspaceImage_errorCode,
    workspaceImage_errorMessage,
    workspaceImage_imageId,
    workspaceImage_name,
    workspaceImage_operatingSystem,
    workspaceImage_ownerAccountId,
    workspaceImage_requiredTenancy,
    workspaceImage_state,
    workspaceImage_updates,

    -- ** WorkspaceProperties
    workspaceProperties_computeTypeName,
    workspaceProperties_protocols,
    workspaceProperties_rootVolumeSizeGib,
    workspaceProperties_runningMode,
    workspaceProperties_runningModeAutoStopTimeoutInMinutes,
    workspaceProperties_userVolumeSizeGib,

    -- ** WorkspaceRequest
    workspaceRequest_rootVolumeEncryptionEnabled,
    workspaceRequest_tags,
    workspaceRequest_userVolumeEncryptionEnabled,
    workspaceRequest_volumeEncryptionKey,
    workspaceRequest_workspaceProperties,
    workspaceRequest_directoryId,
    workspaceRequest_userName,
    workspaceRequest_bundleId,

    -- ** WorkspacesIpGroup
    workspacesIpGroup_groupDesc,
    workspacesIpGroup_groupId,
    workspacesIpGroup_groupName,
    workspacesIpGroup_userRules,
  )
where

import Amazonka.WorkSpaces.AssociateConnectionAlias
import Amazonka.WorkSpaces.AssociateIpGroups
import Amazonka.WorkSpaces.AuthorizeIpRules
import Amazonka.WorkSpaces.CopyWorkspaceImage
import Amazonka.WorkSpaces.CreateConnectClientAddIn
import Amazonka.WorkSpaces.CreateConnectionAlias
import Amazonka.WorkSpaces.CreateIpGroup
import Amazonka.WorkSpaces.CreateStandbyWorkspaces
import Amazonka.WorkSpaces.CreateTags
import Amazonka.WorkSpaces.CreateUpdatedWorkspaceImage
import Amazonka.WorkSpaces.CreateWorkspaceBundle
import Amazonka.WorkSpaces.CreateWorkspaceImage
import Amazonka.WorkSpaces.CreateWorkspaces
import Amazonka.WorkSpaces.DeleteClientBranding
import Amazonka.WorkSpaces.DeleteConnectClientAddIn
import Amazonka.WorkSpaces.DeleteConnectionAlias
import Amazonka.WorkSpaces.DeleteIpGroup
import Amazonka.WorkSpaces.DeleteTags
import Amazonka.WorkSpaces.DeleteWorkspaceBundle
import Amazonka.WorkSpaces.DeleteWorkspaceImage
import Amazonka.WorkSpaces.DeregisterWorkspaceDirectory
import Amazonka.WorkSpaces.DescribeAccount
import Amazonka.WorkSpaces.DescribeAccountModifications
import Amazonka.WorkSpaces.DescribeClientBranding
import Amazonka.WorkSpaces.DescribeClientProperties
import Amazonka.WorkSpaces.DescribeConnectClientAddIns
import Amazonka.WorkSpaces.DescribeConnectionAliasPermissions
import Amazonka.WorkSpaces.DescribeConnectionAliases
import Amazonka.WorkSpaces.DescribeIpGroups
import Amazonka.WorkSpaces.DescribeTags
import Amazonka.WorkSpaces.DescribeWorkspaceBundles
import Amazonka.WorkSpaces.DescribeWorkspaceDirectories
import Amazonka.WorkSpaces.DescribeWorkspaceImagePermissions
import Amazonka.WorkSpaces.DescribeWorkspaceImages
import Amazonka.WorkSpaces.DescribeWorkspaceSnapshots
import Amazonka.WorkSpaces.DescribeWorkspaces
import Amazonka.WorkSpaces.DescribeWorkspacesConnectionStatus
import Amazonka.WorkSpaces.DisassociateConnectionAlias
import Amazonka.WorkSpaces.DisassociateIpGroups
import Amazonka.WorkSpaces.ImportClientBranding
import Amazonka.WorkSpaces.ImportWorkspaceImage
import Amazonka.WorkSpaces.ListAvailableManagementCidrRanges
import Amazonka.WorkSpaces.MigrateWorkspace
import Amazonka.WorkSpaces.ModifyAccount
import Amazonka.WorkSpaces.ModifyCertificateBasedAuthProperties
import Amazonka.WorkSpaces.ModifyClientProperties
import Amazonka.WorkSpaces.ModifySamlProperties
import Amazonka.WorkSpaces.ModifySelfservicePermissions
import Amazonka.WorkSpaces.ModifyWorkspaceAccessProperties
import Amazonka.WorkSpaces.ModifyWorkspaceCreationProperties
import Amazonka.WorkSpaces.ModifyWorkspaceProperties
import Amazonka.WorkSpaces.ModifyWorkspaceState
import Amazonka.WorkSpaces.RebootWorkspaces
import Amazonka.WorkSpaces.RebuildWorkspaces
import Amazonka.WorkSpaces.RegisterWorkspaceDirectory
import Amazonka.WorkSpaces.RestoreWorkspace
import Amazonka.WorkSpaces.RevokeIpRules
import Amazonka.WorkSpaces.StartWorkspaces
import Amazonka.WorkSpaces.StopWorkspaces
import Amazonka.WorkSpaces.TerminateWorkspaces
import Amazonka.WorkSpaces.Types.AccountModification
import Amazonka.WorkSpaces.Types.CertificateBasedAuthProperties
import Amazonka.WorkSpaces.Types.ClientProperties
import Amazonka.WorkSpaces.Types.ClientPropertiesResult
import Amazonka.WorkSpaces.Types.ComputeType
import Amazonka.WorkSpaces.Types.ConnectClientAddIn
import Amazonka.WorkSpaces.Types.ConnectionAlias
import Amazonka.WorkSpaces.Types.ConnectionAliasAssociation
import Amazonka.WorkSpaces.Types.ConnectionAliasPermission
import Amazonka.WorkSpaces.Types.DefaultClientBrandingAttributes
import Amazonka.WorkSpaces.Types.DefaultImportClientBrandingAttributes
import Amazonka.WorkSpaces.Types.DefaultWorkspaceCreationProperties
import Amazonka.WorkSpaces.Types.FailedCreateStandbyWorkspacesRequest
import Amazonka.WorkSpaces.Types.FailedCreateWorkspaceRequest
import Amazonka.WorkSpaces.Types.FailedWorkspaceChangeRequest
import Amazonka.WorkSpaces.Types.ImagePermission
import Amazonka.WorkSpaces.Types.IosClientBrandingAttributes
import Amazonka.WorkSpaces.Types.IosImportClientBrandingAttributes
import Amazonka.WorkSpaces.Types.IpRuleItem
import Amazonka.WorkSpaces.Types.ModificationState
import Amazonka.WorkSpaces.Types.OperatingSystem
import Amazonka.WorkSpaces.Types.PendingCreateStandbyWorkspacesRequest
import Amazonka.WorkSpaces.Types.RebootRequest
import Amazonka.WorkSpaces.Types.RebuildRequest
import Amazonka.WorkSpaces.Types.RelatedWorkspaceProperties
import Amazonka.WorkSpaces.Types.RootStorage
import Amazonka.WorkSpaces.Types.SamlProperties
import Amazonka.WorkSpaces.Types.SelfservicePermissions
import Amazonka.WorkSpaces.Types.Snapshot
import Amazonka.WorkSpaces.Types.StandbyWorkspace
import Amazonka.WorkSpaces.Types.StartRequest
import Amazonka.WorkSpaces.Types.StopRequest
import Amazonka.WorkSpaces.Types.Tag
import Amazonka.WorkSpaces.Types.TerminateRequest
import Amazonka.WorkSpaces.Types.UpdateResult
import Amazonka.WorkSpaces.Types.UserStorage
import Amazonka.WorkSpaces.Types.Workspace
import Amazonka.WorkSpaces.Types.WorkspaceAccessProperties
import Amazonka.WorkSpaces.Types.WorkspaceBundle
import Amazonka.WorkSpaces.Types.WorkspaceConnectionStatus
import Amazonka.WorkSpaces.Types.WorkspaceCreationProperties
import Amazonka.WorkSpaces.Types.WorkspaceDirectory
import Amazonka.WorkSpaces.Types.WorkspaceImage
import Amazonka.WorkSpaces.Types.WorkspaceProperties
import Amazonka.WorkSpaces.Types.WorkspaceRequest
import Amazonka.WorkSpaces.Types.WorkspacesIpGroup
import Amazonka.WorkSpaces.UpdateConnectClientAddIn
import Amazonka.WorkSpaces.UpdateConnectionAliasPermission
import Amazonka.WorkSpaces.UpdateRulesOfIpGroup
import Amazonka.WorkSpaces.UpdateWorkspaceBundle
import Amazonka.WorkSpaces.UpdateWorkspaceImagePermission
