{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkSpaces.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    copyWorkspaceImage_tags,
    copyWorkspaceImage_description,
    copyWorkspaceImage_name,
    copyWorkspaceImage_sourceImageId,
    copyWorkspaceImage_sourceRegion,
    copyWorkspaceImageResponse_imageId,
    copyWorkspaceImageResponse_httpStatus,

    -- ** CreateConnectionAlias
    createConnectionAlias_tags,
    createConnectionAlias_connectionString,
    createConnectionAliasResponse_aliasId,
    createConnectionAliasResponse_httpStatus,

    -- ** CreateIpGroup
    createIpGroup_tags,
    createIpGroup_userRules,
    createIpGroup_groupDesc,
    createIpGroup_groupName,
    createIpGroupResponse_groupId,
    createIpGroupResponse_httpStatus,

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
    createWorkspaceBundle_tags,
    createWorkspaceBundle_rootStorage,
    createWorkspaceBundle_bundleName,
    createWorkspaceBundle_bundleDescription,
    createWorkspaceBundle_imageId,
    createWorkspaceBundle_computeType,
    createWorkspaceBundle_userStorage,
    createWorkspaceBundleResponse_workspaceBundle,
    createWorkspaceBundleResponse_httpStatus,

    -- ** CreateWorkspaces
    createWorkspaces_workspaces,
    createWorkspacesResponse_failedRequests,
    createWorkspacesResponse_pendingRequests,
    createWorkspacesResponse_httpStatus,

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
    describeAccountModificationsResponse_nextToken,
    describeAccountModificationsResponse_accountModifications,
    describeAccountModificationsResponse_httpStatus,

    -- ** DescribeClientProperties
    describeClientProperties_resourceIds,
    describeClientPropertiesResponse_clientPropertiesList,
    describeClientPropertiesResponse_httpStatus,

    -- ** DescribeConnectionAliasPermissions
    describeConnectionAliasPermissions_nextToken,
    describeConnectionAliasPermissions_maxResults,
    describeConnectionAliasPermissions_aliasId,
    describeConnectionAliasPermissionsResponse_nextToken,
    describeConnectionAliasPermissionsResponse_aliasId,
    describeConnectionAliasPermissionsResponse_connectionAliasPermissions,
    describeConnectionAliasPermissionsResponse_httpStatus,

    -- ** DescribeConnectionAliases
    describeConnectionAliases_resourceId,
    describeConnectionAliases_nextToken,
    describeConnectionAliases_aliasIds,
    describeConnectionAliases_limit,
    describeConnectionAliasesResponse_nextToken,
    describeConnectionAliasesResponse_connectionAliases,
    describeConnectionAliasesResponse_httpStatus,

    -- ** DescribeIpGroups
    describeIpGroups_nextToken,
    describeIpGroups_maxResults,
    describeIpGroups_groupIds,
    describeIpGroupsResponse_nextToken,
    describeIpGroupsResponse_result,
    describeIpGroupsResponse_httpStatus,

    -- ** DescribeTags
    describeTags_resourceId,
    describeTagsResponse_tagList,
    describeTagsResponse_httpStatus,

    -- ** DescribeWorkspaceBundles
    describeWorkspaceBundles_nextToken,
    describeWorkspaceBundles_bundleIds,
    describeWorkspaceBundles_owner,
    describeWorkspaceBundlesResponse_nextToken,
    describeWorkspaceBundlesResponse_bundles,
    describeWorkspaceBundlesResponse_httpStatus,

    -- ** DescribeWorkspaceDirectories
    describeWorkspaceDirectories_nextToken,
    describeWorkspaceDirectories_directoryIds,
    describeWorkspaceDirectories_limit,
    describeWorkspaceDirectoriesResponse_nextToken,
    describeWorkspaceDirectoriesResponse_directories,
    describeWorkspaceDirectoriesResponse_httpStatus,

    -- ** DescribeWorkspaceImagePermissions
    describeWorkspaceImagePermissions_nextToken,
    describeWorkspaceImagePermissions_maxResults,
    describeWorkspaceImagePermissions_imageId,
    describeWorkspaceImagePermissionsResponse_nextToken,
    describeWorkspaceImagePermissionsResponse_imagePermissions,
    describeWorkspaceImagePermissionsResponse_imageId,
    describeWorkspaceImagePermissionsResponse_httpStatus,

    -- ** DescribeWorkspaceImages
    describeWorkspaceImages_nextToken,
    describeWorkspaceImages_imageIds,
    describeWorkspaceImages_imageType,
    describeWorkspaceImages_maxResults,
    describeWorkspaceImagesResponse_nextToken,
    describeWorkspaceImagesResponse_images,
    describeWorkspaceImagesResponse_httpStatus,

    -- ** DescribeWorkspaceSnapshots
    describeWorkspaceSnapshots_workspaceId,
    describeWorkspaceSnapshotsResponse_restoreSnapshots,
    describeWorkspaceSnapshotsResponse_rebuildSnapshots,
    describeWorkspaceSnapshotsResponse_httpStatus,

    -- ** DescribeWorkspaces
    describeWorkspaces_directoryId,
    describeWorkspaces_nextToken,
    describeWorkspaces_userName,
    describeWorkspaces_limit,
    describeWorkspaces_bundleId,
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

    -- ** ImportWorkspaceImage
    importWorkspaceImage_tags,
    importWorkspaceImage_applications,
    importWorkspaceImage_ec2ImageId,
    importWorkspaceImage_ingestionProcess,
    importWorkspaceImage_imageName,
    importWorkspaceImage_imageDescription,
    importWorkspaceImageResponse_imageId,
    importWorkspaceImageResponse_httpStatus,

    -- ** ListAvailableManagementCidrRanges
    listAvailableManagementCidrRanges_nextToken,
    listAvailableManagementCidrRanges_maxResults,
    listAvailableManagementCidrRanges_managementCidrRangeConstraint,
    listAvailableManagementCidrRangesResponse_nextToken,
    listAvailableManagementCidrRangesResponse_managementCidrRanges,
    listAvailableManagementCidrRangesResponse_httpStatus,

    -- ** MigrateWorkspace
    migrateWorkspace_sourceWorkspaceId,
    migrateWorkspace_bundleId,
    migrateWorkspaceResponse_targetWorkspaceId,
    migrateWorkspaceResponse_sourceWorkspaceId,
    migrateWorkspaceResponse_httpStatus,

    -- ** ModifyAccount
    modifyAccount_dedicatedTenancyManagementCidrRange,
    modifyAccount_dedicatedTenancySupport,
    modifyAccountResponse_httpStatus,

    -- ** ModifyClientProperties
    modifyClientProperties_resourceId,
    modifyClientProperties_clientProperties,
    modifyClientPropertiesResponse_httpStatus,

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
    registerWorkspaceDirectory_tags,
    registerWorkspaceDirectory_subnetIds,
    registerWorkspaceDirectory_tenancy,
    registerWorkspaceDirectory_enableSelfService,
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
    accountModification_errorMessage,
    accountModification_dedicatedTenancyManagementCidrRange,
    accountModification_modificationState,
    accountModification_dedicatedTenancySupport,
    accountModification_errorCode,
    accountModification_startTime,

    -- ** ClientProperties
    clientProperties_reconnectEnabled,

    -- ** ClientPropertiesResult
    clientPropertiesResult_resourceId,
    clientPropertiesResult_clientProperties,

    -- ** ComputeType
    computeType_name,

    -- ** ConnectionAlias
    connectionAlias_associations,
    connectionAlias_aliasId,
    connectionAlias_state,
    connectionAlias_connectionString,
    connectionAlias_ownerAccountId,

    -- ** ConnectionAliasAssociation
    connectionAliasAssociation_resourceId,
    connectionAliasAssociation_associatedAccountId,
    connectionAliasAssociation_associationStatus,
    connectionAliasAssociation_connectionIdentifier,

    -- ** ConnectionAliasPermission
    connectionAliasPermission_sharedAccountId,
    connectionAliasPermission_allowAssociation,

    -- ** DefaultWorkspaceCreationProperties
    defaultWorkspaceCreationProperties_userEnabledAsLocalAdministrator,
    defaultWorkspaceCreationProperties_enableInternetAccess,
    defaultWorkspaceCreationProperties_enableWorkDocs,
    defaultWorkspaceCreationProperties_defaultOu,
    defaultWorkspaceCreationProperties_enableMaintenanceMode,
    defaultWorkspaceCreationProperties_customSecurityGroupId,

    -- ** FailedCreateWorkspaceRequest
    failedCreateWorkspaceRequest_errorMessage,
    failedCreateWorkspaceRequest_workspaceRequest,
    failedCreateWorkspaceRequest_errorCode,

    -- ** FailedWorkspaceChangeRequest
    failedWorkspaceChangeRequest_errorMessage,
    failedWorkspaceChangeRequest_workspaceId,
    failedWorkspaceChangeRequest_errorCode,

    -- ** ImagePermission
    imagePermission_sharedAccountId,

    -- ** IpRuleItem
    ipRuleItem_ipRule,
    ipRuleItem_ruleDesc,

    -- ** ModificationState
    modificationState_state,
    modificationState_resource,

    -- ** OperatingSystem
    operatingSystem_type,

    -- ** RebootRequest
    rebootRequest_workspaceId,

    -- ** RebuildRequest
    rebuildRequest_workspaceId,

    -- ** RootStorage
    rootStorage_capacity,

    -- ** SelfservicePermissions
    selfservicePermissions_increaseVolumeSize,
    selfservicePermissions_rebuildWorkspace,
    selfservicePermissions_changeComputeType,
    selfservicePermissions_switchRunningMode,
    selfservicePermissions_restartWorkspace,

    -- ** Snapshot
    snapshot_snapshotTime,

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
    updateResult_updateAvailable,
    updateResult_description,

    -- ** UserStorage
    userStorage_capacity,

    -- ** Workspace
    workspace_workspaceProperties,
    workspace_directoryId,
    workspace_volumeEncryptionKey,
    workspace_errorMessage,
    workspace_userName,
    workspace_subnetId,
    workspace_computerName,
    workspace_state,
    workspace_workspaceId,
    workspace_userVolumeEncryptionEnabled,
    workspace_bundleId,
    workspace_errorCode,
    workspace_rootVolumeEncryptionEnabled,
    workspace_modificationStates,
    workspace_ipAddress,

    -- ** WorkspaceAccessProperties
    workspaceAccessProperties_deviceTypeAndroid,
    workspaceAccessProperties_deviceTypeLinux,
    workspaceAccessProperties_deviceTypeWeb,
    workspaceAccessProperties_deviceTypeOsx,
    workspaceAccessProperties_deviceTypeChromeOs,
    workspaceAccessProperties_deviceTypeWindows,
    workspaceAccessProperties_deviceTypeIos,
    workspaceAccessProperties_deviceTypeZeroClient,

    -- ** WorkspaceBundle
    workspaceBundle_name,
    workspaceBundle_rootStorage,
    workspaceBundle_owner,
    workspaceBundle_lastUpdatedTime,
    workspaceBundle_description,
    workspaceBundle_bundleId,
    workspaceBundle_creationTime,
    workspaceBundle_computeType,
    workspaceBundle_userStorage,
    workspaceBundle_imageId,

    -- ** WorkspaceConnectionStatus
    workspaceConnectionStatus_connectionState,
    workspaceConnectionStatus_workspaceId,
    workspaceConnectionStatus_lastKnownUserConnectionTimestamp,
    workspaceConnectionStatus_connectionStateCheckTimestamp,

    -- ** WorkspaceCreationProperties
    workspaceCreationProperties_userEnabledAsLocalAdministrator,
    workspaceCreationProperties_enableInternetAccess,
    workspaceCreationProperties_enableWorkDocs,
    workspaceCreationProperties_defaultOu,
    workspaceCreationProperties_enableMaintenanceMode,
    workspaceCreationProperties_customSecurityGroupId,

    -- ** WorkspaceDirectory
    workspaceDirectory_alias,
    workspaceDirectory_directoryName,
    workspaceDirectory_directoryId,
    workspaceDirectory_workspaceSecurityGroupId,
    workspaceDirectory_ipGroupIds,
    workspaceDirectory_directoryType,
    workspaceDirectory_state,
    workspaceDirectory_customerUserName,
    workspaceDirectory_dnsIpAddresses,
    workspaceDirectory_iamRoleId,
    workspaceDirectory_registrationCode,
    workspaceDirectory_workspaceCreationProperties,
    workspaceDirectory_subnetIds,
    workspaceDirectory_selfservicePermissions,
    workspaceDirectory_workspaceAccessProperties,
    workspaceDirectory_tenancy,

    -- ** WorkspaceImage
    workspaceImage_operatingSystem,
    workspaceImage_name,
    workspaceImage_errorMessage,
    workspaceImage_created,
    workspaceImage_state,
    workspaceImage_description,
    workspaceImage_requiredTenancy,
    workspaceImage_ownerAccountId,
    workspaceImage_errorCode,
    workspaceImage_imageId,
    workspaceImage_updates,

    -- ** WorkspaceProperties
    workspaceProperties_userVolumeSizeGib,
    workspaceProperties_runningMode,
    workspaceProperties_computeTypeName,
    workspaceProperties_rootVolumeSizeGib,
    workspaceProperties_runningModeAutoStopTimeoutInMinutes,

    -- ** WorkspaceRequest
    workspaceRequest_tags,
    workspaceRequest_workspaceProperties,
    workspaceRequest_volumeEncryptionKey,
    workspaceRequest_userVolumeEncryptionEnabled,
    workspaceRequest_rootVolumeEncryptionEnabled,
    workspaceRequest_directoryId,
    workspaceRequest_userName,
    workspaceRequest_bundleId,

    -- ** WorkspacesIpGroup
    workspacesIpGroup_userRules,
    workspacesIpGroup_groupName,
    workspacesIpGroup_groupDesc,
    workspacesIpGroup_groupId,
  )
where

import Amazonka.WorkSpaces.AssociateConnectionAlias
import Amazonka.WorkSpaces.AssociateIpGroups
import Amazonka.WorkSpaces.AuthorizeIpRules
import Amazonka.WorkSpaces.CopyWorkspaceImage
import Amazonka.WorkSpaces.CreateConnectionAlias
import Amazonka.WorkSpaces.CreateIpGroup
import Amazonka.WorkSpaces.CreateTags
import Amazonka.WorkSpaces.CreateUpdatedWorkspaceImage
import Amazonka.WorkSpaces.CreateWorkspaceBundle
import Amazonka.WorkSpaces.CreateWorkspaces
import Amazonka.WorkSpaces.DeleteConnectionAlias
import Amazonka.WorkSpaces.DeleteIpGroup
import Amazonka.WorkSpaces.DeleteTags
import Amazonka.WorkSpaces.DeleteWorkspaceBundle
import Amazonka.WorkSpaces.DeleteWorkspaceImage
import Amazonka.WorkSpaces.DeregisterWorkspaceDirectory
import Amazonka.WorkSpaces.DescribeAccount
import Amazonka.WorkSpaces.DescribeAccountModifications
import Amazonka.WorkSpaces.DescribeClientProperties
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
import Amazonka.WorkSpaces.ImportWorkspaceImage
import Amazonka.WorkSpaces.ListAvailableManagementCidrRanges
import Amazonka.WorkSpaces.MigrateWorkspace
import Amazonka.WorkSpaces.ModifyAccount
import Amazonka.WorkSpaces.ModifyClientProperties
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
import Amazonka.WorkSpaces.Types.ClientProperties
import Amazonka.WorkSpaces.Types.ClientPropertiesResult
import Amazonka.WorkSpaces.Types.ComputeType
import Amazonka.WorkSpaces.Types.ConnectionAlias
import Amazonka.WorkSpaces.Types.ConnectionAliasAssociation
import Amazonka.WorkSpaces.Types.ConnectionAliasPermission
import Amazonka.WorkSpaces.Types.DefaultWorkspaceCreationProperties
import Amazonka.WorkSpaces.Types.FailedCreateWorkspaceRequest
import Amazonka.WorkSpaces.Types.FailedWorkspaceChangeRequest
import Amazonka.WorkSpaces.Types.ImagePermission
import Amazonka.WorkSpaces.Types.IpRuleItem
import Amazonka.WorkSpaces.Types.ModificationState
import Amazonka.WorkSpaces.Types.OperatingSystem
import Amazonka.WorkSpaces.Types.RebootRequest
import Amazonka.WorkSpaces.Types.RebuildRequest
import Amazonka.WorkSpaces.Types.RootStorage
import Amazonka.WorkSpaces.Types.SelfservicePermissions
import Amazonka.WorkSpaces.Types.Snapshot
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
import Amazonka.WorkSpaces.UpdateConnectionAliasPermission
import Amazonka.WorkSpaces.UpdateRulesOfIpGroup
import Amazonka.WorkSpaces.UpdateWorkspaceBundle
import Amazonka.WorkSpaces.UpdateWorkspaceImagePermission
