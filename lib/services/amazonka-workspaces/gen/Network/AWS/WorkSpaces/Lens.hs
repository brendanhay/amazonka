{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Lens
  ( -- * Operations

    -- ** AssociateConnectionAlias
    associateConnectionAlias_aliasId,
    associateConnectionAlias_resourceId,
    associateConnectionAliasResponse_connectionIdentifier,
    associateConnectionAliasResponse_httpStatus,

    -- ** DescribeAccount
    describeAccountResponse_dedicatedTenancySupport,
    describeAccountResponse_dedicatedTenancyManagementCidrRange,
    describeAccountResponse_httpStatus,

    -- ** RevokeIpRules
    revokeIpRules_groupId,
    revokeIpRules_userRules,
    revokeIpRulesResponse_httpStatus,

    -- ** DescribeWorkspaceImages
    describeWorkspaceImages_imageIds,
    describeWorkspaceImages_nextToken,
    describeWorkspaceImages_imageType,
    describeWorkspaceImages_maxResults,
    describeWorkspaceImagesResponse_images,
    describeWorkspaceImagesResponse_nextToken,
    describeWorkspaceImagesResponse_httpStatus,

    -- ** ModifyWorkspaceProperties
    modifyWorkspaceProperties_workspaceId,
    modifyWorkspaceProperties_workspaceProperties,
    modifyWorkspacePropertiesResponse_httpStatus,

    -- ** DeregisterWorkspaceDirectory
    deregisterWorkspaceDirectory_directoryId,
    deregisterWorkspaceDirectoryResponse_httpStatus,

    -- ** MigrateWorkspace
    migrateWorkspace_sourceWorkspaceId,
    migrateWorkspace_bundleId,
    migrateWorkspaceResponse_sourceWorkspaceId,
    migrateWorkspaceResponse_targetWorkspaceId,
    migrateWorkspaceResponse_httpStatus,

    -- ** DescribeTags
    describeTags_resourceId,
    describeTagsResponse_tagList,
    describeTagsResponse_httpStatus,

    -- ** DescribeWorkspaceDirectories
    describeWorkspaceDirectories_nextToken,
    describeWorkspaceDirectories_directoryIds,
    describeWorkspaceDirectories_limit,
    describeWorkspaceDirectoriesResponse_directories,
    describeWorkspaceDirectoriesResponse_nextToken,
    describeWorkspaceDirectoriesResponse_httpStatus,

    -- ** DisassociateIpGroups
    disassociateIpGroups_directoryId,
    disassociateIpGroups_groupIds,
    disassociateIpGroupsResponse_httpStatus,

    -- ** DescribeWorkspaceBundles
    describeWorkspaceBundles_bundleIds,
    describeWorkspaceBundles_owner,
    describeWorkspaceBundles_nextToken,
    describeWorkspaceBundlesResponse_bundles,
    describeWorkspaceBundlesResponse_nextToken,
    describeWorkspaceBundlesResponse_httpStatus,

    -- ** AuthorizeIpRules
    authorizeIpRules_groupId,
    authorizeIpRules_userRules,
    authorizeIpRulesResponse_httpStatus,

    -- ** DescribeWorkspaceImagePermissions
    describeWorkspaceImagePermissions_nextToken,
    describeWorkspaceImagePermissions_maxResults,
    describeWorkspaceImagePermissions_imageId,
    describeWorkspaceImagePermissionsResponse_imagePermissions,
    describeWorkspaceImagePermissionsResponse_nextToken,
    describeWorkspaceImagePermissionsResponse_imageId,
    describeWorkspaceImagePermissionsResponse_httpStatus,

    -- ** RebuildWorkspaces
    rebuildWorkspaces_rebuildWorkspaceRequests,
    rebuildWorkspacesResponse_failedRequests,
    rebuildWorkspacesResponse_httpStatus,

    -- ** ImportWorkspaceImage
    importWorkspaceImage_applications,
    importWorkspaceImage_tags,
    importWorkspaceImage_ec2ImageId,
    importWorkspaceImage_ingestionProcess,
    importWorkspaceImage_imageName,
    importWorkspaceImage_imageDescription,
    importWorkspaceImageResponse_imageId,
    importWorkspaceImageResponse_httpStatus,

    -- ** ModifyWorkspaceState
    modifyWorkspaceState_workspaceId,
    modifyWorkspaceState_workspaceState,
    modifyWorkspaceStateResponse_httpStatus,

    -- ** CreateIpGroup
    createIpGroup_groupDesc,
    createIpGroup_userRules,
    createIpGroup_tags,
    createIpGroup_groupName,
    createIpGroupResponse_groupId,
    createIpGroupResponse_httpStatus,

    -- ** DisassociateConnectionAlias
    disassociateConnectionAlias_aliasId,
    disassociateConnectionAliasResponse_httpStatus,

    -- ** ModifyWorkspaceCreationProperties
    modifyWorkspaceCreationProperties_resourceId,
    modifyWorkspaceCreationProperties_workspaceCreationProperties,
    modifyWorkspaceCreationPropertiesResponse_httpStatus,

    -- ** RegisterWorkspaceDirectory
    registerWorkspaceDirectory_subnetIds,
    registerWorkspaceDirectory_enableSelfService,
    registerWorkspaceDirectory_tenancy,
    registerWorkspaceDirectory_tags,
    registerWorkspaceDirectory_directoryId,
    registerWorkspaceDirectory_enableWorkDocs,
    registerWorkspaceDirectoryResponse_httpStatus,

    -- ** RestoreWorkspace
    restoreWorkspace_workspaceId,
    restoreWorkspaceResponse_httpStatus,

    -- ** DescribeConnectionAliasPermissions
    describeConnectionAliasPermissions_nextToken,
    describeConnectionAliasPermissions_maxResults,
    describeConnectionAliasPermissions_aliasId,
    describeConnectionAliasPermissionsResponse_aliasId,
    describeConnectionAliasPermissionsResponse_nextToken,
    describeConnectionAliasPermissionsResponse_connectionAliasPermissions,
    describeConnectionAliasPermissionsResponse_httpStatus,

    -- ** CreateTags
    createTags_resourceId,
    createTags_tags,
    createTagsResponse_httpStatus,

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

    -- ** DeleteTags
    deleteTags_resourceId,
    deleteTags_tagKeys,
    deleteTagsResponse_httpStatus,

    -- ** ModifyWorkspaceAccessProperties
    modifyWorkspaceAccessProperties_resourceId,
    modifyWorkspaceAccessProperties_workspaceAccessProperties,
    modifyWorkspaceAccessPropertiesResponse_httpStatus,

    -- ** UpdateRulesOfIpGroup
    updateRulesOfIpGroup_groupId,
    updateRulesOfIpGroup_userRules,
    updateRulesOfIpGroupResponse_httpStatus,

    -- ** DeleteWorkspaceImage
    deleteWorkspaceImage_imageId,
    deleteWorkspaceImageResponse_httpStatus,

    -- ** StopWorkspaces
    stopWorkspaces_stopWorkspaceRequests,
    stopWorkspacesResponse_failedRequests,
    stopWorkspacesResponse_httpStatus,

    -- ** AssociateIpGroups
    associateIpGroups_directoryId,
    associateIpGroups_groupIds,
    associateIpGroupsResponse_httpStatus,

    -- ** ModifySelfservicePermissions
    modifySelfservicePermissions_resourceId,
    modifySelfservicePermissions_selfservicePermissions,
    modifySelfservicePermissionsResponse_httpStatus,

    -- ** DeleteConnectionAlias
    deleteConnectionAlias_aliasId,
    deleteConnectionAliasResponse_httpStatus,

    -- ** DescribeWorkspacesConnectionStatus
    describeWorkspacesConnectionStatus_workspaceIds,
    describeWorkspacesConnectionStatus_nextToken,
    describeWorkspacesConnectionStatusResponse_nextToken,
    describeWorkspacesConnectionStatusResponse_workspacesConnectionStatus,
    describeWorkspacesConnectionStatusResponse_httpStatus,

    -- ** CreateConnectionAlias
    createConnectionAlias_tags,
    createConnectionAlias_connectionString,
    createConnectionAliasResponse_aliasId,
    createConnectionAliasResponse_httpStatus,

    -- ** RebootWorkspaces
    rebootWorkspaces_rebootWorkspaceRequests,
    rebootWorkspacesResponse_failedRequests,
    rebootWorkspacesResponse_httpStatus,

    -- ** DeleteIpGroup
    deleteIpGroup_groupId,
    deleteIpGroupResponse_httpStatus,

    -- ** CopyWorkspaceImage
    copyWorkspaceImage_description,
    copyWorkspaceImage_tags,
    copyWorkspaceImage_name,
    copyWorkspaceImage_sourceImageId,
    copyWorkspaceImage_sourceRegion,
    copyWorkspaceImageResponse_imageId,
    copyWorkspaceImageResponse_httpStatus,

    -- ** DescribeWorkspaceSnapshots
    describeWorkspaceSnapshots_workspaceId,
    describeWorkspaceSnapshotsResponse_restoreSnapshots,
    describeWorkspaceSnapshotsResponse_rebuildSnapshots,
    describeWorkspaceSnapshotsResponse_httpStatus,

    -- ** TerminateWorkspaces
    terminateWorkspaces_terminateWorkspaceRequests,
    terminateWorkspacesResponse_failedRequests,
    terminateWorkspacesResponse_httpStatus,

    -- ** UpdateConnectionAliasPermission
    updateConnectionAliasPermission_aliasId,
    updateConnectionAliasPermission_connectionAliasPermission,
    updateConnectionAliasPermissionResponse_httpStatus,

    -- ** CreateWorkspaces
    createWorkspaces_workspaces,
    createWorkspacesResponse_failedRequests,
    createWorkspacesResponse_pendingRequests,
    createWorkspacesResponse_httpStatus,

    -- ** DescribeClientProperties
    describeClientProperties_resourceIds,
    describeClientPropertiesResponse_clientPropertiesList,
    describeClientPropertiesResponse_httpStatus,

    -- ** ModifyClientProperties
    modifyClientProperties_resourceId,
    modifyClientProperties_clientProperties,
    modifyClientPropertiesResponse_httpStatus,

    -- ** DescribeIpGroups
    describeIpGroups_groupIds,
    describeIpGroups_nextToken,
    describeIpGroups_maxResults,
    describeIpGroupsResponse_result,
    describeIpGroupsResponse_nextToken,
    describeIpGroupsResponse_httpStatus,

    -- ** DeleteWorkspaceBundle
    deleteWorkspaceBundle_bundleId,
    deleteWorkspaceBundleResponse_httpStatus,

    -- ** UpdateWorkspaceBundle
    updateWorkspaceBundle_bundleId,
    updateWorkspaceBundle_imageId,
    updateWorkspaceBundleResponse_httpStatus,

    -- ** ListAvailableManagementCidrRanges
    listAvailableManagementCidrRanges_nextToken,
    listAvailableManagementCidrRanges_maxResults,
    listAvailableManagementCidrRanges_managementCidrRangeConstraint,
    listAvailableManagementCidrRangesResponse_managementCidrRanges,
    listAvailableManagementCidrRangesResponse_nextToken,
    listAvailableManagementCidrRangesResponse_httpStatus,

    -- ** UpdateWorkspaceImagePermission
    updateWorkspaceImagePermission_imageId,
    updateWorkspaceImagePermission_allowCopyImage,
    updateWorkspaceImagePermission_sharedAccountId,
    updateWorkspaceImagePermissionResponse_httpStatus,

    -- ** CreateUpdatedWorkspaceImage
    createUpdatedWorkspaceImage_tags,
    createUpdatedWorkspaceImage_name,
    createUpdatedWorkspaceImage_description,
    createUpdatedWorkspaceImage_sourceImageId,
    createUpdatedWorkspaceImageResponse_imageId,
    createUpdatedWorkspaceImageResponse_httpStatus,

    -- ** DescribeWorkspaces
    describeWorkspaces_directoryId,
    describeWorkspaces_workspaceIds,
    describeWorkspaces_userName,
    describeWorkspaces_bundleId,
    describeWorkspaces_nextToken,
    describeWorkspaces_limit,
    describeWorkspacesResponse_nextToken,
    describeWorkspacesResponse_workspaces,
    describeWorkspacesResponse_httpStatus,

    -- ** DescribeConnectionAliases
    describeConnectionAliases_resourceId,
    describeConnectionAliases_aliasIds,
    describeConnectionAliases_nextToken,
    describeConnectionAliases_limit,
    describeConnectionAliasesResponse_connectionAliases,
    describeConnectionAliasesResponse_nextToken,
    describeConnectionAliasesResponse_httpStatus,

    -- ** StartWorkspaces
    startWorkspaces_startWorkspaceRequests,
    startWorkspacesResponse_failedRequests,
    startWorkspacesResponse_httpStatus,

    -- ** DescribeAccountModifications
    describeAccountModifications_nextToken,
    describeAccountModificationsResponse_accountModifications,
    describeAccountModificationsResponse_nextToken,
    describeAccountModificationsResponse_httpStatus,

    -- ** ModifyAccount
    modifyAccount_dedicatedTenancySupport,
    modifyAccount_dedicatedTenancyManagementCidrRange,
    modifyAccountResponse_httpStatus,

    -- * Types

    -- ** AccountModification
    accountModification_startTime,
    accountModification_dedicatedTenancySupport,
    accountModification_modificationState,
    accountModification_dedicatedTenancyManagementCidrRange,
    accountModification_errorCode,
    accountModification_errorMessage,

    -- ** ClientProperties
    clientProperties_reconnectEnabled,

    -- ** ClientPropertiesResult
    clientPropertiesResult_resourceId,
    clientPropertiesResult_clientProperties,

    -- ** ComputeType
    computeType_name,

    -- ** ConnectionAlias
    connectionAlias_state,
    connectionAlias_ownerAccountId,
    connectionAlias_aliasId,
    connectionAlias_associations,
    connectionAlias_connectionString,

    -- ** ConnectionAliasAssociation
    connectionAliasAssociation_associatedAccountId,
    connectionAliasAssociation_resourceId,
    connectionAliasAssociation_associationStatus,
    connectionAliasAssociation_connectionIdentifier,

    -- ** ConnectionAliasPermission
    connectionAliasPermission_sharedAccountId,
    connectionAliasPermission_allowAssociation,

    -- ** DefaultWorkspaceCreationProperties
    defaultWorkspaceCreationProperties_customSecurityGroupId,
    defaultWorkspaceCreationProperties_userEnabledAsLocalAdministrator,
    defaultWorkspaceCreationProperties_enableWorkDocs,
    defaultWorkspaceCreationProperties_enableMaintenanceMode,
    defaultWorkspaceCreationProperties_enableInternetAccess,
    defaultWorkspaceCreationProperties_defaultOu,

    -- ** FailedCreateWorkspaceRequest
    failedCreateWorkspaceRequest_workspaceRequest,
    failedCreateWorkspaceRequest_errorCode,
    failedCreateWorkspaceRequest_errorMessage,

    -- ** FailedWorkspaceChangeRequest
    failedWorkspaceChangeRequest_errorCode,
    failedWorkspaceChangeRequest_workspaceId,
    failedWorkspaceChangeRequest_errorMessage,

    -- ** ImagePermission
    imagePermission_sharedAccountId,

    -- ** IpRuleItem
    ipRuleItem_ruleDesc,
    ipRuleItem_ipRule,

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
    selfservicePermissions_restartWorkspace,
    selfservicePermissions_changeComputeType,
    selfservicePermissions_switchRunningMode,
    selfservicePermissions_rebuildWorkspace,
    selfservicePermissions_increaseVolumeSize,

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
    workspace_directoryId,
    workspace_state,
    workspace_ipAddress,
    workspace_modificationStates,
    workspace_userName,
    workspace_subnetId,
    workspace_bundleId,
    workspace_workspaceProperties,
    workspace_rootVolumeEncryptionEnabled,
    workspace_errorCode,
    workspace_volumeEncryptionKey,
    workspace_computerName,
    workspace_workspaceId,
    workspace_userVolumeEncryptionEnabled,
    workspace_errorMessage,

    -- ** WorkspaceAccessProperties
    workspaceAccessProperties_deviceTypeWindows,
    workspaceAccessProperties_deviceTypeWeb,
    workspaceAccessProperties_deviceTypeAndroid,
    workspaceAccessProperties_deviceTypeLinux,
    workspaceAccessProperties_deviceTypeOsx,
    workspaceAccessProperties_deviceTypeChromeOs,
    workspaceAccessProperties_deviceTypeIos,
    workspaceAccessProperties_deviceTypeZeroClient,

    -- ** WorkspaceBundle
    workspaceBundle_creationTime,
    workspaceBundle_lastUpdatedTime,
    workspaceBundle_bundleId,
    workspaceBundle_owner,
    workspaceBundle_rootStorage,
    workspaceBundle_name,
    workspaceBundle_imageId,
    workspaceBundle_computeType,
    workspaceBundle_userStorage,
    workspaceBundle_description,

    -- ** WorkspaceConnectionStatus
    workspaceConnectionStatus_lastKnownUserConnectionTimestamp,
    workspaceConnectionStatus_connectionStateCheckTimestamp,
    workspaceConnectionStatus_workspaceId,
    workspaceConnectionStatus_connectionState,

    -- ** WorkspaceCreationProperties
    workspaceCreationProperties_customSecurityGroupId,
    workspaceCreationProperties_userEnabledAsLocalAdministrator,
    workspaceCreationProperties_enableWorkDocs,
    workspaceCreationProperties_enableMaintenanceMode,
    workspaceCreationProperties_enableInternetAccess,
    workspaceCreationProperties_defaultOu,

    -- ** WorkspaceDirectory
    workspaceDirectory_registrationCode,
    workspaceDirectory_iamRoleId,
    workspaceDirectory_directoryId,
    workspaceDirectory_state,
    workspaceDirectory_customerUserName,
    workspaceDirectory_subnetIds,
    workspaceDirectory_ipGroupIds,
    workspaceDirectory_alias,
    workspaceDirectory_workspaceSecurityGroupId,
    workspaceDirectory_directoryType,
    workspaceDirectory_tenancy,
    workspaceDirectory_workspaceCreationProperties,
    workspaceDirectory_dnsIpAddresses,
    workspaceDirectory_workspaceAccessProperties,
    workspaceDirectory_directoryName,
    workspaceDirectory_selfservicePermissions,

    -- ** WorkspaceImage
    workspaceImage_state,
    workspaceImage_ownerAccountId,
    workspaceImage_operatingSystem,
    workspaceImage_created,
    workspaceImage_requiredTenancy,
    workspaceImage_name,
    workspaceImage_updates,
    workspaceImage_imageId,
    workspaceImage_errorCode,
    workspaceImage_errorMessage,
    workspaceImage_description,

    -- ** WorkspaceProperties
    workspaceProperties_computeTypeName,
    workspaceProperties_runningMode,
    workspaceProperties_rootVolumeSizeGib,
    workspaceProperties_runningModeAutoStopTimeoutInMinutes,
    workspaceProperties_userVolumeSizeGib,

    -- ** WorkspaceRequest
    workspaceRequest_workspaceProperties,
    workspaceRequest_rootVolumeEncryptionEnabled,
    workspaceRequest_volumeEncryptionKey,
    workspaceRequest_userVolumeEncryptionEnabled,
    workspaceRequest_tags,
    workspaceRequest_directoryId,
    workspaceRequest_userName,
    workspaceRequest_bundleId,

    -- ** WorkspacesIpGroup
    workspacesIpGroup_groupDesc,
    workspacesIpGroup_userRules,
    workspacesIpGroup_groupId,
    workspacesIpGroup_groupName,
  )
where

import Network.AWS.WorkSpaces.AssociateConnectionAlias
import Network.AWS.WorkSpaces.AssociateIpGroups
import Network.AWS.WorkSpaces.AuthorizeIpRules
import Network.AWS.WorkSpaces.CopyWorkspaceImage
import Network.AWS.WorkSpaces.CreateConnectionAlias
import Network.AWS.WorkSpaces.CreateIpGroup
import Network.AWS.WorkSpaces.CreateTags
import Network.AWS.WorkSpaces.CreateUpdatedWorkspaceImage
import Network.AWS.WorkSpaces.CreateWorkspaceBundle
import Network.AWS.WorkSpaces.CreateWorkspaces
import Network.AWS.WorkSpaces.DeleteConnectionAlias
import Network.AWS.WorkSpaces.DeleteIpGroup
import Network.AWS.WorkSpaces.DeleteTags
import Network.AWS.WorkSpaces.DeleteWorkspaceBundle
import Network.AWS.WorkSpaces.DeleteWorkspaceImage
import Network.AWS.WorkSpaces.DeregisterWorkspaceDirectory
import Network.AWS.WorkSpaces.DescribeAccount
import Network.AWS.WorkSpaces.DescribeAccountModifications
import Network.AWS.WorkSpaces.DescribeClientProperties
import Network.AWS.WorkSpaces.DescribeConnectionAliasPermissions
import Network.AWS.WorkSpaces.DescribeConnectionAliases
import Network.AWS.WorkSpaces.DescribeIpGroups
import Network.AWS.WorkSpaces.DescribeTags
import Network.AWS.WorkSpaces.DescribeWorkspaceBundles
import Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
import Network.AWS.WorkSpaces.DescribeWorkspaceImagePermissions
import Network.AWS.WorkSpaces.DescribeWorkspaceImages
import Network.AWS.WorkSpaces.DescribeWorkspaceSnapshots
import Network.AWS.WorkSpaces.DescribeWorkspaces
import Network.AWS.WorkSpaces.DescribeWorkspacesConnectionStatus
import Network.AWS.WorkSpaces.DisassociateConnectionAlias
import Network.AWS.WorkSpaces.DisassociateIpGroups
import Network.AWS.WorkSpaces.ImportWorkspaceImage
import Network.AWS.WorkSpaces.ListAvailableManagementCidrRanges
import Network.AWS.WorkSpaces.MigrateWorkspace
import Network.AWS.WorkSpaces.ModifyAccount
import Network.AWS.WorkSpaces.ModifyClientProperties
import Network.AWS.WorkSpaces.ModifySelfservicePermissions
import Network.AWS.WorkSpaces.ModifyWorkspaceAccessProperties
import Network.AWS.WorkSpaces.ModifyWorkspaceCreationProperties
import Network.AWS.WorkSpaces.ModifyWorkspaceProperties
import Network.AWS.WorkSpaces.ModifyWorkspaceState
import Network.AWS.WorkSpaces.RebootWorkspaces
import Network.AWS.WorkSpaces.RebuildWorkspaces
import Network.AWS.WorkSpaces.RegisterWorkspaceDirectory
import Network.AWS.WorkSpaces.RestoreWorkspace
import Network.AWS.WorkSpaces.RevokeIpRules
import Network.AWS.WorkSpaces.StartWorkspaces
import Network.AWS.WorkSpaces.StopWorkspaces
import Network.AWS.WorkSpaces.TerminateWorkspaces
import Network.AWS.WorkSpaces.Types.AccountModification
import Network.AWS.WorkSpaces.Types.ClientProperties
import Network.AWS.WorkSpaces.Types.ClientPropertiesResult
import Network.AWS.WorkSpaces.Types.ComputeType
import Network.AWS.WorkSpaces.Types.ConnectionAlias
import Network.AWS.WorkSpaces.Types.ConnectionAliasAssociation
import Network.AWS.WorkSpaces.Types.ConnectionAliasPermission
import Network.AWS.WorkSpaces.Types.DefaultWorkspaceCreationProperties
import Network.AWS.WorkSpaces.Types.FailedCreateWorkspaceRequest
import Network.AWS.WorkSpaces.Types.FailedWorkspaceChangeRequest
import Network.AWS.WorkSpaces.Types.ImagePermission
import Network.AWS.WorkSpaces.Types.IpRuleItem
import Network.AWS.WorkSpaces.Types.ModificationState
import Network.AWS.WorkSpaces.Types.OperatingSystem
import Network.AWS.WorkSpaces.Types.RebootRequest
import Network.AWS.WorkSpaces.Types.RebuildRequest
import Network.AWS.WorkSpaces.Types.RootStorage
import Network.AWS.WorkSpaces.Types.SelfservicePermissions
import Network.AWS.WorkSpaces.Types.Snapshot
import Network.AWS.WorkSpaces.Types.StartRequest
import Network.AWS.WorkSpaces.Types.StopRequest
import Network.AWS.WorkSpaces.Types.Tag
import Network.AWS.WorkSpaces.Types.TerminateRequest
import Network.AWS.WorkSpaces.Types.UpdateResult
import Network.AWS.WorkSpaces.Types.UserStorage
import Network.AWS.WorkSpaces.Types.Workspace
import Network.AWS.WorkSpaces.Types.WorkspaceAccessProperties
import Network.AWS.WorkSpaces.Types.WorkspaceBundle
import Network.AWS.WorkSpaces.Types.WorkspaceConnectionStatus
import Network.AWS.WorkSpaces.Types.WorkspaceCreationProperties
import Network.AWS.WorkSpaces.Types.WorkspaceDirectory
import Network.AWS.WorkSpaces.Types.WorkspaceImage
import Network.AWS.WorkSpaces.Types.WorkspaceProperties
import Network.AWS.WorkSpaces.Types.WorkspaceRequest
import Network.AWS.WorkSpaces.Types.WorkspacesIpGroup
import Network.AWS.WorkSpaces.UpdateConnectionAliasPermission
import Network.AWS.WorkSpaces.UpdateRulesOfIpGroup
import Network.AWS.WorkSpaces.UpdateWorkspaceBundle
import Network.AWS.WorkSpaces.UpdateWorkspaceImagePermission
