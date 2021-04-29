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

    -- ** DescribeWorkspaceDirectories
    describeWorkspaceDirectories_nextToken,
    describeWorkspaceDirectories_directoryIds,
    describeWorkspaceDirectories_limit,
    describeWorkspaceDirectoriesResponse_nextToken,
    describeWorkspaceDirectoriesResponse_directories,
    describeWorkspaceDirectoriesResponse_httpStatus,

    -- ** TerminateWorkspaces
    terminateWorkspaces_terminateWorkspaceRequests,
    terminateWorkspacesResponse_failedRequests,
    terminateWorkspacesResponse_httpStatus,

    -- ** DisassociateIpGroups
    disassociateIpGroups_directoryId,
    disassociateIpGroups_groupIds,
    disassociateIpGroupsResponse_httpStatus,

    -- ** DescribeWorkspaceBundles
    describeWorkspaceBundles_nextToken,
    describeWorkspaceBundles_owner,
    describeWorkspaceBundles_bundleIds,
    describeWorkspaceBundlesResponse_nextToken,
    describeWorkspaceBundlesResponse_bundles,
    describeWorkspaceBundlesResponse_httpStatus,

    -- ** AuthorizeIpRules
    authorizeIpRules_groupId,
    authorizeIpRules_userRules,
    authorizeIpRulesResponse_httpStatus,

    -- ** ImportWorkspaceImage
    importWorkspaceImage_tags,
    importWorkspaceImage_applications,
    importWorkspaceImage_ec2ImageId,
    importWorkspaceImage_ingestionProcess,
    importWorkspaceImage_imageName,
    importWorkspaceImage_imageDescription,
    importWorkspaceImageResponse_imageId,
    importWorkspaceImageResponse_httpStatus,

    -- ** DeleteIpGroup
    deleteIpGroup_groupId,
    deleteIpGroupResponse_httpStatus,

    -- ** DeregisterWorkspaceDirectory
    deregisterWorkspaceDirectory_directoryId,
    deregisterWorkspaceDirectoryResponse_httpStatus,

    -- ** AssociateConnectionAlias
    associateConnectionAlias_aliasId,
    associateConnectionAlias_resourceId,
    associateConnectionAliasResponse_connectionIdentifier,
    associateConnectionAliasResponse_httpStatus,

    -- ** DescribeTags
    describeTags_resourceId,
    describeTagsResponse_tagList,
    describeTagsResponse_httpStatus,

    -- ** CreateConnectionAlias
    createConnectionAlias_tags,
    createConnectionAlias_connectionString,
    createConnectionAliasResponse_aliasId,
    createConnectionAliasResponse_httpStatus,

    -- ** MigrateWorkspace
    migrateWorkspace_sourceWorkspaceId,
    migrateWorkspace_bundleId,
    migrateWorkspaceResponse_targetWorkspaceId,
    migrateWorkspaceResponse_sourceWorkspaceId,
    migrateWorkspaceResponse_httpStatus,

    -- ** ModifyAccount
    modifyAccount_dedicatedTenancySupport,
    modifyAccount_dedicatedTenancyManagementCidrRange,
    modifyAccountResponse_httpStatus,

    -- ** DescribeWorkspacesConnectionStatus
    describeWorkspacesConnectionStatus_nextToken,
    describeWorkspacesConnectionStatus_workspaceIds,
    describeWorkspacesConnectionStatusResponse_nextToken,
    describeWorkspacesConnectionStatusResponse_workspacesConnectionStatus,
    describeWorkspacesConnectionStatusResponse_httpStatus,

    -- ** ModifySelfservicePermissions
    modifySelfservicePermissions_resourceId,
    modifySelfservicePermissions_selfservicePermissions,
    modifySelfservicePermissionsResponse_httpStatus,

    -- ** UpdateRulesOfIpGroup
    updateRulesOfIpGroup_groupId,
    updateRulesOfIpGroup_userRules,
    updateRulesOfIpGroupResponse_httpStatus,

    -- ** DescribeConnectionAliases
    describeConnectionAliases_resourceId,
    describeConnectionAliases_nextToken,
    describeConnectionAliases_aliasIds,
    describeConnectionAliases_limit,
    describeConnectionAliasesResponse_nextToken,
    describeConnectionAliasesResponse_connectionAliases,
    describeConnectionAliasesResponse_httpStatus,

    -- ** DeleteWorkspaceImage
    deleteWorkspaceImage_imageId,
    deleteWorkspaceImageResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_resourceId,
    deleteTags_tagKeys,
    deleteTagsResponse_httpStatus,

    -- ** ListAvailableManagementCidrRanges
    listAvailableManagementCidrRanges_nextToken,
    listAvailableManagementCidrRanges_maxResults,
    listAvailableManagementCidrRanges_managementCidrRangeConstraint,
    listAvailableManagementCidrRangesResponse_nextToken,
    listAvailableManagementCidrRangesResponse_managementCidrRanges,
    listAvailableManagementCidrRangesResponse_httpStatus,

    -- ** ModifyWorkspaceCreationProperties
    modifyWorkspaceCreationProperties_resourceId,
    modifyWorkspaceCreationProperties_workspaceCreationProperties,
    modifyWorkspaceCreationPropertiesResponse_httpStatus,

    -- ** DescribeClientProperties
    describeClientProperties_resourceIds,
    describeClientPropertiesResponse_clientPropertiesList,
    describeClientPropertiesResponse_httpStatus,

    -- ** ModifyWorkspaceState
    modifyWorkspaceState_workspaceId,
    modifyWorkspaceState_workspaceState,
    modifyWorkspaceStateResponse_httpStatus,

    -- ** UpdateConnectionAliasPermission
    updateConnectionAliasPermission_aliasId,
    updateConnectionAliasPermission_connectionAliasPermission,
    updateConnectionAliasPermissionResponse_httpStatus,

    -- ** CopyWorkspaceImage
    copyWorkspaceImage_tags,
    copyWorkspaceImage_description,
    copyWorkspaceImage_name,
    copyWorkspaceImage_sourceImageId,
    copyWorkspaceImage_sourceRegion,
    copyWorkspaceImageResponse_imageId,
    copyWorkspaceImageResponse_httpStatus,

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

    -- ** RebootWorkspaces
    rebootWorkspaces_rebootWorkspaceRequests,
    rebootWorkspacesResponse_failedRequests,
    rebootWorkspacesResponse_httpStatus,

    -- ** DescribeWorkspaceSnapshots
    describeWorkspaceSnapshots_workspaceId,
    describeWorkspaceSnapshotsResponse_rebuildSnapshots,
    describeWorkspaceSnapshotsResponse_restoreSnapshots,
    describeWorkspaceSnapshotsResponse_httpStatus,

    -- ** DescribeAccount
    describeAccountResponse_dedicatedTenancySupport,
    describeAccountResponse_dedicatedTenancyManagementCidrRange,
    describeAccountResponse_httpStatus,

    -- ** ModifyWorkspaceProperties
    modifyWorkspaceProperties_workspaceId,
    modifyWorkspaceProperties_workspaceProperties,
    modifyWorkspacePropertiesResponse_httpStatus,

    -- ** RevokeIpRules
    revokeIpRules_groupId,
    revokeIpRules_userRules,
    revokeIpRulesResponse_httpStatus,

    -- ** DescribeWorkspaceImages
    describeWorkspaceImages_imageType,
    describeWorkspaceImages_nextToken,
    describeWorkspaceImages_imageIds,
    describeWorkspaceImages_maxResults,
    describeWorkspaceImagesResponse_nextToken,
    describeWorkspaceImagesResponse_images,
    describeWorkspaceImagesResponse_httpStatus,

    -- ** DescribeAccountModifications
    describeAccountModifications_nextToken,
    describeAccountModificationsResponse_nextToken,
    describeAccountModificationsResponse_accountModifications,
    describeAccountModificationsResponse_httpStatus,

    -- ** DeleteConnectionAlias
    deleteConnectionAlias_aliasId,
    deleteConnectionAliasResponse_httpStatus,

    -- ** AssociateIpGroups
    associateIpGroups_directoryId,
    associateIpGroups_groupIds,
    associateIpGroupsResponse_httpStatus,

    -- ** StopWorkspaces
    stopWorkspaces_stopWorkspaceRequests,
    stopWorkspacesResponse_failedRequests,
    stopWorkspacesResponse_httpStatus,

    -- ** StartWorkspaces
    startWorkspaces_startWorkspaceRequests,
    startWorkspacesResponse_failedRequests,
    startWorkspacesResponse_httpStatus,

    -- ** DescribeWorkspaces
    describeWorkspaces_nextToken,
    describeWorkspaces_bundleId,
    describeWorkspaces_workspaceIds,
    describeWorkspaces_directoryId,
    describeWorkspaces_userName,
    describeWorkspaces_limit,
    describeWorkspacesResponse_nextToken,
    describeWorkspacesResponse_workspaces,
    describeWorkspacesResponse_httpStatus,

    -- ** UpdateWorkspaceImagePermission
    updateWorkspaceImagePermission_imageId,
    updateWorkspaceImagePermission_allowCopyImage,
    updateWorkspaceImagePermission_sharedAccountId,
    updateWorkspaceImagePermissionResponse_httpStatus,

    -- ** ModifyClientProperties
    modifyClientProperties_resourceId,
    modifyClientProperties_clientProperties,
    modifyClientPropertiesResponse_httpStatus,

    -- ** ModifyWorkspaceAccessProperties
    modifyWorkspaceAccessProperties_resourceId,
    modifyWorkspaceAccessProperties_workspaceAccessProperties,
    modifyWorkspaceAccessPropertiesResponse_httpStatus,

    -- ** DescribeIpGroups
    describeIpGroups_nextToken,
    describeIpGroups_groupIds,
    describeIpGroups_maxResults,
    describeIpGroupsResponse_nextToken,
    describeIpGroupsResponse_result,
    describeIpGroupsResponse_httpStatus,

    -- ** RestoreWorkspace
    restoreWorkspace_workspaceId,
    restoreWorkspaceResponse_httpStatus,

    -- ** CreateTags
    createTags_resourceId,
    createTags_tags,
    createTagsResponse_httpStatus,

    -- ** DescribeConnectionAliasPermissions
    describeConnectionAliasPermissions_nextToken,
    describeConnectionAliasPermissions_maxResults,
    describeConnectionAliasPermissions_aliasId,
    describeConnectionAliasPermissionsResponse_nextToken,
    describeConnectionAliasPermissionsResponse_aliasId,
    describeConnectionAliasPermissionsResponse_connectionAliasPermissions,
    describeConnectionAliasPermissionsResponse_httpStatus,

    -- ** RegisterWorkspaceDirectory
    registerWorkspaceDirectory_subnetIds,
    registerWorkspaceDirectory_tenancy,
    registerWorkspaceDirectory_tags,
    registerWorkspaceDirectory_enableSelfService,
    registerWorkspaceDirectory_directoryId,
    registerWorkspaceDirectory_enableWorkDocs,
    registerWorkspaceDirectoryResponse_httpStatus,

    -- ** CreateWorkspaces
    createWorkspaces_workspaces,
    createWorkspacesResponse_failedRequests,
    createWorkspacesResponse_pendingRequests,
    createWorkspacesResponse_httpStatus,

    -- ** CreateIpGroup
    createIpGroup_userRules,
    createIpGroup_groupDesc,
    createIpGroup_tags,
    createIpGroup_groupName,
    createIpGroupResponse_groupId,
    createIpGroupResponse_httpStatus,

    -- ** DisassociateConnectionAlias
    disassociateConnectionAlias_aliasId,
    disassociateConnectionAliasResponse_httpStatus,

    -- * Types

    -- ** AccountModification
    accountModification_dedicatedTenancySupport,
    accountModification_startTime,
    accountModification_dedicatedTenancyManagementCidrRange,
    accountModification_modificationState,
    accountModification_errorMessage,
    accountModification_errorCode,

    -- ** ClientProperties
    clientProperties_reconnectEnabled,

    -- ** ClientPropertiesResult
    clientPropertiesResult_resourceId,
    clientPropertiesResult_clientProperties,

    -- ** ComputeType
    computeType_name,

    -- ** ConnectionAlias
    connectionAlias_state,
    connectionAlias_aliasId,
    connectionAlias_connectionString,
    connectionAlias_ownerAccountId,
    connectionAlias_associations,

    -- ** ConnectionAliasAssociation
    connectionAliasAssociation_resourceId,
    connectionAliasAssociation_connectionIdentifier,
    connectionAliasAssociation_associatedAccountId,
    connectionAliasAssociation_associationStatus,

    -- ** ConnectionAliasPermission
    connectionAliasPermission_sharedAccountId,
    connectionAliasPermission_allowAssociation,

    -- ** DefaultWorkspaceCreationProperties
    defaultWorkspaceCreationProperties_enableMaintenanceMode,
    defaultWorkspaceCreationProperties_defaultOu,
    defaultWorkspaceCreationProperties_enableInternetAccess,
    defaultWorkspaceCreationProperties_enableWorkDocs,
    defaultWorkspaceCreationProperties_customSecurityGroupId,
    defaultWorkspaceCreationProperties_userEnabledAsLocalAdministrator,

    -- ** FailedCreateWorkspaceRequest
    failedCreateWorkspaceRequest_workspaceRequest,
    failedCreateWorkspaceRequest_errorMessage,
    failedCreateWorkspaceRequest_errorCode,

    -- ** FailedWorkspaceChangeRequest
    failedWorkspaceChangeRequest_workspaceId,
    failedWorkspaceChangeRequest_errorMessage,
    failedWorkspaceChangeRequest_errorCode,

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
    selfservicePermissions_switchRunningMode,
    selfservicePermissions_restartWorkspace,
    selfservicePermissions_rebuildWorkspace,
    selfservicePermissions_increaseVolumeSize,
    selfservicePermissions_changeComputeType,

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

    -- ** UserStorage
    userStorage_capacity,

    -- ** Workspace
    workspace_workspaceProperties,
    workspace_rootVolumeEncryptionEnabled,
    workspace_bundleId,
    workspace_userVolumeEncryptionEnabled,
    workspace_volumeEncryptionKey,
    workspace_workspaceId,
    workspace_modificationStates,
    workspace_ipAddress,
    workspace_state,
    workspace_directoryId,
    workspace_userName,
    workspace_subnetId,
    workspace_errorMessage,
    workspace_computerName,
    workspace_errorCode,

    -- ** WorkspaceAccessProperties
    workspaceAccessProperties_deviceTypeOsx,
    workspaceAccessProperties_deviceTypeWindows,
    workspaceAccessProperties_deviceTypeAndroid,
    workspaceAccessProperties_deviceTypeZeroClient,
    workspaceAccessProperties_deviceTypeWeb,
    workspaceAccessProperties_deviceTypeIos,
    workspaceAccessProperties_deviceTypeChromeOs,

    -- ** WorkspaceBundle
    workspaceBundle_rootStorage,
    workspaceBundle_bundleId,
    workspaceBundle_userStorage,
    workspaceBundle_imageId,
    workspaceBundle_name,
    workspaceBundle_owner,
    workspaceBundle_description,
    workspaceBundle_computeType,
    workspaceBundle_lastUpdatedTime,

    -- ** WorkspaceConnectionStatus
    workspaceConnectionStatus_connectionState,
    workspaceConnectionStatus_workspaceId,
    workspaceConnectionStatus_lastKnownUserConnectionTimestamp,
    workspaceConnectionStatus_connectionStateCheckTimestamp,

    -- ** WorkspaceCreationProperties
    workspaceCreationProperties_enableMaintenanceMode,
    workspaceCreationProperties_defaultOu,
    workspaceCreationProperties_enableInternetAccess,
    workspaceCreationProperties_enableWorkDocs,
    workspaceCreationProperties_customSecurityGroupId,
    workspaceCreationProperties_userEnabledAsLocalAdministrator,

    -- ** WorkspaceDirectory
    workspaceDirectory_registrationCode,
    workspaceDirectory_workspaceSecurityGroupId,
    workspaceDirectory_alias,
    workspaceDirectory_ipGroupIds,
    workspaceDirectory_workspaceAccessProperties,
    workspaceDirectory_subnetIds,
    workspaceDirectory_tenancy,
    workspaceDirectory_customerUserName,
    workspaceDirectory_state,
    workspaceDirectory_iamRoleId,
    workspaceDirectory_directoryId,
    workspaceDirectory_selfservicePermissions,
    workspaceDirectory_directoryType,
    workspaceDirectory_directoryName,
    workspaceDirectory_dnsIpAddresses,
    workspaceDirectory_workspaceCreationProperties,

    -- ** WorkspaceImage
    workspaceImage_imageId,
    workspaceImage_state,
    workspaceImage_name,
    workspaceImage_description,
    workspaceImage_errorMessage,
    workspaceImage_requiredTenancy,
    workspaceImage_operatingSystem,
    workspaceImage_created,
    workspaceImage_ownerAccountId,
    workspaceImage_errorCode,

    -- ** WorkspaceProperties
    workspaceProperties_rootVolumeSizeGib,
    workspaceProperties_runningMode,
    workspaceProperties_userVolumeSizeGib,
    workspaceProperties_runningModeAutoStopTimeoutInMinutes,
    workspaceProperties_computeTypeName,

    -- ** WorkspaceRequest
    workspaceRequest_workspaceProperties,
    workspaceRequest_rootVolumeEncryptionEnabled,
    workspaceRequest_userVolumeEncryptionEnabled,
    workspaceRequest_volumeEncryptionKey,
    workspaceRequest_tags,
    workspaceRequest_directoryId,
    workspaceRequest_userName,
    workspaceRequest_bundleId,

    -- ** WorkspacesIpGroup
    workspacesIpGroup_userRules,
    workspacesIpGroup_groupDesc,
    workspacesIpGroup_groupName,
    workspacesIpGroup_groupId,
  )
where

import Network.AWS.WorkSpaces.AssociateConnectionAlias
import Network.AWS.WorkSpaces.AssociateIpGroups
import Network.AWS.WorkSpaces.AuthorizeIpRules
import Network.AWS.WorkSpaces.CopyWorkspaceImage
import Network.AWS.WorkSpaces.CreateConnectionAlias
import Network.AWS.WorkSpaces.CreateIpGroup
import Network.AWS.WorkSpaces.CreateTags
import Network.AWS.WorkSpaces.CreateWorkspaces
import Network.AWS.WorkSpaces.DeleteConnectionAlias
import Network.AWS.WorkSpaces.DeleteIpGroup
import Network.AWS.WorkSpaces.DeleteTags
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
import Network.AWS.WorkSpaces.UpdateWorkspaceImagePermission
