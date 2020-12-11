{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon WorkSpaces Service__
--
-- Amazon WorkSpaces enables you to provision virtual, cloud-based Microsoft Windows and Amazon Linux desktops for your users.
module Network.AWS.WorkSpaces
  ( -- * Service configuration
    workSpacesService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateConnectionAlias
    module Network.AWS.WorkSpaces.AssociateConnectionAlias,

    -- ** DescribeAccount
    module Network.AWS.WorkSpaces.DescribeAccount,

    -- ** RevokeIPRules
    module Network.AWS.WorkSpaces.RevokeIPRules,

    -- ** DescribeWorkspaceImages (Paginated)
    module Network.AWS.WorkSpaces.DescribeWorkspaceImages,

    -- ** ModifyWorkspaceProperties
    module Network.AWS.WorkSpaces.ModifyWorkspaceProperties,

    -- ** DeregisterWorkspaceDirectory
    module Network.AWS.WorkSpaces.DeregisterWorkspaceDirectory,

    -- ** MigrateWorkspace
    module Network.AWS.WorkSpaces.MigrateWorkspace,

    -- ** DescribeTags
    module Network.AWS.WorkSpaces.DescribeTags,

    -- ** DescribeWorkspaceDirectories (Paginated)
    module Network.AWS.WorkSpaces.DescribeWorkspaceDirectories,

    -- ** DisassociateIPGroups
    module Network.AWS.WorkSpaces.DisassociateIPGroups,

    -- ** DescribeWorkspaceBundles (Paginated)
    module Network.AWS.WorkSpaces.DescribeWorkspaceBundles,

    -- ** AuthorizeIPRules
    module Network.AWS.WorkSpaces.AuthorizeIPRules,

    -- ** DescribeWorkspaceImagePermissions
    module Network.AWS.WorkSpaces.DescribeWorkspaceImagePermissions,

    -- ** RebuildWorkspaces
    module Network.AWS.WorkSpaces.RebuildWorkspaces,

    -- ** ImportWorkspaceImage
    module Network.AWS.WorkSpaces.ImportWorkspaceImage,

    -- ** ModifyWorkspaceState
    module Network.AWS.WorkSpaces.ModifyWorkspaceState,

    -- ** CreateIPGroup
    module Network.AWS.WorkSpaces.CreateIPGroup,

    -- ** DisassociateConnectionAlias
    module Network.AWS.WorkSpaces.DisassociateConnectionAlias,

    -- ** ModifyWorkspaceCreationProperties
    module Network.AWS.WorkSpaces.ModifyWorkspaceCreationProperties,

    -- ** RegisterWorkspaceDirectory
    module Network.AWS.WorkSpaces.RegisterWorkspaceDirectory,

    -- ** RestoreWorkspace
    module Network.AWS.WorkSpaces.RestoreWorkspace,

    -- ** DescribeConnectionAliasPermissions
    module Network.AWS.WorkSpaces.DescribeConnectionAliasPermissions,

    -- ** CreateTags
    module Network.AWS.WorkSpaces.CreateTags,

    -- ** DeleteTags
    module Network.AWS.WorkSpaces.DeleteTags,

    -- ** ModifyWorkspaceAccessProperties
    module Network.AWS.WorkSpaces.ModifyWorkspaceAccessProperties,

    -- ** UpdateRulesOfIPGroup
    module Network.AWS.WorkSpaces.UpdateRulesOfIPGroup,

    -- ** DeleteWorkspaceImage
    module Network.AWS.WorkSpaces.DeleteWorkspaceImage,

    -- ** StopWorkspaces
    module Network.AWS.WorkSpaces.StopWorkspaces,

    -- ** AssociateIPGroups
    module Network.AWS.WorkSpaces.AssociateIPGroups,

    -- ** ModifySelfservicePermissions
    module Network.AWS.WorkSpaces.ModifySelfservicePermissions,

    -- ** DeleteConnectionAlias
    module Network.AWS.WorkSpaces.DeleteConnectionAlias,

    -- ** DescribeWorkspacesConnectionStatus (Paginated)
    module Network.AWS.WorkSpaces.DescribeWorkspacesConnectionStatus,

    -- ** CreateConnectionAlias
    module Network.AWS.WorkSpaces.CreateConnectionAlias,

    -- ** RebootWorkspaces
    module Network.AWS.WorkSpaces.RebootWorkspaces,

    -- ** DeleteIPGroup
    module Network.AWS.WorkSpaces.DeleteIPGroup,

    -- ** CopyWorkspaceImage
    module Network.AWS.WorkSpaces.CopyWorkspaceImage,

    -- ** DescribeWorkspaceSnapshots
    module Network.AWS.WorkSpaces.DescribeWorkspaceSnapshots,

    -- ** TerminateWorkspaces
    module Network.AWS.WorkSpaces.TerminateWorkspaces,

    -- ** UpdateConnectionAliasPermission
    module Network.AWS.WorkSpaces.UpdateConnectionAliasPermission,

    -- ** CreateWorkspaces
    module Network.AWS.WorkSpaces.CreateWorkspaces,

    -- ** DescribeClientProperties
    module Network.AWS.WorkSpaces.DescribeClientProperties,

    -- ** ModifyClientProperties
    module Network.AWS.WorkSpaces.ModifyClientProperties,

    -- ** DescribeIPGroups (Paginated)
    module Network.AWS.WorkSpaces.DescribeIPGroups,

    -- ** ListAvailableManagementCidrRanges (Paginated)
    module Network.AWS.WorkSpaces.ListAvailableManagementCidrRanges,

    -- ** UpdateWorkspaceImagePermission
    module Network.AWS.WorkSpaces.UpdateWorkspaceImagePermission,

    -- ** DescribeWorkspaces (Paginated)
    module Network.AWS.WorkSpaces.DescribeWorkspaces,

    -- ** DescribeConnectionAliases
    module Network.AWS.WorkSpaces.DescribeConnectionAliases,

    -- ** StartWorkspaces
    module Network.AWS.WorkSpaces.StartWorkspaces,

    -- ** DescribeAccountModifications (Paginated)
    module Network.AWS.WorkSpaces.DescribeAccountModifications,

    -- ** ModifyAccount
    module Network.AWS.WorkSpaces.ModifyAccount,

    -- * Types

    -- ** AccessPropertyValue
    AccessPropertyValue (..),

    -- ** Application
    Application (..),

    -- ** AssociationStatus
    AssociationStatus (..),

    -- ** Compute
    Compute (..),

    -- ** ConnectionAliasState
    ConnectionAliasState (..),

    -- ** ConnectionState
    ConnectionState (..),

    -- ** DedicatedTenancyModificationStateEnum
    DedicatedTenancyModificationStateEnum (..),

    -- ** DedicatedTenancySupportEnum
    DedicatedTenancySupportEnum (..),

    -- ** DedicatedTenancySupportResultEnum
    DedicatedTenancySupportResultEnum (..),

    -- ** ImageType
    ImageType (..),

    -- ** ModificationResourceEnum
    ModificationResourceEnum (..),

    -- ** ModificationStateEnum
    ModificationStateEnum (..),

    -- ** OperatingSystemType
    OperatingSystemType (..),

    -- ** ReconnectEnum
    ReconnectEnum (..),

    -- ** RunningMode
    RunningMode (..),

    -- ** TargetWorkspaceState
    TargetWorkspaceState (..),

    -- ** Tenancy
    Tenancy (..),

    -- ** WorkspaceDirectoryState
    WorkspaceDirectoryState (..),

    -- ** WorkspaceDirectoryType
    WorkspaceDirectoryType (..),

    -- ** WorkspaceImageIngestionProcess
    WorkspaceImageIngestionProcess (..),

    -- ** WorkspaceImageRequiredTenancy
    WorkspaceImageRequiredTenancy (..),

    -- ** WorkspaceImageState
    WorkspaceImageState (..),

    -- ** WorkspaceState
    WorkspaceState (..),

    -- ** AccountModification
    AccountModification (..),
    mkAccountModification,
    amStartTime,
    amDedicatedTenancySupport,
    amModificationState,
    amDedicatedTenancyManagementCidrRange,
    amErrorCode,
    amErrorMessage,

    -- ** ClientProperties
    ClientProperties (..),
    mkClientProperties,
    cpReconnectEnabled,

    -- ** ClientPropertiesResult
    ClientPropertiesResult (..),
    mkClientPropertiesResult,
    cprResourceId,
    cprClientProperties,

    -- ** ComputeType
    ComputeType (..),
    mkComputeType,
    ctName,

    -- ** ConnectionAlias
    ConnectionAlias (..),
    mkConnectionAlias,
    caState,
    caOwnerAccountId,
    caAliasId,
    caAssociations,
    caConnectionString,

    -- ** ConnectionAliasAssociation
    ConnectionAliasAssociation (..),
    mkConnectionAliasAssociation,
    caaAssociatedAccountId,
    caaResourceId,
    caaAssociationStatus,
    caaConnectionIdentifier,

    -- ** ConnectionAliasPermission
    ConnectionAliasPermission (..),
    mkConnectionAliasPermission,
    capSharedAccountId,
    capAllowAssociation,

    -- ** DefaultWorkspaceCreationProperties
    DefaultWorkspaceCreationProperties (..),
    mkDefaultWorkspaceCreationProperties,
    dwcpCustomSecurityGroupId,
    dwcpUserEnabledAsLocalAdministrator,
    dwcpEnableWorkDocs,
    dwcpEnableMaintenanceMode,
    dwcpEnableInternetAccess,
    dwcpDefaultOu,

    -- ** FailedCreateWorkspaceRequest
    FailedCreateWorkspaceRequest (..),
    mkFailedCreateWorkspaceRequest,
    fcwrWorkspaceRequest,
    fcwrErrorCode,
    fcwrErrorMessage,

    -- ** FailedWorkspaceChangeRequest
    FailedWorkspaceChangeRequest (..),
    mkFailedWorkspaceChangeRequest,
    fwcrErrorCode,
    fwcrWorkspaceId,
    fwcrErrorMessage,

    -- ** IPRuleItem
    IPRuleItem (..),
    mkIPRuleItem,
    iriRuleDesc,
    iriIpRule,

    -- ** ImagePermission
    ImagePermission (..),
    mkImagePermission,
    ipSharedAccountId,

    -- ** ModificationState
    ModificationState (..),
    mkModificationState,
    msState,
    msResource,

    -- ** OperatingSystem
    OperatingSystem (..),
    mkOperatingSystem,
    osType,

    -- ** RebootRequest
    RebootRequest (..),
    mkRebootRequest,
    rWorkspaceId,

    -- ** RebuildRequest
    RebuildRequest (..),
    mkRebuildRequest,
    rrWorkspaceId,

    -- ** RootStorage
    RootStorage (..),
    mkRootStorage,
    rsCapacity,

    -- ** SelfservicePermissions
    SelfservicePermissions (..),
    mkSelfservicePermissions,
    spRestartWorkspace,
    spChangeComputeType,
    spSwitchRunningMode,
    spRebuildWorkspace,
    spIncreaseVolumeSize,

    -- ** Snapshot
    Snapshot (..),
    mkSnapshot,
    sSnapshotTime,

    -- ** StartRequest
    StartRequest (..),
    mkStartRequest,
    sWorkspaceId,

    -- ** StopRequest
    StopRequest (..),
    mkStopRequest,
    srWorkspaceId,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** TerminateRequest
    TerminateRequest (..),
    mkTerminateRequest,
    trWorkspaceId,

    -- ** UserStorage
    UserStorage (..),
    mkUserStorage,
    usCapacity,

    -- ** Workspace
    Workspace (..),
    mkWorkspace,
    wDirectoryId,
    wState,
    wIPAddress,
    wModificationStates,
    wUserName,
    wSubnetId,
    wBundleId,
    wWorkspaceProperties,
    wRootVolumeEncryptionEnabled,
    wErrorCode,
    wVolumeEncryptionKey,
    wComputerName,
    wWorkspaceId,
    wUserVolumeEncryptionEnabled,
    wErrorMessage,

    -- ** WorkspaceAccessProperties
    WorkspaceAccessProperties (..),
    mkWorkspaceAccessProperties,
    wapDeviceTypeWindows,
    wapDeviceTypeWeb,
    wapDeviceTypeAndroid,
    wapDeviceTypeOSx,
    wapDeviceTypeChromeOS,
    wapDeviceTypeIOS,
    wapDeviceTypeZeroClient,

    -- ** WorkspaceBundle
    WorkspaceBundle (..),
    mkWorkspaceBundle,
    wbLastUpdatedTime,
    wbBundleId,
    wbOwner,
    wbRootStorage,
    wbName,
    wbImageId,
    wbComputeType,
    wbUserStorage,
    wbDescription,

    -- ** WorkspaceConnectionStatus
    WorkspaceConnectionStatus (..),
    mkWorkspaceConnectionStatus,
    wcsLastKnownUserConnectionTimestamp,
    wcsConnectionStateCheckTimestamp,
    wcsWorkspaceId,
    wcsConnectionState,

    -- ** WorkspaceCreationProperties
    WorkspaceCreationProperties (..),
    mkWorkspaceCreationProperties,
    wcpCustomSecurityGroupId,
    wcpUserEnabledAsLocalAdministrator,
    wcpEnableWorkDocs,
    wcpEnableMaintenanceMode,
    wcpEnableInternetAccess,
    wcpDefaultOu,

    -- ** WorkspaceDirectory
    WorkspaceDirectory (..),
    mkWorkspaceDirectory,
    wdRegistrationCode,
    wdIAMRoleId,
    wdDirectoryId,
    wdState,
    wdCustomerUserName,
    wdSubnetIds,
    wdIpGroupIds,
    wdAlias,
    wdWorkspaceSecurityGroupId,
    wdDirectoryType,
    wdTenancy,
    wdWorkspaceCreationProperties,
    wdDNSIPAddresses,
    wdWorkspaceAccessProperties,
    wdDirectoryName,
    wdSelfservicePermissions,

    -- ** WorkspaceImage
    WorkspaceImage (..),
    mkWorkspaceImage,
    wiState,
    wiOwnerAccountId,
    wiOperatingSystem,
    wiCreated,
    wiRequiredTenancy,
    wiName,
    wiImageId,
    wiErrorCode,
    wiErrorMessage,
    wiDescription,

    -- ** WorkspaceProperties
    WorkspaceProperties (..),
    mkWorkspaceProperties,
    wpComputeTypeName,
    wpRunningMode,
    wpRootVolumeSizeGib,
    wpRunningModeAutoStopTimeoutInMinutes,
    wpUserVolumeSizeGib,

    -- ** WorkspaceRequest
    WorkspaceRequest (..),
    mkWorkspaceRequest,
    wrWorkspaceProperties,
    wrRootVolumeEncryptionEnabled,
    wrVolumeEncryptionKey,
    wrUserVolumeEncryptionEnabled,
    wrTags,
    wrDirectoryId,
    wrUserName,
    wrBundleId,

    -- ** WorkspacesIPGroup
    WorkspacesIPGroup (..),
    mkWorkspacesIPGroup,
    wigGroupDesc,
    wigUserRules,
    wigGroupId,
    wigGroupName,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.AssociateConnectionAlias
import Network.AWS.WorkSpaces.AssociateIPGroups
import Network.AWS.WorkSpaces.AuthorizeIPRules
import Network.AWS.WorkSpaces.CopyWorkspaceImage
import Network.AWS.WorkSpaces.CreateConnectionAlias
import Network.AWS.WorkSpaces.CreateIPGroup
import Network.AWS.WorkSpaces.CreateTags
import Network.AWS.WorkSpaces.CreateWorkspaces
import Network.AWS.WorkSpaces.DeleteConnectionAlias
import Network.AWS.WorkSpaces.DeleteIPGroup
import Network.AWS.WorkSpaces.DeleteTags
import Network.AWS.WorkSpaces.DeleteWorkspaceImage
import Network.AWS.WorkSpaces.DeregisterWorkspaceDirectory
import Network.AWS.WorkSpaces.DescribeAccount
import Network.AWS.WorkSpaces.DescribeAccountModifications
import Network.AWS.WorkSpaces.DescribeClientProperties
import Network.AWS.WorkSpaces.DescribeConnectionAliasPermissions
import Network.AWS.WorkSpaces.DescribeConnectionAliases
import Network.AWS.WorkSpaces.DescribeIPGroups
import Network.AWS.WorkSpaces.DescribeTags
import Network.AWS.WorkSpaces.DescribeWorkspaceBundles
import Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
import Network.AWS.WorkSpaces.DescribeWorkspaceImagePermissions
import Network.AWS.WorkSpaces.DescribeWorkspaceImages
import Network.AWS.WorkSpaces.DescribeWorkspaceSnapshots
import Network.AWS.WorkSpaces.DescribeWorkspaces
import Network.AWS.WorkSpaces.DescribeWorkspacesConnectionStatus
import Network.AWS.WorkSpaces.DisassociateConnectionAlias
import Network.AWS.WorkSpaces.DisassociateIPGroups
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
import Network.AWS.WorkSpaces.RevokeIPRules
import Network.AWS.WorkSpaces.StartWorkspaces
import Network.AWS.WorkSpaces.StopWorkspaces
import Network.AWS.WorkSpaces.TerminateWorkspaces
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.UpdateConnectionAliasPermission
import Network.AWS.WorkSpaces.UpdateRulesOfIPGroup
import Network.AWS.WorkSpaces.UpdateWorkspaceImagePermission
import Network.AWS.WorkSpaces.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'WorkSpaces'.

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
