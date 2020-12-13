-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types
  ( -- * Service configuration
    workSpacesService,

    -- * Errors

    -- * AccessPropertyValue
    AccessPropertyValue (..),

    -- * Application
    Application (..),

    -- * AssociationStatus
    AssociationStatus (..),

    -- * Compute
    Compute (..),

    -- * ConnectionAliasState
    ConnectionAliasState (..),

    -- * ConnectionState
    ConnectionState (..),

    -- * DedicatedTenancyModificationStateEnum
    DedicatedTenancyModificationStateEnum (..),

    -- * DedicatedTenancySupportEnum
    DedicatedTenancySupportEnum (..),

    -- * DedicatedTenancySupportResultEnum
    DedicatedTenancySupportResultEnum (..),

    -- * ImageType
    ImageType (..),

    -- * ModificationResourceEnum
    ModificationResourceEnum (..),

    -- * ModificationStateEnum
    ModificationStateEnum (..),

    -- * OperatingSystemType
    OperatingSystemType (..),

    -- * ReconnectEnum
    ReconnectEnum (..),

    -- * RunningMode
    RunningMode (..),

    -- * TargetWorkspaceState
    TargetWorkspaceState (..),

    -- * Tenancy
    Tenancy (..),

    -- * WorkspaceDirectoryState
    WorkspaceDirectoryState (..),

    -- * WorkspaceDirectoryType
    WorkspaceDirectoryType (..),

    -- * WorkspaceImageIngestionProcess
    WorkspaceImageIngestionProcess (..),

    -- * WorkspaceImageRequiredTenancy
    WorkspaceImageRequiredTenancy (..),

    -- * WorkspaceImageState
    WorkspaceImageState (..),

    -- * WorkspaceState
    WorkspaceState (..),

    -- * AccountModification
    AccountModification (..),
    mkAccountModification,
    amStartTime,
    amDedicatedTenancySupport,
    amModificationState,
    amDedicatedTenancyManagementCidrRange,
    amErrorCode,
    amErrorMessage,

    -- * ClientProperties
    ClientProperties (..),
    mkClientProperties,
    cpReconnectEnabled,

    -- * ClientPropertiesResult
    ClientPropertiesResult (..),
    mkClientPropertiesResult,
    cprResourceId,
    cprClientProperties,

    -- * ComputeType
    ComputeType (..),
    mkComputeType,
    ctName,

    -- * ConnectionAlias
    ConnectionAlias (..),
    mkConnectionAlias,
    caState,
    caOwnerAccountId,
    caAliasId,
    caAssociations,
    caConnectionString,

    -- * ConnectionAliasAssociation
    ConnectionAliasAssociation (..),
    mkConnectionAliasAssociation,
    caaAssociatedAccountId,
    caaResourceId,
    caaAssociationStatus,
    caaConnectionIdentifier,

    -- * ConnectionAliasPermission
    ConnectionAliasPermission (..),
    mkConnectionAliasPermission,
    capSharedAccountId,
    capAllowAssociation,

    -- * DefaultWorkspaceCreationProperties
    DefaultWorkspaceCreationProperties (..),
    mkDefaultWorkspaceCreationProperties,
    dwcpCustomSecurityGroupId,
    dwcpUserEnabledAsLocalAdministrator,
    dwcpEnableWorkDocs,
    dwcpEnableMaintenanceMode,
    dwcpEnableInternetAccess,
    dwcpDefaultOu,

    -- * FailedCreateWorkspaceRequest
    FailedCreateWorkspaceRequest (..),
    mkFailedCreateWorkspaceRequest,
    fcwrWorkspaceRequest,
    fcwrErrorCode,
    fcwrErrorMessage,

    -- * FailedWorkspaceChangeRequest
    FailedWorkspaceChangeRequest (..),
    mkFailedWorkspaceChangeRequest,
    fwcrErrorCode,
    fwcrWorkspaceId,
    fwcrErrorMessage,

    -- * IPRuleItem
    IPRuleItem (..),
    mkIPRuleItem,
    iriRuleDesc,
    iriIpRule,

    -- * ImagePermission
    ImagePermission (..),
    mkImagePermission,
    ipSharedAccountId,

    -- * ModificationState
    ModificationState (..),
    mkModificationState,
    msState,
    msResource,

    -- * OperatingSystem
    OperatingSystem (..),
    mkOperatingSystem,
    osType,

    -- * RebootRequest
    RebootRequest (..),
    mkRebootRequest,
    rWorkspaceId,

    -- * RebuildRequest
    RebuildRequest (..),
    mkRebuildRequest,
    rrWorkspaceId,

    -- * RootStorage
    RootStorage (..),
    mkRootStorage,
    rsCapacity,

    -- * SelfservicePermissions
    SelfservicePermissions (..),
    mkSelfservicePermissions,
    spRestartWorkspace,
    spChangeComputeType,
    spSwitchRunningMode,
    spRebuildWorkspace,
    spIncreaseVolumeSize,

    -- * Snapshot
    Snapshot (..),
    mkSnapshot,
    sSnapshotTime,

    -- * StartRequest
    StartRequest (..),
    mkStartRequest,
    sWorkspaceId,

    -- * StopRequest
    StopRequest (..),
    mkStopRequest,
    srWorkspaceId,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * TerminateRequest
    TerminateRequest (..),
    mkTerminateRequest,
    trWorkspaceId,

    -- * UserStorage
    UserStorage (..),
    mkUserStorage,
    usCapacity,

    -- * Workspace
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

    -- * WorkspaceAccessProperties
    WorkspaceAccessProperties (..),
    mkWorkspaceAccessProperties,
    wapDeviceTypeWindows,
    wapDeviceTypeWeb,
    wapDeviceTypeAndroid,
    wapDeviceTypeOSx,
    wapDeviceTypeChromeOS,
    wapDeviceTypeIOS,
    wapDeviceTypeZeroClient,

    -- * WorkspaceBundle
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

    -- * WorkspaceConnectionStatus
    WorkspaceConnectionStatus (..),
    mkWorkspaceConnectionStatus,
    wcsLastKnownUserConnectionTimestamp,
    wcsConnectionStateCheckTimestamp,
    wcsWorkspaceId,
    wcsConnectionState,

    -- * WorkspaceCreationProperties
    WorkspaceCreationProperties (..),
    mkWorkspaceCreationProperties,
    wcpCustomSecurityGroupId,
    wcpUserEnabledAsLocalAdministrator,
    wcpEnableWorkDocs,
    wcpEnableMaintenanceMode,
    wcpEnableInternetAccess,
    wcpDefaultOu,

    -- * WorkspaceDirectory
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

    -- * WorkspaceImage
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

    -- * WorkspaceProperties
    WorkspaceProperties (..),
    mkWorkspaceProperties,
    wpComputeTypeName,
    wpRunningMode,
    wpRootVolumeSizeGib,
    wpRunningModeAutoStopTimeoutInMinutes,
    wpUserVolumeSizeGib,

    -- * WorkspaceRequest
    WorkspaceRequest (..),
    mkWorkspaceRequest,
    wrDirectoryId,
    wrUserName,
    wrBundleId,
    wrWorkspaceProperties,
    wrRootVolumeEncryptionEnabled,
    wrVolumeEncryptionKey,
    wrUserVolumeEncryptionEnabled,
    wrTags,

    -- * WorkspacesIPGroup
    WorkspacesIPGroup (..),
    mkWorkspacesIPGroup,
    wigGroupDesc,
    wigUserRules,
    wigGroupId,
    wigGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.WorkSpaces.Types.AccessPropertyValue
import Network.AWS.WorkSpaces.Types.AccountModification
import Network.AWS.WorkSpaces.Types.Application
import Network.AWS.WorkSpaces.Types.AssociationStatus
import Network.AWS.WorkSpaces.Types.ClientProperties
import Network.AWS.WorkSpaces.Types.ClientPropertiesResult
import Network.AWS.WorkSpaces.Types.Compute
import Network.AWS.WorkSpaces.Types.ComputeType
import Network.AWS.WorkSpaces.Types.ConnectionAlias
import Network.AWS.WorkSpaces.Types.ConnectionAliasAssociation
import Network.AWS.WorkSpaces.Types.ConnectionAliasPermission
import Network.AWS.WorkSpaces.Types.ConnectionAliasState
import Network.AWS.WorkSpaces.Types.ConnectionState
import Network.AWS.WorkSpaces.Types.DedicatedTenancyModificationStateEnum
import Network.AWS.WorkSpaces.Types.DedicatedTenancySupportEnum
import Network.AWS.WorkSpaces.Types.DedicatedTenancySupportResultEnum
import Network.AWS.WorkSpaces.Types.DefaultWorkspaceCreationProperties
import Network.AWS.WorkSpaces.Types.FailedCreateWorkspaceRequest
import Network.AWS.WorkSpaces.Types.FailedWorkspaceChangeRequest
import Network.AWS.WorkSpaces.Types.IPRuleItem
import Network.AWS.WorkSpaces.Types.ImagePermission
import Network.AWS.WorkSpaces.Types.ImageType
import Network.AWS.WorkSpaces.Types.ModificationResourceEnum
import Network.AWS.WorkSpaces.Types.ModificationState
import Network.AWS.WorkSpaces.Types.ModificationStateEnum
import Network.AWS.WorkSpaces.Types.OperatingSystem
import Network.AWS.WorkSpaces.Types.OperatingSystemType
import Network.AWS.WorkSpaces.Types.RebootRequest
import Network.AWS.WorkSpaces.Types.RebuildRequest
import Network.AWS.WorkSpaces.Types.ReconnectEnum
import Network.AWS.WorkSpaces.Types.RootStorage
import Network.AWS.WorkSpaces.Types.RunningMode
import Network.AWS.WorkSpaces.Types.SelfservicePermissions
import Network.AWS.WorkSpaces.Types.Snapshot
import Network.AWS.WorkSpaces.Types.StartRequest
import Network.AWS.WorkSpaces.Types.StopRequest
import Network.AWS.WorkSpaces.Types.Tag
import Network.AWS.WorkSpaces.Types.TargetWorkspaceState
import Network.AWS.WorkSpaces.Types.Tenancy
import Network.AWS.WorkSpaces.Types.TerminateRequest
import Network.AWS.WorkSpaces.Types.UserStorage
import Network.AWS.WorkSpaces.Types.Workspace
import Network.AWS.WorkSpaces.Types.WorkspaceAccessProperties
import Network.AWS.WorkSpaces.Types.WorkspaceBundle
import Network.AWS.WorkSpaces.Types.WorkspaceConnectionStatus
import Network.AWS.WorkSpaces.Types.WorkspaceCreationProperties
import Network.AWS.WorkSpaces.Types.WorkspaceDirectory
import Network.AWS.WorkSpaces.Types.WorkspaceDirectoryState
import Network.AWS.WorkSpaces.Types.WorkspaceDirectoryType
import Network.AWS.WorkSpaces.Types.WorkspaceImage
import Network.AWS.WorkSpaces.Types.WorkspaceImageIngestionProcess
import Network.AWS.WorkSpaces.Types.WorkspaceImageRequiredTenancy
import Network.AWS.WorkSpaces.Types.WorkspaceImageState
import Network.AWS.WorkSpaces.Types.WorkspaceProperties
import Network.AWS.WorkSpaces.Types.WorkspaceRequest
import Network.AWS.WorkSpaces.Types.WorkspaceState
import Network.AWS.WorkSpaces.Types.WorkspacesIPGroup

-- | API version @2015-04-08@ of the Amazon WorkSpaces SDK configuration.
workSpacesService :: Lude.Service
workSpacesService =
  Lude.Service
    { Lude._svcAbbrev = "WorkSpaces",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "workspaces",
      Lude._svcVersion = "2015-04-08",
      Lude._svcEndpoint = Lude.defaultEndpoint workSpacesService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "WorkSpaces",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
