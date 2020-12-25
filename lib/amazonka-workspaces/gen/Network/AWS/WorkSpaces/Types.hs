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
    mkServiceConfig,

    -- * Errors
    _UnsupportedNetworkConfigurationException,
    _AccessDeniedException,
    _ResourceCreationFailedException,
    _ResourceUnavailableException,
    _InvalidParameterValuesException,
    _ResourceAssociatedException,
    _OperationInProgressException,
    _ResourceAlreadyExistsException,
    _ResourceLimitExceededException,
    _InvalidResourceStateException,
    _OperationNotSupportedException,
    _UnsupportedWorkspaceConfigurationException,
    _WorkspacesDefaultRoleNotFoundException,
    _ResourceNotFoundException,

    -- * WorkspaceRequest
    WorkspaceRequest (..),
    mkWorkspaceRequest,
    wrDirectoryId,
    wrUserName,
    wrBundleId,
    wrRootVolumeEncryptionEnabled,
    wrTags,
    wrUserVolumeEncryptionEnabled,
    wrVolumeEncryptionKey,
    wrWorkspaceProperties,

    -- * WorkspaceDirectory
    WorkspaceDirectory (..),
    mkWorkspaceDirectory,
    wdAlias,
    wdCustomerUserName,
    wdDirectoryId,
    wdDirectoryName,
    wdDirectoryType,
    wdDnsIpAddresses,
    wdIamRoleId,
    wdRegistrationCode,
    wdSelfservicePermissions,
    wdState,
    wdSubnetIds,
    wdTenancy,
    wdWorkspaceAccessProperties,
    wdWorkspaceCreationProperties,
    wdWorkspaceSecurityGroupId,
    wdIpGroupIds,

    -- * RegistrationCode
    RegistrationCode (..),

    -- * DedicatedTenancySupportResultEnum
    DedicatedTenancySupportResultEnum (..),

    -- * Snapshot
    Snapshot (..),
    mkSnapshot,
    sSnapshotTime,

    -- * DirectoryId
    DirectoryId (..),

    -- * WorkspaceImageErrorCode
    WorkspaceImageErrorCode (..),

    -- * PaginationToken
    PaginationToken (..),

    -- * ModificationResourceEnum
    ModificationResourceEnum (..),

    -- * WorkspaceImageId
    WorkspaceImageId (..),

    -- * IpGroupDesc
    IpGroupDesc (..),

    -- * AccountModification
    AccountModification (..),
    mkAccountModification,
    amDedicatedTenancyManagementCidrRange,
    amDedicatedTenancySupport,
    amErrorCode,
    amErrorMessage,
    amModificationState,
    amStartTime,

    -- * IpAddress
    IpAddress (..),

    -- * ConnectionAliasState
    ConnectionAliasState (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * Application
    Application (..),

    -- * IpRuleItem
    IpRuleItem (..),
    mkIpRuleItem,
    iriIpRule,
    iriRuleDesc,

    -- * DedicatedTenancySupportEnum
    DedicatedTenancySupportEnum (..),

    -- * ManagementCidrRangeConstraint
    ManagementCidrRangeConstraint (..),

    -- * Compute
    Compute (..),

    -- * Workspace
    Workspace (..),
    mkWorkspace,
    wBundleId,
    wComputerName,
    wDirectoryId,
    wErrorCode,
    wErrorMessage,
    wIpAddress,
    wModificationStates,
    wRootVolumeEncryptionEnabled,
    wState,
    wSubnetId,
    wUserName,
    wUserVolumeEncryptionEnabled,
    wVolumeEncryptionKey,
    wWorkspaceId,
    wWorkspaceProperties,

    -- * WorkspaceImageName
    WorkspaceImageName (..),

    -- * OperatingSystem
    OperatingSystem (..),
    mkOperatingSystem,
    osType,

    -- * ConnectionAlias
    ConnectionAlias (..),
    mkConnectionAlias,
    caAliasId,
    caAssociations,
    caConnectionString,
    caOwnerAccountId,
    caState,

    -- * ClientProperties
    ClientProperties (..),
    mkClientProperties,
    cpReconnectEnabled,

    -- * RebuildRequest
    RebuildRequest (..),
    mkRebuildRequest,
    rrWorkspaceId,

    -- * FailedCreateWorkspaceRequest
    FailedCreateWorkspaceRequest (..),
    mkFailedCreateWorkspaceRequest,
    fcwrErrorCode,
    fcwrErrorMessage,
    fcwrWorkspaceRequest,

    -- * WorkspaceDirectoryState
    WorkspaceDirectoryState (..),

    -- * IpGroupId
    IpGroupId (..),

    -- * TargetWorkspaceState
    TargetWorkspaceState (..),

    -- * DedicatedTenancyModificationStateEnum
    DedicatedTenancyModificationStateEnum (..),

    -- * Ec2ImageId
    Ec2ImageId (..),

    -- * Alias
    Alias (..),

    -- * UserName
    UserName (..),

    -- * SubnetId
    SubnetId (..),

    -- * FailedWorkspaceChangeRequest
    FailedWorkspaceChangeRequest (..),
    mkFailedWorkspaceChangeRequest,
    fwcrErrorCode,
    fwcrErrorMessage,
    fwcrWorkspaceId,

    -- * ModificationState
    ModificationState (..),
    mkModificationState,
    msResource,
    msState,

    -- * IpGroupName
    IpGroupName (..),

    -- * ClientPropertiesResult
    ClientPropertiesResult (..),
    mkClientPropertiesResult,
    cprClientProperties,
    cprResourceId,

    -- * BundleId
    BundleId (..),

    -- * DedicatedTenancyManagementCidrRange
    DedicatedTenancyManagementCidrRange (..),

    -- * StopRequest
    StopRequest (..),
    mkStopRequest,
    srWorkspaceId,

    -- * SecurityGroupId
    SecurityGroupId (..),

    -- * WorkspaceImageDescription
    WorkspaceImageDescription (..),

    -- * AwsAccount
    AwsAccount (..),

    -- * WorkspaceProperties
    WorkspaceProperties (..),
    mkWorkspaceProperties,
    wpComputeTypeName,
    wpRootVolumeSizeGib,
    wpRunningMode,
    wpRunningModeAutoStopTimeoutInMinutes,
    wpUserVolumeSizeGib,

    -- * RunningMode
    RunningMode (..),

    -- * ImagePermission
    ImagePermission (..),
    mkImagePermission,
    ipSharedAccountId,

    -- * WorkspaceDirectoryType
    WorkspaceDirectoryType (..),

    -- * NonEmptyString
    NonEmptyString (..),

    -- * RootStorage
    RootStorage (..),
    mkRootStorage,
    rsCapacity,

    -- * ImageType
    ImageType (..),

    -- * IpRule
    IpRule (..),

    -- * WorkspaceConnectionStatus
    WorkspaceConnectionStatus (..),
    mkWorkspaceConnectionStatus,
    wcsConnectionState,
    wcsConnectionStateCheckTimestamp,
    wcsLastKnownUserConnectionTimestamp,
    wcsWorkspaceId,

    -- * WorkspaceState
    WorkspaceState (..),

    -- * Tenancy
    Tenancy (..),

    -- * ModificationStateEnum
    ModificationStateEnum (..),

    -- * ReconnectEnum
    ReconnectEnum (..),

    -- * WorkspaceImage
    WorkspaceImage (..),
    mkWorkspaceImage,
    wiCreated,
    wiDescription,
    wiErrorCode,
    wiErrorMessage,
    wiImageId,
    wiName,
    wiOperatingSystem,
    wiOwnerAccountId,
    wiRequiredTenancy,
    wiState,

    -- * BundleOwner
    BundleOwner (..),

    -- * AssociationStatus
    AssociationStatus (..),

    -- * WorkspaceCreationProperties
    WorkspaceCreationProperties (..),
    mkWorkspaceCreationProperties,
    wcpCustomSecurityGroupId,
    wcpDefaultOu,
    wcpEnableInternetAccess,
    wcpEnableMaintenanceMode,
    wcpEnableWorkDocs,
    wcpUserEnabledAsLocalAdministrator,

    -- * ComputeType
    ComputeType (..),
    mkComputeType,
    ctName,

    -- * ConnectionIdentifier
    ConnectionIdentifier (..),

    -- * ConnectionAliasId
    ConnectionAliasId (..),

    -- * WorkspaceImageState
    WorkspaceImageState (..),

    -- * VolumeEncryptionKey
    VolumeEncryptionKey (..),

    -- * RebootRequest
    RebootRequest (..),
    mkRebootRequest,
    rWorkspaceId,

    -- * UserStorage
    UserStorage (..),
    mkUserStorage,
    usCapacity,

    -- * AccessPropertyValue
    AccessPropertyValue (..),

    -- * ComputerName
    ComputerName (..),

    -- * OperatingSystemType
    OperatingSystemType (..),

    -- * WorkspacesIpGroup
    WorkspacesIpGroup (..),
    mkWorkspacesIpGroup,
    wigGroupDesc,
    wigGroupId,
    wigGroupName,
    wigUserRules,

    -- * WorkspaceAccessProperties
    WorkspaceAccessProperties (..),
    mkWorkspaceAccessProperties,
    wapDeviceTypeAndroid,
    wapDeviceTypeChromeOs,
    wapDeviceTypeIos,
    wapDeviceTypeOsx,
    wapDeviceTypeWeb,
    wapDeviceTypeWindows,
    wapDeviceTypeZeroClient,

    -- * TerminateRequest
    TerminateRequest (..),
    mkTerminateRequest,
    trWorkspaceId,

    -- * WorkspaceId
    WorkspaceId (..),

    -- * WorkspaceImageIngestionProcess
    WorkspaceImageIngestionProcess (..),

    -- * ConnectionAliasPermission
    ConnectionAliasPermission (..),
    mkConnectionAliasPermission,
    capSharedAccountId,
    capAllowAssociation,

    -- * ConnectionString
    ConnectionString (..),

    -- * Description
    Description (..),

    -- * DefaultWorkspaceCreationProperties
    DefaultWorkspaceCreationProperties (..),
    mkDefaultWorkspaceCreationProperties,
    dwcpCustomSecurityGroupId,
    dwcpDefaultOu,
    dwcpEnableInternetAccess,
    dwcpEnableMaintenanceMode,
    dwcpEnableWorkDocs,
    dwcpUserEnabledAsLocalAdministrator,

    -- * DirectoryName
    DirectoryName (..),

    -- * ConnectionAliasAssociation
    ConnectionAliasAssociation (..),
    mkConnectionAliasAssociation,
    caaAssociatedAccountId,
    caaAssociationStatus,
    caaConnectionIdentifier,
    caaResourceId,

    -- * DefaultOu
    DefaultOu (..),

    -- * ConnectionState
    ConnectionState (..),

    -- * SelfservicePermissions
    SelfservicePermissions (..),
    mkSelfservicePermissions,
    spChangeComputeType,
    spIncreaseVolumeSize,
    spRebuildWorkspace,
    spRestartWorkspace,
    spSwitchRunningMode,

    -- * WorkspaceBundle
    WorkspaceBundle (..),
    mkWorkspaceBundle,
    wbBundleId,
    wbComputeType,
    wbDescription,
    wbImageId,
    wbLastUpdatedTime,
    wbName,
    wbOwner,
    wbRootStorage,
    wbUserStorage,

    -- * WorkspaceImageRequiredTenancy
    WorkspaceImageRequiredTenancy (..),

    -- * StartRequest
    StartRequest (..),
    mkStartRequest,
    sWorkspaceId,

    -- * CustomerUserName
    CustomerUserName (..),

    -- * IamRoleId
    IamRoleId (..),

    -- * WorkspaceSecurityGroupId
    WorkspaceSecurityGroupId (..),

    -- * AliasId
    AliasId (..),

    -- * NextToken
    NextToken (..),

    -- * ResourceId
    ResourceId (..),

    -- * ErrorCode
    ErrorCode (..),

    -- * ErrorMessage
    ErrorMessage (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * GroupId
    GroupId (..),

    -- * RuleDesc
    RuleDesc (..),

    -- * SourceWorkspaceId
    SourceWorkspaceId (..),

    -- * TargetWorkspaceId
    TargetWorkspaceId (..),

    -- * OwnerAccountId
    OwnerAccountId (..),

    -- * SourceRegion
    SourceRegion (..),

    -- * SharedAccountId
    SharedAccountId (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.WorkSpaces.Types.AccessPropertyValue
import Network.AWS.WorkSpaces.Types.AccountModification
import Network.AWS.WorkSpaces.Types.Alias
import Network.AWS.WorkSpaces.Types.AliasId
import Network.AWS.WorkSpaces.Types.Application
import Network.AWS.WorkSpaces.Types.AssociationStatus
import Network.AWS.WorkSpaces.Types.AwsAccount
import Network.AWS.WorkSpaces.Types.BundleId
import Network.AWS.WorkSpaces.Types.BundleOwner
import Network.AWS.WorkSpaces.Types.ClientProperties
import Network.AWS.WorkSpaces.Types.ClientPropertiesResult
import Network.AWS.WorkSpaces.Types.Compute
import Network.AWS.WorkSpaces.Types.ComputeType
import Network.AWS.WorkSpaces.Types.ComputerName
import Network.AWS.WorkSpaces.Types.ConnectionAlias
import Network.AWS.WorkSpaces.Types.ConnectionAliasAssociation
import Network.AWS.WorkSpaces.Types.ConnectionAliasId
import Network.AWS.WorkSpaces.Types.ConnectionAliasPermission
import Network.AWS.WorkSpaces.Types.ConnectionAliasState
import Network.AWS.WorkSpaces.Types.ConnectionIdentifier
import Network.AWS.WorkSpaces.Types.ConnectionState
import Network.AWS.WorkSpaces.Types.ConnectionString
import Network.AWS.WorkSpaces.Types.CustomerUserName
import Network.AWS.WorkSpaces.Types.DedicatedTenancyManagementCidrRange
import Network.AWS.WorkSpaces.Types.DedicatedTenancyModificationStateEnum
import Network.AWS.WorkSpaces.Types.DedicatedTenancySupportEnum
import Network.AWS.WorkSpaces.Types.DedicatedTenancySupportResultEnum
import Network.AWS.WorkSpaces.Types.DefaultOu
import Network.AWS.WorkSpaces.Types.DefaultWorkspaceCreationProperties
import Network.AWS.WorkSpaces.Types.Description
import Network.AWS.WorkSpaces.Types.DirectoryId
import Network.AWS.WorkSpaces.Types.DirectoryName
import Network.AWS.WorkSpaces.Types.Ec2ImageId
import Network.AWS.WorkSpaces.Types.ErrorCode
import Network.AWS.WorkSpaces.Types.ErrorMessage
import Network.AWS.WorkSpaces.Types.FailedCreateWorkspaceRequest
import Network.AWS.WorkSpaces.Types.FailedWorkspaceChangeRequest
import Network.AWS.WorkSpaces.Types.GroupId
import Network.AWS.WorkSpaces.Types.IamRoleId
import Network.AWS.WorkSpaces.Types.ImagePermission
import Network.AWS.WorkSpaces.Types.ImageType
import Network.AWS.WorkSpaces.Types.IpAddress
import Network.AWS.WorkSpaces.Types.IpGroupDesc
import Network.AWS.WorkSpaces.Types.IpGroupId
import Network.AWS.WorkSpaces.Types.IpGroupName
import Network.AWS.WorkSpaces.Types.IpRule
import Network.AWS.WorkSpaces.Types.IpRuleItem
import Network.AWS.WorkSpaces.Types.Key
import Network.AWS.WorkSpaces.Types.ManagementCidrRangeConstraint
import Network.AWS.WorkSpaces.Types.ModificationResourceEnum
import Network.AWS.WorkSpaces.Types.ModificationState
import Network.AWS.WorkSpaces.Types.ModificationStateEnum
import Network.AWS.WorkSpaces.Types.NextToken
import Network.AWS.WorkSpaces.Types.NonEmptyString
import Network.AWS.WorkSpaces.Types.OperatingSystem
import Network.AWS.WorkSpaces.Types.OperatingSystemType
import Network.AWS.WorkSpaces.Types.OwnerAccountId
import Network.AWS.WorkSpaces.Types.PaginationToken
import Network.AWS.WorkSpaces.Types.RebootRequest
import Network.AWS.WorkSpaces.Types.RebuildRequest
import Network.AWS.WorkSpaces.Types.ReconnectEnum
import Network.AWS.WorkSpaces.Types.RegistrationCode
import Network.AWS.WorkSpaces.Types.ResourceId
import Network.AWS.WorkSpaces.Types.RootStorage
import Network.AWS.WorkSpaces.Types.RuleDesc
import Network.AWS.WorkSpaces.Types.RunningMode
import Network.AWS.WorkSpaces.Types.SecurityGroupId
import Network.AWS.WorkSpaces.Types.SelfservicePermissions
import Network.AWS.WorkSpaces.Types.SharedAccountId
import Network.AWS.WorkSpaces.Types.Snapshot
import Network.AWS.WorkSpaces.Types.SourceRegion
import Network.AWS.WorkSpaces.Types.SourceWorkspaceId
import Network.AWS.WorkSpaces.Types.StartRequest
import Network.AWS.WorkSpaces.Types.StopRequest
import Network.AWS.WorkSpaces.Types.SubnetId
import Network.AWS.WorkSpaces.Types.Tag
import Network.AWS.WorkSpaces.Types.TargetWorkspaceId
import Network.AWS.WorkSpaces.Types.TargetWorkspaceState
import Network.AWS.WorkSpaces.Types.Tenancy
import Network.AWS.WorkSpaces.Types.TerminateRequest
import Network.AWS.WorkSpaces.Types.UserName
import Network.AWS.WorkSpaces.Types.UserStorage
import Network.AWS.WorkSpaces.Types.Value
import Network.AWS.WorkSpaces.Types.VolumeEncryptionKey
import Network.AWS.WorkSpaces.Types.Workspace
import Network.AWS.WorkSpaces.Types.WorkspaceAccessProperties
import Network.AWS.WorkSpaces.Types.WorkspaceBundle
import Network.AWS.WorkSpaces.Types.WorkspaceConnectionStatus
import Network.AWS.WorkSpaces.Types.WorkspaceCreationProperties
import Network.AWS.WorkSpaces.Types.WorkspaceDirectory
import Network.AWS.WorkSpaces.Types.WorkspaceDirectoryState
import Network.AWS.WorkSpaces.Types.WorkspaceDirectoryType
import Network.AWS.WorkSpaces.Types.WorkspaceId
import Network.AWS.WorkSpaces.Types.WorkspaceImage
import Network.AWS.WorkSpaces.Types.WorkspaceImageDescription
import Network.AWS.WorkSpaces.Types.WorkspaceImageErrorCode
import Network.AWS.WorkSpaces.Types.WorkspaceImageId
import Network.AWS.WorkSpaces.Types.WorkspaceImageIngestionProcess
import Network.AWS.WorkSpaces.Types.WorkspaceImageName
import Network.AWS.WorkSpaces.Types.WorkspaceImageRequiredTenancy
import Network.AWS.WorkSpaces.Types.WorkspaceImageState
import Network.AWS.WorkSpaces.Types.WorkspaceProperties
import Network.AWS.WorkSpaces.Types.WorkspaceRequest
import Network.AWS.WorkSpaces.Types.WorkspaceSecurityGroupId
import Network.AWS.WorkSpaces.Types.WorkspaceState
import Network.AWS.WorkSpaces.Types.WorkspacesIpGroup

-- | API version @2015-04-08@ of the Amazon WorkSpaces SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "WorkSpaces",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "workspaces",
      Core._svcVersion = "2015-04-08",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "WorkSpaces",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The configuration of this network is not supported for this operation, or your network configuration conflicts with the Amazon WorkSpaces management network IP range. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-vpc.html Configure a VPC for Amazon WorkSpaces> .
_UnsupportedNetworkConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedNetworkConfigurationException =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedNetworkConfigurationException"
{-# DEPRECATED _UnsupportedNetworkConfigurationException "Use generic-lens or generic-optics instead." #-}

-- | The user is not authorized to access a resource.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError mkServiceConfig "AccessDeniedException"
{-# DEPRECATED _AccessDeniedException "Use generic-lens or generic-optics instead." #-}

-- | The resource could not be created.
_ResourceCreationFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceCreationFailedException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceCreationFailedException"
{-# DEPRECATED _ResourceCreationFailedException "Use generic-lens or generic-optics instead." #-}

-- | The specified resource is not available.
_ResourceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceUnavailableException"
{-# DEPRECATED _ResourceUnavailableException "Use generic-lens or generic-optics instead." #-}

-- | One or more parameter values are not valid.
_InvalidParameterValuesException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValuesException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidParameterValuesException"
{-# DEPRECATED _InvalidParameterValuesException "Use generic-lens or generic-optics instead." #-}

-- | The resource is associated with a directory.
_ResourceAssociatedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAssociatedException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceAssociatedException"
{-# DEPRECATED _ResourceAssociatedException "Use generic-lens or generic-optics instead." #-}

-- | The properties of this WorkSpace are currently being modified. Try again in a moment.
_OperationInProgressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationInProgressException =
  Core._MatchServiceError
    mkServiceConfig
    "OperationInProgressException"
{-# DEPRECATED _OperationInProgressException "Use generic-lens or generic-optics instead." #-}

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceAlreadyExistsException"
{-# DEPRECATED _ResourceAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | Your resource limits have been exceeded.
_ResourceLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceLimitExceededException"
{-# DEPRECATED _ResourceLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The state of the resource is not valid for this operation.
_InvalidResourceStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidResourceStateException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidResourceStateException"
{-# DEPRECATED _InvalidResourceStateException "Use generic-lens or generic-optics instead." #-}

-- | This operation is not supported.
_OperationNotSupportedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationNotSupportedException =
  Core._MatchServiceError
    mkServiceConfig
    "OperationNotSupportedException"
{-# DEPRECATED _OperationNotSupportedException "Use generic-lens or generic-optics instead." #-}

-- | The configuration of this WorkSpace is not supported for this operation. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/required-service-components.html Required Configuration and Service Components for WorkSpaces > .
_UnsupportedWorkspaceConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedWorkspaceConfigurationException =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedWorkspaceConfigurationException"
{-# DEPRECATED _UnsupportedWorkspaceConfigurationException "Use generic-lens or generic-optics instead." #-}

-- | The workspaces_DefaultRole role could not be found. If this is the first time you are registering a directory, you will need to create the workspaces_DefaultRole role before you can register a directory. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspaces-access-control.html#create-default-role Creating the workspaces_DefaultRole Role> .
_WorkspacesDefaultRoleNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WorkspacesDefaultRoleNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "WorkspacesDefaultRoleNotFoundException"
{-# DEPRECATED _WorkspacesDefaultRoleNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}
