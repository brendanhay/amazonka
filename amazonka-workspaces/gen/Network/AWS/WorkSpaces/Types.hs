{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceUnavailableException,
    _UnsupportedNetworkConfigurationException,
    _OperationNotSupportedException,
    _ResourceLimitExceededException,
    _ResourceAlreadyExistsException,
    _OperationInProgressException,
    _WorkspacesDefaultRoleNotFoundException,
    _ResourceAssociatedException,
    _UnsupportedWorkspaceConfigurationException,
    _InvalidParameterValuesException,
    _ResourceCreationFailedException,
    _AccessDeniedException,
    _ResourceNotFoundException,
    _InvalidResourceStateException,

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
    newAccountModification,
    accountModification_dedicatedTenancySupport,
    accountModification_startTime,
    accountModification_modificationState,
    accountModification_dedicatedTenancyManagementCidrRange,
    accountModification_errorMessage,
    accountModification_errorCode,

    -- * ClientProperties
    ClientProperties (..),
    newClientProperties,
    clientProperties_reconnectEnabled,

    -- * ClientPropertiesResult
    ClientPropertiesResult (..),
    newClientPropertiesResult,
    clientPropertiesResult_resourceId,
    clientPropertiesResult_clientProperties,

    -- * ComputeType
    ComputeType (..),
    newComputeType,
    computeType_name,

    -- * ConnectionAlias
    ConnectionAlias (..),
    newConnectionAlias,
    connectionAlias_state,
    connectionAlias_connectionString,
    connectionAlias_aliasId,
    connectionAlias_ownerAccountId,
    connectionAlias_associations,

    -- * ConnectionAliasAssociation
    ConnectionAliasAssociation (..),
    newConnectionAliasAssociation,
    connectionAliasAssociation_resourceId,
    connectionAliasAssociation_connectionIdentifier,
    connectionAliasAssociation_associatedAccountId,
    connectionAliasAssociation_associationStatus,

    -- * ConnectionAliasPermission
    ConnectionAliasPermission (..),
    newConnectionAliasPermission,
    connectionAliasPermission_sharedAccountId,
    connectionAliasPermission_allowAssociation,

    -- * DefaultWorkspaceCreationProperties
    DefaultWorkspaceCreationProperties (..),
    newDefaultWorkspaceCreationProperties,
    defaultWorkspaceCreationProperties_enableMaintenanceMode,
    defaultWorkspaceCreationProperties_defaultOu,
    defaultWorkspaceCreationProperties_enableInternetAccess,
    defaultWorkspaceCreationProperties_enableWorkDocs,
    defaultWorkspaceCreationProperties_customSecurityGroupId,
    defaultWorkspaceCreationProperties_userEnabledAsLocalAdministrator,

    -- * FailedCreateWorkspaceRequest
    FailedCreateWorkspaceRequest (..),
    newFailedCreateWorkspaceRequest,
    failedCreateWorkspaceRequest_workspaceRequest,
    failedCreateWorkspaceRequest_errorMessage,
    failedCreateWorkspaceRequest_errorCode,

    -- * FailedWorkspaceChangeRequest
    FailedWorkspaceChangeRequest (..),
    newFailedWorkspaceChangeRequest,
    failedWorkspaceChangeRequest_workspaceId,
    failedWorkspaceChangeRequest_errorMessage,
    failedWorkspaceChangeRequest_errorCode,

    -- * ImagePermission
    ImagePermission (..),
    newImagePermission,
    imagePermission_sharedAccountId,

    -- * IpRuleItem
    IpRuleItem (..),
    newIpRuleItem,
    ipRuleItem_ruleDesc,
    ipRuleItem_ipRule,

    -- * ModificationState
    ModificationState (..),
    newModificationState,
    modificationState_state,
    modificationState_resource,

    -- * OperatingSystem
    OperatingSystem (..),
    newOperatingSystem,
    operatingSystem_type,

    -- * RebootRequest
    RebootRequest (..),
    newRebootRequest,
    rebootRequest_workspaceId,

    -- * RebuildRequest
    RebuildRequest (..),
    newRebuildRequest,
    rebuildRequest_workspaceId,

    -- * RootStorage
    RootStorage (..),
    newRootStorage,
    rootStorage_capacity,

    -- * SelfservicePermissions
    SelfservicePermissions (..),
    newSelfservicePermissions,
    selfservicePermissions_switchRunningMode,
    selfservicePermissions_restartWorkspace,
    selfservicePermissions_rebuildWorkspace,
    selfservicePermissions_increaseVolumeSize,
    selfservicePermissions_changeComputeType,

    -- * Snapshot
    Snapshot (..),
    newSnapshot,
    snapshot_snapshotTime,

    -- * StartRequest
    StartRequest (..),
    newStartRequest,
    startRequest_workspaceId,

    -- * StopRequest
    StopRequest (..),
    newStopRequest,
    stopRequest_workspaceId,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TerminateRequest
    TerminateRequest (..),
    newTerminateRequest,
    terminateRequest_workspaceId,

    -- * UserStorage
    UserStorage (..),
    newUserStorage,
    userStorage_capacity,

    -- * Workspace
    Workspace (..),
    newWorkspace,
    workspace_workspaceProperties,
    workspace_rootVolumeEncryptionEnabled,
    workspace_bundleId,
    workspace_userVolumeEncryptionEnabled,
    workspace_volumeEncryptionKey,
    workspace_workspaceId,
    workspace_modificationStates,
    workspace_state,
    workspace_ipAddress,
    workspace_directoryId,
    workspace_userName,
    workspace_subnetId,
    workspace_errorMessage,
    workspace_computerName,
    workspace_errorCode,

    -- * WorkspaceAccessProperties
    WorkspaceAccessProperties (..),
    newWorkspaceAccessProperties,
    workspaceAccessProperties_deviceTypeOsx,
    workspaceAccessProperties_deviceTypeLinux,
    workspaceAccessProperties_deviceTypeWindows,
    workspaceAccessProperties_deviceTypeZeroClient,
    workspaceAccessProperties_deviceTypeAndroid,
    workspaceAccessProperties_deviceTypeWeb,
    workspaceAccessProperties_deviceTypeIos,
    workspaceAccessProperties_deviceTypeChromeOs,

    -- * WorkspaceBundle
    WorkspaceBundle (..),
    newWorkspaceBundle,
    workspaceBundle_rootStorage,
    workspaceBundle_creationTime,
    workspaceBundle_bundleId,
    workspaceBundle_userStorage,
    workspaceBundle_imageId,
    workspaceBundle_name,
    workspaceBundle_owner,
    workspaceBundle_description,
    workspaceBundle_computeType,
    workspaceBundle_lastUpdatedTime,

    -- * WorkspaceConnectionStatus
    WorkspaceConnectionStatus (..),
    newWorkspaceConnectionStatus,
    workspaceConnectionStatus_connectionState,
    workspaceConnectionStatus_workspaceId,
    workspaceConnectionStatus_lastKnownUserConnectionTimestamp,
    workspaceConnectionStatus_connectionStateCheckTimestamp,

    -- * WorkspaceCreationProperties
    WorkspaceCreationProperties (..),
    newWorkspaceCreationProperties,
    workspaceCreationProperties_enableMaintenanceMode,
    workspaceCreationProperties_defaultOu,
    workspaceCreationProperties_enableInternetAccess,
    workspaceCreationProperties_enableWorkDocs,
    workspaceCreationProperties_customSecurityGroupId,
    workspaceCreationProperties_userEnabledAsLocalAdministrator,

    -- * WorkspaceDirectory
    WorkspaceDirectory (..),
    newWorkspaceDirectory,
    workspaceDirectory_registrationCode,
    workspaceDirectory_workspaceSecurityGroupId,
    workspaceDirectory_alias,
    workspaceDirectory_workspaceAccessProperties,
    workspaceDirectory_ipGroupIds,
    workspaceDirectory_subnetIds,
    workspaceDirectory_customerUserName,
    workspaceDirectory_tenancy,
    workspaceDirectory_state,
    workspaceDirectory_directoryId,
    workspaceDirectory_iamRoleId,
    workspaceDirectory_directoryType,
    workspaceDirectory_selfservicePermissions,
    workspaceDirectory_directoryName,
    workspaceDirectory_dnsIpAddresses,
    workspaceDirectory_workspaceCreationProperties,

    -- * WorkspaceImage
    WorkspaceImage (..),
    newWorkspaceImage,
    workspaceImage_imageId,
    workspaceImage_name,
    workspaceImage_state,
    workspaceImage_description,
    workspaceImage_requiredTenancy,
    workspaceImage_errorMessage,
    workspaceImage_created,
    workspaceImage_operatingSystem,
    workspaceImage_ownerAccountId,
    workspaceImage_errorCode,

    -- * WorkspaceProperties
    WorkspaceProperties (..),
    newWorkspaceProperties,
    workspaceProperties_rootVolumeSizeGib,
    workspaceProperties_runningMode,
    workspaceProperties_userVolumeSizeGib,
    workspaceProperties_runningModeAutoStopTimeoutInMinutes,
    workspaceProperties_computeTypeName,

    -- * WorkspaceRequest
    WorkspaceRequest (..),
    newWorkspaceRequest,
    workspaceRequest_workspaceProperties,
    workspaceRequest_rootVolumeEncryptionEnabled,
    workspaceRequest_userVolumeEncryptionEnabled,
    workspaceRequest_volumeEncryptionKey,
    workspaceRequest_tags,
    workspaceRequest_directoryId,
    workspaceRequest_userName,
    workspaceRequest_bundleId,

    -- * WorkspacesIpGroup
    WorkspacesIpGroup (..),
    newWorkspacesIpGroup,
    workspacesIpGroup_userRules,
    workspacesIpGroup_groupDesc,
    workspacesIpGroup_groupName,
    workspacesIpGroup_groupId,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
import Network.AWS.WorkSpaces.Types.ImagePermission
import Network.AWS.WorkSpaces.Types.ImageType
import Network.AWS.WorkSpaces.Types.IpRuleItem
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
import Network.AWS.WorkSpaces.Types.WorkspacesIpGroup

-- | API version @2015-04-08@ of the Amazon WorkSpaces SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "WorkSpaces",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "workspaces",
      Core._serviceSigningName = "workspaces",
      Core._serviceVersion = "2015-04-08",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "WorkSpaces",
      Core._serviceRetry = retry
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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified resource is not available.
_ResourceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"

-- | The configuration of this network is not supported for this operation,
-- or your network configuration conflicts with the Amazon WorkSpaces
-- management network IP range. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-vpc.html Configure a VPC for Amazon WorkSpaces>.
_UnsupportedNetworkConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedNetworkConfigurationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedNetworkConfigurationException"

-- | This operation is not supported.
_OperationNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotSupportedException =
  Core._MatchServiceError
    defaultService
    "OperationNotSupportedException"

-- | Your resource limits have been exceeded.
_ResourceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The properties of this WorkSpace are currently being modified. Try again
-- in a moment.
_OperationInProgressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationInProgressException =
  Core._MatchServiceError
    defaultService
    "OperationInProgressException"

-- | The workspaces_DefaultRole role could not be found. If this is the first
-- time you are registering a directory, you will need to create the
-- workspaces_DefaultRole role before you can register a directory. For
-- more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspaces-access-control.html#create-default-role Creating the workspaces_DefaultRole Role>.
_WorkspacesDefaultRoleNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_WorkspacesDefaultRoleNotFoundException =
  Core._MatchServiceError
    defaultService
    "WorkspacesDefaultRoleNotFoundException"

-- | The resource is associated with a directory.
_ResourceAssociatedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAssociatedException =
  Core._MatchServiceError
    defaultService
    "ResourceAssociatedException"

-- | The configuration of this WorkSpace is not supported for this operation.
-- For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/required-service-components.html Required Configuration and Service Components for WorkSpaces>
-- .
_UnsupportedWorkspaceConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedWorkspaceConfigurationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedWorkspaceConfigurationException"

-- | One or more parameter values are not valid.
_InvalidParameterValuesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValuesException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValuesException"

-- | The resource could not be created.
_ResourceCreationFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceCreationFailedException =
  Core._MatchServiceError
    defaultService
    "ResourceCreationFailedException"

-- | The user is not authorized to access a resource.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The state of the resource is not valid for this operation.
_InvalidResourceStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceStateException =
  Core._MatchServiceError
    defaultService
    "InvalidResourceStateException"
