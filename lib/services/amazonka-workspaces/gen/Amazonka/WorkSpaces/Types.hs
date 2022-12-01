{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkSpaces.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceAlreadyExistsException,
    _WorkspacesDefaultRoleNotFoundException,
    _ResourceUnavailableException,
    _ResourceAssociatedException,
    _ResourceCreationFailedException,
    _InvalidParameterValuesException,
    _AccessDeniedException,
    _InvalidResourceStateException,
    _UnsupportedWorkspaceConfigurationException,
    _ResourceNotFoundException,
    _ResourceLimitExceededException,
    _OperationInProgressException,
    _UnsupportedNetworkConfigurationException,
    _OperationNotSupportedException,

    -- * AccessPropertyValue
    AccessPropertyValue (..),

    -- * Application
    Application (..),

    -- * AssociationStatus
    AssociationStatus (..),

    -- * BundleType
    BundleType (..),

    -- * CertificateBasedAuthStatusEnum
    CertificateBasedAuthStatusEnum (..),

    -- * ClientDeviceType
    ClientDeviceType (..),

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

    -- * DeletableCertificateBasedAuthProperty
    DeletableCertificateBasedAuthProperty (..),

    -- * DeletableSamlProperty
    DeletableSamlProperty (..),

    -- * ImageType
    ImageType (..),

    -- * LogUploadEnum
    LogUploadEnum (..),

    -- * ModificationResourceEnum
    ModificationResourceEnum (..),

    -- * ModificationStateEnum
    ModificationStateEnum (..),

    -- * OperatingSystemType
    OperatingSystemType (..),

    -- * Protocol
    Protocol (..),

    -- * ReconnectEnum
    ReconnectEnum (..),

    -- * RunningMode
    RunningMode (..),

    -- * SamlStatusEnum
    SamlStatusEnum (..),

    -- * StandbyWorkspaceRelationshipType
    StandbyWorkspaceRelationshipType (..),

    -- * TargetWorkspaceState
    TargetWorkspaceState (..),

    -- * Tenancy
    Tenancy (..),

    -- * WorkspaceBundleState
    WorkspaceBundleState (..),

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
    accountModification_errorMessage,
    accountModification_dedicatedTenancyManagementCidrRange,
    accountModification_modificationState,
    accountModification_dedicatedTenancySupport,
    accountModification_errorCode,
    accountModification_startTime,

    -- * CertificateBasedAuthProperties
    CertificateBasedAuthProperties (..),
    newCertificateBasedAuthProperties,
    certificateBasedAuthProperties_certificateAuthorityArn,
    certificateBasedAuthProperties_status,

    -- * ClientProperties
    ClientProperties (..),
    newClientProperties,
    clientProperties_reconnectEnabled,
    clientProperties_logUploadEnabled,

    -- * ClientPropertiesResult
    ClientPropertiesResult (..),
    newClientPropertiesResult,
    clientPropertiesResult_resourceId,
    clientPropertiesResult_clientProperties,

    -- * ComputeType
    ComputeType (..),
    newComputeType,
    computeType_name,

    -- * ConnectClientAddIn
    ConnectClientAddIn (..),
    newConnectClientAddIn,
    connectClientAddIn_resourceId,
    connectClientAddIn_name,
    connectClientAddIn_addInId,
    connectClientAddIn_url,

    -- * ConnectionAlias
    ConnectionAlias (..),
    newConnectionAlias,
    connectionAlias_associations,
    connectionAlias_aliasId,
    connectionAlias_state,
    connectionAlias_connectionString,
    connectionAlias_ownerAccountId,

    -- * ConnectionAliasAssociation
    ConnectionAliasAssociation (..),
    newConnectionAliasAssociation,
    connectionAliasAssociation_resourceId,
    connectionAliasAssociation_associatedAccountId,
    connectionAliasAssociation_associationStatus,
    connectionAliasAssociation_connectionIdentifier,

    -- * ConnectionAliasPermission
    ConnectionAliasPermission (..),
    newConnectionAliasPermission,
    connectionAliasPermission_sharedAccountId,
    connectionAliasPermission_allowAssociation,

    -- * DefaultClientBrandingAttributes
    DefaultClientBrandingAttributes (..),
    newDefaultClientBrandingAttributes,
    defaultClientBrandingAttributes_supportLink,
    defaultClientBrandingAttributes_supportEmail,
    defaultClientBrandingAttributes_logoUrl,
    defaultClientBrandingAttributes_loginMessage,
    defaultClientBrandingAttributes_forgotPasswordLink,

    -- * DefaultImportClientBrandingAttributes
    DefaultImportClientBrandingAttributes (..),
    newDefaultImportClientBrandingAttributes,
    defaultImportClientBrandingAttributes_supportLink,
    defaultImportClientBrandingAttributes_supportEmail,
    defaultImportClientBrandingAttributes_logo,
    defaultImportClientBrandingAttributes_loginMessage,
    defaultImportClientBrandingAttributes_forgotPasswordLink,

    -- * DefaultWorkspaceCreationProperties
    DefaultWorkspaceCreationProperties (..),
    newDefaultWorkspaceCreationProperties,
    defaultWorkspaceCreationProperties_userEnabledAsLocalAdministrator,
    defaultWorkspaceCreationProperties_enableInternetAccess,
    defaultWorkspaceCreationProperties_enableWorkDocs,
    defaultWorkspaceCreationProperties_defaultOu,
    defaultWorkspaceCreationProperties_enableMaintenanceMode,
    defaultWorkspaceCreationProperties_customSecurityGroupId,

    -- * FailedCreateStandbyWorkspacesRequest
    FailedCreateStandbyWorkspacesRequest (..),
    newFailedCreateStandbyWorkspacesRequest,
    failedCreateStandbyWorkspacesRequest_errorMessage,
    failedCreateStandbyWorkspacesRequest_standbyWorkspaceRequest,
    failedCreateStandbyWorkspacesRequest_errorCode,

    -- * FailedCreateWorkspaceRequest
    FailedCreateWorkspaceRequest (..),
    newFailedCreateWorkspaceRequest,
    failedCreateWorkspaceRequest_errorMessage,
    failedCreateWorkspaceRequest_workspaceRequest,
    failedCreateWorkspaceRequest_errorCode,

    -- * FailedWorkspaceChangeRequest
    FailedWorkspaceChangeRequest (..),
    newFailedWorkspaceChangeRequest,
    failedWorkspaceChangeRequest_errorMessage,
    failedWorkspaceChangeRequest_workspaceId,
    failedWorkspaceChangeRequest_errorCode,

    -- * ImagePermission
    ImagePermission (..),
    newImagePermission,
    imagePermission_sharedAccountId,

    -- * IosClientBrandingAttributes
    IosClientBrandingAttributes (..),
    newIosClientBrandingAttributes,
    iosClientBrandingAttributes_supportLink,
    iosClientBrandingAttributes_supportEmail,
    iosClientBrandingAttributes_logo2xUrl,
    iosClientBrandingAttributes_logoUrl,
    iosClientBrandingAttributes_logo3xUrl,
    iosClientBrandingAttributes_loginMessage,
    iosClientBrandingAttributes_forgotPasswordLink,

    -- * IosImportClientBrandingAttributes
    IosImportClientBrandingAttributes (..),
    newIosImportClientBrandingAttributes,
    iosImportClientBrandingAttributes_supportLink,
    iosImportClientBrandingAttributes_supportEmail,
    iosImportClientBrandingAttributes_logo,
    iosImportClientBrandingAttributes_logo3x,
    iosImportClientBrandingAttributes_loginMessage,
    iosImportClientBrandingAttributes_forgotPasswordLink,
    iosImportClientBrandingAttributes_logo2x,

    -- * IpRuleItem
    IpRuleItem (..),
    newIpRuleItem,
    ipRuleItem_ipRule,
    ipRuleItem_ruleDesc,

    -- * ModificationState
    ModificationState (..),
    newModificationState,
    modificationState_state,
    modificationState_resource,

    -- * OperatingSystem
    OperatingSystem (..),
    newOperatingSystem,
    operatingSystem_type,

    -- * PendingCreateStandbyWorkspacesRequest
    PendingCreateStandbyWorkspacesRequest (..),
    newPendingCreateStandbyWorkspacesRequest,
    pendingCreateStandbyWorkspacesRequest_directoryId,
    pendingCreateStandbyWorkspacesRequest_userName,
    pendingCreateStandbyWorkspacesRequest_state,
    pendingCreateStandbyWorkspacesRequest_workspaceId,

    -- * RebootRequest
    RebootRequest (..),
    newRebootRequest,
    rebootRequest_workspaceId,

    -- * RebuildRequest
    RebuildRequest (..),
    newRebuildRequest,
    rebuildRequest_workspaceId,

    -- * RelatedWorkspaceProperties
    RelatedWorkspaceProperties (..),
    newRelatedWorkspaceProperties,
    relatedWorkspaceProperties_type,
    relatedWorkspaceProperties_state,
    relatedWorkspaceProperties_workspaceId,
    relatedWorkspaceProperties_region,

    -- * RootStorage
    RootStorage (..),
    newRootStorage,
    rootStorage_capacity,

    -- * SamlProperties
    SamlProperties (..),
    newSamlProperties,
    samlProperties_status,
    samlProperties_relayStateParameterName,
    samlProperties_userAccessUrl,

    -- * SelfservicePermissions
    SelfservicePermissions (..),
    newSelfservicePermissions,
    selfservicePermissions_increaseVolumeSize,
    selfservicePermissions_rebuildWorkspace,
    selfservicePermissions_changeComputeType,
    selfservicePermissions_switchRunningMode,
    selfservicePermissions_restartWorkspace,

    -- * Snapshot
    Snapshot (..),
    newSnapshot,
    snapshot_snapshotTime,

    -- * StandbyWorkspace
    StandbyWorkspace (..),
    newStandbyWorkspace,
    standbyWorkspace_tags,
    standbyWorkspace_volumeEncryptionKey,
    standbyWorkspace_primaryWorkspaceId,
    standbyWorkspace_directoryId,

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

    -- * UpdateResult
    UpdateResult (..),
    newUpdateResult,
    updateResult_updateAvailable,
    updateResult_description,

    -- * UserStorage
    UserStorage (..),
    newUserStorage,
    userStorage_capacity,

    -- * Workspace
    Workspace (..),
    newWorkspace,
    workspace_workspaceProperties,
    workspace_directoryId,
    workspace_relatedWorkspaces,
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

    -- * WorkspaceAccessProperties
    WorkspaceAccessProperties (..),
    newWorkspaceAccessProperties,
    workspaceAccessProperties_deviceTypeAndroid,
    workspaceAccessProperties_deviceTypeLinux,
    workspaceAccessProperties_deviceTypeWeb,
    workspaceAccessProperties_deviceTypeOsx,
    workspaceAccessProperties_deviceTypeChromeOs,
    workspaceAccessProperties_deviceTypeWindows,
    workspaceAccessProperties_deviceTypeIos,
    workspaceAccessProperties_deviceTypeZeroClient,

    -- * WorkspaceBundle
    WorkspaceBundle (..),
    newWorkspaceBundle,
    workspaceBundle_bundleType,
    workspaceBundle_name,
    workspaceBundle_rootStorage,
    workspaceBundle_state,
    workspaceBundle_owner,
    workspaceBundle_lastUpdatedTime,
    workspaceBundle_description,
    workspaceBundle_bundleId,
    workspaceBundle_creationTime,
    workspaceBundle_computeType,
    workspaceBundle_userStorage,
    workspaceBundle_imageId,

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
    workspaceCreationProperties_userEnabledAsLocalAdministrator,
    workspaceCreationProperties_enableInternetAccess,
    workspaceCreationProperties_enableWorkDocs,
    workspaceCreationProperties_defaultOu,
    workspaceCreationProperties_enableMaintenanceMode,
    workspaceCreationProperties_customSecurityGroupId,

    -- * WorkspaceDirectory
    WorkspaceDirectory (..),
    newWorkspaceDirectory,
    workspaceDirectory_alias,
    workspaceDirectory_directoryName,
    workspaceDirectory_directoryId,
    workspaceDirectory_workspaceSecurityGroupId,
    workspaceDirectory_ipGroupIds,
    workspaceDirectory_directoryType,
    workspaceDirectory_certificateBasedAuthProperties,
    workspaceDirectory_state,
    workspaceDirectory_customerUserName,
    workspaceDirectory_samlProperties,
    workspaceDirectory_dnsIpAddresses,
    workspaceDirectory_iamRoleId,
    workspaceDirectory_registrationCode,
    workspaceDirectory_workspaceCreationProperties,
    workspaceDirectory_subnetIds,
    workspaceDirectory_selfservicePermissions,
    workspaceDirectory_workspaceAccessProperties,
    workspaceDirectory_tenancy,

    -- * WorkspaceImage
    WorkspaceImage (..),
    newWorkspaceImage,
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

    -- * WorkspaceProperties
    WorkspaceProperties (..),
    newWorkspaceProperties,
    workspaceProperties_userVolumeSizeGib,
    workspaceProperties_runningMode,
    workspaceProperties_protocols,
    workspaceProperties_computeTypeName,
    workspaceProperties_rootVolumeSizeGib,
    workspaceProperties_runningModeAutoStopTimeoutInMinutes,

    -- * WorkspaceRequest
    WorkspaceRequest (..),
    newWorkspaceRequest,
    workspaceRequest_tags,
    workspaceRequest_workspaceProperties,
    workspaceRequest_volumeEncryptionKey,
    workspaceRequest_userVolumeEncryptionEnabled,
    workspaceRequest_rootVolumeEncryptionEnabled,
    workspaceRequest_directoryId,
    workspaceRequest_userName,
    workspaceRequest_bundleId,

    -- * WorkspacesIpGroup
    WorkspacesIpGroup (..),
    newWorkspacesIpGroup,
    workspacesIpGroup_userRules,
    workspacesIpGroup_groupName,
    workspacesIpGroup_groupDesc,
    workspacesIpGroup_groupId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.WorkSpaces.Types.AccessPropertyValue
import Amazonka.WorkSpaces.Types.AccountModification
import Amazonka.WorkSpaces.Types.Application
import Amazonka.WorkSpaces.Types.AssociationStatus
import Amazonka.WorkSpaces.Types.BundleType
import Amazonka.WorkSpaces.Types.CertificateBasedAuthProperties
import Amazonka.WorkSpaces.Types.CertificateBasedAuthStatusEnum
import Amazonka.WorkSpaces.Types.ClientDeviceType
import Amazonka.WorkSpaces.Types.ClientProperties
import Amazonka.WorkSpaces.Types.ClientPropertiesResult
import Amazonka.WorkSpaces.Types.Compute
import Amazonka.WorkSpaces.Types.ComputeType
import Amazonka.WorkSpaces.Types.ConnectClientAddIn
import Amazonka.WorkSpaces.Types.ConnectionAlias
import Amazonka.WorkSpaces.Types.ConnectionAliasAssociation
import Amazonka.WorkSpaces.Types.ConnectionAliasPermission
import Amazonka.WorkSpaces.Types.ConnectionAliasState
import Amazonka.WorkSpaces.Types.ConnectionState
import Amazonka.WorkSpaces.Types.DedicatedTenancyModificationStateEnum
import Amazonka.WorkSpaces.Types.DedicatedTenancySupportEnum
import Amazonka.WorkSpaces.Types.DedicatedTenancySupportResultEnum
import Amazonka.WorkSpaces.Types.DefaultClientBrandingAttributes
import Amazonka.WorkSpaces.Types.DefaultImportClientBrandingAttributes
import Amazonka.WorkSpaces.Types.DefaultWorkspaceCreationProperties
import Amazonka.WorkSpaces.Types.DeletableCertificateBasedAuthProperty
import Amazonka.WorkSpaces.Types.DeletableSamlProperty
import Amazonka.WorkSpaces.Types.FailedCreateStandbyWorkspacesRequest
import Amazonka.WorkSpaces.Types.FailedCreateWorkspaceRequest
import Amazonka.WorkSpaces.Types.FailedWorkspaceChangeRequest
import Amazonka.WorkSpaces.Types.ImagePermission
import Amazonka.WorkSpaces.Types.ImageType
import Amazonka.WorkSpaces.Types.IosClientBrandingAttributes
import Amazonka.WorkSpaces.Types.IosImportClientBrandingAttributes
import Amazonka.WorkSpaces.Types.IpRuleItem
import Amazonka.WorkSpaces.Types.LogUploadEnum
import Amazonka.WorkSpaces.Types.ModificationResourceEnum
import Amazonka.WorkSpaces.Types.ModificationState
import Amazonka.WorkSpaces.Types.ModificationStateEnum
import Amazonka.WorkSpaces.Types.OperatingSystem
import Amazonka.WorkSpaces.Types.OperatingSystemType
import Amazonka.WorkSpaces.Types.PendingCreateStandbyWorkspacesRequest
import Amazonka.WorkSpaces.Types.Protocol
import Amazonka.WorkSpaces.Types.RebootRequest
import Amazonka.WorkSpaces.Types.RebuildRequest
import Amazonka.WorkSpaces.Types.ReconnectEnum
import Amazonka.WorkSpaces.Types.RelatedWorkspaceProperties
import Amazonka.WorkSpaces.Types.RootStorage
import Amazonka.WorkSpaces.Types.RunningMode
import Amazonka.WorkSpaces.Types.SamlProperties
import Amazonka.WorkSpaces.Types.SamlStatusEnum
import Amazonka.WorkSpaces.Types.SelfservicePermissions
import Amazonka.WorkSpaces.Types.Snapshot
import Amazonka.WorkSpaces.Types.StandbyWorkspace
import Amazonka.WorkSpaces.Types.StandbyWorkspaceRelationshipType
import Amazonka.WorkSpaces.Types.StartRequest
import Amazonka.WorkSpaces.Types.StopRequest
import Amazonka.WorkSpaces.Types.Tag
import Amazonka.WorkSpaces.Types.TargetWorkspaceState
import Amazonka.WorkSpaces.Types.Tenancy
import Amazonka.WorkSpaces.Types.TerminateRequest
import Amazonka.WorkSpaces.Types.UpdateResult
import Amazonka.WorkSpaces.Types.UserStorage
import Amazonka.WorkSpaces.Types.Workspace
import Amazonka.WorkSpaces.Types.WorkspaceAccessProperties
import Amazonka.WorkSpaces.Types.WorkspaceBundle
import Amazonka.WorkSpaces.Types.WorkspaceBundleState
import Amazonka.WorkSpaces.Types.WorkspaceConnectionStatus
import Amazonka.WorkSpaces.Types.WorkspaceCreationProperties
import Amazonka.WorkSpaces.Types.WorkspaceDirectory
import Amazonka.WorkSpaces.Types.WorkspaceDirectoryState
import Amazonka.WorkSpaces.Types.WorkspaceDirectoryType
import Amazonka.WorkSpaces.Types.WorkspaceImage
import Amazonka.WorkSpaces.Types.WorkspaceImageIngestionProcess
import Amazonka.WorkSpaces.Types.WorkspaceImageRequiredTenancy
import Amazonka.WorkSpaces.Types.WorkspaceImageState
import Amazonka.WorkSpaces.Types.WorkspaceProperties
import Amazonka.WorkSpaces.Types.WorkspaceRequest
import Amazonka.WorkSpaces.Types.WorkspaceState
import Amazonka.WorkSpaces.Types.WorkspacesIpGroup

-- | API version @2015-04-08@ of the Amazon WorkSpaces SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "WorkSpaces",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "workspaces",
      Core.signingName = "workspaces",
      Core.version = "2015-04-08",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "WorkSpaces",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

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

-- | The specified resource is not available.
_ResourceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"

-- | The resource is associated with a directory.
_ResourceAssociatedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAssociatedException =
  Core._MatchServiceError
    defaultService
    "ResourceAssociatedException"

-- | The resource could not be created.
_ResourceCreationFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceCreationFailedException =
  Core._MatchServiceError
    defaultService
    "ResourceCreationFailedException"

-- | One or more parameter values are not valid.
_InvalidParameterValuesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValuesException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValuesException"

-- | The user is not authorized to access a resource.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The state of the resource is not valid for this operation.
_InvalidResourceStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceStateException =
  Core._MatchServiceError
    defaultService
    "InvalidResourceStateException"

-- | The configuration of this WorkSpace is not supported for this operation.
-- For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/required-service-components.html Required Configuration and Service Components for WorkSpaces>
-- .
_UnsupportedWorkspaceConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedWorkspaceConfigurationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedWorkspaceConfigurationException"

-- | The resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Your resource limits have been exceeded.
_ResourceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"

-- | The properties of this WorkSpace are currently being modified. Try again
-- in a moment.
_OperationInProgressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationInProgressException =
  Core._MatchServiceError
    defaultService
    "OperationInProgressException"

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
