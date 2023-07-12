{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Nimble.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerErrorException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * AutomaticTerminationMode
    AutomaticTerminationMode (..),

    -- * LaunchProfilePersona
    LaunchProfilePersona (..),

    -- * LaunchProfilePlatform
    LaunchProfilePlatform (..),

    -- * LaunchProfileState
    LaunchProfileState (..),

    -- * LaunchProfileStatusCode
    LaunchProfileStatusCode (..),

    -- * LaunchProfileValidationState
    LaunchProfileValidationState (..),

    -- * LaunchProfileValidationStatusCode
    LaunchProfileValidationStatusCode (..),

    -- * LaunchProfileValidationType
    LaunchProfileValidationType (..),

    -- * SessionBackupMode
    SessionBackupMode (..),

    -- * SessionPersistenceMode
    SessionPersistenceMode (..),

    -- * StreamingClipboardMode
    StreamingClipboardMode (..),

    -- * StreamingImageEncryptionConfigurationKeyType
    StreamingImageEncryptionConfigurationKeyType (..),

    -- * StreamingImageState
    StreamingImageState (..),

    -- * StreamingImageStatusCode
    StreamingImageStatusCode (..),

    -- * StreamingInstanceType
    StreamingInstanceType (..),

    -- * StreamingSessionState
    StreamingSessionState (..),

    -- * StreamingSessionStatusCode
    StreamingSessionStatusCode (..),

    -- * StreamingSessionStorageMode
    StreamingSessionStorageMode (..),

    -- * StreamingSessionStreamState
    StreamingSessionStreamState (..),

    -- * StreamingSessionStreamStatusCode
    StreamingSessionStreamStatusCode (..),

    -- * StudioComponentInitializationScriptRunContext
    StudioComponentInitializationScriptRunContext (..),

    -- * StudioComponentState
    StudioComponentState (..),

    -- * StudioComponentStatusCode
    StudioComponentStatusCode (..),

    -- * StudioComponentSubtype
    StudioComponentSubtype (..),

    -- * StudioComponentType
    StudioComponentType (..),

    -- * StudioEncryptionConfigurationKeyType
    StudioEncryptionConfigurationKeyType (..),

    -- * StudioPersona
    StudioPersona (..),

    -- * StudioState
    StudioState (..),

    -- * StudioStatusCode
    StudioStatusCode (..),

    -- * VolumeRetentionMode
    VolumeRetentionMode (..),

    -- * ActiveDirectoryComputerAttribute
    ActiveDirectoryComputerAttribute (..),
    newActiveDirectoryComputerAttribute,
    activeDirectoryComputerAttribute_name,
    activeDirectoryComputerAttribute_value,

    -- * ActiveDirectoryConfiguration
    ActiveDirectoryConfiguration (..),
    newActiveDirectoryConfiguration,
    activeDirectoryConfiguration_computerAttributes,
    activeDirectoryConfiguration_directoryId,
    activeDirectoryConfiguration_organizationalUnitDistinguishedName,

    -- * ComputeFarmConfiguration
    ComputeFarmConfiguration (..),
    newComputeFarmConfiguration,
    computeFarmConfiguration_activeDirectoryUser,
    computeFarmConfiguration_endpoint,

    -- * Eula
    Eula (..),
    newEula,
    eula_content,
    eula_createdAt,
    eula_eulaId,
    eula_name,
    eula_updatedAt,

    -- * EulaAcceptance
    EulaAcceptance (..),
    newEulaAcceptance,
    eulaAcceptance_acceptedAt,
    eulaAcceptance_acceptedBy,
    eulaAcceptance_accepteeId,
    eulaAcceptance_eulaAcceptanceId,
    eulaAcceptance_eulaId,

    -- * LaunchProfile
    LaunchProfile (..),
    newLaunchProfile,
    launchProfile_arn,
    launchProfile_createdAt,
    launchProfile_createdBy,
    launchProfile_description,
    launchProfile_ec2SubnetIds,
    launchProfile_launchProfileId,
    launchProfile_launchProfileProtocolVersions,
    launchProfile_name,
    launchProfile_state,
    launchProfile_statusCode,
    launchProfile_statusMessage,
    launchProfile_streamConfiguration,
    launchProfile_studioComponentIds,
    launchProfile_tags,
    launchProfile_updatedAt,
    launchProfile_updatedBy,
    launchProfile_validationResults,

    -- * LaunchProfileInitialization
    LaunchProfileInitialization (..),
    newLaunchProfileInitialization,
    launchProfileInitialization_activeDirectory,
    launchProfileInitialization_ec2SecurityGroupIds,
    launchProfileInitialization_launchProfileId,
    launchProfileInitialization_launchProfileProtocolVersion,
    launchProfileInitialization_launchPurpose,
    launchProfileInitialization_name,
    launchProfileInitialization_platform,
    launchProfileInitialization_systemInitializationScripts,
    launchProfileInitialization_userInitializationScripts,

    -- * LaunchProfileInitializationActiveDirectory
    LaunchProfileInitializationActiveDirectory (..),
    newLaunchProfileInitializationActiveDirectory,
    launchProfileInitializationActiveDirectory_computerAttributes,
    launchProfileInitializationActiveDirectory_directoryId,
    launchProfileInitializationActiveDirectory_directoryName,
    launchProfileInitializationActiveDirectory_dnsIpAddresses,
    launchProfileInitializationActiveDirectory_organizationalUnitDistinguishedName,
    launchProfileInitializationActiveDirectory_studioComponentId,
    launchProfileInitializationActiveDirectory_studioComponentName,

    -- * LaunchProfileInitializationScript
    LaunchProfileInitializationScript (..),
    newLaunchProfileInitializationScript,
    launchProfileInitializationScript_runtimeRoleArn,
    launchProfileInitializationScript_script,
    launchProfileInitializationScript_secureInitializationRoleArn,
    launchProfileInitializationScript_studioComponentId,
    launchProfileInitializationScript_studioComponentName,

    -- * LaunchProfileMembership
    LaunchProfileMembership (..),
    newLaunchProfileMembership,
    launchProfileMembership_identityStoreId,
    launchProfileMembership_persona,
    launchProfileMembership_principalId,
    launchProfileMembership_sid,

    -- * LicenseServiceConfiguration
    LicenseServiceConfiguration (..),
    newLicenseServiceConfiguration,
    licenseServiceConfiguration_endpoint,

    -- * NewLaunchProfileMember
    NewLaunchProfileMember (..),
    newNewLaunchProfileMember,
    newLaunchProfileMember_persona,
    newLaunchProfileMember_principalId,

    -- * NewStudioMember
    NewStudioMember (..),
    newNewStudioMember,
    newStudioMember_persona,
    newStudioMember_principalId,

    -- * ScriptParameterKeyValue
    ScriptParameterKeyValue (..),
    newScriptParameterKeyValue,
    scriptParameterKeyValue_key,
    scriptParameterKeyValue_value,

    -- * SharedFileSystemConfiguration
    SharedFileSystemConfiguration (..),
    newSharedFileSystemConfiguration,
    sharedFileSystemConfiguration_endpoint,
    sharedFileSystemConfiguration_fileSystemId,
    sharedFileSystemConfiguration_linuxMountPoint,
    sharedFileSystemConfiguration_shareName,
    sharedFileSystemConfiguration_windowsMountDrive,

    -- * StreamConfiguration
    StreamConfiguration (..),
    newStreamConfiguration,
    streamConfiguration_automaticTerminationMode,
    streamConfiguration_maxSessionLengthInMinutes,
    streamConfiguration_maxStoppedSessionLengthInMinutes,
    streamConfiguration_sessionBackup,
    streamConfiguration_sessionPersistenceMode,
    streamConfiguration_sessionStorage,
    streamConfiguration_volumeConfiguration,
    streamConfiguration_clipboardMode,
    streamConfiguration_ec2InstanceTypes,
    streamConfiguration_streamingImageIds,

    -- * StreamConfigurationCreate
    StreamConfigurationCreate (..),
    newStreamConfigurationCreate,
    streamConfigurationCreate_automaticTerminationMode,
    streamConfigurationCreate_maxSessionLengthInMinutes,
    streamConfigurationCreate_maxStoppedSessionLengthInMinutes,
    streamConfigurationCreate_sessionBackup,
    streamConfigurationCreate_sessionPersistenceMode,
    streamConfigurationCreate_sessionStorage,
    streamConfigurationCreate_volumeConfiguration,
    streamConfigurationCreate_clipboardMode,
    streamConfigurationCreate_ec2InstanceTypes,
    streamConfigurationCreate_streamingImageIds,

    -- * StreamConfigurationSessionBackup
    StreamConfigurationSessionBackup (..),
    newStreamConfigurationSessionBackup,
    streamConfigurationSessionBackup_maxBackupsToRetain,
    streamConfigurationSessionBackup_mode,

    -- * StreamConfigurationSessionStorage
    StreamConfigurationSessionStorage (..),
    newStreamConfigurationSessionStorage,
    streamConfigurationSessionStorage_root,
    streamConfigurationSessionStorage_mode,

    -- * StreamingImage
    StreamingImage (..),
    newStreamingImage,
    streamingImage_arn,
    streamingImage_description,
    streamingImage_ec2ImageId,
    streamingImage_encryptionConfiguration,
    streamingImage_eulaIds,
    streamingImage_name,
    streamingImage_owner,
    streamingImage_platform,
    streamingImage_state,
    streamingImage_statusCode,
    streamingImage_statusMessage,
    streamingImage_streamingImageId,
    streamingImage_tags,

    -- * StreamingImageEncryptionConfiguration
    StreamingImageEncryptionConfiguration (..),
    newStreamingImageEncryptionConfiguration,
    streamingImageEncryptionConfiguration_keyArn,
    streamingImageEncryptionConfiguration_keyType,

    -- * StreamingSession
    StreamingSession (..),
    newStreamingSession,
    streamingSession_arn,
    streamingSession_automaticTerminationMode,
    streamingSession_backupMode,
    streamingSession_createdAt,
    streamingSession_createdBy,
    streamingSession_ec2InstanceType,
    streamingSession_launchProfileId,
    streamingSession_maxBackupsToRetain,
    streamingSession_ownedBy,
    streamingSession_sessionId,
    streamingSession_sessionPersistenceMode,
    streamingSession_startedAt,
    streamingSession_startedBy,
    streamingSession_startedFromBackupId,
    streamingSession_state,
    streamingSession_statusCode,
    streamingSession_statusMessage,
    streamingSession_stopAt,
    streamingSession_stoppedAt,
    streamingSession_stoppedBy,
    streamingSession_streamingImageId,
    streamingSession_tags,
    streamingSession_terminateAt,
    streamingSession_updatedAt,
    streamingSession_updatedBy,
    streamingSession_volumeConfiguration,
    streamingSession_volumeRetentionMode,

    -- * StreamingSessionBackup
    StreamingSessionBackup (..),
    newStreamingSessionBackup,
    streamingSessionBackup_arn,
    streamingSessionBackup_backupId,
    streamingSessionBackup_createdAt,
    streamingSessionBackup_launchProfileId,
    streamingSessionBackup_ownedBy,
    streamingSessionBackup_sessionId,
    streamingSessionBackup_state,
    streamingSessionBackup_statusCode,
    streamingSessionBackup_statusMessage,
    streamingSessionBackup_tags,

    -- * StreamingSessionStorageRoot
    StreamingSessionStorageRoot (..),
    newStreamingSessionStorageRoot,
    streamingSessionStorageRoot_linux,
    streamingSessionStorageRoot_windows,

    -- * StreamingSessionStream
    StreamingSessionStream (..),
    newStreamingSessionStream,
    streamingSessionStream_createdAt,
    streamingSessionStream_createdBy,
    streamingSessionStream_expiresAt,
    streamingSessionStream_ownedBy,
    streamingSessionStream_state,
    streamingSessionStream_statusCode,
    streamingSessionStream_streamId,
    streamingSessionStream_url,

    -- * Studio
    Studio (..),
    newStudio,
    studio_adminRoleArn,
    studio_arn,
    studio_createdAt,
    studio_displayName,
    studio_homeRegion,
    studio_ssoClientId,
    studio_state,
    studio_statusCode,
    studio_statusMessage,
    studio_studioEncryptionConfiguration,
    studio_studioId,
    studio_studioName,
    studio_studioUrl,
    studio_tags,
    studio_updatedAt,
    studio_userRoleArn,

    -- * StudioComponent
    StudioComponent (..),
    newStudioComponent,
    studioComponent_arn,
    studioComponent_configuration,
    studioComponent_createdAt,
    studioComponent_createdBy,
    studioComponent_description,
    studioComponent_ec2SecurityGroupIds,
    studioComponent_initializationScripts,
    studioComponent_name,
    studioComponent_runtimeRoleArn,
    studioComponent_scriptParameters,
    studioComponent_secureInitializationRoleArn,
    studioComponent_state,
    studioComponent_statusCode,
    studioComponent_statusMessage,
    studioComponent_studioComponentId,
    studioComponent_subtype,
    studioComponent_tags,
    studioComponent_type,
    studioComponent_updatedAt,
    studioComponent_updatedBy,

    -- * StudioComponentConfiguration
    StudioComponentConfiguration (..),
    newStudioComponentConfiguration,
    studioComponentConfiguration_activeDirectoryConfiguration,
    studioComponentConfiguration_computeFarmConfiguration,
    studioComponentConfiguration_licenseServiceConfiguration,
    studioComponentConfiguration_sharedFileSystemConfiguration,

    -- * StudioComponentInitializationScript
    StudioComponentInitializationScript (..),
    newStudioComponentInitializationScript,
    studioComponentInitializationScript_launchProfileProtocolVersion,
    studioComponentInitializationScript_platform,
    studioComponentInitializationScript_runContext,
    studioComponentInitializationScript_script,

    -- * StudioComponentSummary
    StudioComponentSummary (..),
    newStudioComponentSummary,
    studioComponentSummary_createdAt,
    studioComponentSummary_createdBy,
    studioComponentSummary_description,
    studioComponentSummary_name,
    studioComponentSummary_studioComponentId,
    studioComponentSummary_subtype,
    studioComponentSummary_type,
    studioComponentSummary_updatedAt,
    studioComponentSummary_updatedBy,

    -- * StudioEncryptionConfiguration
    StudioEncryptionConfiguration (..),
    newStudioEncryptionConfiguration,
    studioEncryptionConfiguration_keyArn,
    studioEncryptionConfiguration_keyType,

    -- * StudioMembership
    StudioMembership (..),
    newStudioMembership,
    studioMembership_identityStoreId,
    studioMembership_persona,
    studioMembership_principalId,
    studioMembership_sid,

    -- * ValidationResult
    ValidationResult (..),
    newValidationResult,
    validationResult_state,
    validationResult_statusCode,
    validationResult_statusMessage,
    validationResult_type,

    -- * VolumeConfiguration
    VolumeConfiguration (..),
    newVolumeConfiguration,
    volumeConfiguration_iops,
    volumeConfiguration_size,
    volumeConfiguration_throughput,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Nimble.Types.ActiveDirectoryComputerAttribute
import Amazonka.Nimble.Types.ActiveDirectoryConfiguration
import Amazonka.Nimble.Types.AutomaticTerminationMode
import Amazonka.Nimble.Types.ComputeFarmConfiguration
import Amazonka.Nimble.Types.Eula
import Amazonka.Nimble.Types.EulaAcceptance
import Amazonka.Nimble.Types.LaunchProfile
import Amazonka.Nimble.Types.LaunchProfileInitialization
import Amazonka.Nimble.Types.LaunchProfileInitializationActiveDirectory
import Amazonka.Nimble.Types.LaunchProfileInitializationScript
import Amazonka.Nimble.Types.LaunchProfileMembership
import Amazonka.Nimble.Types.LaunchProfilePersona
import Amazonka.Nimble.Types.LaunchProfilePlatform
import Amazonka.Nimble.Types.LaunchProfileState
import Amazonka.Nimble.Types.LaunchProfileStatusCode
import Amazonka.Nimble.Types.LaunchProfileValidationState
import Amazonka.Nimble.Types.LaunchProfileValidationStatusCode
import Amazonka.Nimble.Types.LaunchProfileValidationType
import Amazonka.Nimble.Types.LicenseServiceConfiguration
import Amazonka.Nimble.Types.NewLaunchProfileMember
import Amazonka.Nimble.Types.NewStudioMember
import Amazonka.Nimble.Types.ScriptParameterKeyValue
import Amazonka.Nimble.Types.SessionBackupMode
import Amazonka.Nimble.Types.SessionPersistenceMode
import Amazonka.Nimble.Types.SharedFileSystemConfiguration
import Amazonka.Nimble.Types.StreamConfiguration
import Amazonka.Nimble.Types.StreamConfigurationCreate
import Amazonka.Nimble.Types.StreamConfigurationSessionBackup
import Amazonka.Nimble.Types.StreamConfigurationSessionStorage
import Amazonka.Nimble.Types.StreamingClipboardMode
import Amazonka.Nimble.Types.StreamingImage
import Amazonka.Nimble.Types.StreamingImageEncryptionConfiguration
import Amazonka.Nimble.Types.StreamingImageEncryptionConfigurationKeyType
import Amazonka.Nimble.Types.StreamingImageState
import Amazonka.Nimble.Types.StreamingImageStatusCode
import Amazonka.Nimble.Types.StreamingInstanceType
import Amazonka.Nimble.Types.StreamingSession
import Amazonka.Nimble.Types.StreamingSessionBackup
import Amazonka.Nimble.Types.StreamingSessionState
import Amazonka.Nimble.Types.StreamingSessionStatusCode
import Amazonka.Nimble.Types.StreamingSessionStorageMode
import Amazonka.Nimble.Types.StreamingSessionStorageRoot
import Amazonka.Nimble.Types.StreamingSessionStream
import Amazonka.Nimble.Types.StreamingSessionStreamState
import Amazonka.Nimble.Types.StreamingSessionStreamStatusCode
import Amazonka.Nimble.Types.Studio
import Amazonka.Nimble.Types.StudioComponent
import Amazonka.Nimble.Types.StudioComponentConfiguration
import Amazonka.Nimble.Types.StudioComponentInitializationScript
import Amazonka.Nimble.Types.StudioComponentInitializationScriptRunContext
import Amazonka.Nimble.Types.StudioComponentState
import Amazonka.Nimble.Types.StudioComponentStatusCode
import Amazonka.Nimble.Types.StudioComponentSubtype
import Amazonka.Nimble.Types.StudioComponentSummary
import Amazonka.Nimble.Types.StudioComponentType
import Amazonka.Nimble.Types.StudioEncryptionConfiguration
import Amazonka.Nimble.Types.StudioEncryptionConfigurationKeyType
import Amazonka.Nimble.Types.StudioMembership
import Amazonka.Nimble.Types.StudioPersona
import Amazonka.Nimble.Types.StudioState
import Amazonka.Nimble.Types.StudioStatusCode
import Amazonka.Nimble.Types.ValidationResult
import Amazonka.Nimble.Types.VolumeConfiguration
import Amazonka.Nimble.Types.VolumeRetentionMode
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-08-01@ of the Amazon NimbleStudio SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Nimble",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "nimble",
      Core.signingName = "nimble",
      Core.version = "2020-08-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Nimble",
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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You are not authorized to perform this operation. Check your IAM
-- policies, and ensure that you are using the correct access keys.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Another operation is in progress.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | An internal error has occurred. Please retry your request.
_InternalServerErrorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | The specified resource could not be found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Your current quota does not allow you to perform the request action. You
-- can request increases for some quotas, and other quotas cannot be
-- increased.
--
-- Please use Amazon Web Services Service Quotas to request an increase.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request throughput limit was exceeded.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | One of the parameters in the request is invalid.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
