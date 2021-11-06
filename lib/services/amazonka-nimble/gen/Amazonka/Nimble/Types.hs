{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Nimble.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerErrorException,
    _ResourceNotFoundException,

    -- * LaunchProfilePersona
    LaunchProfilePersona (..),

    -- * LaunchProfilePlatform
    LaunchProfilePlatform (..),

    -- * LaunchProfileState
    LaunchProfileState (..),

    -- * LaunchProfileStatusCode
    LaunchProfileStatusCode (..),

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

    -- * ActiveDirectoryComputerAttribute
    ActiveDirectoryComputerAttribute (..),
    newActiveDirectoryComputerAttribute,
    activeDirectoryComputerAttribute_value,
    activeDirectoryComputerAttribute_name,

    -- * ActiveDirectoryConfiguration
    ActiveDirectoryConfiguration (..),
    newActiveDirectoryConfiguration,
    activeDirectoryConfiguration_directoryId,
    activeDirectoryConfiguration_computerAttributes,
    activeDirectoryConfiguration_organizationalUnitDistinguishedName,

    -- * ComputeFarmConfiguration
    ComputeFarmConfiguration (..),
    newComputeFarmConfiguration,
    computeFarmConfiguration_activeDirectoryUser,
    computeFarmConfiguration_endpoint,

    -- * Eula
    Eula (..),
    newEula,
    eula_createdAt,
    eula_eulaId,
    eula_content,
    eula_name,
    eula_updatedAt,

    -- * EulaAcceptance
    EulaAcceptance (..),
    newEulaAcceptance,
    eulaAcceptance_accepteeId,
    eulaAcceptance_eulaId,
    eulaAcceptance_acceptedAt,
    eulaAcceptance_acceptedBy,
    eulaAcceptance_eulaAcceptanceId,

    -- * LaunchProfile
    LaunchProfile (..),
    newLaunchProfile,
    launchProfile_state,
    launchProfile_arn,
    launchProfile_createdAt,
    launchProfile_createdBy,
    launchProfile_launchProfileId,
    launchProfile_updatedBy,
    launchProfile_launchProfileProtocolVersions,
    launchProfile_ec2SubnetIds,
    launchProfile_streamConfiguration,
    launchProfile_name,
    launchProfile_statusMessage,
    launchProfile_updatedAt,
    launchProfile_description,
    launchProfile_tags,
    launchProfile_statusCode,
    launchProfile_studioComponentIds,

    -- * LaunchProfileInitialization
    LaunchProfileInitialization (..),
    newLaunchProfileInitialization,
    launchProfileInitialization_platform,
    launchProfileInitialization_activeDirectory,
    launchProfileInitialization_launchPurpose,
    launchProfileInitialization_launchProfileId,
    launchProfileInitialization_ec2SecurityGroupIds,
    launchProfileInitialization_name,
    launchProfileInitialization_launchProfileProtocolVersion,
    launchProfileInitialization_userInitializationScripts,
    launchProfileInitialization_systemInitializationScripts,

    -- * LaunchProfileInitializationActiveDirectory
    LaunchProfileInitializationActiveDirectory (..),
    newLaunchProfileInitializationActiveDirectory,
    launchProfileInitializationActiveDirectory_directoryId,
    launchProfileInitializationActiveDirectory_studioComponentId,
    launchProfileInitializationActiveDirectory_studioComponentName,
    launchProfileInitializationActiveDirectory_dnsIpAddresses,
    launchProfileInitializationActiveDirectory_computerAttributes,
    launchProfileInitializationActiveDirectory_organizationalUnitDistinguishedName,
    launchProfileInitializationActiveDirectory_directoryName,

    -- * LaunchProfileInitializationScript
    LaunchProfileInitializationScript (..),
    newLaunchProfileInitializationScript,
    launchProfileInitializationScript_script,
    launchProfileInitializationScript_studioComponentId,
    launchProfileInitializationScript_studioComponentName,

    -- * LaunchProfileMembership
    LaunchProfileMembership (..),
    newLaunchProfileMembership,
    launchProfileMembership_identityStoreId,
    launchProfileMembership_principalId,
    launchProfileMembership_persona,

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
    scriptParameterKeyValue_value,
    scriptParameterKeyValue_key,

    -- * SharedFileSystemConfiguration
    SharedFileSystemConfiguration (..),
    newSharedFileSystemConfiguration,
    sharedFileSystemConfiguration_fileSystemId,
    sharedFileSystemConfiguration_windowsMountDrive,
    sharedFileSystemConfiguration_linuxMountPoint,
    sharedFileSystemConfiguration_shareName,
    sharedFileSystemConfiguration_endpoint,

    -- * StreamConfiguration
    StreamConfiguration (..),
    newStreamConfiguration,
    streamConfiguration_streamingImageIds,
    streamConfiguration_maxSessionLengthInMinutes,
    streamConfiguration_clipboardMode,
    streamConfiguration_ec2InstanceTypes,

    -- * StreamConfigurationCreate
    StreamConfigurationCreate (..),
    newStreamConfigurationCreate,
    streamConfigurationCreate_maxSessionLengthInMinutes,
    streamConfigurationCreate_clipboardMode,
    streamConfigurationCreate_streamingImageIds,
    streamConfigurationCreate_ec2InstanceTypes,

    -- * StreamingImage
    StreamingImage (..),
    newStreamingImage,
    streamingImage_state,
    streamingImage_platform,
    streamingImage_arn,
    streamingImage_streamingImageId,
    streamingImage_ec2ImageId,
    streamingImage_owner,
    streamingImage_name,
    streamingImage_encryptionConfiguration,
    streamingImage_statusMessage,
    streamingImage_eulaIds,
    streamingImage_description,
    streamingImage_tags,
    streamingImage_statusCode,

    -- * StreamingImageEncryptionConfiguration
    StreamingImageEncryptionConfiguration (..),
    newStreamingImageEncryptionConfiguration,
    streamingImageEncryptionConfiguration_keyArn,
    streamingImageEncryptionConfiguration_keyType,

    -- * StreamingSession
    StreamingSession (..),
    newStreamingSession,
    streamingSession_ownedBy,
    streamingSession_state,
    streamingSession_arn,
    streamingSession_createdAt,
    streamingSession_ec2InstanceType,
    streamingSession_createdBy,
    streamingSession_launchProfileId,
    streamingSession_streamingImageId,
    streamingSession_updatedBy,
    streamingSession_terminateAt,
    streamingSession_statusMessage,
    streamingSession_updatedAt,
    streamingSession_sessionId,
    streamingSession_tags,
    streamingSession_statusCode,

    -- * StreamingSessionStream
    StreamingSessionStream (..),
    newStreamingSessionStream,
    streamingSessionStream_ownedBy,
    streamingSessionStream_state,
    streamingSessionStream_createdAt,
    streamingSessionStream_expiresAt,
    streamingSessionStream_url,
    streamingSessionStream_createdBy,
    streamingSessionStream_streamId,
    streamingSessionStream_statusCode,

    -- * Studio
    Studio (..),
    newStudio,
    studio_studioEncryptionConfiguration,
    studio_state,
    studio_studioName,
    studio_arn,
    studio_createdAt,
    studio_studioId,
    studio_userRoleArn,
    studio_ssoClientId,
    studio_homeRegion,
    studio_statusMessage,
    studio_displayName,
    studio_updatedAt,
    studio_studioUrl,
    studio_adminRoleArn,
    studio_tags,
    studio_statusCode,

    -- * StudioComponent
    StudioComponent (..),
    newStudioComponent,
    studioComponent_initializationScripts,
    studioComponent_state,
    studioComponent_studioComponentId,
    studioComponent_arn,
    studioComponent_createdAt,
    studioComponent_createdBy,
    studioComponent_ec2SecurityGroupIds,
    studioComponent_updatedBy,
    studioComponent_subtype,
    studioComponent_name,
    studioComponent_statusMessage,
    studioComponent_scriptParameters,
    studioComponent_updatedAt,
    studioComponent_type,
    studioComponent_configuration,
    studioComponent_description,
    studioComponent_tags,
    studioComponent_statusCode,

    -- * StudioComponentConfiguration
    StudioComponentConfiguration (..),
    newStudioComponentConfiguration,
    studioComponentConfiguration_activeDirectoryConfiguration,
    studioComponentConfiguration_licenseServiceConfiguration,
    studioComponentConfiguration_sharedFileSystemConfiguration,
    studioComponentConfiguration_computeFarmConfiguration,

    -- * StudioComponentInitializationScript
    StudioComponentInitializationScript (..),
    newStudioComponentInitializationScript,
    studioComponentInitializationScript_script,
    studioComponentInitializationScript_platform,
    studioComponentInitializationScript_runContext,
    studioComponentInitializationScript_launchProfileProtocolVersion,

    -- * StudioComponentSummary
    StudioComponentSummary (..),
    newStudioComponentSummary,
    studioComponentSummary_studioComponentId,
    studioComponentSummary_createdAt,
    studioComponentSummary_createdBy,
    studioComponentSummary_updatedBy,
    studioComponentSummary_subtype,
    studioComponentSummary_name,
    studioComponentSummary_updatedAt,
    studioComponentSummary_type,
    studioComponentSummary_description,

    -- * StudioEncryptionConfiguration
    StudioEncryptionConfiguration (..),
    newStudioEncryptionConfiguration,
    studioEncryptionConfiguration_keyArn,
    studioEncryptionConfiguration_keyType,

    -- * StudioMembership
    StudioMembership (..),
    newStudioMembership,
    studioMembership_identityStoreId,
    studioMembership_principalId,
    studioMembership_persona,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types.ActiveDirectoryComputerAttribute
import Amazonka.Nimble.Types.ActiveDirectoryConfiguration
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
import Amazonka.Nimble.Types.LicenseServiceConfiguration
import Amazonka.Nimble.Types.NewLaunchProfileMember
import Amazonka.Nimble.Types.NewStudioMember
import Amazonka.Nimble.Types.ScriptParameterKeyValue
import Amazonka.Nimble.Types.SharedFileSystemConfiguration
import Amazonka.Nimble.Types.StreamConfiguration
import Amazonka.Nimble.Types.StreamConfigurationCreate
import Amazonka.Nimble.Types.StreamingClipboardMode
import Amazonka.Nimble.Types.StreamingImage
import Amazonka.Nimble.Types.StreamingImageEncryptionConfiguration
import Amazonka.Nimble.Types.StreamingImageEncryptionConfigurationKeyType
import Amazonka.Nimble.Types.StreamingImageState
import Amazonka.Nimble.Types.StreamingImageStatusCode
import Amazonka.Nimble.Types.StreamingInstanceType
import Amazonka.Nimble.Types.StreamingSession
import Amazonka.Nimble.Types.StreamingSessionState
import Amazonka.Nimble.Types.StreamingSessionStatusCode
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
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-08-01@ of the Amazon NimbleStudio SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Nimble",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "nimble",
      Core._serviceSigningName = "nimble",
      Core._serviceVersion = "2020-08-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Nimble",
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
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
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- |
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- |
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- |
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- |
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- |
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- |
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- |
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
