{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppStream.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConcurrentModificationException,
    _EntitlementAlreadyExistsException,
    _EntitlementNotFoundException,
    _IncompatibleImageException,
    _InvalidAccountStatusException,
    _InvalidParameterCombinationException,
    _InvalidRoleException,
    _LimitExceededException,
    _OperationNotPermittedException,
    _RequestLimitExceededException,
    _ResourceAlreadyExistsException,
    _ResourceInUseException,
    _ResourceNotAvailableException,
    _ResourceNotFoundException,

    -- * AccessEndpointType
    AccessEndpointType (..),

    -- * Action
    Action (..),

    -- * AppVisibility
    AppVisibility (..),

    -- * ApplicationAttribute
    ApplicationAttribute (..),

    -- * AuthenticationType
    AuthenticationType (..),

    -- * CertificateBasedAuthStatus
    CertificateBasedAuthStatus (..),

    -- * FleetAttribute
    FleetAttribute (..),

    -- * FleetErrorCode
    FleetErrorCode (..),

    -- * FleetState
    FleetState (..),

    -- * FleetType
    FleetType (..),

    -- * ImageBuilderState
    ImageBuilderState (..),

    -- * ImageBuilderStateChangeReasonCode
    ImageBuilderStateChangeReasonCode (..),

    -- * ImageState
    ImageState (..),

    -- * ImageStateChangeReasonCode
    ImageStateChangeReasonCode (..),

    -- * MessageAction
    MessageAction (..),

    -- * Permission
    Permission (..),

    -- * PlatformType
    PlatformType (..),

    -- * PreferredProtocol
    PreferredProtocol (..),

    -- * SessionConnectionState
    SessionConnectionState (..),

    -- * SessionState
    SessionState (..),

    -- * StackAttribute
    StackAttribute (..),

    -- * StackErrorCode
    StackErrorCode (..),

    -- * StorageConnectorType
    StorageConnectorType (..),

    -- * StreamView
    StreamView (..),

    -- * UsageReportExecutionErrorCode
    UsageReportExecutionErrorCode (..),

    -- * UsageReportSchedule
    UsageReportSchedule (..),

    -- * UserStackAssociationErrorCode
    UserStackAssociationErrorCode (..),

    -- * VisibilityType
    VisibilityType (..),

    -- * AccessEndpoint
    AccessEndpoint (..),
    newAccessEndpoint,
    accessEndpoint_vpceId,
    accessEndpoint_endpointType,

    -- * AppBlock
    AppBlock (..),
    newAppBlock,
    appBlock_createdTime,
    appBlock_description,
    appBlock_displayName,
    appBlock_sourceS3Location,
    appBlock_name,
    appBlock_arn,
    appBlock_setupScriptDetails,

    -- * Application
    Application (..),
    newApplication,
    application_appBlockArn,
    application_arn,
    application_createdTime,
    application_description,
    application_displayName,
    application_enabled,
    application_iconS3Location,
    application_iconURL,
    application_instanceFamilies,
    application_launchParameters,
    application_launchPath,
    application_metadata,
    application_name,
    application_platforms,
    application_workingDirectory,

    -- * ApplicationFleetAssociation
    ApplicationFleetAssociation (..),
    newApplicationFleetAssociation,
    applicationFleetAssociation_fleetName,
    applicationFleetAssociation_applicationArn,

    -- * ApplicationSettings
    ApplicationSettings (..),
    newApplicationSettings,
    applicationSettings_settingsGroup,
    applicationSettings_enabled,

    -- * ApplicationSettingsResponse
    ApplicationSettingsResponse (..),
    newApplicationSettingsResponse,
    applicationSettingsResponse_enabled,
    applicationSettingsResponse_s3BucketName,
    applicationSettingsResponse_settingsGroup,

    -- * CertificateBasedAuthProperties
    CertificateBasedAuthProperties (..),
    newCertificateBasedAuthProperties,
    certificateBasedAuthProperties_certificateAuthorityArn,
    certificateBasedAuthProperties_status,

    -- * ComputeCapacity
    ComputeCapacity (..),
    newComputeCapacity,
    computeCapacity_desiredInstances,

    -- * ComputeCapacityStatus
    ComputeCapacityStatus (..),
    newComputeCapacityStatus,
    computeCapacityStatus_available,
    computeCapacityStatus_inUse,
    computeCapacityStatus_running,
    computeCapacityStatus_desired,

    -- * DirectoryConfig
    DirectoryConfig (..),
    newDirectoryConfig,
    directoryConfig_certificateBasedAuthProperties,
    directoryConfig_createdTime,
    directoryConfig_organizationalUnitDistinguishedNames,
    directoryConfig_serviceAccountCredentials,
    directoryConfig_directoryName,

    -- * DomainJoinInfo
    DomainJoinInfo (..),
    newDomainJoinInfo,
    domainJoinInfo_directoryName,
    domainJoinInfo_organizationalUnitDistinguishedName,

    -- * EntitledApplication
    EntitledApplication (..),
    newEntitledApplication,
    entitledApplication_applicationIdentifier,

    -- * Entitlement
    Entitlement (..),
    newEntitlement,
    entitlement_createdTime,
    entitlement_description,
    entitlement_lastModifiedTime,
    entitlement_name,
    entitlement_stackName,
    entitlement_appVisibility,
    entitlement_attributes,

    -- * EntitlementAttribute
    EntitlementAttribute (..),
    newEntitlementAttribute,
    entitlementAttribute_name,
    entitlementAttribute_value,

    -- * Fleet
    Fleet (..),
    newFleet,
    fleet_createdTime,
    fleet_description,
    fleet_disconnectTimeoutInSeconds,
    fleet_displayName,
    fleet_domainJoinInfo,
    fleet_enableDefaultInternetAccess,
    fleet_fleetErrors,
    fleet_fleetType,
    fleet_iamRoleArn,
    fleet_idleDisconnectTimeoutInSeconds,
    fleet_imageArn,
    fleet_imageName,
    fleet_maxConcurrentSessions,
    fleet_maxUserDurationInSeconds,
    fleet_platform,
    fleet_sessionScriptS3Location,
    fleet_streamView,
    fleet_usbDeviceFilterStrings,
    fleet_vpcConfig,
    fleet_arn,
    fleet_name,
    fleet_instanceType,
    fleet_computeCapacityStatus,
    fleet_state,

    -- * FleetError
    FleetError (..),
    newFleetError,
    fleetError_errorCode,
    fleetError_errorMessage,

    -- * Image
    Image (..),
    newImage,
    image_applications,
    image_appstreamAgentVersion,
    image_arn,
    image_baseImageArn,
    image_createdTime,
    image_description,
    image_displayName,
    image_imageBuilderName,
    image_imageBuilderSupported,
    image_imageErrors,
    image_imagePermissions,
    image_platform,
    image_publicBaseImageReleasedDate,
    image_state,
    image_stateChangeReason,
    image_visibility,
    image_name,

    -- * ImageBuilder
    ImageBuilder (..),
    newImageBuilder,
    imageBuilder_accessEndpoints,
    imageBuilder_appstreamAgentVersion,
    imageBuilder_arn,
    imageBuilder_createdTime,
    imageBuilder_description,
    imageBuilder_displayName,
    imageBuilder_domainJoinInfo,
    imageBuilder_enableDefaultInternetAccess,
    imageBuilder_iamRoleArn,
    imageBuilder_imageArn,
    imageBuilder_imageBuilderErrors,
    imageBuilder_instanceType,
    imageBuilder_networkAccessConfiguration,
    imageBuilder_platform,
    imageBuilder_state,
    imageBuilder_stateChangeReason,
    imageBuilder_vpcConfig,
    imageBuilder_name,

    -- * ImageBuilderStateChangeReason
    ImageBuilderStateChangeReason (..),
    newImageBuilderStateChangeReason,
    imageBuilderStateChangeReason_code,
    imageBuilderStateChangeReason_message,

    -- * ImagePermissions
    ImagePermissions (..),
    newImagePermissions,
    imagePermissions_allowFleet,
    imagePermissions_allowImageBuilder,

    -- * ImageStateChangeReason
    ImageStateChangeReason (..),
    newImageStateChangeReason,
    imageStateChangeReason_code,
    imageStateChangeReason_message,

    -- * LastReportGenerationExecutionError
    LastReportGenerationExecutionError (..),
    newLastReportGenerationExecutionError,
    lastReportGenerationExecutionError_errorCode,
    lastReportGenerationExecutionError_errorMessage,

    -- * NetworkAccessConfiguration
    NetworkAccessConfiguration (..),
    newNetworkAccessConfiguration,
    networkAccessConfiguration_eniId,
    networkAccessConfiguration_eniPrivateIpAddress,

    -- * ResourceError
    ResourceError (..),
    newResourceError,
    resourceError_errorCode,
    resourceError_errorMessage,
    resourceError_errorTimestamp,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_s3Bucket,
    s3Location_s3Key,

    -- * ScriptDetails
    ScriptDetails (..),
    newScriptDetails,
    scriptDetails_executableParameters,
    scriptDetails_scriptS3Location,
    scriptDetails_executablePath,
    scriptDetails_timeoutInSeconds,

    -- * ServiceAccountCredentials
    ServiceAccountCredentials (..),
    newServiceAccountCredentials,
    serviceAccountCredentials_accountName,
    serviceAccountCredentials_accountPassword,

    -- * Session
    Session (..),
    newSession,
    session_authenticationType,
    session_connectionState,
    session_maxExpirationTime,
    session_networkAccessConfiguration,
    session_startTime,
    session_id,
    session_userId,
    session_stackName,
    session_fleetName,
    session_state,

    -- * SharedImagePermissions
    SharedImagePermissions (..),
    newSharedImagePermissions,
    sharedImagePermissions_sharedAccountId,
    sharedImagePermissions_imagePermissions,

    -- * Stack
    Stack (..),
    newStack,
    stack_accessEndpoints,
    stack_applicationSettings,
    stack_arn,
    stack_createdTime,
    stack_description,
    stack_displayName,
    stack_embedHostDomains,
    stack_feedbackURL,
    stack_redirectURL,
    stack_stackErrors,
    stack_storageConnectors,
    stack_streamingExperienceSettings,
    stack_userSettings,
    stack_name,

    -- * StackError
    StackError (..),
    newStackError,
    stackError_errorCode,
    stackError_errorMessage,

    -- * StorageConnector
    StorageConnector (..),
    newStorageConnector,
    storageConnector_domains,
    storageConnector_resourceIdentifier,
    storageConnector_connectorType,

    -- * StreamingExperienceSettings
    StreamingExperienceSettings (..),
    newStreamingExperienceSettings,
    streamingExperienceSettings_preferredProtocol,

    -- * UsageReportSubscription
    UsageReportSubscription (..),
    newUsageReportSubscription,
    usageReportSubscription_lastGeneratedReportDate,
    usageReportSubscription_s3BucketName,
    usageReportSubscription_schedule,
    usageReportSubscription_subscriptionErrors,

    -- * User
    User (..),
    newUser,
    user_arn,
    user_createdTime,
    user_enabled,
    user_firstName,
    user_lastName,
    user_status,
    user_userName,
    user_authenticationType,

    -- * UserSetting
    UserSetting (..),
    newUserSetting,
    userSetting_action,
    userSetting_permission,

    -- * UserStackAssociation
    UserStackAssociation (..),
    newUserStackAssociation,
    userStackAssociation_sendEmailNotification,
    userStackAssociation_stackName,
    userStackAssociation_userName,
    userStackAssociation_authenticationType,

    -- * UserStackAssociationError
    UserStackAssociationError (..),
    newUserStackAssociationError,
    userStackAssociationError_errorCode,
    userStackAssociationError_errorMessage,
    userStackAssociationError_userStackAssociation,

    -- * VpcConfig
    VpcConfig (..),
    newVpcConfig,
    vpcConfig_securityGroupIds,
    vpcConfig_subnetIds,
  )
where

import Amazonka.AppStream.Types.AccessEndpoint
import Amazonka.AppStream.Types.AccessEndpointType
import Amazonka.AppStream.Types.Action
import Amazonka.AppStream.Types.AppBlock
import Amazonka.AppStream.Types.AppVisibility
import Amazonka.AppStream.Types.Application
import Amazonka.AppStream.Types.ApplicationAttribute
import Amazonka.AppStream.Types.ApplicationFleetAssociation
import Amazonka.AppStream.Types.ApplicationSettings
import Amazonka.AppStream.Types.ApplicationSettingsResponse
import Amazonka.AppStream.Types.AuthenticationType
import Amazonka.AppStream.Types.CertificateBasedAuthProperties
import Amazonka.AppStream.Types.CertificateBasedAuthStatus
import Amazonka.AppStream.Types.ComputeCapacity
import Amazonka.AppStream.Types.ComputeCapacityStatus
import Amazonka.AppStream.Types.DirectoryConfig
import Amazonka.AppStream.Types.DomainJoinInfo
import Amazonka.AppStream.Types.EntitledApplication
import Amazonka.AppStream.Types.Entitlement
import Amazonka.AppStream.Types.EntitlementAttribute
import Amazonka.AppStream.Types.Fleet
import Amazonka.AppStream.Types.FleetAttribute
import Amazonka.AppStream.Types.FleetError
import Amazonka.AppStream.Types.FleetErrorCode
import Amazonka.AppStream.Types.FleetState
import Amazonka.AppStream.Types.FleetType
import Amazonka.AppStream.Types.Image
import Amazonka.AppStream.Types.ImageBuilder
import Amazonka.AppStream.Types.ImageBuilderState
import Amazonka.AppStream.Types.ImageBuilderStateChangeReason
import Amazonka.AppStream.Types.ImageBuilderStateChangeReasonCode
import Amazonka.AppStream.Types.ImagePermissions
import Amazonka.AppStream.Types.ImageState
import Amazonka.AppStream.Types.ImageStateChangeReason
import Amazonka.AppStream.Types.ImageStateChangeReasonCode
import Amazonka.AppStream.Types.LastReportGenerationExecutionError
import Amazonka.AppStream.Types.MessageAction
import Amazonka.AppStream.Types.NetworkAccessConfiguration
import Amazonka.AppStream.Types.Permission
import Amazonka.AppStream.Types.PlatformType
import Amazonka.AppStream.Types.PreferredProtocol
import Amazonka.AppStream.Types.ResourceError
import Amazonka.AppStream.Types.S3Location
import Amazonka.AppStream.Types.ScriptDetails
import Amazonka.AppStream.Types.ServiceAccountCredentials
import Amazonka.AppStream.Types.Session
import Amazonka.AppStream.Types.SessionConnectionState
import Amazonka.AppStream.Types.SessionState
import Amazonka.AppStream.Types.SharedImagePermissions
import Amazonka.AppStream.Types.Stack
import Amazonka.AppStream.Types.StackAttribute
import Amazonka.AppStream.Types.StackError
import Amazonka.AppStream.Types.StackErrorCode
import Amazonka.AppStream.Types.StorageConnector
import Amazonka.AppStream.Types.StorageConnectorType
import Amazonka.AppStream.Types.StreamView
import Amazonka.AppStream.Types.StreamingExperienceSettings
import Amazonka.AppStream.Types.UsageReportExecutionErrorCode
import Amazonka.AppStream.Types.UsageReportSchedule
import Amazonka.AppStream.Types.UsageReportSubscription
import Amazonka.AppStream.Types.User
import Amazonka.AppStream.Types.UserSetting
import Amazonka.AppStream.Types.UserStackAssociation
import Amazonka.AppStream.Types.UserStackAssociationError
import Amazonka.AppStream.Types.UserStackAssociationErrorCode
import Amazonka.AppStream.Types.VisibilityType
import Amazonka.AppStream.Types.VpcConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-12-01@ of the Amazon AppStream SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "AppStream",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "appstream2",
      Core.signingName = "appstream",
      Core.version = "2016-12-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "AppStream",
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

-- | An API error occurred. Wait a few minutes and try again.
_ConcurrentModificationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The entitlement already exists.
_EntitlementAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EntitlementAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "EntitlementAlreadyExistsException"

-- | The entitlement can\'t be found.
_EntitlementNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EntitlementNotFoundException =
  Core._MatchServiceError
    defaultService
    "EntitlementNotFoundException"

-- | The image can\'t be updated because it\'s not compatible for updates.
_IncompatibleImageException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IncompatibleImageException =
  Core._MatchServiceError
    defaultService
    "IncompatibleImageException"

-- | The resource cannot be created because your AWS account is suspended.
-- For assistance, contact AWS Support.
_InvalidAccountStatusException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidAccountStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidAccountStatusException"

-- | Indicates an incorrect combination of parameters, or a missing
-- parameter.
_InvalidParameterCombinationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterCombinationException"

-- | The specified role is invalid.
_InvalidRoleException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRoleException =
  Core._MatchServiceError
    defaultService
    "InvalidRoleException"

-- | The requested limit exceeds the permitted limit for an account.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The attempted operation is not permitted.
_OperationNotPermittedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OperationNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OperationNotPermittedException"

-- | AppStream 2.0 can’t process the request right now because the Describe
-- calls from your AWS account are being throttled by Amazon EC2. Try again
-- later.
_RequestLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RequestLimitExceededException =
  Core._MatchServiceError
    defaultService
    "RequestLimitExceededException"

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The specified resource is in use.
_ResourceInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The specified resource exists and is not in use, but isn\'t available.
_ResourceNotAvailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotAvailableException =
  Core._MatchServiceError
    defaultService
    "ResourceNotAvailableException"

-- | The specified resource was not found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
