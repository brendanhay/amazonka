{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppStream.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidRoleException,
    _RequestLimitExceededException,
    _ResourceAlreadyExistsException,
    _IncompatibleImageException,
    _ConcurrentModificationException,
    _OperationNotPermittedException,
    _InvalidAccountStatusException,
    _ResourceNotFoundException,
    _InvalidParameterCombinationException,
    _ResourceNotAvailableException,
    _LimitExceededException,
    _ResourceInUseException,

    -- * AccessEndpointType
    AccessEndpointType (..),

    -- * Action
    Action (..),

    -- * AuthenticationType
    AuthenticationType (..),

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

    -- * Application
    Application (..),
    newApplication,
    application_enabled,
    application_launchPath,
    application_launchParameters,
    application_name,
    application_displayName,
    application_metadata,
    application_iconURL,

    -- * ApplicationSettings
    ApplicationSettings (..),
    newApplicationSettings,
    applicationSettings_settingsGroup,
    applicationSettings_enabled,

    -- * ApplicationSettingsResponse
    ApplicationSettingsResponse (..),
    newApplicationSettingsResponse,
    applicationSettingsResponse_enabled,
    applicationSettingsResponse_settingsGroup,
    applicationSettingsResponse_s3BucketName,

    -- * ComputeCapacity
    ComputeCapacity (..),
    newComputeCapacity,
    computeCapacity_desiredInstances,

    -- * ComputeCapacityStatus
    ComputeCapacityStatus (..),
    newComputeCapacityStatus,
    computeCapacityStatus_inUse,
    computeCapacityStatus_running,
    computeCapacityStatus_available,
    computeCapacityStatus_desired,

    -- * DirectoryConfig
    DirectoryConfig (..),
    newDirectoryConfig,
    directoryConfig_createdTime,
    directoryConfig_serviceAccountCredentials,
    directoryConfig_organizationalUnitDistinguishedNames,
    directoryConfig_directoryName,

    -- * DomainJoinInfo
    DomainJoinInfo (..),
    newDomainJoinInfo,
    domainJoinInfo_organizationalUnitDistinguishedName,
    domainJoinInfo_directoryName,

    -- * Fleet
    Fleet (..),
    newFleet,
    fleet_domainJoinInfo,
    fleet_iamRoleArn,
    fleet_disconnectTimeoutInSeconds,
    fleet_maxUserDurationInSeconds,
    fleet_createdTime,
    fleet_idleDisconnectTimeoutInSeconds,
    fleet_fleetType,
    fleet_vpcConfig,
    fleet_imageArn,
    fleet_fleetErrors,
    fleet_displayName,
    fleet_enableDefaultInternetAccess,
    fleet_imageName,
    fleet_description,
    fleet_streamView,
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
    image_state,
    image_imagePermissions,
    image_platform,
    image_publicBaseImageReleasedDate,
    image_stateChangeReason,
    image_arn,
    image_createdTime,
    image_imageBuilderSupported,
    image_visibility,
    image_imageBuilderName,
    image_imageErrors,
    image_baseImageArn,
    image_displayName,
    image_description,
    image_appstreamAgentVersion,
    image_applications,
    image_name,

    -- * ImageBuilder
    ImageBuilder (..),
    newImageBuilder,
    imageBuilder_domainJoinInfo,
    imageBuilder_iamRoleArn,
    imageBuilder_state,
    imageBuilder_platform,
    imageBuilder_networkAccessConfiguration,
    imageBuilder_stateChangeReason,
    imageBuilder_arn,
    imageBuilder_createdTime,
    imageBuilder_imageBuilderErrors,
    imageBuilder_instanceType,
    imageBuilder_accessEndpoints,
    imageBuilder_vpcConfig,
    imageBuilder_imageArn,
    imageBuilder_displayName,
    imageBuilder_enableDefaultInternetAccess,
    imageBuilder_description,
    imageBuilder_appstreamAgentVersion,
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

    -- * ServiceAccountCredentials
    ServiceAccountCredentials (..),
    newServiceAccountCredentials,
    serviceAccountCredentials_accountName,
    serviceAccountCredentials_accountPassword,

    -- * Session
    Session (..),
    newSession,
    session_networkAccessConfiguration,
    session_maxExpirationTime,
    session_startTime,
    session_authenticationType,
    session_connectionState,
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
    stack_userSettings,
    stack_applicationSettings,
    stack_feedbackURL,
    stack_arn,
    stack_createdTime,
    stack_storageConnectors,
    stack_accessEndpoints,
    stack_displayName,
    stack_stackErrors,
    stack_embedHostDomains,
    stack_description,
    stack_redirectURL,
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

    -- * UsageReportSubscription
    UsageReportSubscription (..),
    newUsageReportSubscription,
    usageReportSubscription_lastGeneratedReportDate,
    usageReportSubscription_schedule,
    usageReportSubscription_subscriptionErrors,
    usageReportSubscription_s3BucketName,

    -- * User
    User (..),
    newUser,
    user_status,
    user_enabled,
    user_lastName,
    user_arn,
    user_createdTime,
    user_userName,
    user_firstName,
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
    userStackAssociationError_userStackAssociation,
    userStackAssociationError_errorCode,
    userStackAssociationError_errorMessage,

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
import Amazonka.AppStream.Types.Application
import Amazonka.AppStream.Types.ApplicationSettings
import Amazonka.AppStream.Types.ApplicationSettingsResponse
import Amazonka.AppStream.Types.AuthenticationType
import Amazonka.AppStream.Types.ComputeCapacity
import Amazonka.AppStream.Types.ComputeCapacityStatus
import Amazonka.AppStream.Types.DirectoryConfig
import Amazonka.AppStream.Types.DomainJoinInfo
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
import Amazonka.AppStream.Types.ResourceError
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-12-01@ of the Amazon AppStream SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "AppStream",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "appstream2",
      Core._serviceSigningName = "appstream",
      Core._serviceVersion = "2016-12-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "AppStream",
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

-- | The specified role is invalid.
_InvalidRoleException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRoleException =
  Core._MatchServiceError
    defaultService
    "InvalidRoleException"

-- | AppStream 2.0 can’t process the request right now because the Describe
-- calls from your AWS account are being throttled by Amazon EC2. Try again
-- later.
_RequestLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestLimitExceededException =
  Core._MatchServiceError
    defaultService
    "RequestLimitExceededException"

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The image can\'t be updated because it\'s not compatible for updates.
_IncompatibleImageException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IncompatibleImageException =
  Core._MatchServiceError
    defaultService
    "IncompatibleImageException"

-- | An API error occurred. Wait a few minutes and try again.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The attempted operation is not permitted.
_OperationNotPermittedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OperationNotPermittedException"

-- | The resource cannot be created because your AWS account is suspended.
-- For assistance, contact AWS Support.
_InvalidAccountStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAccountStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidAccountStatusException"

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Indicates an incorrect combination of parameters, or a missing
-- parameter.
_InvalidParameterCombinationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterCombinationException"

-- | The specified resource exists and is not in use, but isn\'t available.
_ResourceNotAvailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotAvailableException =
  Core._MatchServiceError
    defaultService
    "ResourceNotAvailableException"

-- | The requested limit exceeds the permitted limit for an account.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The specified resource is in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
