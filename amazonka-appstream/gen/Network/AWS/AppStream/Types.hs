{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceNotAvailableException,
    _IncompatibleImageException,
    _InvalidParameterCombinationException,
    _ResourceAlreadyExistsException,
    _InvalidAccountStatusException,
    _OperationNotPermittedException,
    _ConcurrentModificationException,
    _InvalidRoleException,
    _ResourceInUseException,
    _LimitExceededException,
    _ResourceNotFoundException,
    _RequestLimitExceededException,

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
    application_iconURL,
    application_launchPath,
    application_enabled,
    application_metadata,
    application_launchParameters,
    application_name,
    application_displayName,

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
    computeCapacityStatus_running,
    computeCapacityStatus_available,
    computeCapacityStatus_inUse,
    computeCapacityStatus_desired,

    -- * DirectoryConfig
    DirectoryConfig (..),
    newDirectoryConfig,
    directoryConfig_serviceAccountCredentials,
    directoryConfig_createdTime,
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
    fleet_maxUserDurationInSeconds,
    fleet_disconnectTimeoutInSeconds,
    fleet_vpcConfig,
    fleet_iamRoleArn,
    fleet_domainJoinInfo,
    fleet_fleetType,
    fleet_idleDisconnectTimeoutInSeconds,
    fleet_imageName,
    fleet_createdTime,
    fleet_streamView,
    fleet_description,
    fleet_displayName,
    fleet_enableDefaultInternetAccess,
    fleet_fleetErrors,
    fleet_imageArn,
    fleet_arn,
    fleet_name,
    fleet_instanceType,
    fleet_computeCapacityStatus,
    fleet_state,

    -- * FleetError
    FleetError (..),
    newFleetError,
    fleetError_errorMessage,
    fleetError_errorCode,

    -- * Image
    Image (..),
    newImage,
    image_imagePermissions,
    image_platform,
    image_imageBuilderName,
    image_arn,
    image_stateChangeReason,
    image_createdTime,
    image_state,
    image_baseImageArn,
    image_applications,
    image_visibility,
    image_appstreamAgentVersion,
    image_description,
    image_imageBuilderSupported,
    image_displayName,
    image_publicBaseImageReleasedDate,
    image_name,

    -- * ImageBuilder
    ImageBuilder (..),
    newImageBuilder,
    imageBuilder_platform,
    imageBuilder_vpcConfig,
    imageBuilder_iamRoleArn,
    imageBuilder_accessEndpoints,
    imageBuilder_domainJoinInfo,
    imageBuilder_instanceType,
    imageBuilder_arn,
    imageBuilder_stateChangeReason,
    imageBuilder_createdTime,
    imageBuilder_networkAccessConfiguration,
    imageBuilder_state,
    imageBuilder_appstreamAgentVersion,
    imageBuilder_description,
    imageBuilder_displayName,
    imageBuilder_enableDefaultInternetAccess,
    imageBuilder_imageBuilderErrors,
    imageBuilder_imageArn,
    imageBuilder_name,

    -- * ImageBuilderStateChangeReason
    ImageBuilderStateChangeReason (..),
    newImageBuilderStateChangeReason,
    imageBuilderStateChangeReason_message,
    imageBuilderStateChangeReason_code,

    -- * ImagePermissions
    ImagePermissions (..),
    newImagePermissions,
    imagePermissions_allowImageBuilder,
    imagePermissions_allowFleet,

    -- * ImageStateChangeReason
    ImageStateChangeReason (..),
    newImageStateChangeReason,
    imageStateChangeReason_message,
    imageStateChangeReason_code,

    -- * LastReportGenerationExecutionError
    LastReportGenerationExecutionError (..),
    newLastReportGenerationExecutionError,
    lastReportGenerationExecutionError_errorMessage,
    lastReportGenerationExecutionError_errorCode,

    -- * NetworkAccessConfiguration
    NetworkAccessConfiguration (..),
    newNetworkAccessConfiguration,
    networkAccessConfiguration_eniId,
    networkAccessConfiguration_eniPrivateIpAddress,

    -- * ResourceError
    ResourceError (..),
    newResourceError,
    resourceError_errorTimestamp,
    resourceError_errorMessage,
    resourceError_errorCode,

    -- * ServiceAccountCredentials
    ServiceAccountCredentials (..),
    newServiceAccountCredentials,
    serviceAccountCredentials_accountName,
    serviceAccountCredentials_accountPassword,

    -- * Session
    Session (..),
    newSession,
    session_connectionState,
    session_startTime,
    session_networkAccessConfiguration,
    session_authenticationType,
    session_maxExpirationTime,
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
    stack_userSettings,
    stack_redirectURL,
    stack_arn,
    stack_createdTime,
    stack_applicationSettings,
    stack_storageConnectors,
    stack_description,
    stack_embedHostDomains,
    stack_displayName,
    stack_stackErrors,
    stack_feedbackURL,
    stack_name,

    -- * StackError
    StackError (..),
    newStackError,
    stackError_errorMessage,
    stackError_errorCode,

    -- * StorageConnector
    StorageConnector (..),
    newStorageConnector,
    storageConnector_domains,
    storageConnector_resourceIdentifier,
    storageConnector_connectorType,

    -- * UsageReportSubscription
    UsageReportSubscription (..),
    newUsageReportSubscription,
    usageReportSubscription_subscriptionErrors,
    usageReportSubscription_lastGeneratedReportDate,
    usageReportSubscription_s3BucketName,
    usageReportSubscription_schedule,

    -- * User
    User (..),
    newUser,
    user_status,
    user_arn,
    user_enabled,
    user_createdTime,
    user_userName,
    user_firstName,
    user_lastName,
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
    userStackAssociationError_errorMessage,
    userStackAssociationError_errorCode,

    -- * VpcConfig
    VpcConfig (..),
    newVpcConfig,
    vpcConfig_securityGroupIds,
    vpcConfig_subnetIds,
  )
where

import Network.AWS.AppStream.Types.AccessEndpoint
import Network.AWS.AppStream.Types.AccessEndpointType
import Network.AWS.AppStream.Types.Action
import Network.AWS.AppStream.Types.Application
import Network.AWS.AppStream.Types.ApplicationSettings
import Network.AWS.AppStream.Types.ApplicationSettingsResponse
import Network.AWS.AppStream.Types.AuthenticationType
import Network.AWS.AppStream.Types.ComputeCapacity
import Network.AWS.AppStream.Types.ComputeCapacityStatus
import Network.AWS.AppStream.Types.DirectoryConfig
import Network.AWS.AppStream.Types.DomainJoinInfo
import Network.AWS.AppStream.Types.Fleet
import Network.AWS.AppStream.Types.FleetAttribute
import Network.AWS.AppStream.Types.FleetError
import Network.AWS.AppStream.Types.FleetErrorCode
import Network.AWS.AppStream.Types.FleetState
import Network.AWS.AppStream.Types.FleetType
import Network.AWS.AppStream.Types.Image
import Network.AWS.AppStream.Types.ImageBuilder
import Network.AWS.AppStream.Types.ImageBuilderState
import Network.AWS.AppStream.Types.ImageBuilderStateChangeReason
import Network.AWS.AppStream.Types.ImageBuilderStateChangeReasonCode
import Network.AWS.AppStream.Types.ImagePermissions
import Network.AWS.AppStream.Types.ImageState
import Network.AWS.AppStream.Types.ImageStateChangeReason
import Network.AWS.AppStream.Types.ImageStateChangeReasonCode
import Network.AWS.AppStream.Types.LastReportGenerationExecutionError
import Network.AWS.AppStream.Types.MessageAction
import Network.AWS.AppStream.Types.NetworkAccessConfiguration
import Network.AWS.AppStream.Types.Permission
import Network.AWS.AppStream.Types.PlatformType
import Network.AWS.AppStream.Types.ResourceError
import Network.AWS.AppStream.Types.ServiceAccountCredentials
import Network.AWS.AppStream.Types.Session
import Network.AWS.AppStream.Types.SessionConnectionState
import Network.AWS.AppStream.Types.SessionState
import Network.AWS.AppStream.Types.SharedImagePermissions
import Network.AWS.AppStream.Types.Stack
import Network.AWS.AppStream.Types.StackAttribute
import Network.AWS.AppStream.Types.StackError
import Network.AWS.AppStream.Types.StackErrorCode
import Network.AWS.AppStream.Types.StorageConnector
import Network.AWS.AppStream.Types.StorageConnectorType
import Network.AWS.AppStream.Types.StreamView
import Network.AWS.AppStream.Types.UsageReportExecutionErrorCode
import Network.AWS.AppStream.Types.UsageReportSchedule
import Network.AWS.AppStream.Types.UsageReportSubscription
import Network.AWS.AppStream.Types.User
import Network.AWS.AppStream.Types.UserSetting
import Network.AWS.AppStream.Types.UserStackAssociation
import Network.AWS.AppStream.Types.UserStackAssociationError
import Network.AWS.AppStream.Types.UserStackAssociationErrorCode
import Network.AWS.AppStream.Types.VisibilityType
import Network.AWS.AppStream.Types.VpcConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-12-01@ of the Amazon AppStream SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "AppStream",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "appstream2",
      Prelude._svcVersion = "2016-12-01",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "AppStream",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified resource exists and is not in use, but isn\'t available.
_ResourceNotAvailableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotAvailableException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotAvailableException"

-- | The image does not support storage connectors.
_IncompatibleImageException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IncompatibleImageException =
  Prelude._MatchServiceError
    defaultService
    "IncompatibleImageException"

-- | Indicates an incorrect combination of parameters, or a missing
-- parameter.
_InvalidParameterCombinationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterCombinationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterCombinationException"

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The resource cannot be created because your AWS account is suspended.
-- For assistance, contact AWS Support.
_InvalidAccountStatusException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidAccountStatusException =
  Prelude._MatchServiceError
    defaultService
    "InvalidAccountStatusException"

-- | The attempted operation is not permitted.
_OperationNotPermittedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OperationNotPermittedException =
  Prelude._MatchServiceError
    defaultService
    "OperationNotPermittedException"

-- | An API error occurred. Wait a few minutes and try again.
_ConcurrentModificationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConcurrentModificationException =
  Prelude._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The specified role is invalid.
_InvalidRoleException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRoleException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRoleException"

-- | The specified resource is in use.
_ResourceInUseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceInUseException =
  Prelude._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The requested limit exceeds the permitted limit for an account.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The specified resource was not found.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | AppStream 2.0 can’t process the request right now because the Describe
-- calls from your AWS account are being throttled by Amazon EC2. Try again
-- later.
_RequestLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RequestLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "RequestLimitExceededException"
