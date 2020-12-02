{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types
  ( -- * Service Configuration
    appStream,

    -- * Errors

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
    AccessEndpoint,
    accessEndpoint,
    aeVPCeId,
    aeEndpointType,

    -- * Application
    Application,
    application,
    appEnabled,
    appLaunchPath,
    appLaunchParameters,
    appName,
    appDisplayName,
    appMetadata,
    appIconURL,

    -- * ApplicationSettings
    ApplicationSettings,
    applicationSettings,
    aSettingsGroup,
    aEnabled,

    -- * ApplicationSettingsResponse
    ApplicationSettingsResponse,
    applicationSettingsResponse,
    asEnabled,
    asSettingsGroup,
    asS3BucketName,

    -- * ComputeCapacity
    ComputeCapacity,
    computeCapacity,
    ccDesiredInstances,

    -- * ComputeCapacityStatus
    ComputeCapacityStatus,
    computeCapacityStatus,
    ccsInUse,
    ccsRunning,
    ccsAvailable,
    ccsDesired,

    -- * DirectoryConfig
    DirectoryConfig,
    directoryConfig,
    dcCreatedTime,
    dcServiceAccountCredentials,
    dcOrganizationalUnitDistinguishedNames,
    dcDirectoryName,

    -- * DomainJoinInfo
    DomainJoinInfo,
    domainJoinInfo,
    djiOrganizationalUnitDistinguishedName,
    djiDirectoryName,

    -- * Fleet
    Fleet,
    fleet,
    fDomainJoinInfo,
    fIAMRoleARN,
    fDisconnectTimeoutInSeconds,
    fMaxUserDurationInSeconds,
    fCreatedTime,
    fIdleDisconnectTimeoutInSeconds,
    fFleetType,
    fVPCConfig,
    fImageARN,
    fFleetErrors,
    fDisplayName,
    fEnableDefaultInternetAccess,
    fImageName,
    fDescription,
    fStreamView,
    fARN,
    fName,
    fInstanceType,
    fComputeCapacityStatus,
    fState,

    -- * FleetError
    FleetError,
    fleetError,
    feErrorCode,
    feErrorMessage,

    -- * Image
    Image,
    image,
    iState,
    iImagePermissions,
    iPlatform,
    iPublicBaseImageReleasedDate,
    iStateChangeReason,
    iARN,
    iCreatedTime,
    iImageBuilderSupported,
    iVisibility,
    iImageBuilderName,
    iBaseImageARN,
    iDisplayName,
    iDescription,
    iAppstreamAgentVersion,
    iApplications,
    iName,

    -- * ImageBuilder
    ImageBuilder,
    imageBuilder,
    ibDomainJoinInfo,
    ibIAMRoleARN,
    ibState,
    ibPlatform,
    ibNetworkAccessConfiguration,
    ibStateChangeReason,
    ibARN,
    ibCreatedTime,
    ibImageBuilderErrors,
    ibInstanceType,
    ibAccessEndpoints,
    ibVPCConfig,
    ibImageARN,
    ibDisplayName,
    ibEnableDefaultInternetAccess,
    ibDescription,
    ibAppstreamAgentVersion,
    ibName,

    -- * ImageBuilderStateChangeReason
    ImageBuilderStateChangeReason,
    imageBuilderStateChangeReason,
    ibscrCode,
    ibscrMessage,

    -- * ImagePermissions
    ImagePermissions,
    imagePermissions,
    ipAllowFleet,
    ipAllowImageBuilder,

    -- * ImageStateChangeReason
    ImageStateChangeReason,
    imageStateChangeReason,
    iscrCode,
    iscrMessage,

    -- * LastReportGenerationExecutionError
    LastReportGenerationExecutionError,
    lastReportGenerationExecutionError,
    lrgeeErrorCode,
    lrgeeErrorMessage,

    -- * NetworkAccessConfiguration
    NetworkAccessConfiguration,
    networkAccessConfiguration,
    nacEniId,
    nacEniPrivateIPAddress,

    -- * ResourceError
    ResourceError,
    resourceError,
    reErrorCode,
    reErrorMessage,
    reErrorTimestamp,

    -- * ServiceAccountCredentials
    ServiceAccountCredentials,
    serviceAccountCredentials,
    sacAccountName,
    sacAccountPassword,

    -- * Session
    Session,
    session,
    sNetworkAccessConfiguration,
    sMaxExpirationTime,
    sStartTime,
    sAuthenticationType,
    sConnectionState,
    sId,
    sUserId,
    sStackName,
    sFleetName,
    sState,

    -- * SharedImagePermissions
    SharedImagePermissions,
    sharedImagePermissions,
    sipSharedAccountId,
    sipImagePermissions,

    -- * Stack
    Stack,
    stack,
    sUserSettings,
    sApplicationSettings,
    sFeedbackURL,
    sARN,
    sCreatedTime,
    sStorageConnectors,
    sAccessEndpoints,
    sDisplayName,
    sStackErrors,
    sEmbedHostDomains,
    sDescription,
    sRedirectURL,
    sName,

    -- * StackError
    StackError,
    stackError,
    seErrorCode,
    seErrorMessage,

    -- * StorageConnector
    StorageConnector,
    storageConnector,
    scDomains,
    scResourceIdentifier,
    scConnectorType,

    -- * UsageReportSubscription
    UsageReportSubscription,
    usageReportSubscription,
    ursLastGeneratedReportDate,
    ursSchedule,
    ursSubscriptionErrors,
    ursS3BucketName,

    -- * User
    User,
    user,
    uStatus,
    uEnabled,
    uLastName,
    uARN,
    uCreatedTime,
    uUserName,
    uFirstName,
    uAuthenticationType,

    -- * UserSetting
    UserSetting,
    userSetting,
    usAction,
    usPermission,

    -- * UserStackAssociation
    UserStackAssociation,
    userStackAssociation,
    usaSendEmailNotification,
    usaStackName,
    usaUserName,
    usaAuthenticationType,

    -- * UserStackAssociationError
    UserStackAssociationError,
    userStackAssociationError,
    usaeUserStackAssociation,
    usaeErrorCode,
    usaeErrorMessage,

    -- * VPCConfig
    VPCConfig,
    vpcConfig,
    vcSecurityGroupIds,
    vcSubnetIds,
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
import Network.AWS.AppStream.Types.VPCConfig
import Network.AWS.AppStream.Types.VisibilityType
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-12-01@ of the Amazon AppStream SDK configuration.
appStream :: Service
appStream =
  Service
    { _svcAbbrev = "AppStream",
      _svcSigner = v4,
      _svcPrefix = "appstream2",
      _svcVersion = "2016-12-01",
      _svcEndpoint = defaultEndpoint appStream,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "AppStream",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
