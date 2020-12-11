-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types
  ( -- * Service configuration
    appStreamService,

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
    AccessEndpoint (..),
    mkAccessEndpoint,
    aeVPCeId,
    aeEndpointType,

    -- * Application
    Application (..),
    mkApplication,
    appEnabled,
    appLaunchPath,
    appLaunchParameters,
    appName,
    appDisplayName,
    appMetadata,
    appIconURL,

    -- * ApplicationSettings
    ApplicationSettings (..),
    mkApplicationSettings,
    aSettingsGroup,
    aEnabled,

    -- * ApplicationSettingsResponse
    ApplicationSettingsResponse (..),
    mkApplicationSettingsResponse,
    asEnabled,
    asSettingsGroup,
    asS3BucketName,

    -- * ComputeCapacity
    ComputeCapacity (..),
    mkComputeCapacity,
    ccDesiredInstances,

    -- * ComputeCapacityStatus
    ComputeCapacityStatus (..),
    mkComputeCapacityStatus,
    ccsInUse,
    ccsRunning,
    ccsAvailable,
    ccsDesired,

    -- * DirectoryConfig
    DirectoryConfig (..),
    mkDirectoryConfig,
    dcCreatedTime,
    dcServiceAccountCredentials,
    dcOrganizationalUnitDistinguishedNames,
    dcDirectoryName,

    -- * DomainJoinInfo
    DomainJoinInfo (..),
    mkDomainJoinInfo,
    djiOrganizationalUnitDistinguishedName,
    djiDirectoryName,

    -- * Fleet
    Fleet (..),
    mkFleet,
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
    FleetError (..),
    mkFleetError,
    feErrorCode,
    feErrorMessage,

    -- * Image
    Image (..),
    mkImage,
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
    ImageBuilder (..),
    mkImageBuilder,
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
    ImageBuilderStateChangeReason (..),
    mkImageBuilderStateChangeReason,
    ibscrCode,
    ibscrMessage,

    -- * ImagePermissions
    ImagePermissions (..),
    mkImagePermissions,
    ipAllowFleet,
    ipAllowImageBuilder,

    -- * ImageStateChangeReason
    ImageStateChangeReason (..),
    mkImageStateChangeReason,
    iscrCode,
    iscrMessage,

    -- * LastReportGenerationExecutionError
    LastReportGenerationExecutionError (..),
    mkLastReportGenerationExecutionError,
    lrgeeErrorCode,
    lrgeeErrorMessage,

    -- * NetworkAccessConfiguration
    NetworkAccessConfiguration (..),
    mkNetworkAccessConfiguration,
    nacEniId,
    nacEniPrivateIPAddress,

    -- * ResourceError
    ResourceError (..),
    mkResourceError,
    reErrorCode,
    reErrorMessage,
    reErrorTimestamp,

    -- * ServiceAccountCredentials
    ServiceAccountCredentials (..),
    mkServiceAccountCredentials,
    sacAccountName,
    sacAccountPassword,

    -- * Session
    Session (..),
    mkSession,
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
    SharedImagePermissions (..),
    mkSharedImagePermissions,
    sipSharedAccountId,
    sipImagePermissions,

    -- * Stack
    Stack (..),
    mkStack,
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
    StackError (..),
    mkStackError,
    seErrorCode,
    seErrorMessage,

    -- * StorageConnector
    StorageConnector (..),
    mkStorageConnector,
    scDomains,
    scResourceIdentifier,
    scConnectorType,

    -- * UsageReportSubscription
    UsageReportSubscription (..),
    mkUsageReportSubscription,
    ursLastGeneratedReportDate,
    ursSchedule,
    ursSubscriptionErrors,
    ursS3BucketName,

    -- * User
    User (..),
    mkUser,
    uStatus,
    uEnabled,
    uLastName,
    uARN,
    uCreatedTime,
    uUserName,
    uFirstName,
    uAuthenticationType,

    -- * UserSetting
    UserSetting (..),
    mkUserSetting,
    usAction,
    usPermission,

    -- * UserStackAssociation
    UserStackAssociation (..),
    mkUserStackAssociation,
    usaSendEmailNotification,
    usaStackName,
    usaUserName,
    usaAuthenticationType,

    -- * UserStackAssociationError
    UserStackAssociationError (..),
    mkUserStackAssociationError,
    usaeUserStackAssociation,
    usaeErrorCode,
    usaeErrorMessage,

    -- * VPCConfig
    VPCConfig (..),
    mkVPCConfig,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-12-01@ of the Amazon AppStream SDK configuration.
appStreamService :: Lude.Service
appStreamService =
  Lude.Service
    { Lude._svcAbbrev = "AppStream",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "appstream2",
      Lude._svcVersion = "2016-12-01",
      Lude._svcEndpoint = Lude.defaultEndpoint appStreamService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "AppStream",
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
