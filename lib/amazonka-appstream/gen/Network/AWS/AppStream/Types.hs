-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _InvalidRoleException
    , _RequestLimitExceededException
    , _ResourceAlreadyExistsException
    , _IncompatibleImageException
    , _ConcurrentModificationException
    , _OperationNotPermittedException
    , _InvalidAccountStatusException
    , _ResourceNotFoundException
    , _InvalidParameterCombinationException
    , _ResourceNotAvailableException
    , _LimitExceededException
    , _ResourceInUseException

    -- * ApplicationSettingsResponse
    , ApplicationSettingsResponse (..)
    , mkApplicationSettingsResponse
    , asrEnabled
    , asrS3BucketName
    , asrSettingsGroup

    -- * DomainJoinInfo
    , DomainJoinInfo (..)
    , mkDomainJoinInfo
    , djiDirectoryName
    , djiOrganizationalUnitDistinguishedName

    -- * UsageReportExecutionErrorCode
    , UsageReportExecutionErrorCode (..)

    -- * ImagePermissions
    , ImagePermissions (..)
    , mkImagePermissions
    , ipAllowFleet
    , ipAllowImageBuilder

    -- * AccessEndpointType
    , AccessEndpointType (..)

    -- * ImageStateChangeReasonCode
    , ImageStateChangeReasonCode (..)

    -- * ApplicationSettings
    , ApplicationSettings (..)
    , mkApplicationSettings
    , asEnabled
    , asSettingsGroup

    -- * UserSetting
    , UserSetting (..)
    , mkUserSetting
    , usAction
    , usPermission

    -- * RegionName
    , RegionName (..)

    -- * Image
    , Image (..)
    , mkImage
    , iName
    , iApplications
    , iAppstreamAgentVersion
    , iArn
    , iBaseImageArn
    , iCreatedTime
    , iDescription
    , iDisplayName
    , iImageBuilderName
    , iImageBuilderSupported
    , iImagePermissions
    , iPlatform
    , iPublicBaseImageReleasedDate
    , iState
    , iStateChangeReason
    , iVisibility

    -- * Application
    , Application (..)
    , mkApplication
    , aDisplayName
    , aEnabled
    , aIconURL
    , aLaunchParameters
    , aLaunchPath
    , aMetadata
    , aName

    -- * FleetAttribute
    , FleetAttribute (..)

    -- * FeedbackURL
    , FeedbackURL (..)

    -- * StackAttribute
    , StackAttribute (..)

    -- * NetworkAccessConfiguration
    , NetworkAccessConfiguration (..)
    , mkNetworkAccessConfiguration
    , nacEniId
    , nacEniPrivateIpAddress

    -- * StorageConnectorType
    , StorageConnectorType (..)

    -- * Arn
    , Arn (..)

    -- * MessageAction
    , MessageAction (..)

    -- * Domain
    , Domain (..)

    -- * UserStackAssociationErrorCode
    , UserStackAssociationErrorCode (..)

    -- * FleetType
    , FleetType (..)

    -- * Action
    , Action (..)

    -- * AccountName
    , AccountName (..)

    -- * UserStackAssociation
    , UserStackAssociation (..)
    , mkUserStackAssociation
    , usaStackName
    , usaUserName
    , usaAuthenticationType
    , usaSendEmailNotification

    -- * ImageBuilder
    , ImageBuilder (..)
    , mkImageBuilder
    , ibName
    , ibAccessEndpoints
    , ibAppstreamAgentVersion
    , ibArn
    , ibCreatedTime
    , ibDescription
    , ibDisplayName
    , ibDomainJoinInfo
    , ibEnableDefaultInternetAccess
    , ibIamRoleArn
    , ibImageArn
    , ibImageBuilderErrors
    , ibInstanceType
    , ibNetworkAccessConfiguration
    , ibPlatform
    , ibState
    , ibStateChangeReason
    , ibVpcConfig

    -- * Username
    , Username (..)

    -- * UserStackAssociationError
    , UserStackAssociationError (..)
    , mkUserStackAssociationError
    , usaeErrorCode
    , usaeErrorMessage
    , usaeUserStackAssociation

    -- * AccountPassword
    , AccountPassword (..)

    -- * TagValue
    , TagValue (..)

    -- * AwsAccountId
    , AwsAccountId (..)

    -- * SessionState
    , SessionState (..)

    -- * User
    , User (..)
    , mkUser
    , uAuthenticationType
    , uArn
    , uCreatedTime
    , uEnabled
    , uFirstName
    , uLastName
    , uStatus
    , uUserName

    -- * FleetState
    , FleetState (..)

    -- * UserId
    , UserId (..)

    -- * ServiceAccountCredentials
    , ServiceAccountCredentials (..)
    , mkServiceAccountCredentials
    , sacAccountName
    , sacAccountPassword

    -- * VpcConfig
    , VpcConfig (..)
    , mkVpcConfig
    , vcSecurityGroupIds
    , vcSubnetIds

    -- * SessionConnectionState
    , SessionConnectionState (..)

    -- * Name
    , Name (..)

    -- * PlatformType
    , PlatformType (..)

    -- * AccessEndpoint
    , AccessEndpoint (..)
    , mkAccessEndpoint
    , aeEndpointType
    , aeVpceId

    -- * ImageStateChangeReason
    , ImageStateChangeReason (..)
    , mkImageStateChangeReason
    , iscrCode
    , iscrMessage

    -- * ResourceError
    , ResourceError (..)
    , mkResourceError
    , reErrorCode
    , reErrorMessage
    , reErrorTimestamp

    -- * SettingsGroup
    , SettingsGroup (..)

    -- * StorageConnector
    , StorageConnector (..)
    , mkStorageConnector
    , scConnectorType
    , scDomains
    , scResourceIdentifier

    -- * ComputeCapacityStatus
    , ComputeCapacityStatus (..)
    , mkComputeCapacityStatus
    , ccsDesired
    , ccsAvailable
    , ccsInUse
    , ccsRunning

    -- * DisplayName
    , DisplayName (..)

    -- * TagKey
    , TagKey (..)

    -- * LastReportGenerationExecutionError
    , LastReportGenerationExecutionError (..)
    , mkLastReportGenerationExecutionError
    , lrgeeErrorCode
    , lrgeeErrorMessage

    -- * AuthenticationType
    , AuthenticationType (..)

    -- * UsageReportSubscription
    , UsageReportSubscription (..)
    , mkUsageReportSubscription
    , ursLastGeneratedReportDate
    , ursS3BucketName
    , ursSchedule
    , ursSubscriptionErrors

    -- * ImageState
    , ImageState (..)

    -- * FleetErrorCode
    , FleetErrorCode (..)

    -- * Permission
    , Permission (..)

    -- * ImageBuilderStateChangeReason
    , ImageBuilderStateChangeReason (..)
    , mkImageBuilderStateChangeReason
    , ibscrCode
    , ibscrMessage

    -- * DirectoryConfig
    , DirectoryConfig (..)
    , mkDirectoryConfig
    , dcDirectoryName
    , dcCreatedTime
    , dcOrganizationalUnitDistinguishedNames
    , dcServiceAccountCredentials

    -- * StackErrorCode
    , StackErrorCode (..)

    -- * OrganizationalUnitDistinguishedName
    , OrganizationalUnitDistinguishedName (..)

    -- * EmbedHostDomain
    , EmbedHostDomain (..)

    -- * SharedImagePermissions
    , SharedImagePermissions (..)
    , mkSharedImagePermissions
    , sipSharedAccountId
    , sipImagePermissions

    -- * Description
    , Description (..)

    -- * AppstreamAgentVersion
    , AppstreamAgentVersion (..)

    -- * FleetError
    , FleetError (..)
    , mkFleetError
    , feErrorCode
    , feErrorMessage

    -- * DirectoryName
    , DirectoryName (..)

    -- * Stack
    , Stack (..)
    , mkStack
    , sName
    , sAccessEndpoints
    , sApplicationSettings
    , sArn
    , sCreatedTime
    , sDescription
    , sDisplayName
    , sEmbedHostDomains
    , sFeedbackURL
    , sRedirectURL
    , sStackErrors
    , sStorageConnectors
    , sUserSettings

    -- * ImageBuilderStateChangeReasonCode
    , ImageBuilderStateChangeReasonCode (..)

    -- * Session
    , Session (..)
    , mkSession
    , sId
    , sUserId
    , sStackName
    , sFleetName
    , sState
    , sAuthenticationType
    , sConnectionState
    , sMaxExpirationTime
    , sNetworkAccessConfiguration
    , sStartTime

    -- * UsageReportSchedule
    , UsageReportSchedule (..)

    -- * Fleet
    , Fleet (..)
    , mkFleet
    , fArn
    , fName
    , fInstanceType
    , fComputeCapacityStatus
    , fState
    , fCreatedTime
    , fDescription
    , fDisconnectTimeoutInSeconds
    , fDisplayName
    , fDomainJoinInfo
    , fEnableDefaultInternetAccess
    , fFleetErrors
    , fFleetType
    , fIamRoleArn
    , fIdleDisconnectTimeoutInSeconds
    , fImageArn
    , fImageName
    , fMaxUserDurationInSeconds
    , fStreamView
    , fVpcConfig

    -- * StackError
    , StackError (..)
    , mkStackError
    , seErrorCode
    , seErrorMessage

    -- * VisibilityType
    , VisibilityType (..)

    -- * ResourceIdentifier
    , ResourceIdentifier (..)

    -- * StreamView
    , StreamView (..)

    -- * ImageBuilderState
    , ImageBuilderState (..)

    -- * ComputeCapacity
    , ComputeCapacity (..)
    , mkComputeCapacity
    , ccDesiredInstances

    -- * RedirectURL
    , RedirectURL (..)

    -- * SharedAccountId
    , SharedAccountId (..)

    -- * BaseImageArn
    , BaseImageArn (..)

    -- * SourceImageName
    , SourceImageName (..)

    -- * DestinationImageName
    , DestinationImageName (..)

    -- * DestinationImageDescription
    , DestinationImageDescription (..)

    -- * UserName
    , UserName (..)

    -- * FirstName
    , FirstName (..)

    -- * LastName
    , LastName (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.AppStream.Types.ApplicationSettingsResponse
  
import Network.AWS.AppStream.Types.DomainJoinInfo
  
import Network.AWS.AppStream.Types.UsageReportExecutionErrorCode
  
import Network.AWS.AppStream.Types.ImagePermissions
  
import Network.AWS.AppStream.Types.AccessEndpointType
  
import Network.AWS.AppStream.Types.ImageStateChangeReasonCode
  
import Network.AWS.AppStream.Types.ApplicationSettings
  
import Network.AWS.AppStream.Types.UserSetting
  
import Network.AWS.AppStream.Types.RegionName
  
import Network.AWS.AppStream.Types.Image
  
import Network.AWS.AppStream.Types.Application
  
import Network.AWS.AppStream.Types.FleetAttribute
  
import Network.AWS.AppStream.Types.FeedbackURL
  
import Network.AWS.AppStream.Types.StackAttribute
  
import Network.AWS.AppStream.Types.NetworkAccessConfiguration
  
import Network.AWS.AppStream.Types.StorageConnectorType
  
  
import Network.AWS.AppStream.Types.Arn
  
import Network.AWS.AppStream.Types.MessageAction
  
  
import Network.AWS.AppStream.Types.Domain
  
  
import Network.AWS.AppStream.Types.UserStackAssociationErrorCode
  
import Network.AWS.AppStream.Types.FleetType
  
import Network.AWS.AppStream.Types.Action
  
import Network.AWS.AppStream.Types.AccountName
  
import Network.AWS.AppStream.Types.UserStackAssociation
  
import Network.AWS.AppStream.Types.ImageBuilder
  
import Network.AWS.AppStream.Types.Username
  
import Network.AWS.AppStream.Types.UserStackAssociationError
  
import Network.AWS.AppStream.Types.AccountPassword
  
import Network.AWS.AppStream.Types.TagValue
  
import Network.AWS.AppStream.Types.AwsAccountId
  
import Network.AWS.AppStream.Types.SessionState
  
import Network.AWS.AppStream.Types.User
  
  
import Network.AWS.AppStream.Types.FleetState
  
import Network.AWS.AppStream.Types.UserId
  
import Network.AWS.AppStream.Types.ServiceAccountCredentials
  
import Network.AWS.AppStream.Types.VpcConfig
  
import Network.AWS.AppStream.Types.SessionConnectionState
  
import Network.AWS.AppStream.Types.Name
  
import Network.AWS.AppStream.Types.PlatformType
  
import Network.AWS.AppStream.Types.AccessEndpoint
  
  
import Network.AWS.AppStream.Types.ImageStateChangeReason
  
import Network.AWS.AppStream.Types.ResourceError
  
import Network.AWS.AppStream.Types.SettingsGroup
  
import Network.AWS.AppStream.Types.StorageConnector
  
import Network.AWS.AppStream.Types.ComputeCapacityStatus
  
import Network.AWS.AppStream.Types.DisplayName
  
  
import Network.AWS.AppStream.Types.TagKey
  
import Network.AWS.AppStream.Types.LastReportGenerationExecutionError
  
import Network.AWS.AppStream.Types.AuthenticationType
  
import Network.AWS.AppStream.Types.UsageReportSubscription
  
import Network.AWS.AppStream.Types.ImageState
  
import Network.AWS.AppStream.Types.FleetErrorCode
  
import Network.AWS.AppStream.Types.Permission
  
import Network.AWS.AppStream.Types.ImageBuilderStateChangeReason
  
  
import Network.AWS.AppStream.Types.DirectoryConfig
  
import Network.AWS.AppStream.Types.StackErrorCode
  
import Network.AWS.AppStream.Types.OrganizationalUnitDistinguishedName
  
import Network.AWS.AppStream.Types.EmbedHostDomain
  
import Network.AWS.AppStream.Types.SharedImagePermissions
  
import Network.AWS.AppStream.Types.Description
  
import Network.AWS.AppStream.Types.AppstreamAgentVersion
  
import Network.AWS.AppStream.Types.FleetError
  
import Network.AWS.AppStream.Types.DirectoryName
  
import Network.AWS.AppStream.Types.Stack
  
import Network.AWS.AppStream.Types.ImageBuilderStateChangeReasonCode
  
import Network.AWS.AppStream.Types.Session
  
import Network.AWS.AppStream.Types.UsageReportSchedule
  
import Network.AWS.AppStream.Types.Fleet
  
import Network.AWS.AppStream.Types.StackError
  
import Network.AWS.AppStream.Types.VisibilityType
  
  
  
import Network.AWS.AppStream.Types.ResourceIdentifier
  
import Network.AWS.AppStream.Types.StreamView
  
import Network.AWS.AppStream.Types.ImageBuilderState
  
  
import Network.AWS.AppStream.Types.ComputeCapacity
  
  
import Network.AWS.AppStream.Types.RedirectURL
  
  
import Network.AWS.AppStream.Types.SharedAccountId
  
import Network.AWS.AppStream.Types.BaseImageArn
  
import Network.AWS.AppStream.Types.SourceImageName
  
import Network.AWS.AppStream.Types.DestinationImageName
  
import Network.AWS.AppStream.Types.DestinationImageDescription
  
import Network.AWS.AppStream.Types.UserName
  
import Network.AWS.AppStream.Types.FirstName
  
import Network.AWS.AppStream.Types.LastName
  

-- | API version @2016-12-01@ of the Amazon AppStream SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "AppStream",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "appstream2",
                 Core._svcVersion = "2016-12-01", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "AppStream",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The specified role is invalid.
_InvalidRoleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRoleException
  = Core._MatchServiceError mkServiceConfig "InvalidRoleException"
{-# INLINEABLE _InvalidRoleException #-}
{-# DEPRECATED _InvalidRoleException "Use generic-lens or generic-optics instead"  #-}

-- | AppStream 2.0 can’t process the request right now because the Describe calls from your AWS account are being throttled by Amazon EC2. Try again later.
_RequestLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "RequestLimitExceededException"
{-# INLINEABLE _RequestLimitExceededException #-}
{-# DEPRECATED _RequestLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "ResourceAlreadyExistsException"
{-# INLINEABLE _ResourceAlreadyExistsException #-}
{-# DEPRECATED _ResourceAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | The image does not support storage connectors.
_IncompatibleImageException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IncompatibleImageException
  = Core._MatchServiceError mkServiceConfig
      "IncompatibleImageException"
{-# INLINEABLE _IncompatibleImageException #-}
{-# DEPRECATED _IncompatibleImageException "Use generic-lens or generic-optics instead"  #-}

-- | An API error occurred. Wait a few minutes and try again.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException
  = Core._MatchServiceError mkServiceConfig
      "ConcurrentModificationException"
{-# INLINEABLE _ConcurrentModificationException #-}
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead"  #-}

-- | The attempted operation is not permitted.
_OperationNotPermittedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedException
  = Core._MatchServiceError mkServiceConfig
      "OperationNotPermittedException"
{-# INLINEABLE _OperationNotPermittedException #-}
{-# DEPRECATED _OperationNotPermittedException "Use generic-lens or generic-optics instead"  #-}

-- | The resource cannot be created because your AWS account is suspended. For assistance, contact AWS Support. 
_InvalidAccountStatusException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAccountStatusException
  = Core._MatchServiceError mkServiceConfig
      "InvalidAccountStatusException"
{-# INLINEABLE _InvalidAccountStatusException #-}
{-# DEPRECATED _InvalidAccountStatusException "Use generic-lens or generic-optics instead"  #-}

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates an incorrect combination of parameters, or a missing parameter.
_InvalidParameterCombinationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterCombinationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterCombinationException"
{-# INLINEABLE _InvalidParameterCombinationException #-}
{-# DEPRECATED _InvalidParameterCombinationException "Use generic-lens or generic-optics instead"  #-}

-- | The specified resource exists and is not in use, but isn't available.
_ResourceNotAvailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotAvailableException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotAvailableException"
{-# INLINEABLE _ResourceNotAvailableException #-}
{-# DEPRECATED _ResourceNotAvailableException "Use generic-lens or generic-optics instead"  #-}

-- | The requested limit exceeds the permitted limit for an account.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The specified resource is in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException
  = Core._MatchServiceError mkServiceConfig "ResourceInUseException"
{-# INLINEABLE _ResourceInUseException #-}
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead"  #-}
