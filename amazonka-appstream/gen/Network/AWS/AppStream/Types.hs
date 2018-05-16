{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types
    (
    -- * Service Configuration
      appStream

    -- * Errors
    , _InvalidRoleException
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

    -- * AuthenticationType
    , AuthenticationType (..)

    -- * FleetAttribute
    , FleetAttribute (..)

    -- * FleetErrorCode
    , FleetErrorCode (..)

    -- * FleetState
    , FleetState (..)

    -- * FleetType
    , FleetType (..)

    -- * ImageBuilderState
    , ImageBuilderState (..)

    -- * ImageBuilderStateChangeReasonCode
    , ImageBuilderStateChangeReasonCode (..)

    -- * ImageState
    , ImageState (..)

    -- * ImageStateChangeReasonCode
    , ImageStateChangeReasonCode (..)

    -- * PlatformType
    , PlatformType (..)

    -- * SessionState
    , SessionState (..)

    -- * StackAttribute
    , StackAttribute (..)

    -- * StackErrorCode
    , StackErrorCode (..)

    -- * StorageConnectorType
    , StorageConnectorType (..)

    -- * VisibilityType
    , VisibilityType (..)

    -- * Application
    , Application
    , application
    , aEnabled
    , aLaunchPath
    , aLaunchParameters
    , aName
    , aDisplayName
    , aMetadata
    , aIconURL

    -- * ComputeCapacity
    , ComputeCapacity
    , computeCapacity
    , ccDesiredInstances

    -- * ComputeCapacityStatus
    , ComputeCapacityStatus
    , computeCapacityStatus
    , ccsInUse
    , ccsRunning
    , ccsAvailable
    , ccsDesired

    -- * DirectoryConfig
    , DirectoryConfig
    , directoryConfig
    , dcCreatedTime
    , dcServiceAccountCredentials
    , dcOrganizationalUnitDistinguishedNames
    , dcDirectoryName

    -- * DomainJoinInfo
    , DomainJoinInfo
    , domainJoinInfo
    , djiOrganizationalUnitDistinguishedName
    , djiDirectoryName

    -- * Fleet
    , Fleet
    , fleet
    , fDomainJoinInfo
    , fDisconnectTimeoutInSeconds
    , fMaxUserDurationInSeconds
    , fCreatedTime
    , fFleetType
    , fVPCConfig
    , fFleetErrors
    , fDisplayName
    , fEnableDefaultInternetAccess
    , fDescription
    , fARN
    , fName
    , fImageName
    , fInstanceType
    , fComputeCapacityStatus
    , fState

    -- * FleetError
    , FleetError
    , fleetError
    , feErrorCode
    , feErrorMessage

    -- * Image
    , Image
    , image
    , iState
    , iPlatform
    , iPublicBaseImageReleasedDate
    , iStateChangeReason
    , iARN
    , iCreatedTime
    , iImageBuilderSupported
    , iVisibility
    , iBaseImageARN
    , iDisplayName
    , iDescription
    , iAppstreamAgentVersion
    , iApplications
    , iName

    -- * ImageBuilder
    , ImageBuilder
    , imageBuilder
    , ibDomainJoinInfo
    , ibState
    , ibPlatform
    , ibStateChangeReason
    , ibARN
    , ibCreatedTime
    , ibImageBuilderErrors
    , ibInstanceType
    , ibVPCConfig
    , ibImageARN
    , ibDisplayName
    , ibEnableDefaultInternetAccess
    , ibDescription
    , ibAppstreamAgentVersion
    , ibName

    -- * ImageBuilderStateChangeReason
    , ImageBuilderStateChangeReason
    , imageBuilderStateChangeReason
    , ibscrCode
    , ibscrMessage

    -- * ImageStateChangeReason
    , ImageStateChangeReason
    , imageStateChangeReason
    , iscrCode
    , iscrMessage

    -- * ResourceError
    , ResourceError
    , resourceError
    , reErrorCode
    , reErrorMessage
    , reErrorTimestamp

    -- * ServiceAccountCredentials
    , ServiceAccountCredentials
    , serviceAccountCredentials
    , sacAccountName
    , sacAccountPassword

    -- * Session
    , Session
    , session
    , sAuthenticationType
    , sId
    , sUserId
    , sStackName
    , sFleetName
    , sState

    -- * Stack
    , Stack
    , stack
    , sFeedbackURL
    , sARN
    , sCreatedTime
    , sStorageConnectors
    , sDisplayName
    , sStackErrors
    , sDescription
    , sRedirectURL
    , sName

    -- * StackError
    , StackError
    , stackError
    , seErrorCode
    , seErrorMessage

    -- * StorageConnector
    , StorageConnector
    , storageConnector
    , scResourceIdentifier
    , scConnectorType

    -- * VPCConfig
    , VPCConfig
    , vpcConfig
    , vcSecurityGroupIds
    , vcSubnetIds
    ) where

import Network.AWS.AppStream.Types.Product
import Network.AWS.AppStream.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-12-01@ of the Amazon AppStream SDK configuration.
appStream :: Service
appStream =
  Service
    { _svcAbbrev = "AppStream"
    , _svcSigner = v4
    , _svcPrefix = "appstream2"
    , _svcVersion = "2016-12-01"
    , _svcEndpoint = defaultEndpoint appStream
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "AppStream"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The specified role is invalid.
--
--
_InvalidRoleException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRoleException = _MatchServiceError appStream "InvalidRoleException"


-- | The specified resource already exists.
--
--
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException =
  _MatchServiceError appStream "ResourceAlreadyExistsException"


-- | The image does not support storage connectors.
--
--
_IncompatibleImageException :: AsError a => Getting (First ServiceError) a ServiceError
_IncompatibleImageException =
  _MatchServiceError appStream "IncompatibleImageException"


-- | An API error occurred. Wait a few minutes and try again.
--
--
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
  _MatchServiceError appStream "ConcurrentModificationException"


-- | The attempted operation is not permitted.
--
--
_OperationNotPermittedException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationNotPermittedException =
  _MatchServiceError appStream "OperationNotPermittedException"


-- | The resource cannot be created because your AWS account is suspended. For assistance, contact AWS Support.
--
--
_InvalidAccountStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAccountStatusException =
  _MatchServiceError appStream "InvalidAccountStatusException"


-- | The specified resource was not found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError appStream "ResourceNotFoundException"


-- | Indicates an incorrect combination of parameters, or a missing parameter.
--
--
_InvalidParameterCombinationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterCombinationException =
  _MatchServiceError appStream "InvalidParameterCombinationException"


-- | The specified resource exists and is not in use, but isn't available.
--
--
_ResourceNotAvailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotAvailableException =
  _MatchServiceError appStream "ResourceNotAvailableException"


-- | The requested limit exceeds the permitted limit for an account.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _MatchServiceError appStream "LimitExceededException"


-- | The specified resource is in use.
--
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException = _MatchServiceError appStream "ResourceInUseException"

