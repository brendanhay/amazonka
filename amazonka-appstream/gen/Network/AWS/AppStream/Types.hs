{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , _OperationNotPermittedException
    , _ResourceNotFoundException
    , _ResourceNotAvailableException
    , _LimitExceededException
    , _ResourceInUseException

    -- * FleetErrorCode
    , FleetErrorCode (..)

    -- * FleetState
    , FleetState (..)

    -- * ImageState
    , ImageState (..)

    -- * ImageStateChangeReasonCode
    , ImageStateChangeReasonCode (..)

    -- * PlatformType
    , PlatformType (..)

    -- * SessionState
    , SessionState (..)

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

    -- * Fleet
    , Fleet
    , fleet
    , fDisconnectTimeoutInSeconds
    , fMaxUserDurationInSeconds
    , fCreatedTime
    , fVPCConfig
    , fFleetErrors
    , fDisplayName
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
    , iStateChangeReason
    , iARN
    , iCreatedTime
    , iVisibility
    , iBaseImageARN
    , iDisplayName
    , iDescription
    , iApplications
    , iName

    -- * ImageStateChangeReason
    , ImageStateChangeReason
    , imageStateChangeReason
    , iscrCode
    , iscrMessage

    -- * Session
    , Session
    , session
    , sId
    , sUserId
    , sStackName
    , sFleetName
    , sState

    -- * Stack
    , Stack
    , stack
    , sARN
    , sCreatedTime
    , sDisplayName
    , sDescription
    , sName

    -- * VPCConfig
    , VPCConfig
    , vpcConfig
    , vcSubnetIds
    ) where

import           Network.AWS.AppStream.Types.Product
import           Network.AWS.AppStream.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

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
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
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

-- | The attempted operation is not permitted.
--
--
_OperationNotPermittedException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationNotPermittedException =
    _MatchServiceError appStream "OperationNotPermittedException"

-- | The specified resource was not found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _MatchServiceError appStream "ResourceNotFoundException"

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
