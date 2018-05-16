{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Mobile.Types
    (
    -- * Service Configuration
      mobile

    -- * Errors
    , _NotFoundException
    , _TooManyRequestsException
    , _InternalFailureException
    , _ServiceUnavailableException
    , _UnauthorizedException
    , _BadRequestException
    , _LimitExceededException
    , _AccountActionRequiredException

    -- * Platform
    , Platform (..)

    -- * ProjectState
    , ProjectState (..)

    -- * BundleDetails
    , BundleDetails
    , bundleDetails
    , bdAvailablePlatforms
    , bdBundleId
    , bdVersion
    , bdIconURL
    , bdTitle
    , bdDescription

    -- * ProjectDetails
    , ProjectDetails
    , projectDetails
    , pdState
    , pdResources
    , pdCreatedDate
    , pdConsoleURL
    , pdName
    , pdRegion
    , pdProjectId
    , pdLastUpdatedDate

    -- * ProjectSummary
    , ProjectSummary
    , projectSummary
    , psName
    , psProjectId

    -- * Resource
    , Resource
    , resource
    , rFeature
    , rArn
    , rName
    , rAttributes
    , rType
    ) where

import Network.AWS.Lens
import Network.AWS.Mobile.Types.Product
import Network.AWS.Mobile.Types.Sum
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-07-01@ of the Amazon Mobile SDK configuration.
mobile :: Service
mobile =
  Service
    { _svcAbbrev = "Mobile"
    , _svcSigner = v4
    , _svcPrefix = "mobile"
    , _svcVersion = "2017-07-01"
    , _svcEndpoint = defaultEndpoint mobile
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Mobile"
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


-- | No entity can be found with the specified identifier.
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException =
  _MatchServiceError mobile "NotFoundException" . hasStatus 404


-- | Too many requests have been received for this AWS account in too short a time. The request should be retried after some time delay.
--
--
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
  _MatchServiceError mobile "TooManyRequestsException" . hasStatus 429


-- | The service has encountered an unexpected error condition which prevents it from servicing the request.
--
--
_InternalFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalFailureException =
  _MatchServiceError mobile "InternalFailureException" . hasStatus 500


-- | The service is temporarily unavailable. The request should be retried after some time delay.
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
  _MatchServiceError mobile "ServiceUnavailableException" . hasStatus 503


-- | Credentials of the caller are insufficient to authorize the request.
--
--
_UnauthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedException =
  _MatchServiceError mobile "UnauthorizedException" . hasStatus 401


-- | The request cannot be processed because some parameter is not valid or the project state prevents the operation from being performed.
--
--
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException =
  _MatchServiceError mobile "BadRequestException" . hasStatus 400


-- | There are too many AWS Mobile Hub projects in the account or the account has exceeded the maximum number of resources in some AWS service. You should create another sub-account using AWS Organizations or remove some resources and retry your request.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError mobile "LimitExceededException" . hasStatus 429


-- | Account Action is required in order to continue the request.
--
--
_AccountActionRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_AccountActionRequiredException =
  _MatchServiceError mobile "AccountActionRequiredException" . hasStatus 403

