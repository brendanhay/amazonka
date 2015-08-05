{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentity.Types
    (
    -- * Service
      CognitoIdentity

    -- * Errors
    , _InvalidIdentityPoolConfigurationException
    , _InvalidParameterException
    , _NotAuthorizedException
    , _InternalErrorException
    , _ExternalServiceException
    , _ConcurrentModificationException
    , _TooManyRequestsException
    , _ResourceConflictException
    , _DeveloperUserAlreadyRegisteredException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * CognitoErrorCode
    , CognitoErrorCode (..)

    -- * Credentials
    , Credentials
    , credentials
    , cSessionToken
    , cExpiration
    , cSecretKey
    , cAccessKeyId

    -- * IdentityDescription
    , IdentityDescription
    , identityDescription
    , idLastModifiedDate
    , idCreationDate
    , idLogins
    , idIdentityId

    -- * IdentityPool
    , IdentityPool
    , identityPool
    , ipSupportedLoginProviders
    , ipDeveloperProviderName
    , ipOpenIdConnectProviderARNs
    , ipIdentityPoolId
    , ipIdentityPoolName
    , ipAllowUnauthenticatedIdentities

    -- * IdentityPoolShortDescription
    , IdentityPoolShortDescription
    , identityPoolShortDescription
    , ipsdIdentityPoolId
    , ipsdIdentityPoolName

    -- * UnprocessedIdentityId
    , UnprocessedIdentityId
    , unprocessedIdentityId
    , uiiErrorCode
    , uiiIdentityId
    ) where

import           Network.AWS.CognitoIdentity.Types.Product
import           Network.AWS.CognitoIdentity.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2014-06-30@ of the Amazon Cognito Identity SDK.
data CognitoIdentity

instance AWSService CognitoIdentity where
    type Sg CognitoIdentity = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "CognitoIdentity"
            , _svcPrefix = "cognito-identity"
            , _svcVersion = "2014-06-30"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | Thrown if the identity pool has no role associated for the given auth
-- type (auth\/unauth) or if the AssumeRole fails.
_InvalidIdentityPoolConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidIdentityPoolConfigurationException =
    _ServiceError .
    hasStatus 400 . hasCode "InvalidIdentityPoolConfigurationException"

-- | Thrown for missing or bad input parameter(s).
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameterException"

-- | Thrown when a user is not authorized to access the requested resource.
_NotAuthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_NotAuthorizedException =
    _ServiceError . hasStatus 403 . hasCode "NotAuthorizedException"

-- | Thrown when the service encounters an error during processing the
-- request.
_InternalErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalErrorException = _ServiceError . hasCode "InternalErrorException"

-- | An exception thrown when a dependent service such as Facebook or Twitter
-- is not responding
_ExternalServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_ExternalServiceException =
    _ServiceError . hasStatus 400 . hasCode "ExternalServiceException"

-- | Thrown if there are parallel requests to modify a resource.
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
    _ServiceError . hasStatus 400 . hasCode "ConcurrentModificationException"

-- | Thrown when a request is throttled.
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
    _ServiceError . hasStatus 429 . hasCode "TooManyRequestsException"

-- | Thrown when a user tries to use a login which is already linked to
-- another account.
_ResourceConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceConflictException =
    _ServiceError . hasStatus 409 . hasCode "ResourceConflictException"

-- | The provided developer user identifier is already registered with
-- Cognito under a different identity ID.
_DeveloperUserAlreadyRegisteredException :: AsError a => Getting (First ServiceError) a ServiceError
_DeveloperUserAlreadyRegisteredException =
    _ServiceError .
    hasStatus 400 . hasCode "DeveloperUserAlreadyRegisteredException"

-- | Thrown when the requested resource (for example, a dataset or record)
-- does not exist.
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasStatus 404 . hasCode "ResourceNotFoundException"

-- | Thrown when the total number of user pools has exceeded a preset limit.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 400 . hasCode "LimitExceededException"
