{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentity.Types
    (
    -- * Service Configuration
      cognitoIdentity

    -- * Errors
    , _InvalidIdentityPoolConfigurationException
    , _InvalidParameterException
    , _NotAuthorizedException
    , _InternalErrorException
    , _ExternalServiceException
    , _TooManyRequestsException
    , _ConcurrentModificationException
    , _ResourceConflictException
    , _DeveloperUserAlreadyRegisteredException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * CognitoErrorCode
    , CognitoErrorCode (..)

    -- * CognitoIdentityProvider
    , CognitoIdentityProvider
    , cognitoIdentityProvider
    , cipClientId
    , cipProviderName

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
    , ipCognitoIdentityProviders
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
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2014-06-30' of the Amazon Cognito Identity SDK configuration.
cognitoIdentity :: Service
cognitoIdentity =
    Service
    { _svcAbbrev = "CognitoIdentity"
    , _svcSigner = v4
    , _svcPrefix = "cognito-identity"
    , _svcVersion = "2014-06-30"
    , _svcEndpoint = defaultEndpoint cognitoIdentity
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "CognitoIdentity"
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

-- | Thrown if the identity pool has no role associated for the given auth type (auth\/unauth) or if the AssumeRole fails.
_InvalidIdentityPoolConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidIdentityPoolConfigurationException =
    _ServiceError . hasCode "InvalidIdentityPoolConfigurationException"

-- | Thrown for missing or bad input parameter(s).
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
    _ServiceError . hasCode "InvalidParameterException"

-- | Thrown when a user is not authorized to access the requested resource.
_NotAuthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_NotAuthorizedException = _ServiceError . hasCode "NotAuthorizedException"

-- | Thrown when the service encounters an error during processing the request.
_InternalErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalErrorException = _ServiceError . hasCode "InternalErrorException"

-- | An exception thrown when a dependent service such as Facebook or Twitter is not responding
_ExternalServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_ExternalServiceException = _ServiceError . hasCode "ExternalServiceException"

-- | Thrown when a request is throttled.
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException = _ServiceError . hasCode "TooManyRequestsException"

-- | Thrown if there are parallel requests to modify a resource.
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
    _ServiceError . hasCode "ConcurrentModificationException"

-- | Thrown when a user tries to use a login which is already linked to another account.
_ResourceConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceConflictException =
    _ServiceError . hasCode "ResourceConflictException"

-- | The provided developer user identifier is already registered with Cognito under a different identity ID.
_DeveloperUserAlreadyRegisteredException :: AsError a => Getting (First ServiceError) a ServiceError
_DeveloperUserAlreadyRegisteredException =
    _ServiceError . hasCode "DeveloperUserAlreadyRegisteredException"

-- | Thrown when the requested resource (for example, a dataset or record) does not exist.
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasCode "ResourceNotFoundException"

-- | Thrown when the total number of user pools has exceeded a preset limit.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"
