{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceConflictException,
    _InternalErrorException,
    _ConcurrentModificationException,
    _ExternalServiceException,
    _InvalidParameterException,
    _InvalidIdentityPoolConfigurationException,
    _LimitExceededException,
    _DeveloperUserAlreadyRegisteredException,
    _ResourceNotFoundException,
    _NotAuthorizedException,
    _TooManyRequestsException,

    -- * AmbiguousRoleResolutionType
    AmbiguousRoleResolutionType (..),

    -- * CognitoErrorCode
    CognitoErrorCode (..),

    -- * MappingRuleMatchType
    MappingRuleMatchType (..),

    -- * RoleMappingType
    RoleMappingType (..),

    -- * CognitoIdentityProvider
    CognitoIdentityProvider (..),
    newCognitoIdentityProvider,
    cognitoIdentityProvider_clientId,
    cognitoIdentityProvider_providerName,
    cognitoIdentityProvider_serverSideTokenCheck,

    -- * Credentials
    Credentials (..),
    newCredentials,
    credentials_expiration,
    credentials_secretKey,
    credentials_accessKeyId,
    credentials_sessionToken,

    -- * IdentityDescription
    IdentityDescription (..),
    newIdentityDescription,
    identityDescription_lastModifiedDate,
    identityDescription_creationDate,
    identityDescription_identityId,
    identityDescription_logins,

    -- * IdentityPool
    IdentityPool (..),
    newIdentityPool,
    identityPool_allowClassicFlow,
    identityPool_samlProviderARNs,
    identityPool_identityPoolTags,
    identityPool_openIdConnectProviderARNs,
    identityPool_supportedLoginProviders,
    identityPool_cognitoIdentityProviders,
    identityPool_developerProviderName,
    identityPool_identityPoolId,
    identityPool_identityPoolName,
    identityPool_allowUnauthenticatedIdentities,

    -- * IdentityPoolShortDescription
    IdentityPoolShortDescription (..),
    newIdentityPoolShortDescription,
    identityPoolShortDescription_identityPoolId,
    identityPoolShortDescription_identityPoolName,

    -- * MappingRule
    MappingRule (..),
    newMappingRule,
    mappingRule_claim,
    mappingRule_matchType,
    mappingRule_value,
    mappingRule_roleARN,

    -- * RoleMapping
    RoleMapping (..),
    newRoleMapping,
    roleMapping_ambiguousRoleResolution,
    roleMapping_rulesConfiguration,
    roleMapping_type,

    -- * RulesConfigurationType
    RulesConfigurationType (..),
    newRulesConfigurationType,
    rulesConfigurationType_rules,

    -- * UnprocessedIdentityId
    UnprocessedIdentityId (..),
    newUnprocessedIdentityId,
    unprocessedIdentityId_identityId,
    unprocessedIdentityId_errorCode,
  )
where

import Network.AWS.CognitoIdentity.Types.AmbiguousRoleResolutionType
import Network.AWS.CognitoIdentity.Types.CognitoErrorCode
import Network.AWS.CognitoIdentity.Types.CognitoIdentityProvider
import Network.AWS.CognitoIdentity.Types.Credentials
import Network.AWS.CognitoIdentity.Types.IdentityDescription
import Network.AWS.CognitoIdentity.Types.IdentityPool
import Network.AWS.CognitoIdentity.Types.IdentityPoolShortDescription
import Network.AWS.CognitoIdentity.Types.MappingRule
import Network.AWS.CognitoIdentity.Types.MappingRuleMatchType
import Network.AWS.CognitoIdentity.Types.RoleMapping
import Network.AWS.CognitoIdentity.Types.RoleMappingType
import Network.AWS.CognitoIdentity.Types.RulesConfigurationType
import Network.AWS.CognitoIdentity.Types.UnprocessedIdentityId
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-06-30@ of the Amazon Cognito Identity SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "CognitoIdentity",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "cognito-identity",
      Prelude._svcVersion = "2014-06-30",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "CognitoIdentity",
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

-- | Thrown when a user tries to use a login which is already linked to
-- another account.
_ResourceConflictException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceConflictException =
  Prelude._MatchServiceError
    defaultService
    "ResourceConflictException"

-- | Thrown when the service encounters an error during processing the
-- request.
_InternalErrorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalErrorException =
  Prelude._MatchServiceError
    defaultService
    "InternalErrorException"

-- | Thrown if there are parallel requests to modify a resource.
_ConcurrentModificationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConcurrentModificationException =
  Prelude._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | An exception thrown when a dependent service such as Facebook or Twitter
-- is not responding
_ExternalServiceException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ExternalServiceException =
  Prelude._MatchServiceError
    defaultService
    "ExternalServiceException"

-- | Thrown for missing or bad input parameter(s).
_InvalidParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | Thrown if the identity pool has no role associated for the given auth
-- type (auth\/unauth) or if the AssumeRole fails.
_InvalidIdentityPoolConfigurationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidIdentityPoolConfigurationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidIdentityPoolConfigurationException"

-- | Thrown when the total number of user pools has exceeded a preset limit.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The provided developer user identifier is already registered with
-- Cognito under a different identity ID.
_DeveloperUserAlreadyRegisteredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeveloperUserAlreadyRegisteredException =
  Prelude._MatchServiceError
    defaultService
    "DeveloperUserAlreadyRegisteredException"

-- | Thrown when the requested resource (for example, a dataset or record)
-- does not exist.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Thrown when a user is not authorized to access the requested resource.
_NotAuthorizedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotAuthorizedException =
  Prelude._MatchServiceError
    defaultService
    "NotAuthorizedException"

-- | Thrown when a request is throttled.
_TooManyRequestsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyRequestsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyRequestsException"
