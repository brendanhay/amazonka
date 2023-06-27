{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CognitoIdentity.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentity.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConcurrentModificationException,
    _DeveloperUserAlreadyRegisteredException,
    _ExternalServiceException,
    _InternalErrorException,
    _InvalidIdentityPoolConfigurationException,
    _InvalidParameterException,
    _LimitExceededException,
    _NotAuthorizedException,
    _ResourceConflictException,
    _ResourceNotFoundException,
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
    credentials_accessKeyId,
    credentials_expiration,
    credentials_secretKey,
    credentials_sessionToken,

    -- * IdentityDescription
    IdentityDescription (..),
    newIdentityDescription,
    identityDescription_creationDate,
    identityDescription_identityId,
    identityDescription_lastModifiedDate,
    identityDescription_logins,

    -- * IdentityPool
    IdentityPool (..),
    newIdentityPool,
    identityPool_allowClassicFlow,
    identityPool_cognitoIdentityProviders,
    identityPool_developerProviderName,
    identityPool_identityPoolTags,
    identityPool_openIdConnectProviderARNs,
    identityPool_samlProviderARNs,
    identityPool_supportedLoginProviders,
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
    unprocessedIdentityId_errorCode,
    unprocessedIdentityId_identityId,
  )
where

import Amazonka.CognitoIdentity.Types.AmbiguousRoleResolutionType
import Amazonka.CognitoIdentity.Types.CognitoErrorCode
import Amazonka.CognitoIdentity.Types.CognitoIdentityProvider
import Amazonka.CognitoIdentity.Types.Credentials
import Amazonka.CognitoIdentity.Types.IdentityDescription
import Amazonka.CognitoIdentity.Types.IdentityPool
import Amazonka.CognitoIdentity.Types.IdentityPoolShortDescription
import Amazonka.CognitoIdentity.Types.MappingRule
import Amazonka.CognitoIdentity.Types.MappingRuleMatchType
import Amazonka.CognitoIdentity.Types.RoleMapping
import Amazonka.CognitoIdentity.Types.RoleMappingType
import Amazonka.CognitoIdentity.Types.RulesConfigurationType
import Amazonka.CognitoIdentity.Types.UnprocessedIdentityId
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2014-06-30@ of the Amazon Cognito Identity SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CognitoIdentity",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "cognito-identity",
      Core.signingName = "cognito-identity",
      Core.version = "2014-06-30",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "CognitoIdentity",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Thrown if there are parallel requests to modify a resource.
_ConcurrentModificationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The provided developer user identifier is already registered with
-- Cognito under a different identity ID.
_DeveloperUserAlreadyRegisteredException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DeveloperUserAlreadyRegisteredException =
  Core._MatchServiceError
    defaultService
    "DeveloperUserAlreadyRegisteredException"

-- | An exception thrown when a dependent service such as Facebook or Twitter
-- is not responding
_ExternalServiceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ExternalServiceException =
  Core._MatchServiceError
    defaultService
    "ExternalServiceException"

-- | Thrown when the service encounters an error during processing the
-- request.
_InternalErrorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError
    defaultService
    "InternalErrorException"

-- | Thrown if the identity pool has no role associated for the given auth
-- type (auth\/unauth) or if the AssumeRole fails.
_InvalidIdentityPoolConfigurationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidIdentityPoolConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidIdentityPoolConfigurationException"

-- | Thrown for missing or bad input parameter(s).
_InvalidParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | Thrown when the total number of user pools has exceeded a preset limit.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | Thrown when a user is not authorized to access the requested resource.
_NotAuthorizedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotAuthorizedException =
  Core._MatchServiceError
    defaultService
    "NotAuthorizedException"

-- | Thrown when a user tries to use a login which is already linked to
-- another account.
_ResourceConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceConflictException"

-- | Thrown when the requested resource (for example, a dataset or record)
-- does not exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Thrown when a request is throttled.
_TooManyRequestsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
