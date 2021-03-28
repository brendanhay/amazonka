-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentity.Types
    (
    -- * Service configuration
      mkServiceConfig

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

    -- * RoleType
    , RoleType (..)

    -- * IdentityProviderToken
    , IdentityProviderToken (..)

    -- * CognitoIdentityProviderClientId
    , CognitoIdentityProviderClientId (..)

    -- * DeveloperUserIdentifier
    , DeveloperUserIdentifier (..)

    -- * SecretKeyString
    , SecretKeyString (..)

    -- * IdentityPoolId
    , IdentityPoolId (..)

    -- * MappingRule
    , MappingRule (..)
    , mkMappingRule
    , mrClaim
    , mrMatchType
    , mrValue
    , mrRoleARN

    -- * UnprocessedIdentityId
    , UnprocessedIdentityId (..)
    , mkUnprocessedIdentityId
    , uiiErrorCode
    , uiiIdentityId

    -- * IdentityProviderId
    , IdentityProviderId (..)

    -- * IdentityPoolName
    , IdentityPoolName (..)

    -- * Credentials
    , Credentials (..)
    , mkCredentials
    , cAccessKeyId
    , cExpiration
    , cSecretKey
    , cSessionToken

    -- * CognitoIdentityProvider
    , CognitoIdentityProvider (..)
    , mkCognitoIdentityProvider
    , cipClientId
    , cipProviderName
    , cipServerSideTokenCheck

    -- * IdentityProviderName
    , IdentityProviderName (..)

    -- * IdentityDescription
    , IdentityDescription (..)
    , mkIdentityDescription
    , idCreationDate
    , idIdentityId
    , idLastModifiedDate
    , idLogins

    -- * ARNString
    , ARNString (..)

    -- * DeveloperProviderName
    , DeveloperProviderName (..)

    -- * RoleMapping
    , RoleMapping (..)
    , mkRoleMapping
    , rmType
    , rmAmbiguousRoleResolution
    , rmRulesConfiguration

    -- * AmbiguousRoleResolutionType
    , AmbiguousRoleResolutionType (..)

    -- * MappingRuleMatchType
    , MappingRuleMatchType (..)

    -- * AccountId
    , AccountId (..)

    -- * IdentityPool
    , IdentityPool (..)
    , mkIdentityPool
    , ipIdentityPoolId
    , ipIdentityPoolName
    , ipAllowUnauthenticatedIdentities
    , ipAllowClassicFlow
    , ipCognitoIdentityProviders
    , ipDeveloperProviderName
    , ipIdentityPoolTags
    , ipOpenIdConnectProviderARNs
    , ipSamlProviderARNs
    , ipSupportedLoginProviders

    -- * CognitoErrorCode
    , CognitoErrorCode (..)

    -- * RulesConfigurationType
    , RulesConfigurationType (..)
    , mkRulesConfigurationType
    , rctRules

    -- * RoleMappingType
    , RoleMappingType (..)

    -- * IdentityId
    , IdentityId (..)

    -- * PaginationKey
    , PaginationKey (..)

    -- * TagKeysType
    , TagKeysType (..)

    -- * IdentityPoolShortDescription
    , IdentityPoolShortDescription (..)
    , mkIdentityPoolShortDescription
    , ipsdIdentityPoolId
    , ipsdIdentityPoolName

    -- * TagValueType
    , TagValueType (..)

    -- * Token
    , Token (..)

    -- * NextToken
    , NextToken (..)

    -- * Claim
    , Claim (..)

    -- * Value
    , Value (..)

    -- * RoleARN
    , RoleARN (..)

    -- * CustomRoleArn
    , CustomRoleArn (..)

    -- * AccessKeyId
    , AccessKeyId (..)

    -- * SessionToken
    , SessionToken (..)

    -- * ProviderName
    , ProviderName (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.CognitoIdentity.Types.RoleType
  
import Network.AWS.CognitoIdentity.Types.IdentityProviderToken
  
import Network.AWS.CognitoIdentity.Types.CognitoIdentityProviderClientId
  
import Network.AWS.CognitoIdentity.Types.DeveloperUserIdentifier
  
import Network.AWS.CognitoIdentity.Types.SecretKeyString
  
  
  
import Network.AWS.CognitoIdentity.Types.IdentityPoolId
  
import Network.AWS.CognitoIdentity.Types.MappingRule
  
import Network.AWS.CognitoIdentity.Types.UnprocessedIdentityId
  
import Network.AWS.CognitoIdentity.Types.IdentityProviderId
  
import Network.AWS.CognitoIdentity.Types.IdentityPoolName
  
  
import Network.AWS.CognitoIdentity.Types.Credentials
  
import Network.AWS.CognitoIdentity.Types.CognitoIdentityProvider
  
import Network.AWS.CognitoIdentity.Types.IdentityProviderName
  
import Network.AWS.CognitoIdentity.Types.IdentityDescription
  
import Network.AWS.CognitoIdentity.Types.ARNString
  
import Network.AWS.CognitoIdentity.Types.DeveloperProviderName
  
  
import Network.AWS.CognitoIdentity.Types.RoleMapping
  
import Network.AWS.CognitoIdentity.Types.AmbiguousRoleResolutionType
  
import Network.AWS.CognitoIdentity.Types.MappingRuleMatchType
  
import Network.AWS.CognitoIdentity.Types.AccountId
  
  
  
  
import Network.AWS.CognitoIdentity.Types.IdentityPool
  
import Network.AWS.CognitoIdentity.Types.CognitoErrorCode
  
import Network.AWS.CognitoIdentity.Types.RulesConfigurationType
  
import Network.AWS.CognitoIdentity.Types.RoleMappingType
  
import Network.AWS.CognitoIdentity.Types.IdentityId
  
  
import Network.AWS.CognitoIdentity.Types.PaginationKey
  
import Network.AWS.CognitoIdentity.Types.TagKeysType
  
  
  
import Network.AWS.CognitoIdentity.Types.IdentityPoolShortDescription
  
import Network.AWS.CognitoIdentity.Types.TagValueType
  
  
import Network.AWS.CognitoIdentity.Types.Token
  
import Network.AWS.CognitoIdentity.Types.NextToken
  
import Network.AWS.CognitoIdentity.Types.Claim
  
import Network.AWS.CognitoIdentity.Types.Value
  
import Network.AWS.CognitoIdentity.Types.RoleARN
  
import Network.AWS.CognitoIdentity.Types.CustomRoleArn
  
import Network.AWS.CognitoIdentity.Types.AccessKeyId
  
import Network.AWS.CognitoIdentity.Types.SessionToken
  
import Network.AWS.CognitoIdentity.Types.ProviderName
  

-- | API version @2014-06-30@ of the Amazon Cognito Identity SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "CognitoIdentity",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "cognito-identity",
                 Core._svcVersion = "2014-06-30", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "CognitoIdentity",
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

-- | Thrown if the identity pool has no role associated for the given auth type (auth/unauth) or if the AssumeRole fails.
_InvalidIdentityPoolConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidIdentityPoolConfigurationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidIdentityPoolConfigurationException"
{-# INLINEABLE _InvalidIdentityPoolConfigurationException #-}
{-# DEPRECATED _InvalidIdentityPoolConfigurationException "Use generic-lens or generic-optics instead"  #-}

-- | Thrown for missing or bad input parameter(s).
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterException"
{-# INLINEABLE _InvalidParameterException #-}
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead"  #-}

-- | Thrown when a user is not authorized to access the requested resource.
_NotAuthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotAuthorizedException
  = Core._MatchServiceError mkServiceConfig "NotAuthorizedException"
{-# INLINEABLE _NotAuthorizedException #-}
{-# DEPRECATED _NotAuthorizedException "Use generic-lens or generic-optics instead"  #-}

-- | Thrown when the service encounters an error during processing the request.
_InternalErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalErrorException
  = Core._MatchServiceError mkServiceConfig "InternalErrorException"
{-# INLINEABLE _InternalErrorException #-}
{-# DEPRECATED _InternalErrorException "Use generic-lens or generic-optics instead"  #-}

-- | An exception thrown when a dependent service such as Facebook or Twitter is not responding
_ExternalServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExternalServiceException
  = Core._MatchServiceError mkServiceConfig
      "ExternalServiceException"
{-# INLINEABLE _ExternalServiceException #-}
{-# DEPRECATED _ExternalServiceException "Use generic-lens or generic-optics instead"  #-}

-- | Thrown when a request is throttled.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException
  = Core._MatchServiceError mkServiceConfig
      "TooManyRequestsException"
{-# INLINEABLE _TooManyRequestsException #-}
{-# DEPRECATED _TooManyRequestsException "Use generic-lens or generic-optics instead"  #-}

-- | Thrown if there are parallel requests to modify a resource.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException
  = Core._MatchServiceError mkServiceConfig
      "ConcurrentModificationException"
{-# INLINEABLE _ConcurrentModificationException #-}
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead"  #-}

-- | Thrown when a user tries to use a login which is already linked to another account.
_ResourceConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceConflictException
  = Core._MatchServiceError mkServiceConfig
      "ResourceConflictException"
{-# INLINEABLE _ResourceConflictException #-}
{-# DEPRECATED _ResourceConflictException "Use generic-lens or generic-optics instead"  #-}

-- | The provided developer user identifier is already registered with Cognito under a different identity ID.
_DeveloperUserAlreadyRegisteredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeveloperUserAlreadyRegisteredException
  = Core._MatchServiceError mkServiceConfig
      "DeveloperUserAlreadyRegisteredException"
{-# INLINEABLE _DeveloperUserAlreadyRegisteredException #-}
{-# DEPRECATED _DeveloperUserAlreadyRegisteredException "Use generic-lens or generic-optics instead"  #-}

-- | Thrown when the requested resource (for example, a dataset or record) does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | Thrown when the total number of user pools has exceeded a preset limit.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}
