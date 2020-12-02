{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types
  ( -- * Service Configuration
    cognitoIdentity,

    -- * Errors

    -- * AmbiguousRoleResolutionType
    AmbiguousRoleResolutionType (..),

    -- * CognitoErrorCode
    CognitoErrorCode (..),

    -- * MappingRuleMatchType
    MappingRuleMatchType (..),

    -- * RoleMappingType
    RoleMappingType (..),

    -- * CognitoIdentityProvider
    CognitoIdentityProvider,
    cognitoIdentityProvider,
    cipClientId,
    cipServerSideTokenCheck,
    cipProviderName,

    -- * Credentials
    Credentials,
    credentials,
    cSessionToken,
    cExpiration,
    cSecretKey,
    cAccessKeyId,

    -- * IdentityDescription
    IdentityDescription,
    identityDescription,
    idLastModifiedDate,
    idCreationDate,
    idLogins,
    idIdentityId,

    -- * IdentityPool
    IdentityPool,
    identityPool,
    ipSamlProviderARNs,
    ipSupportedLoginProviders,
    ipAllowClassicFlow,
    ipDeveloperProviderName,
    ipIdentityPoolTags,
    ipOpenIdConnectProviderARNs,
    ipCognitoIdentityProviders,
    ipIdentityPoolId,
    ipIdentityPoolName,
    ipAllowUnauthenticatedIdentities,

    -- * IdentityPoolShortDescription
    IdentityPoolShortDescription,
    identityPoolShortDescription,
    ipsdIdentityPoolId,
    ipsdIdentityPoolName,

    -- * MappingRule
    MappingRule,
    mappingRule,
    mrClaim,
    mrMatchType,
    mrValue,
    mrRoleARN,

    -- * RoleMapping
    RoleMapping,
    roleMapping,
    rmRulesConfiguration,
    rmAmbiguousRoleResolution,
    rmType,

    -- * RulesConfigurationType
    RulesConfigurationType,
    rulesConfigurationType,
    rctRules,

    -- * UnprocessedIdentityId
    UnprocessedIdentityId,
    unprocessedIdentityId,
    uiiErrorCode,
    uiiIdentityId,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2014-06-30@ of the Amazon Cognito Identity SDK configuration.
cognitoIdentity :: Service
cognitoIdentity =
  Service
    { _svcAbbrev = "CognitoIdentity",
      _svcSigner = v4,
      _svcPrefix = "cognito-identity",
      _svcVersion = "2014-06-30",
      _svcEndpoint = defaultEndpoint cognitoIdentity,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "CognitoIdentity",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
