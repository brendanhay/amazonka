-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types
  ( -- * Service configuration
    cognitoIdentityService,

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
    CognitoIdentityProvider (..),
    mkCognitoIdentityProvider,
    cipClientId,
    cipServerSideTokenCheck,
    cipProviderName,

    -- * Credentials
    Credentials (..),
    mkCredentials,
    cSessionToken,
    cExpiration,
    cSecretKey,
    cAccessKeyId,

    -- * IdentityDescription
    IdentityDescription (..),
    mkIdentityDescription,
    idLastModifiedDate,
    idCreationDate,
    idLogins,
    idIdentityId,

    -- * IdentityPool
    IdentityPool (..),
    mkIdentityPool,
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
    IdentityPoolShortDescription (..),
    mkIdentityPoolShortDescription,
    ipsdIdentityPoolId,
    ipsdIdentityPoolName,

    -- * MappingRule
    MappingRule (..),
    mkMappingRule,
    mrClaim,
    mrMatchType,
    mrValue,
    mrRoleARN,

    -- * RoleMapping
    RoleMapping (..),
    mkRoleMapping,
    rmRulesConfiguration,
    rmAmbiguousRoleResolution,
    rmType,

    -- * RulesConfigurationType
    RulesConfigurationType (..),
    mkRulesConfigurationType,
    rctRules,

    -- * UnprocessedIdentityId
    UnprocessedIdentityId (..),
    mkUnprocessedIdentityId,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-06-30@ of the Amazon Cognito Identity SDK configuration.
cognitoIdentityService :: Lude.Service
cognitoIdentityService =
  Lude.Service
    { Lude._svcAbbrev = "CognitoIdentity",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "cognito-identity",
      Lude._svcVersion = "2014-06-30",
      Lude._svcEndpoint = Lude.defaultEndpoint cognitoIdentityService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "CognitoIdentity",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
