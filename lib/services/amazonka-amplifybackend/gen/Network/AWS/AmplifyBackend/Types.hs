{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmplifyBackend.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AmplifyBackend.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _GatewayTimeoutException,
    _NotFoundException,
    _TooManyRequestsException,
    _BadRequestException,

    -- * AdditionalConstraintsElement
    AdditionalConstraintsElement (..),

    -- * AuthResources
    AuthResources (..),

    -- * DeliveryMethod
    DeliveryMethod (..),

    -- * MFAMode
    MFAMode (..),

    -- * MfaTypesElement
    MfaTypesElement (..),

    -- * Mode
    Mode (..),

    -- * OAuthGrantType
    OAuthGrantType (..),

    -- * OAuthScopesElement
    OAuthScopesElement (..),

    -- * RequiredSignUpAttributesElement
    RequiredSignUpAttributesElement (..),

    -- * ResolutionStrategy
    ResolutionStrategy (..),

    -- * Service
    Service (..),

    -- * SignInMethod
    SignInMethod (..),

    -- * Status
    Status (..),

    -- * BackendAPIAppSyncAuthSettings
    BackendAPIAppSyncAuthSettings (..),
    newBackendAPIAppSyncAuthSettings,
    backendAPIAppSyncAuthSettings_openIDProviderName,
    backendAPIAppSyncAuthSettings_openIDClientId,
    backendAPIAppSyncAuthSettings_openIDIssueURL,
    backendAPIAppSyncAuthSettings_cognitoUserPoolId,
    backendAPIAppSyncAuthSettings_openIDAuthTTL,
    backendAPIAppSyncAuthSettings_description,
    backendAPIAppSyncAuthSettings_expirationTime,
    backendAPIAppSyncAuthSettings_openIDIatTTL,

    -- * BackendAPIAuthType
    BackendAPIAuthType (..),
    newBackendAPIAuthType,
    backendAPIAuthType_settings,
    backendAPIAuthType_mode,

    -- * BackendAPIConflictResolution
    BackendAPIConflictResolution (..),
    newBackendAPIConflictResolution,
    backendAPIConflictResolution_resolutionStrategy,

    -- * BackendAPIResourceConfig
    BackendAPIResourceConfig (..),
    newBackendAPIResourceConfig,
    backendAPIResourceConfig_apiName,
    backendAPIResourceConfig_service,
    backendAPIResourceConfig_transformSchema,
    backendAPIResourceConfig_additionalAuthTypes,
    backendAPIResourceConfig_conflictResolution,
    backendAPIResourceConfig_defaultAuthType,

    -- * BackendAuthAppleProviderConfig
    BackendAuthAppleProviderConfig (..),
    newBackendAuthAppleProviderConfig,
    backendAuthAppleProviderConfig_clientId,
    backendAuthAppleProviderConfig_privateKey,
    backendAuthAppleProviderConfig_keyId,
    backendAuthAppleProviderConfig_teamId,

    -- * BackendAuthSocialProviderConfig
    BackendAuthSocialProviderConfig (..),
    newBackendAuthSocialProviderConfig,
    backendAuthSocialProviderConfig_clientId,
    backendAuthSocialProviderConfig_clientSecret,

    -- * BackendJobRespObj
    BackendJobRespObj (..),
    newBackendJobRespObj,
    backendJobRespObj_status,
    backendJobRespObj_jobId,
    backendJobRespObj_operation,
    backendJobRespObj_error,
    backendJobRespObj_updateTime,
    backendJobRespObj_createTime,
    backendJobRespObj_appId,
    backendJobRespObj_backendEnvironmentName,

    -- * CreateBackendAuthForgotPasswordConfig
    CreateBackendAuthForgotPasswordConfig (..),
    newCreateBackendAuthForgotPasswordConfig,
    createBackendAuthForgotPasswordConfig_emailSettings,
    createBackendAuthForgotPasswordConfig_smsSettings,
    createBackendAuthForgotPasswordConfig_deliveryMethod,

    -- * CreateBackendAuthIdentityPoolConfig
    CreateBackendAuthIdentityPoolConfig (..),
    newCreateBackendAuthIdentityPoolConfig,
    createBackendAuthIdentityPoolConfig_unauthenticatedLogin,
    createBackendAuthIdentityPoolConfig_identityPoolName,

    -- * CreateBackendAuthMFAConfig
    CreateBackendAuthMFAConfig (..),
    newCreateBackendAuthMFAConfig,
    createBackendAuthMFAConfig_settings,
    createBackendAuthMFAConfig_mfaMode,

    -- * CreateBackendAuthOAuthConfig
    CreateBackendAuthOAuthConfig (..),
    newCreateBackendAuthOAuthConfig,
    createBackendAuthOAuthConfig_socialProviderSettings,
    createBackendAuthOAuthConfig_domainPrefix,
    createBackendAuthOAuthConfig_redirectSignOutURIs,
    createBackendAuthOAuthConfig_redirectSignInURIs,
    createBackendAuthOAuthConfig_oAuthGrantType,
    createBackendAuthOAuthConfig_oAuthScopes,

    -- * CreateBackendAuthPasswordPolicyConfig
    CreateBackendAuthPasswordPolicyConfig (..),
    newCreateBackendAuthPasswordPolicyConfig,
    createBackendAuthPasswordPolicyConfig_additionalConstraints,
    createBackendAuthPasswordPolicyConfig_minimumLength,

    -- * CreateBackendAuthResourceConfig
    CreateBackendAuthResourceConfig (..),
    newCreateBackendAuthResourceConfig,
    createBackendAuthResourceConfig_identityPoolConfigs,
    createBackendAuthResourceConfig_authResources,
    createBackendAuthResourceConfig_userPoolConfigs,
    createBackendAuthResourceConfig_service,

    -- * CreateBackendAuthUserPoolConfig
    CreateBackendAuthUserPoolConfig (..),
    newCreateBackendAuthUserPoolConfig,
    createBackendAuthUserPoolConfig_passwordPolicy,
    createBackendAuthUserPoolConfig_mfa,
    createBackendAuthUserPoolConfig_forgotPassword,
    createBackendAuthUserPoolConfig_oAuth,
    createBackendAuthUserPoolConfig_requiredSignUpAttributes,
    createBackendAuthUserPoolConfig_signInMethod,
    createBackendAuthUserPoolConfig_userPoolName,

    -- * EmailSettings
    EmailSettings (..),
    newEmailSettings,
    emailSettings_emailSubject,
    emailSettings_emailMessage,

    -- * LoginAuthConfigReqObj
    LoginAuthConfigReqObj (..),
    newLoginAuthConfigReqObj,
    loginAuthConfigReqObj_awsUserPoolsWebClientId,
    loginAuthConfigReqObj_awsCognitoIdentityPoolId,
    loginAuthConfigReqObj_awsUserPoolsId,
    loginAuthConfigReqObj_awsCognitoRegion,

    -- * ResourceConfig
    ResourceConfig (..),
    newResourceConfig,

    -- * Settings
    Settings (..),
    newSettings,
    settings_smsMessage,
    settings_mfaTypes,

    -- * SmsSettings
    SmsSettings (..),
    newSmsSettings,
    smsSettings_smsMessage,

    -- * SocialProviderSettings
    SocialProviderSettings (..),
    newSocialProviderSettings,
    socialProviderSettings_loginWithAmazon,
    socialProviderSettings_signInWithApple,
    socialProviderSettings_facebook,
    socialProviderSettings_google,

    -- * UpdateBackendAuthForgotPasswordConfig
    UpdateBackendAuthForgotPasswordConfig (..),
    newUpdateBackendAuthForgotPasswordConfig,
    updateBackendAuthForgotPasswordConfig_emailSettings,
    updateBackendAuthForgotPasswordConfig_smsSettings,
    updateBackendAuthForgotPasswordConfig_deliveryMethod,

    -- * UpdateBackendAuthIdentityPoolConfig
    UpdateBackendAuthIdentityPoolConfig (..),
    newUpdateBackendAuthIdentityPoolConfig,
    updateBackendAuthIdentityPoolConfig_unauthenticatedLogin,

    -- * UpdateBackendAuthMFAConfig
    UpdateBackendAuthMFAConfig (..),
    newUpdateBackendAuthMFAConfig,
    updateBackendAuthMFAConfig_settings,
    updateBackendAuthMFAConfig_mfaMode,

    -- * UpdateBackendAuthOAuthConfig
    UpdateBackendAuthOAuthConfig (..),
    newUpdateBackendAuthOAuthConfig,
    updateBackendAuthOAuthConfig_socialProviderSettings,
    updateBackendAuthOAuthConfig_domainPrefix,
    updateBackendAuthOAuthConfig_oAuthScopes,
    updateBackendAuthOAuthConfig_oAuthGrantType,
    updateBackendAuthOAuthConfig_redirectSignOutURIs,
    updateBackendAuthOAuthConfig_redirectSignInURIs,

    -- * UpdateBackendAuthPasswordPolicyConfig
    UpdateBackendAuthPasswordPolicyConfig (..),
    newUpdateBackendAuthPasswordPolicyConfig,
    updateBackendAuthPasswordPolicyConfig_additionalConstraints,
    updateBackendAuthPasswordPolicyConfig_minimumLength,

    -- * UpdateBackendAuthResourceConfig
    UpdateBackendAuthResourceConfig (..),
    newUpdateBackendAuthResourceConfig,
    updateBackendAuthResourceConfig_identityPoolConfigs,
    updateBackendAuthResourceConfig_authResources,
    updateBackendAuthResourceConfig_userPoolConfigs,
    updateBackendAuthResourceConfig_service,

    -- * UpdateBackendAuthUserPoolConfig
    UpdateBackendAuthUserPoolConfig (..),
    newUpdateBackendAuthUserPoolConfig,
    updateBackendAuthUserPoolConfig_passwordPolicy,
    updateBackendAuthUserPoolConfig_mfa,
    updateBackendAuthUserPoolConfig_forgotPassword,
    updateBackendAuthUserPoolConfig_oAuth,
  )
where

import Network.AWS.AmplifyBackend.Types.AdditionalConstraintsElement
import Network.AWS.AmplifyBackend.Types.AuthResources
import Network.AWS.AmplifyBackend.Types.BackendAPIAppSyncAuthSettings
import Network.AWS.AmplifyBackend.Types.BackendAPIAuthType
import Network.AWS.AmplifyBackend.Types.BackendAPIConflictResolution
import Network.AWS.AmplifyBackend.Types.BackendAPIResourceConfig
import Network.AWS.AmplifyBackend.Types.BackendAuthAppleProviderConfig
import Network.AWS.AmplifyBackend.Types.BackendAuthSocialProviderConfig
import Network.AWS.AmplifyBackend.Types.BackendJobRespObj
import Network.AWS.AmplifyBackend.Types.CreateBackendAuthForgotPasswordConfig
import Network.AWS.AmplifyBackend.Types.CreateBackendAuthIdentityPoolConfig
import Network.AWS.AmplifyBackend.Types.CreateBackendAuthMFAConfig
import Network.AWS.AmplifyBackend.Types.CreateBackendAuthOAuthConfig
import Network.AWS.AmplifyBackend.Types.CreateBackendAuthPasswordPolicyConfig
import Network.AWS.AmplifyBackend.Types.CreateBackendAuthResourceConfig
import Network.AWS.AmplifyBackend.Types.CreateBackendAuthUserPoolConfig
import Network.AWS.AmplifyBackend.Types.DeliveryMethod
import Network.AWS.AmplifyBackend.Types.EmailSettings
import Network.AWS.AmplifyBackend.Types.LoginAuthConfigReqObj
import Network.AWS.AmplifyBackend.Types.MFAMode
import Network.AWS.AmplifyBackend.Types.MfaTypesElement
import Network.AWS.AmplifyBackend.Types.Mode
import Network.AWS.AmplifyBackend.Types.OAuthGrantType
import Network.AWS.AmplifyBackend.Types.OAuthScopesElement
import Network.AWS.AmplifyBackend.Types.RequiredSignUpAttributesElement
import Network.AWS.AmplifyBackend.Types.ResolutionStrategy
import Network.AWS.AmplifyBackend.Types.ResourceConfig
import Network.AWS.AmplifyBackend.Types.Service
import Network.AWS.AmplifyBackend.Types.Settings
import Network.AWS.AmplifyBackend.Types.SignInMethod
import Network.AWS.AmplifyBackend.Types.SmsSettings
import Network.AWS.AmplifyBackend.Types.SocialProviderSettings
import Network.AWS.AmplifyBackend.Types.Status
import Network.AWS.AmplifyBackend.Types.UpdateBackendAuthForgotPasswordConfig
import Network.AWS.AmplifyBackend.Types.UpdateBackendAuthIdentityPoolConfig
import Network.AWS.AmplifyBackend.Types.UpdateBackendAuthMFAConfig
import Network.AWS.AmplifyBackend.Types.UpdateBackendAuthOAuthConfig
import Network.AWS.AmplifyBackend.Types.UpdateBackendAuthPasswordPolicyConfig
import Network.AWS.AmplifyBackend.Types.UpdateBackendAuthResourceConfig
import Network.AWS.AmplifyBackend.Types.UpdateBackendAuthUserPoolConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-08-11@ of the Amazon AmplifyBackend SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "AmplifyBackend",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "amplifybackend",
      Core._serviceSigningName = "amplifybackend",
      Core._serviceVersion = "2020-08-11",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "AmplifyBackend",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | An error returned if there\'s a temporary issue with the service.
_GatewayTimeoutException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GatewayTimeoutException =
  Core._MatchServiceError
    defaultService
    "GatewayTimeoutException"
    Prelude.. Core.hasStatus 504

-- | An error returned when a specific resource type is not found.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | An error that is returned when a limit of a specific type has been
-- exceeded.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | An error returned if a request is not formed properly.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400
