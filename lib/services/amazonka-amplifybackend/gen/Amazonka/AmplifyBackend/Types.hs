{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AmplifyBackend.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BadRequestException,
    _GatewayTimeoutException,
    _NotFoundException,
    _TooManyRequestsException,

    -- * AdditionalConstraintsElement
    AdditionalConstraintsElement (..),

    -- * AuthResources
    AuthResources (..),

    -- * AuthenticatedElement
    AuthenticatedElement (..),

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

    -- * ServiceName
    ServiceName (..),

    -- * SignInMethod
    SignInMethod (..),

    -- * Status
    Status (..),

    -- * UnAuthenticatedElement
    UnAuthenticatedElement (..),

    -- * BackendAPIAppSyncAuthSettings
    BackendAPIAppSyncAuthSettings (..),
    newBackendAPIAppSyncAuthSettings,
    backendAPIAppSyncAuthSettings_cognitoUserPoolId,
    backendAPIAppSyncAuthSettings_description,
    backendAPIAppSyncAuthSettings_expirationTime,
    backendAPIAppSyncAuthSettings_openIDAuthTTL,
    backendAPIAppSyncAuthSettings_openIDClientId,
    backendAPIAppSyncAuthSettings_openIDIatTTL,
    backendAPIAppSyncAuthSettings_openIDIssueURL,
    backendAPIAppSyncAuthSettings_openIDProviderName,

    -- * BackendAPIAuthType
    BackendAPIAuthType (..),
    newBackendAPIAuthType,
    backendAPIAuthType_mode,
    backendAPIAuthType_settings,

    -- * BackendAPIConflictResolution
    BackendAPIConflictResolution (..),
    newBackendAPIConflictResolution,
    backendAPIConflictResolution_resolutionStrategy,

    -- * BackendAPIResourceConfig
    BackendAPIResourceConfig (..),
    newBackendAPIResourceConfig,
    backendAPIResourceConfig_additionalAuthTypes,
    backendAPIResourceConfig_apiName,
    backendAPIResourceConfig_conflictResolution,
    backendAPIResourceConfig_defaultAuthType,
    backendAPIResourceConfig_service,
    backendAPIResourceConfig_transformSchema,

    -- * BackendAuthAppleProviderConfig
    BackendAuthAppleProviderConfig (..),
    newBackendAuthAppleProviderConfig,
    backendAuthAppleProviderConfig_clientId,
    backendAuthAppleProviderConfig_keyId,
    backendAuthAppleProviderConfig_privateKey,
    backendAuthAppleProviderConfig_teamId,

    -- * BackendAuthSocialProviderConfig
    BackendAuthSocialProviderConfig (..),
    newBackendAuthSocialProviderConfig,
    backendAuthSocialProviderConfig_clientId,
    backendAuthSocialProviderConfig_clientSecret,

    -- * BackendJobRespObj
    BackendJobRespObj (..),
    newBackendJobRespObj,
    backendJobRespObj_createTime,
    backendJobRespObj_error,
    backendJobRespObj_jobId,
    backendJobRespObj_operation,
    backendJobRespObj_status,
    backendJobRespObj_updateTime,
    backendJobRespObj_appId,
    backendJobRespObj_backendEnvironmentName,

    -- * BackendStoragePermissions
    BackendStoragePermissions (..),
    newBackendStoragePermissions,
    backendStoragePermissions_unAuthenticated,
    backendStoragePermissions_authenticated,

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
    createBackendAuthOAuthConfig_domainPrefix,
    createBackendAuthOAuthConfig_socialProviderSettings,
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
    createBackendAuthUserPoolConfig_forgotPassword,
    createBackendAuthUserPoolConfig_mfa,
    createBackendAuthUserPoolConfig_oAuth,
    createBackendAuthUserPoolConfig_passwordPolicy,
    createBackendAuthUserPoolConfig_verificationMessage,
    createBackendAuthUserPoolConfig_requiredSignUpAttributes,
    createBackendAuthUserPoolConfig_signInMethod,
    createBackendAuthUserPoolConfig_userPoolName,

    -- * CreateBackendAuthVerificationMessageConfig
    CreateBackendAuthVerificationMessageConfig (..),
    newCreateBackendAuthVerificationMessageConfig,
    createBackendAuthVerificationMessageConfig_emailSettings,
    createBackendAuthVerificationMessageConfig_smsSettings,
    createBackendAuthVerificationMessageConfig_deliveryMethod,

    -- * CreateBackendStorageResourceConfig
    CreateBackendStorageResourceConfig (..),
    newCreateBackendStorageResourceConfig,
    createBackendStorageResourceConfig_bucketName,
    createBackendStorageResourceConfig_serviceName,
    createBackendStorageResourceConfig_permissions,

    -- * EmailSettings
    EmailSettings (..),
    newEmailSettings,
    emailSettings_emailMessage,
    emailSettings_emailSubject,

    -- * GetBackendStorageResourceConfig
    GetBackendStorageResourceConfig (..),
    newGetBackendStorageResourceConfig,
    getBackendStorageResourceConfig_bucketName,
    getBackendStorageResourceConfig_permissions,
    getBackendStorageResourceConfig_serviceName,
    getBackendStorageResourceConfig_imported,

    -- * LoginAuthConfigReqObj
    LoginAuthConfigReqObj (..),
    newLoginAuthConfigReqObj,
    loginAuthConfigReqObj_awsCognitoIdentityPoolId,
    loginAuthConfigReqObj_awsCognitoRegion,
    loginAuthConfigReqObj_awsUserPoolsId,
    loginAuthConfigReqObj_awsUserPoolsWebClientId,

    -- * ResourceConfig
    ResourceConfig (..),
    newResourceConfig,

    -- * S3BucketInfo
    S3BucketInfo (..),
    newS3BucketInfo,
    s3BucketInfo_creationDate,
    s3BucketInfo_name,

    -- * Settings
    Settings (..),
    newSettings,
    settings_mfaTypes,
    settings_smsMessage,

    -- * SmsSettings
    SmsSettings (..),
    newSmsSettings,
    smsSettings_smsMessage,

    -- * SocialProviderSettings
    SocialProviderSettings (..),
    newSocialProviderSettings,
    socialProviderSettings_facebook,
    socialProviderSettings_google,
    socialProviderSettings_loginWithAmazon,
    socialProviderSettings_signInWithApple,

    -- * UpdateBackendAuthForgotPasswordConfig
    UpdateBackendAuthForgotPasswordConfig (..),
    newUpdateBackendAuthForgotPasswordConfig,
    updateBackendAuthForgotPasswordConfig_deliveryMethod,
    updateBackendAuthForgotPasswordConfig_emailSettings,
    updateBackendAuthForgotPasswordConfig_smsSettings,

    -- * UpdateBackendAuthIdentityPoolConfig
    UpdateBackendAuthIdentityPoolConfig (..),
    newUpdateBackendAuthIdentityPoolConfig,
    updateBackendAuthIdentityPoolConfig_unauthenticatedLogin,

    -- * UpdateBackendAuthMFAConfig
    UpdateBackendAuthMFAConfig (..),
    newUpdateBackendAuthMFAConfig,
    updateBackendAuthMFAConfig_mfaMode,
    updateBackendAuthMFAConfig_settings,

    -- * UpdateBackendAuthOAuthConfig
    UpdateBackendAuthOAuthConfig (..),
    newUpdateBackendAuthOAuthConfig,
    updateBackendAuthOAuthConfig_domainPrefix,
    updateBackendAuthOAuthConfig_oAuthGrantType,
    updateBackendAuthOAuthConfig_oAuthScopes,
    updateBackendAuthOAuthConfig_redirectSignInURIs,
    updateBackendAuthOAuthConfig_redirectSignOutURIs,
    updateBackendAuthOAuthConfig_socialProviderSettings,

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
    updateBackendAuthUserPoolConfig_forgotPassword,
    updateBackendAuthUserPoolConfig_mfa,
    updateBackendAuthUserPoolConfig_oAuth,
    updateBackendAuthUserPoolConfig_passwordPolicy,
    updateBackendAuthUserPoolConfig_verificationMessage,

    -- * UpdateBackendAuthVerificationMessageConfig
    UpdateBackendAuthVerificationMessageConfig (..),
    newUpdateBackendAuthVerificationMessageConfig,
    updateBackendAuthVerificationMessageConfig_emailSettings,
    updateBackendAuthVerificationMessageConfig_smsSettings,
    updateBackendAuthVerificationMessageConfig_deliveryMethod,

    -- * UpdateBackendStorageResourceConfig
    UpdateBackendStorageResourceConfig (..),
    newUpdateBackendStorageResourceConfig,
    updateBackendStorageResourceConfig_serviceName,
    updateBackendStorageResourceConfig_permissions,
  )
where

import Amazonka.AmplifyBackend.Types.AdditionalConstraintsElement
import Amazonka.AmplifyBackend.Types.AuthResources
import Amazonka.AmplifyBackend.Types.AuthenticatedElement
import Amazonka.AmplifyBackend.Types.BackendAPIAppSyncAuthSettings
import Amazonka.AmplifyBackend.Types.BackendAPIAuthType
import Amazonka.AmplifyBackend.Types.BackendAPIConflictResolution
import Amazonka.AmplifyBackend.Types.BackendAPIResourceConfig
import Amazonka.AmplifyBackend.Types.BackendAuthAppleProviderConfig
import Amazonka.AmplifyBackend.Types.BackendAuthSocialProviderConfig
import Amazonka.AmplifyBackend.Types.BackendJobRespObj
import Amazonka.AmplifyBackend.Types.BackendStoragePermissions
import Amazonka.AmplifyBackend.Types.CreateBackendAuthForgotPasswordConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthIdentityPoolConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthMFAConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthOAuthConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthPasswordPolicyConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthResourceConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthUserPoolConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthVerificationMessageConfig
import Amazonka.AmplifyBackend.Types.CreateBackendStorageResourceConfig
import Amazonka.AmplifyBackend.Types.DeliveryMethod
import Amazonka.AmplifyBackend.Types.EmailSettings
import Amazonka.AmplifyBackend.Types.GetBackendStorageResourceConfig
import Amazonka.AmplifyBackend.Types.LoginAuthConfigReqObj
import Amazonka.AmplifyBackend.Types.MFAMode
import Amazonka.AmplifyBackend.Types.MfaTypesElement
import Amazonka.AmplifyBackend.Types.Mode
import Amazonka.AmplifyBackend.Types.OAuthGrantType
import Amazonka.AmplifyBackend.Types.OAuthScopesElement
import Amazonka.AmplifyBackend.Types.RequiredSignUpAttributesElement
import Amazonka.AmplifyBackend.Types.ResolutionStrategy
import Amazonka.AmplifyBackend.Types.ResourceConfig
import Amazonka.AmplifyBackend.Types.S3BucketInfo
import Amazonka.AmplifyBackend.Types.Service
import Amazonka.AmplifyBackend.Types.ServiceName
import Amazonka.AmplifyBackend.Types.Settings
import Amazonka.AmplifyBackend.Types.SignInMethod
import Amazonka.AmplifyBackend.Types.SmsSettings
import Amazonka.AmplifyBackend.Types.SocialProviderSettings
import Amazonka.AmplifyBackend.Types.Status
import Amazonka.AmplifyBackend.Types.UnAuthenticatedElement
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthForgotPasswordConfig
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthIdentityPoolConfig
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthMFAConfig
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthOAuthConfig
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthPasswordPolicyConfig
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthResourceConfig
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthUserPoolConfig
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthVerificationMessageConfig
import Amazonka.AmplifyBackend.Types.UpdateBackendStorageResourceConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-08-11@ of the Amazon AmplifyBackend SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "AmplifyBackend",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "amplifybackend",
      Core.signingName = "amplifybackend",
      Core.version = "2020-08-11",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "AmplifyBackend",
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

-- | An error returned if a request is not formed properly.
_BadRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | An error returned if there\'s a temporary issue with the service.
_GatewayTimeoutException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_GatewayTimeoutException =
  Core._MatchServiceError
    defaultService
    "GatewayTimeoutException"
    Prelude.. Core.hasStatus 504

-- | An error returned when a specific resource type is not found.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | An error that is returned when a limit of a specific type has been
-- exceeded.
_TooManyRequestsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429
