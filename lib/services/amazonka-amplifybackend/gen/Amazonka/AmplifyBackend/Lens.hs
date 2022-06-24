{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AmplifyBackend.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Lens
  ( -- * Operations

    -- ** CloneBackend
    cloneBackend_appId,
    cloneBackend_backendEnvironmentName,
    cloneBackend_targetEnvironmentName,
    cloneBackendResponse_jobId,
    cloneBackendResponse_status,
    cloneBackendResponse_error,
    cloneBackendResponse_operation,
    cloneBackendResponse_appId,
    cloneBackendResponse_backendEnvironmentName,
    cloneBackendResponse_httpStatus,

    -- ** CreateBackend
    createBackend_resourceName,
    createBackend_resourceConfig,
    createBackend_appId,
    createBackend_backendEnvironmentName,
    createBackend_appName,
    createBackendResponse_jobId,
    createBackendResponse_status,
    createBackendResponse_error,
    createBackendResponse_operation,
    createBackendResponse_appId,
    createBackendResponse_backendEnvironmentName,
    createBackendResponse_httpStatus,

    -- ** CreateBackendAPI
    createBackendAPI_appId,
    createBackendAPI_resourceName,
    createBackendAPI_backendEnvironmentName,
    createBackendAPI_resourceConfig,
    createBackendAPIResponse_jobId,
    createBackendAPIResponse_status,
    createBackendAPIResponse_error,
    createBackendAPIResponse_operation,
    createBackendAPIResponse_appId,
    createBackendAPIResponse_backendEnvironmentName,
    createBackendAPIResponse_httpStatus,

    -- ** CreateBackendAuth
    createBackendAuth_appId,
    createBackendAuth_resourceName,
    createBackendAuth_backendEnvironmentName,
    createBackendAuth_resourceConfig,
    createBackendAuthResponse_jobId,
    createBackendAuthResponse_status,
    createBackendAuthResponse_error,
    createBackendAuthResponse_operation,
    createBackendAuthResponse_appId,
    createBackendAuthResponse_backendEnvironmentName,
    createBackendAuthResponse_httpStatus,

    -- ** CreateBackendConfig
    createBackendConfig_backendManagerAppId,
    createBackendConfig_appId,
    createBackendConfigResponse_jobId,
    createBackendConfigResponse_status,
    createBackendConfigResponse_appId,
    createBackendConfigResponse_backendEnvironmentName,
    createBackendConfigResponse_httpStatus,

    -- ** CreateToken
    createToken_appId,
    createTokenResponse_challengeCode,
    createTokenResponse_ttl,
    createTokenResponse_sessionId,
    createTokenResponse_appId,
    createTokenResponse_httpStatus,

    -- ** DeleteBackend
    deleteBackend_appId,
    deleteBackend_backendEnvironmentName,
    deleteBackendResponse_jobId,
    deleteBackendResponse_status,
    deleteBackendResponse_error,
    deleteBackendResponse_operation,
    deleteBackendResponse_appId,
    deleteBackendResponse_backendEnvironmentName,
    deleteBackendResponse_httpStatus,

    -- ** DeleteBackendAPI
    deleteBackendAPI_resourceConfig,
    deleteBackendAPI_appId,
    deleteBackendAPI_backendEnvironmentName,
    deleteBackendAPI_resourceName,
    deleteBackendAPIResponse_jobId,
    deleteBackendAPIResponse_status,
    deleteBackendAPIResponse_error,
    deleteBackendAPIResponse_operation,
    deleteBackendAPIResponse_appId,
    deleteBackendAPIResponse_backendEnvironmentName,
    deleteBackendAPIResponse_httpStatus,

    -- ** DeleteBackendAuth
    deleteBackendAuth_appId,
    deleteBackendAuth_backendEnvironmentName,
    deleteBackendAuth_resourceName,
    deleteBackendAuthResponse_jobId,
    deleteBackendAuthResponse_status,
    deleteBackendAuthResponse_error,
    deleteBackendAuthResponse_operation,
    deleteBackendAuthResponse_appId,
    deleteBackendAuthResponse_backendEnvironmentName,
    deleteBackendAuthResponse_httpStatus,

    -- ** DeleteToken
    deleteToken_sessionId,
    deleteToken_appId,
    deleteTokenResponse_isSuccess,
    deleteTokenResponse_httpStatus,

    -- ** GenerateBackendAPIModels
    generateBackendAPIModels_appId,
    generateBackendAPIModels_backendEnvironmentName,
    generateBackendAPIModels_resourceName,
    generateBackendAPIModelsResponse_jobId,
    generateBackendAPIModelsResponse_status,
    generateBackendAPIModelsResponse_error,
    generateBackendAPIModelsResponse_operation,
    generateBackendAPIModelsResponse_appId,
    generateBackendAPIModelsResponse_backendEnvironmentName,
    generateBackendAPIModelsResponse_httpStatus,

    -- ** GetBackend
    getBackend_backendEnvironmentName,
    getBackend_appId,
    getBackendResponse_appName,
    getBackendResponse_amplifyFeatureFlags,
    getBackendResponse_backendEnvironmentList,
    getBackendResponse_amplifyMetaConfig,
    getBackendResponse_error,
    getBackendResponse_appId,
    getBackendResponse_backendEnvironmentName,
    getBackendResponse_httpStatus,

    -- ** GetBackendAPI
    getBackendAPI_resourceConfig,
    getBackendAPI_appId,
    getBackendAPI_backendEnvironmentName,
    getBackendAPI_resourceName,
    getBackendAPIResponse_resourceName,
    getBackendAPIResponse_resourceConfig,
    getBackendAPIResponse_error,
    getBackendAPIResponse_appId,
    getBackendAPIResponse_backendEnvironmentName,
    getBackendAPIResponse_httpStatus,

    -- ** GetBackendAPIModels
    getBackendAPIModels_appId,
    getBackendAPIModels_backendEnvironmentName,
    getBackendAPIModels_resourceName,
    getBackendAPIModelsResponse_models,
    getBackendAPIModelsResponse_status,
    getBackendAPIModelsResponse_httpStatus,

    -- ** GetBackendAuth
    getBackendAuth_appId,
    getBackendAuth_backendEnvironmentName,
    getBackendAuth_resourceName,
    getBackendAuthResponse_resourceName,
    getBackendAuthResponse_resourceConfig,
    getBackendAuthResponse_error,
    getBackendAuthResponse_appId,
    getBackendAuthResponse_backendEnvironmentName,
    getBackendAuthResponse_httpStatus,

    -- ** GetBackendJob
    getBackendJob_appId,
    getBackendJob_backendEnvironmentName,
    getBackendJob_jobId,
    getBackendJobResponse_jobId,
    getBackendJobResponse_status,
    getBackendJobResponse_updateTime,
    getBackendJobResponse_createTime,
    getBackendJobResponse_error,
    getBackendJobResponse_operation,
    getBackendJobResponse_appId,
    getBackendJobResponse_backendEnvironmentName,
    getBackendJobResponse_httpStatus,

    -- ** GetToken
    getToken_sessionId,
    getToken_appId,
    getTokenResponse_challengeCode,
    getTokenResponse_ttl,
    getTokenResponse_sessionId,
    getTokenResponse_appId,
    getTokenResponse_httpStatus,

    -- ** ImportBackendAuth
    importBackendAuth_identityPoolId,
    importBackendAuth_appId,
    importBackendAuth_backendEnvironmentName,
    importBackendAuth_userPoolId,
    importBackendAuth_nativeClientId,
    importBackendAuth_webClientId,
    importBackendAuthResponse_jobId,
    importBackendAuthResponse_status,
    importBackendAuthResponse_error,
    importBackendAuthResponse_operation,
    importBackendAuthResponse_appId,
    importBackendAuthResponse_backendEnvironmentName,
    importBackendAuthResponse_httpStatus,

    -- ** ListBackendJobs
    listBackendJobs_nextToken,
    listBackendJobs_jobId,
    listBackendJobs_status,
    listBackendJobs_maxResults,
    listBackendJobs_operation,
    listBackendJobs_appId,
    listBackendJobs_backendEnvironmentName,
    listBackendJobsResponse_nextToken,
    listBackendJobsResponse_jobs,
    listBackendJobsResponse_httpStatus,

    -- ** RemoveAllBackends
    removeAllBackends_cleanAmplifyApp,
    removeAllBackends_appId,
    removeAllBackendsResponse_jobId,
    removeAllBackendsResponse_status,
    removeAllBackendsResponse_error,
    removeAllBackendsResponse_operation,
    removeAllBackendsResponse_appId,
    removeAllBackendsResponse_httpStatus,

    -- ** RemoveBackendConfig
    removeBackendConfig_appId,
    removeBackendConfigResponse_error,
    removeBackendConfigResponse_httpStatus,

    -- ** UpdateBackendAPI
    updateBackendAPI_resourceConfig,
    updateBackendAPI_appId,
    updateBackendAPI_backendEnvironmentName,
    updateBackendAPI_resourceName,
    updateBackendAPIResponse_jobId,
    updateBackendAPIResponse_status,
    updateBackendAPIResponse_error,
    updateBackendAPIResponse_operation,
    updateBackendAPIResponse_appId,
    updateBackendAPIResponse_backendEnvironmentName,
    updateBackendAPIResponse_httpStatus,

    -- ** UpdateBackendAuth
    updateBackendAuth_appId,
    updateBackendAuth_backendEnvironmentName,
    updateBackendAuth_resourceName,
    updateBackendAuth_resourceConfig,
    updateBackendAuthResponse_jobId,
    updateBackendAuthResponse_status,
    updateBackendAuthResponse_error,
    updateBackendAuthResponse_operation,
    updateBackendAuthResponse_appId,
    updateBackendAuthResponse_backendEnvironmentName,
    updateBackendAuthResponse_httpStatus,

    -- ** UpdateBackendConfig
    updateBackendConfig_loginAuthConfig,
    updateBackendConfig_appId,
    updateBackendConfigResponse_backendManagerAppId,
    updateBackendConfigResponse_loginAuthConfig,
    updateBackendConfigResponse_error,
    updateBackendConfigResponse_appId,
    updateBackendConfigResponse_httpStatus,

    -- ** UpdateBackendJob
    updateBackendJob_status,
    updateBackendJob_operation,
    updateBackendJob_appId,
    updateBackendJob_backendEnvironmentName,
    updateBackendJob_jobId,
    updateBackendJobResponse_jobId,
    updateBackendJobResponse_status,
    updateBackendJobResponse_updateTime,
    updateBackendJobResponse_createTime,
    updateBackendJobResponse_error,
    updateBackendJobResponse_operation,
    updateBackendJobResponse_appId,
    updateBackendJobResponse_backendEnvironmentName,
    updateBackendJobResponse_httpStatus,

    -- * Types

    -- ** BackendAPIAppSyncAuthSettings
    backendAPIAppSyncAuthSettings_expirationTime,
    backendAPIAppSyncAuthSettings_openIDAuthTTL,
    backendAPIAppSyncAuthSettings_description,
    backendAPIAppSyncAuthSettings_openIDIssueURL,
    backendAPIAppSyncAuthSettings_openIDClientId,
    backendAPIAppSyncAuthSettings_openIDIatTTL,
    backendAPIAppSyncAuthSettings_openIDProviderName,
    backendAPIAppSyncAuthSettings_cognitoUserPoolId,

    -- ** BackendAPIAuthType
    backendAPIAuthType_settings,
    backendAPIAuthType_mode,

    -- ** BackendAPIConflictResolution
    backendAPIConflictResolution_resolutionStrategy,

    -- ** BackendAPIResourceConfig
    backendAPIResourceConfig_additionalAuthTypes,
    backendAPIResourceConfig_conflictResolution,
    backendAPIResourceConfig_service,
    backendAPIResourceConfig_transformSchema,
    backendAPIResourceConfig_defaultAuthType,
    backendAPIResourceConfig_apiName,

    -- ** BackendAuthAppleProviderConfig
    backendAuthAppleProviderConfig_clientId,
    backendAuthAppleProviderConfig_privateKey,
    backendAuthAppleProviderConfig_teamId,
    backendAuthAppleProviderConfig_keyId,

    -- ** BackendAuthSocialProviderConfig
    backendAuthSocialProviderConfig_clientSecret,
    backendAuthSocialProviderConfig_clientId,

    -- ** BackendJobRespObj
    backendJobRespObj_jobId,
    backendJobRespObj_status,
    backendJobRespObj_updateTime,
    backendJobRespObj_createTime,
    backendJobRespObj_error,
    backendJobRespObj_operation,
    backendJobRespObj_appId,
    backendJobRespObj_backendEnvironmentName,

    -- ** CreateBackendAuthForgotPasswordConfig
    createBackendAuthForgotPasswordConfig_emailSettings,
    createBackendAuthForgotPasswordConfig_smsSettings,
    createBackendAuthForgotPasswordConfig_deliveryMethod,

    -- ** CreateBackendAuthIdentityPoolConfig
    createBackendAuthIdentityPoolConfig_unauthenticatedLogin,
    createBackendAuthIdentityPoolConfig_identityPoolName,

    -- ** CreateBackendAuthMFAConfig
    createBackendAuthMFAConfig_settings,
    createBackendAuthMFAConfig_mfaMode,

    -- ** CreateBackendAuthOAuthConfig
    createBackendAuthOAuthConfig_domainPrefix,
    createBackendAuthOAuthConfig_socialProviderSettings,
    createBackendAuthOAuthConfig_redirectSignOutURIs,
    createBackendAuthOAuthConfig_redirectSignInURIs,
    createBackendAuthOAuthConfig_oAuthGrantType,
    createBackendAuthOAuthConfig_oAuthScopes,

    -- ** CreateBackendAuthPasswordPolicyConfig
    createBackendAuthPasswordPolicyConfig_additionalConstraints,
    createBackendAuthPasswordPolicyConfig_minimumLength,

    -- ** CreateBackendAuthResourceConfig
    createBackendAuthResourceConfig_identityPoolConfigs,
    createBackendAuthResourceConfig_authResources,
    createBackendAuthResourceConfig_userPoolConfigs,
    createBackendAuthResourceConfig_service,

    -- ** CreateBackendAuthUserPoolConfig
    createBackendAuthUserPoolConfig_oAuth,
    createBackendAuthUserPoolConfig_passwordPolicy,
    createBackendAuthUserPoolConfig_forgotPassword,
    createBackendAuthUserPoolConfig_mfa,
    createBackendAuthUserPoolConfig_requiredSignUpAttributes,
    createBackendAuthUserPoolConfig_signInMethod,
    createBackendAuthUserPoolConfig_userPoolName,

    -- ** EmailSettings
    emailSettings_emailSubject,
    emailSettings_emailMessage,

    -- ** LoginAuthConfigReqObj
    loginAuthConfigReqObj_awsCognitoIdentityPoolId,
    loginAuthConfigReqObj_awsCognitoRegion,
    loginAuthConfigReqObj_awsUserPoolsId,
    loginAuthConfigReqObj_awsUserPoolsWebClientId,

    -- ** ResourceConfig

    -- ** Settings
    settings_mfaTypes,
    settings_smsMessage,

    -- ** SmsSettings
    smsSettings_smsMessage,

    -- ** SocialProviderSettings
    socialProviderSettings_loginWithAmazon,
    socialProviderSettings_signInWithApple,
    socialProviderSettings_facebook,
    socialProviderSettings_google,

    -- ** UpdateBackendAuthForgotPasswordConfig
    updateBackendAuthForgotPasswordConfig_emailSettings,
    updateBackendAuthForgotPasswordConfig_deliveryMethod,
    updateBackendAuthForgotPasswordConfig_smsSettings,

    -- ** UpdateBackendAuthIdentityPoolConfig
    updateBackendAuthIdentityPoolConfig_unauthenticatedLogin,

    -- ** UpdateBackendAuthMFAConfig
    updateBackendAuthMFAConfig_settings,
    updateBackendAuthMFAConfig_mfaMode,

    -- ** UpdateBackendAuthOAuthConfig
    updateBackendAuthOAuthConfig_domainPrefix,
    updateBackendAuthOAuthConfig_redirectSignOutURIs,
    updateBackendAuthOAuthConfig_redirectSignInURIs,
    updateBackendAuthOAuthConfig_oAuthScopes,
    updateBackendAuthOAuthConfig_socialProviderSettings,
    updateBackendAuthOAuthConfig_oAuthGrantType,

    -- ** UpdateBackendAuthPasswordPolicyConfig
    updateBackendAuthPasswordPolicyConfig_additionalConstraints,
    updateBackendAuthPasswordPolicyConfig_minimumLength,

    -- ** UpdateBackendAuthResourceConfig
    updateBackendAuthResourceConfig_identityPoolConfigs,
    updateBackendAuthResourceConfig_authResources,
    updateBackendAuthResourceConfig_userPoolConfigs,
    updateBackendAuthResourceConfig_service,

    -- ** UpdateBackendAuthUserPoolConfig
    updateBackendAuthUserPoolConfig_oAuth,
    updateBackendAuthUserPoolConfig_passwordPolicy,
    updateBackendAuthUserPoolConfig_forgotPassword,
    updateBackendAuthUserPoolConfig_mfa,
  )
where

import Amazonka.AmplifyBackend.CloneBackend
import Amazonka.AmplifyBackend.CreateBackend
import Amazonka.AmplifyBackend.CreateBackendAPI
import Amazonka.AmplifyBackend.CreateBackendAuth
import Amazonka.AmplifyBackend.CreateBackendConfig
import Amazonka.AmplifyBackend.CreateToken
import Amazonka.AmplifyBackend.DeleteBackend
import Amazonka.AmplifyBackend.DeleteBackendAPI
import Amazonka.AmplifyBackend.DeleteBackendAuth
import Amazonka.AmplifyBackend.DeleteToken
import Amazonka.AmplifyBackend.GenerateBackendAPIModels
import Amazonka.AmplifyBackend.GetBackend
import Amazonka.AmplifyBackend.GetBackendAPI
import Amazonka.AmplifyBackend.GetBackendAPIModels
import Amazonka.AmplifyBackend.GetBackendAuth
import Amazonka.AmplifyBackend.GetBackendJob
import Amazonka.AmplifyBackend.GetToken
import Amazonka.AmplifyBackend.ImportBackendAuth
import Amazonka.AmplifyBackend.ListBackendJobs
import Amazonka.AmplifyBackend.RemoveAllBackends
import Amazonka.AmplifyBackend.RemoveBackendConfig
import Amazonka.AmplifyBackend.Types.BackendAPIAppSyncAuthSettings
import Amazonka.AmplifyBackend.Types.BackendAPIAuthType
import Amazonka.AmplifyBackend.Types.BackendAPIConflictResolution
import Amazonka.AmplifyBackend.Types.BackendAPIResourceConfig
import Amazonka.AmplifyBackend.Types.BackendAuthAppleProviderConfig
import Amazonka.AmplifyBackend.Types.BackendAuthSocialProviderConfig
import Amazonka.AmplifyBackend.Types.BackendJobRespObj
import Amazonka.AmplifyBackend.Types.CreateBackendAuthForgotPasswordConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthIdentityPoolConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthMFAConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthOAuthConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthPasswordPolicyConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthResourceConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthUserPoolConfig
import Amazonka.AmplifyBackend.Types.EmailSettings
import Amazonka.AmplifyBackend.Types.LoginAuthConfigReqObj
import Amazonka.AmplifyBackend.Types.ResourceConfig
import Amazonka.AmplifyBackend.Types.Settings
import Amazonka.AmplifyBackend.Types.SmsSettings
import Amazonka.AmplifyBackend.Types.SocialProviderSettings
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthForgotPasswordConfig
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthIdentityPoolConfig
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthMFAConfig
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthOAuthConfig
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthPasswordPolicyConfig
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthResourceConfig
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthUserPoolConfig
import Amazonka.AmplifyBackend.UpdateBackendAPI
import Amazonka.AmplifyBackend.UpdateBackendAuth
import Amazonka.AmplifyBackend.UpdateBackendConfig
import Amazonka.AmplifyBackend.UpdateBackendJob
