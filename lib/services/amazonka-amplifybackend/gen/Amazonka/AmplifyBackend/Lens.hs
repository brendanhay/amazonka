{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AmplifyBackend.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Lens
  ( -- * Operations

    -- ** CloneBackend
    cloneBackend_appId,
    cloneBackend_backendEnvironmentName,
    cloneBackend_targetEnvironmentName,
    cloneBackendResponse_appId,
    cloneBackendResponse_backendEnvironmentName,
    cloneBackendResponse_error,
    cloneBackendResponse_jobId,
    cloneBackendResponse_operation,
    cloneBackendResponse_status,
    cloneBackendResponse_httpStatus,

    -- ** CreateBackend
    createBackend_resourceConfig,
    createBackend_resourceName,
    createBackend_appId,
    createBackend_backendEnvironmentName,
    createBackend_appName,
    createBackendResponse_appId,
    createBackendResponse_backendEnvironmentName,
    createBackendResponse_error,
    createBackendResponse_jobId,
    createBackendResponse_operation,
    createBackendResponse_status,
    createBackendResponse_httpStatus,

    -- ** CreateBackendAPI
    createBackendAPI_appId,
    createBackendAPI_resourceName,
    createBackendAPI_backendEnvironmentName,
    createBackendAPI_resourceConfig,
    createBackendAPIResponse_appId,
    createBackendAPIResponse_backendEnvironmentName,
    createBackendAPIResponse_error,
    createBackendAPIResponse_jobId,
    createBackendAPIResponse_operation,
    createBackendAPIResponse_status,
    createBackendAPIResponse_httpStatus,

    -- ** CreateBackendAuth
    createBackendAuth_appId,
    createBackendAuth_resourceName,
    createBackendAuth_backendEnvironmentName,
    createBackendAuth_resourceConfig,
    createBackendAuthResponse_appId,
    createBackendAuthResponse_backendEnvironmentName,
    createBackendAuthResponse_error,
    createBackendAuthResponse_jobId,
    createBackendAuthResponse_operation,
    createBackendAuthResponse_status,
    createBackendAuthResponse_httpStatus,

    -- ** CreateBackendConfig
    createBackendConfig_backendManagerAppId,
    createBackendConfig_appId,
    createBackendConfigResponse_appId,
    createBackendConfigResponse_backendEnvironmentName,
    createBackendConfigResponse_jobId,
    createBackendConfigResponse_status,
    createBackendConfigResponse_httpStatus,

    -- ** CreateBackendStorage
    createBackendStorage_appId,
    createBackendStorage_resourceName,
    createBackendStorage_backendEnvironmentName,
    createBackendStorage_resourceConfig,
    createBackendStorageResponse_appId,
    createBackendStorageResponse_backendEnvironmentName,
    createBackendStorageResponse_jobId,
    createBackendStorageResponse_status,
    createBackendStorageResponse_httpStatus,

    -- ** CreateToken
    createToken_appId,
    createTokenResponse_appId,
    createTokenResponse_challengeCode,
    createTokenResponse_sessionId,
    createTokenResponse_ttl,
    createTokenResponse_httpStatus,

    -- ** DeleteBackend
    deleteBackend_appId,
    deleteBackend_backendEnvironmentName,
    deleteBackendResponse_appId,
    deleteBackendResponse_backendEnvironmentName,
    deleteBackendResponse_error,
    deleteBackendResponse_jobId,
    deleteBackendResponse_operation,
    deleteBackendResponse_status,
    deleteBackendResponse_httpStatus,

    -- ** DeleteBackendAPI
    deleteBackendAPI_resourceConfig,
    deleteBackendAPI_appId,
    deleteBackendAPI_backendEnvironmentName,
    deleteBackendAPI_resourceName,
    deleteBackendAPIResponse_appId,
    deleteBackendAPIResponse_backendEnvironmentName,
    deleteBackendAPIResponse_error,
    deleteBackendAPIResponse_jobId,
    deleteBackendAPIResponse_operation,
    deleteBackendAPIResponse_status,
    deleteBackendAPIResponse_httpStatus,

    -- ** DeleteBackendAuth
    deleteBackendAuth_appId,
    deleteBackendAuth_backendEnvironmentName,
    deleteBackendAuth_resourceName,
    deleteBackendAuthResponse_appId,
    deleteBackendAuthResponse_backendEnvironmentName,
    deleteBackendAuthResponse_error,
    deleteBackendAuthResponse_jobId,
    deleteBackendAuthResponse_operation,
    deleteBackendAuthResponse_status,
    deleteBackendAuthResponse_httpStatus,

    -- ** DeleteBackendStorage
    deleteBackendStorage_appId,
    deleteBackendStorage_backendEnvironmentName,
    deleteBackendStorage_serviceName,
    deleteBackendStorage_resourceName,
    deleteBackendStorageResponse_appId,
    deleteBackendStorageResponse_backendEnvironmentName,
    deleteBackendStorageResponse_jobId,
    deleteBackendStorageResponse_status,
    deleteBackendStorageResponse_httpStatus,

    -- ** DeleteToken
    deleteToken_sessionId,
    deleteToken_appId,
    deleteTokenResponse_isSuccess,
    deleteTokenResponse_httpStatus,

    -- ** GenerateBackendAPIModels
    generateBackendAPIModels_appId,
    generateBackendAPIModels_backendEnvironmentName,
    generateBackendAPIModels_resourceName,
    generateBackendAPIModelsResponse_appId,
    generateBackendAPIModelsResponse_backendEnvironmentName,
    generateBackendAPIModelsResponse_error,
    generateBackendAPIModelsResponse_jobId,
    generateBackendAPIModelsResponse_operation,
    generateBackendAPIModelsResponse_status,
    generateBackendAPIModelsResponse_httpStatus,

    -- ** GetBackend
    getBackend_backendEnvironmentName,
    getBackend_appId,
    getBackendResponse_amplifyFeatureFlags,
    getBackendResponse_amplifyMetaConfig,
    getBackendResponse_appId,
    getBackendResponse_appName,
    getBackendResponse_backendEnvironmentList,
    getBackendResponse_backendEnvironmentName,
    getBackendResponse_error,
    getBackendResponse_httpStatus,

    -- ** GetBackendAPI
    getBackendAPI_resourceConfig,
    getBackendAPI_appId,
    getBackendAPI_backendEnvironmentName,
    getBackendAPI_resourceName,
    getBackendAPIResponse_appId,
    getBackendAPIResponse_backendEnvironmentName,
    getBackendAPIResponse_error,
    getBackendAPIResponse_resourceConfig,
    getBackendAPIResponse_resourceName,
    getBackendAPIResponse_httpStatus,

    -- ** GetBackendAPIModels
    getBackendAPIModels_appId,
    getBackendAPIModels_backendEnvironmentName,
    getBackendAPIModels_resourceName,
    getBackendAPIModelsResponse_modelIntrospectionSchema,
    getBackendAPIModelsResponse_models,
    getBackendAPIModelsResponse_status,
    getBackendAPIModelsResponse_httpStatus,

    -- ** GetBackendAuth
    getBackendAuth_appId,
    getBackendAuth_backendEnvironmentName,
    getBackendAuth_resourceName,
    getBackendAuthResponse_appId,
    getBackendAuthResponse_backendEnvironmentName,
    getBackendAuthResponse_error,
    getBackendAuthResponse_resourceConfig,
    getBackendAuthResponse_resourceName,
    getBackendAuthResponse_httpStatus,

    -- ** GetBackendJob
    getBackendJob_appId,
    getBackendJob_backendEnvironmentName,
    getBackendJob_jobId,
    getBackendJobResponse_appId,
    getBackendJobResponse_backendEnvironmentName,
    getBackendJobResponse_createTime,
    getBackendJobResponse_error,
    getBackendJobResponse_jobId,
    getBackendJobResponse_operation,
    getBackendJobResponse_status,
    getBackendJobResponse_updateTime,
    getBackendJobResponse_httpStatus,

    -- ** GetBackendStorage
    getBackendStorage_appId,
    getBackendStorage_backendEnvironmentName,
    getBackendStorage_resourceName,
    getBackendStorageResponse_appId,
    getBackendStorageResponse_backendEnvironmentName,
    getBackendStorageResponse_resourceConfig,
    getBackendStorageResponse_resourceName,
    getBackendStorageResponse_httpStatus,

    -- ** GetToken
    getToken_sessionId,
    getToken_appId,
    getTokenResponse_appId,
    getTokenResponse_challengeCode,
    getTokenResponse_sessionId,
    getTokenResponse_ttl,
    getTokenResponse_httpStatus,

    -- ** ImportBackendAuth
    importBackendAuth_identityPoolId,
    importBackendAuth_appId,
    importBackendAuth_backendEnvironmentName,
    importBackendAuth_userPoolId,
    importBackendAuth_nativeClientId,
    importBackendAuth_webClientId,
    importBackendAuthResponse_appId,
    importBackendAuthResponse_backendEnvironmentName,
    importBackendAuthResponse_error,
    importBackendAuthResponse_jobId,
    importBackendAuthResponse_operation,
    importBackendAuthResponse_status,
    importBackendAuthResponse_httpStatus,

    -- ** ImportBackendStorage
    importBackendStorage_bucketName,
    importBackendStorage_appId,
    importBackendStorage_backendEnvironmentName,
    importBackendStorage_serviceName,
    importBackendStorageResponse_appId,
    importBackendStorageResponse_backendEnvironmentName,
    importBackendStorageResponse_jobId,
    importBackendStorageResponse_status,
    importBackendStorageResponse_httpStatus,

    -- ** ListBackendJobs
    listBackendJobs_jobId,
    listBackendJobs_maxResults,
    listBackendJobs_nextToken,
    listBackendJobs_operation,
    listBackendJobs_status,
    listBackendJobs_appId,
    listBackendJobs_backendEnvironmentName,
    listBackendJobsResponse_jobs,
    listBackendJobsResponse_nextToken,
    listBackendJobsResponse_httpStatus,

    -- ** ListS3Buckets
    listS3Buckets_nextToken,
    listS3BucketsResponse_buckets,
    listS3BucketsResponse_nextToken,
    listS3BucketsResponse_httpStatus,

    -- ** RemoveAllBackends
    removeAllBackends_cleanAmplifyApp,
    removeAllBackends_appId,
    removeAllBackendsResponse_appId,
    removeAllBackendsResponse_error,
    removeAllBackendsResponse_jobId,
    removeAllBackendsResponse_operation,
    removeAllBackendsResponse_status,
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
    updateBackendAPIResponse_appId,
    updateBackendAPIResponse_backendEnvironmentName,
    updateBackendAPIResponse_error,
    updateBackendAPIResponse_jobId,
    updateBackendAPIResponse_operation,
    updateBackendAPIResponse_status,
    updateBackendAPIResponse_httpStatus,

    -- ** UpdateBackendAuth
    updateBackendAuth_appId,
    updateBackendAuth_backendEnvironmentName,
    updateBackendAuth_resourceName,
    updateBackendAuth_resourceConfig,
    updateBackendAuthResponse_appId,
    updateBackendAuthResponse_backendEnvironmentName,
    updateBackendAuthResponse_error,
    updateBackendAuthResponse_jobId,
    updateBackendAuthResponse_operation,
    updateBackendAuthResponse_status,
    updateBackendAuthResponse_httpStatus,

    -- ** UpdateBackendConfig
    updateBackendConfig_loginAuthConfig,
    updateBackendConfig_appId,
    updateBackendConfigResponse_appId,
    updateBackendConfigResponse_backendManagerAppId,
    updateBackendConfigResponse_error,
    updateBackendConfigResponse_loginAuthConfig,
    updateBackendConfigResponse_httpStatus,

    -- ** UpdateBackendJob
    updateBackendJob_operation,
    updateBackendJob_status,
    updateBackendJob_appId,
    updateBackendJob_backendEnvironmentName,
    updateBackendJob_jobId,
    updateBackendJobResponse_appId,
    updateBackendJobResponse_backendEnvironmentName,
    updateBackendJobResponse_createTime,
    updateBackendJobResponse_error,
    updateBackendJobResponse_jobId,
    updateBackendJobResponse_operation,
    updateBackendJobResponse_status,
    updateBackendJobResponse_updateTime,
    updateBackendJobResponse_httpStatus,

    -- ** UpdateBackendStorage
    updateBackendStorage_appId,
    updateBackendStorage_backendEnvironmentName,
    updateBackendStorage_resourceName,
    updateBackendStorage_resourceConfig,
    updateBackendStorageResponse_appId,
    updateBackendStorageResponse_backendEnvironmentName,
    updateBackendStorageResponse_jobId,
    updateBackendStorageResponse_status,
    updateBackendStorageResponse_httpStatus,

    -- * Types

    -- ** BackendAPIAppSyncAuthSettings
    backendAPIAppSyncAuthSettings_cognitoUserPoolId,
    backendAPIAppSyncAuthSettings_description,
    backendAPIAppSyncAuthSettings_expirationTime,
    backendAPIAppSyncAuthSettings_openIDAuthTTL,
    backendAPIAppSyncAuthSettings_openIDClientId,
    backendAPIAppSyncAuthSettings_openIDIatTTL,
    backendAPIAppSyncAuthSettings_openIDIssueURL,
    backendAPIAppSyncAuthSettings_openIDProviderName,

    -- ** BackendAPIAuthType
    backendAPIAuthType_mode,
    backendAPIAuthType_settings,

    -- ** BackendAPIConflictResolution
    backendAPIConflictResolution_resolutionStrategy,

    -- ** BackendAPIResourceConfig
    backendAPIResourceConfig_additionalAuthTypes,
    backendAPIResourceConfig_apiName,
    backendAPIResourceConfig_conflictResolution,
    backendAPIResourceConfig_defaultAuthType,
    backendAPIResourceConfig_service,
    backendAPIResourceConfig_transformSchema,

    -- ** BackendAuthAppleProviderConfig
    backendAuthAppleProviderConfig_clientId,
    backendAuthAppleProviderConfig_keyId,
    backendAuthAppleProviderConfig_privateKey,
    backendAuthAppleProviderConfig_teamId,

    -- ** BackendAuthSocialProviderConfig
    backendAuthSocialProviderConfig_clientId,
    backendAuthSocialProviderConfig_clientSecret,

    -- ** BackendJobRespObj
    backendJobRespObj_createTime,
    backendJobRespObj_error,
    backendJobRespObj_jobId,
    backendJobRespObj_operation,
    backendJobRespObj_status,
    backendJobRespObj_updateTime,
    backendJobRespObj_appId,
    backendJobRespObj_backendEnvironmentName,

    -- ** BackendStoragePermissions
    backendStoragePermissions_unAuthenticated,
    backendStoragePermissions_authenticated,

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
    createBackendAuthUserPoolConfig_forgotPassword,
    createBackendAuthUserPoolConfig_mfa,
    createBackendAuthUserPoolConfig_oAuth,
    createBackendAuthUserPoolConfig_passwordPolicy,
    createBackendAuthUserPoolConfig_verificationMessage,
    createBackendAuthUserPoolConfig_requiredSignUpAttributes,
    createBackendAuthUserPoolConfig_signInMethod,
    createBackendAuthUserPoolConfig_userPoolName,

    -- ** CreateBackendAuthVerificationMessageConfig
    createBackendAuthVerificationMessageConfig_emailSettings,
    createBackendAuthVerificationMessageConfig_smsSettings,
    createBackendAuthVerificationMessageConfig_deliveryMethod,

    -- ** CreateBackendStorageResourceConfig
    createBackendStorageResourceConfig_bucketName,
    createBackendStorageResourceConfig_serviceName,
    createBackendStorageResourceConfig_permissions,

    -- ** EmailSettings
    emailSettings_emailMessage,
    emailSettings_emailSubject,

    -- ** GetBackendStorageResourceConfig
    getBackendStorageResourceConfig_bucketName,
    getBackendStorageResourceConfig_permissions,
    getBackendStorageResourceConfig_serviceName,
    getBackendStorageResourceConfig_imported,

    -- ** LoginAuthConfigReqObj
    loginAuthConfigReqObj_awsCognitoIdentityPoolId,
    loginAuthConfigReqObj_awsCognitoRegion,
    loginAuthConfigReqObj_awsUserPoolsId,
    loginAuthConfigReqObj_awsUserPoolsWebClientId,

    -- ** ResourceConfig

    -- ** S3BucketInfo
    s3BucketInfo_creationDate,
    s3BucketInfo_name,

    -- ** Settings
    settings_mfaTypes,
    settings_smsMessage,

    -- ** SmsSettings
    smsSettings_smsMessage,

    -- ** SocialProviderSettings
    socialProviderSettings_facebook,
    socialProviderSettings_google,
    socialProviderSettings_loginWithAmazon,
    socialProviderSettings_signInWithApple,

    -- ** UpdateBackendAuthForgotPasswordConfig
    updateBackendAuthForgotPasswordConfig_deliveryMethod,
    updateBackendAuthForgotPasswordConfig_emailSettings,
    updateBackendAuthForgotPasswordConfig_smsSettings,

    -- ** UpdateBackendAuthIdentityPoolConfig
    updateBackendAuthIdentityPoolConfig_unauthenticatedLogin,

    -- ** UpdateBackendAuthMFAConfig
    updateBackendAuthMFAConfig_mfaMode,
    updateBackendAuthMFAConfig_settings,

    -- ** UpdateBackendAuthOAuthConfig
    updateBackendAuthOAuthConfig_domainPrefix,
    updateBackendAuthOAuthConfig_oAuthGrantType,
    updateBackendAuthOAuthConfig_oAuthScopes,
    updateBackendAuthOAuthConfig_redirectSignInURIs,
    updateBackendAuthOAuthConfig_redirectSignOutURIs,
    updateBackendAuthOAuthConfig_socialProviderSettings,

    -- ** UpdateBackendAuthPasswordPolicyConfig
    updateBackendAuthPasswordPolicyConfig_additionalConstraints,
    updateBackendAuthPasswordPolicyConfig_minimumLength,

    -- ** UpdateBackendAuthResourceConfig
    updateBackendAuthResourceConfig_identityPoolConfigs,
    updateBackendAuthResourceConfig_authResources,
    updateBackendAuthResourceConfig_userPoolConfigs,
    updateBackendAuthResourceConfig_service,

    -- ** UpdateBackendAuthUserPoolConfig
    updateBackendAuthUserPoolConfig_forgotPassword,
    updateBackendAuthUserPoolConfig_mfa,
    updateBackendAuthUserPoolConfig_oAuth,
    updateBackendAuthUserPoolConfig_passwordPolicy,
    updateBackendAuthUserPoolConfig_verificationMessage,

    -- ** UpdateBackendAuthVerificationMessageConfig
    updateBackendAuthVerificationMessageConfig_emailSettings,
    updateBackendAuthVerificationMessageConfig_smsSettings,
    updateBackendAuthVerificationMessageConfig_deliveryMethod,

    -- ** UpdateBackendStorageResourceConfig
    updateBackendStorageResourceConfig_serviceName,
    updateBackendStorageResourceConfig_permissions,
  )
where

import Amazonka.AmplifyBackend.CloneBackend
import Amazonka.AmplifyBackend.CreateBackend
import Amazonka.AmplifyBackend.CreateBackendAPI
import Amazonka.AmplifyBackend.CreateBackendAuth
import Amazonka.AmplifyBackend.CreateBackendConfig
import Amazonka.AmplifyBackend.CreateBackendStorage
import Amazonka.AmplifyBackend.CreateToken
import Amazonka.AmplifyBackend.DeleteBackend
import Amazonka.AmplifyBackend.DeleteBackendAPI
import Amazonka.AmplifyBackend.DeleteBackendAuth
import Amazonka.AmplifyBackend.DeleteBackendStorage
import Amazonka.AmplifyBackend.DeleteToken
import Amazonka.AmplifyBackend.GenerateBackendAPIModels
import Amazonka.AmplifyBackend.GetBackend
import Amazonka.AmplifyBackend.GetBackendAPI
import Amazonka.AmplifyBackend.GetBackendAPIModels
import Amazonka.AmplifyBackend.GetBackendAuth
import Amazonka.AmplifyBackend.GetBackendJob
import Amazonka.AmplifyBackend.GetBackendStorage
import Amazonka.AmplifyBackend.GetToken
import Amazonka.AmplifyBackend.ImportBackendAuth
import Amazonka.AmplifyBackend.ImportBackendStorage
import Amazonka.AmplifyBackend.ListBackendJobs
import Amazonka.AmplifyBackend.ListS3Buckets
import Amazonka.AmplifyBackend.RemoveAllBackends
import Amazonka.AmplifyBackend.RemoveBackendConfig
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
import Amazonka.AmplifyBackend.Types.EmailSettings
import Amazonka.AmplifyBackend.Types.GetBackendStorageResourceConfig
import Amazonka.AmplifyBackend.Types.LoginAuthConfigReqObj
import Amazonka.AmplifyBackend.Types.ResourceConfig
import Amazonka.AmplifyBackend.Types.S3BucketInfo
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
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthVerificationMessageConfig
import Amazonka.AmplifyBackend.Types.UpdateBackendStorageResourceConfig
import Amazonka.AmplifyBackend.UpdateBackendAPI
import Amazonka.AmplifyBackend.UpdateBackendAuth
import Amazonka.AmplifyBackend.UpdateBackendConfig
import Amazonka.AmplifyBackend.UpdateBackendJob
import Amazonka.AmplifyBackend.UpdateBackendStorage
