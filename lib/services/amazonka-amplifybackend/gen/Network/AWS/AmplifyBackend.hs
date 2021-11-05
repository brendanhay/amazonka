{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.AmplifyBackend
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-08-11@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Amplify Admin API
module Network.AWS.AmplifyBackend
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** GatewayTimeoutException
    _GatewayTimeoutException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** BadRequestException
    _BadRequestException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CloneBackend
    CloneBackend (CloneBackend'),
    newCloneBackend,
    CloneBackendResponse (CloneBackendResponse'),
    newCloneBackendResponse,

    -- ** DeleteToken
    DeleteToken (DeleteToken'),
    newDeleteToken,
    DeleteTokenResponse (DeleteTokenResponse'),
    newDeleteTokenResponse,

    -- ** CreateBackendConfig
    CreateBackendConfig (CreateBackendConfig'),
    newCreateBackendConfig,
    CreateBackendConfigResponse (CreateBackendConfigResponse'),
    newCreateBackendConfigResponse,

    -- ** ListBackendJobs (Paginated)
    ListBackendJobs (ListBackendJobs'),
    newListBackendJobs,
    ListBackendJobsResponse (ListBackendJobsResponse'),
    newListBackendJobsResponse,

    -- ** GetBackend
    GetBackend (GetBackend'),
    newGetBackend,
    GetBackendResponse (GetBackendResponse'),
    newGetBackendResponse,

    -- ** GetBackendAuth
    GetBackendAuth (GetBackendAuth'),
    newGetBackendAuth,
    GetBackendAuthResponse (GetBackendAuthResponse'),
    newGetBackendAuthResponse,

    -- ** CreateBackendAuth
    CreateBackendAuth (CreateBackendAuth'),
    newCreateBackendAuth,
    CreateBackendAuthResponse (CreateBackendAuthResponse'),
    newCreateBackendAuthResponse,

    -- ** RemoveBackendConfig
    RemoveBackendConfig (RemoveBackendConfig'),
    newRemoveBackendConfig,
    RemoveBackendConfigResponse (RemoveBackendConfigResponse'),
    newRemoveBackendConfigResponse,

    -- ** CreateBackend
    CreateBackend (CreateBackend'),
    newCreateBackend,
    CreateBackendResponse (CreateBackendResponse'),
    newCreateBackendResponse,

    -- ** GenerateBackendAPIModels
    GenerateBackendAPIModels (GenerateBackendAPIModels'),
    newGenerateBackendAPIModels,
    GenerateBackendAPIModelsResponse (GenerateBackendAPIModelsResponse'),
    newGenerateBackendAPIModelsResponse,

    -- ** UpdateBackendConfig
    UpdateBackendConfig (UpdateBackendConfig'),
    newUpdateBackendConfig,
    UpdateBackendConfigResponse (UpdateBackendConfigResponse'),
    newUpdateBackendConfigResponse,

    -- ** CreateToken
    CreateToken (CreateToken'),
    newCreateToken,
    CreateTokenResponse (CreateTokenResponse'),
    newCreateTokenResponse,

    -- ** GetBackendAPI
    GetBackendAPI (GetBackendAPI'),
    newGetBackendAPI,
    GetBackendAPIResponse (GetBackendAPIResponse'),
    newGetBackendAPIResponse,

    -- ** UpdateBackendJob
    UpdateBackendJob (UpdateBackendJob'),
    newUpdateBackendJob,
    UpdateBackendJobResponse (UpdateBackendJobResponse'),
    newUpdateBackendJobResponse,

    -- ** UpdateBackendAPI
    UpdateBackendAPI (UpdateBackendAPI'),
    newUpdateBackendAPI,
    UpdateBackendAPIResponse (UpdateBackendAPIResponse'),
    newUpdateBackendAPIResponse,

    -- ** DeleteBackendAPI
    DeleteBackendAPI (DeleteBackendAPI'),
    newDeleteBackendAPI,
    DeleteBackendAPIResponse (DeleteBackendAPIResponse'),
    newDeleteBackendAPIResponse,

    -- ** GetBackendJob
    GetBackendJob (GetBackendJob'),
    newGetBackendJob,
    GetBackendJobResponse (GetBackendJobResponse'),
    newGetBackendJobResponse,

    -- ** DeleteBackend
    DeleteBackend (DeleteBackend'),
    newDeleteBackend,
    DeleteBackendResponse (DeleteBackendResponse'),
    newDeleteBackendResponse,

    -- ** DeleteBackendAuth
    DeleteBackendAuth (DeleteBackendAuth'),
    newDeleteBackendAuth,
    DeleteBackendAuthResponse (DeleteBackendAuthResponse'),
    newDeleteBackendAuthResponse,

    -- ** UpdateBackendAuth
    UpdateBackendAuth (UpdateBackendAuth'),
    newUpdateBackendAuth,
    UpdateBackendAuthResponse (UpdateBackendAuthResponse'),
    newUpdateBackendAuthResponse,

    -- ** GetToken
    GetToken (GetToken'),
    newGetToken,
    GetTokenResponse (GetTokenResponse'),
    newGetTokenResponse,

    -- ** RemoveAllBackends
    RemoveAllBackends (RemoveAllBackends'),
    newRemoveAllBackends,
    RemoveAllBackendsResponse (RemoveAllBackendsResponse'),
    newRemoveAllBackendsResponse,

    -- ** CreateBackendAPI
    CreateBackendAPI (CreateBackendAPI'),
    newCreateBackendAPI,
    CreateBackendAPIResponse (CreateBackendAPIResponse'),
    newCreateBackendAPIResponse,

    -- ** GetBackendAPIModels
    GetBackendAPIModels (GetBackendAPIModels'),
    newGetBackendAPIModels,
    GetBackendAPIModelsResponse (GetBackendAPIModelsResponse'),
    newGetBackendAPIModelsResponse,

    -- ** ImportBackendAuth
    ImportBackendAuth (ImportBackendAuth'),
    newImportBackendAuth,
    ImportBackendAuthResponse (ImportBackendAuthResponse'),
    newImportBackendAuthResponse,

    -- * Types

    -- ** AdditionalConstraintsElement
    AdditionalConstraintsElement (..),

    -- ** AuthResources
    AuthResources (..),

    -- ** DeliveryMethod
    DeliveryMethod (..),

    -- ** MFAMode
    MFAMode (..),

    -- ** MfaTypesElement
    MfaTypesElement (..),

    -- ** Mode
    Mode (..),

    -- ** OAuthGrantType
    OAuthGrantType (..),

    -- ** OAuthScopesElement
    OAuthScopesElement (..),

    -- ** RequiredSignUpAttributesElement
    RequiredSignUpAttributesElement (..),

    -- ** ResolutionStrategy
    ResolutionStrategy (..),

    -- ** Service
    Service (..),

    -- ** SignInMethod
    SignInMethod (..),

    -- ** Status
    Status (..),

    -- ** BackendAPIAppSyncAuthSettings
    BackendAPIAppSyncAuthSettings (BackendAPIAppSyncAuthSettings'),
    newBackendAPIAppSyncAuthSettings,

    -- ** BackendAPIAuthType
    BackendAPIAuthType (BackendAPIAuthType'),
    newBackendAPIAuthType,

    -- ** BackendAPIConflictResolution
    BackendAPIConflictResolution (BackendAPIConflictResolution'),
    newBackendAPIConflictResolution,

    -- ** BackendAPIResourceConfig
    BackendAPIResourceConfig (BackendAPIResourceConfig'),
    newBackendAPIResourceConfig,

    -- ** BackendAuthAppleProviderConfig
    BackendAuthAppleProviderConfig (BackendAuthAppleProviderConfig'),
    newBackendAuthAppleProviderConfig,

    -- ** BackendAuthSocialProviderConfig
    BackendAuthSocialProviderConfig (BackendAuthSocialProviderConfig'),
    newBackendAuthSocialProviderConfig,

    -- ** BackendJobRespObj
    BackendJobRespObj (BackendJobRespObj'),
    newBackendJobRespObj,

    -- ** CreateBackendAuthForgotPasswordConfig
    CreateBackendAuthForgotPasswordConfig (CreateBackendAuthForgotPasswordConfig'),
    newCreateBackendAuthForgotPasswordConfig,

    -- ** CreateBackendAuthIdentityPoolConfig
    CreateBackendAuthIdentityPoolConfig (CreateBackendAuthIdentityPoolConfig'),
    newCreateBackendAuthIdentityPoolConfig,

    -- ** CreateBackendAuthMFAConfig
    CreateBackendAuthMFAConfig (CreateBackendAuthMFAConfig'),
    newCreateBackendAuthMFAConfig,

    -- ** CreateBackendAuthOAuthConfig
    CreateBackendAuthOAuthConfig (CreateBackendAuthOAuthConfig'),
    newCreateBackendAuthOAuthConfig,

    -- ** CreateBackendAuthPasswordPolicyConfig
    CreateBackendAuthPasswordPolicyConfig (CreateBackendAuthPasswordPolicyConfig'),
    newCreateBackendAuthPasswordPolicyConfig,

    -- ** CreateBackendAuthResourceConfig
    CreateBackendAuthResourceConfig (CreateBackendAuthResourceConfig'),
    newCreateBackendAuthResourceConfig,

    -- ** CreateBackendAuthUserPoolConfig
    CreateBackendAuthUserPoolConfig (CreateBackendAuthUserPoolConfig'),
    newCreateBackendAuthUserPoolConfig,

    -- ** EmailSettings
    EmailSettings (EmailSettings'),
    newEmailSettings,

    -- ** LoginAuthConfigReqObj
    LoginAuthConfigReqObj (LoginAuthConfigReqObj'),
    newLoginAuthConfigReqObj,

    -- ** ResourceConfig
    ResourceConfig (ResourceConfig'),
    newResourceConfig,

    -- ** Settings
    Settings (Settings'),
    newSettings,

    -- ** SmsSettings
    SmsSettings (SmsSettings'),
    newSmsSettings,

    -- ** SocialProviderSettings
    SocialProviderSettings (SocialProviderSettings'),
    newSocialProviderSettings,

    -- ** UpdateBackendAuthForgotPasswordConfig
    UpdateBackendAuthForgotPasswordConfig (UpdateBackendAuthForgotPasswordConfig'),
    newUpdateBackendAuthForgotPasswordConfig,

    -- ** UpdateBackendAuthIdentityPoolConfig
    UpdateBackendAuthIdentityPoolConfig (UpdateBackendAuthIdentityPoolConfig'),
    newUpdateBackendAuthIdentityPoolConfig,

    -- ** UpdateBackendAuthMFAConfig
    UpdateBackendAuthMFAConfig (UpdateBackendAuthMFAConfig'),
    newUpdateBackendAuthMFAConfig,

    -- ** UpdateBackendAuthOAuthConfig
    UpdateBackendAuthOAuthConfig (UpdateBackendAuthOAuthConfig'),
    newUpdateBackendAuthOAuthConfig,

    -- ** UpdateBackendAuthPasswordPolicyConfig
    UpdateBackendAuthPasswordPolicyConfig (UpdateBackendAuthPasswordPolicyConfig'),
    newUpdateBackendAuthPasswordPolicyConfig,

    -- ** UpdateBackendAuthResourceConfig
    UpdateBackendAuthResourceConfig (UpdateBackendAuthResourceConfig'),
    newUpdateBackendAuthResourceConfig,

    -- ** UpdateBackendAuthUserPoolConfig
    UpdateBackendAuthUserPoolConfig (UpdateBackendAuthUserPoolConfig'),
    newUpdateBackendAuthUserPoolConfig,
  )
where

import Network.AWS.AmplifyBackend.CloneBackend
import Network.AWS.AmplifyBackend.CreateBackend
import Network.AWS.AmplifyBackend.CreateBackendAPI
import Network.AWS.AmplifyBackend.CreateBackendAuth
import Network.AWS.AmplifyBackend.CreateBackendConfig
import Network.AWS.AmplifyBackend.CreateToken
import Network.AWS.AmplifyBackend.DeleteBackend
import Network.AWS.AmplifyBackend.DeleteBackendAPI
import Network.AWS.AmplifyBackend.DeleteBackendAuth
import Network.AWS.AmplifyBackend.DeleteToken
import Network.AWS.AmplifyBackend.GenerateBackendAPIModels
import Network.AWS.AmplifyBackend.GetBackend
import Network.AWS.AmplifyBackend.GetBackendAPI
import Network.AWS.AmplifyBackend.GetBackendAPIModels
import Network.AWS.AmplifyBackend.GetBackendAuth
import Network.AWS.AmplifyBackend.GetBackendJob
import Network.AWS.AmplifyBackend.GetToken
import Network.AWS.AmplifyBackend.ImportBackendAuth
import Network.AWS.AmplifyBackend.Lens
import Network.AWS.AmplifyBackend.ListBackendJobs
import Network.AWS.AmplifyBackend.RemoveAllBackends
import Network.AWS.AmplifyBackend.RemoveBackendConfig
import Network.AWS.AmplifyBackend.Types
import Network.AWS.AmplifyBackend.UpdateBackendAPI
import Network.AWS.AmplifyBackend.UpdateBackendAuth
import Network.AWS.AmplifyBackend.UpdateBackendConfig
import Network.AWS.AmplifyBackend.UpdateBackendJob
import Network.AWS.AmplifyBackend.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'AmplifyBackend'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
