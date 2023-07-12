{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.AmplifyBackend
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-08-11@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Amplify Admin API
module Amazonka.AmplifyBackend
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** GatewayTimeoutException
    _GatewayTimeoutException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CloneBackend
    CloneBackend (CloneBackend'),
    newCloneBackend,
    CloneBackendResponse (CloneBackendResponse'),
    newCloneBackendResponse,

    -- ** CreateBackend
    CreateBackend (CreateBackend'),
    newCreateBackend,
    CreateBackendResponse (CreateBackendResponse'),
    newCreateBackendResponse,

    -- ** CreateBackendAPI
    CreateBackendAPI (CreateBackendAPI'),
    newCreateBackendAPI,
    CreateBackendAPIResponse (CreateBackendAPIResponse'),
    newCreateBackendAPIResponse,

    -- ** CreateBackendAuth
    CreateBackendAuth (CreateBackendAuth'),
    newCreateBackendAuth,
    CreateBackendAuthResponse (CreateBackendAuthResponse'),
    newCreateBackendAuthResponse,

    -- ** CreateBackendConfig
    CreateBackendConfig (CreateBackendConfig'),
    newCreateBackendConfig,
    CreateBackendConfigResponse (CreateBackendConfigResponse'),
    newCreateBackendConfigResponse,

    -- ** CreateBackendStorage
    CreateBackendStorage (CreateBackendStorage'),
    newCreateBackendStorage,
    CreateBackendStorageResponse (CreateBackendStorageResponse'),
    newCreateBackendStorageResponse,

    -- ** CreateToken
    CreateToken (CreateToken'),
    newCreateToken,
    CreateTokenResponse (CreateTokenResponse'),
    newCreateTokenResponse,

    -- ** DeleteBackend
    DeleteBackend (DeleteBackend'),
    newDeleteBackend,
    DeleteBackendResponse (DeleteBackendResponse'),
    newDeleteBackendResponse,

    -- ** DeleteBackendAPI
    DeleteBackendAPI (DeleteBackendAPI'),
    newDeleteBackendAPI,
    DeleteBackendAPIResponse (DeleteBackendAPIResponse'),
    newDeleteBackendAPIResponse,

    -- ** DeleteBackendAuth
    DeleteBackendAuth (DeleteBackendAuth'),
    newDeleteBackendAuth,
    DeleteBackendAuthResponse (DeleteBackendAuthResponse'),
    newDeleteBackendAuthResponse,

    -- ** DeleteBackendStorage
    DeleteBackendStorage (DeleteBackendStorage'),
    newDeleteBackendStorage,
    DeleteBackendStorageResponse (DeleteBackendStorageResponse'),
    newDeleteBackendStorageResponse,

    -- ** DeleteToken
    DeleteToken (DeleteToken'),
    newDeleteToken,
    DeleteTokenResponse (DeleteTokenResponse'),
    newDeleteTokenResponse,

    -- ** GenerateBackendAPIModels
    GenerateBackendAPIModels (GenerateBackendAPIModels'),
    newGenerateBackendAPIModels,
    GenerateBackendAPIModelsResponse (GenerateBackendAPIModelsResponse'),
    newGenerateBackendAPIModelsResponse,

    -- ** GetBackend
    GetBackend (GetBackend'),
    newGetBackend,
    GetBackendResponse (GetBackendResponse'),
    newGetBackendResponse,

    -- ** GetBackendAPI
    GetBackendAPI (GetBackendAPI'),
    newGetBackendAPI,
    GetBackendAPIResponse (GetBackendAPIResponse'),
    newGetBackendAPIResponse,

    -- ** GetBackendAPIModels
    GetBackendAPIModels (GetBackendAPIModels'),
    newGetBackendAPIModels,
    GetBackendAPIModelsResponse (GetBackendAPIModelsResponse'),
    newGetBackendAPIModelsResponse,

    -- ** GetBackendAuth
    GetBackendAuth (GetBackendAuth'),
    newGetBackendAuth,
    GetBackendAuthResponse (GetBackendAuthResponse'),
    newGetBackendAuthResponse,

    -- ** GetBackendJob
    GetBackendJob (GetBackendJob'),
    newGetBackendJob,
    GetBackendJobResponse (GetBackendJobResponse'),
    newGetBackendJobResponse,

    -- ** GetBackendStorage
    GetBackendStorage (GetBackendStorage'),
    newGetBackendStorage,
    GetBackendStorageResponse (GetBackendStorageResponse'),
    newGetBackendStorageResponse,

    -- ** GetToken
    GetToken (GetToken'),
    newGetToken,
    GetTokenResponse (GetTokenResponse'),
    newGetTokenResponse,

    -- ** ImportBackendAuth
    ImportBackendAuth (ImportBackendAuth'),
    newImportBackendAuth,
    ImportBackendAuthResponse (ImportBackendAuthResponse'),
    newImportBackendAuthResponse,

    -- ** ImportBackendStorage
    ImportBackendStorage (ImportBackendStorage'),
    newImportBackendStorage,
    ImportBackendStorageResponse (ImportBackendStorageResponse'),
    newImportBackendStorageResponse,

    -- ** ListBackendJobs (Paginated)
    ListBackendJobs (ListBackendJobs'),
    newListBackendJobs,
    ListBackendJobsResponse (ListBackendJobsResponse'),
    newListBackendJobsResponse,

    -- ** ListS3Buckets
    ListS3Buckets (ListS3Buckets'),
    newListS3Buckets,
    ListS3BucketsResponse (ListS3BucketsResponse'),
    newListS3BucketsResponse,

    -- ** RemoveAllBackends
    RemoveAllBackends (RemoveAllBackends'),
    newRemoveAllBackends,
    RemoveAllBackendsResponse (RemoveAllBackendsResponse'),
    newRemoveAllBackendsResponse,

    -- ** RemoveBackendConfig
    RemoveBackendConfig (RemoveBackendConfig'),
    newRemoveBackendConfig,
    RemoveBackendConfigResponse (RemoveBackendConfigResponse'),
    newRemoveBackendConfigResponse,

    -- ** UpdateBackendAPI
    UpdateBackendAPI (UpdateBackendAPI'),
    newUpdateBackendAPI,
    UpdateBackendAPIResponse (UpdateBackendAPIResponse'),
    newUpdateBackendAPIResponse,

    -- ** UpdateBackendAuth
    UpdateBackendAuth (UpdateBackendAuth'),
    newUpdateBackendAuth,
    UpdateBackendAuthResponse (UpdateBackendAuthResponse'),
    newUpdateBackendAuthResponse,

    -- ** UpdateBackendConfig
    UpdateBackendConfig (UpdateBackendConfig'),
    newUpdateBackendConfig,
    UpdateBackendConfigResponse (UpdateBackendConfigResponse'),
    newUpdateBackendConfigResponse,

    -- ** UpdateBackendJob
    UpdateBackendJob (UpdateBackendJob'),
    newUpdateBackendJob,
    UpdateBackendJobResponse (UpdateBackendJobResponse'),
    newUpdateBackendJobResponse,

    -- ** UpdateBackendStorage
    UpdateBackendStorage (UpdateBackendStorage'),
    newUpdateBackendStorage,
    UpdateBackendStorageResponse (UpdateBackendStorageResponse'),
    newUpdateBackendStorageResponse,

    -- * Types

    -- ** AdditionalConstraintsElement
    AdditionalConstraintsElement (..),

    -- ** AuthResources
    AuthResources (..),

    -- ** AuthenticatedElement
    AuthenticatedElement (..),

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

    -- ** ServiceName
    ServiceName (..),

    -- ** SignInMethod
    SignInMethod (..),

    -- ** Status
    Status (..),

    -- ** UnAuthenticatedElement
    UnAuthenticatedElement (..),

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

    -- ** BackendStoragePermissions
    BackendStoragePermissions (BackendStoragePermissions'),
    newBackendStoragePermissions,

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

    -- ** CreateBackendAuthVerificationMessageConfig
    CreateBackendAuthVerificationMessageConfig (CreateBackendAuthVerificationMessageConfig'),
    newCreateBackendAuthVerificationMessageConfig,

    -- ** CreateBackendStorageResourceConfig
    CreateBackendStorageResourceConfig (CreateBackendStorageResourceConfig'),
    newCreateBackendStorageResourceConfig,

    -- ** EmailSettings
    EmailSettings (EmailSettings'),
    newEmailSettings,

    -- ** GetBackendStorageResourceConfig
    GetBackendStorageResourceConfig (GetBackendStorageResourceConfig'),
    newGetBackendStorageResourceConfig,

    -- ** LoginAuthConfigReqObj
    LoginAuthConfigReqObj (LoginAuthConfigReqObj'),
    newLoginAuthConfigReqObj,

    -- ** ResourceConfig
    ResourceConfig (ResourceConfig'),
    newResourceConfig,

    -- ** S3BucketInfo
    S3BucketInfo (S3BucketInfo'),
    newS3BucketInfo,

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

    -- ** UpdateBackendAuthVerificationMessageConfig
    UpdateBackendAuthVerificationMessageConfig (UpdateBackendAuthVerificationMessageConfig'),
    newUpdateBackendAuthVerificationMessageConfig,

    -- ** UpdateBackendStorageResourceConfig
    UpdateBackendStorageResourceConfig (UpdateBackendStorageResourceConfig'),
    newUpdateBackendStorageResourceConfig,
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
import Amazonka.AmplifyBackend.Lens
import Amazonka.AmplifyBackend.ListBackendJobs
import Amazonka.AmplifyBackend.ListS3Buckets
import Amazonka.AmplifyBackend.RemoveAllBackends
import Amazonka.AmplifyBackend.RemoveBackendConfig
import Amazonka.AmplifyBackend.Types
import Amazonka.AmplifyBackend.UpdateBackendAPI
import Amazonka.AmplifyBackend.UpdateBackendAuth
import Amazonka.AmplifyBackend.UpdateBackendConfig
import Amazonka.AmplifyBackend.UpdateBackendJob
import Amazonka.AmplifyBackend.UpdateBackendStorage
import Amazonka.AmplifyBackend.Waiters

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
