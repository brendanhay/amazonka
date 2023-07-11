{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SSOOIDC
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-06-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS IAM Identity Center (successor to AWS Single Sign-On) OpenID Connect
-- (OIDC) is a web service that enables a client (such as AWS CLI or a
-- native application) to register with IAM Identity Center. The service
-- also enables the client to fetch the user’s access token upon successful
-- authentication and authorization with IAM Identity Center.
--
-- Although AWS Single Sign-On was renamed, the @sso@ and @identitystore@
-- API namespaces will continue to retain their original name for backward
-- compatibility purposes. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/userguide/what-is.html#renamed IAM Identity Center rename>.
--
-- __Considerations for Using This Guide__
--
-- Before you begin using this guide, we recommend that you first review
-- the following important information about how the IAM Identity Center
-- OIDC service works.
--
-- -   The IAM Identity Center OIDC service currently implements only the
--     portions of the OAuth 2.0 Device Authorization Grant standard
--     (<https://tools.ietf.org/html/rfc8628>) that are necessary to enable
--     single sign-on authentication with the AWS CLI. Support for other
--     OIDC flows frequently needed for native applications, such as
--     Authorization Code Flow (+ PKCE), will be addressed in future
--     releases.
--
-- -   The service emits only OIDC access tokens, such that obtaining a new
--     token (For example, token refresh) requires explicit user
--     re-authentication.
--
-- -   The access tokens provided by this service grant access to all AWS
--     account entitlements assigned to an IAM Identity Center user, not
--     just a particular application.
--
-- -   The documentation in this guide does not describe the mechanism to
--     convert the access token into AWS Auth (“sigv4”) credentials for use
--     with IAM-protected AWS service endpoints. For more information, see
--     <https://docs.aws.amazon.com/singlesignon/latest/PortalAPIReference/API_GetRoleCredentials.html GetRoleCredentials>
--     in the /IAM Identity Center Portal API Reference Guide/.
--
-- For general information about IAM Identity Center, see
-- <https://docs.aws.amazon.com/singlesignon/latest/userguide/what-is.html What is IAM Identity Center?>
-- in the /IAM Identity Center User Guide/.
module Amazonka.SSOOIDC
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** AuthorizationPendingException
    _AuthorizationPendingException,

    -- ** ExpiredTokenException
    _ExpiredTokenException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** InvalidClientException
    _InvalidClientException,

    -- ** InvalidClientMetadataException
    _InvalidClientMetadataException,

    -- ** InvalidGrantException
    _InvalidGrantException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** InvalidScopeException
    _InvalidScopeException,

    -- ** SlowDownException
    _SlowDownException,

    -- ** UnauthorizedClientException
    _UnauthorizedClientException,

    -- ** UnsupportedGrantTypeException
    _UnsupportedGrantTypeException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateToken
    CreateToken (CreateToken'),
    newCreateToken,
    CreateTokenResponse (CreateTokenResponse'),
    newCreateTokenResponse,

    -- ** RegisterClient
    RegisterClient (RegisterClient'),
    newRegisterClient,
    RegisterClientResponse (RegisterClientResponse'),
    newRegisterClientResponse,

    -- ** StartDeviceAuthorization
    StartDeviceAuthorization (StartDeviceAuthorization'),
    newStartDeviceAuthorization,
    StartDeviceAuthorizationResponse (StartDeviceAuthorizationResponse'),
    newStartDeviceAuthorizationResponse,

    -- * Types
  )
where

import Amazonka.SSOOIDC.CreateToken
import Amazonka.SSOOIDC.Lens
import Amazonka.SSOOIDC.RegisterClient
import Amazonka.SSOOIDC.StartDeviceAuthorization
import Amazonka.SSOOIDC.Types
import Amazonka.SSOOIDC.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SSOOIDC'.

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
