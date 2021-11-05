{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSOOIDC.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOOIDC.Lens
  ( -- * Operations

    -- ** StartDeviceAuthorization
    startDeviceAuthorization_clientId,
    startDeviceAuthorization_clientSecret,
    startDeviceAuthorization_startUrl,
    startDeviceAuthorizationResponse_userCode,
    startDeviceAuthorizationResponse_interval,
    startDeviceAuthorizationResponse_expiresIn,
    startDeviceAuthorizationResponse_verificationUri,
    startDeviceAuthorizationResponse_deviceCode,
    startDeviceAuthorizationResponse_verificationUriComplete,
    startDeviceAuthorizationResponse_httpStatus,

    -- ** CreateToken
    createToken_redirectUri,
    createToken_refreshToken,
    createToken_scope,
    createToken_code,
    createToken_clientId,
    createToken_clientSecret,
    createToken_grantType,
    createToken_deviceCode,
    createTokenResponse_accessToken,
    createTokenResponse_refreshToken,
    createTokenResponse_expiresIn,
    createTokenResponse_tokenType,
    createTokenResponse_idToken,
    createTokenResponse_httpStatus,

    -- ** RegisterClient
    registerClient_scopes,
    registerClient_clientName,
    registerClient_clientType,
    registerClientResponse_clientId,
    registerClientResponse_clientSecret,
    registerClientResponse_clientIdIssuedAt,
    registerClientResponse_clientSecretExpiresAt,
    registerClientResponse_tokenEndpoint,
    registerClientResponse_authorizationEndpoint,
    registerClientResponse_httpStatus,

    -- * Types
  )
where

import Amazonka.SSOOIDC.CreateToken
import Amazonka.SSOOIDC.RegisterClient
import Amazonka.SSOOIDC.StartDeviceAuthorization
