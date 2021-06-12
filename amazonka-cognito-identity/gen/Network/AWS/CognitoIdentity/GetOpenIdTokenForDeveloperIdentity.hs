{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers (or retrieves) a Cognito @IdentityId@ and an OpenID Connect
-- token for a user authenticated by your backend authentication process.
-- Supplying multiple logins will create an implicit linked account. You
-- can only specify one developer provider as part of the @Logins@ map,
-- which is linked to the identity pool. The developer provider is the
-- \"domain\" by which Cognito will refer to your users.
--
-- You can use @GetOpenIdTokenForDeveloperIdentity@ to create a new
-- identity and to link new logins (that is, user credentials issued by a
-- public provider or developer provider) to an existing identity. When you
-- want to create a new identity, the @IdentityId@ should be null. When you
-- want to associate a new login with an existing
-- authenticated\/unauthenticated identity, you can do so by providing the
-- existing @IdentityId@. This API will create the identity in the
-- specified @IdentityPoolId@.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity
  ( -- * Creating a Request
    GetOpenIdTokenForDeveloperIdentity (..),
    newGetOpenIdTokenForDeveloperIdentity,

    -- * Request Lenses
    getOpenIdTokenForDeveloperIdentity_tokenDuration,
    getOpenIdTokenForDeveloperIdentity_identityId,
    getOpenIdTokenForDeveloperIdentity_principalTags,
    getOpenIdTokenForDeveloperIdentity_identityPoolId,
    getOpenIdTokenForDeveloperIdentity_logins,

    -- * Destructuring the Response
    GetOpenIdTokenForDeveloperIdentityResponse (..),
    newGetOpenIdTokenForDeveloperIdentityResponse,

    -- * Response Lenses
    getOpenIdTokenForDeveloperIdentityResponse_identityId,
    getOpenIdTokenForDeveloperIdentityResponse_token,
    getOpenIdTokenForDeveloperIdentityResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the @GetOpenIdTokenForDeveloperIdentity@ action.
--
-- /See:/ 'newGetOpenIdTokenForDeveloperIdentity' smart constructor.
data GetOpenIdTokenForDeveloperIdentity = GetOpenIdTokenForDeveloperIdentity'
  { -- | The expiration time of the token, in seconds. You can specify a custom
    -- expiration time for the token so that you can cache it. If you don\'t
    -- provide an expiration time, the token is valid for 15 minutes. You can
    -- exchange the token with Amazon STS for temporary AWS credentials, which
    -- are valid for a maximum of one hour. The maximum token duration you can
    -- set is 24 hours. You should take care in setting the expiration time for
    -- a token, as there are significant security implications: an attacker
    -- could use a leaked token to access your AWS resources for the token\'s
    -- duration.
    --
    -- Please provide for a small grace period, usually no more than 5 minutes,
    -- to account for clock skew.
    tokenDuration :: Core.Maybe Core.Natural,
    -- | A unique identifier in the format REGION:GUID.
    identityId :: Core.Maybe Core.Text,
    -- | Use this operation to configure attribute mappings for custom providers.
    principalTags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Core.Text,
    -- | A set of optional name-value pairs that map provider names to provider
    -- tokens. Each name-value pair represents a user from a public provider or
    -- developer provider. If the user is from a developer provider, the
    -- name-value pair will follow the syntax
    -- @\"developer_provider_name\": \"developer_user_identifier\"@. The
    -- developer provider is the \"domain\" by which Cognito will refer to your
    -- users; you provided this domain while creating\/updating the identity
    -- pool. The developer user identifier is an identifier from your backend
    -- that uniquely identifies a user. When you create an identity pool, you
    -- can specify the supported logins.
    logins :: Core.HashMap Core.Text Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetOpenIdTokenForDeveloperIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tokenDuration', 'getOpenIdTokenForDeveloperIdentity_tokenDuration' - The expiration time of the token, in seconds. You can specify a custom
-- expiration time for the token so that you can cache it. If you don\'t
-- provide an expiration time, the token is valid for 15 minutes. You can
-- exchange the token with Amazon STS for temporary AWS credentials, which
-- are valid for a maximum of one hour. The maximum token duration you can
-- set is 24 hours. You should take care in setting the expiration time for
-- a token, as there are significant security implications: an attacker
-- could use a leaked token to access your AWS resources for the token\'s
-- duration.
--
-- Please provide for a small grace period, usually no more than 5 minutes,
-- to account for clock skew.
--
-- 'identityId', 'getOpenIdTokenForDeveloperIdentity_identityId' - A unique identifier in the format REGION:GUID.
--
-- 'principalTags', 'getOpenIdTokenForDeveloperIdentity_principalTags' - Use this operation to configure attribute mappings for custom providers.
--
-- 'identityPoolId', 'getOpenIdTokenForDeveloperIdentity_identityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- 'logins', 'getOpenIdTokenForDeveloperIdentity_logins' - A set of optional name-value pairs that map provider names to provider
-- tokens. Each name-value pair represents a user from a public provider or
-- developer provider. If the user is from a developer provider, the
-- name-value pair will follow the syntax
-- @\"developer_provider_name\": \"developer_user_identifier\"@. The
-- developer provider is the \"domain\" by which Cognito will refer to your
-- users; you provided this domain while creating\/updating the identity
-- pool. The developer user identifier is an identifier from your backend
-- that uniquely identifies a user. When you create an identity pool, you
-- can specify the supported logins.
newGetOpenIdTokenForDeveloperIdentity ::
  -- | 'identityPoolId'
  Core.Text ->
  GetOpenIdTokenForDeveloperIdentity
newGetOpenIdTokenForDeveloperIdentity
  pIdentityPoolId_ =
    GetOpenIdTokenForDeveloperIdentity'
      { tokenDuration =
          Core.Nothing,
        identityId = Core.Nothing,
        principalTags = Core.Nothing,
        identityPoolId = pIdentityPoolId_,
        logins = Core.mempty
      }

-- | The expiration time of the token, in seconds. You can specify a custom
-- expiration time for the token so that you can cache it. If you don\'t
-- provide an expiration time, the token is valid for 15 minutes. You can
-- exchange the token with Amazon STS for temporary AWS credentials, which
-- are valid for a maximum of one hour. The maximum token duration you can
-- set is 24 hours. You should take care in setting the expiration time for
-- a token, as there are significant security implications: an attacker
-- could use a leaked token to access your AWS resources for the token\'s
-- duration.
--
-- Please provide for a small grace period, usually no more than 5 minutes,
-- to account for clock skew.
getOpenIdTokenForDeveloperIdentity_tokenDuration :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity (Core.Maybe Core.Natural)
getOpenIdTokenForDeveloperIdentity_tokenDuration = Lens.lens (\GetOpenIdTokenForDeveloperIdentity' {tokenDuration} -> tokenDuration) (\s@GetOpenIdTokenForDeveloperIdentity' {} a -> s {tokenDuration = a} :: GetOpenIdTokenForDeveloperIdentity)

-- | A unique identifier in the format REGION:GUID.
getOpenIdTokenForDeveloperIdentity_identityId :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity (Core.Maybe Core.Text)
getOpenIdTokenForDeveloperIdentity_identityId = Lens.lens (\GetOpenIdTokenForDeveloperIdentity' {identityId} -> identityId) (\s@GetOpenIdTokenForDeveloperIdentity' {} a -> s {identityId = a} :: GetOpenIdTokenForDeveloperIdentity)

-- | Use this operation to configure attribute mappings for custom providers.
getOpenIdTokenForDeveloperIdentity_principalTags :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity (Core.Maybe (Core.HashMap Core.Text Core.Text))
getOpenIdTokenForDeveloperIdentity_principalTags = Lens.lens (\GetOpenIdTokenForDeveloperIdentity' {principalTags} -> principalTags) (\s@GetOpenIdTokenForDeveloperIdentity' {} a -> s {principalTags = a} :: GetOpenIdTokenForDeveloperIdentity) Core.. Lens.mapping Lens._Coerce

-- | An identity pool ID in the format REGION:GUID.
getOpenIdTokenForDeveloperIdentity_identityPoolId :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity Core.Text
getOpenIdTokenForDeveloperIdentity_identityPoolId = Lens.lens (\GetOpenIdTokenForDeveloperIdentity' {identityPoolId} -> identityPoolId) (\s@GetOpenIdTokenForDeveloperIdentity' {} a -> s {identityPoolId = a} :: GetOpenIdTokenForDeveloperIdentity)

-- | A set of optional name-value pairs that map provider names to provider
-- tokens. Each name-value pair represents a user from a public provider or
-- developer provider. If the user is from a developer provider, the
-- name-value pair will follow the syntax
-- @\"developer_provider_name\": \"developer_user_identifier\"@. The
-- developer provider is the \"domain\" by which Cognito will refer to your
-- users; you provided this domain while creating\/updating the identity
-- pool. The developer user identifier is an identifier from your backend
-- that uniquely identifies a user. When you create an identity pool, you
-- can specify the supported logins.
getOpenIdTokenForDeveloperIdentity_logins :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity (Core.HashMap Core.Text Core.Text)
getOpenIdTokenForDeveloperIdentity_logins = Lens.lens (\GetOpenIdTokenForDeveloperIdentity' {logins} -> logins) (\s@GetOpenIdTokenForDeveloperIdentity' {} a -> s {logins = a} :: GetOpenIdTokenForDeveloperIdentity) Core.. Lens._Coerce

instance
  Core.AWSRequest
    GetOpenIdTokenForDeveloperIdentity
  where
  type
    AWSResponse GetOpenIdTokenForDeveloperIdentity =
      GetOpenIdTokenForDeveloperIdentityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOpenIdTokenForDeveloperIdentityResponse'
            Core.<$> (x Core..?> "IdentityId")
            Core.<*> (x Core..?> "Token")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetOpenIdTokenForDeveloperIdentity

instance
  Core.NFData
    GetOpenIdTokenForDeveloperIdentity

instance
  Core.ToHeaders
    GetOpenIdTokenForDeveloperIdentity
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityService.GetOpenIdTokenForDeveloperIdentity" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    GetOpenIdTokenForDeveloperIdentity
  where
  toJSON GetOpenIdTokenForDeveloperIdentity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TokenDuration" Core..=) Core.<$> tokenDuration,
            ("IdentityId" Core..=) Core.<$> identityId,
            ("PrincipalTags" Core..=) Core.<$> principalTags,
            Core.Just ("IdentityPoolId" Core..= identityPoolId),
            Core.Just ("Logins" Core..= logins)
          ]
      )

instance
  Core.ToPath
    GetOpenIdTokenForDeveloperIdentity
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetOpenIdTokenForDeveloperIdentity
  where
  toQuery = Core.const Core.mempty

-- | Returned in response to a successful
-- @GetOpenIdTokenForDeveloperIdentity@ request.
--
-- /See:/ 'newGetOpenIdTokenForDeveloperIdentityResponse' smart constructor.
data GetOpenIdTokenForDeveloperIdentityResponse = GetOpenIdTokenForDeveloperIdentityResponse'
  { -- | A unique identifier in the format REGION:GUID.
    identityId :: Core.Maybe Core.Text,
    -- | An OpenID token.
    token :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetOpenIdTokenForDeveloperIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityId', 'getOpenIdTokenForDeveloperIdentityResponse_identityId' - A unique identifier in the format REGION:GUID.
--
-- 'token', 'getOpenIdTokenForDeveloperIdentityResponse_token' - An OpenID token.
--
-- 'httpStatus', 'getOpenIdTokenForDeveloperIdentityResponse_httpStatus' - The response's http status code.
newGetOpenIdTokenForDeveloperIdentityResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetOpenIdTokenForDeveloperIdentityResponse
newGetOpenIdTokenForDeveloperIdentityResponse
  pHttpStatus_ =
    GetOpenIdTokenForDeveloperIdentityResponse'
      { identityId =
          Core.Nothing,
        token = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A unique identifier in the format REGION:GUID.
getOpenIdTokenForDeveloperIdentityResponse_identityId :: Lens.Lens' GetOpenIdTokenForDeveloperIdentityResponse (Core.Maybe Core.Text)
getOpenIdTokenForDeveloperIdentityResponse_identityId = Lens.lens (\GetOpenIdTokenForDeveloperIdentityResponse' {identityId} -> identityId) (\s@GetOpenIdTokenForDeveloperIdentityResponse' {} a -> s {identityId = a} :: GetOpenIdTokenForDeveloperIdentityResponse)

-- | An OpenID token.
getOpenIdTokenForDeveloperIdentityResponse_token :: Lens.Lens' GetOpenIdTokenForDeveloperIdentityResponse (Core.Maybe Core.Text)
getOpenIdTokenForDeveloperIdentityResponse_token = Lens.lens (\GetOpenIdTokenForDeveloperIdentityResponse' {token} -> token) (\s@GetOpenIdTokenForDeveloperIdentityResponse' {} a -> s {token = a} :: GetOpenIdTokenForDeveloperIdentityResponse)

-- | The response's http status code.
getOpenIdTokenForDeveloperIdentityResponse_httpStatus :: Lens.Lens' GetOpenIdTokenForDeveloperIdentityResponse Core.Int
getOpenIdTokenForDeveloperIdentityResponse_httpStatus = Lens.lens (\GetOpenIdTokenForDeveloperIdentityResponse' {httpStatus} -> httpStatus) (\s@GetOpenIdTokenForDeveloperIdentityResponse' {} a -> s {httpStatus = a} :: GetOpenIdTokenForDeveloperIdentityResponse)

instance
  Core.NFData
    GetOpenIdTokenForDeveloperIdentityResponse
