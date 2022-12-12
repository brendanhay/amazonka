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
-- Module      : Amazonka.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity
  ( -- * Creating a Request
    GetOpenIdTokenForDeveloperIdentity (..),
    newGetOpenIdTokenForDeveloperIdentity,

    -- * Request Lenses
    getOpenIdTokenForDeveloperIdentity_identityId,
    getOpenIdTokenForDeveloperIdentity_principalTags,
    getOpenIdTokenForDeveloperIdentity_tokenDuration,
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

import Amazonka.CognitoIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Input to the @GetOpenIdTokenForDeveloperIdentity@ action.
--
-- /See:/ 'newGetOpenIdTokenForDeveloperIdentity' smart constructor.
data GetOpenIdTokenForDeveloperIdentity = GetOpenIdTokenForDeveloperIdentity'
  { -- | A unique identifier in the format REGION:GUID.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | Use this operation to configure attribute mappings for custom providers.
    principalTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
    tokenDuration :: Prelude.Maybe Prelude.Natural,
    -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Prelude.Text,
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
    logins :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOpenIdTokenForDeveloperIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityId', 'getOpenIdTokenForDeveloperIdentity_identityId' - A unique identifier in the format REGION:GUID.
--
-- 'principalTags', 'getOpenIdTokenForDeveloperIdentity_principalTags' - Use this operation to configure attribute mappings for custom providers.
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
  Prelude.Text ->
  GetOpenIdTokenForDeveloperIdentity
newGetOpenIdTokenForDeveloperIdentity
  pIdentityPoolId_ =
    GetOpenIdTokenForDeveloperIdentity'
      { identityId =
          Prelude.Nothing,
        principalTags = Prelude.Nothing,
        tokenDuration = Prelude.Nothing,
        identityPoolId = pIdentityPoolId_,
        logins = Prelude.mempty
      }

-- | A unique identifier in the format REGION:GUID.
getOpenIdTokenForDeveloperIdentity_identityId :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity (Prelude.Maybe Prelude.Text)
getOpenIdTokenForDeveloperIdentity_identityId = Lens.lens (\GetOpenIdTokenForDeveloperIdentity' {identityId} -> identityId) (\s@GetOpenIdTokenForDeveloperIdentity' {} a -> s {identityId = a} :: GetOpenIdTokenForDeveloperIdentity)

-- | Use this operation to configure attribute mappings for custom providers.
getOpenIdTokenForDeveloperIdentity_principalTags :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getOpenIdTokenForDeveloperIdentity_principalTags = Lens.lens (\GetOpenIdTokenForDeveloperIdentity' {principalTags} -> principalTags) (\s@GetOpenIdTokenForDeveloperIdentity' {} a -> s {principalTags = a} :: GetOpenIdTokenForDeveloperIdentity) Prelude.. Lens.mapping Lens.coerced

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
getOpenIdTokenForDeveloperIdentity_tokenDuration :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity (Prelude.Maybe Prelude.Natural)
getOpenIdTokenForDeveloperIdentity_tokenDuration = Lens.lens (\GetOpenIdTokenForDeveloperIdentity' {tokenDuration} -> tokenDuration) (\s@GetOpenIdTokenForDeveloperIdentity' {} a -> s {tokenDuration = a} :: GetOpenIdTokenForDeveloperIdentity)

-- | An identity pool ID in the format REGION:GUID.
getOpenIdTokenForDeveloperIdentity_identityPoolId :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity Prelude.Text
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
getOpenIdTokenForDeveloperIdentity_logins :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity (Prelude.HashMap Prelude.Text Prelude.Text)
getOpenIdTokenForDeveloperIdentity_logins = Lens.lens (\GetOpenIdTokenForDeveloperIdentity' {logins} -> logins) (\s@GetOpenIdTokenForDeveloperIdentity' {} a -> s {logins = a} :: GetOpenIdTokenForDeveloperIdentity) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    GetOpenIdTokenForDeveloperIdentity
  where
  type
    AWSResponse GetOpenIdTokenForDeveloperIdentity =
      GetOpenIdTokenForDeveloperIdentityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOpenIdTokenForDeveloperIdentityResponse'
            Prelude.<$> (x Data..?> "IdentityId")
              Prelude.<*> (x Data..?> "Token")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetOpenIdTokenForDeveloperIdentity
  where
  hashWithSalt
    _salt
    GetOpenIdTokenForDeveloperIdentity' {..} =
      _salt `Prelude.hashWithSalt` identityId
        `Prelude.hashWithSalt` principalTags
        `Prelude.hashWithSalt` tokenDuration
        `Prelude.hashWithSalt` identityPoolId
        `Prelude.hashWithSalt` logins

instance
  Prelude.NFData
    GetOpenIdTokenForDeveloperIdentity
  where
  rnf GetOpenIdTokenForDeveloperIdentity' {..} =
    Prelude.rnf identityId
      `Prelude.seq` Prelude.rnf principalTags
      `Prelude.seq` Prelude.rnf tokenDuration
      `Prelude.seq` Prelude.rnf identityPoolId
      `Prelude.seq` Prelude.rnf logins

instance
  Data.ToHeaders
    GetOpenIdTokenForDeveloperIdentity
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityService.GetOpenIdTokenForDeveloperIdentity" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    GetOpenIdTokenForDeveloperIdentity
  where
  toJSON GetOpenIdTokenForDeveloperIdentity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IdentityId" Data..=) Prelude.<$> identityId,
            ("PrincipalTags" Data..=) Prelude.<$> principalTags,
            ("TokenDuration" Data..=) Prelude.<$> tokenDuration,
            Prelude.Just
              ("IdentityPoolId" Data..= identityPoolId),
            Prelude.Just ("Logins" Data..= logins)
          ]
      )

instance
  Data.ToPath
    GetOpenIdTokenForDeveloperIdentity
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetOpenIdTokenForDeveloperIdentity
  where
  toQuery = Prelude.const Prelude.mempty

-- | Returned in response to a successful
-- @GetOpenIdTokenForDeveloperIdentity@ request.
--
-- /See:/ 'newGetOpenIdTokenForDeveloperIdentityResponse' smart constructor.
data GetOpenIdTokenForDeveloperIdentityResponse = GetOpenIdTokenForDeveloperIdentityResponse'
  { -- | A unique identifier in the format REGION:GUID.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | An OpenID token.
    token :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetOpenIdTokenForDeveloperIdentityResponse
newGetOpenIdTokenForDeveloperIdentityResponse
  pHttpStatus_ =
    GetOpenIdTokenForDeveloperIdentityResponse'
      { identityId =
          Prelude.Nothing,
        token = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A unique identifier in the format REGION:GUID.
getOpenIdTokenForDeveloperIdentityResponse_identityId :: Lens.Lens' GetOpenIdTokenForDeveloperIdentityResponse (Prelude.Maybe Prelude.Text)
getOpenIdTokenForDeveloperIdentityResponse_identityId = Lens.lens (\GetOpenIdTokenForDeveloperIdentityResponse' {identityId} -> identityId) (\s@GetOpenIdTokenForDeveloperIdentityResponse' {} a -> s {identityId = a} :: GetOpenIdTokenForDeveloperIdentityResponse)

-- | An OpenID token.
getOpenIdTokenForDeveloperIdentityResponse_token :: Lens.Lens' GetOpenIdTokenForDeveloperIdentityResponse (Prelude.Maybe Prelude.Text)
getOpenIdTokenForDeveloperIdentityResponse_token = Lens.lens (\GetOpenIdTokenForDeveloperIdentityResponse' {token} -> token) (\s@GetOpenIdTokenForDeveloperIdentityResponse' {} a -> s {token = a} :: GetOpenIdTokenForDeveloperIdentityResponse)

-- | The response's http status code.
getOpenIdTokenForDeveloperIdentityResponse_httpStatus :: Lens.Lens' GetOpenIdTokenForDeveloperIdentityResponse Prelude.Int
getOpenIdTokenForDeveloperIdentityResponse_httpStatus = Lens.lens (\GetOpenIdTokenForDeveloperIdentityResponse' {httpStatus} -> httpStatus) (\s@GetOpenIdTokenForDeveloperIdentityResponse' {} a -> s {httpStatus = a} :: GetOpenIdTokenForDeveloperIdentityResponse)

instance
  Prelude.NFData
    GetOpenIdTokenForDeveloperIdentityResponse
  where
  rnf GetOpenIdTokenForDeveloperIdentityResponse' {..} =
    Prelude.rnf identityId
      `Prelude.seq` Prelude.rnf token
      `Prelude.seq` Prelude.rnf httpStatus
