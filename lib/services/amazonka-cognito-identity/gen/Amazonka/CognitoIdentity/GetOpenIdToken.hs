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
-- Module      : Amazonka.CognitoIdentity.GetOpenIdToken
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an OpenID token, using a known Cognito ID. This known Cognito ID is
-- returned by GetId. You can optionally add additional logins for the
-- identity. Supplying multiple logins creates an implicit link.
--
-- The OpenID token is valid for 10 minutes.
--
-- This is a public API. You do not need any credentials to call this API.
module Amazonka.CognitoIdentity.GetOpenIdToken
  ( -- * Creating a Request
    GetOpenIdToken (..),
    newGetOpenIdToken,

    -- * Request Lenses
    getOpenIdToken_logins,
    getOpenIdToken_identityId,

    -- * Destructuring the Response
    GetOpenIdTokenResponse (..),
    newGetOpenIdTokenResponse,

    -- * Response Lenses
    getOpenIdTokenResponse_identityId,
    getOpenIdTokenResponse_token,
    getOpenIdTokenResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Input to the GetOpenIdToken action.
--
-- /See:/ 'newGetOpenIdToken' smart constructor.
data GetOpenIdToken = GetOpenIdToken'
  { -- | A set of optional name-value pairs that map provider names to provider
    -- tokens. When using graph.facebook.com and www.amazon.com, supply the
    -- access_token returned from the provider\'s authflow. For
    -- accounts.google.com, an Amazon Cognito user pool provider, or any other
    -- OpenID Connect provider, always include the @id_token@.
    logins :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique identifier in the format REGION:GUID.
    identityId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOpenIdToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logins', 'getOpenIdToken_logins' - A set of optional name-value pairs that map provider names to provider
-- tokens. When using graph.facebook.com and www.amazon.com, supply the
-- access_token returned from the provider\'s authflow. For
-- accounts.google.com, an Amazon Cognito user pool provider, or any other
-- OpenID Connect provider, always include the @id_token@.
--
-- 'identityId', 'getOpenIdToken_identityId' - A unique identifier in the format REGION:GUID.
newGetOpenIdToken ::
  -- | 'identityId'
  Prelude.Text ->
  GetOpenIdToken
newGetOpenIdToken pIdentityId_ =
  GetOpenIdToken'
    { logins = Prelude.Nothing,
      identityId = pIdentityId_
    }

-- | A set of optional name-value pairs that map provider names to provider
-- tokens. When using graph.facebook.com and www.amazon.com, supply the
-- access_token returned from the provider\'s authflow. For
-- accounts.google.com, an Amazon Cognito user pool provider, or any other
-- OpenID Connect provider, always include the @id_token@.
getOpenIdToken_logins :: Lens.Lens' GetOpenIdToken (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getOpenIdToken_logins = Lens.lens (\GetOpenIdToken' {logins} -> logins) (\s@GetOpenIdToken' {} a -> s {logins = a} :: GetOpenIdToken) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier in the format REGION:GUID.
getOpenIdToken_identityId :: Lens.Lens' GetOpenIdToken Prelude.Text
getOpenIdToken_identityId = Lens.lens (\GetOpenIdToken' {identityId} -> identityId) (\s@GetOpenIdToken' {} a -> s {identityId = a} :: GetOpenIdToken)

instance Core.AWSRequest GetOpenIdToken where
  type
    AWSResponse GetOpenIdToken =
      GetOpenIdTokenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOpenIdTokenResponse'
            Prelude.<$> (x Data..?> "IdentityId")
            Prelude.<*> (x Data..?> "Token")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOpenIdToken where
  hashWithSalt _salt GetOpenIdToken' {..} =
    _salt `Prelude.hashWithSalt` logins
      `Prelude.hashWithSalt` identityId

instance Prelude.NFData GetOpenIdToken where
  rnf GetOpenIdToken' {..} =
    Prelude.rnf logins
      `Prelude.seq` Prelude.rnf identityId

instance Data.ToHeaders GetOpenIdToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityService.GetOpenIdToken" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetOpenIdToken where
  toJSON GetOpenIdToken' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Logins" Data..=) Prelude.<$> logins,
            Prelude.Just ("IdentityId" Data..= identityId)
          ]
      )

instance Data.ToPath GetOpenIdToken where
  toPath = Prelude.const "/"

instance Data.ToQuery GetOpenIdToken where
  toQuery = Prelude.const Prelude.mempty

-- | Returned in response to a successful GetOpenIdToken request.
--
-- /See:/ 'newGetOpenIdTokenResponse' smart constructor.
data GetOpenIdTokenResponse = GetOpenIdTokenResponse'
  { -- | A unique identifier in the format REGION:GUID. Note that the IdentityId
    -- returned may not match the one passed on input.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | An OpenID token, valid for 10 minutes.
    token :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOpenIdTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityId', 'getOpenIdTokenResponse_identityId' - A unique identifier in the format REGION:GUID. Note that the IdentityId
-- returned may not match the one passed on input.
--
-- 'token', 'getOpenIdTokenResponse_token' - An OpenID token, valid for 10 minutes.
--
-- 'httpStatus', 'getOpenIdTokenResponse_httpStatus' - The response's http status code.
newGetOpenIdTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOpenIdTokenResponse
newGetOpenIdTokenResponse pHttpStatus_ =
  GetOpenIdTokenResponse'
    { identityId =
        Prelude.Nothing,
      token = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier in the format REGION:GUID. Note that the IdentityId
-- returned may not match the one passed on input.
getOpenIdTokenResponse_identityId :: Lens.Lens' GetOpenIdTokenResponse (Prelude.Maybe Prelude.Text)
getOpenIdTokenResponse_identityId = Lens.lens (\GetOpenIdTokenResponse' {identityId} -> identityId) (\s@GetOpenIdTokenResponse' {} a -> s {identityId = a} :: GetOpenIdTokenResponse)

-- | An OpenID token, valid for 10 minutes.
getOpenIdTokenResponse_token :: Lens.Lens' GetOpenIdTokenResponse (Prelude.Maybe Prelude.Text)
getOpenIdTokenResponse_token = Lens.lens (\GetOpenIdTokenResponse' {token} -> token) (\s@GetOpenIdTokenResponse' {} a -> s {token = a} :: GetOpenIdTokenResponse)

-- | The response's http status code.
getOpenIdTokenResponse_httpStatus :: Lens.Lens' GetOpenIdTokenResponse Prelude.Int
getOpenIdTokenResponse_httpStatus = Lens.lens (\GetOpenIdTokenResponse' {httpStatus} -> httpStatus) (\s@GetOpenIdTokenResponse' {} a -> s {httpStatus = a} :: GetOpenIdTokenResponse)

instance Prelude.NFData GetOpenIdTokenResponse where
  rnf GetOpenIdTokenResponse' {..} =
    Prelude.rnf identityId
      `Prelude.seq` Prelude.rnf token
      `Prelude.seq` Prelude.rnf httpStatus
