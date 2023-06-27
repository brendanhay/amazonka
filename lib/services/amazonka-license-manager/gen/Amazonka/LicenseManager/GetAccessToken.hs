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
-- Module      : Amazonka.LicenseManager.GetAccessToken
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a temporary access token to use with AssumeRoleWithWebIdentity.
-- Access tokens are valid for one hour.
module Amazonka.LicenseManager.GetAccessToken
  ( -- * Creating a Request
    GetAccessToken (..),
    newGetAccessToken,

    -- * Request Lenses
    getAccessToken_tokenProperties,
    getAccessToken_token,

    -- * Destructuring the Response
    GetAccessTokenResponse (..),
    newGetAccessTokenResponse,

    -- * Response Lenses
    getAccessTokenResponse_accessToken,
    getAccessTokenResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAccessToken' smart constructor.
data GetAccessToken = GetAccessToken'
  { -- | Token properties to validate against those present in the JWT token.
    tokenProperties :: Prelude.Maybe [Prelude.Text],
    -- | Refresh token, encoded as a JWT token.
    token :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccessToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tokenProperties', 'getAccessToken_tokenProperties' - Token properties to validate against those present in the JWT token.
--
-- 'token', 'getAccessToken_token' - Refresh token, encoded as a JWT token.
newGetAccessToken ::
  -- | 'token'
  Prelude.Text ->
  GetAccessToken
newGetAccessToken pToken_ =
  GetAccessToken'
    { tokenProperties = Prelude.Nothing,
      token = pToken_
    }

-- | Token properties to validate against those present in the JWT token.
getAccessToken_tokenProperties :: Lens.Lens' GetAccessToken (Prelude.Maybe [Prelude.Text])
getAccessToken_tokenProperties = Lens.lens (\GetAccessToken' {tokenProperties} -> tokenProperties) (\s@GetAccessToken' {} a -> s {tokenProperties = a} :: GetAccessToken) Prelude.. Lens.mapping Lens.coerced

-- | Refresh token, encoded as a JWT token.
getAccessToken_token :: Lens.Lens' GetAccessToken Prelude.Text
getAccessToken_token = Lens.lens (\GetAccessToken' {token} -> token) (\s@GetAccessToken' {} a -> s {token = a} :: GetAccessToken)

instance Core.AWSRequest GetAccessToken where
  type
    AWSResponse GetAccessToken =
      GetAccessTokenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccessTokenResponse'
            Prelude.<$> (x Data..?> "AccessToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccessToken where
  hashWithSalt _salt GetAccessToken' {..} =
    _salt
      `Prelude.hashWithSalt` tokenProperties
      `Prelude.hashWithSalt` token

instance Prelude.NFData GetAccessToken where
  rnf GetAccessToken' {..} =
    Prelude.rnf tokenProperties
      `Prelude.seq` Prelude.rnf token

instance Data.ToHeaders GetAccessToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.GetAccessToken" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAccessToken where
  toJSON GetAccessToken' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TokenProperties" Data..=)
              Prelude.<$> tokenProperties,
            Prelude.Just ("Token" Data..= token)
          ]
      )

instance Data.ToPath GetAccessToken where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAccessToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAccessTokenResponse' smart constructor.
data GetAccessTokenResponse = GetAccessTokenResponse'
  { -- | Temporary access token.
    accessToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccessTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'getAccessTokenResponse_accessToken' - Temporary access token.
--
-- 'httpStatus', 'getAccessTokenResponse_httpStatus' - The response's http status code.
newGetAccessTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccessTokenResponse
newGetAccessTokenResponse pHttpStatus_ =
  GetAccessTokenResponse'
    { accessToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Temporary access token.
getAccessTokenResponse_accessToken :: Lens.Lens' GetAccessTokenResponse (Prelude.Maybe Prelude.Text)
getAccessTokenResponse_accessToken = Lens.lens (\GetAccessTokenResponse' {accessToken} -> accessToken) (\s@GetAccessTokenResponse' {} a -> s {accessToken = a} :: GetAccessTokenResponse)

-- | The response's http status code.
getAccessTokenResponse_httpStatus :: Lens.Lens' GetAccessTokenResponse Prelude.Int
getAccessTokenResponse_httpStatus = Lens.lens (\GetAccessTokenResponse' {httpStatus} -> httpStatus) (\s@GetAccessTokenResponse' {} a -> s {httpStatus = a} :: GetAccessTokenResponse)

instance Prelude.NFData GetAccessTokenResponse where
  rnf GetAccessTokenResponse' {..} =
    Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf httpStatus
