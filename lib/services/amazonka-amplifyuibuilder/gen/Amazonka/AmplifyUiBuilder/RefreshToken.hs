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
-- Module      : Amazonka.AmplifyUiBuilder.RefreshToken
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Refreshes a previously issued access token that might have expired.
module Amazonka.AmplifyUiBuilder.RefreshToken
  ( -- * Creating a Request
    RefreshToken (..),
    newRefreshToken,

    -- * Request Lenses
    refreshToken_provider,
    refreshToken_refreshTokenBody,

    -- * Destructuring the Response
    RefreshTokenResponse (..),
    newRefreshTokenResponse,

    -- * Response Lenses
    refreshTokenResponse_httpStatus,
    refreshTokenResponse_accessToken,
    refreshTokenResponse_expiresIn,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRefreshToken' smart constructor.
data RefreshToken = RefreshToken'
  { -- | The third-party provider for the token. The only valid value is @figma@.
    provider :: TokenProviders,
    -- | Information about the refresh token request.
    refreshTokenBody :: RefreshTokenRequestBody
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RefreshToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provider', 'refreshToken_provider' - The third-party provider for the token. The only valid value is @figma@.
--
-- 'refreshTokenBody', 'refreshToken_refreshTokenBody' - Information about the refresh token request.
newRefreshToken ::
  -- | 'provider'
  TokenProviders ->
  -- | 'refreshTokenBody'
  RefreshTokenRequestBody ->
  RefreshToken
newRefreshToken pProvider_ pRefreshTokenBody_ =
  RefreshToken'
    { provider = pProvider_,
      refreshTokenBody = pRefreshTokenBody_
    }

-- | The third-party provider for the token. The only valid value is @figma@.
refreshToken_provider :: Lens.Lens' RefreshToken TokenProviders
refreshToken_provider = Lens.lens (\RefreshToken' {provider} -> provider) (\s@RefreshToken' {} a -> s {provider = a} :: RefreshToken)

-- | Information about the refresh token request.
refreshToken_refreshTokenBody :: Lens.Lens' RefreshToken RefreshTokenRequestBody
refreshToken_refreshTokenBody = Lens.lens (\RefreshToken' {refreshTokenBody} -> refreshTokenBody) (\s@RefreshToken' {} a -> s {refreshTokenBody = a} :: RefreshToken)

instance Core.AWSRequest RefreshToken where
  type AWSResponse RefreshToken = RefreshTokenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RefreshTokenResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "accessToken")
            Prelude.<*> (x Data..:> "expiresIn")
      )

instance Prelude.Hashable RefreshToken where
  hashWithSalt _salt RefreshToken' {..} =
    _salt
      `Prelude.hashWithSalt` provider
      `Prelude.hashWithSalt` refreshTokenBody

instance Prelude.NFData RefreshToken where
  rnf RefreshToken' {..} =
    Prelude.rnf provider `Prelude.seq`
      Prelude.rnf refreshTokenBody

instance Data.ToHeaders RefreshToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RefreshToken where
  toJSON RefreshToken' {..} =
    Data.toJSON refreshTokenBody

instance Data.ToPath RefreshToken where
  toPath RefreshToken' {..} =
    Prelude.mconcat
      ["/tokens/", Data.toBS provider, "/refresh"]

instance Data.ToQuery RefreshToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRefreshTokenResponse' smart constructor.
data RefreshTokenResponse = RefreshTokenResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The access token.
    accessToken :: Data.Sensitive Prelude.Text,
    -- | The date and time when the new access token expires.
    expiresIn :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RefreshTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'refreshTokenResponse_httpStatus' - The response's http status code.
--
-- 'accessToken', 'refreshTokenResponse_accessToken' - The access token.
--
-- 'expiresIn', 'refreshTokenResponse_expiresIn' - The date and time when the new access token expires.
newRefreshTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'accessToken'
  Prelude.Text ->
  -- | 'expiresIn'
  Prelude.Int ->
  RefreshTokenResponse
newRefreshTokenResponse
  pHttpStatus_
  pAccessToken_
  pExpiresIn_ =
    RefreshTokenResponse'
      { httpStatus = pHttpStatus_,
        accessToken = Data._Sensitive Lens.# pAccessToken_,
        expiresIn = pExpiresIn_
      }

-- | The response's http status code.
refreshTokenResponse_httpStatus :: Lens.Lens' RefreshTokenResponse Prelude.Int
refreshTokenResponse_httpStatus = Lens.lens (\RefreshTokenResponse' {httpStatus} -> httpStatus) (\s@RefreshTokenResponse' {} a -> s {httpStatus = a} :: RefreshTokenResponse)

-- | The access token.
refreshTokenResponse_accessToken :: Lens.Lens' RefreshTokenResponse Prelude.Text
refreshTokenResponse_accessToken = Lens.lens (\RefreshTokenResponse' {accessToken} -> accessToken) (\s@RefreshTokenResponse' {} a -> s {accessToken = a} :: RefreshTokenResponse) Prelude.. Data._Sensitive

-- | The date and time when the new access token expires.
refreshTokenResponse_expiresIn :: Lens.Lens' RefreshTokenResponse Prelude.Int
refreshTokenResponse_expiresIn = Lens.lens (\RefreshTokenResponse' {expiresIn} -> expiresIn) (\s@RefreshTokenResponse' {} a -> s {expiresIn = a} :: RefreshTokenResponse)

instance Prelude.NFData RefreshTokenResponse where
  rnf RefreshTokenResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf accessToken `Prelude.seq`
        Prelude.rnf expiresIn
