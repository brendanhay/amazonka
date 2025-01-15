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
-- Module      : Amazonka.AmplifyUiBuilder.ExchangeCodeForToken
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exchanges an access code for a token.
module Amazonka.AmplifyUiBuilder.ExchangeCodeForToken
  ( -- * Creating a Request
    ExchangeCodeForToken (..),
    newExchangeCodeForToken,

    -- * Request Lenses
    exchangeCodeForToken_provider,
    exchangeCodeForToken_request,

    -- * Destructuring the Response
    ExchangeCodeForTokenResponse (..),
    newExchangeCodeForTokenResponse,

    -- * Response Lenses
    exchangeCodeForTokenResponse_httpStatus,
    exchangeCodeForTokenResponse_accessToken,
    exchangeCodeForTokenResponse_expiresIn,
    exchangeCodeForTokenResponse_refreshToken,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExchangeCodeForToken' smart constructor.
data ExchangeCodeForToken = ExchangeCodeForToken'
  { -- | The third-party provider for the token. The only valid value is @figma@.
    provider :: TokenProviders,
    -- | Describes the configuration of the request.
    request :: ExchangeCodeForTokenRequestBody
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExchangeCodeForToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provider', 'exchangeCodeForToken_provider' - The third-party provider for the token. The only valid value is @figma@.
--
-- 'request', 'exchangeCodeForToken_request' - Describes the configuration of the request.
newExchangeCodeForToken ::
  -- | 'provider'
  TokenProviders ->
  -- | 'request'
  ExchangeCodeForTokenRequestBody ->
  ExchangeCodeForToken
newExchangeCodeForToken pProvider_ pRequest_ =
  ExchangeCodeForToken'
    { provider = pProvider_,
      request = pRequest_
    }

-- | The third-party provider for the token. The only valid value is @figma@.
exchangeCodeForToken_provider :: Lens.Lens' ExchangeCodeForToken TokenProviders
exchangeCodeForToken_provider = Lens.lens (\ExchangeCodeForToken' {provider} -> provider) (\s@ExchangeCodeForToken' {} a -> s {provider = a} :: ExchangeCodeForToken)

-- | Describes the configuration of the request.
exchangeCodeForToken_request :: Lens.Lens' ExchangeCodeForToken ExchangeCodeForTokenRequestBody
exchangeCodeForToken_request = Lens.lens (\ExchangeCodeForToken' {request} -> request) (\s@ExchangeCodeForToken' {} a -> s {request = a} :: ExchangeCodeForToken)

instance Core.AWSRequest ExchangeCodeForToken where
  type
    AWSResponse ExchangeCodeForToken =
      ExchangeCodeForTokenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExchangeCodeForTokenResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "accessToken")
            Prelude.<*> (x Data..:> "expiresIn")
            Prelude.<*> (x Data..:> "refreshToken")
      )

instance Prelude.Hashable ExchangeCodeForToken where
  hashWithSalt _salt ExchangeCodeForToken' {..} =
    _salt
      `Prelude.hashWithSalt` provider
      `Prelude.hashWithSalt` request

instance Prelude.NFData ExchangeCodeForToken where
  rnf ExchangeCodeForToken' {..} =
    Prelude.rnf provider `Prelude.seq`
      Prelude.rnf request

instance Data.ToHeaders ExchangeCodeForToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExchangeCodeForToken where
  toJSON ExchangeCodeForToken' {..} =
    Data.toJSON request

instance Data.ToPath ExchangeCodeForToken where
  toPath ExchangeCodeForToken' {..} =
    Prelude.mconcat ["/tokens/", Data.toBS provider]

instance Data.ToQuery ExchangeCodeForToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExchangeCodeForTokenResponse' smart constructor.
data ExchangeCodeForTokenResponse = ExchangeCodeForTokenResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The access token.
    accessToken :: Data.Sensitive Prelude.Text,
    -- | The date and time when the new access token expires.
    expiresIn :: Prelude.Int,
    -- | The token to use to refresh a previously issued access token that might
    -- have expired.
    refreshToken :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExchangeCodeForTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'exchangeCodeForTokenResponse_httpStatus' - The response's http status code.
--
-- 'accessToken', 'exchangeCodeForTokenResponse_accessToken' - The access token.
--
-- 'expiresIn', 'exchangeCodeForTokenResponse_expiresIn' - The date and time when the new access token expires.
--
-- 'refreshToken', 'exchangeCodeForTokenResponse_refreshToken' - The token to use to refresh a previously issued access token that might
-- have expired.
newExchangeCodeForTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'accessToken'
  Prelude.Text ->
  -- | 'expiresIn'
  Prelude.Int ->
  -- | 'refreshToken'
  Prelude.Text ->
  ExchangeCodeForTokenResponse
newExchangeCodeForTokenResponse
  pHttpStatus_
  pAccessToken_
  pExpiresIn_
  pRefreshToken_ =
    ExchangeCodeForTokenResponse'
      { httpStatus =
          pHttpStatus_,
        accessToken =
          Data._Sensitive Lens.# pAccessToken_,
        expiresIn = pExpiresIn_,
        refreshToken =
          Data._Sensitive Lens.# pRefreshToken_
      }

-- | The response's http status code.
exchangeCodeForTokenResponse_httpStatus :: Lens.Lens' ExchangeCodeForTokenResponse Prelude.Int
exchangeCodeForTokenResponse_httpStatus = Lens.lens (\ExchangeCodeForTokenResponse' {httpStatus} -> httpStatus) (\s@ExchangeCodeForTokenResponse' {} a -> s {httpStatus = a} :: ExchangeCodeForTokenResponse)

-- | The access token.
exchangeCodeForTokenResponse_accessToken :: Lens.Lens' ExchangeCodeForTokenResponse Prelude.Text
exchangeCodeForTokenResponse_accessToken = Lens.lens (\ExchangeCodeForTokenResponse' {accessToken} -> accessToken) (\s@ExchangeCodeForTokenResponse' {} a -> s {accessToken = a} :: ExchangeCodeForTokenResponse) Prelude.. Data._Sensitive

-- | The date and time when the new access token expires.
exchangeCodeForTokenResponse_expiresIn :: Lens.Lens' ExchangeCodeForTokenResponse Prelude.Int
exchangeCodeForTokenResponse_expiresIn = Lens.lens (\ExchangeCodeForTokenResponse' {expiresIn} -> expiresIn) (\s@ExchangeCodeForTokenResponse' {} a -> s {expiresIn = a} :: ExchangeCodeForTokenResponse)

-- | The token to use to refresh a previously issued access token that might
-- have expired.
exchangeCodeForTokenResponse_refreshToken :: Lens.Lens' ExchangeCodeForTokenResponse Prelude.Text
exchangeCodeForTokenResponse_refreshToken = Lens.lens (\ExchangeCodeForTokenResponse' {refreshToken} -> refreshToken) (\s@ExchangeCodeForTokenResponse' {} a -> s {refreshToken = a} :: ExchangeCodeForTokenResponse) Prelude.. Data._Sensitive

instance Prelude.NFData ExchangeCodeForTokenResponse where
  rnf ExchangeCodeForTokenResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf accessToken `Prelude.seq`
        Prelude.rnf expiresIn `Prelude.seq`
          Prelude.rnf refreshToken
