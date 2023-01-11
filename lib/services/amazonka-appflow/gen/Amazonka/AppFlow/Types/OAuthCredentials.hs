{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppFlow.Types.OAuthCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.OAuthCredentials where

import Amazonka.AppFlow.Types.ConnectorOAuthRequest
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The OAuth credentials required for OAuth type authentication.
--
-- /See:/ 'newOAuthCredentials' smart constructor.
data OAuthCredentials = OAuthCredentials'
  { -- | The access token used to access protected SAPOData resources.
    accessToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The OAuth requirement needed to request security tokens from the
    -- connector endpoint.
    oAuthRequest :: Prelude.Maybe ConnectorOAuthRequest,
    -- | The refresh token used to refresh expired access token.
    refreshToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the desired client.
    clientId :: Prelude.Text,
    -- | The client secret used by the OAuth client to authenticate to the
    -- authorization server.
    clientSecret :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OAuthCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'oAuthCredentials_accessToken' - The access token used to access protected SAPOData resources.
--
-- 'oAuthRequest', 'oAuthCredentials_oAuthRequest' - The OAuth requirement needed to request security tokens from the
-- connector endpoint.
--
-- 'refreshToken', 'oAuthCredentials_refreshToken' - The refresh token used to refresh expired access token.
--
-- 'clientId', 'oAuthCredentials_clientId' - The identifier for the desired client.
--
-- 'clientSecret', 'oAuthCredentials_clientSecret' - The client secret used by the OAuth client to authenticate to the
-- authorization server.
newOAuthCredentials ::
  -- | 'clientId'
  Prelude.Text ->
  -- | 'clientSecret'
  Prelude.Text ->
  OAuthCredentials
newOAuthCredentials pClientId_ pClientSecret_ =
  OAuthCredentials'
    { accessToken = Prelude.Nothing,
      oAuthRequest = Prelude.Nothing,
      refreshToken = Prelude.Nothing,
      clientId = pClientId_,
      clientSecret = Data._Sensitive Lens.# pClientSecret_
    }

-- | The access token used to access protected SAPOData resources.
oAuthCredentials_accessToken :: Lens.Lens' OAuthCredentials (Prelude.Maybe Prelude.Text)
oAuthCredentials_accessToken = Lens.lens (\OAuthCredentials' {accessToken} -> accessToken) (\s@OAuthCredentials' {} a -> s {accessToken = a} :: OAuthCredentials) Prelude.. Lens.mapping Data._Sensitive

-- | The OAuth requirement needed to request security tokens from the
-- connector endpoint.
oAuthCredentials_oAuthRequest :: Lens.Lens' OAuthCredentials (Prelude.Maybe ConnectorOAuthRequest)
oAuthCredentials_oAuthRequest = Lens.lens (\OAuthCredentials' {oAuthRequest} -> oAuthRequest) (\s@OAuthCredentials' {} a -> s {oAuthRequest = a} :: OAuthCredentials)

-- | The refresh token used to refresh expired access token.
oAuthCredentials_refreshToken :: Lens.Lens' OAuthCredentials (Prelude.Maybe Prelude.Text)
oAuthCredentials_refreshToken = Lens.lens (\OAuthCredentials' {refreshToken} -> refreshToken) (\s@OAuthCredentials' {} a -> s {refreshToken = a} :: OAuthCredentials)

-- | The identifier for the desired client.
oAuthCredentials_clientId :: Lens.Lens' OAuthCredentials Prelude.Text
oAuthCredentials_clientId = Lens.lens (\OAuthCredentials' {clientId} -> clientId) (\s@OAuthCredentials' {} a -> s {clientId = a} :: OAuthCredentials)

-- | The client secret used by the OAuth client to authenticate to the
-- authorization server.
oAuthCredentials_clientSecret :: Lens.Lens' OAuthCredentials Prelude.Text
oAuthCredentials_clientSecret = Lens.lens (\OAuthCredentials' {clientSecret} -> clientSecret) (\s@OAuthCredentials' {} a -> s {clientSecret = a} :: OAuthCredentials) Prelude.. Data._Sensitive

instance Prelude.Hashable OAuthCredentials where
  hashWithSalt _salt OAuthCredentials' {..} =
    _salt `Prelude.hashWithSalt` accessToken
      `Prelude.hashWithSalt` oAuthRequest
      `Prelude.hashWithSalt` refreshToken
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` clientSecret

instance Prelude.NFData OAuthCredentials where
  rnf OAuthCredentials' {..} =
    Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf oAuthRequest
      `Prelude.seq` Prelude.rnf refreshToken
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf clientSecret

instance Data.ToJSON OAuthCredentials where
  toJSON OAuthCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accessToken" Data..=) Prelude.<$> accessToken,
            ("oAuthRequest" Data..=) Prelude.<$> oAuthRequest,
            ("refreshToken" Data..=) Prelude.<$> refreshToken,
            Prelude.Just ("clientId" Data..= clientId),
            Prelude.Just ("clientSecret" Data..= clientSecret)
          ]
      )
