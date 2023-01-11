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
-- Module      : Amazonka.AppFlow.Types.OAuth2Credentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.OAuth2Credentials where

import Amazonka.AppFlow.Types.ConnectorOAuthRequest
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The OAuth 2.0 credentials required for OAuth 2.0 authentication.
--
-- /See:/ 'newOAuth2Credentials' smart constructor.
data OAuth2Credentials = OAuth2Credentials'
  { -- | The access token used to access the connector on your behalf.
    accessToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The identifier for the desired client.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | The client secret used by the OAuth client to authenticate to the
    -- authorization server.
    clientSecret :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    oAuthRequest :: Prelude.Maybe ConnectorOAuthRequest,
    -- | The refresh token used to refresh an expired access token.
    refreshToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OAuth2Credentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'oAuth2Credentials_accessToken' - The access token used to access the connector on your behalf.
--
-- 'clientId', 'oAuth2Credentials_clientId' - The identifier for the desired client.
--
-- 'clientSecret', 'oAuth2Credentials_clientSecret' - The client secret used by the OAuth client to authenticate to the
-- authorization server.
--
-- 'oAuthRequest', 'oAuth2Credentials_oAuthRequest' - Undocumented member.
--
-- 'refreshToken', 'oAuth2Credentials_refreshToken' - The refresh token used to refresh an expired access token.
newOAuth2Credentials ::
  OAuth2Credentials
newOAuth2Credentials =
  OAuth2Credentials'
    { accessToken = Prelude.Nothing,
      clientId = Prelude.Nothing,
      clientSecret = Prelude.Nothing,
      oAuthRequest = Prelude.Nothing,
      refreshToken = Prelude.Nothing
    }

-- | The access token used to access the connector on your behalf.
oAuth2Credentials_accessToken :: Lens.Lens' OAuth2Credentials (Prelude.Maybe Prelude.Text)
oAuth2Credentials_accessToken = Lens.lens (\OAuth2Credentials' {accessToken} -> accessToken) (\s@OAuth2Credentials' {} a -> s {accessToken = a} :: OAuth2Credentials) Prelude.. Lens.mapping Data._Sensitive

-- | The identifier for the desired client.
oAuth2Credentials_clientId :: Lens.Lens' OAuth2Credentials (Prelude.Maybe Prelude.Text)
oAuth2Credentials_clientId = Lens.lens (\OAuth2Credentials' {clientId} -> clientId) (\s@OAuth2Credentials' {} a -> s {clientId = a} :: OAuth2Credentials)

-- | The client secret used by the OAuth client to authenticate to the
-- authorization server.
oAuth2Credentials_clientSecret :: Lens.Lens' OAuth2Credentials (Prelude.Maybe Prelude.Text)
oAuth2Credentials_clientSecret = Lens.lens (\OAuth2Credentials' {clientSecret} -> clientSecret) (\s@OAuth2Credentials' {} a -> s {clientSecret = a} :: OAuth2Credentials) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
oAuth2Credentials_oAuthRequest :: Lens.Lens' OAuth2Credentials (Prelude.Maybe ConnectorOAuthRequest)
oAuth2Credentials_oAuthRequest = Lens.lens (\OAuth2Credentials' {oAuthRequest} -> oAuthRequest) (\s@OAuth2Credentials' {} a -> s {oAuthRequest = a} :: OAuth2Credentials)

-- | The refresh token used to refresh an expired access token.
oAuth2Credentials_refreshToken :: Lens.Lens' OAuth2Credentials (Prelude.Maybe Prelude.Text)
oAuth2Credentials_refreshToken = Lens.lens (\OAuth2Credentials' {refreshToken} -> refreshToken) (\s@OAuth2Credentials' {} a -> s {refreshToken = a} :: OAuth2Credentials)

instance Prelude.Hashable OAuth2Credentials where
  hashWithSalt _salt OAuth2Credentials' {..} =
    _salt `Prelude.hashWithSalt` accessToken
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` clientSecret
      `Prelude.hashWithSalt` oAuthRequest
      `Prelude.hashWithSalt` refreshToken

instance Prelude.NFData OAuth2Credentials where
  rnf OAuth2Credentials' {..} =
    Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf clientSecret
      `Prelude.seq` Prelude.rnf oAuthRequest
      `Prelude.seq` Prelude.rnf refreshToken

instance Data.ToJSON OAuth2Credentials where
  toJSON OAuth2Credentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accessToken" Data..=) Prelude.<$> accessToken,
            ("clientId" Data..=) Prelude.<$> clientId,
            ("clientSecret" Data..=) Prelude.<$> clientSecret,
            ("oAuthRequest" Data..=) Prelude.<$> oAuthRequest,
            ("refreshToken" Data..=) Prelude.<$> refreshToken
          ]
      )
