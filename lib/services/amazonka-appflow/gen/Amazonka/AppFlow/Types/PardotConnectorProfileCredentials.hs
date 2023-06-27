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
-- Module      : Amazonka.AppFlow.Types.PardotConnectorProfileCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.PardotConnectorProfileCredentials where

import Amazonka.AppFlow.Types.ConnectorOAuthRequest
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile credentials required when using
-- Salesforce Pardot.
--
-- /See:/ 'newPardotConnectorProfileCredentials' smart constructor.
data PardotConnectorProfileCredentials = PardotConnectorProfileCredentials'
  { -- | The credentials used to access protected Salesforce Pardot resources.
    accessToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The secret manager ARN, which contains the client ID and client secret
    -- of the connected app.
    clientCredentialsArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    oAuthRequest :: Prelude.Maybe ConnectorOAuthRequest,
    -- | The credentials used to acquire new access tokens.
    refreshToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PardotConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'pardotConnectorProfileCredentials_accessToken' - The credentials used to access protected Salesforce Pardot resources.
--
-- 'clientCredentialsArn', 'pardotConnectorProfileCredentials_clientCredentialsArn' - The secret manager ARN, which contains the client ID and client secret
-- of the connected app.
--
-- 'oAuthRequest', 'pardotConnectorProfileCredentials_oAuthRequest' - Undocumented member.
--
-- 'refreshToken', 'pardotConnectorProfileCredentials_refreshToken' - The credentials used to acquire new access tokens.
newPardotConnectorProfileCredentials ::
  PardotConnectorProfileCredentials
newPardotConnectorProfileCredentials =
  PardotConnectorProfileCredentials'
    { accessToken =
        Prelude.Nothing,
      clientCredentialsArn = Prelude.Nothing,
      oAuthRequest = Prelude.Nothing,
      refreshToken = Prelude.Nothing
    }

-- | The credentials used to access protected Salesforce Pardot resources.
pardotConnectorProfileCredentials_accessToken :: Lens.Lens' PardotConnectorProfileCredentials (Prelude.Maybe Prelude.Text)
pardotConnectorProfileCredentials_accessToken = Lens.lens (\PardotConnectorProfileCredentials' {accessToken} -> accessToken) (\s@PardotConnectorProfileCredentials' {} a -> s {accessToken = a} :: PardotConnectorProfileCredentials) Prelude.. Lens.mapping Data._Sensitive

-- | The secret manager ARN, which contains the client ID and client secret
-- of the connected app.
pardotConnectorProfileCredentials_clientCredentialsArn :: Lens.Lens' PardotConnectorProfileCredentials (Prelude.Maybe Prelude.Text)
pardotConnectorProfileCredentials_clientCredentialsArn = Lens.lens (\PardotConnectorProfileCredentials' {clientCredentialsArn} -> clientCredentialsArn) (\s@PardotConnectorProfileCredentials' {} a -> s {clientCredentialsArn = a} :: PardotConnectorProfileCredentials) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
pardotConnectorProfileCredentials_oAuthRequest :: Lens.Lens' PardotConnectorProfileCredentials (Prelude.Maybe ConnectorOAuthRequest)
pardotConnectorProfileCredentials_oAuthRequest = Lens.lens (\PardotConnectorProfileCredentials' {oAuthRequest} -> oAuthRequest) (\s@PardotConnectorProfileCredentials' {} a -> s {oAuthRequest = a} :: PardotConnectorProfileCredentials)

-- | The credentials used to acquire new access tokens.
pardotConnectorProfileCredentials_refreshToken :: Lens.Lens' PardotConnectorProfileCredentials (Prelude.Maybe Prelude.Text)
pardotConnectorProfileCredentials_refreshToken = Lens.lens (\PardotConnectorProfileCredentials' {refreshToken} -> refreshToken) (\s@PardotConnectorProfileCredentials' {} a -> s {refreshToken = a} :: PardotConnectorProfileCredentials)

instance
  Prelude.Hashable
    PardotConnectorProfileCredentials
  where
  hashWithSalt
    _salt
    PardotConnectorProfileCredentials' {..} =
      _salt
        `Prelude.hashWithSalt` accessToken
        `Prelude.hashWithSalt` clientCredentialsArn
        `Prelude.hashWithSalt` oAuthRequest
        `Prelude.hashWithSalt` refreshToken

instance
  Prelude.NFData
    PardotConnectorProfileCredentials
  where
  rnf PardotConnectorProfileCredentials' {..} =
    Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf clientCredentialsArn
      `Prelude.seq` Prelude.rnf oAuthRequest
      `Prelude.seq` Prelude.rnf refreshToken

instance
  Data.ToJSON
    PardotConnectorProfileCredentials
  where
  toJSON PardotConnectorProfileCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accessToken" Data..=) Prelude.<$> accessToken,
            ("clientCredentialsArn" Data..=)
              Prelude.<$> clientCredentialsArn,
            ("oAuthRequest" Data..=) Prelude.<$> oAuthRequest,
            ("refreshToken" Data..=) Prelude.<$> refreshToken
          ]
      )
