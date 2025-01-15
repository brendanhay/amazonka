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
-- Module      : Amazonka.AppFlow.Types.MarketoConnectorProfileCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.MarketoConnectorProfileCredentials where

import Amazonka.AppFlow.Types.ConnectorOAuthRequest
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile credentials required by Marketo.
--
-- /See:/ 'newMarketoConnectorProfileCredentials' smart constructor.
data MarketoConnectorProfileCredentials = MarketoConnectorProfileCredentials'
  { -- | The credentials used to access protected Marketo resources.
    accessToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The OAuth requirement needed to request security tokens from the
    -- connector endpoint.
    oAuthRequest :: Prelude.Maybe ConnectorOAuthRequest,
    -- | The identifier for the desired client.
    clientId :: Prelude.Text,
    -- | The client secret used by the OAuth client to authenticate to the
    -- authorization server.
    clientSecret :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MarketoConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'marketoConnectorProfileCredentials_accessToken' - The credentials used to access protected Marketo resources.
--
-- 'oAuthRequest', 'marketoConnectorProfileCredentials_oAuthRequest' - The OAuth requirement needed to request security tokens from the
-- connector endpoint.
--
-- 'clientId', 'marketoConnectorProfileCredentials_clientId' - The identifier for the desired client.
--
-- 'clientSecret', 'marketoConnectorProfileCredentials_clientSecret' - The client secret used by the OAuth client to authenticate to the
-- authorization server.
newMarketoConnectorProfileCredentials ::
  -- | 'clientId'
  Prelude.Text ->
  -- | 'clientSecret'
  Prelude.Text ->
  MarketoConnectorProfileCredentials
newMarketoConnectorProfileCredentials
  pClientId_
  pClientSecret_ =
    MarketoConnectorProfileCredentials'
      { accessToken =
          Prelude.Nothing,
        oAuthRequest = Prelude.Nothing,
        clientId = pClientId_,
        clientSecret =
          Data._Sensitive Lens.# pClientSecret_
      }

-- | The credentials used to access protected Marketo resources.
marketoConnectorProfileCredentials_accessToken :: Lens.Lens' MarketoConnectorProfileCredentials (Prelude.Maybe Prelude.Text)
marketoConnectorProfileCredentials_accessToken = Lens.lens (\MarketoConnectorProfileCredentials' {accessToken} -> accessToken) (\s@MarketoConnectorProfileCredentials' {} a -> s {accessToken = a} :: MarketoConnectorProfileCredentials) Prelude.. Lens.mapping Data._Sensitive

-- | The OAuth requirement needed to request security tokens from the
-- connector endpoint.
marketoConnectorProfileCredentials_oAuthRequest :: Lens.Lens' MarketoConnectorProfileCredentials (Prelude.Maybe ConnectorOAuthRequest)
marketoConnectorProfileCredentials_oAuthRequest = Lens.lens (\MarketoConnectorProfileCredentials' {oAuthRequest} -> oAuthRequest) (\s@MarketoConnectorProfileCredentials' {} a -> s {oAuthRequest = a} :: MarketoConnectorProfileCredentials)

-- | The identifier for the desired client.
marketoConnectorProfileCredentials_clientId :: Lens.Lens' MarketoConnectorProfileCredentials Prelude.Text
marketoConnectorProfileCredentials_clientId = Lens.lens (\MarketoConnectorProfileCredentials' {clientId} -> clientId) (\s@MarketoConnectorProfileCredentials' {} a -> s {clientId = a} :: MarketoConnectorProfileCredentials)

-- | The client secret used by the OAuth client to authenticate to the
-- authorization server.
marketoConnectorProfileCredentials_clientSecret :: Lens.Lens' MarketoConnectorProfileCredentials Prelude.Text
marketoConnectorProfileCredentials_clientSecret = Lens.lens (\MarketoConnectorProfileCredentials' {clientSecret} -> clientSecret) (\s@MarketoConnectorProfileCredentials' {} a -> s {clientSecret = a} :: MarketoConnectorProfileCredentials) Prelude.. Data._Sensitive

instance
  Prelude.Hashable
    MarketoConnectorProfileCredentials
  where
  hashWithSalt
    _salt
    MarketoConnectorProfileCredentials' {..} =
      _salt
        `Prelude.hashWithSalt` accessToken
        `Prelude.hashWithSalt` oAuthRequest
        `Prelude.hashWithSalt` clientId
        `Prelude.hashWithSalt` clientSecret

instance
  Prelude.NFData
    MarketoConnectorProfileCredentials
  where
  rnf MarketoConnectorProfileCredentials' {..} =
    Prelude.rnf accessToken `Prelude.seq`
      Prelude.rnf oAuthRequest `Prelude.seq`
        Prelude.rnf clientId `Prelude.seq`
          Prelude.rnf clientSecret

instance
  Data.ToJSON
    MarketoConnectorProfileCredentials
  where
  toJSON MarketoConnectorProfileCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accessToken" Data..=) Prelude.<$> accessToken,
            ("oAuthRequest" Data..=) Prelude.<$> oAuthRequest,
            Prelude.Just ("clientId" Data..= clientId),
            Prelude.Just ("clientSecret" Data..= clientSecret)
          ]
      )
