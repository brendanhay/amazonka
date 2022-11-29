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
-- Module      : Amazonka.AppFlow.Types.ZendeskConnectorProfileCredentials
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ZendeskConnectorProfileCredentials where

import Amazonka.AppFlow.Types.ConnectorOAuthRequest
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile credentials required when using Zendesk.
--
-- /See:/ 'newZendeskConnectorProfileCredentials' smart constructor.
data ZendeskConnectorProfileCredentials = ZendeskConnectorProfileCredentials'
  { -- | The credentials used to access protected Zendesk resources.
    accessToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The OAuth requirement needed to request security tokens from the
    -- connector endpoint.
    oAuthRequest :: Prelude.Maybe ConnectorOAuthRequest,
    -- | The identifier for the desired client.
    clientId :: Prelude.Text,
    -- | The client secret used by the OAuth client to authenticate to the
    -- authorization server.
    clientSecret :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ZendeskConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'zendeskConnectorProfileCredentials_accessToken' - The credentials used to access protected Zendesk resources.
--
-- 'oAuthRequest', 'zendeskConnectorProfileCredentials_oAuthRequest' - The OAuth requirement needed to request security tokens from the
-- connector endpoint.
--
-- 'clientId', 'zendeskConnectorProfileCredentials_clientId' - The identifier for the desired client.
--
-- 'clientSecret', 'zendeskConnectorProfileCredentials_clientSecret' - The client secret used by the OAuth client to authenticate to the
-- authorization server.
newZendeskConnectorProfileCredentials ::
  -- | 'clientId'
  Prelude.Text ->
  -- | 'clientSecret'
  Prelude.Text ->
  ZendeskConnectorProfileCredentials
newZendeskConnectorProfileCredentials
  pClientId_
  pClientSecret_ =
    ZendeskConnectorProfileCredentials'
      { accessToken =
          Prelude.Nothing,
        oAuthRequest = Prelude.Nothing,
        clientId = pClientId_,
        clientSecret =
          Core._Sensitive Lens.# pClientSecret_
      }

-- | The credentials used to access protected Zendesk resources.
zendeskConnectorProfileCredentials_accessToken :: Lens.Lens' ZendeskConnectorProfileCredentials (Prelude.Maybe Prelude.Text)
zendeskConnectorProfileCredentials_accessToken = Lens.lens (\ZendeskConnectorProfileCredentials' {accessToken} -> accessToken) (\s@ZendeskConnectorProfileCredentials' {} a -> s {accessToken = a} :: ZendeskConnectorProfileCredentials) Prelude.. Lens.mapping Core._Sensitive

-- | The OAuth requirement needed to request security tokens from the
-- connector endpoint.
zendeskConnectorProfileCredentials_oAuthRequest :: Lens.Lens' ZendeskConnectorProfileCredentials (Prelude.Maybe ConnectorOAuthRequest)
zendeskConnectorProfileCredentials_oAuthRequest = Lens.lens (\ZendeskConnectorProfileCredentials' {oAuthRequest} -> oAuthRequest) (\s@ZendeskConnectorProfileCredentials' {} a -> s {oAuthRequest = a} :: ZendeskConnectorProfileCredentials)

-- | The identifier for the desired client.
zendeskConnectorProfileCredentials_clientId :: Lens.Lens' ZendeskConnectorProfileCredentials Prelude.Text
zendeskConnectorProfileCredentials_clientId = Lens.lens (\ZendeskConnectorProfileCredentials' {clientId} -> clientId) (\s@ZendeskConnectorProfileCredentials' {} a -> s {clientId = a} :: ZendeskConnectorProfileCredentials)

-- | The client secret used by the OAuth client to authenticate to the
-- authorization server.
zendeskConnectorProfileCredentials_clientSecret :: Lens.Lens' ZendeskConnectorProfileCredentials Prelude.Text
zendeskConnectorProfileCredentials_clientSecret = Lens.lens (\ZendeskConnectorProfileCredentials' {clientSecret} -> clientSecret) (\s@ZendeskConnectorProfileCredentials' {} a -> s {clientSecret = a} :: ZendeskConnectorProfileCredentials) Prelude.. Core._Sensitive

instance
  Prelude.Hashable
    ZendeskConnectorProfileCredentials
  where
  hashWithSalt
    _salt
    ZendeskConnectorProfileCredentials' {..} =
      _salt `Prelude.hashWithSalt` accessToken
        `Prelude.hashWithSalt` oAuthRequest
        `Prelude.hashWithSalt` clientId
        `Prelude.hashWithSalt` clientSecret

instance
  Prelude.NFData
    ZendeskConnectorProfileCredentials
  where
  rnf ZendeskConnectorProfileCredentials' {..} =
    Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf oAuthRequest
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf clientSecret

instance
  Core.ToJSON
    ZendeskConnectorProfileCredentials
  where
  toJSON ZendeskConnectorProfileCredentials' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("accessToken" Core..=) Prelude.<$> accessToken,
            ("oAuthRequest" Core..=) Prelude.<$> oAuthRequest,
            Prelude.Just ("clientId" Core..= clientId),
            Prelude.Just ("clientSecret" Core..= clientSecret)
          ]
      )
