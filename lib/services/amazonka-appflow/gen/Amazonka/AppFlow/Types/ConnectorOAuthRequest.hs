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
-- Module      : Amazonka.AppFlow.Types.ConnectorOAuthRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ConnectorOAuthRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Used by select connectors for which the OAuth workflow is supported,
-- such as Salesforce, Google Analytics, Marketo, Zendesk, and Slack.
--
-- /See:/ 'newConnectorOAuthRequest' smart constructor.
data ConnectorOAuthRequest = ConnectorOAuthRequest'
  { -- | The code provided by the connector when it has been authenticated via
    -- the connected app.
    authCode :: Prelude.Maybe Prelude.Text,
    -- | The URL to which the authentication server redirects the browser after
    -- authorization has been granted.
    redirectUri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorOAuthRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authCode', 'connectorOAuthRequest_authCode' - The code provided by the connector when it has been authenticated via
-- the connected app.
--
-- 'redirectUri', 'connectorOAuthRequest_redirectUri' - The URL to which the authentication server redirects the browser after
-- authorization has been granted.
newConnectorOAuthRequest ::
  ConnectorOAuthRequest
newConnectorOAuthRequest =
  ConnectorOAuthRequest'
    { authCode = Prelude.Nothing,
      redirectUri = Prelude.Nothing
    }

-- | The code provided by the connector when it has been authenticated via
-- the connected app.
connectorOAuthRequest_authCode :: Lens.Lens' ConnectorOAuthRequest (Prelude.Maybe Prelude.Text)
connectorOAuthRequest_authCode = Lens.lens (\ConnectorOAuthRequest' {authCode} -> authCode) (\s@ConnectorOAuthRequest' {} a -> s {authCode = a} :: ConnectorOAuthRequest)

-- | The URL to which the authentication server redirects the browser after
-- authorization has been granted.
connectorOAuthRequest_redirectUri :: Lens.Lens' ConnectorOAuthRequest (Prelude.Maybe Prelude.Text)
connectorOAuthRequest_redirectUri = Lens.lens (\ConnectorOAuthRequest' {redirectUri} -> redirectUri) (\s@ConnectorOAuthRequest' {} a -> s {redirectUri = a} :: ConnectorOAuthRequest)

instance Prelude.Hashable ConnectorOAuthRequest where
  hashWithSalt _salt ConnectorOAuthRequest' {..} =
    _salt
      `Prelude.hashWithSalt` authCode
      `Prelude.hashWithSalt` redirectUri

instance Prelude.NFData ConnectorOAuthRequest where
  rnf ConnectorOAuthRequest' {..} =
    Prelude.rnf authCode `Prelude.seq`
      Prelude.rnf redirectUri

instance Data.ToJSON ConnectorOAuthRequest where
  toJSON ConnectorOAuthRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("authCode" Data..=) Prelude.<$> authCode,
            ("redirectUri" Data..=) Prelude.<$> redirectUri
          ]
      )
