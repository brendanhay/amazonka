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
-- Module      : Amazonka.CloudWatchEvents.Types.UpdateConnectionOAuthRequestParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.UpdateConnectionOAuthRequestParameters where

import Amazonka.CloudWatchEvents.Types.ConnectionHttpParameters
import Amazonka.CloudWatchEvents.Types.ConnectionOAuthHttpMethod
import Amazonka.CloudWatchEvents.Types.UpdateConnectionOAuthClientRequestParameters
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the OAuth request parameters to use for the connection.
--
-- /See:/ 'newUpdateConnectionOAuthRequestParameters' smart constructor.
data UpdateConnectionOAuthRequestParameters = UpdateConnectionOAuthRequestParameters'
  { -- | The URL to the authorization endpoint when OAuth is specified as the
    -- authorization type.
    authorizationEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The additional HTTP parameters used for the OAuth authorization request.
    oAuthHttpParameters :: Prelude.Maybe ConnectionHttpParameters,
    -- | A @UpdateConnectionOAuthClientRequestParameters@ object that contains
    -- the client parameters to use for the connection when OAuth is specified
    -- as the authorization type.
    clientParameters :: Prelude.Maybe UpdateConnectionOAuthClientRequestParameters,
    -- | The method used to connect to the HTTP endpoint.
    httpMethod :: Prelude.Maybe ConnectionOAuthHttpMethod
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectionOAuthRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizationEndpoint', 'updateConnectionOAuthRequestParameters_authorizationEndpoint' - The URL to the authorization endpoint when OAuth is specified as the
-- authorization type.
--
-- 'oAuthHttpParameters', 'updateConnectionOAuthRequestParameters_oAuthHttpParameters' - The additional HTTP parameters used for the OAuth authorization request.
--
-- 'clientParameters', 'updateConnectionOAuthRequestParameters_clientParameters' - A @UpdateConnectionOAuthClientRequestParameters@ object that contains
-- the client parameters to use for the connection when OAuth is specified
-- as the authorization type.
--
-- 'httpMethod', 'updateConnectionOAuthRequestParameters_httpMethod' - The method used to connect to the HTTP endpoint.
newUpdateConnectionOAuthRequestParameters ::
  UpdateConnectionOAuthRequestParameters
newUpdateConnectionOAuthRequestParameters =
  UpdateConnectionOAuthRequestParameters'
    { authorizationEndpoint =
        Prelude.Nothing,
      oAuthHttpParameters =
        Prelude.Nothing,
      clientParameters = Prelude.Nothing,
      httpMethod = Prelude.Nothing
    }

-- | The URL to the authorization endpoint when OAuth is specified as the
-- authorization type.
updateConnectionOAuthRequestParameters_authorizationEndpoint :: Lens.Lens' UpdateConnectionOAuthRequestParameters (Prelude.Maybe Prelude.Text)
updateConnectionOAuthRequestParameters_authorizationEndpoint = Lens.lens (\UpdateConnectionOAuthRequestParameters' {authorizationEndpoint} -> authorizationEndpoint) (\s@UpdateConnectionOAuthRequestParameters' {} a -> s {authorizationEndpoint = a} :: UpdateConnectionOAuthRequestParameters)

-- | The additional HTTP parameters used for the OAuth authorization request.
updateConnectionOAuthRequestParameters_oAuthHttpParameters :: Lens.Lens' UpdateConnectionOAuthRequestParameters (Prelude.Maybe ConnectionHttpParameters)
updateConnectionOAuthRequestParameters_oAuthHttpParameters = Lens.lens (\UpdateConnectionOAuthRequestParameters' {oAuthHttpParameters} -> oAuthHttpParameters) (\s@UpdateConnectionOAuthRequestParameters' {} a -> s {oAuthHttpParameters = a} :: UpdateConnectionOAuthRequestParameters)

-- | A @UpdateConnectionOAuthClientRequestParameters@ object that contains
-- the client parameters to use for the connection when OAuth is specified
-- as the authorization type.
updateConnectionOAuthRequestParameters_clientParameters :: Lens.Lens' UpdateConnectionOAuthRequestParameters (Prelude.Maybe UpdateConnectionOAuthClientRequestParameters)
updateConnectionOAuthRequestParameters_clientParameters = Lens.lens (\UpdateConnectionOAuthRequestParameters' {clientParameters} -> clientParameters) (\s@UpdateConnectionOAuthRequestParameters' {} a -> s {clientParameters = a} :: UpdateConnectionOAuthRequestParameters)

-- | The method used to connect to the HTTP endpoint.
updateConnectionOAuthRequestParameters_httpMethod :: Lens.Lens' UpdateConnectionOAuthRequestParameters (Prelude.Maybe ConnectionOAuthHttpMethod)
updateConnectionOAuthRequestParameters_httpMethod = Lens.lens (\UpdateConnectionOAuthRequestParameters' {httpMethod} -> httpMethod) (\s@UpdateConnectionOAuthRequestParameters' {} a -> s {httpMethod = a} :: UpdateConnectionOAuthRequestParameters)

instance
  Prelude.Hashable
    UpdateConnectionOAuthRequestParameters
  where
  hashWithSalt
    _salt
    UpdateConnectionOAuthRequestParameters' {..} =
      _salt `Prelude.hashWithSalt` authorizationEndpoint
        `Prelude.hashWithSalt` oAuthHttpParameters
        `Prelude.hashWithSalt` clientParameters
        `Prelude.hashWithSalt` httpMethod

instance
  Prelude.NFData
    UpdateConnectionOAuthRequestParameters
  where
  rnf UpdateConnectionOAuthRequestParameters' {..} =
    Prelude.rnf authorizationEndpoint
      `Prelude.seq` Prelude.rnf oAuthHttpParameters
      `Prelude.seq` Prelude.rnf clientParameters
      `Prelude.seq` Prelude.rnf httpMethod

instance
  Core.ToJSON
    UpdateConnectionOAuthRequestParameters
  where
  toJSON UpdateConnectionOAuthRequestParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AuthorizationEndpoint" Core..=)
              Prelude.<$> authorizationEndpoint,
            ("OAuthHttpParameters" Core..=)
              Prelude.<$> oAuthHttpParameters,
            ("ClientParameters" Core..=)
              Prelude.<$> clientParameters,
            ("HttpMethod" Core..=) Prelude.<$> httpMethod
          ]
      )
