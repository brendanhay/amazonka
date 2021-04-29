{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudWatchEvents.Types.UpdateConnectionOAuthRequestParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.UpdateConnectionOAuthRequestParameters where

import Network.AWS.CloudWatchEvents.Types.ConnectionHttpParameters
import Network.AWS.CloudWatchEvents.Types.ConnectionOAuthHttpMethod
import Network.AWS.CloudWatchEvents.Types.UpdateConnectionOAuthClientRequestParameters
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the OAuth request parameters to use for the connection.
--
-- /See:/ 'newUpdateConnectionOAuthRequestParameters' smart constructor.
data UpdateConnectionOAuthRequestParameters = UpdateConnectionOAuthRequestParameters'
  { -- | The method used to connect to the HTTP endpoint.
    httpMethod :: Prelude.Maybe ConnectionOAuthHttpMethod,
    -- | A @UpdateConnectionOAuthClientRequestParameters@ object that contains
    -- the client parameters to use for the connection when OAuth is specified
    -- as the authorization type.
    clientParameters :: Prelude.Maybe UpdateConnectionOAuthClientRequestParameters,
    -- | The URL to the authorization endpoint when OAuth is specified as the
    -- authorization type.
    authorizationEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The additional HTTP parameters used for the OAuth authorization request.
    oAuthHttpParameters :: Prelude.Maybe ConnectionHttpParameters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectionOAuthRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpMethod', 'updateConnectionOAuthRequestParameters_httpMethod' - The method used to connect to the HTTP endpoint.
--
-- 'clientParameters', 'updateConnectionOAuthRequestParameters_clientParameters' - A @UpdateConnectionOAuthClientRequestParameters@ object that contains
-- the client parameters to use for the connection when OAuth is specified
-- as the authorization type.
--
-- 'authorizationEndpoint', 'updateConnectionOAuthRequestParameters_authorizationEndpoint' - The URL to the authorization endpoint when OAuth is specified as the
-- authorization type.
--
-- 'oAuthHttpParameters', 'updateConnectionOAuthRequestParameters_oAuthHttpParameters' - The additional HTTP parameters used for the OAuth authorization request.
newUpdateConnectionOAuthRequestParameters ::
  UpdateConnectionOAuthRequestParameters
newUpdateConnectionOAuthRequestParameters =
  UpdateConnectionOAuthRequestParameters'
    { httpMethod =
        Prelude.Nothing,
      clientParameters = Prelude.Nothing,
      authorizationEndpoint =
        Prelude.Nothing,
      oAuthHttpParameters =
        Prelude.Nothing
    }

-- | The method used to connect to the HTTP endpoint.
updateConnectionOAuthRequestParameters_httpMethod :: Lens.Lens' UpdateConnectionOAuthRequestParameters (Prelude.Maybe ConnectionOAuthHttpMethod)
updateConnectionOAuthRequestParameters_httpMethod = Lens.lens (\UpdateConnectionOAuthRequestParameters' {httpMethod} -> httpMethod) (\s@UpdateConnectionOAuthRequestParameters' {} a -> s {httpMethod = a} :: UpdateConnectionOAuthRequestParameters)

-- | A @UpdateConnectionOAuthClientRequestParameters@ object that contains
-- the client parameters to use for the connection when OAuth is specified
-- as the authorization type.
updateConnectionOAuthRequestParameters_clientParameters :: Lens.Lens' UpdateConnectionOAuthRequestParameters (Prelude.Maybe UpdateConnectionOAuthClientRequestParameters)
updateConnectionOAuthRequestParameters_clientParameters = Lens.lens (\UpdateConnectionOAuthRequestParameters' {clientParameters} -> clientParameters) (\s@UpdateConnectionOAuthRequestParameters' {} a -> s {clientParameters = a} :: UpdateConnectionOAuthRequestParameters)

-- | The URL to the authorization endpoint when OAuth is specified as the
-- authorization type.
updateConnectionOAuthRequestParameters_authorizationEndpoint :: Lens.Lens' UpdateConnectionOAuthRequestParameters (Prelude.Maybe Prelude.Text)
updateConnectionOAuthRequestParameters_authorizationEndpoint = Lens.lens (\UpdateConnectionOAuthRequestParameters' {authorizationEndpoint} -> authorizationEndpoint) (\s@UpdateConnectionOAuthRequestParameters' {} a -> s {authorizationEndpoint = a} :: UpdateConnectionOAuthRequestParameters)

-- | The additional HTTP parameters used for the OAuth authorization request.
updateConnectionOAuthRequestParameters_oAuthHttpParameters :: Lens.Lens' UpdateConnectionOAuthRequestParameters (Prelude.Maybe ConnectionHttpParameters)
updateConnectionOAuthRequestParameters_oAuthHttpParameters = Lens.lens (\UpdateConnectionOAuthRequestParameters' {oAuthHttpParameters} -> oAuthHttpParameters) (\s@UpdateConnectionOAuthRequestParameters' {} a -> s {oAuthHttpParameters = a} :: UpdateConnectionOAuthRequestParameters)

instance
  Prelude.Hashable
    UpdateConnectionOAuthRequestParameters

instance
  Prelude.NFData
    UpdateConnectionOAuthRequestParameters

instance
  Prelude.ToJSON
    UpdateConnectionOAuthRequestParameters
  where
  toJSON UpdateConnectionOAuthRequestParameters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("HttpMethod" Prelude..=) Prelude.<$> httpMethod,
            ("ClientParameters" Prelude..=)
              Prelude.<$> clientParameters,
            ("AuthorizationEndpoint" Prelude..=)
              Prelude.<$> authorizationEndpoint,
            ("OAuthHttpParameters" Prelude..=)
              Prelude.<$> oAuthHttpParameters
          ]
      )
