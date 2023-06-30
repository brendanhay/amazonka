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
-- Module      : Amazonka.CloudWatchEvents.Types.CreateConnectionOAuthRequestParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.CreateConnectionOAuthRequestParameters where

import Amazonka.CloudWatchEvents.Types.ConnectionHttpParameters
import Amazonka.CloudWatchEvents.Types.ConnectionOAuthHttpMethod
import Amazonka.CloudWatchEvents.Types.CreateConnectionOAuthClientRequestParameters
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the OAuth authorization parameters to use for the connection.
--
-- /See:/ 'newCreateConnectionOAuthRequestParameters' smart constructor.
data CreateConnectionOAuthRequestParameters = CreateConnectionOAuthRequestParameters'
  { -- | A @ConnectionHttpParameters@ object that contains details about the
    -- additional parameters to use for the connection.
    oAuthHttpParameters :: Prelude.Maybe ConnectionHttpParameters,
    -- | A @CreateConnectionOAuthClientRequestParameters@ object that contains
    -- the client parameters for OAuth authorization.
    clientParameters :: CreateConnectionOAuthClientRequestParameters,
    -- | The URL to the authorization endpoint when OAuth is specified as the
    -- authorization type.
    authorizationEndpoint :: Prelude.Text,
    -- | The method to use for the authorization request.
    httpMethod :: ConnectionOAuthHttpMethod
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectionOAuthRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oAuthHttpParameters', 'createConnectionOAuthRequestParameters_oAuthHttpParameters' - A @ConnectionHttpParameters@ object that contains details about the
-- additional parameters to use for the connection.
--
-- 'clientParameters', 'createConnectionOAuthRequestParameters_clientParameters' - A @CreateConnectionOAuthClientRequestParameters@ object that contains
-- the client parameters for OAuth authorization.
--
-- 'authorizationEndpoint', 'createConnectionOAuthRequestParameters_authorizationEndpoint' - The URL to the authorization endpoint when OAuth is specified as the
-- authorization type.
--
-- 'httpMethod', 'createConnectionOAuthRequestParameters_httpMethod' - The method to use for the authorization request.
newCreateConnectionOAuthRequestParameters ::
  -- | 'clientParameters'
  CreateConnectionOAuthClientRequestParameters ->
  -- | 'authorizationEndpoint'
  Prelude.Text ->
  -- | 'httpMethod'
  ConnectionOAuthHttpMethod ->
  CreateConnectionOAuthRequestParameters
newCreateConnectionOAuthRequestParameters
  pClientParameters_
  pAuthorizationEndpoint_
  pHttpMethod_ =
    CreateConnectionOAuthRequestParameters'
      { oAuthHttpParameters =
          Prelude.Nothing,
        clientParameters =
          pClientParameters_,
        authorizationEndpoint =
          pAuthorizationEndpoint_,
        httpMethod = pHttpMethod_
      }

-- | A @ConnectionHttpParameters@ object that contains details about the
-- additional parameters to use for the connection.
createConnectionOAuthRequestParameters_oAuthHttpParameters :: Lens.Lens' CreateConnectionOAuthRequestParameters (Prelude.Maybe ConnectionHttpParameters)
createConnectionOAuthRequestParameters_oAuthHttpParameters = Lens.lens (\CreateConnectionOAuthRequestParameters' {oAuthHttpParameters} -> oAuthHttpParameters) (\s@CreateConnectionOAuthRequestParameters' {} a -> s {oAuthHttpParameters = a} :: CreateConnectionOAuthRequestParameters)

-- | A @CreateConnectionOAuthClientRequestParameters@ object that contains
-- the client parameters for OAuth authorization.
createConnectionOAuthRequestParameters_clientParameters :: Lens.Lens' CreateConnectionOAuthRequestParameters CreateConnectionOAuthClientRequestParameters
createConnectionOAuthRequestParameters_clientParameters = Lens.lens (\CreateConnectionOAuthRequestParameters' {clientParameters} -> clientParameters) (\s@CreateConnectionOAuthRequestParameters' {} a -> s {clientParameters = a} :: CreateConnectionOAuthRequestParameters)

-- | The URL to the authorization endpoint when OAuth is specified as the
-- authorization type.
createConnectionOAuthRequestParameters_authorizationEndpoint :: Lens.Lens' CreateConnectionOAuthRequestParameters Prelude.Text
createConnectionOAuthRequestParameters_authorizationEndpoint = Lens.lens (\CreateConnectionOAuthRequestParameters' {authorizationEndpoint} -> authorizationEndpoint) (\s@CreateConnectionOAuthRequestParameters' {} a -> s {authorizationEndpoint = a} :: CreateConnectionOAuthRequestParameters)

-- | The method to use for the authorization request.
createConnectionOAuthRequestParameters_httpMethod :: Lens.Lens' CreateConnectionOAuthRequestParameters ConnectionOAuthHttpMethod
createConnectionOAuthRequestParameters_httpMethod = Lens.lens (\CreateConnectionOAuthRequestParameters' {httpMethod} -> httpMethod) (\s@CreateConnectionOAuthRequestParameters' {} a -> s {httpMethod = a} :: CreateConnectionOAuthRequestParameters)

instance
  Prelude.Hashable
    CreateConnectionOAuthRequestParameters
  where
  hashWithSalt
    _salt
    CreateConnectionOAuthRequestParameters' {..} =
      _salt
        `Prelude.hashWithSalt` oAuthHttpParameters
        `Prelude.hashWithSalt` clientParameters
        `Prelude.hashWithSalt` authorizationEndpoint
        `Prelude.hashWithSalt` httpMethod

instance
  Prelude.NFData
    CreateConnectionOAuthRequestParameters
  where
  rnf CreateConnectionOAuthRequestParameters' {..} =
    Prelude.rnf oAuthHttpParameters
      `Prelude.seq` Prelude.rnf clientParameters
      `Prelude.seq` Prelude.rnf authorizationEndpoint
      `Prelude.seq` Prelude.rnf httpMethod

instance
  Data.ToJSON
    CreateConnectionOAuthRequestParameters
  where
  toJSON CreateConnectionOAuthRequestParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OAuthHttpParameters" Data..=)
              Prelude.<$> oAuthHttpParameters,
            Prelude.Just
              ("ClientParameters" Data..= clientParameters),
            Prelude.Just
              ( "AuthorizationEndpoint"
                  Data..= authorizationEndpoint
              ),
            Prelude.Just ("HttpMethod" Data..= httpMethod)
          ]
      )
