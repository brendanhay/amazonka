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
-- Module      : Network.AWS.CloudWatchEvents.Types.CreateConnectionOAuthRequestParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.CreateConnectionOAuthRequestParameters where

import Network.AWS.CloudWatchEvents.Types.ConnectionHttpParameters
import Network.AWS.CloudWatchEvents.Types.ConnectionOAuthHttpMethod
import Network.AWS.CloudWatchEvents.Types.CreateConnectionOAuthClientRequestParameters
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the OAuth authorization parameters to use for the connection.
--
-- /See:/ 'newCreateConnectionOAuthRequestParameters' smart constructor.
data CreateConnectionOAuthRequestParameters = CreateConnectionOAuthRequestParameters'
  { -- | A @ConnectionHttpParameters@ object that contains details about the
    -- additional parameters to use for the connection.
    oAuthHttpParameters :: Core.Maybe ConnectionHttpParameters,
    -- | A @CreateConnectionOAuthClientRequestParameters@ object that contains
    -- the client parameters for OAuth authorization.
    clientParameters :: CreateConnectionOAuthClientRequestParameters,
    -- | The URL to the authorization endpoint when OAuth is specified as the
    -- authorization type.
    authorizationEndpoint :: Core.Text,
    -- | The method to use for the authorization request.
    httpMethod :: ConnectionOAuthHttpMethod
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'httpMethod'
  ConnectionOAuthHttpMethod ->
  CreateConnectionOAuthRequestParameters
newCreateConnectionOAuthRequestParameters
  pClientParameters_
  pAuthorizationEndpoint_
  pHttpMethod_ =
    CreateConnectionOAuthRequestParameters'
      { oAuthHttpParameters =
          Core.Nothing,
        clientParameters =
          pClientParameters_,
        authorizationEndpoint =
          pAuthorizationEndpoint_,
        httpMethod = pHttpMethod_
      }

-- | A @ConnectionHttpParameters@ object that contains details about the
-- additional parameters to use for the connection.
createConnectionOAuthRequestParameters_oAuthHttpParameters :: Lens.Lens' CreateConnectionOAuthRequestParameters (Core.Maybe ConnectionHttpParameters)
createConnectionOAuthRequestParameters_oAuthHttpParameters = Lens.lens (\CreateConnectionOAuthRequestParameters' {oAuthHttpParameters} -> oAuthHttpParameters) (\s@CreateConnectionOAuthRequestParameters' {} a -> s {oAuthHttpParameters = a} :: CreateConnectionOAuthRequestParameters)

-- | A @CreateConnectionOAuthClientRequestParameters@ object that contains
-- the client parameters for OAuth authorization.
createConnectionOAuthRequestParameters_clientParameters :: Lens.Lens' CreateConnectionOAuthRequestParameters CreateConnectionOAuthClientRequestParameters
createConnectionOAuthRequestParameters_clientParameters = Lens.lens (\CreateConnectionOAuthRequestParameters' {clientParameters} -> clientParameters) (\s@CreateConnectionOAuthRequestParameters' {} a -> s {clientParameters = a} :: CreateConnectionOAuthRequestParameters)

-- | The URL to the authorization endpoint when OAuth is specified as the
-- authorization type.
createConnectionOAuthRequestParameters_authorizationEndpoint :: Lens.Lens' CreateConnectionOAuthRequestParameters Core.Text
createConnectionOAuthRequestParameters_authorizationEndpoint = Lens.lens (\CreateConnectionOAuthRequestParameters' {authorizationEndpoint} -> authorizationEndpoint) (\s@CreateConnectionOAuthRequestParameters' {} a -> s {authorizationEndpoint = a} :: CreateConnectionOAuthRequestParameters)

-- | The method to use for the authorization request.
createConnectionOAuthRequestParameters_httpMethod :: Lens.Lens' CreateConnectionOAuthRequestParameters ConnectionOAuthHttpMethod
createConnectionOAuthRequestParameters_httpMethod = Lens.lens (\CreateConnectionOAuthRequestParameters' {httpMethod} -> httpMethod) (\s@CreateConnectionOAuthRequestParameters' {} a -> s {httpMethod = a} :: CreateConnectionOAuthRequestParameters)

instance
  Core.Hashable
    CreateConnectionOAuthRequestParameters

instance
  Core.NFData
    CreateConnectionOAuthRequestParameters

instance
  Core.ToJSON
    CreateConnectionOAuthRequestParameters
  where
  toJSON CreateConnectionOAuthRequestParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("OAuthHttpParameters" Core..=)
              Core.<$> oAuthHttpParameters,
            Core.Just
              ("ClientParameters" Core..= clientParameters),
            Core.Just
              ( "AuthorizationEndpoint"
                  Core..= authorizationEndpoint
              ),
            Core.Just ("HttpMethod" Core..= httpMethod)
          ]
      )
