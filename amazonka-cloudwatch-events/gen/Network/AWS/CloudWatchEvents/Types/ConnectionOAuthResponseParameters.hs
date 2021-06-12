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
-- Module      : Network.AWS.CloudWatchEvents.Types.ConnectionOAuthResponseParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ConnectionOAuthResponseParameters where

import Network.AWS.CloudWatchEvents.Types.ConnectionHttpParameters
import Network.AWS.CloudWatchEvents.Types.ConnectionOAuthClientResponseParameters
import Network.AWS.CloudWatchEvents.Types.ConnectionOAuthHttpMethod
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the response parameters when OAuth is specified as the
-- authorization type.
--
-- /See:/ 'newConnectionOAuthResponseParameters' smart constructor.
data ConnectionOAuthResponseParameters = ConnectionOAuthResponseParameters'
  { -- | The method used to connect to the HTTP endpoint.
    httpMethod :: Core.Maybe ConnectionOAuthHttpMethod,
    -- | A @ConnectionOAuthClientResponseParameters@ object that contains details
    -- about the client parameters returned when OAuth is specified as the
    -- authorization type.
    clientParameters :: Core.Maybe ConnectionOAuthClientResponseParameters,
    -- | The URL to the HTTP endpoint that authorized the request.
    authorizationEndpoint :: Core.Maybe Core.Text,
    -- | The additional HTTP parameters used for the OAuth authorization request.
    oAuthHttpParameters :: Core.Maybe ConnectionHttpParameters
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConnectionOAuthResponseParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpMethod', 'connectionOAuthResponseParameters_httpMethod' - The method used to connect to the HTTP endpoint.
--
-- 'clientParameters', 'connectionOAuthResponseParameters_clientParameters' - A @ConnectionOAuthClientResponseParameters@ object that contains details
-- about the client parameters returned when OAuth is specified as the
-- authorization type.
--
-- 'authorizationEndpoint', 'connectionOAuthResponseParameters_authorizationEndpoint' - The URL to the HTTP endpoint that authorized the request.
--
-- 'oAuthHttpParameters', 'connectionOAuthResponseParameters_oAuthHttpParameters' - The additional HTTP parameters used for the OAuth authorization request.
newConnectionOAuthResponseParameters ::
  ConnectionOAuthResponseParameters
newConnectionOAuthResponseParameters =
  ConnectionOAuthResponseParameters'
    { httpMethod =
        Core.Nothing,
      clientParameters = Core.Nothing,
      authorizationEndpoint = Core.Nothing,
      oAuthHttpParameters = Core.Nothing
    }

-- | The method used to connect to the HTTP endpoint.
connectionOAuthResponseParameters_httpMethod :: Lens.Lens' ConnectionOAuthResponseParameters (Core.Maybe ConnectionOAuthHttpMethod)
connectionOAuthResponseParameters_httpMethod = Lens.lens (\ConnectionOAuthResponseParameters' {httpMethod} -> httpMethod) (\s@ConnectionOAuthResponseParameters' {} a -> s {httpMethod = a} :: ConnectionOAuthResponseParameters)

-- | A @ConnectionOAuthClientResponseParameters@ object that contains details
-- about the client parameters returned when OAuth is specified as the
-- authorization type.
connectionOAuthResponseParameters_clientParameters :: Lens.Lens' ConnectionOAuthResponseParameters (Core.Maybe ConnectionOAuthClientResponseParameters)
connectionOAuthResponseParameters_clientParameters = Lens.lens (\ConnectionOAuthResponseParameters' {clientParameters} -> clientParameters) (\s@ConnectionOAuthResponseParameters' {} a -> s {clientParameters = a} :: ConnectionOAuthResponseParameters)

-- | The URL to the HTTP endpoint that authorized the request.
connectionOAuthResponseParameters_authorizationEndpoint :: Lens.Lens' ConnectionOAuthResponseParameters (Core.Maybe Core.Text)
connectionOAuthResponseParameters_authorizationEndpoint = Lens.lens (\ConnectionOAuthResponseParameters' {authorizationEndpoint} -> authorizationEndpoint) (\s@ConnectionOAuthResponseParameters' {} a -> s {authorizationEndpoint = a} :: ConnectionOAuthResponseParameters)

-- | The additional HTTP parameters used for the OAuth authorization request.
connectionOAuthResponseParameters_oAuthHttpParameters :: Lens.Lens' ConnectionOAuthResponseParameters (Core.Maybe ConnectionHttpParameters)
connectionOAuthResponseParameters_oAuthHttpParameters = Lens.lens (\ConnectionOAuthResponseParameters' {oAuthHttpParameters} -> oAuthHttpParameters) (\s@ConnectionOAuthResponseParameters' {} a -> s {oAuthHttpParameters = a} :: ConnectionOAuthResponseParameters)

instance
  Core.FromJSON
    ConnectionOAuthResponseParameters
  where
  parseJSON =
    Core.withObject
      "ConnectionOAuthResponseParameters"
      ( \x ->
          ConnectionOAuthResponseParameters'
            Core.<$> (x Core..:? "HttpMethod")
            Core.<*> (x Core..:? "ClientParameters")
            Core.<*> (x Core..:? "AuthorizationEndpoint")
            Core.<*> (x Core..:? "OAuthHttpParameters")
      )

instance
  Core.Hashable
    ConnectionOAuthResponseParameters

instance
  Core.NFData
    ConnectionOAuthResponseParameters
