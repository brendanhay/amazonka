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
-- Module      : Network.AWS.AppSync.Types.HttpDataSourceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.HttpDataSourceConfig where

import Network.AWS.AppSync.Types.AuthorizationConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an HTTP data source configuration.
--
-- /See:/ 'newHttpDataSourceConfig' smart constructor.
data HttpDataSourceConfig = HttpDataSourceConfig'
  { -- | The authorization config in case the HTTP endpoint requires
    -- authorization.
    authorizationConfig :: Prelude.Maybe AuthorizationConfig,
    -- | The HTTP URL endpoint. You can either specify the domain name or IP, and
    -- port combination, and the URL scheme must be HTTP or HTTPS. If the port
    -- is not specified, AWS AppSync uses the default port 80 for the HTTP
    -- endpoint and port 443 for HTTPS endpoints.
    endpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HttpDataSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizationConfig', 'httpDataSourceConfig_authorizationConfig' - The authorization config in case the HTTP endpoint requires
-- authorization.
--
-- 'endpoint', 'httpDataSourceConfig_endpoint' - The HTTP URL endpoint. You can either specify the domain name or IP, and
-- port combination, and the URL scheme must be HTTP or HTTPS. If the port
-- is not specified, AWS AppSync uses the default port 80 for the HTTP
-- endpoint and port 443 for HTTPS endpoints.
newHttpDataSourceConfig ::
  HttpDataSourceConfig
newHttpDataSourceConfig =
  HttpDataSourceConfig'
    { authorizationConfig =
        Prelude.Nothing,
      endpoint = Prelude.Nothing
    }

-- | The authorization config in case the HTTP endpoint requires
-- authorization.
httpDataSourceConfig_authorizationConfig :: Lens.Lens' HttpDataSourceConfig (Prelude.Maybe AuthorizationConfig)
httpDataSourceConfig_authorizationConfig = Lens.lens (\HttpDataSourceConfig' {authorizationConfig} -> authorizationConfig) (\s@HttpDataSourceConfig' {} a -> s {authorizationConfig = a} :: HttpDataSourceConfig)

-- | The HTTP URL endpoint. You can either specify the domain name or IP, and
-- port combination, and the URL scheme must be HTTP or HTTPS. If the port
-- is not specified, AWS AppSync uses the default port 80 for the HTTP
-- endpoint and port 443 for HTTPS endpoints.
httpDataSourceConfig_endpoint :: Lens.Lens' HttpDataSourceConfig (Prelude.Maybe Prelude.Text)
httpDataSourceConfig_endpoint = Lens.lens (\HttpDataSourceConfig' {endpoint} -> endpoint) (\s@HttpDataSourceConfig' {} a -> s {endpoint = a} :: HttpDataSourceConfig)

instance Prelude.FromJSON HttpDataSourceConfig where
  parseJSON =
    Prelude.withObject
      "HttpDataSourceConfig"
      ( \x ->
          HttpDataSourceConfig'
            Prelude.<$> (x Prelude..:? "authorizationConfig")
            Prelude.<*> (x Prelude..:? "endpoint")
      )

instance Prelude.Hashable HttpDataSourceConfig

instance Prelude.NFData HttpDataSourceConfig

instance Prelude.ToJSON HttpDataSourceConfig where
  toJSON HttpDataSourceConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("authorizationConfig" Prelude..=)
              Prelude.<$> authorizationConfig,
            ("endpoint" Prelude..=) Prelude.<$> endpoint
          ]
      )
