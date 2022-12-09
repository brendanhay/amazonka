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
-- Module      : Amazonka.AppSync.Types.HttpDataSourceConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.HttpDataSourceConfig where

import Amazonka.AppSync.Types.AuthorizationConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an HTTP data source configuration.
--
-- /See:/ 'newHttpDataSourceConfig' smart constructor.
data HttpDataSourceConfig = HttpDataSourceConfig'
  { -- | The authorization configuration in case the HTTP endpoint requires
    -- authorization.
    authorizationConfig :: Prelude.Maybe AuthorizationConfig,
    -- | The HTTP URL endpoint. You can specify either the domain name or IP, and
    -- port combination, and the URL scheme must be HTTP or HTTPS. If you
    -- don\'t specify the port, AppSync uses the default port 80 for the HTTP
    -- endpoint and port 443 for HTTPS endpoints.
    endpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpDataSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizationConfig', 'httpDataSourceConfig_authorizationConfig' - The authorization configuration in case the HTTP endpoint requires
-- authorization.
--
-- 'endpoint', 'httpDataSourceConfig_endpoint' - The HTTP URL endpoint. You can specify either the domain name or IP, and
-- port combination, and the URL scheme must be HTTP or HTTPS. If you
-- don\'t specify the port, AppSync uses the default port 80 for the HTTP
-- endpoint and port 443 for HTTPS endpoints.
newHttpDataSourceConfig ::
  HttpDataSourceConfig
newHttpDataSourceConfig =
  HttpDataSourceConfig'
    { authorizationConfig =
        Prelude.Nothing,
      endpoint = Prelude.Nothing
    }

-- | The authorization configuration in case the HTTP endpoint requires
-- authorization.
httpDataSourceConfig_authorizationConfig :: Lens.Lens' HttpDataSourceConfig (Prelude.Maybe AuthorizationConfig)
httpDataSourceConfig_authorizationConfig = Lens.lens (\HttpDataSourceConfig' {authorizationConfig} -> authorizationConfig) (\s@HttpDataSourceConfig' {} a -> s {authorizationConfig = a} :: HttpDataSourceConfig)

-- | The HTTP URL endpoint. You can specify either the domain name or IP, and
-- port combination, and the URL scheme must be HTTP or HTTPS. If you
-- don\'t specify the port, AppSync uses the default port 80 for the HTTP
-- endpoint and port 443 for HTTPS endpoints.
httpDataSourceConfig_endpoint :: Lens.Lens' HttpDataSourceConfig (Prelude.Maybe Prelude.Text)
httpDataSourceConfig_endpoint = Lens.lens (\HttpDataSourceConfig' {endpoint} -> endpoint) (\s@HttpDataSourceConfig' {} a -> s {endpoint = a} :: HttpDataSourceConfig)

instance Data.FromJSON HttpDataSourceConfig where
  parseJSON =
    Data.withObject
      "HttpDataSourceConfig"
      ( \x ->
          HttpDataSourceConfig'
            Prelude.<$> (x Data..:? "authorizationConfig")
            Prelude.<*> (x Data..:? "endpoint")
      )

instance Prelude.Hashable HttpDataSourceConfig where
  hashWithSalt _salt HttpDataSourceConfig' {..} =
    _salt `Prelude.hashWithSalt` authorizationConfig
      `Prelude.hashWithSalt` endpoint

instance Prelude.NFData HttpDataSourceConfig where
  rnf HttpDataSourceConfig' {..} =
    Prelude.rnf authorizationConfig
      `Prelude.seq` Prelude.rnf endpoint

instance Data.ToJSON HttpDataSourceConfig where
  toJSON HttpDataSourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("authorizationConfig" Data..=)
              Prelude.<$> authorizationConfig,
            ("endpoint" Data..=) Prelude.<$> endpoint
          ]
      )
