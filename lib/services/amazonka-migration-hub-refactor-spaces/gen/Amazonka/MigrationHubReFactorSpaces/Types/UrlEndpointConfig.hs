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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.UrlEndpointConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.UrlEndpointConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for the URL endpoint type.
--
-- /See:/ 'newUrlEndpointConfig' smart constructor.
data UrlEndpointConfig = UrlEndpointConfig'
  { -- | The health check URL of the URL endpoint type.
    healthUrl :: Prelude.Maybe Prelude.Text,
    -- | The HTTP URL endpoint.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UrlEndpointConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthUrl', 'urlEndpointConfig_healthUrl' - The health check URL of the URL endpoint type.
--
-- 'url', 'urlEndpointConfig_url' - The HTTP URL endpoint.
newUrlEndpointConfig ::
  UrlEndpointConfig
newUrlEndpointConfig =
  UrlEndpointConfig'
    { healthUrl = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The health check URL of the URL endpoint type.
urlEndpointConfig_healthUrl :: Lens.Lens' UrlEndpointConfig (Prelude.Maybe Prelude.Text)
urlEndpointConfig_healthUrl = Lens.lens (\UrlEndpointConfig' {healthUrl} -> healthUrl) (\s@UrlEndpointConfig' {} a -> s {healthUrl = a} :: UrlEndpointConfig)

-- | The HTTP URL endpoint.
urlEndpointConfig_url :: Lens.Lens' UrlEndpointConfig (Prelude.Maybe Prelude.Text)
urlEndpointConfig_url = Lens.lens (\UrlEndpointConfig' {url} -> url) (\s@UrlEndpointConfig' {} a -> s {url = a} :: UrlEndpointConfig)

instance Data.FromJSON UrlEndpointConfig where
  parseJSON =
    Data.withObject
      "UrlEndpointConfig"
      ( \x ->
          UrlEndpointConfig'
            Prelude.<$> (x Data..:? "HealthUrl")
            Prelude.<*> (x Data..:? "Url")
      )

instance Prelude.Hashable UrlEndpointConfig where
  hashWithSalt _salt UrlEndpointConfig' {..} =
    _salt
      `Prelude.hashWithSalt` healthUrl
      `Prelude.hashWithSalt` url

instance Prelude.NFData UrlEndpointConfig where
  rnf UrlEndpointConfig' {..} =
    Prelude.rnf healthUrl `Prelude.seq` Prelude.rnf url
