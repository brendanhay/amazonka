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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.UrlEndpointConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration for the URL endpoint type.
--
-- /See:/ 'newUrlEndpointConfig' smart constructor.
data UrlEndpointConfig = UrlEndpointConfig'
  { -- | The HTTP URL endpoint.
    url :: Prelude.Maybe Prelude.Text,
    -- | The health check URL of the URL endpoint type.
    healthUrl :: Prelude.Maybe Prelude.Text
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
-- 'url', 'urlEndpointConfig_url' - The HTTP URL endpoint.
--
-- 'healthUrl', 'urlEndpointConfig_healthUrl' - The health check URL of the URL endpoint type.
newUrlEndpointConfig ::
  UrlEndpointConfig
newUrlEndpointConfig =
  UrlEndpointConfig'
    { url = Prelude.Nothing,
      healthUrl = Prelude.Nothing
    }

-- | The HTTP URL endpoint.
urlEndpointConfig_url :: Lens.Lens' UrlEndpointConfig (Prelude.Maybe Prelude.Text)
urlEndpointConfig_url = Lens.lens (\UrlEndpointConfig' {url} -> url) (\s@UrlEndpointConfig' {} a -> s {url = a} :: UrlEndpointConfig)

-- | The health check URL of the URL endpoint type.
urlEndpointConfig_healthUrl :: Lens.Lens' UrlEndpointConfig (Prelude.Maybe Prelude.Text)
urlEndpointConfig_healthUrl = Lens.lens (\UrlEndpointConfig' {healthUrl} -> healthUrl) (\s@UrlEndpointConfig' {} a -> s {healthUrl = a} :: UrlEndpointConfig)

instance Core.FromJSON UrlEndpointConfig where
  parseJSON =
    Core.withObject
      "UrlEndpointConfig"
      ( \x ->
          UrlEndpointConfig'
            Prelude.<$> (x Core..:? "Url")
            Prelude.<*> (x Core..:? "HealthUrl")
      )

instance Prelude.Hashable UrlEndpointConfig where
  hashWithSalt _salt UrlEndpointConfig' {..} =
    _salt `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` healthUrl

instance Prelude.NFData UrlEndpointConfig where
  rnf UrlEndpointConfig' {..} =
    Prelude.rnf url `Prelude.seq` Prelude.rnf healthUrl
