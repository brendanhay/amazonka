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
-- Module      : Amazonka.Wisdom.Types.SourceConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.SourceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Wisdom.Types.AppIntegrationsConfiguration

-- | Configuration information about the external data source.
--
-- /See:/ 'newSourceConfiguration' smart constructor.
data SourceConfiguration = SourceConfiguration'
  { -- | Configuration information for Amazon AppIntegrations to automatically
    -- ingest content.
    appIntegrations :: Prelude.Maybe AppIntegrationsConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appIntegrations', 'sourceConfiguration_appIntegrations' - Configuration information for Amazon AppIntegrations to automatically
-- ingest content.
newSourceConfiguration ::
  SourceConfiguration
newSourceConfiguration =
  SourceConfiguration'
    { appIntegrations =
        Prelude.Nothing
    }

-- | Configuration information for Amazon AppIntegrations to automatically
-- ingest content.
sourceConfiguration_appIntegrations :: Lens.Lens' SourceConfiguration (Prelude.Maybe AppIntegrationsConfiguration)
sourceConfiguration_appIntegrations = Lens.lens (\SourceConfiguration' {appIntegrations} -> appIntegrations) (\s@SourceConfiguration' {} a -> s {appIntegrations = a} :: SourceConfiguration)

instance Core.FromJSON SourceConfiguration where
  parseJSON =
    Core.withObject
      "SourceConfiguration"
      ( \x ->
          SourceConfiguration'
            Prelude.<$> (x Core..:? "appIntegrations")
      )

instance Prelude.Hashable SourceConfiguration where
  hashWithSalt _salt SourceConfiguration' {..} =
    _salt `Prelude.hashWithSalt` appIntegrations

instance Prelude.NFData SourceConfiguration where
  rnf SourceConfiguration' {..} =
    Prelude.rnf appIntegrations

instance Core.ToJSON SourceConfiguration where
  toJSON SourceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("appIntegrations" Core..=)
              Prelude.<$> appIntegrations
          ]
      )
