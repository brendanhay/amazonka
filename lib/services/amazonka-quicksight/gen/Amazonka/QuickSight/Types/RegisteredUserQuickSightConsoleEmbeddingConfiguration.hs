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
-- Module      : Amazonka.QuickSight.Types.RegisteredUserQuickSightConsoleEmbeddingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RegisteredUserQuickSightConsoleEmbeddingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.RegisteredUserConsoleFeatureConfigurations

-- | Information about the Amazon QuickSight console that you want to embed.
--
-- /See:/ 'newRegisteredUserQuickSightConsoleEmbeddingConfiguration' smart constructor.
data RegisteredUserQuickSightConsoleEmbeddingConfiguration = RegisteredUserQuickSightConsoleEmbeddingConfiguration'
  { -- | The embedding configuration of an embedded Amazon QuickSight console.
    featureConfigurations :: Prelude.Maybe RegisteredUserConsoleFeatureConfigurations,
    -- | The initial URL path for the Amazon QuickSight console. @InitialPath@ is
    -- required.
    --
    -- The entry point URL is constrained to the following paths:
    --
    -- -   @\/start@
    --
    -- -   @\/start\/analyses@
    --
    -- -   @\/start\/dashboards@
    --
    -- -   @\/start\/favorites@
    --
    -- -   @\/dashboards\/DashboardId@. /DashboardId/ is the actual ID key from
    --     the Amazon QuickSight console URL of the dashboard.
    --
    -- -   @\/analyses\/AnalysisId@. /AnalysisId/ is the actual ID key from the
    --     Amazon QuickSight console URL of the analysis.
    initialPath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisteredUserQuickSightConsoleEmbeddingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureConfigurations', 'registeredUserQuickSightConsoleEmbeddingConfiguration_featureConfigurations' - The embedding configuration of an embedded Amazon QuickSight console.
--
-- 'initialPath', 'registeredUserQuickSightConsoleEmbeddingConfiguration_initialPath' - The initial URL path for the Amazon QuickSight console. @InitialPath@ is
-- required.
--
-- The entry point URL is constrained to the following paths:
--
-- -   @\/start@
--
-- -   @\/start\/analyses@
--
-- -   @\/start\/dashboards@
--
-- -   @\/start\/favorites@
--
-- -   @\/dashboards\/DashboardId@. /DashboardId/ is the actual ID key from
--     the Amazon QuickSight console URL of the dashboard.
--
-- -   @\/analyses\/AnalysisId@. /AnalysisId/ is the actual ID key from the
--     Amazon QuickSight console URL of the analysis.
newRegisteredUserQuickSightConsoleEmbeddingConfiguration ::
  RegisteredUserQuickSightConsoleEmbeddingConfiguration
newRegisteredUserQuickSightConsoleEmbeddingConfiguration =
  RegisteredUserQuickSightConsoleEmbeddingConfiguration'
    { featureConfigurations =
        Prelude.Nothing,
      initialPath =
        Prelude.Nothing
    }

-- | The embedding configuration of an embedded Amazon QuickSight console.
registeredUserQuickSightConsoleEmbeddingConfiguration_featureConfigurations :: Lens.Lens' RegisteredUserQuickSightConsoleEmbeddingConfiguration (Prelude.Maybe RegisteredUserConsoleFeatureConfigurations)
registeredUserQuickSightConsoleEmbeddingConfiguration_featureConfigurations = Lens.lens (\RegisteredUserQuickSightConsoleEmbeddingConfiguration' {featureConfigurations} -> featureConfigurations) (\s@RegisteredUserQuickSightConsoleEmbeddingConfiguration' {} a -> s {featureConfigurations = a} :: RegisteredUserQuickSightConsoleEmbeddingConfiguration)

-- | The initial URL path for the Amazon QuickSight console. @InitialPath@ is
-- required.
--
-- The entry point URL is constrained to the following paths:
--
-- -   @\/start@
--
-- -   @\/start\/analyses@
--
-- -   @\/start\/dashboards@
--
-- -   @\/start\/favorites@
--
-- -   @\/dashboards\/DashboardId@. /DashboardId/ is the actual ID key from
--     the Amazon QuickSight console URL of the dashboard.
--
-- -   @\/analyses\/AnalysisId@. /AnalysisId/ is the actual ID key from the
--     Amazon QuickSight console URL of the analysis.
registeredUserQuickSightConsoleEmbeddingConfiguration_initialPath :: Lens.Lens' RegisteredUserQuickSightConsoleEmbeddingConfiguration (Prelude.Maybe Prelude.Text)
registeredUserQuickSightConsoleEmbeddingConfiguration_initialPath = Lens.lens (\RegisteredUserQuickSightConsoleEmbeddingConfiguration' {initialPath} -> initialPath) (\s@RegisteredUserQuickSightConsoleEmbeddingConfiguration' {} a -> s {initialPath = a} :: RegisteredUserQuickSightConsoleEmbeddingConfiguration)

instance
  Prelude.Hashable
    RegisteredUserQuickSightConsoleEmbeddingConfiguration
  where
  hashWithSalt
    _salt
    RegisteredUserQuickSightConsoleEmbeddingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` featureConfigurations
        `Prelude.hashWithSalt` initialPath

instance
  Prelude.NFData
    RegisteredUserQuickSightConsoleEmbeddingConfiguration
  where
  rnf
    RegisteredUserQuickSightConsoleEmbeddingConfiguration' {..} =
      Prelude.rnf featureConfigurations
        `Prelude.seq` Prelude.rnf initialPath

instance
  Data.ToJSON
    RegisteredUserQuickSightConsoleEmbeddingConfiguration
  where
  toJSON
    RegisteredUserQuickSightConsoleEmbeddingConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("FeatureConfigurations" Data..=)
                Prelude.<$> featureConfigurations,
              ("InitialPath" Data..=) Prelude.<$> initialPath
            ]
        )
