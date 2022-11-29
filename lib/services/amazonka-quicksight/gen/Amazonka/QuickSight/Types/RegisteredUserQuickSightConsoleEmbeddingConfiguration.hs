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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RegisteredUserQuickSightConsoleEmbeddingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the Amazon QuickSight console that you want to embed.
--
-- /See:/ 'newRegisteredUserQuickSightConsoleEmbeddingConfiguration' smart constructor.
data RegisteredUserQuickSightConsoleEmbeddingConfiguration = RegisteredUserQuickSightConsoleEmbeddingConfiguration'
  { -- | The initial URL path for the Amazon QuickSight console. @InitialPath@ is
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
    { initialPath =
        Prelude.Nothing
    }

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
      _salt `Prelude.hashWithSalt` initialPath

instance
  Prelude.NFData
    RegisteredUserQuickSightConsoleEmbeddingConfiguration
  where
  rnf
    RegisteredUserQuickSightConsoleEmbeddingConfiguration' {..} =
      Prelude.rnf initialPath

instance
  Core.ToJSON
    RegisteredUserQuickSightConsoleEmbeddingConfiguration
  where
  toJSON
    RegisteredUserQuickSightConsoleEmbeddingConfiguration' {..} =
      Core.object
        ( Prelude.catMaybes
            [("InitialPath" Core..=) Prelude.<$> initialPath]
        )
