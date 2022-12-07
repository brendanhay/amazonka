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
-- Module      : Amazonka.QuickSight.Types.AnonymousUserEmbeddingExperienceConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AnonymousUserEmbeddingExperienceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AnonymousUserDashboardEmbeddingConfiguration
import Amazonka.QuickSight.Types.AnonymousUserDashboardVisualEmbeddingConfiguration
import Amazonka.QuickSight.Types.AnonymousUserQSearchBarEmbeddingConfiguration

-- | The type of experience you want to embed. For anonymous users, you can
-- embed Amazon QuickSight dashboards.
--
-- /See:/ 'newAnonymousUserEmbeddingExperienceConfiguration' smart constructor.
data AnonymousUserEmbeddingExperienceConfiguration = AnonymousUserEmbeddingExperienceConfiguration'
  { -- | The type of embedding experience. In this case, Amazon QuickSight
    -- visuals.
    dashboardVisual :: Prelude.Maybe AnonymousUserDashboardVisualEmbeddingConfiguration,
    -- | The type of embedding experience. In this case, Amazon QuickSight
    -- dashboards.
    dashboard :: Prelude.Maybe AnonymousUserDashboardEmbeddingConfiguration,
    -- | The Q search bar that you want to use for anonymous user embedding.
    qSearchBar :: Prelude.Maybe AnonymousUserQSearchBarEmbeddingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnonymousUserEmbeddingExperienceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardVisual', 'anonymousUserEmbeddingExperienceConfiguration_dashboardVisual' - The type of embedding experience. In this case, Amazon QuickSight
-- visuals.
--
-- 'dashboard', 'anonymousUserEmbeddingExperienceConfiguration_dashboard' - The type of embedding experience. In this case, Amazon QuickSight
-- dashboards.
--
-- 'qSearchBar', 'anonymousUserEmbeddingExperienceConfiguration_qSearchBar' - The Q search bar that you want to use for anonymous user embedding.
newAnonymousUserEmbeddingExperienceConfiguration ::
  AnonymousUserEmbeddingExperienceConfiguration
newAnonymousUserEmbeddingExperienceConfiguration =
  AnonymousUserEmbeddingExperienceConfiguration'
    { dashboardVisual =
        Prelude.Nothing,
      dashboard = Prelude.Nothing,
      qSearchBar = Prelude.Nothing
    }

-- | The type of embedding experience. In this case, Amazon QuickSight
-- visuals.
anonymousUserEmbeddingExperienceConfiguration_dashboardVisual :: Lens.Lens' AnonymousUserEmbeddingExperienceConfiguration (Prelude.Maybe AnonymousUserDashboardVisualEmbeddingConfiguration)
anonymousUserEmbeddingExperienceConfiguration_dashboardVisual = Lens.lens (\AnonymousUserEmbeddingExperienceConfiguration' {dashboardVisual} -> dashboardVisual) (\s@AnonymousUserEmbeddingExperienceConfiguration' {} a -> s {dashboardVisual = a} :: AnonymousUserEmbeddingExperienceConfiguration)

-- | The type of embedding experience. In this case, Amazon QuickSight
-- dashboards.
anonymousUserEmbeddingExperienceConfiguration_dashboard :: Lens.Lens' AnonymousUserEmbeddingExperienceConfiguration (Prelude.Maybe AnonymousUserDashboardEmbeddingConfiguration)
anonymousUserEmbeddingExperienceConfiguration_dashboard = Lens.lens (\AnonymousUserEmbeddingExperienceConfiguration' {dashboard} -> dashboard) (\s@AnonymousUserEmbeddingExperienceConfiguration' {} a -> s {dashboard = a} :: AnonymousUserEmbeddingExperienceConfiguration)

-- | The Q search bar that you want to use for anonymous user embedding.
anonymousUserEmbeddingExperienceConfiguration_qSearchBar :: Lens.Lens' AnonymousUserEmbeddingExperienceConfiguration (Prelude.Maybe AnonymousUserQSearchBarEmbeddingConfiguration)
anonymousUserEmbeddingExperienceConfiguration_qSearchBar = Lens.lens (\AnonymousUserEmbeddingExperienceConfiguration' {qSearchBar} -> qSearchBar) (\s@AnonymousUserEmbeddingExperienceConfiguration' {} a -> s {qSearchBar = a} :: AnonymousUserEmbeddingExperienceConfiguration)

instance
  Prelude.Hashable
    AnonymousUserEmbeddingExperienceConfiguration
  where
  hashWithSalt
    _salt
    AnonymousUserEmbeddingExperienceConfiguration' {..} =
      _salt `Prelude.hashWithSalt` dashboardVisual
        `Prelude.hashWithSalt` dashboard
        `Prelude.hashWithSalt` qSearchBar

instance
  Prelude.NFData
    AnonymousUserEmbeddingExperienceConfiguration
  where
  rnf
    AnonymousUserEmbeddingExperienceConfiguration' {..} =
      Prelude.rnf dashboardVisual
        `Prelude.seq` Prelude.rnf dashboard
        `Prelude.seq` Prelude.rnf qSearchBar

instance
  Data.ToJSON
    AnonymousUserEmbeddingExperienceConfiguration
  where
  toJSON
    AnonymousUserEmbeddingExperienceConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("DashboardVisual" Data..=)
                Prelude.<$> dashboardVisual,
              ("Dashboard" Data..=) Prelude.<$> dashboard,
              ("QSearchBar" Data..=) Prelude.<$> qSearchBar
            ]
        )
