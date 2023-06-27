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
-- Module      : Amazonka.QuickSight.Types.AssetBundleImportJobDashboardOverrideParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleImportJobDashboardOverrideParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The override parameters for a single dashboard that is being imported.
--
-- /See:/ 'newAssetBundleImportJobDashboardOverrideParameters' smart constructor.
data AssetBundleImportJobDashboardOverrideParameters = AssetBundleImportJobDashboardOverrideParameters'
  { -- | A new name for the dashboard.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the dashboard that you want to apply overrides to.
    dashboardId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleImportJobDashboardOverrideParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'assetBundleImportJobDashboardOverrideParameters_name' - A new name for the dashboard.
--
-- 'dashboardId', 'assetBundleImportJobDashboardOverrideParameters_dashboardId' - The ID of the dashboard that you want to apply overrides to.
newAssetBundleImportJobDashboardOverrideParameters ::
  -- | 'dashboardId'
  Prelude.Text ->
  AssetBundleImportJobDashboardOverrideParameters
newAssetBundleImportJobDashboardOverrideParameters
  pDashboardId_ =
    AssetBundleImportJobDashboardOverrideParameters'
      { name =
          Prelude.Nothing,
        dashboardId =
          pDashboardId_
      }

-- | A new name for the dashboard.
assetBundleImportJobDashboardOverrideParameters_name :: Lens.Lens' AssetBundleImportJobDashboardOverrideParameters (Prelude.Maybe Prelude.Text)
assetBundleImportJobDashboardOverrideParameters_name = Lens.lens (\AssetBundleImportJobDashboardOverrideParameters' {name} -> name) (\s@AssetBundleImportJobDashboardOverrideParameters' {} a -> s {name = a} :: AssetBundleImportJobDashboardOverrideParameters)

-- | The ID of the dashboard that you want to apply overrides to.
assetBundleImportJobDashboardOverrideParameters_dashboardId :: Lens.Lens' AssetBundleImportJobDashboardOverrideParameters Prelude.Text
assetBundleImportJobDashboardOverrideParameters_dashboardId = Lens.lens (\AssetBundleImportJobDashboardOverrideParameters' {dashboardId} -> dashboardId) (\s@AssetBundleImportJobDashboardOverrideParameters' {} a -> s {dashboardId = a} :: AssetBundleImportJobDashboardOverrideParameters)

instance
  Data.FromJSON
    AssetBundleImportJobDashboardOverrideParameters
  where
  parseJSON =
    Data.withObject
      "AssetBundleImportJobDashboardOverrideParameters"
      ( \x ->
          AssetBundleImportJobDashboardOverrideParameters'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..: "DashboardId")
      )

instance
  Prelude.Hashable
    AssetBundleImportJobDashboardOverrideParameters
  where
  hashWithSalt
    _salt
    AssetBundleImportJobDashboardOverrideParameters' {..} =
      _salt
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` dashboardId

instance
  Prelude.NFData
    AssetBundleImportJobDashboardOverrideParameters
  where
  rnf
    AssetBundleImportJobDashboardOverrideParameters' {..} =
      Prelude.rnf name
        `Prelude.seq` Prelude.rnf dashboardId

instance
  Data.ToJSON
    AssetBundleImportJobDashboardOverrideParameters
  where
  toJSON
    AssetBundleImportJobDashboardOverrideParameters' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Name" Data..=) Prelude.<$> name,
              Prelude.Just ("DashboardId" Data..= dashboardId)
            ]
        )
