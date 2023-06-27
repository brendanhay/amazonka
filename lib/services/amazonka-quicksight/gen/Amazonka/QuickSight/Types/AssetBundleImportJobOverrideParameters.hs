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
-- Module      : Amazonka.QuickSight.Types.AssetBundleImportJobOverrideParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleImportJobOverrideParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AssetBundleImportJobAnalysisOverrideParameters
import Amazonka.QuickSight.Types.AssetBundleImportJobDashboardOverrideParameters
import Amazonka.QuickSight.Types.AssetBundleImportJobDataSetOverrideParameters
import Amazonka.QuickSight.Types.AssetBundleImportJobDataSourceOverrideParameters
import Amazonka.QuickSight.Types.AssetBundleImportJobRefreshScheduleOverrideParameters
import Amazonka.QuickSight.Types.AssetBundleImportJobResourceIdOverrideConfiguration
import Amazonka.QuickSight.Types.AssetBundleImportJobThemeOverrideParameters
import Amazonka.QuickSight.Types.AssetBundleImportJobVPCConnectionOverrideParameters

-- | A list of overrides that modify the asset bundle resource configuration
-- before the resource is imported.
--
-- /See:/ 'newAssetBundleImportJobOverrideParameters' smart constructor.
data AssetBundleImportJobOverrideParameters = AssetBundleImportJobOverrideParameters'
  { -- | A list of overrides for any @Analysis@ resources that are present in the
    -- asset bundle that is imported.
    analyses :: Prelude.Maybe (Prelude.NonEmpty AssetBundleImportJobAnalysisOverrideParameters),
    -- | A list of overrides for any @Dashboard@ resources that are present in
    -- the asset bundle that is imported.
    dashboards :: Prelude.Maybe (Prelude.NonEmpty AssetBundleImportJobDashboardOverrideParameters),
    -- | A list of overrides for any @DataSet@ resources that are present in the
    -- asset bundle that is imported.
    dataSets :: Prelude.Maybe (Prelude.NonEmpty AssetBundleImportJobDataSetOverrideParameters),
    -- | A list of overrides for any @DataSource@ resources that are present in
    -- the asset bundle that is imported.
    dataSources :: Prelude.Maybe (Prelude.NonEmpty AssetBundleImportJobDataSourceOverrideParameters),
    -- | A list of overrides for any @RefreshSchedule@ resources that are present
    -- in the asset bundle that is imported.
    refreshSchedules :: Prelude.Maybe (Prelude.NonEmpty AssetBundleImportJobRefreshScheduleOverrideParameters),
    -- | An optional structure that configures resource ID overrides to be
    -- applied within the import job.
    resourceIdOverrideConfiguration :: Prelude.Maybe AssetBundleImportJobResourceIdOverrideConfiguration,
    -- | A list of overrides for any @Theme@ resources that are present in the
    -- asset bundle that is imported.
    themes :: Prelude.Maybe (Prelude.NonEmpty AssetBundleImportJobThemeOverrideParameters),
    -- | A list of overrides for any @VPCConnection@ resources that are present
    -- in the asset bundle that is imported.
    vPCConnections :: Prelude.Maybe (Prelude.NonEmpty AssetBundleImportJobVPCConnectionOverrideParameters)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleImportJobOverrideParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyses', 'assetBundleImportJobOverrideParameters_analyses' - A list of overrides for any @Analysis@ resources that are present in the
-- asset bundle that is imported.
--
-- 'dashboards', 'assetBundleImportJobOverrideParameters_dashboards' - A list of overrides for any @Dashboard@ resources that are present in
-- the asset bundle that is imported.
--
-- 'dataSets', 'assetBundleImportJobOverrideParameters_dataSets' - A list of overrides for any @DataSet@ resources that are present in the
-- asset bundle that is imported.
--
-- 'dataSources', 'assetBundleImportJobOverrideParameters_dataSources' - A list of overrides for any @DataSource@ resources that are present in
-- the asset bundle that is imported.
--
-- 'refreshSchedules', 'assetBundleImportJobOverrideParameters_refreshSchedules' - A list of overrides for any @RefreshSchedule@ resources that are present
-- in the asset bundle that is imported.
--
-- 'resourceIdOverrideConfiguration', 'assetBundleImportJobOverrideParameters_resourceIdOverrideConfiguration' - An optional structure that configures resource ID overrides to be
-- applied within the import job.
--
-- 'themes', 'assetBundleImportJobOverrideParameters_themes' - A list of overrides for any @Theme@ resources that are present in the
-- asset bundle that is imported.
--
-- 'vPCConnections', 'assetBundleImportJobOverrideParameters_vPCConnections' - A list of overrides for any @VPCConnection@ resources that are present
-- in the asset bundle that is imported.
newAssetBundleImportJobOverrideParameters ::
  AssetBundleImportJobOverrideParameters
newAssetBundleImportJobOverrideParameters =
  AssetBundleImportJobOverrideParameters'
    { analyses =
        Prelude.Nothing,
      dashboards = Prelude.Nothing,
      dataSets = Prelude.Nothing,
      dataSources = Prelude.Nothing,
      refreshSchedules = Prelude.Nothing,
      resourceIdOverrideConfiguration =
        Prelude.Nothing,
      themes = Prelude.Nothing,
      vPCConnections = Prelude.Nothing
    }

-- | A list of overrides for any @Analysis@ resources that are present in the
-- asset bundle that is imported.
assetBundleImportJobOverrideParameters_analyses :: Lens.Lens' AssetBundleImportJobOverrideParameters (Prelude.Maybe (Prelude.NonEmpty AssetBundleImportJobAnalysisOverrideParameters))
assetBundleImportJobOverrideParameters_analyses = Lens.lens (\AssetBundleImportJobOverrideParameters' {analyses} -> analyses) (\s@AssetBundleImportJobOverrideParameters' {} a -> s {analyses = a} :: AssetBundleImportJobOverrideParameters) Prelude.. Lens.mapping Lens.coerced

-- | A list of overrides for any @Dashboard@ resources that are present in
-- the asset bundle that is imported.
assetBundleImportJobOverrideParameters_dashboards :: Lens.Lens' AssetBundleImportJobOverrideParameters (Prelude.Maybe (Prelude.NonEmpty AssetBundleImportJobDashboardOverrideParameters))
assetBundleImportJobOverrideParameters_dashboards = Lens.lens (\AssetBundleImportJobOverrideParameters' {dashboards} -> dashboards) (\s@AssetBundleImportJobOverrideParameters' {} a -> s {dashboards = a} :: AssetBundleImportJobOverrideParameters) Prelude.. Lens.mapping Lens.coerced

-- | A list of overrides for any @DataSet@ resources that are present in the
-- asset bundle that is imported.
assetBundleImportJobOverrideParameters_dataSets :: Lens.Lens' AssetBundleImportJobOverrideParameters (Prelude.Maybe (Prelude.NonEmpty AssetBundleImportJobDataSetOverrideParameters))
assetBundleImportJobOverrideParameters_dataSets = Lens.lens (\AssetBundleImportJobOverrideParameters' {dataSets} -> dataSets) (\s@AssetBundleImportJobOverrideParameters' {} a -> s {dataSets = a} :: AssetBundleImportJobOverrideParameters) Prelude.. Lens.mapping Lens.coerced

-- | A list of overrides for any @DataSource@ resources that are present in
-- the asset bundle that is imported.
assetBundleImportJobOverrideParameters_dataSources :: Lens.Lens' AssetBundleImportJobOverrideParameters (Prelude.Maybe (Prelude.NonEmpty AssetBundleImportJobDataSourceOverrideParameters))
assetBundleImportJobOverrideParameters_dataSources = Lens.lens (\AssetBundleImportJobOverrideParameters' {dataSources} -> dataSources) (\s@AssetBundleImportJobOverrideParameters' {} a -> s {dataSources = a} :: AssetBundleImportJobOverrideParameters) Prelude.. Lens.mapping Lens.coerced

-- | A list of overrides for any @RefreshSchedule@ resources that are present
-- in the asset bundle that is imported.
assetBundleImportJobOverrideParameters_refreshSchedules :: Lens.Lens' AssetBundleImportJobOverrideParameters (Prelude.Maybe (Prelude.NonEmpty AssetBundleImportJobRefreshScheduleOverrideParameters))
assetBundleImportJobOverrideParameters_refreshSchedules = Lens.lens (\AssetBundleImportJobOverrideParameters' {refreshSchedules} -> refreshSchedules) (\s@AssetBundleImportJobOverrideParameters' {} a -> s {refreshSchedules = a} :: AssetBundleImportJobOverrideParameters) Prelude.. Lens.mapping Lens.coerced

-- | An optional structure that configures resource ID overrides to be
-- applied within the import job.
assetBundleImportJobOverrideParameters_resourceIdOverrideConfiguration :: Lens.Lens' AssetBundleImportJobOverrideParameters (Prelude.Maybe AssetBundleImportJobResourceIdOverrideConfiguration)
assetBundleImportJobOverrideParameters_resourceIdOverrideConfiguration = Lens.lens (\AssetBundleImportJobOverrideParameters' {resourceIdOverrideConfiguration} -> resourceIdOverrideConfiguration) (\s@AssetBundleImportJobOverrideParameters' {} a -> s {resourceIdOverrideConfiguration = a} :: AssetBundleImportJobOverrideParameters)

-- | A list of overrides for any @Theme@ resources that are present in the
-- asset bundle that is imported.
assetBundleImportJobOverrideParameters_themes :: Lens.Lens' AssetBundleImportJobOverrideParameters (Prelude.Maybe (Prelude.NonEmpty AssetBundleImportJobThemeOverrideParameters))
assetBundleImportJobOverrideParameters_themes = Lens.lens (\AssetBundleImportJobOverrideParameters' {themes} -> themes) (\s@AssetBundleImportJobOverrideParameters' {} a -> s {themes = a} :: AssetBundleImportJobOverrideParameters) Prelude.. Lens.mapping Lens.coerced

-- | A list of overrides for any @VPCConnection@ resources that are present
-- in the asset bundle that is imported.
assetBundleImportJobOverrideParameters_vPCConnections :: Lens.Lens' AssetBundleImportJobOverrideParameters (Prelude.Maybe (Prelude.NonEmpty AssetBundleImportJobVPCConnectionOverrideParameters))
assetBundleImportJobOverrideParameters_vPCConnections = Lens.lens (\AssetBundleImportJobOverrideParameters' {vPCConnections} -> vPCConnections) (\s@AssetBundleImportJobOverrideParameters' {} a -> s {vPCConnections = a} :: AssetBundleImportJobOverrideParameters) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AssetBundleImportJobOverrideParameters
  where
  parseJSON =
    Data.withObject
      "AssetBundleImportJobOverrideParameters"
      ( \x ->
          AssetBundleImportJobOverrideParameters'
            Prelude.<$> (x Data..:? "Analyses")
            Prelude.<*> (x Data..:? "Dashboards")
            Prelude.<*> (x Data..:? "DataSets")
            Prelude.<*> (x Data..:? "DataSources")
            Prelude.<*> (x Data..:? "RefreshSchedules")
            Prelude.<*> (x Data..:? "ResourceIdOverrideConfiguration")
            Prelude.<*> (x Data..:? "Themes")
            Prelude.<*> (x Data..:? "VPCConnections")
      )

instance
  Prelude.Hashable
    AssetBundleImportJobOverrideParameters
  where
  hashWithSalt
    _salt
    AssetBundleImportJobOverrideParameters' {..} =
      _salt
        `Prelude.hashWithSalt` analyses
        `Prelude.hashWithSalt` dashboards
        `Prelude.hashWithSalt` dataSets
        `Prelude.hashWithSalt` dataSources
        `Prelude.hashWithSalt` refreshSchedules
        `Prelude.hashWithSalt` resourceIdOverrideConfiguration
        `Prelude.hashWithSalt` themes
        `Prelude.hashWithSalt` vPCConnections

instance
  Prelude.NFData
    AssetBundleImportJobOverrideParameters
  where
  rnf AssetBundleImportJobOverrideParameters' {..} =
    Prelude.rnf analyses
      `Prelude.seq` Prelude.rnf dashboards
      `Prelude.seq` Prelude.rnf dataSets
      `Prelude.seq` Prelude.rnf dataSources
      `Prelude.seq` Prelude.rnf refreshSchedules
      `Prelude.seq` Prelude.rnf resourceIdOverrideConfiguration
      `Prelude.seq` Prelude.rnf themes
      `Prelude.seq` Prelude.rnf vPCConnections

instance
  Data.ToJSON
    AssetBundleImportJobOverrideParameters
  where
  toJSON AssetBundleImportJobOverrideParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Analyses" Data..=) Prelude.<$> analyses,
            ("Dashboards" Data..=) Prelude.<$> dashboards,
            ("DataSets" Data..=) Prelude.<$> dataSets,
            ("DataSources" Data..=) Prelude.<$> dataSources,
            ("RefreshSchedules" Data..=)
              Prelude.<$> refreshSchedules,
            ("ResourceIdOverrideConfiguration" Data..=)
              Prelude.<$> resourceIdOverrideConfiguration,
            ("Themes" Data..=) Prelude.<$> themes,
            ("VPCConnections" Data..=)
              Prelude.<$> vPCConnections
          ]
      )
