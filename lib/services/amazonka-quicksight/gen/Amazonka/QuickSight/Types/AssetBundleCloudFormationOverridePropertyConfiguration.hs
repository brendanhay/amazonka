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
-- Module      : Amazonka.QuickSight.Types.AssetBundleCloudFormationOverridePropertyConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleCloudFormationOverridePropertyConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AssetBundleExportJobAnalysisOverrideProperties
import Amazonka.QuickSight.Types.AssetBundleExportJobDashboardOverrideProperties
import Amazonka.QuickSight.Types.AssetBundleExportJobDataSetOverrideProperties
import Amazonka.QuickSight.Types.AssetBundleExportJobDataSourceOverrideProperties
import Amazonka.QuickSight.Types.AssetBundleExportJobRefreshScheduleOverrideProperties
import Amazonka.QuickSight.Types.AssetBundleExportJobResourceIdOverrideConfiguration
import Amazonka.QuickSight.Types.AssetBundleExportJobThemeOverrideProperties
import Amazonka.QuickSight.Types.AssetBundleExportJobVPCConnectionOverrideProperties

-- | An optional collection of CloudFormation property configurations that
-- control how the export job is generated.
--
-- /See:/ 'newAssetBundleCloudFormationOverridePropertyConfiguration' smart constructor.
data AssetBundleCloudFormationOverridePropertyConfiguration = AssetBundleCloudFormationOverridePropertyConfiguration'
  { -- | An optional list of structures that control how @Analysis@ resources are
    -- parameterized in the returned CloudFormation template.
    analyses :: Prelude.Maybe (Prelude.NonEmpty AssetBundleExportJobAnalysisOverrideProperties),
    -- | An optional list of structures that control how @Dashboard@ resources
    -- are parameterized in the returned CloudFormation template.
    dashboards :: Prelude.Maybe (Prelude.NonEmpty AssetBundleExportJobDashboardOverrideProperties),
    -- | An optional list of structures that control how @DataSet@ resources are
    -- parameterized in the returned CloudFormation template.
    dataSets :: Prelude.Maybe (Prelude.NonEmpty AssetBundleExportJobDataSetOverrideProperties),
    -- | An optional list of structures that control how @DataSource@ resources
    -- are parameterized in the returned CloudFormation template.
    dataSources :: Prelude.Maybe (Prelude.NonEmpty AssetBundleExportJobDataSourceOverrideProperties),
    -- | An optional list of structures that control how @RefreshSchedule@
    -- resources are parameterized in the returned CloudFormation template.
    refreshSchedules :: Prelude.Maybe (Prelude.NonEmpty AssetBundleExportJobRefreshScheduleOverrideProperties),
    -- | An optional list of structures that control how resource IDs are
    -- parameterized in the returned CloudFormation template.
    resourceIdOverrideConfiguration :: Prelude.Maybe AssetBundleExportJobResourceIdOverrideConfiguration,
    -- | An optional list of structures that control how @Theme@ resources are
    -- parameterized in the returned CloudFormation template.
    themes :: Prelude.Maybe (Prelude.NonEmpty AssetBundleExportJobThemeOverrideProperties),
    -- | An optional list of structures that control how @VPCConnection@
    -- resources are parameterized in the returned CloudFormation template.
    vPCConnections :: Prelude.Maybe (Prelude.NonEmpty AssetBundleExportJobVPCConnectionOverrideProperties)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleCloudFormationOverridePropertyConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyses', 'assetBundleCloudFormationOverridePropertyConfiguration_analyses' - An optional list of structures that control how @Analysis@ resources are
-- parameterized in the returned CloudFormation template.
--
-- 'dashboards', 'assetBundleCloudFormationOverridePropertyConfiguration_dashboards' - An optional list of structures that control how @Dashboard@ resources
-- are parameterized in the returned CloudFormation template.
--
-- 'dataSets', 'assetBundleCloudFormationOverridePropertyConfiguration_dataSets' - An optional list of structures that control how @DataSet@ resources are
-- parameterized in the returned CloudFormation template.
--
-- 'dataSources', 'assetBundleCloudFormationOverridePropertyConfiguration_dataSources' - An optional list of structures that control how @DataSource@ resources
-- are parameterized in the returned CloudFormation template.
--
-- 'refreshSchedules', 'assetBundleCloudFormationOverridePropertyConfiguration_refreshSchedules' - An optional list of structures that control how @RefreshSchedule@
-- resources are parameterized in the returned CloudFormation template.
--
-- 'resourceIdOverrideConfiguration', 'assetBundleCloudFormationOverridePropertyConfiguration_resourceIdOverrideConfiguration' - An optional list of structures that control how resource IDs are
-- parameterized in the returned CloudFormation template.
--
-- 'themes', 'assetBundleCloudFormationOverridePropertyConfiguration_themes' - An optional list of structures that control how @Theme@ resources are
-- parameterized in the returned CloudFormation template.
--
-- 'vPCConnections', 'assetBundleCloudFormationOverridePropertyConfiguration_vPCConnections' - An optional list of structures that control how @VPCConnection@
-- resources are parameterized in the returned CloudFormation template.
newAssetBundleCloudFormationOverridePropertyConfiguration ::
  AssetBundleCloudFormationOverridePropertyConfiguration
newAssetBundleCloudFormationOverridePropertyConfiguration =
  AssetBundleCloudFormationOverridePropertyConfiguration'
    { analyses =
        Prelude.Nothing,
      dashboards =
        Prelude.Nothing,
      dataSets =
        Prelude.Nothing,
      dataSources =
        Prelude.Nothing,
      refreshSchedules =
        Prelude.Nothing,
      resourceIdOverrideConfiguration =
        Prelude.Nothing,
      themes =
        Prelude.Nothing,
      vPCConnections =
        Prelude.Nothing
    }

-- | An optional list of structures that control how @Analysis@ resources are
-- parameterized in the returned CloudFormation template.
assetBundleCloudFormationOverridePropertyConfiguration_analyses :: Lens.Lens' AssetBundleCloudFormationOverridePropertyConfiguration (Prelude.Maybe (Prelude.NonEmpty AssetBundleExportJobAnalysisOverrideProperties))
assetBundleCloudFormationOverridePropertyConfiguration_analyses = Lens.lens (\AssetBundleCloudFormationOverridePropertyConfiguration' {analyses} -> analyses) (\s@AssetBundleCloudFormationOverridePropertyConfiguration' {} a -> s {analyses = a} :: AssetBundleCloudFormationOverridePropertyConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | An optional list of structures that control how @Dashboard@ resources
-- are parameterized in the returned CloudFormation template.
assetBundleCloudFormationOverridePropertyConfiguration_dashboards :: Lens.Lens' AssetBundleCloudFormationOverridePropertyConfiguration (Prelude.Maybe (Prelude.NonEmpty AssetBundleExportJobDashboardOverrideProperties))
assetBundleCloudFormationOverridePropertyConfiguration_dashboards = Lens.lens (\AssetBundleCloudFormationOverridePropertyConfiguration' {dashboards} -> dashboards) (\s@AssetBundleCloudFormationOverridePropertyConfiguration' {} a -> s {dashboards = a} :: AssetBundleCloudFormationOverridePropertyConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | An optional list of structures that control how @DataSet@ resources are
-- parameterized in the returned CloudFormation template.
assetBundleCloudFormationOverridePropertyConfiguration_dataSets :: Lens.Lens' AssetBundleCloudFormationOverridePropertyConfiguration (Prelude.Maybe (Prelude.NonEmpty AssetBundleExportJobDataSetOverrideProperties))
assetBundleCloudFormationOverridePropertyConfiguration_dataSets = Lens.lens (\AssetBundleCloudFormationOverridePropertyConfiguration' {dataSets} -> dataSets) (\s@AssetBundleCloudFormationOverridePropertyConfiguration' {} a -> s {dataSets = a} :: AssetBundleCloudFormationOverridePropertyConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | An optional list of structures that control how @DataSource@ resources
-- are parameterized in the returned CloudFormation template.
assetBundleCloudFormationOverridePropertyConfiguration_dataSources :: Lens.Lens' AssetBundleCloudFormationOverridePropertyConfiguration (Prelude.Maybe (Prelude.NonEmpty AssetBundleExportJobDataSourceOverrideProperties))
assetBundleCloudFormationOverridePropertyConfiguration_dataSources = Lens.lens (\AssetBundleCloudFormationOverridePropertyConfiguration' {dataSources} -> dataSources) (\s@AssetBundleCloudFormationOverridePropertyConfiguration' {} a -> s {dataSources = a} :: AssetBundleCloudFormationOverridePropertyConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | An optional list of structures that control how @RefreshSchedule@
-- resources are parameterized in the returned CloudFormation template.
assetBundleCloudFormationOverridePropertyConfiguration_refreshSchedules :: Lens.Lens' AssetBundleCloudFormationOverridePropertyConfiguration (Prelude.Maybe (Prelude.NonEmpty AssetBundleExportJobRefreshScheduleOverrideProperties))
assetBundleCloudFormationOverridePropertyConfiguration_refreshSchedules = Lens.lens (\AssetBundleCloudFormationOverridePropertyConfiguration' {refreshSchedules} -> refreshSchedules) (\s@AssetBundleCloudFormationOverridePropertyConfiguration' {} a -> s {refreshSchedules = a} :: AssetBundleCloudFormationOverridePropertyConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | An optional list of structures that control how resource IDs are
-- parameterized in the returned CloudFormation template.
assetBundleCloudFormationOverridePropertyConfiguration_resourceIdOverrideConfiguration :: Lens.Lens' AssetBundleCloudFormationOverridePropertyConfiguration (Prelude.Maybe AssetBundleExportJobResourceIdOverrideConfiguration)
assetBundleCloudFormationOverridePropertyConfiguration_resourceIdOverrideConfiguration = Lens.lens (\AssetBundleCloudFormationOverridePropertyConfiguration' {resourceIdOverrideConfiguration} -> resourceIdOverrideConfiguration) (\s@AssetBundleCloudFormationOverridePropertyConfiguration' {} a -> s {resourceIdOverrideConfiguration = a} :: AssetBundleCloudFormationOverridePropertyConfiguration)

-- | An optional list of structures that control how @Theme@ resources are
-- parameterized in the returned CloudFormation template.
assetBundleCloudFormationOverridePropertyConfiguration_themes :: Lens.Lens' AssetBundleCloudFormationOverridePropertyConfiguration (Prelude.Maybe (Prelude.NonEmpty AssetBundleExportJobThemeOverrideProperties))
assetBundleCloudFormationOverridePropertyConfiguration_themes = Lens.lens (\AssetBundleCloudFormationOverridePropertyConfiguration' {themes} -> themes) (\s@AssetBundleCloudFormationOverridePropertyConfiguration' {} a -> s {themes = a} :: AssetBundleCloudFormationOverridePropertyConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | An optional list of structures that control how @VPCConnection@
-- resources are parameterized in the returned CloudFormation template.
assetBundleCloudFormationOverridePropertyConfiguration_vPCConnections :: Lens.Lens' AssetBundleCloudFormationOverridePropertyConfiguration (Prelude.Maybe (Prelude.NonEmpty AssetBundleExportJobVPCConnectionOverrideProperties))
assetBundleCloudFormationOverridePropertyConfiguration_vPCConnections = Lens.lens (\AssetBundleCloudFormationOverridePropertyConfiguration' {vPCConnections} -> vPCConnections) (\s@AssetBundleCloudFormationOverridePropertyConfiguration' {} a -> s {vPCConnections = a} :: AssetBundleCloudFormationOverridePropertyConfiguration) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AssetBundleCloudFormationOverridePropertyConfiguration
  where
  parseJSON =
    Data.withObject
      "AssetBundleCloudFormationOverridePropertyConfiguration"
      ( \x ->
          AssetBundleCloudFormationOverridePropertyConfiguration'
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
    AssetBundleCloudFormationOverridePropertyConfiguration
  where
  hashWithSalt
    _salt
    AssetBundleCloudFormationOverridePropertyConfiguration' {..} =
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
    AssetBundleCloudFormationOverridePropertyConfiguration
  where
  rnf
    AssetBundleCloudFormationOverridePropertyConfiguration' {..} =
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
    AssetBundleCloudFormationOverridePropertyConfiguration
  where
  toJSON
    AssetBundleCloudFormationOverridePropertyConfiguration' {..} =
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
