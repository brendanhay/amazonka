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
-- Module      : Amazonka.DataBrew.Types.ProfileConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.ProfileConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataBrew.Types.ColumnSelector
import Amazonka.DataBrew.Types.ColumnStatisticsConfiguration
import Amazonka.DataBrew.Types.EntityDetectorConfiguration
import Amazonka.DataBrew.Types.StatisticsConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Configuration for profile jobs. Configuration can be used to select
-- columns, do evaluations, and override default parameters of evaluations.
-- When configuration is undefined, the profile job will apply default
-- settings to all supported columns.
--
-- /See:/ 'newProfileConfiguration' smart constructor.
data ProfileConfiguration = ProfileConfiguration'
  { -- | List of configurations for column evaluations.
    -- ColumnStatisticsConfigurations are used to select evaluations and
    -- override parameters of evaluations for particular columns. When
    -- ColumnStatisticsConfigurations is undefined, the profile job will
    -- profile all supported columns and run all supported evaluations.
    columnStatisticsConfigurations :: Prelude.Maybe (Prelude.NonEmpty ColumnStatisticsConfiguration),
    -- | Configuration for inter-column evaluations. Configuration can be used to
    -- select evaluations and override parameters of evaluations. When
    -- configuration is undefined, the profile job will run all supported
    -- inter-column evaluations.
    datasetStatisticsConfiguration :: Prelude.Maybe StatisticsConfiguration,
    -- | List of column selectors. ProfileColumns can be used to select columns
    -- from the dataset. When ProfileColumns is undefined, the profile job will
    -- profile all supported columns.
    profileColumns :: Prelude.Maybe (Prelude.NonEmpty ColumnSelector),
    -- | Configuration of entity detection for a profile job. When undefined,
    -- entity detection is disabled.
    entityDetectorConfiguration :: Prelude.Maybe EntityDetectorConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfileConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnStatisticsConfigurations', 'profileConfiguration_columnStatisticsConfigurations' - List of configurations for column evaluations.
-- ColumnStatisticsConfigurations are used to select evaluations and
-- override parameters of evaluations for particular columns. When
-- ColumnStatisticsConfigurations is undefined, the profile job will
-- profile all supported columns and run all supported evaluations.
--
-- 'datasetStatisticsConfiguration', 'profileConfiguration_datasetStatisticsConfiguration' - Configuration for inter-column evaluations. Configuration can be used to
-- select evaluations and override parameters of evaluations. When
-- configuration is undefined, the profile job will run all supported
-- inter-column evaluations.
--
-- 'profileColumns', 'profileConfiguration_profileColumns' - List of column selectors. ProfileColumns can be used to select columns
-- from the dataset. When ProfileColumns is undefined, the profile job will
-- profile all supported columns.
--
-- 'entityDetectorConfiguration', 'profileConfiguration_entityDetectorConfiguration' - Configuration of entity detection for a profile job. When undefined,
-- entity detection is disabled.
newProfileConfiguration ::
  ProfileConfiguration
newProfileConfiguration =
  ProfileConfiguration'
    { columnStatisticsConfigurations =
        Prelude.Nothing,
      datasetStatisticsConfiguration = Prelude.Nothing,
      profileColumns = Prelude.Nothing,
      entityDetectorConfiguration = Prelude.Nothing
    }

-- | List of configurations for column evaluations.
-- ColumnStatisticsConfigurations are used to select evaluations and
-- override parameters of evaluations for particular columns. When
-- ColumnStatisticsConfigurations is undefined, the profile job will
-- profile all supported columns and run all supported evaluations.
profileConfiguration_columnStatisticsConfigurations :: Lens.Lens' ProfileConfiguration (Prelude.Maybe (Prelude.NonEmpty ColumnStatisticsConfiguration))
profileConfiguration_columnStatisticsConfigurations = Lens.lens (\ProfileConfiguration' {columnStatisticsConfigurations} -> columnStatisticsConfigurations) (\s@ProfileConfiguration' {} a -> s {columnStatisticsConfigurations = a} :: ProfileConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Configuration for inter-column evaluations. Configuration can be used to
-- select evaluations and override parameters of evaluations. When
-- configuration is undefined, the profile job will run all supported
-- inter-column evaluations.
profileConfiguration_datasetStatisticsConfiguration :: Lens.Lens' ProfileConfiguration (Prelude.Maybe StatisticsConfiguration)
profileConfiguration_datasetStatisticsConfiguration = Lens.lens (\ProfileConfiguration' {datasetStatisticsConfiguration} -> datasetStatisticsConfiguration) (\s@ProfileConfiguration' {} a -> s {datasetStatisticsConfiguration = a} :: ProfileConfiguration)

-- | List of column selectors. ProfileColumns can be used to select columns
-- from the dataset. When ProfileColumns is undefined, the profile job will
-- profile all supported columns.
profileConfiguration_profileColumns :: Lens.Lens' ProfileConfiguration (Prelude.Maybe (Prelude.NonEmpty ColumnSelector))
profileConfiguration_profileColumns = Lens.lens (\ProfileConfiguration' {profileColumns} -> profileColumns) (\s@ProfileConfiguration' {} a -> s {profileColumns = a} :: ProfileConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Configuration of entity detection for a profile job. When undefined,
-- entity detection is disabled.
profileConfiguration_entityDetectorConfiguration :: Lens.Lens' ProfileConfiguration (Prelude.Maybe EntityDetectorConfiguration)
profileConfiguration_entityDetectorConfiguration = Lens.lens (\ProfileConfiguration' {entityDetectorConfiguration} -> entityDetectorConfiguration) (\s@ProfileConfiguration' {} a -> s {entityDetectorConfiguration = a} :: ProfileConfiguration)

instance Core.FromJSON ProfileConfiguration where
  parseJSON =
    Core.withObject
      "ProfileConfiguration"
      ( \x ->
          ProfileConfiguration'
            Prelude.<$> (x Core..:? "ColumnStatisticsConfigurations")
            Prelude.<*> (x Core..:? "DatasetStatisticsConfiguration")
            Prelude.<*> (x Core..:? "ProfileColumns")
            Prelude.<*> (x Core..:? "EntityDetectorConfiguration")
      )

instance Prelude.Hashable ProfileConfiguration where
  hashWithSalt _salt ProfileConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` columnStatisticsConfigurations
      `Prelude.hashWithSalt` datasetStatisticsConfiguration
      `Prelude.hashWithSalt` profileColumns
      `Prelude.hashWithSalt` entityDetectorConfiguration

instance Prelude.NFData ProfileConfiguration where
  rnf ProfileConfiguration' {..} =
    Prelude.rnf columnStatisticsConfigurations
      `Prelude.seq` Prelude.rnf datasetStatisticsConfiguration
      `Prelude.seq` Prelude.rnf profileColumns
      `Prelude.seq` Prelude.rnf entityDetectorConfiguration

instance Core.ToJSON ProfileConfiguration where
  toJSON ProfileConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ColumnStatisticsConfigurations" Core..=)
              Prelude.<$> columnStatisticsConfigurations,
            ("DatasetStatisticsConfiguration" Core..=)
              Prelude.<$> datasetStatisticsConfiguration,
            ("ProfileColumns" Core..=)
              Prelude.<$> profileColumns,
            ("EntityDetectorConfiguration" Core..=)
              Prelude.<$> entityDetectorConfiguration
          ]
      )
