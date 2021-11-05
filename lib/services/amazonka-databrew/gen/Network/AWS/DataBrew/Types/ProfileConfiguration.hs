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
-- Module      : Network.AWS.DataBrew.Types.ProfileConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataBrew.Types.ProfileConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.DataBrew.Types.ColumnSelector
import Network.AWS.DataBrew.Types.ColumnStatisticsConfiguration
import Network.AWS.DataBrew.Types.StatisticsConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration for profile jobs. Configuration can be used to select
-- columns, do evaluations, and override default parameters of evaluations.
-- When configuration is undefined, the profile job will apply default
-- settings to all supported columns.
--
-- /See:/ 'newProfileConfiguration' smart constructor.
data ProfileConfiguration = ProfileConfiguration'
  { -- | Configuration for inter-column evaluations. Configuration can be used to
    -- select evaluations and override parameters of evaluations. When
    -- configuration is undefined, the profile job will run all supported
    -- inter-column evaluations.
    datasetStatisticsConfiguration :: Prelude.Maybe StatisticsConfiguration,
    -- | List of configurations for column evaluations.
    -- ColumnStatisticsConfigurations are used to select evaluations and
    -- override parameters of evaluations for particular columns. When
    -- ColumnStatisticsConfigurations is undefined, the profile job will
    -- profile all supported columns and run all supported evaluations.
    columnStatisticsConfigurations :: Prelude.Maybe (Prelude.NonEmpty ColumnStatisticsConfiguration),
    -- | List of column selectors. ProfileColumns can be used to select columns
    -- from the dataset. When ProfileColumns is undefined, the profile job will
    -- profile all supported columns.
    profileColumns :: Prelude.Maybe (Prelude.NonEmpty ColumnSelector)
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
-- 'datasetStatisticsConfiguration', 'profileConfiguration_datasetStatisticsConfiguration' - Configuration for inter-column evaluations. Configuration can be used to
-- select evaluations and override parameters of evaluations. When
-- configuration is undefined, the profile job will run all supported
-- inter-column evaluations.
--
-- 'columnStatisticsConfigurations', 'profileConfiguration_columnStatisticsConfigurations' - List of configurations for column evaluations.
-- ColumnStatisticsConfigurations are used to select evaluations and
-- override parameters of evaluations for particular columns. When
-- ColumnStatisticsConfigurations is undefined, the profile job will
-- profile all supported columns and run all supported evaluations.
--
-- 'profileColumns', 'profileConfiguration_profileColumns' - List of column selectors. ProfileColumns can be used to select columns
-- from the dataset. When ProfileColumns is undefined, the profile job will
-- profile all supported columns.
newProfileConfiguration ::
  ProfileConfiguration
newProfileConfiguration =
  ProfileConfiguration'
    { datasetStatisticsConfiguration =
        Prelude.Nothing,
      columnStatisticsConfigurations = Prelude.Nothing,
      profileColumns = Prelude.Nothing
    }

-- | Configuration for inter-column evaluations. Configuration can be used to
-- select evaluations and override parameters of evaluations. When
-- configuration is undefined, the profile job will run all supported
-- inter-column evaluations.
profileConfiguration_datasetStatisticsConfiguration :: Lens.Lens' ProfileConfiguration (Prelude.Maybe StatisticsConfiguration)
profileConfiguration_datasetStatisticsConfiguration = Lens.lens (\ProfileConfiguration' {datasetStatisticsConfiguration} -> datasetStatisticsConfiguration) (\s@ProfileConfiguration' {} a -> s {datasetStatisticsConfiguration = a} :: ProfileConfiguration)

-- | List of configurations for column evaluations.
-- ColumnStatisticsConfigurations are used to select evaluations and
-- override parameters of evaluations for particular columns. When
-- ColumnStatisticsConfigurations is undefined, the profile job will
-- profile all supported columns and run all supported evaluations.
profileConfiguration_columnStatisticsConfigurations :: Lens.Lens' ProfileConfiguration (Prelude.Maybe (Prelude.NonEmpty ColumnStatisticsConfiguration))
profileConfiguration_columnStatisticsConfigurations = Lens.lens (\ProfileConfiguration' {columnStatisticsConfigurations} -> columnStatisticsConfigurations) (\s@ProfileConfiguration' {} a -> s {columnStatisticsConfigurations = a} :: ProfileConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | List of column selectors. ProfileColumns can be used to select columns
-- from the dataset. When ProfileColumns is undefined, the profile job will
-- profile all supported columns.
profileConfiguration_profileColumns :: Lens.Lens' ProfileConfiguration (Prelude.Maybe (Prelude.NonEmpty ColumnSelector))
profileConfiguration_profileColumns = Lens.lens (\ProfileConfiguration' {profileColumns} -> profileColumns) (\s@ProfileConfiguration' {} a -> s {profileColumns = a} :: ProfileConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ProfileConfiguration where
  parseJSON =
    Core.withObject
      "ProfileConfiguration"
      ( \x ->
          ProfileConfiguration'
            Prelude.<$> (x Core..:? "DatasetStatisticsConfiguration")
            Prelude.<*> (x Core..:? "ColumnStatisticsConfigurations")
            Prelude.<*> (x Core..:? "ProfileColumns")
      )

instance Prelude.Hashable ProfileConfiguration

instance Prelude.NFData ProfileConfiguration

instance Core.ToJSON ProfileConfiguration where
  toJSON ProfileConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DatasetStatisticsConfiguration" Core..=)
              Prelude.<$> datasetStatisticsConfiguration,
            ("ColumnStatisticsConfigurations" Core..=)
              Prelude.<$> columnStatisticsConfigurations,
            ("ProfileColumns" Core..=)
              Prelude.<$> profileColumns
          ]
      )
