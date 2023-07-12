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
-- Module      : Amazonka.DataBrew.Types.ColumnStatisticsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.ColumnStatisticsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types.ColumnSelector
import Amazonka.DataBrew.Types.StatisticsConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Configuration for column evaluations for a profile job.
-- ColumnStatisticsConfiguration can be used to select evaluations and
-- override parameters of evaluations for particular columns.
--
-- /See:/ 'newColumnStatisticsConfiguration' smart constructor.
data ColumnStatisticsConfiguration = ColumnStatisticsConfiguration'
  { -- | List of column selectors. Selectors can be used to select columns from
    -- the dataset. When selectors are undefined, configuration will be applied
    -- to all supported columns.
    selectors :: Prelude.Maybe (Prelude.NonEmpty ColumnSelector),
    -- | Configuration for evaluations. Statistics can be used to select
    -- evaluations and override parameters of evaluations.
    statistics :: StatisticsConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnStatisticsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selectors', 'columnStatisticsConfiguration_selectors' - List of column selectors. Selectors can be used to select columns from
-- the dataset. When selectors are undefined, configuration will be applied
-- to all supported columns.
--
-- 'statistics', 'columnStatisticsConfiguration_statistics' - Configuration for evaluations. Statistics can be used to select
-- evaluations and override parameters of evaluations.
newColumnStatisticsConfiguration ::
  -- | 'statistics'
  StatisticsConfiguration ->
  ColumnStatisticsConfiguration
newColumnStatisticsConfiguration pStatistics_ =
  ColumnStatisticsConfiguration'
    { selectors =
        Prelude.Nothing,
      statistics = pStatistics_
    }

-- | List of column selectors. Selectors can be used to select columns from
-- the dataset. When selectors are undefined, configuration will be applied
-- to all supported columns.
columnStatisticsConfiguration_selectors :: Lens.Lens' ColumnStatisticsConfiguration (Prelude.Maybe (Prelude.NonEmpty ColumnSelector))
columnStatisticsConfiguration_selectors = Lens.lens (\ColumnStatisticsConfiguration' {selectors} -> selectors) (\s@ColumnStatisticsConfiguration' {} a -> s {selectors = a} :: ColumnStatisticsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Configuration for evaluations. Statistics can be used to select
-- evaluations and override parameters of evaluations.
columnStatisticsConfiguration_statistics :: Lens.Lens' ColumnStatisticsConfiguration StatisticsConfiguration
columnStatisticsConfiguration_statistics = Lens.lens (\ColumnStatisticsConfiguration' {statistics} -> statistics) (\s@ColumnStatisticsConfiguration' {} a -> s {statistics = a} :: ColumnStatisticsConfiguration)

instance Data.FromJSON ColumnStatisticsConfiguration where
  parseJSON =
    Data.withObject
      "ColumnStatisticsConfiguration"
      ( \x ->
          ColumnStatisticsConfiguration'
            Prelude.<$> (x Data..:? "Selectors")
            Prelude.<*> (x Data..: "Statistics")
      )

instance
  Prelude.Hashable
    ColumnStatisticsConfiguration
  where
  hashWithSalt _salt ColumnStatisticsConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` selectors
      `Prelude.hashWithSalt` statistics

instance Prelude.NFData ColumnStatisticsConfiguration where
  rnf ColumnStatisticsConfiguration' {..} =
    Prelude.rnf selectors
      `Prelude.seq` Prelude.rnf statistics

instance Data.ToJSON ColumnStatisticsConfiguration where
  toJSON ColumnStatisticsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Selectors" Data..=) Prelude.<$> selectors,
            Prelude.Just ("Statistics" Data..= statistics)
          ]
      )
