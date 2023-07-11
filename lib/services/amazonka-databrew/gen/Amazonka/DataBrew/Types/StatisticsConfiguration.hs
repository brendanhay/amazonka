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
-- Module      : Amazonka.DataBrew.Types.StatisticsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.StatisticsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types.StatisticOverride
import qualified Amazonka.Prelude as Prelude

-- | Configuration of evaluations for a profile job. This configuration can
-- be used to select evaluations and override the parameters of selected
-- evaluations.
--
-- /See:/ 'newStatisticsConfiguration' smart constructor.
data StatisticsConfiguration = StatisticsConfiguration'
  { -- | List of included evaluations. When the list is undefined, all supported
    -- evaluations will be included.
    includedStatistics :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | List of overrides for evaluations.
    overrides :: Prelude.Maybe (Prelude.NonEmpty StatisticOverride)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatisticsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includedStatistics', 'statisticsConfiguration_includedStatistics' - List of included evaluations. When the list is undefined, all supported
-- evaluations will be included.
--
-- 'overrides', 'statisticsConfiguration_overrides' - List of overrides for evaluations.
newStatisticsConfiguration ::
  StatisticsConfiguration
newStatisticsConfiguration =
  StatisticsConfiguration'
    { includedStatistics =
        Prelude.Nothing,
      overrides = Prelude.Nothing
    }

-- | List of included evaluations. When the list is undefined, all supported
-- evaluations will be included.
statisticsConfiguration_includedStatistics :: Lens.Lens' StatisticsConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
statisticsConfiguration_includedStatistics = Lens.lens (\StatisticsConfiguration' {includedStatistics} -> includedStatistics) (\s@StatisticsConfiguration' {} a -> s {includedStatistics = a} :: StatisticsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | List of overrides for evaluations.
statisticsConfiguration_overrides :: Lens.Lens' StatisticsConfiguration (Prelude.Maybe (Prelude.NonEmpty StatisticOverride))
statisticsConfiguration_overrides = Lens.lens (\StatisticsConfiguration' {overrides} -> overrides) (\s@StatisticsConfiguration' {} a -> s {overrides = a} :: StatisticsConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON StatisticsConfiguration where
  parseJSON =
    Data.withObject
      "StatisticsConfiguration"
      ( \x ->
          StatisticsConfiguration'
            Prelude.<$> (x Data..:? "IncludedStatistics")
            Prelude.<*> (x Data..:? "Overrides")
      )

instance Prelude.Hashable StatisticsConfiguration where
  hashWithSalt _salt StatisticsConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` includedStatistics
      `Prelude.hashWithSalt` overrides

instance Prelude.NFData StatisticsConfiguration where
  rnf StatisticsConfiguration' {..} =
    Prelude.rnf includedStatistics
      `Prelude.seq` Prelude.rnf overrides

instance Data.ToJSON StatisticsConfiguration where
  toJSON StatisticsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IncludedStatistics" Data..=)
              Prelude.<$> includedStatistics,
            ("Overrides" Data..=) Prelude.<$> overrides
          ]
      )
