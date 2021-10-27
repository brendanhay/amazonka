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
-- Module      : Network.AWS.DataBrew.Types.StatisticsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataBrew.Types.StatisticsConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.DataBrew.Types.StatisticOverride
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration of evaluations for a profile job. This configuration can
-- be used to select evaluations and override the parameters of selected
-- evaluations.
--
-- /See:/ 'newStatisticsConfiguration' smart constructor.
data StatisticsConfiguration = StatisticsConfiguration'
  { -- | List of overrides for evaluations.
    overrides :: Prelude.Maybe (Prelude.NonEmpty StatisticOverride),
    -- | List of included evaluations. When the list is undefined, all supported
    -- evaluations will be included.
    includedStatistics :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
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
-- 'overrides', 'statisticsConfiguration_overrides' - List of overrides for evaluations.
--
-- 'includedStatistics', 'statisticsConfiguration_includedStatistics' - List of included evaluations. When the list is undefined, all supported
-- evaluations will be included.
newStatisticsConfiguration ::
  StatisticsConfiguration
newStatisticsConfiguration =
  StatisticsConfiguration'
    { overrides =
        Prelude.Nothing,
      includedStatistics = Prelude.Nothing
    }

-- | List of overrides for evaluations.
statisticsConfiguration_overrides :: Lens.Lens' StatisticsConfiguration (Prelude.Maybe (Prelude.NonEmpty StatisticOverride))
statisticsConfiguration_overrides = Lens.lens (\StatisticsConfiguration' {overrides} -> overrides) (\s@StatisticsConfiguration' {} a -> s {overrides = a} :: StatisticsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | List of included evaluations. When the list is undefined, all supported
-- evaluations will be included.
statisticsConfiguration_includedStatistics :: Lens.Lens' StatisticsConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
statisticsConfiguration_includedStatistics = Lens.lens (\StatisticsConfiguration' {includedStatistics} -> includedStatistics) (\s@StatisticsConfiguration' {} a -> s {includedStatistics = a} :: StatisticsConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON StatisticsConfiguration where
  parseJSON =
    Core.withObject
      "StatisticsConfiguration"
      ( \x ->
          StatisticsConfiguration'
            Prelude.<$> (x Core..:? "Overrides")
            Prelude.<*> (x Core..:? "IncludedStatistics")
      )

instance Prelude.Hashable StatisticsConfiguration

instance Prelude.NFData StatisticsConfiguration

instance Core.ToJSON StatisticsConfiguration where
  toJSON StatisticsConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Overrides" Core..=) Prelude.<$> overrides,
            ("IncludedStatistics" Core..=)
              Prelude.<$> includedStatistics
          ]
      )
