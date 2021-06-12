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
-- Module      : Network.AWS.GuardDuty.Types.UsageStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UsageStatistics where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types.UsageAccountResult
import Network.AWS.GuardDuty.Types.UsageDataSourceResult
import Network.AWS.GuardDuty.Types.UsageResourceResult
import qualified Network.AWS.Lens as Lens

-- | Contains the result of GuardDuty usage. If a UsageStatisticType is
-- provided the result for other types will be null.
--
-- /See:/ 'newUsageStatistics' smart constructor.
data UsageStatistics = UsageStatistics'
  { -- | The usage statistic sum organized by on data source.
    sumByDataSource :: Core.Maybe [UsageDataSourceResult],
    -- | Lists the top 50 resources that have generated the most GuardDuty usage,
    -- in order from most to least expensive.
    topResources :: Core.Maybe [UsageResourceResult],
    -- | The usage statistic sum organized by account ID.
    sumByAccount :: Core.Maybe [UsageAccountResult],
    -- | The usage statistic sum organized by resource.
    sumByResource :: Core.Maybe [UsageResourceResult]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UsageStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sumByDataSource', 'usageStatistics_sumByDataSource' - The usage statistic sum organized by on data source.
--
-- 'topResources', 'usageStatistics_topResources' - Lists the top 50 resources that have generated the most GuardDuty usage,
-- in order from most to least expensive.
--
-- 'sumByAccount', 'usageStatistics_sumByAccount' - The usage statistic sum organized by account ID.
--
-- 'sumByResource', 'usageStatistics_sumByResource' - The usage statistic sum organized by resource.
newUsageStatistics ::
  UsageStatistics
newUsageStatistics =
  UsageStatistics'
    { sumByDataSource = Core.Nothing,
      topResources = Core.Nothing,
      sumByAccount = Core.Nothing,
      sumByResource = Core.Nothing
    }

-- | The usage statistic sum organized by on data source.
usageStatistics_sumByDataSource :: Lens.Lens' UsageStatistics (Core.Maybe [UsageDataSourceResult])
usageStatistics_sumByDataSource = Lens.lens (\UsageStatistics' {sumByDataSource} -> sumByDataSource) (\s@UsageStatistics' {} a -> s {sumByDataSource = a} :: UsageStatistics) Core.. Lens.mapping Lens._Coerce

-- | Lists the top 50 resources that have generated the most GuardDuty usage,
-- in order from most to least expensive.
usageStatistics_topResources :: Lens.Lens' UsageStatistics (Core.Maybe [UsageResourceResult])
usageStatistics_topResources = Lens.lens (\UsageStatistics' {topResources} -> topResources) (\s@UsageStatistics' {} a -> s {topResources = a} :: UsageStatistics) Core.. Lens.mapping Lens._Coerce

-- | The usage statistic sum organized by account ID.
usageStatistics_sumByAccount :: Lens.Lens' UsageStatistics (Core.Maybe [UsageAccountResult])
usageStatistics_sumByAccount = Lens.lens (\UsageStatistics' {sumByAccount} -> sumByAccount) (\s@UsageStatistics' {} a -> s {sumByAccount = a} :: UsageStatistics) Core.. Lens.mapping Lens._Coerce

-- | The usage statistic sum organized by resource.
usageStatistics_sumByResource :: Lens.Lens' UsageStatistics (Core.Maybe [UsageResourceResult])
usageStatistics_sumByResource = Lens.lens (\UsageStatistics' {sumByResource} -> sumByResource) (\s@UsageStatistics' {} a -> s {sumByResource = a} :: UsageStatistics) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON UsageStatistics where
  parseJSON =
    Core.withObject
      "UsageStatistics"
      ( \x ->
          UsageStatistics'
            Core.<$> (x Core..:? "sumByDataSource" Core..!= Core.mempty)
            Core.<*> (x Core..:? "topResources" Core..!= Core.mempty)
            Core.<*> (x Core..:? "sumByAccount" Core..!= Core.mempty)
            Core.<*> (x Core..:? "sumByResource" Core..!= Core.mempty)
      )

instance Core.Hashable UsageStatistics

instance Core.NFData UsageStatistics
