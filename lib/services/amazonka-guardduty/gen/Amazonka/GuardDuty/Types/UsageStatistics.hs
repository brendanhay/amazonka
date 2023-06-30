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
-- Module      : Amazonka.GuardDuty.Types.UsageStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.UsageStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.UsageAccountResult
import Amazonka.GuardDuty.Types.UsageDataSourceResult
import Amazonka.GuardDuty.Types.UsageResourceResult
import qualified Amazonka.Prelude as Prelude

-- | Contains the result of GuardDuty usage. If a UsageStatisticType is
-- provided the result for other types will be null.
--
-- /See:/ 'newUsageStatistics' smart constructor.
data UsageStatistics = UsageStatistics'
  { -- | The usage statistic sum organized by account ID.
    sumByAccount :: Prelude.Maybe [UsageAccountResult],
    -- | The usage statistic sum organized by on data source.
    sumByDataSource :: Prelude.Maybe [UsageDataSourceResult],
    -- | The usage statistic sum organized by resource.
    sumByResource :: Prelude.Maybe [UsageResourceResult],
    -- | Lists the top 50 resources that have generated the most GuardDuty usage,
    -- in order from most to least expensive.
    topResources :: Prelude.Maybe [UsageResourceResult]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsageStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sumByAccount', 'usageStatistics_sumByAccount' - The usage statistic sum organized by account ID.
--
-- 'sumByDataSource', 'usageStatistics_sumByDataSource' - The usage statistic sum organized by on data source.
--
-- 'sumByResource', 'usageStatistics_sumByResource' - The usage statistic sum organized by resource.
--
-- 'topResources', 'usageStatistics_topResources' - Lists the top 50 resources that have generated the most GuardDuty usage,
-- in order from most to least expensive.
newUsageStatistics ::
  UsageStatistics
newUsageStatistics =
  UsageStatistics'
    { sumByAccount = Prelude.Nothing,
      sumByDataSource = Prelude.Nothing,
      sumByResource = Prelude.Nothing,
      topResources = Prelude.Nothing
    }

-- | The usage statistic sum organized by account ID.
usageStatistics_sumByAccount :: Lens.Lens' UsageStatistics (Prelude.Maybe [UsageAccountResult])
usageStatistics_sumByAccount = Lens.lens (\UsageStatistics' {sumByAccount} -> sumByAccount) (\s@UsageStatistics' {} a -> s {sumByAccount = a} :: UsageStatistics) Prelude.. Lens.mapping Lens.coerced

-- | The usage statistic sum organized by on data source.
usageStatistics_sumByDataSource :: Lens.Lens' UsageStatistics (Prelude.Maybe [UsageDataSourceResult])
usageStatistics_sumByDataSource = Lens.lens (\UsageStatistics' {sumByDataSource} -> sumByDataSource) (\s@UsageStatistics' {} a -> s {sumByDataSource = a} :: UsageStatistics) Prelude.. Lens.mapping Lens.coerced

-- | The usage statistic sum organized by resource.
usageStatistics_sumByResource :: Lens.Lens' UsageStatistics (Prelude.Maybe [UsageResourceResult])
usageStatistics_sumByResource = Lens.lens (\UsageStatistics' {sumByResource} -> sumByResource) (\s@UsageStatistics' {} a -> s {sumByResource = a} :: UsageStatistics) Prelude.. Lens.mapping Lens.coerced

-- | Lists the top 50 resources that have generated the most GuardDuty usage,
-- in order from most to least expensive.
usageStatistics_topResources :: Lens.Lens' UsageStatistics (Prelude.Maybe [UsageResourceResult])
usageStatistics_topResources = Lens.lens (\UsageStatistics' {topResources} -> topResources) (\s@UsageStatistics' {} a -> s {topResources = a} :: UsageStatistics) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON UsageStatistics where
  parseJSON =
    Data.withObject
      "UsageStatistics"
      ( \x ->
          UsageStatistics'
            Prelude.<$> (x Data..:? "sumByAccount" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "sumByDataSource"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "sumByResource" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "topResources" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable UsageStatistics where
  hashWithSalt _salt UsageStatistics' {..} =
    _salt
      `Prelude.hashWithSalt` sumByAccount
      `Prelude.hashWithSalt` sumByDataSource
      `Prelude.hashWithSalt` sumByResource
      `Prelude.hashWithSalt` topResources

instance Prelude.NFData UsageStatistics where
  rnf UsageStatistics' {..} =
    Prelude.rnf sumByAccount
      `Prelude.seq` Prelude.rnf sumByDataSource
      `Prelude.seq` Prelude.rnf sumByResource
      `Prelude.seq` Prelude.rnf topResources
