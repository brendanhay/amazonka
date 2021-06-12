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
-- Module      : Network.AWS.GuardDuty.Types.UsageDataSourceResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UsageDataSourceResult where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types.DataSource
import Network.AWS.GuardDuty.Types.Total
import qualified Network.AWS.Lens as Lens

-- | Contains information on the result of usage based on data source type.
--
-- /See:/ 'newUsageDataSourceResult' smart constructor.
data UsageDataSourceResult = UsageDataSourceResult'
  { -- | The data source type that generated usage.
    dataSource :: Core.Maybe DataSource,
    -- | Represents the total of usage for the specified data source.
    total :: Core.Maybe Total
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UsageDataSourceResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSource', 'usageDataSourceResult_dataSource' - The data source type that generated usage.
--
-- 'total', 'usageDataSourceResult_total' - Represents the total of usage for the specified data source.
newUsageDataSourceResult ::
  UsageDataSourceResult
newUsageDataSourceResult =
  UsageDataSourceResult'
    { dataSource = Core.Nothing,
      total = Core.Nothing
    }

-- | The data source type that generated usage.
usageDataSourceResult_dataSource :: Lens.Lens' UsageDataSourceResult (Core.Maybe DataSource)
usageDataSourceResult_dataSource = Lens.lens (\UsageDataSourceResult' {dataSource} -> dataSource) (\s@UsageDataSourceResult' {} a -> s {dataSource = a} :: UsageDataSourceResult)

-- | Represents the total of usage for the specified data source.
usageDataSourceResult_total :: Lens.Lens' UsageDataSourceResult (Core.Maybe Total)
usageDataSourceResult_total = Lens.lens (\UsageDataSourceResult' {total} -> total) (\s@UsageDataSourceResult' {} a -> s {total = a} :: UsageDataSourceResult)

instance Core.FromJSON UsageDataSourceResult where
  parseJSON =
    Core.withObject
      "UsageDataSourceResult"
      ( \x ->
          UsageDataSourceResult'
            Core.<$> (x Core..:? "dataSource")
            Core.<*> (x Core..:? "total")
      )

instance Core.Hashable UsageDataSourceResult

instance Core.NFData UsageDataSourceResult
