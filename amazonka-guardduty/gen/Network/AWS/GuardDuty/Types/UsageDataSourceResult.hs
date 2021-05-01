{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.GuardDuty.Types.DataSource
import Network.AWS.GuardDuty.Types.Total
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information on the result of usage based on data source type.
--
-- /See:/ 'newUsageDataSourceResult' smart constructor.
data UsageDataSourceResult = UsageDataSourceResult'
  { -- | The data source type that generated usage.
    dataSource :: Prelude.Maybe DataSource,
    -- | Represents the total of usage for the specified data source.
    total :: Prelude.Maybe Total
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { dataSource =
        Prelude.Nothing,
      total = Prelude.Nothing
    }

-- | The data source type that generated usage.
usageDataSourceResult_dataSource :: Lens.Lens' UsageDataSourceResult (Prelude.Maybe DataSource)
usageDataSourceResult_dataSource = Lens.lens (\UsageDataSourceResult' {dataSource} -> dataSource) (\s@UsageDataSourceResult' {} a -> s {dataSource = a} :: UsageDataSourceResult)

-- | Represents the total of usage for the specified data source.
usageDataSourceResult_total :: Lens.Lens' UsageDataSourceResult (Prelude.Maybe Total)
usageDataSourceResult_total = Lens.lens (\UsageDataSourceResult' {total} -> total) (\s@UsageDataSourceResult' {} a -> s {total = a} :: UsageDataSourceResult)

instance Prelude.FromJSON UsageDataSourceResult where
  parseJSON =
    Prelude.withObject
      "UsageDataSourceResult"
      ( \x ->
          UsageDataSourceResult'
            Prelude.<$> (x Prelude..:? "dataSource")
            Prelude.<*> (x Prelude..:? "total")
      )

instance Prelude.Hashable UsageDataSourceResult

instance Prelude.NFData UsageDataSourceResult
