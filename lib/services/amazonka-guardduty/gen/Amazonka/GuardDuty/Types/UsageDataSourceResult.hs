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
-- Module      : Amazonka.GuardDuty.Types.UsageDataSourceResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.UsageDataSourceResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.DataSource
import Amazonka.GuardDuty.Types.Total
import qualified Amazonka.Prelude as Prelude

-- | Contains information on the result of usage based on data source type.
--
-- /See:/ 'newUsageDataSourceResult' smart constructor.
data UsageDataSourceResult = UsageDataSourceResult'
  { -- | Represents the total of usage for the specified data source.
    total :: Prelude.Maybe Total,
    -- | The data source type that generated usage.
    dataSource :: Prelude.Maybe DataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsageDataSourceResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'total', 'usageDataSourceResult_total' - Represents the total of usage for the specified data source.
--
-- 'dataSource', 'usageDataSourceResult_dataSource' - The data source type that generated usage.
newUsageDataSourceResult ::
  UsageDataSourceResult
newUsageDataSourceResult =
  UsageDataSourceResult'
    { total = Prelude.Nothing,
      dataSource = Prelude.Nothing
    }

-- | Represents the total of usage for the specified data source.
usageDataSourceResult_total :: Lens.Lens' UsageDataSourceResult (Prelude.Maybe Total)
usageDataSourceResult_total = Lens.lens (\UsageDataSourceResult' {total} -> total) (\s@UsageDataSourceResult' {} a -> s {total = a} :: UsageDataSourceResult)

-- | The data source type that generated usage.
usageDataSourceResult_dataSource :: Lens.Lens' UsageDataSourceResult (Prelude.Maybe DataSource)
usageDataSourceResult_dataSource = Lens.lens (\UsageDataSourceResult' {dataSource} -> dataSource) (\s@UsageDataSourceResult' {} a -> s {dataSource = a} :: UsageDataSourceResult)

instance Core.FromJSON UsageDataSourceResult where
  parseJSON =
    Core.withObject
      "UsageDataSourceResult"
      ( \x ->
          UsageDataSourceResult'
            Prelude.<$> (x Core..:? "total")
            Prelude.<*> (x Core..:? "dataSource")
      )

instance Prelude.Hashable UsageDataSourceResult where
  hashWithSalt _salt UsageDataSourceResult' {..} =
    _salt `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` dataSource

instance Prelude.NFData UsageDataSourceResult where
  rnf UsageDataSourceResult' {..} =
    Prelude.rnf total
      `Prelude.seq` Prelude.rnf dataSource
