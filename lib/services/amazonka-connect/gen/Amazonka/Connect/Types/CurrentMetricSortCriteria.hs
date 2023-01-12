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
-- Module      : Amazonka.Connect.Types.CurrentMetricSortCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.CurrentMetricSortCriteria where

import Amazonka.Connect.Types.CurrentMetricName
import Amazonka.Connect.Types.SortOrder
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The way to sort the resulting response based on metrics. By default
-- resources are sorted based on @AGENTS_ONLINE@, @DESCENDING@. The metric
-- collection is sorted based on the input metrics.
--
-- /See:/ 'newCurrentMetricSortCriteria' smart constructor.
data CurrentMetricSortCriteria = CurrentMetricSortCriteria'
  { sortByMetric :: Prelude.Maybe CurrentMetricName,
    -- | The way to sort.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CurrentMetricSortCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortByMetric', 'currentMetricSortCriteria_sortByMetric' - Undocumented member.
--
-- 'sortOrder', 'currentMetricSortCriteria_sortOrder' - The way to sort.
newCurrentMetricSortCriteria ::
  CurrentMetricSortCriteria
newCurrentMetricSortCriteria =
  CurrentMetricSortCriteria'
    { sortByMetric =
        Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | Undocumented member.
currentMetricSortCriteria_sortByMetric :: Lens.Lens' CurrentMetricSortCriteria (Prelude.Maybe CurrentMetricName)
currentMetricSortCriteria_sortByMetric = Lens.lens (\CurrentMetricSortCriteria' {sortByMetric} -> sortByMetric) (\s@CurrentMetricSortCriteria' {} a -> s {sortByMetric = a} :: CurrentMetricSortCriteria)

-- | The way to sort.
currentMetricSortCriteria_sortOrder :: Lens.Lens' CurrentMetricSortCriteria (Prelude.Maybe SortOrder)
currentMetricSortCriteria_sortOrder = Lens.lens (\CurrentMetricSortCriteria' {sortOrder} -> sortOrder) (\s@CurrentMetricSortCriteria' {} a -> s {sortOrder = a} :: CurrentMetricSortCriteria)

instance Prelude.Hashable CurrentMetricSortCriteria where
  hashWithSalt _salt CurrentMetricSortCriteria' {..} =
    _salt `Prelude.hashWithSalt` sortByMetric
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData CurrentMetricSortCriteria where
  rnf CurrentMetricSortCriteria' {..} =
    Prelude.rnf sortByMetric
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToJSON CurrentMetricSortCriteria where
  toJSON CurrentMetricSortCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SortByMetric" Data..=) Prelude.<$> sortByMetric,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )
