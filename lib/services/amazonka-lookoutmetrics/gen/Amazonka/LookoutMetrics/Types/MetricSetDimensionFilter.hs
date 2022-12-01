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
-- Module      : Amazonka.LookoutMetrics.Types.MetricSetDimensionFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.MetricSetDimensionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types.Filter
import qualified Amazonka.Prelude as Prelude

-- | Describes a list of filters for choosing a subset of dimension values.
-- Each filter consists of the dimension and one of its values that you
-- want to include. When multiple dimensions or values are specified, the
-- dimensions are joined with an AND operation and the values are joined
-- with an OR operation.
--
-- /See:/ 'newMetricSetDimensionFilter' smart constructor.
data MetricSetDimensionFilter = MetricSetDimensionFilter'
  { -- | The dimension that you want to filter on.
    name :: Prelude.Maybe Prelude.Text,
    -- | The list of filters that you are applying.
    filterList :: Prelude.Maybe (Prelude.NonEmpty Filter)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricSetDimensionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'metricSetDimensionFilter_name' - The dimension that you want to filter on.
--
-- 'filterList', 'metricSetDimensionFilter_filterList' - The list of filters that you are applying.
newMetricSetDimensionFilter ::
  MetricSetDimensionFilter
newMetricSetDimensionFilter =
  MetricSetDimensionFilter'
    { name = Prelude.Nothing,
      filterList = Prelude.Nothing
    }

-- | The dimension that you want to filter on.
metricSetDimensionFilter_name :: Lens.Lens' MetricSetDimensionFilter (Prelude.Maybe Prelude.Text)
metricSetDimensionFilter_name = Lens.lens (\MetricSetDimensionFilter' {name} -> name) (\s@MetricSetDimensionFilter' {} a -> s {name = a} :: MetricSetDimensionFilter)

-- | The list of filters that you are applying.
metricSetDimensionFilter_filterList :: Lens.Lens' MetricSetDimensionFilter (Prelude.Maybe (Prelude.NonEmpty Filter))
metricSetDimensionFilter_filterList = Lens.lens (\MetricSetDimensionFilter' {filterList} -> filterList) (\s@MetricSetDimensionFilter' {} a -> s {filterList = a} :: MetricSetDimensionFilter) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON MetricSetDimensionFilter where
  parseJSON =
    Core.withObject
      "MetricSetDimensionFilter"
      ( \x ->
          MetricSetDimensionFilter'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "FilterList")
      )

instance Prelude.Hashable MetricSetDimensionFilter where
  hashWithSalt _salt MetricSetDimensionFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` filterList

instance Prelude.NFData MetricSetDimensionFilter where
  rnf MetricSetDimensionFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf filterList

instance Core.ToJSON MetricSetDimensionFilter where
  toJSON MetricSetDimensionFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("FilterList" Core..=) Prelude.<$> filterList
          ]
      )
