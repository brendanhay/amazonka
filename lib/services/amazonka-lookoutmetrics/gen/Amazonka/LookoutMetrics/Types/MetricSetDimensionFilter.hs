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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.MetricSetDimensionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | The list of filters that you are applying.
    filterList :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | The dimension that you want to filter on.
    name :: Prelude.Maybe Prelude.Text
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
-- 'filterList', 'metricSetDimensionFilter_filterList' - The list of filters that you are applying.
--
-- 'name', 'metricSetDimensionFilter_name' - The dimension that you want to filter on.
newMetricSetDimensionFilter ::
  MetricSetDimensionFilter
newMetricSetDimensionFilter =
  MetricSetDimensionFilter'
    { filterList =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The list of filters that you are applying.
metricSetDimensionFilter_filterList :: Lens.Lens' MetricSetDimensionFilter (Prelude.Maybe (Prelude.NonEmpty Filter))
metricSetDimensionFilter_filterList = Lens.lens (\MetricSetDimensionFilter' {filterList} -> filterList) (\s@MetricSetDimensionFilter' {} a -> s {filterList = a} :: MetricSetDimensionFilter) Prelude.. Lens.mapping Lens.coerced

-- | The dimension that you want to filter on.
metricSetDimensionFilter_name :: Lens.Lens' MetricSetDimensionFilter (Prelude.Maybe Prelude.Text)
metricSetDimensionFilter_name = Lens.lens (\MetricSetDimensionFilter' {name} -> name) (\s@MetricSetDimensionFilter' {} a -> s {name = a} :: MetricSetDimensionFilter)

instance Data.FromJSON MetricSetDimensionFilter where
  parseJSON =
    Data.withObject
      "MetricSetDimensionFilter"
      ( \x ->
          MetricSetDimensionFilter'
            Prelude.<$> (x Data..:? "FilterList")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable MetricSetDimensionFilter where
  hashWithSalt _salt MetricSetDimensionFilter' {..} =
    _salt `Prelude.hashWithSalt` filterList
      `Prelude.hashWithSalt` name

instance Prelude.NFData MetricSetDimensionFilter where
  rnf MetricSetDimensionFilter' {..} =
    Prelude.rnf filterList
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON MetricSetDimensionFilter where
  toJSON MetricSetDimensionFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FilterList" Data..=) Prelude.<$> filterList,
            ("Name" Data..=) Prelude.<$> name
          ]
      )
