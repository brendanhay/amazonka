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
-- Module      : Amazonka.LookoutMetrics.Types.AlertFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AlertFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.DimensionFilter
import qualified Amazonka.Prelude as Prelude

-- | The configuration of the alert filters.
--
-- /See:/ 'newAlertFilters' smart constructor.
data AlertFilters = AlertFilters'
  { -- | The list of DimensionFilter objects that are used for dimension-based
    -- filtering.
    dimensionFilterList :: Prelude.Maybe (Prelude.NonEmpty DimensionFilter),
    -- | The list of measures that you want to get alerts for.
    metricList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlertFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionFilterList', 'alertFilters_dimensionFilterList' - The list of DimensionFilter objects that are used for dimension-based
-- filtering.
--
-- 'metricList', 'alertFilters_metricList' - The list of measures that you want to get alerts for.
newAlertFilters ::
  AlertFilters
newAlertFilters =
  AlertFilters'
    { dimensionFilterList =
        Prelude.Nothing,
      metricList = Prelude.Nothing
    }

-- | The list of DimensionFilter objects that are used for dimension-based
-- filtering.
alertFilters_dimensionFilterList :: Lens.Lens' AlertFilters (Prelude.Maybe (Prelude.NonEmpty DimensionFilter))
alertFilters_dimensionFilterList = Lens.lens (\AlertFilters' {dimensionFilterList} -> dimensionFilterList) (\s@AlertFilters' {} a -> s {dimensionFilterList = a} :: AlertFilters) Prelude.. Lens.mapping Lens.coerced

-- | The list of measures that you want to get alerts for.
alertFilters_metricList :: Lens.Lens' AlertFilters (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
alertFilters_metricList = Lens.lens (\AlertFilters' {metricList} -> metricList) (\s@AlertFilters' {} a -> s {metricList = a} :: AlertFilters) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AlertFilters where
  parseJSON =
    Data.withObject
      "AlertFilters"
      ( \x ->
          AlertFilters'
            Prelude.<$> (x Data..:? "DimensionFilterList")
            Prelude.<*> (x Data..:? "MetricList")
      )

instance Prelude.Hashable AlertFilters where
  hashWithSalt _salt AlertFilters' {..} =
    _salt `Prelude.hashWithSalt` dimensionFilterList
      `Prelude.hashWithSalt` metricList

instance Prelude.NFData AlertFilters where
  rnf AlertFilters' {..} =
    Prelude.rnf dimensionFilterList
      `Prelude.seq` Prelude.rnf metricList

instance Data.ToJSON AlertFilters where
  toJSON AlertFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DimensionFilterList" Data..=)
              Prelude.<$> dimensionFilterList,
            ("MetricList" Data..=) Prelude.<$> metricList
          ]
      )
