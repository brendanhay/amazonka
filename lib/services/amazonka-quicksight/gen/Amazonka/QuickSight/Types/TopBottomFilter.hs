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
-- Module      : Amazonka.QuickSight.Types.TopBottomFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopBottomFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AggregationSortConfiguration
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.TimeGranularity

-- | A @TopBottomFilter@ filters values that are at the top or the bottom.
--
-- /See:/ 'newTopBottomFilter' smart constructor.
data TopBottomFilter = TopBottomFilter'
  { -- | The number of items to include in the top bottom filter results.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The parameter whose value should be used for the filter value.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | The level of time precision that is used to aggregate @DateTime@ values.
    timeGranularity :: Prelude.Maybe TimeGranularity,
    -- | An identifier that uniquely identifies a filter within a dashboard,
    -- analysis, or template.
    filterId :: Prelude.Text,
    -- | The column that the filter is applied to.
    column :: ColumnIdentifier,
    -- | The aggregation and sort configuration of the top bottom filter.
    aggregationSortConfigurations :: [AggregationSortConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopBottomFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'topBottomFilter_limit' - The number of items to include in the top bottom filter results.
--
-- 'parameterName', 'topBottomFilter_parameterName' - The parameter whose value should be used for the filter value.
--
-- 'timeGranularity', 'topBottomFilter_timeGranularity' - The level of time precision that is used to aggregate @DateTime@ values.
--
-- 'filterId', 'topBottomFilter_filterId' - An identifier that uniquely identifies a filter within a dashboard,
-- analysis, or template.
--
-- 'column', 'topBottomFilter_column' - The column that the filter is applied to.
--
-- 'aggregationSortConfigurations', 'topBottomFilter_aggregationSortConfigurations' - The aggregation and sort configuration of the top bottom filter.
newTopBottomFilter ::
  -- | 'filterId'
  Prelude.Text ->
  -- | 'column'
  ColumnIdentifier ->
  TopBottomFilter
newTopBottomFilter pFilterId_ pColumn_ =
  TopBottomFilter'
    { limit = Prelude.Nothing,
      parameterName = Prelude.Nothing,
      timeGranularity = Prelude.Nothing,
      filterId = pFilterId_,
      column = pColumn_,
      aggregationSortConfigurations = Prelude.mempty
    }

-- | The number of items to include in the top bottom filter results.
topBottomFilter_limit :: Lens.Lens' TopBottomFilter (Prelude.Maybe Prelude.Int)
topBottomFilter_limit = Lens.lens (\TopBottomFilter' {limit} -> limit) (\s@TopBottomFilter' {} a -> s {limit = a} :: TopBottomFilter)

-- | The parameter whose value should be used for the filter value.
topBottomFilter_parameterName :: Lens.Lens' TopBottomFilter (Prelude.Maybe Prelude.Text)
topBottomFilter_parameterName = Lens.lens (\TopBottomFilter' {parameterName} -> parameterName) (\s@TopBottomFilter' {} a -> s {parameterName = a} :: TopBottomFilter)

-- | The level of time precision that is used to aggregate @DateTime@ values.
topBottomFilter_timeGranularity :: Lens.Lens' TopBottomFilter (Prelude.Maybe TimeGranularity)
topBottomFilter_timeGranularity = Lens.lens (\TopBottomFilter' {timeGranularity} -> timeGranularity) (\s@TopBottomFilter' {} a -> s {timeGranularity = a} :: TopBottomFilter)

-- | An identifier that uniquely identifies a filter within a dashboard,
-- analysis, or template.
topBottomFilter_filterId :: Lens.Lens' TopBottomFilter Prelude.Text
topBottomFilter_filterId = Lens.lens (\TopBottomFilter' {filterId} -> filterId) (\s@TopBottomFilter' {} a -> s {filterId = a} :: TopBottomFilter)

-- | The column that the filter is applied to.
topBottomFilter_column :: Lens.Lens' TopBottomFilter ColumnIdentifier
topBottomFilter_column = Lens.lens (\TopBottomFilter' {column} -> column) (\s@TopBottomFilter' {} a -> s {column = a} :: TopBottomFilter)

-- | The aggregation and sort configuration of the top bottom filter.
topBottomFilter_aggregationSortConfigurations :: Lens.Lens' TopBottomFilter [AggregationSortConfiguration]
topBottomFilter_aggregationSortConfigurations = Lens.lens (\TopBottomFilter' {aggregationSortConfigurations} -> aggregationSortConfigurations) (\s@TopBottomFilter' {} a -> s {aggregationSortConfigurations = a} :: TopBottomFilter) Prelude.. Lens.coerced

instance Data.FromJSON TopBottomFilter where
  parseJSON =
    Data.withObject
      "TopBottomFilter"
      ( \x ->
          TopBottomFilter'
            Prelude.<$> (x Data..:? "Limit")
            Prelude.<*> (x Data..:? "ParameterName")
            Prelude.<*> (x Data..:? "TimeGranularity")
            Prelude.<*> (x Data..: "FilterId")
            Prelude.<*> (x Data..: "Column")
            Prelude.<*> ( x Data..:? "AggregationSortConfigurations"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TopBottomFilter where
  hashWithSalt _salt TopBottomFilter' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` parameterName
      `Prelude.hashWithSalt` timeGranularity
      `Prelude.hashWithSalt` filterId
      `Prelude.hashWithSalt` column
      `Prelude.hashWithSalt` aggregationSortConfigurations

instance Prelude.NFData TopBottomFilter where
  rnf TopBottomFilter' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf parameterName
      `Prelude.seq` Prelude.rnf timeGranularity
      `Prelude.seq` Prelude.rnf filterId
      `Prelude.seq` Prelude.rnf column
      `Prelude.seq` Prelude.rnf aggregationSortConfigurations

instance Data.ToJSON TopBottomFilter where
  toJSON TopBottomFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("ParameterName" Data..=) Prelude.<$> parameterName,
            ("TimeGranularity" Data..=)
              Prelude.<$> timeGranularity,
            Prelude.Just ("FilterId" Data..= filterId),
            Prelude.Just ("Column" Data..= column),
            Prelude.Just
              ( "AggregationSortConfigurations"
                  Data..= aggregationSortConfigurations
              )
          ]
      )
