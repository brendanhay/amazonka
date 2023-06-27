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
-- Module      : Amazonka.QuickSight.Types.NamedEntityDefinitionMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NamedEntityDefinitionMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NamedEntityAggType

-- | A structure that represents a metric.
--
-- /See:/ 'newNamedEntityDefinitionMetric' smart constructor.
data NamedEntityDefinitionMetric = NamedEntityDefinitionMetric'
  { -- | The aggregation of a named entity. Valid values for this structure are
    -- @SUM@, @MIN@, @MAX@, @COUNT@, @AVERAGE@, @DISTINCT_COUNT@, @STDEV@,
    -- @STDEVP@, @VAR@, @VARP@, @PERCENTILE@, @MEDIAN@, and @CUSTOM@.
    aggregation :: Prelude.Maybe NamedEntityAggType,
    -- | The additional parameters for an aggregation function.
    aggregationFunctionParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NamedEntityDefinitionMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregation', 'namedEntityDefinitionMetric_aggregation' - The aggregation of a named entity. Valid values for this structure are
-- @SUM@, @MIN@, @MAX@, @COUNT@, @AVERAGE@, @DISTINCT_COUNT@, @STDEV@,
-- @STDEVP@, @VAR@, @VARP@, @PERCENTILE@, @MEDIAN@, and @CUSTOM@.
--
-- 'aggregationFunctionParameters', 'namedEntityDefinitionMetric_aggregationFunctionParameters' - The additional parameters for an aggregation function.
newNamedEntityDefinitionMetric ::
  NamedEntityDefinitionMetric
newNamedEntityDefinitionMetric =
  NamedEntityDefinitionMetric'
    { aggregation =
        Prelude.Nothing,
      aggregationFunctionParameters =
        Prelude.Nothing
    }

-- | The aggregation of a named entity. Valid values for this structure are
-- @SUM@, @MIN@, @MAX@, @COUNT@, @AVERAGE@, @DISTINCT_COUNT@, @STDEV@,
-- @STDEVP@, @VAR@, @VARP@, @PERCENTILE@, @MEDIAN@, and @CUSTOM@.
namedEntityDefinitionMetric_aggregation :: Lens.Lens' NamedEntityDefinitionMetric (Prelude.Maybe NamedEntityAggType)
namedEntityDefinitionMetric_aggregation = Lens.lens (\NamedEntityDefinitionMetric' {aggregation} -> aggregation) (\s@NamedEntityDefinitionMetric' {} a -> s {aggregation = a} :: NamedEntityDefinitionMetric)

-- | The additional parameters for an aggregation function.
namedEntityDefinitionMetric_aggregationFunctionParameters :: Lens.Lens' NamedEntityDefinitionMetric (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
namedEntityDefinitionMetric_aggregationFunctionParameters = Lens.lens (\NamedEntityDefinitionMetric' {aggregationFunctionParameters} -> aggregationFunctionParameters) (\s@NamedEntityDefinitionMetric' {} a -> s {aggregationFunctionParameters = a} :: NamedEntityDefinitionMetric) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON NamedEntityDefinitionMetric where
  parseJSON =
    Data.withObject
      "NamedEntityDefinitionMetric"
      ( \x ->
          NamedEntityDefinitionMetric'
            Prelude.<$> (x Data..:? "Aggregation")
            Prelude.<*> ( x
                            Data..:? "AggregationFunctionParameters"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable NamedEntityDefinitionMetric where
  hashWithSalt _salt NamedEntityDefinitionMetric' {..} =
    _salt
      `Prelude.hashWithSalt` aggregation
      `Prelude.hashWithSalt` aggregationFunctionParameters

instance Prelude.NFData NamedEntityDefinitionMetric where
  rnf NamedEntityDefinitionMetric' {..} =
    Prelude.rnf aggregation
      `Prelude.seq` Prelude.rnf aggregationFunctionParameters

instance Data.ToJSON NamedEntityDefinitionMetric where
  toJSON NamedEntityDefinitionMetric' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Aggregation" Data..=) Prelude.<$> aggregation,
            ("AggregationFunctionParameters" Data..=)
              Prelude.<$> aggregationFunctionParameters
          ]
      )
