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
-- Module      : Amazonka.QuickSight.Types.TopicNumericRangeFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicNumericRangeFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NamedFilterAggType
import Amazonka.QuickSight.Types.TopicRangeFilterConstant

-- | A filter that filters topics based on the value of a numeric field. The
-- filter includes only topics whose numeric field value falls within the
-- specified range.
--
-- /See:/ 'newTopicNumericRangeFilter' smart constructor.
data TopicNumericRangeFilter = TopicNumericRangeFilter'
  { -- | An aggregation function that specifies how to calculate the value of a
    -- numeric field for a topic, Valid values for this structure are
    -- @NO_AGGREGATION@, @SUM@, @AVERAGE@, @COUNT@, @DISTINCT_COUNT@, @MAX@,
    -- @MEDIAN@, @MIN@, @STDEV@, @STDEVP@, @VAR@, and @VARP@.
    aggregation :: Prelude.Maybe NamedFilterAggType,
    -- | The constant used in a numeric range filter.
    constant :: Prelude.Maybe (Data.Sensitive TopicRangeFilterConstant),
    -- | A Boolean value that indicates whether the endpoints of the numeric
    -- range are included in the filter. If set to true, topics whose numeric
    -- field value is equal to the endpoint values will be included in the
    -- filter. If set to false, topics whose numeric field value is equal to
    -- the endpoint values will be excluded from the filter.
    inclusive :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicNumericRangeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregation', 'topicNumericRangeFilter_aggregation' - An aggregation function that specifies how to calculate the value of a
-- numeric field for a topic, Valid values for this structure are
-- @NO_AGGREGATION@, @SUM@, @AVERAGE@, @COUNT@, @DISTINCT_COUNT@, @MAX@,
-- @MEDIAN@, @MIN@, @STDEV@, @STDEVP@, @VAR@, and @VARP@.
--
-- 'constant', 'topicNumericRangeFilter_constant' - The constant used in a numeric range filter.
--
-- 'inclusive', 'topicNumericRangeFilter_inclusive' - A Boolean value that indicates whether the endpoints of the numeric
-- range are included in the filter. If set to true, topics whose numeric
-- field value is equal to the endpoint values will be included in the
-- filter. If set to false, topics whose numeric field value is equal to
-- the endpoint values will be excluded from the filter.
newTopicNumericRangeFilter ::
  TopicNumericRangeFilter
newTopicNumericRangeFilter =
  TopicNumericRangeFilter'
    { aggregation =
        Prelude.Nothing,
      constant = Prelude.Nothing,
      inclusive = Prelude.Nothing
    }

-- | An aggregation function that specifies how to calculate the value of a
-- numeric field for a topic, Valid values for this structure are
-- @NO_AGGREGATION@, @SUM@, @AVERAGE@, @COUNT@, @DISTINCT_COUNT@, @MAX@,
-- @MEDIAN@, @MIN@, @STDEV@, @STDEVP@, @VAR@, and @VARP@.
topicNumericRangeFilter_aggregation :: Lens.Lens' TopicNumericRangeFilter (Prelude.Maybe NamedFilterAggType)
topicNumericRangeFilter_aggregation = Lens.lens (\TopicNumericRangeFilter' {aggregation} -> aggregation) (\s@TopicNumericRangeFilter' {} a -> s {aggregation = a} :: TopicNumericRangeFilter)

-- | The constant used in a numeric range filter.
topicNumericRangeFilter_constant :: Lens.Lens' TopicNumericRangeFilter (Prelude.Maybe TopicRangeFilterConstant)
topicNumericRangeFilter_constant = Lens.lens (\TopicNumericRangeFilter' {constant} -> constant) (\s@TopicNumericRangeFilter' {} a -> s {constant = a} :: TopicNumericRangeFilter) Prelude.. Lens.mapping Data._Sensitive

-- | A Boolean value that indicates whether the endpoints of the numeric
-- range are included in the filter. If set to true, topics whose numeric
-- field value is equal to the endpoint values will be included in the
-- filter. If set to false, topics whose numeric field value is equal to
-- the endpoint values will be excluded from the filter.
topicNumericRangeFilter_inclusive :: Lens.Lens' TopicNumericRangeFilter (Prelude.Maybe Prelude.Bool)
topicNumericRangeFilter_inclusive = Lens.lens (\TopicNumericRangeFilter' {inclusive} -> inclusive) (\s@TopicNumericRangeFilter' {} a -> s {inclusive = a} :: TopicNumericRangeFilter)

instance Data.FromJSON TopicNumericRangeFilter where
  parseJSON =
    Data.withObject
      "TopicNumericRangeFilter"
      ( \x ->
          TopicNumericRangeFilter'
            Prelude.<$> (x Data..:? "Aggregation")
            Prelude.<*> (x Data..:? "Constant")
            Prelude.<*> (x Data..:? "Inclusive")
      )

instance Prelude.Hashable TopicNumericRangeFilter where
  hashWithSalt _salt TopicNumericRangeFilter' {..} =
    _salt
      `Prelude.hashWithSalt` aggregation
      `Prelude.hashWithSalt` constant
      `Prelude.hashWithSalt` inclusive

instance Prelude.NFData TopicNumericRangeFilter where
  rnf TopicNumericRangeFilter' {..} =
    Prelude.rnf aggregation
      `Prelude.seq` Prelude.rnf constant
      `Prelude.seq` Prelude.rnf inclusive

instance Data.ToJSON TopicNumericRangeFilter where
  toJSON TopicNumericRangeFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Aggregation" Data..=) Prelude.<$> aggregation,
            ("Constant" Data..=) Prelude.<$> constant,
            ("Inclusive" Data..=) Prelude.<$> inclusive
          ]
      )
