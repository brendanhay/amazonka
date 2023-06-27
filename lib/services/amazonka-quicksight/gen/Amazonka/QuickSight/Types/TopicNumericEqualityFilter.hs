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
-- Module      : Amazonka.QuickSight.Types.TopicNumericEqualityFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicNumericEqualityFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NamedFilterAggType
import Amazonka.QuickSight.Types.TopicSingularFilterConstant

-- | A filter that filters topics based on the value of a numeric field. The
-- filter includes only topics whose numeric field value matches the
-- specified value.
--
-- /See:/ 'newTopicNumericEqualityFilter' smart constructor.
data TopicNumericEqualityFilter = TopicNumericEqualityFilter'
  { -- | An aggregation function that specifies how to calculate the value of a
    -- numeric field for a topic. Valid values for this structure are
    -- @NO_AGGREGATION@, @SUM@, @AVERAGE@, @COUNT@, @DISTINCT_COUNT@, @MAX@,
    -- @MEDIAN@, @MIN@, @STDEV@, @STDEVP@, @VAR@, and @VARP@.
    aggregation :: Prelude.Maybe NamedFilterAggType,
    -- | The constant used in a numeric equality filter.
    constant :: Prelude.Maybe (Data.Sensitive TopicSingularFilterConstant)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicNumericEqualityFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregation', 'topicNumericEqualityFilter_aggregation' - An aggregation function that specifies how to calculate the value of a
-- numeric field for a topic. Valid values for this structure are
-- @NO_AGGREGATION@, @SUM@, @AVERAGE@, @COUNT@, @DISTINCT_COUNT@, @MAX@,
-- @MEDIAN@, @MIN@, @STDEV@, @STDEVP@, @VAR@, and @VARP@.
--
-- 'constant', 'topicNumericEqualityFilter_constant' - The constant used in a numeric equality filter.
newTopicNumericEqualityFilter ::
  TopicNumericEqualityFilter
newTopicNumericEqualityFilter =
  TopicNumericEqualityFilter'
    { aggregation =
        Prelude.Nothing,
      constant = Prelude.Nothing
    }

-- | An aggregation function that specifies how to calculate the value of a
-- numeric field for a topic. Valid values for this structure are
-- @NO_AGGREGATION@, @SUM@, @AVERAGE@, @COUNT@, @DISTINCT_COUNT@, @MAX@,
-- @MEDIAN@, @MIN@, @STDEV@, @STDEVP@, @VAR@, and @VARP@.
topicNumericEqualityFilter_aggregation :: Lens.Lens' TopicNumericEqualityFilter (Prelude.Maybe NamedFilterAggType)
topicNumericEqualityFilter_aggregation = Lens.lens (\TopicNumericEqualityFilter' {aggregation} -> aggregation) (\s@TopicNumericEqualityFilter' {} a -> s {aggregation = a} :: TopicNumericEqualityFilter)

-- | The constant used in a numeric equality filter.
topicNumericEqualityFilter_constant :: Lens.Lens' TopicNumericEqualityFilter (Prelude.Maybe TopicSingularFilterConstant)
topicNumericEqualityFilter_constant = Lens.lens (\TopicNumericEqualityFilter' {constant} -> constant) (\s@TopicNumericEqualityFilter' {} a -> s {constant = a} :: TopicNumericEqualityFilter) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON TopicNumericEqualityFilter where
  parseJSON =
    Data.withObject
      "TopicNumericEqualityFilter"
      ( \x ->
          TopicNumericEqualityFilter'
            Prelude.<$> (x Data..:? "Aggregation")
            Prelude.<*> (x Data..:? "Constant")
      )

instance Prelude.Hashable TopicNumericEqualityFilter where
  hashWithSalt _salt TopicNumericEqualityFilter' {..} =
    _salt
      `Prelude.hashWithSalt` aggregation
      `Prelude.hashWithSalt` constant

instance Prelude.NFData TopicNumericEqualityFilter where
  rnf TopicNumericEqualityFilter' {..} =
    Prelude.rnf aggregation
      `Prelude.seq` Prelude.rnf constant

instance Data.ToJSON TopicNumericEqualityFilter where
  toJSON TopicNumericEqualityFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Aggregation" Data..=) Prelude.<$> aggregation,
            ("Constant" Data..=) Prelude.<$> constant
          ]
      )
