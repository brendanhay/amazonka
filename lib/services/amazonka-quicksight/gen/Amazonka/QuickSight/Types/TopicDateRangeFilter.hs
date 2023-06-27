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
-- Module      : Amazonka.QuickSight.Types.TopicDateRangeFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicDateRangeFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TopicRangeFilterConstant

-- | A filter used to restrict data based on a range of dates or times.
--
-- /See:/ 'newTopicDateRangeFilter' smart constructor.
data TopicDateRangeFilter = TopicDateRangeFilter'
  { -- | The constant used in a date range filter.
    constant :: Prelude.Maybe (Data.Sensitive TopicRangeFilterConstant),
    -- | A Boolean value that indicates whether the date range filter should
    -- include the boundary values. If set to true, the filter includes the
    -- start and end dates. If set to false, the filter excludes them.
    inclusive :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicDateRangeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constant', 'topicDateRangeFilter_constant' - The constant used in a date range filter.
--
-- 'inclusive', 'topicDateRangeFilter_inclusive' - A Boolean value that indicates whether the date range filter should
-- include the boundary values. If set to true, the filter includes the
-- start and end dates. If set to false, the filter excludes them.
newTopicDateRangeFilter ::
  TopicDateRangeFilter
newTopicDateRangeFilter =
  TopicDateRangeFilter'
    { constant = Prelude.Nothing,
      inclusive = Prelude.Nothing
    }

-- | The constant used in a date range filter.
topicDateRangeFilter_constant :: Lens.Lens' TopicDateRangeFilter (Prelude.Maybe TopicRangeFilterConstant)
topicDateRangeFilter_constant = Lens.lens (\TopicDateRangeFilter' {constant} -> constant) (\s@TopicDateRangeFilter' {} a -> s {constant = a} :: TopicDateRangeFilter) Prelude.. Lens.mapping Data._Sensitive

-- | A Boolean value that indicates whether the date range filter should
-- include the boundary values. If set to true, the filter includes the
-- start and end dates. If set to false, the filter excludes them.
topicDateRangeFilter_inclusive :: Lens.Lens' TopicDateRangeFilter (Prelude.Maybe Prelude.Bool)
topicDateRangeFilter_inclusive = Lens.lens (\TopicDateRangeFilter' {inclusive} -> inclusive) (\s@TopicDateRangeFilter' {} a -> s {inclusive = a} :: TopicDateRangeFilter)

instance Data.FromJSON TopicDateRangeFilter where
  parseJSON =
    Data.withObject
      "TopicDateRangeFilter"
      ( \x ->
          TopicDateRangeFilter'
            Prelude.<$> (x Data..:? "Constant")
            Prelude.<*> (x Data..:? "Inclusive")
      )

instance Prelude.Hashable TopicDateRangeFilter where
  hashWithSalt _salt TopicDateRangeFilter' {..} =
    _salt
      `Prelude.hashWithSalt` constant
      `Prelude.hashWithSalt` inclusive

instance Prelude.NFData TopicDateRangeFilter where
  rnf TopicDateRangeFilter' {..} =
    Prelude.rnf constant
      `Prelude.seq` Prelude.rnf inclusive

instance Data.ToJSON TopicDateRangeFilter where
  toJSON TopicDateRangeFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Constant" Data..=) Prelude.<$> constant,
            ("Inclusive" Data..=) Prelude.<$> inclusive
          ]
      )
