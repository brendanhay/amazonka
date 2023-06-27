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
-- Module      : Amazonka.QuickSight.Types.TopicRelativeDateFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicRelativeDateFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TopicRelativeDateFilterFunction
import Amazonka.QuickSight.Types.TopicSingularFilterConstant
import Amazonka.QuickSight.Types.TopicTimeGranularity

-- | A structure that represents a relative date filter.
--
-- /See:/ 'newTopicRelativeDateFilter' smart constructor.
data TopicRelativeDateFilter = TopicRelativeDateFilter'
  { -- | The constant used in a relative date filter.
    constant :: Prelude.Maybe (Data.Sensitive TopicSingularFilterConstant),
    -- | The function to be used in a relative date filter to determine the range
    -- of dates to include in the results. Valid values for this structure are
    -- @BEFORE@, @AFTER@, and @BETWEEN@.
    relativeDateFilterFunction :: Prelude.Maybe TopicRelativeDateFilterFunction,
    -- | The level of time precision that is used to aggregate @DateTime@ values.
    timeGranularity :: Prelude.Maybe TopicTimeGranularity
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicRelativeDateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constant', 'topicRelativeDateFilter_constant' - The constant used in a relative date filter.
--
-- 'relativeDateFilterFunction', 'topicRelativeDateFilter_relativeDateFilterFunction' - The function to be used in a relative date filter to determine the range
-- of dates to include in the results. Valid values for this structure are
-- @BEFORE@, @AFTER@, and @BETWEEN@.
--
-- 'timeGranularity', 'topicRelativeDateFilter_timeGranularity' - The level of time precision that is used to aggregate @DateTime@ values.
newTopicRelativeDateFilter ::
  TopicRelativeDateFilter
newTopicRelativeDateFilter =
  TopicRelativeDateFilter'
    { constant =
        Prelude.Nothing,
      relativeDateFilterFunction = Prelude.Nothing,
      timeGranularity = Prelude.Nothing
    }

-- | The constant used in a relative date filter.
topicRelativeDateFilter_constant :: Lens.Lens' TopicRelativeDateFilter (Prelude.Maybe TopicSingularFilterConstant)
topicRelativeDateFilter_constant = Lens.lens (\TopicRelativeDateFilter' {constant} -> constant) (\s@TopicRelativeDateFilter' {} a -> s {constant = a} :: TopicRelativeDateFilter) Prelude.. Lens.mapping Data._Sensitive

-- | The function to be used in a relative date filter to determine the range
-- of dates to include in the results. Valid values for this structure are
-- @BEFORE@, @AFTER@, and @BETWEEN@.
topicRelativeDateFilter_relativeDateFilterFunction :: Lens.Lens' TopicRelativeDateFilter (Prelude.Maybe TopicRelativeDateFilterFunction)
topicRelativeDateFilter_relativeDateFilterFunction = Lens.lens (\TopicRelativeDateFilter' {relativeDateFilterFunction} -> relativeDateFilterFunction) (\s@TopicRelativeDateFilter' {} a -> s {relativeDateFilterFunction = a} :: TopicRelativeDateFilter)

-- | The level of time precision that is used to aggregate @DateTime@ values.
topicRelativeDateFilter_timeGranularity :: Lens.Lens' TopicRelativeDateFilter (Prelude.Maybe TopicTimeGranularity)
topicRelativeDateFilter_timeGranularity = Lens.lens (\TopicRelativeDateFilter' {timeGranularity} -> timeGranularity) (\s@TopicRelativeDateFilter' {} a -> s {timeGranularity = a} :: TopicRelativeDateFilter)

instance Data.FromJSON TopicRelativeDateFilter where
  parseJSON =
    Data.withObject
      "TopicRelativeDateFilter"
      ( \x ->
          TopicRelativeDateFilter'
            Prelude.<$> (x Data..:? "Constant")
            Prelude.<*> (x Data..:? "RelativeDateFilterFunction")
            Prelude.<*> (x Data..:? "TimeGranularity")
      )

instance Prelude.Hashable TopicRelativeDateFilter where
  hashWithSalt _salt TopicRelativeDateFilter' {..} =
    _salt
      `Prelude.hashWithSalt` constant
      `Prelude.hashWithSalt` relativeDateFilterFunction
      `Prelude.hashWithSalt` timeGranularity

instance Prelude.NFData TopicRelativeDateFilter where
  rnf TopicRelativeDateFilter' {..} =
    Prelude.rnf constant
      `Prelude.seq` Prelude.rnf relativeDateFilterFunction
      `Prelude.seq` Prelude.rnf timeGranularity

instance Data.ToJSON TopicRelativeDateFilter where
  toJSON TopicRelativeDateFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Constant" Data..=) Prelude.<$> constant,
            ("RelativeDateFilterFunction" Data..=)
              Prelude.<$> relativeDateFilterFunction,
            ("TimeGranularity" Data..=)
              Prelude.<$> timeGranularity
          ]
      )
