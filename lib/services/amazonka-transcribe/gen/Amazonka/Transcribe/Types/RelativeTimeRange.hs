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
-- Module      : Amazonka.Transcribe.Types.RelativeTimeRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.RelativeTimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that allows percentages to specify the proportion of the call
-- where you would like to apply a filter. For example, you can specify the
-- first half of the call. You can also specify the period of time between
-- halfway through to three-quarters of the way through the call. Because
-- the length of conversation can vary between calls, you can apply
-- relative time ranges across all calls.
--
-- /See:/ 'newRelativeTimeRange' smart constructor.
data RelativeTimeRange = RelativeTimeRange'
  { -- | A range that takes the portion of the call from the time in milliseconds
    -- set by the value that you\'ve specified to the end of the call. For
    -- example, if you specify @120000@, the time range is set for the last
    -- 120,000 milliseconds of the call.
    last :: Prelude.Maybe Prelude.Natural,
    -- | A value that indicates the percentage of the end of the time range. To
    -- set a relative time range, you must specify a start percentage and an
    -- end percentage. For example, if you specify the following values:
    --
    -- -   StartPercentage - 10
    --
    -- -   EndPercentage - 50
    --
    -- This looks at the time range starting from 10% of the way into the call
    -- to 50% of the way through the call. For a call that lasts 100,000
    -- milliseconds, this example range would apply from the 10,000 millisecond
    -- mark to the 50,000 millisecond mark.
    endPercentage :: Prelude.Maybe Prelude.Natural,
    -- | A value that indicates the percentage of the beginning of the time
    -- range. To set a relative time range, you must specify a start percentage
    -- and an end percentage. For example, if you specify the following values:
    --
    -- -   StartPercentage - 10
    --
    -- -   EndPercentage - 50
    --
    -- This looks at the time range starting from 10% of the way into the call
    -- to 50% of the way through the call. For a call that lasts 100,000
    -- milliseconds, this example range would apply from the 10,000 millisecond
    -- mark to the 50,000 millisecond mark.
    startPercentage :: Prelude.Maybe Prelude.Natural,
    -- | A range that takes the portion of the call up to the time in
    -- milliseconds set by the value that you\'ve specified. For example, if
    -- you specify @120000@, the time range is set for the first 120,000
    -- milliseconds of the call.
    first :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelativeTimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'last', 'relativeTimeRange_last' - A range that takes the portion of the call from the time in milliseconds
-- set by the value that you\'ve specified to the end of the call. For
-- example, if you specify @120000@, the time range is set for the last
-- 120,000 milliseconds of the call.
--
-- 'endPercentage', 'relativeTimeRange_endPercentage' - A value that indicates the percentage of the end of the time range. To
-- set a relative time range, you must specify a start percentage and an
-- end percentage. For example, if you specify the following values:
--
-- -   StartPercentage - 10
--
-- -   EndPercentage - 50
--
-- This looks at the time range starting from 10% of the way into the call
-- to 50% of the way through the call. For a call that lasts 100,000
-- milliseconds, this example range would apply from the 10,000 millisecond
-- mark to the 50,000 millisecond mark.
--
-- 'startPercentage', 'relativeTimeRange_startPercentage' - A value that indicates the percentage of the beginning of the time
-- range. To set a relative time range, you must specify a start percentage
-- and an end percentage. For example, if you specify the following values:
--
-- -   StartPercentage - 10
--
-- -   EndPercentage - 50
--
-- This looks at the time range starting from 10% of the way into the call
-- to 50% of the way through the call. For a call that lasts 100,000
-- milliseconds, this example range would apply from the 10,000 millisecond
-- mark to the 50,000 millisecond mark.
--
-- 'first', 'relativeTimeRange_first' - A range that takes the portion of the call up to the time in
-- milliseconds set by the value that you\'ve specified. For example, if
-- you specify @120000@, the time range is set for the first 120,000
-- milliseconds of the call.
newRelativeTimeRange ::
  RelativeTimeRange
newRelativeTimeRange =
  RelativeTimeRange'
    { last = Prelude.Nothing,
      endPercentage = Prelude.Nothing,
      startPercentage = Prelude.Nothing,
      first = Prelude.Nothing
    }

-- | A range that takes the portion of the call from the time in milliseconds
-- set by the value that you\'ve specified to the end of the call. For
-- example, if you specify @120000@, the time range is set for the last
-- 120,000 milliseconds of the call.
relativeTimeRange_last :: Lens.Lens' RelativeTimeRange (Prelude.Maybe Prelude.Natural)
relativeTimeRange_last = Lens.lens (\RelativeTimeRange' {last} -> last) (\s@RelativeTimeRange' {} a -> s {last = a} :: RelativeTimeRange)

-- | A value that indicates the percentage of the end of the time range. To
-- set a relative time range, you must specify a start percentage and an
-- end percentage. For example, if you specify the following values:
--
-- -   StartPercentage - 10
--
-- -   EndPercentage - 50
--
-- This looks at the time range starting from 10% of the way into the call
-- to 50% of the way through the call. For a call that lasts 100,000
-- milliseconds, this example range would apply from the 10,000 millisecond
-- mark to the 50,000 millisecond mark.
relativeTimeRange_endPercentage :: Lens.Lens' RelativeTimeRange (Prelude.Maybe Prelude.Natural)
relativeTimeRange_endPercentage = Lens.lens (\RelativeTimeRange' {endPercentage} -> endPercentage) (\s@RelativeTimeRange' {} a -> s {endPercentage = a} :: RelativeTimeRange)

-- | A value that indicates the percentage of the beginning of the time
-- range. To set a relative time range, you must specify a start percentage
-- and an end percentage. For example, if you specify the following values:
--
-- -   StartPercentage - 10
--
-- -   EndPercentage - 50
--
-- This looks at the time range starting from 10% of the way into the call
-- to 50% of the way through the call. For a call that lasts 100,000
-- milliseconds, this example range would apply from the 10,000 millisecond
-- mark to the 50,000 millisecond mark.
relativeTimeRange_startPercentage :: Lens.Lens' RelativeTimeRange (Prelude.Maybe Prelude.Natural)
relativeTimeRange_startPercentage = Lens.lens (\RelativeTimeRange' {startPercentage} -> startPercentage) (\s@RelativeTimeRange' {} a -> s {startPercentage = a} :: RelativeTimeRange)

-- | A range that takes the portion of the call up to the time in
-- milliseconds set by the value that you\'ve specified. For example, if
-- you specify @120000@, the time range is set for the first 120,000
-- milliseconds of the call.
relativeTimeRange_first :: Lens.Lens' RelativeTimeRange (Prelude.Maybe Prelude.Natural)
relativeTimeRange_first = Lens.lens (\RelativeTimeRange' {first} -> first) (\s@RelativeTimeRange' {} a -> s {first = a} :: RelativeTimeRange)

instance Core.FromJSON RelativeTimeRange where
  parseJSON =
    Core.withObject
      "RelativeTimeRange"
      ( \x ->
          RelativeTimeRange'
            Prelude.<$> (x Core..:? "Last")
            Prelude.<*> (x Core..:? "EndPercentage")
            Prelude.<*> (x Core..:? "StartPercentage")
            Prelude.<*> (x Core..:? "First")
      )

instance Prelude.Hashable RelativeTimeRange where
  hashWithSalt _salt RelativeTimeRange' {..} =
    _salt `Prelude.hashWithSalt` last
      `Prelude.hashWithSalt` endPercentage
      `Prelude.hashWithSalt` startPercentage
      `Prelude.hashWithSalt` first

instance Prelude.NFData RelativeTimeRange where
  rnf RelativeTimeRange' {..} =
    Prelude.rnf last
      `Prelude.seq` Prelude.rnf endPercentage
      `Prelude.seq` Prelude.rnf startPercentage
      `Prelude.seq` Prelude.rnf first

instance Core.ToJSON RelativeTimeRange where
  toJSON RelativeTimeRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Last" Core..=) Prelude.<$> last,
            ("EndPercentage" Core..=) Prelude.<$> endPercentage,
            ("StartPercentage" Core..=)
              Prelude.<$> startPercentage,
            ("First" Core..=) Prelude.<$> first
          ]
      )
