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
-- Module      : Amazonka.Transcribe.Types.AbsoluteTimeRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.AbsoluteTimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A time range, set in seconds, between two points in the call.
--
-- /See:/ 'newAbsoluteTimeRange' smart constructor.
data AbsoluteTimeRange = AbsoluteTimeRange'
  { -- | A time range from the beginning of the call to the value that you\'ve
    -- specified. For example, if you specify 100000, the time range is set to
    -- the first 100,000 milliseconds of the call.
    first :: Prelude.Maybe Prelude.Natural,
    -- | A value that indicates the beginning of the time range in seconds. To
    -- set absolute time range, you must specify a start time and an end time.
    -- For example, if you specify the following values:
    --
    -- -   StartTime - 10000
    --
    -- -   Endtime - 50000
    --
    -- The time range is set between 10,000 milliseconds and 50,000
    -- milliseconds into the call.
    startTime :: Prelude.Maybe Prelude.Natural,
    -- | A time range from the value that you\'ve specified to the end of the
    -- call. For example, if you specify 100000, the time range is set to the
    -- last 100,000 milliseconds of the call.
    last :: Prelude.Maybe Prelude.Natural,
    -- | A value that indicates the end of the time range in milliseconds. To set
    -- absolute time range, you must specify a start time and an end time. For
    -- example, if you specify the following values:
    --
    -- -   StartTime - 10000
    --
    -- -   Endtime - 50000
    --
    -- The time range is set between 10,000 milliseconds and 50,000
    -- milliseconds into the call.
    endTime :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AbsoluteTimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'first', 'absoluteTimeRange_first' - A time range from the beginning of the call to the value that you\'ve
-- specified. For example, if you specify 100000, the time range is set to
-- the first 100,000 milliseconds of the call.
--
-- 'startTime', 'absoluteTimeRange_startTime' - A value that indicates the beginning of the time range in seconds. To
-- set absolute time range, you must specify a start time and an end time.
-- For example, if you specify the following values:
--
-- -   StartTime - 10000
--
-- -   Endtime - 50000
--
-- The time range is set between 10,000 milliseconds and 50,000
-- milliseconds into the call.
--
-- 'last', 'absoluteTimeRange_last' - A time range from the value that you\'ve specified to the end of the
-- call. For example, if you specify 100000, the time range is set to the
-- last 100,000 milliseconds of the call.
--
-- 'endTime', 'absoluteTimeRange_endTime' - A value that indicates the end of the time range in milliseconds. To set
-- absolute time range, you must specify a start time and an end time. For
-- example, if you specify the following values:
--
-- -   StartTime - 10000
--
-- -   Endtime - 50000
--
-- The time range is set between 10,000 milliseconds and 50,000
-- milliseconds into the call.
newAbsoluteTimeRange ::
  AbsoluteTimeRange
newAbsoluteTimeRange =
  AbsoluteTimeRange'
    { first = Prelude.Nothing,
      startTime = Prelude.Nothing,
      last = Prelude.Nothing,
      endTime = Prelude.Nothing
    }

-- | A time range from the beginning of the call to the value that you\'ve
-- specified. For example, if you specify 100000, the time range is set to
-- the first 100,000 milliseconds of the call.
absoluteTimeRange_first :: Lens.Lens' AbsoluteTimeRange (Prelude.Maybe Prelude.Natural)
absoluteTimeRange_first = Lens.lens (\AbsoluteTimeRange' {first} -> first) (\s@AbsoluteTimeRange' {} a -> s {first = a} :: AbsoluteTimeRange)

-- | A value that indicates the beginning of the time range in seconds. To
-- set absolute time range, you must specify a start time and an end time.
-- For example, if you specify the following values:
--
-- -   StartTime - 10000
--
-- -   Endtime - 50000
--
-- The time range is set between 10,000 milliseconds and 50,000
-- milliseconds into the call.
absoluteTimeRange_startTime :: Lens.Lens' AbsoluteTimeRange (Prelude.Maybe Prelude.Natural)
absoluteTimeRange_startTime = Lens.lens (\AbsoluteTimeRange' {startTime} -> startTime) (\s@AbsoluteTimeRange' {} a -> s {startTime = a} :: AbsoluteTimeRange)

-- | A time range from the value that you\'ve specified to the end of the
-- call. For example, if you specify 100000, the time range is set to the
-- last 100,000 milliseconds of the call.
absoluteTimeRange_last :: Lens.Lens' AbsoluteTimeRange (Prelude.Maybe Prelude.Natural)
absoluteTimeRange_last = Lens.lens (\AbsoluteTimeRange' {last} -> last) (\s@AbsoluteTimeRange' {} a -> s {last = a} :: AbsoluteTimeRange)

-- | A value that indicates the end of the time range in milliseconds. To set
-- absolute time range, you must specify a start time and an end time. For
-- example, if you specify the following values:
--
-- -   StartTime - 10000
--
-- -   Endtime - 50000
--
-- The time range is set between 10,000 milliseconds and 50,000
-- milliseconds into the call.
absoluteTimeRange_endTime :: Lens.Lens' AbsoluteTimeRange (Prelude.Maybe Prelude.Natural)
absoluteTimeRange_endTime = Lens.lens (\AbsoluteTimeRange' {endTime} -> endTime) (\s@AbsoluteTimeRange' {} a -> s {endTime = a} :: AbsoluteTimeRange)

instance Core.FromJSON AbsoluteTimeRange where
  parseJSON =
    Core.withObject
      "AbsoluteTimeRange"
      ( \x ->
          AbsoluteTimeRange'
            Prelude.<$> (x Core..:? "First")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "Last")
            Prelude.<*> (x Core..:? "EndTime")
      )

instance Prelude.Hashable AbsoluteTimeRange where
  hashWithSalt salt' AbsoluteTimeRange' {..} =
    salt' `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` last
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` first

instance Prelude.NFData AbsoluteTimeRange where
  rnf AbsoluteTimeRange' {..} =
    Prelude.rnf first `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf last
      `Prelude.seq` Prelude.rnf startTime

instance Core.ToJSON AbsoluteTimeRange where
  toJSON AbsoluteTimeRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("First" Core..=) Prelude.<$> first,
            ("StartTime" Core..=) Prelude.<$> startTime,
            ("Last" Core..=) Prelude.<$> last,
            ("EndTime" Core..=) Prelude.<$> endTime
          ]
      )
