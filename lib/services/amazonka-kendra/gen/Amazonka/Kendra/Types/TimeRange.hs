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
-- Module      : Amazonka.Kendra.Types.TimeRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.TimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a range of time.
--
-- /See:/ 'newTimeRange' smart constructor.
data TimeRange = TimeRange'
  { -- | The UNIX datetime of the end of the time range.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The UNIX datetime of the beginning of the time range.
    startTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'timeRange_endTime' - The UNIX datetime of the end of the time range.
--
-- 'startTime', 'timeRange_startTime' - The UNIX datetime of the beginning of the time range.
newTimeRange ::
  TimeRange
newTimeRange =
  TimeRange'
    { endTime = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The UNIX datetime of the end of the time range.
timeRange_endTime :: Lens.Lens' TimeRange (Prelude.Maybe Prelude.UTCTime)
timeRange_endTime = Lens.lens (\TimeRange' {endTime} -> endTime) (\s@TimeRange' {} a -> s {endTime = a} :: TimeRange) Prelude.. Lens.mapping Data._Time

-- | The UNIX datetime of the beginning of the time range.
timeRange_startTime :: Lens.Lens' TimeRange (Prelude.Maybe Prelude.UTCTime)
timeRange_startTime = Lens.lens (\TimeRange' {startTime} -> startTime) (\s@TimeRange' {} a -> s {startTime = a} :: TimeRange) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON TimeRange where
  parseJSON =
    Data.withObject
      "TimeRange"
      ( \x ->
          TimeRange'
            Prelude.<$> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "StartTime")
      )

instance Prelude.Hashable TimeRange where
  hashWithSalt _salt TimeRange' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData TimeRange where
  rnf TimeRange' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToJSON TimeRange where
  toJSON TimeRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndTime" Data..=) Prelude.<$> endTime,
            ("StartTime" Data..=) Prelude.<$> startTime
          ]
      )
