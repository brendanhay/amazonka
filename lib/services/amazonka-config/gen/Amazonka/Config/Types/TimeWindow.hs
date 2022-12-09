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
-- Module      : Amazonka.Config.Types.TimeWindow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.TimeWindow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filters evaluation results based on start and end times.
--
-- /See:/ 'newTimeWindow' smart constructor.
data TimeWindow = TimeWindow'
  { -- | The end time of an execution. The end time must be after the start date.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The start time of an execution.
    startTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'timeWindow_endTime' - The end time of an execution. The end time must be after the start date.
--
-- 'startTime', 'timeWindow_startTime' - The start time of an execution.
newTimeWindow ::
  TimeWindow
newTimeWindow =
  TimeWindow'
    { endTime = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The end time of an execution. The end time must be after the start date.
timeWindow_endTime :: Lens.Lens' TimeWindow (Prelude.Maybe Prelude.UTCTime)
timeWindow_endTime = Lens.lens (\TimeWindow' {endTime} -> endTime) (\s@TimeWindow' {} a -> s {endTime = a} :: TimeWindow) Prelude.. Lens.mapping Data._Time

-- | The start time of an execution.
timeWindow_startTime :: Lens.Lens' TimeWindow (Prelude.Maybe Prelude.UTCTime)
timeWindow_startTime = Lens.lens (\TimeWindow' {startTime} -> startTime) (\s@TimeWindow' {} a -> s {startTime = a} :: TimeWindow) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable TimeWindow where
  hashWithSalt _salt TimeWindow' {..} =
    _salt `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData TimeWindow where
  rnf TimeWindow' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToJSON TimeWindow where
  toJSON TimeWindow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndTime" Data..=) Prelude.<$> endTime,
            ("StartTime" Data..=) Prelude.<$> startTime
          ]
      )
