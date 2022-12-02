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
-- Module      : Amazonka.EC2.Types.InstanceEventWindowTimeRangeRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceEventWindowTimeRangeRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.WeekDay
import qualified Amazonka.Prelude as Prelude

-- | The start day and time and the end day and time of the time range, in
-- UTC.
--
-- /See:/ 'newInstanceEventWindowTimeRangeRequest' smart constructor.
data InstanceEventWindowTimeRangeRequest = InstanceEventWindowTimeRangeRequest'
  { -- | The hour when the time range ends.
    endHour :: Prelude.Maybe Prelude.Natural,
    -- | The day on which the time range ends.
    endWeekDay :: Prelude.Maybe WeekDay,
    -- | The day on which the time range begins.
    startWeekDay :: Prelude.Maybe WeekDay,
    -- | The hour when the time range begins.
    startHour :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceEventWindowTimeRangeRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endHour', 'instanceEventWindowTimeRangeRequest_endHour' - The hour when the time range ends.
--
-- 'endWeekDay', 'instanceEventWindowTimeRangeRequest_endWeekDay' - The day on which the time range ends.
--
-- 'startWeekDay', 'instanceEventWindowTimeRangeRequest_startWeekDay' - The day on which the time range begins.
--
-- 'startHour', 'instanceEventWindowTimeRangeRequest_startHour' - The hour when the time range begins.
newInstanceEventWindowTimeRangeRequest ::
  InstanceEventWindowTimeRangeRequest
newInstanceEventWindowTimeRangeRequest =
  InstanceEventWindowTimeRangeRequest'
    { endHour =
        Prelude.Nothing,
      endWeekDay = Prelude.Nothing,
      startWeekDay = Prelude.Nothing,
      startHour = Prelude.Nothing
    }

-- | The hour when the time range ends.
instanceEventWindowTimeRangeRequest_endHour :: Lens.Lens' InstanceEventWindowTimeRangeRequest (Prelude.Maybe Prelude.Natural)
instanceEventWindowTimeRangeRequest_endHour = Lens.lens (\InstanceEventWindowTimeRangeRequest' {endHour} -> endHour) (\s@InstanceEventWindowTimeRangeRequest' {} a -> s {endHour = a} :: InstanceEventWindowTimeRangeRequest)

-- | The day on which the time range ends.
instanceEventWindowTimeRangeRequest_endWeekDay :: Lens.Lens' InstanceEventWindowTimeRangeRequest (Prelude.Maybe WeekDay)
instanceEventWindowTimeRangeRequest_endWeekDay = Lens.lens (\InstanceEventWindowTimeRangeRequest' {endWeekDay} -> endWeekDay) (\s@InstanceEventWindowTimeRangeRequest' {} a -> s {endWeekDay = a} :: InstanceEventWindowTimeRangeRequest)

-- | The day on which the time range begins.
instanceEventWindowTimeRangeRequest_startWeekDay :: Lens.Lens' InstanceEventWindowTimeRangeRequest (Prelude.Maybe WeekDay)
instanceEventWindowTimeRangeRequest_startWeekDay = Lens.lens (\InstanceEventWindowTimeRangeRequest' {startWeekDay} -> startWeekDay) (\s@InstanceEventWindowTimeRangeRequest' {} a -> s {startWeekDay = a} :: InstanceEventWindowTimeRangeRequest)

-- | The hour when the time range begins.
instanceEventWindowTimeRangeRequest_startHour :: Lens.Lens' InstanceEventWindowTimeRangeRequest (Prelude.Maybe Prelude.Natural)
instanceEventWindowTimeRangeRequest_startHour = Lens.lens (\InstanceEventWindowTimeRangeRequest' {startHour} -> startHour) (\s@InstanceEventWindowTimeRangeRequest' {} a -> s {startHour = a} :: InstanceEventWindowTimeRangeRequest)

instance
  Prelude.Hashable
    InstanceEventWindowTimeRangeRequest
  where
  hashWithSalt
    _salt
    InstanceEventWindowTimeRangeRequest' {..} =
      _salt `Prelude.hashWithSalt` endHour
        `Prelude.hashWithSalt` endWeekDay
        `Prelude.hashWithSalt` startWeekDay
        `Prelude.hashWithSalt` startHour

instance
  Prelude.NFData
    InstanceEventWindowTimeRangeRequest
  where
  rnf InstanceEventWindowTimeRangeRequest' {..} =
    Prelude.rnf endHour
      `Prelude.seq` Prelude.rnf endWeekDay
      `Prelude.seq` Prelude.rnf startWeekDay
      `Prelude.seq` Prelude.rnf startHour

instance
  Data.ToQuery
    InstanceEventWindowTimeRangeRequest
  where
  toQuery InstanceEventWindowTimeRangeRequest' {..} =
    Prelude.mconcat
      [ "EndHour" Data.=: endHour,
        "EndWeekDay" Data.=: endWeekDay,
        "StartWeekDay" Data.=: startWeekDay,
        "StartHour" Data.=: startHour
      ]
