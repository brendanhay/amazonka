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
-- Module      : Network.AWS.EC2.Types.InstanceEventWindowTimeRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceEventWindowTimeRange where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.WeekDay
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The start day and time and the end day and time of the time range, in
-- UTC.
--
-- /See:/ 'newInstanceEventWindowTimeRange' smart constructor.
data InstanceEventWindowTimeRange = InstanceEventWindowTimeRange'
  { -- | The hour when the time range begins.
    startHour :: Prelude.Maybe Prelude.Natural,
    -- | The day on which the time range ends.
    endWeekDay :: Prelude.Maybe WeekDay,
    -- | The hour when the time range ends.
    endHour :: Prelude.Maybe Prelude.Natural,
    -- | The day on which the time range begins.
    startWeekDay :: Prelude.Maybe WeekDay
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceEventWindowTimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startHour', 'instanceEventWindowTimeRange_startHour' - The hour when the time range begins.
--
-- 'endWeekDay', 'instanceEventWindowTimeRange_endWeekDay' - The day on which the time range ends.
--
-- 'endHour', 'instanceEventWindowTimeRange_endHour' - The hour when the time range ends.
--
-- 'startWeekDay', 'instanceEventWindowTimeRange_startWeekDay' - The day on which the time range begins.
newInstanceEventWindowTimeRange ::
  InstanceEventWindowTimeRange
newInstanceEventWindowTimeRange =
  InstanceEventWindowTimeRange'
    { startHour =
        Prelude.Nothing,
      endWeekDay = Prelude.Nothing,
      endHour = Prelude.Nothing,
      startWeekDay = Prelude.Nothing
    }

-- | The hour when the time range begins.
instanceEventWindowTimeRange_startHour :: Lens.Lens' InstanceEventWindowTimeRange (Prelude.Maybe Prelude.Natural)
instanceEventWindowTimeRange_startHour = Lens.lens (\InstanceEventWindowTimeRange' {startHour} -> startHour) (\s@InstanceEventWindowTimeRange' {} a -> s {startHour = a} :: InstanceEventWindowTimeRange)

-- | The day on which the time range ends.
instanceEventWindowTimeRange_endWeekDay :: Lens.Lens' InstanceEventWindowTimeRange (Prelude.Maybe WeekDay)
instanceEventWindowTimeRange_endWeekDay = Lens.lens (\InstanceEventWindowTimeRange' {endWeekDay} -> endWeekDay) (\s@InstanceEventWindowTimeRange' {} a -> s {endWeekDay = a} :: InstanceEventWindowTimeRange)

-- | The hour when the time range ends.
instanceEventWindowTimeRange_endHour :: Lens.Lens' InstanceEventWindowTimeRange (Prelude.Maybe Prelude.Natural)
instanceEventWindowTimeRange_endHour = Lens.lens (\InstanceEventWindowTimeRange' {endHour} -> endHour) (\s@InstanceEventWindowTimeRange' {} a -> s {endHour = a} :: InstanceEventWindowTimeRange)

-- | The day on which the time range begins.
instanceEventWindowTimeRange_startWeekDay :: Lens.Lens' InstanceEventWindowTimeRange (Prelude.Maybe WeekDay)
instanceEventWindowTimeRange_startWeekDay = Lens.lens (\InstanceEventWindowTimeRange' {startWeekDay} -> startWeekDay) (\s@InstanceEventWindowTimeRange' {} a -> s {startWeekDay = a} :: InstanceEventWindowTimeRange)

instance Core.FromXML InstanceEventWindowTimeRange where
  parseXML x =
    InstanceEventWindowTimeRange'
      Prelude.<$> (x Core..@? "startHour")
      Prelude.<*> (x Core..@? "endWeekDay")
      Prelude.<*> (x Core..@? "endHour")
      Prelude.<*> (x Core..@? "startWeekDay")

instance
  Prelude.Hashable
    InstanceEventWindowTimeRange

instance Prelude.NFData InstanceEventWindowTimeRange
