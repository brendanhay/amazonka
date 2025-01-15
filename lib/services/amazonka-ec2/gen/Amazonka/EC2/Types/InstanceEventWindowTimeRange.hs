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
-- Module      : Amazonka.EC2.Types.InstanceEventWindowTimeRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceEventWindowTimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.WeekDay
import qualified Amazonka.Prelude as Prelude

-- | The start day and time and the end day and time of the time range, in
-- UTC.
--
-- /See:/ 'newInstanceEventWindowTimeRange' smart constructor.
data InstanceEventWindowTimeRange = InstanceEventWindowTimeRange'
  { -- | The hour when the time range ends.
    endHour :: Prelude.Maybe Prelude.Natural,
    -- | The day on which the time range ends.
    endWeekDay :: Prelude.Maybe WeekDay,
    -- | The hour when the time range begins.
    startHour :: Prelude.Maybe Prelude.Natural,
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
-- 'endHour', 'instanceEventWindowTimeRange_endHour' - The hour when the time range ends.
--
-- 'endWeekDay', 'instanceEventWindowTimeRange_endWeekDay' - The day on which the time range ends.
--
-- 'startHour', 'instanceEventWindowTimeRange_startHour' - The hour when the time range begins.
--
-- 'startWeekDay', 'instanceEventWindowTimeRange_startWeekDay' - The day on which the time range begins.
newInstanceEventWindowTimeRange ::
  InstanceEventWindowTimeRange
newInstanceEventWindowTimeRange =
  InstanceEventWindowTimeRange'
    { endHour =
        Prelude.Nothing,
      endWeekDay = Prelude.Nothing,
      startHour = Prelude.Nothing,
      startWeekDay = Prelude.Nothing
    }

-- | The hour when the time range ends.
instanceEventWindowTimeRange_endHour :: Lens.Lens' InstanceEventWindowTimeRange (Prelude.Maybe Prelude.Natural)
instanceEventWindowTimeRange_endHour = Lens.lens (\InstanceEventWindowTimeRange' {endHour} -> endHour) (\s@InstanceEventWindowTimeRange' {} a -> s {endHour = a} :: InstanceEventWindowTimeRange)

-- | The day on which the time range ends.
instanceEventWindowTimeRange_endWeekDay :: Lens.Lens' InstanceEventWindowTimeRange (Prelude.Maybe WeekDay)
instanceEventWindowTimeRange_endWeekDay = Lens.lens (\InstanceEventWindowTimeRange' {endWeekDay} -> endWeekDay) (\s@InstanceEventWindowTimeRange' {} a -> s {endWeekDay = a} :: InstanceEventWindowTimeRange)

-- | The hour when the time range begins.
instanceEventWindowTimeRange_startHour :: Lens.Lens' InstanceEventWindowTimeRange (Prelude.Maybe Prelude.Natural)
instanceEventWindowTimeRange_startHour = Lens.lens (\InstanceEventWindowTimeRange' {startHour} -> startHour) (\s@InstanceEventWindowTimeRange' {} a -> s {startHour = a} :: InstanceEventWindowTimeRange)

-- | The day on which the time range begins.
instanceEventWindowTimeRange_startWeekDay :: Lens.Lens' InstanceEventWindowTimeRange (Prelude.Maybe WeekDay)
instanceEventWindowTimeRange_startWeekDay = Lens.lens (\InstanceEventWindowTimeRange' {startWeekDay} -> startWeekDay) (\s@InstanceEventWindowTimeRange' {} a -> s {startWeekDay = a} :: InstanceEventWindowTimeRange)

instance Data.FromXML InstanceEventWindowTimeRange where
  parseXML x =
    InstanceEventWindowTimeRange'
      Prelude.<$> (x Data..@? "endHour")
      Prelude.<*> (x Data..@? "endWeekDay")
      Prelude.<*> (x Data..@? "startHour")
      Prelude.<*> (x Data..@? "startWeekDay")

instance
  Prelude.Hashable
    InstanceEventWindowTimeRange
  where
  hashWithSalt _salt InstanceEventWindowTimeRange' {..} =
    _salt
      `Prelude.hashWithSalt` endHour
      `Prelude.hashWithSalt` endWeekDay
      `Prelude.hashWithSalt` startHour
      `Prelude.hashWithSalt` startWeekDay

instance Prelude.NFData InstanceEventWindowTimeRange where
  rnf InstanceEventWindowTimeRange' {..} =
    Prelude.rnf endHour `Prelude.seq`
      Prelude.rnf endWeekDay `Prelude.seq`
        Prelude.rnf startHour `Prelude.seq`
          Prelude.rnf startWeekDay
