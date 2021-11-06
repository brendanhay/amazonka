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
-- Module      : Amazonka.DLM.Types.CreateRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.CreateRule where

import qualified Amazonka.Core as Core
import Amazonka.DLM.Types.IntervalUnitValues
import Amazonka.DLM.Types.LocationValues
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies when to create snapshots of EBS volumes.
--
-- You must specify either a Cron expression or an interval, interval unit,
-- and start time. You cannot specify both.
--
-- /See:/ 'newCreateRule' smart constructor.
data CreateRule = CreateRule'
  { -- | Specifies the destination for snapshots created by the policy. To create
    -- snapshots in the same Region as the source resource, specify @CLOUD@. To
    -- create snapshots on the same Outpost as the source resource, specify
    -- @OUTPOST_LOCAL@. If you omit this parameter, @CLOUD@ is used by default.
    --
    -- If the policy targets resources in an Amazon Web Services Region, then
    -- you must create snapshots in the same Region as the source resource.
    --
    -- If the policy targets resources on an Outpost, then you can create
    -- snapshots on the same Outpost as the source resource, or in the Region
    -- of that Outpost.
    location :: Prelude.Maybe LocationValues,
    -- | The interval between snapshots. The supported values are 1, 2, 3, 4, 6,
    -- 8, 12, and 24.
    interval :: Prelude.Maybe Prelude.Natural,
    -- | The schedule, as a Cron expression. The schedule interval must be
    -- between 1 hour and 1 year. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron expressions>
    -- in the /Amazon CloudWatch User Guide/.
    cronExpression :: Prelude.Maybe Prelude.Text,
    -- | The time, in UTC, to start the operation. The supported format is hh:mm.
    --
    -- The operation occurs within a one-hour window following the specified
    -- time. If you do not specify a time, Amazon DLM selects a time within the
    -- next 24 hours.
    times :: Prelude.Maybe [Prelude.Text],
    -- | The interval unit.
    intervalUnit :: Prelude.Maybe IntervalUnitValues
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'createRule_location' - Specifies the destination for snapshots created by the policy. To create
-- snapshots in the same Region as the source resource, specify @CLOUD@. To
-- create snapshots on the same Outpost as the source resource, specify
-- @OUTPOST_LOCAL@. If you omit this parameter, @CLOUD@ is used by default.
--
-- If the policy targets resources in an Amazon Web Services Region, then
-- you must create snapshots in the same Region as the source resource.
--
-- If the policy targets resources on an Outpost, then you can create
-- snapshots on the same Outpost as the source resource, or in the Region
-- of that Outpost.
--
-- 'interval', 'createRule_interval' - The interval between snapshots. The supported values are 1, 2, 3, 4, 6,
-- 8, 12, and 24.
--
-- 'cronExpression', 'createRule_cronExpression' - The schedule, as a Cron expression. The schedule interval must be
-- between 1 hour and 1 year. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron expressions>
-- in the /Amazon CloudWatch User Guide/.
--
-- 'times', 'createRule_times' - The time, in UTC, to start the operation. The supported format is hh:mm.
--
-- The operation occurs within a one-hour window following the specified
-- time. If you do not specify a time, Amazon DLM selects a time within the
-- next 24 hours.
--
-- 'intervalUnit', 'createRule_intervalUnit' - The interval unit.
newCreateRule ::
  CreateRule
newCreateRule =
  CreateRule'
    { location = Prelude.Nothing,
      interval = Prelude.Nothing,
      cronExpression = Prelude.Nothing,
      times = Prelude.Nothing,
      intervalUnit = Prelude.Nothing
    }

-- | Specifies the destination for snapshots created by the policy. To create
-- snapshots in the same Region as the source resource, specify @CLOUD@. To
-- create snapshots on the same Outpost as the source resource, specify
-- @OUTPOST_LOCAL@. If you omit this parameter, @CLOUD@ is used by default.
--
-- If the policy targets resources in an Amazon Web Services Region, then
-- you must create snapshots in the same Region as the source resource.
--
-- If the policy targets resources on an Outpost, then you can create
-- snapshots on the same Outpost as the source resource, or in the Region
-- of that Outpost.
createRule_location :: Lens.Lens' CreateRule (Prelude.Maybe LocationValues)
createRule_location = Lens.lens (\CreateRule' {location} -> location) (\s@CreateRule' {} a -> s {location = a} :: CreateRule)

-- | The interval between snapshots. The supported values are 1, 2, 3, 4, 6,
-- 8, 12, and 24.
createRule_interval :: Lens.Lens' CreateRule (Prelude.Maybe Prelude.Natural)
createRule_interval = Lens.lens (\CreateRule' {interval} -> interval) (\s@CreateRule' {} a -> s {interval = a} :: CreateRule)

-- | The schedule, as a Cron expression. The schedule interval must be
-- between 1 hour and 1 year. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron expressions>
-- in the /Amazon CloudWatch User Guide/.
createRule_cronExpression :: Lens.Lens' CreateRule (Prelude.Maybe Prelude.Text)
createRule_cronExpression = Lens.lens (\CreateRule' {cronExpression} -> cronExpression) (\s@CreateRule' {} a -> s {cronExpression = a} :: CreateRule)

-- | The time, in UTC, to start the operation. The supported format is hh:mm.
--
-- The operation occurs within a one-hour window following the specified
-- time. If you do not specify a time, Amazon DLM selects a time within the
-- next 24 hours.
createRule_times :: Lens.Lens' CreateRule (Prelude.Maybe [Prelude.Text])
createRule_times = Lens.lens (\CreateRule' {times} -> times) (\s@CreateRule' {} a -> s {times = a} :: CreateRule) Prelude.. Lens.mapping Lens.coerced

-- | The interval unit.
createRule_intervalUnit :: Lens.Lens' CreateRule (Prelude.Maybe IntervalUnitValues)
createRule_intervalUnit = Lens.lens (\CreateRule' {intervalUnit} -> intervalUnit) (\s@CreateRule' {} a -> s {intervalUnit = a} :: CreateRule)

instance Core.FromJSON CreateRule where
  parseJSON =
    Core.withObject
      "CreateRule"
      ( \x ->
          CreateRule'
            Prelude.<$> (x Core..:? "Location")
            Prelude.<*> (x Core..:? "Interval")
            Prelude.<*> (x Core..:? "CronExpression")
            Prelude.<*> (x Core..:? "Times" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "IntervalUnit")
      )

instance Prelude.Hashable CreateRule

instance Prelude.NFData CreateRule

instance Core.ToJSON CreateRule where
  toJSON CreateRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Location" Core..=) Prelude.<$> location,
            ("Interval" Core..=) Prelude.<$> interval,
            ("CronExpression" Core..=)
              Prelude.<$> cronExpression,
            ("Times" Core..=) Prelude.<$> times,
            ("IntervalUnit" Core..=) Prelude.<$> intervalUnit
          ]
      )
