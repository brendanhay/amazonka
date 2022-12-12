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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.CreateRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types.IntervalUnitValues
import Amazonka.DLM.Types.LocationValues
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | __[Snapshot and AMI policies only]__ Specifies when the policy should
-- create snapshots or AMIs.
--
-- -   You must specify either __CronExpression__, or __Interval__,
--     __IntervalUnit__, and __Times__.
--
-- -   If you need to specify an ArchiveRule for the schedule, then you
--     must specify a creation frequency of at least 28 days.
--
-- /See:/ 'newCreateRule' smart constructor.
data CreateRule = CreateRule'
  { -- | The schedule, as a Cron expression. The schedule interval must be
    -- between 1 hour and 1 year. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron expressions>
    -- in the /Amazon CloudWatch User Guide/.
    cronExpression :: Prelude.Maybe Prelude.Text,
    -- | The interval between snapshots. The supported values are 1, 2, 3, 4, 6,
    -- 8, 12, and 24.
    interval :: Prelude.Maybe Prelude.Natural,
    -- | The interval unit.
    intervalUnit :: Prelude.Maybe IntervalUnitValues,
    -- | __[Snapshot policies only]__ Specifies the destination for snapshots
    -- created by the policy. To create snapshots in the same Region as the
    -- source resource, specify @CLOUD@. To create snapshots on the same
    -- Outpost as the source resource, specify @OUTPOST_LOCAL@. If you omit
    -- this parameter, @CLOUD@ is used by default.
    --
    -- If the policy targets resources in an Amazon Web Services Region, then
    -- you must create snapshots in the same Region as the source resource. If
    -- the policy targets resources on an Outpost, then you can create
    -- snapshots on the same Outpost as the source resource, or in the Region
    -- of that Outpost.
    location :: Prelude.Maybe LocationValues,
    -- | The time, in UTC, to start the operation. The supported format is hh:mm.
    --
    -- The operation occurs within a one-hour window following the specified
    -- time. If you do not specify a time, Amazon Data Lifecycle Manager
    -- selects a time within the next 24 hours.
    times :: Prelude.Maybe [Prelude.Text]
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
-- 'cronExpression', 'createRule_cronExpression' - The schedule, as a Cron expression. The schedule interval must be
-- between 1 hour and 1 year. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron expressions>
-- in the /Amazon CloudWatch User Guide/.
--
-- 'interval', 'createRule_interval' - The interval between snapshots. The supported values are 1, 2, 3, 4, 6,
-- 8, 12, and 24.
--
-- 'intervalUnit', 'createRule_intervalUnit' - The interval unit.
--
-- 'location', 'createRule_location' - __[Snapshot policies only]__ Specifies the destination for snapshots
-- created by the policy. To create snapshots in the same Region as the
-- source resource, specify @CLOUD@. To create snapshots on the same
-- Outpost as the source resource, specify @OUTPOST_LOCAL@. If you omit
-- this parameter, @CLOUD@ is used by default.
--
-- If the policy targets resources in an Amazon Web Services Region, then
-- you must create snapshots in the same Region as the source resource. If
-- the policy targets resources on an Outpost, then you can create
-- snapshots on the same Outpost as the source resource, or in the Region
-- of that Outpost.
--
-- 'times', 'createRule_times' - The time, in UTC, to start the operation. The supported format is hh:mm.
--
-- The operation occurs within a one-hour window following the specified
-- time. If you do not specify a time, Amazon Data Lifecycle Manager
-- selects a time within the next 24 hours.
newCreateRule ::
  CreateRule
newCreateRule =
  CreateRule'
    { cronExpression = Prelude.Nothing,
      interval = Prelude.Nothing,
      intervalUnit = Prelude.Nothing,
      location = Prelude.Nothing,
      times = Prelude.Nothing
    }

-- | The schedule, as a Cron expression. The schedule interval must be
-- between 1 hour and 1 year. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron expressions>
-- in the /Amazon CloudWatch User Guide/.
createRule_cronExpression :: Lens.Lens' CreateRule (Prelude.Maybe Prelude.Text)
createRule_cronExpression = Lens.lens (\CreateRule' {cronExpression} -> cronExpression) (\s@CreateRule' {} a -> s {cronExpression = a} :: CreateRule)

-- | The interval between snapshots. The supported values are 1, 2, 3, 4, 6,
-- 8, 12, and 24.
createRule_interval :: Lens.Lens' CreateRule (Prelude.Maybe Prelude.Natural)
createRule_interval = Lens.lens (\CreateRule' {interval} -> interval) (\s@CreateRule' {} a -> s {interval = a} :: CreateRule)

-- | The interval unit.
createRule_intervalUnit :: Lens.Lens' CreateRule (Prelude.Maybe IntervalUnitValues)
createRule_intervalUnit = Lens.lens (\CreateRule' {intervalUnit} -> intervalUnit) (\s@CreateRule' {} a -> s {intervalUnit = a} :: CreateRule)

-- | __[Snapshot policies only]__ Specifies the destination for snapshots
-- created by the policy. To create snapshots in the same Region as the
-- source resource, specify @CLOUD@. To create snapshots on the same
-- Outpost as the source resource, specify @OUTPOST_LOCAL@. If you omit
-- this parameter, @CLOUD@ is used by default.
--
-- If the policy targets resources in an Amazon Web Services Region, then
-- you must create snapshots in the same Region as the source resource. If
-- the policy targets resources on an Outpost, then you can create
-- snapshots on the same Outpost as the source resource, or in the Region
-- of that Outpost.
createRule_location :: Lens.Lens' CreateRule (Prelude.Maybe LocationValues)
createRule_location = Lens.lens (\CreateRule' {location} -> location) (\s@CreateRule' {} a -> s {location = a} :: CreateRule)

-- | The time, in UTC, to start the operation. The supported format is hh:mm.
--
-- The operation occurs within a one-hour window following the specified
-- time. If you do not specify a time, Amazon Data Lifecycle Manager
-- selects a time within the next 24 hours.
createRule_times :: Lens.Lens' CreateRule (Prelude.Maybe [Prelude.Text])
createRule_times = Lens.lens (\CreateRule' {times} -> times) (\s@CreateRule' {} a -> s {times = a} :: CreateRule) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CreateRule where
  parseJSON =
    Data.withObject
      "CreateRule"
      ( \x ->
          CreateRule'
            Prelude.<$> (x Data..:? "CronExpression")
            Prelude.<*> (x Data..:? "Interval")
            Prelude.<*> (x Data..:? "IntervalUnit")
            Prelude.<*> (x Data..:? "Location")
            Prelude.<*> (x Data..:? "Times" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CreateRule where
  hashWithSalt _salt CreateRule' {..} =
    _salt `Prelude.hashWithSalt` cronExpression
      `Prelude.hashWithSalt` interval
      `Prelude.hashWithSalt` intervalUnit
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` times

instance Prelude.NFData CreateRule where
  rnf CreateRule' {..} =
    Prelude.rnf cronExpression
      `Prelude.seq` Prelude.rnf interval
      `Prelude.seq` Prelude.rnf intervalUnit
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf times

instance Data.ToJSON CreateRule where
  toJSON CreateRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CronExpression" Data..=)
              Prelude.<$> cronExpression,
            ("Interval" Data..=) Prelude.<$> interval,
            ("IntervalUnit" Data..=) Prelude.<$> intervalUnit,
            ("Location" Data..=) Prelude.<$> location,
            ("Times" Data..=) Prelude.<$> times
          ]
      )
