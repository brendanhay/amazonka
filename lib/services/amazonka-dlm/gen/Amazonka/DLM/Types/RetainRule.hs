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
-- Module      : Amazonka.DLM.Types.RetainRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.RetainRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types.RetentionIntervalUnitValues
import qualified Amazonka.Prelude as Prelude

-- | __[Snapshot and AMI policies only]__ Specifies a retention rule for
-- snapshots created by snapshot policies, or for AMIs created by AMI
-- policies.
--
-- For snapshot policies that have an ArchiveRule, this retention rule
-- applies to standard tier retention. When the retention threshold is met,
-- snapshots are moved from the standard to the archive tier.
--
-- For snapshot policies that do not have an __ArchiveRule__, snapshots are
-- permanently deleted when this retention threshold is met.
--
-- You can retain snapshots based on either a count or a time interval.
--
-- -   __Count-based retention__
--
--     You must specify __Count__. If you specify an ArchiveRule for the
--     schedule, then you can specify a retention count of @0@ to archive
--     snapshots immediately after creation. If you specify a
--     FastRestoreRule, ShareRule, or a CrossRegionCopyRule, then you must
--     specify a retention count of @1@ or more.
--
-- -   __Age-based retention__
--
--     You must specify __Interval__ and __IntervalUnit__. If you specify
--     an ArchiveRule for the schedule, then you can specify a retention
--     interval of @0@ days to archive snapshots immediately after
--     creation. If you specify a FastRestoreRule, ShareRule, or a
--     CrossRegionCopyRule, then you must specify a retention interval of
--     @1@ day or more.
--
-- /See:/ 'newRetainRule' smart constructor.
data RetainRule = RetainRule'
  { -- | The amount of time to retain each snapshot. The maximum is 100 years.
    -- This is equivalent to 1200 months, 5200 weeks, or 36500 days.
    interval :: Prelude.Maybe Prelude.Natural,
    -- | The number of snapshots to retain for each volume, up to a maximum of
    -- 1000. For example if you want to retain a maximum of three snapshots,
    -- specify @3@. When the fourth snapshot is created, the oldest retained
    -- snapshot is deleted, or it is moved to the archive tier if you have
    -- specified an ArchiveRule.
    count :: Prelude.Maybe Prelude.Natural,
    -- | The unit of time for time-based retention. For example, to retain
    -- snapshots for 3 months, specify @Interval=3@ and @IntervalUnit=MONTHS@.
    -- Once the snapshot has been retained for 3 months, it is deleted, or it
    -- is moved to the archive tier if you have specified an ArchiveRule.
    intervalUnit :: Prelude.Maybe RetentionIntervalUnitValues
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetainRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'interval', 'retainRule_interval' - The amount of time to retain each snapshot. The maximum is 100 years.
-- This is equivalent to 1200 months, 5200 weeks, or 36500 days.
--
-- 'count', 'retainRule_count' - The number of snapshots to retain for each volume, up to a maximum of
-- 1000. For example if you want to retain a maximum of three snapshots,
-- specify @3@. When the fourth snapshot is created, the oldest retained
-- snapshot is deleted, or it is moved to the archive tier if you have
-- specified an ArchiveRule.
--
-- 'intervalUnit', 'retainRule_intervalUnit' - The unit of time for time-based retention. For example, to retain
-- snapshots for 3 months, specify @Interval=3@ and @IntervalUnit=MONTHS@.
-- Once the snapshot has been retained for 3 months, it is deleted, or it
-- is moved to the archive tier if you have specified an ArchiveRule.
newRetainRule ::
  RetainRule
newRetainRule =
  RetainRule'
    { interval = Prelude.Nothing,
      count = Prelude.Nothing,
      intervalUnit = Prelude.Nothing
    }

-- | The amount of time to retain each snapshot. The maximum is 100 years.
-- This is equivalent to 1200 months, 5200 weeks, or 36500 days.
retainRule_interval :: Lens.Lens' RetainRule (Prelude.Maybe Prelude.Natural)
retainRule_interval = Lens.lens (\RetainRule' {interval} -> interval) (\s@RetainRule' {} a -> s {interval = a} :: RetainRule)

-- | The number of snapshots to retain for each volume, up to a maximum of
-- 1000. For example if you want to retain a maximum of three snapshots,
-- specify @3@. When the fourth snapshot is created, the oldest retained
-- snapshot is deleted, or it is moved to the archive tier if you have
-- specified an ArchiveRule.
retainRule_count :: Lens.Lens' RetainRule (Prelude.Maybe Prelude.Natural)
retainRule_count = Lens.lens (\RetainRule' {count} -> count) (\s@RetainRule' {} a -> s {count = a} :: RetainRule)

-- | The unit of time for time-based retention. For example, to retain
-- snapshots for 3 months, specify @Interval=3@ and @IntervalUnit=MONTHS@.
-- Once the snapshot has been retained for 3 months, it is deleted, or it
-- is moved to the archive tier if you have specified an ArchiveRule.
retainRule_intervalUnit :: Lens.Lens' RetainRule (Prelude.Maybe RetentionIntervalUnitValues)
retainRule_intervalUnit = Lens.lens (\RetainRule' {intervalUnit} -> intervalUnit) (\s@RetainRule' {} a -> s {intervalUnit = a} :: RetainRule)

instance Core.FromJSON RetainRule where
  parseJSON =
    Core.withObject
      "RetainRule"
      ( \x ->
          RetainRule'
            Prelude.<$> (x Core..:? "Interval")
            Prelude.<*> (x Core..:? "Count")
            Prelude.<*> (x Core..:? "IntervalUnit")
      )

instance Prelude.Hashable RetainRule where
  hashWithSalt _salt RetainRule' {..} =
    _salt `Prelude.hashWithSalt` interval
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` intervalUnit

instance Prelude.NFData RetainRule where
  rnf RetainRule' {..} =
    Prelude.rnf interval
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf intervalUnit

instance Core.ToJSON RetainRule where
  toJSON RetainRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Interval" Core..=) Prelude.<$> interval,
            ("Count" Core..=) Prelude.<$> count,
            ("IntervalUnit" Core..=) Prelude.<$> intervalUnit
          ]
      )
