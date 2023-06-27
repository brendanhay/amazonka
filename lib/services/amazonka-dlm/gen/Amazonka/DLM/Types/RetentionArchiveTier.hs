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
-- Module      : Amazonka.DLM.Types.RetentionArchiveTier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.RetentionArchiveTier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types.RetentionIntervalUnitValues
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | __[Snapshot policies only]__ Describes the retention rule for archived
-- snapshots. Once the archive retention threshold is met, the snapshots
-- are permanently deleted from the archive tier.
--
-- The archive retention rule must retain snapshots in the archive tier for
-- a minimum of 90 days.
--
-- For __count-based schedules__, you must specify __Count__. For
-- __age-based schedules__, you must specify __Interval__ and
-- __IntervalUnit__.
--
-- For more information about using snapshot archiving, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshot-ami-policy.html#dlm-archive Considerations for snapshot lifecycle policies>.
--
-- /See:/ 'newRetentionArchiveTier' smart constructor.
data RetentionArchiveTier = RetentionArchiveTier'
  { -- | The maximum number of snapshots to retain in the archive storage tier
    -- for each volume. The count must ensure that each snapshot remains in the
    -- archive tier for at least 90 days. For example, if the schedule creates
    -- snapshots every 30 days, you must specify a count of 3 or more to ensure
    -- that each snapshot is archived for at least 90 days.
    count :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the period of time to retain snapshots in the archive tier.
    -- After this period expires, the snapshot is permanently deleted.
    interval :: Prelude.Maybe Prelude.Natural,
    -- | The unit of time in which to measure the __Interval__. For example, to
    -- retain a snapshots in the archive tier for 6 months, specify
    -- @Interval=6@ and @IntervalUnit=MONTHS@.
    intervalUnit :: Prelude.Maybe RetentionIntervalUnitValues
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetentionArchiveTier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'retentionArchiveTier_count' - The maximum number of snapshots to retain in the archive storage tier
-- for each volume. The count must ensure that each snapshot remains in the
-- archive tier for at least 90 days. For example, if the schedule creates
-- snapshots every 30 days, you must specify a count of 3 or more to ensure
-- that each snapshot is archived for at least 90 days.
--
-- 'interval', 'retentionArchiveTier_interval' - Specifies the period of time to retain snapshots in the archive tier.
-- After this period expires, the snapshot is permanently deleted.
--
-- 'intervalUnit', 'retentionArchiveTier_intervalUnit' - The unit of time in which to measure the __Interval__. For example, to
-- retain a snapshots in the archive tier for 6 months, specify
-- @Interval=6@ and @IntervalUnit=MONTHS@.
newRetentionArchiveTier ::
  RetentionArchiveTier
newRetentionArchiveTier =
  RetentionArchiveTier'
    { count = Prelude.Nothing,
      interval = Prelude.Nothing,
      intervalUnit = Prelude.Nothing
    }

-- | The maximum number of snapshots to retain in the archive storage tier
-- for each volume. The count must ensure that each snapshot remains in the
-- archive tier for at least 90 days. For example, if the schedule creates
-- snapshots every 30 days, you must specify a count of 3 or more to ensure
-- that each snapshot is archived for at least 90 days.
retentionArchiveTier_count :: Lens.Lens' RetentionArchiveTier (Prelude.Maybe Prelude.Natural)
retentionArchiveTier_count = Lens.lens (\RetentionArchiveTier' {count} -> count) (\s@RetentionArchiveTier' {} a -> s {count = a} :: RetentionArchiveTier)

-- | Specifies the period of time to retain snapshots in the archive tier.
-- After this period expires, the snapshot is permanently deleted.
retentionArchiveTier_interval :: Lens.Lens' RetentionArchiveTier (Prelude.Maybe Prelude.Natural)
retentionArchiveTier_interval = Lens.lens (\RetentionArchiveTier' {interval} -> interval) (\s@RetentionArchiveTier' {} a -> s {interval = a} :: RetentionArchiveTier)

-- | The unit of time in which to measure the __Interval__. For example, to
-- retain a snapshots in the archive tier for 6 months, specify
-- @Interval=6@ and @IntervalUnit=MONTHS@.
retentionArchiveTier_intervalUnit :: Lens.Lens' RetentionArchiveTier (Prelude.Maybe RetentionIntervalUnitValues)
retentionArchiveTier_intervalUnit = Lens.lens (\RetentionArchiveTier' {intervalUnit} -> intervalUnit) (\s@RetentionArchiveTier' {} a -> s {intervalUnit = a} :: RetentionArchiveTier)

instance Data.FromJSON RetentionArchiveTier where
  parseJSON =
    Data.withObject
      "RetentionArchiveTier"
      ( \x ->
          RetentionArchiveTier'
            Prelude.<$> (x Data..:? "Count")
            Prelude.<*> (x Data..:? "Interval")
            Prelude.<*> (x Data..:? "IntervalUnit")
      )

instance Prelude.Hashable RetentionArchiveTier where
  hashWithSalt _salt RetentionArchiveTier' {..} =
    _salt
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` interval
      `Prelude.hashWithSalt` intervalUnit

instance Prelude.NFData RetentionArchiveTier where
  rnf RetentionArchiveTier' {..} =
    Prelude.rnf count
      `Prelude.seq` Prelude.rnf interval
      `Prelude.seq` Prelude.rnf intervalUnit

instance Data.ToJSON RetentionArchiveTier where
  toJSON RetentionArchiveTier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Count" Data..=) Prelude.<$> count,
            ("Interval" Data..=) Prelude.<$> interval,
            ("IntervalUnit" Data..=) Prelude.<$> intervalUnit
          ]
      )
