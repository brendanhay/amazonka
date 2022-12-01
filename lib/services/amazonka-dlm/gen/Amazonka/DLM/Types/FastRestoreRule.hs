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
-- Module      : Amazonka.DLM.Types.FastRestoreRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.FastRestoreRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types.RetentionIntervalUnitValues
import qualified Amazonka.Prelude as Prelude

-- | __[Snapshot policies only]__ Specifies a rule for enabling fast snapshot
-- restore for snapshots created by snapshot policies. You can enable fast
-- snapshot restore based on either a count or a time interval.
--
-- /See:/ 'newFastRestoreRule' smart constructor.
data FastRestoreRule = FastRestoreRule'
  { -- | The amount of time to enable fast snapshot restore. The maximum is 100
    -- years. This is equivalent to 1200 months, 5200 weeks, or 36500 days.
    interval :: Prelude.Maybe Prelude.Natural,
    -- | The number of snapshots to be enabled with fast snapshot restore.
    count :: Prelude.Maybe Prelude.Natural,
    -- | The unit of time for enabling fast snapshot restore.
    intervalUnit :: Prelude.Maybe RetentionIntervalUnitValues,
    -- | The Availability Zones in which to enable fast snapshot restore.
    availabilityZones :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FastRestoreRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'interval', 'fastRestoreRule_interval' - The amount of time to enable fast snapshot restore. The maximum is 100
-- years. This is equivalent to 1200 months, 5200 weeks, or 36500 days.
--
-- 'count', 'fastRestoreRule_count' - The number of snapshots to be enabled with fast snapshot restore.
--
-- 'intervalUnit', 'fastRestoreRule_intervalUnit' - The unit of time for enabling fast snapshot restore.
--
-- 'availabilityZones', 'fastRestoreRule_availabilityZones' - The Availability Zones in which to enable fast snapshot restore.
newFastRestoreRule ::
  -- | 'availabilityZones'
  Prelude.NonEmpty Prelude.Text ->
  FastRestoreRule
newFastRestoreRule pAvailabilityZones_ =
  FastRestoreRule'
    { interval = Prelude.Nothing,
      count = Prelude.Nothing,
      intervalUnit = Prelude.Nothing,
      availabilityZones =
        Lens.coerced Lens.# pAvailabilityZones_
    }

-- | The amount of time to enable fast snapshot restore. The maximum is 100
-- years. This is equivalent to 1200 months, 5200 weeks, or 36500 days.
fastRestoreRule_interval :: Lens.Lens' FastRestoreRule (Prelude.Maybe Prelude.Natural)
fastRestoreRule_interval = Lens.lens (\FastRestoreRule' {interval} -> interval) (\s@FastRestoreRule' {} a -> s {interval = a} :: FastRestoreRule)

-- | The number of snapshots to be enabled with fast snapshot restore.
fastRestoreRule_count :: Lens.Lens' FastRestoreRule (Prelude.Maybe Prelude.Natural)
fastRestoreRule_count = Lens.lens (\FastRestoreRule' {count} -> count) (\s@FastRestoreRule' {} a -> s {count = a} :: FastRestoreRule)

-- | The unit of time for enabling fast snapshot restore.
fastRestoreRule_intervalUnit :: Lens.Lens' FastRestoreRule (Prelude.Maybe RetentionIntervalUnitValues)
fastRestoreRule_intervalUnit = Lens.lens (\FastRestoreRule' {intervalUnit} -> intervalUnit) (\s@FastRestoreRule' {} a -> s {intervalUnit = a} :: FastRestoreRule)

-- | The Availability Zones in which to enable fast snapshot restore.
fastRestoreRule_availabilityZones :: Lens.Lens' FastRestoreRule (Prelude.NonEmpty Prelude.Text)
fastRestoreRule_availabilityZones = Lens.lens (\FastRestoreRule' {availabilityZones} -> availabilityZones) (\s@FastRestoreRule' {} a -> s {availabilityZones = a} :: FastRestoreRule) Prelude.. Lens.coerced

instance Core.FromJSON FastRestoreRule where
  parseJSON =
    Core.withObject
      "FastRestoreRule"
      ( \x ->
          FastRestoreRule'
            Prelude.<$> (x Core..:? "Interval")
            Prelude.<*> (x Core..:? "Count")
            Prelude.<*> (x Core..:? "IntervalUnit")
            Prelude.<*> (x Core..: "AvailabilityZones")
      )

instance Prelude.Hashable FastRestoreRule where
  hashWithSalt _salt FastRestoreRule' {..} =
    _salt `Prelude.hashWithSalt` interval
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` intervalUnit
      `Prelude.hashWithSalt` availabilityZones

instance Prelude.NFData FastRestoreRule where
  rnf FastRestoreRule' {..} =
    Prelude.rnf interval
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf intervalUnit
      `Prelude.seq` Prelude.rnf availabilityZones

instance Core.ToJSON FastRestoreRule where
  toJSON FastRestoreRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Interval" Core..=) Prelude.<$> interval,
            ("Count" Core..=) Prelude.<$> count,
            ("IntervalUnit" Core..=) Prelude.<$> intervalUnit,
            Prelude.Just
              ("AvailabilityZones" Core..= availabilityZones)
          ]
      )
