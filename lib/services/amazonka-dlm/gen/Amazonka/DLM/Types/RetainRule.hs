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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.RetainRule where

import qualified Amazonka.Core as Core
import Amazonka.DLM.Types.RetentionIntervalUnitValues
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the retention rule for a lifecycle policy. You can retain
-- snapshots based on either a count or a time interval.
--
-- /See:/ 'newRetainRule' smart constructor.
data RetainRule = RetainRule'
  { -- | The number of snapshots to retain for each volume, up to a maximum of
    -- 1000.
    count :: Prelude.Maybe Prelude.Natural,
    -- | The amount of time to retain each snapshot. The maximum is 100 years.
    -- This is equivalent to 1200 months, 5200 weeks, or 36500 days.
    interval :: Prelude.Maybe Prelude.Natural,
    -- | The unit of time for time-based retention.
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
-- 'count', 'retainRule_count' - The number of snapshots to retain for each volume, up to a maximum of
-- 1000.
--
-- 'interval', 'retainRule_interval' - The amount of time to retain each snapshot. The maximum is 100 years.
-- This is equivalent to 1200 months, 5200 weeks, or 36500 days.
--
-- 'intervalUnit', 'retainRule_intervalUnit' - The unit of time for time-based retention.
newRetainRule ::
  RetainRule
newRetainRule =
  RetainRule'
    { count = Prelude.Nothing,
      interval = Prelude.Nothing,
      intervalUnit = Prelude.Nothing
    }

-- | The number of snapshots to retain for each volume, up to a maximum of
-- 1000.
retainRule_count :: Lens.Lens' RetainRule (Prelude.Maybe Prelude.Natural)
retainRule_count = Lens.lens (\RetainRule' {count} -> count) (\s@RetainRule' {} a -> s {count = a} :: RetainRule)

-- | The amount of time to retain each snapshot. The maximum is 100 years.
-- This is equivalent to 1200 months, 5200 weeks, or 36500 days.
retainRule_interval :: Lens.Lens' RetainRule (Prelude.Maybe Prelude.Natural)
retainRule_interval = Lens.lens (\RetainRule' {interval} -> interval) (\s@RetainRule' {} a -> s {interval = a} :: RetainRule)

-- | The unit of time for time-based retention.
retainRule_intervalUnit :: Lens.Lens' RetainRule (Prelude.Maybe RetentionIntervalUnitValues)
retainRule_intervalUnit = Lens.lens (\RetainRule' {intervalUnit} -> intervalUnit) (\s@RetainRule' {} a -> s {intervalUnit = a} :: RetainRule)

instance Core.FromJSON RetainRule where
  parseJSON =
    Core.withObject
      "RetainRule"
      ( \x ->
          RetainRule'
            Prelude.<$> (x Core..:? "Count")
            Prelude.<*> (x Core..:? "Interval")
            Prelude.<*> (x Core..:? "IntervalUnit")
      )

instance Prelude.Hashable RetainRule where
  hashWithSalt _salt RetainRule' {..} =
    _salt `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` interval
      `Prelude.hashWithSalt` intervalUnit

instance Prelude.NFData RetainRule where
  rnf RetainRule' {..} =
    Prelude.rnf count
      `Prelude.seq` Prelude.rnf interval
      `Prelude.seq` Prelude.rnf intervalUnit

instance Core.ToJSON RetainRule where
  toJSON RetainRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Count" Core..=) Prelude.<$> count,
            ("Interval" Core..=) Prelude.<$> interval,
            ("IntervalUnit" Core..=) Prelude.<$> intervalUnit
          ]
      )
