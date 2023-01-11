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
-- Module      : Amazonka.DLM.Types.CrossRegionCopyRetainRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.CrossRegionCopyRetainRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types.RetentionIntervalUnitValues
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a retention rule for cross-Region snapshot copies created by
-- snapshot or event-based policies, or cross-Region AMI copies created by
-- AMI policies. After the retention period expires, the cross-Region copy
-- is deleted.
--
-- /See:/ 'newCrossRegionCopyRetainRule' smart constructor.
data CrossRegionCopyRetainRule = CrossRegionCopyRetainRule'
  { -- | The amount of time to retain a cross-Region snapshot or AMI copy. The
    -- maximum is 100 years. This is equivalent to 1200 months, 5200 weeks, or
    -- 36500 days.
    interval :: Prelude.Maybe Prelude.Natural,
    -- | The unit of time for time-based retention. For example, to retain a
    -- cross-Region copy for 3 months, specify @Interval=3@ and
    -- @IntervalUnit=MONTHS@.
    intervalUnit :: Prelude.Maybe RetentionIntervalUnitValues
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CrossRegionCopyRetainRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'interval', 'crossRegionCopyRetainRule_interval' - The amount of time to retain a cross-Region snapshot or AMI copy. The
-- maximum is 100 years. This is equivalent to 1200 months, 5200 weeks, or
-- 36500 days.
--
-- 'intervalUnit', 'crossRegionCopyRetainRule_intervalUnit' - The unit of time for time-based retention. For example, to retain a
-- cross-Region copy for 3 months, specify @Interval=3@ and
-- @IntervalUnit=MONTHS@.
newCrossRegionCopyRetainRule ::
  CrossRegionCopyRetainRule
newCrossRegionCopyRetainRule =
  CrossRegionCopyRetainRule'
    { interval =
        Prelude.Nothing,
      intervalUnit = Prelude.Nothing
    }

-- | The amount of time to retain a cross-Region snapshot or AMI copy. The
-- maximum is 100 years. This is equivalent to 1200 months, 5200 weeks, or
-- 36500 days.
crossRegionCopyRetainRule_interval :: Lens.Lens' CrossRegionCopyRetainRule (Prelude.Maybe Prelude.Natural)
crossRegionCopyRetainRule_interval = Lens.lens (\CrossRegionCopyRetainRule' {interval} -> interval) (\s@CrossRegionCopyRetainRule' {} a -> s {interval = a} :: CrossRegionCopyRetainRule)

-- | The unit of time for time-based retention. For example, to retain a
-- cross-Region copy for 3 months, specify @Interval=3@ and
-- @IntervalUnit=MONTHS@.
crossRegionCopyRetainRule_intervalUnit :: Lens.Lens' CrossRegionCopyRetainRule (Prelude.Maybe RetentionIntervalUnitValues)
crossRegionCopyRetainRule_intervalUnit = Lens.lens (\CrossRegionCopyRetainRule' {intervalUnit} -> intervalUnit) (\s@CrossRegionCopyRetainRule' {} a -> s {intervalUnit = a} :: CrossRegionCopyRetainRule)

instance Data.FromJSON CrossRegionCopyRetainRule where
  parseJSON =
    Data.withObject
      "CrossRegionCopyRetainRule"
      ( \x ->
          CrossRegionCopyRetainRule'
            Prelude.<$> (x Data..:? "Interval")
            Prelude.<*> (x Data..:? "IntervalUnit")
      )

instance Prelude.Hashable CrossRegionCopyRetainRule where
  hashWithSalt _salt CrossRegionCopyRetainRule' {..} =
    _salt `Prelude.hashWithSalt` interval
      `Prelude.hashWithSalt` intervalUnit

instance Prelude.NFData CrossRegionCopyRetainRule where
  rnf CrossRegionCopyRetainRule' {..} =
    Prelude.rnf interval
      `Prelude.seq` Prelude.rnf intervalUnit

instance Data.ToJSON CrossRegionCopyRetainRule where
  toJSON CrossRegionCopyRetainRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Interval" Data..=) Prelude.<$> interval,
            ("IntervalUnit" Data..=) Prelude.<$> intervalUnit
          ]
      )
