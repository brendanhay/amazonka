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
-- Module      : Network.AWS.DLM.Types.DeprecateRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DLM.Types.DeprecateRule where

import qualified Network.AWS.Core as Core
import Network.AWS.DLM.Types.RetentionIntervalUnitValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies an AMI deprecation rule for a schedule.
--
-- /See:/ 'newDeprecateRule' smart constructor.
data DeprecateRule = DeprecateRule'
  { -- | If the schedule has a count-based retention rule, this parameter
    -- specifies the number of oldest AMIs to deprecate. The count must be less
    -- than or equal to the schedule\'s retention count, and it can\'t be
    -- greater than 1000.
    count :: Prelude.Maybe Prelude.Natural,
    -- | If the schedule has an age-based retention rule, this parameter
    -- specifies the period after which to deprecate AMIs created by the
    -- schedule. The period must be less than or equal to the schedule\'s
    -- retention period, and it can\'t be greater than 10 years. This is
    -- equivalent to 120 months, 520 weeks, or 3650 days.
    interval :: Prelude.Maybe Prelude.Natural,
    -- | The unit of time in which to measure the __Interval__.
    intervalUnit :: Prelude.Maybe RetentionIntervalUnitValues
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeprecateRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'deprecateRule_count' - If the schedule has a count-based retention rule, this parameter
-- specifies the number of oldest AMIs to deprecate. The count must be less
-- than or equal to the schedule\'s retention count, and it can\'t be
-- greater than 1000.
--
-- 'interval', 'deprecateRule_interval' - If the schedule has an age-based retention rule, this parameter
-- specifies the period after which to deprecate AMIs created by the
-- schedule. The period must be less than or equal to the schedule\'s
-- retention period, and it can\'t be greater than 10 years. This is
-- equivalent to 120 months, 520 weeks, or 3650 days.
--
-- 'intervalUnit', 'deprecateRule_intervalUnit' - The unit of time in which to measure the __Interval__.
newDeprecateRule ::
  DeprecateRule
newDeprecateRule =
  DeprecateRule'
    { count = Prelude.Nothing,
      interval = Prelude.Nothing,
      intervalUnit = Prelude.Nothing
    }

-- | If the schedule has a count-based retention rule, this parameter
-- specifies the number of oldest AMIs to deprecate. The count must be less
-- than or equal to the schedule\'s retention count, and it can\'t be
-- greater than 1000.
deprecateRule_count :: Lens.Lens' DeprecateRule (Prelude.Maybe Prelude.Natural)
deprecateRule_count = Lens.lens (\DeprecateRule' {count} -> count) (\s@DeprecateRule' {} a -> s {count = a} :: DeprecateRule)

-- | If the schedule has an age-based retention rule, this parameter
-- specifies the period after which to deprecate AMIs created by the
-- schedule. The period must be less than or equal to the schedule\'s
-- retention period, and it can\'t be greater than 10 years. This is
-- equivalent to 120 months, 520 weeks, or 3650 days.
deprecateRule_interval :: Lens.Lens' DeprecateRule (Prelude.Maybe Prelude.Natural)
deprecateRule_interval = Lens.lens (\DeprecateRule' {interval} -> interval) (\s@DeprecateRule' {} a -> s {interval = a} :: DeprecateRule)

-- | The unit of time in which to measure the __Interval__.
deprecateRule_intervalUnit :: Lens.Lens' DeprecateRule (Prelude.Maybe RetentionIntervalUnitValues)
deprecateRule_intervalUnit = Lens.lens (\DeprecateRule' {intervalUnit} -> intervalUnit) (\s@DeprecateRule' {} a -> s {intervalUnit = a} :: DeprecateRule)

instance Core.FromJSON DeprecateRule where
  parseJSON =
    Core.withObject
      "DeprecateRule"
      ( \x ->
          DeprecateRule'
            Prelude.<$> (x Core..:? "Count")
            Prelude.<*> (x Core..:? "Interval")
            Prelude.<*> (x Core..:? "IntervalUnit")
      )

instance Prelude.Hashable DeprecateRule

instance Prelude.NFData DeprecateRule

instance Core.ToJSON DeprecateRule where
  toJSON DeprecateRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Count" Core..=) Prelude.<$> count,
            ("Interval" Core..=) Prelude.<$> interval,
            ("IntervalUnit" Core..=) Prelude.<$> intervalUnit
          ]
      )
