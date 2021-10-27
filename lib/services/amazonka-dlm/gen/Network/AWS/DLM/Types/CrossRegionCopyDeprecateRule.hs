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
-- Module      : Network.AWS.DLM.Types.CrossRegionCopyDeprecateRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DLM.Types.CrossRegionCopyDeprecateRule where

import qualified Network.AWS.Core as Core
import Network.AWS.DLM.Types.RetentionIntervalUnitValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies an AMI deprecation rule for cross-Region AMI copies created by
-- a cross-Region copy rule.
--
-- /See:/ 'newCrossRegionCopyDeprecateRule' smart constructor.
data CrossRegionCopyDeprecateRule = CrossRegionCopyDeprecateRule'
  { -- | The period after which to deprecate the cross-Region AMI copies. The
    -- period must be less than or equal to the cross-Region AMI copy retention
    -- period, and it can\'t be greater than 10 years. This is equivalent to
    -- 120 months, 520 weeks, or 3650 days.
    interval :: Prelude.Maybe Prelude.Natural,
    -- | The unit of time in which to measure the __Interval__.
    intervalUnit :: Prelude.Maybe RetentionIntervalUnitValues
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CrossRegionCopyDeprecateRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'interval', 'crossRegionCopyDeprecateRule_interval' - The period after which to deprecate the cross-Region AMI copies. The
-- period must be less than or equal to the cross-Region AMI copy retention
-- period, and it can\'t be greater than 10 years. This is equivalent to
-- 120 months, 520 weeks, or 3650 days.
--
-- 'intervalUnit', 'crossRegionCopyDeprecateRule_intervalUnit' - The unit of time in which to measure the __Interval__.
newCrossRegionCopyDeprecateRule ::
  CrossRegionCopyDeprecateRule
newCrossRegionCopyDeprecateRule =
  CrossRegionCopyDeprecateRule'
    { interval =
        Prelude.Nothing,
      intervalUnit = Prelude.Nothing
    }

-- | The period after which to deprecate the cross-Region AMI copies. The
-- period must be less than or equal to the cross-Region AMI copy retention
-- period, and it can\'t be greater than 10 years. This is equivalent to
-- 120 months, 520 weeks, or 3650 days.
crossRegionCopyDeprecateRule_interval :: Lens.Lens' CrossRegionCopyDeprecateRule (Prelude.Maybe Prelude.Natural)
crossRegionCopyDeprecateRule_interval = Lens.lens (\CrossRegionCopyDeprecateRule' {interval} -> interval) (\s@CrossRegionCopyDeprecateRule' {} a -> s {interval = a} :: CrossRegionCopyDeprecateRule)

-- | The unit of time in which to measure the __Interval__.
crossRegionCopyDeprecateRule_intervalUnit :: Lens.Lens' CrossRegionCopyDeprecateRule (Prelude.Maybe RetentionIntervalUnitValues)
crossRegionCopyDeprecateRule_intervalUnit = Lens.lens (\CrossRegionCopyDeprecateRule' {intervalUnit} -> intervalUnit) (\s@CrossRegionCopyDeprecateRule' {} a -> s {intervalUnit = a} :: CrossRegionCopyDeprecateRule)

instance Core.FromJSON CrossRegionCopyDeprecateRule where
  parseJSON =
    Core.withObject
      "CrossRegionCopyDeprecateRule"
      ( \x ->
          CrossRegionCopyDeprecateRule'
            Prelude.<$> (x Core..:? "Interval")
            Prelude.<*> (x Core..:? "IntervalUnit")
      )

instance
  Prelude.Hashable
    CrossRegionCopyDeprecateRule

instance Prelude.NFData CrossRegionCopyDeprecateRule

instance Core.ToJSON CrossRegionCopyDeprecateRule where
  toJSON CrossRegionCopyDeprecateRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Interval" Core..=) Prelude.<$> interval,
            ("IntervalUnit" Core..=) Prelude.<$> intervalUnit
          ]
      )
