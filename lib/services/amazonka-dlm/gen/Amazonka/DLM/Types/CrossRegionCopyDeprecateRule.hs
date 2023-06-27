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
-- Module      : Amazonka.DLM.Types.CrossRegionCopyDeprecateRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.CrossRegionCopyDeprecateRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types.RetentionIntervalUnitValues
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | __[AMI policies only]__ Specifies an AMI deprecation rule for
-- cross-Region AMI copies created by an AMI policy.
--
-- /See:/ 'newCrossRegionCopyDeprecateRule' smart constructor.
data CrossRegionCopyDeprecateRule = CrossRegionCopyDeprecateRule'
  { -- | The period after which to deprecate the cross-Region AMI copies. The
    -- period must be less than or equal to the cross-Region AMI copy retention
    -- period, and it can\'t be greater than 10 years. This is equivalent to
    -- 120 months, 520 weeks, or 3650 days.
    interval :: Prelude.Maybe Prelude.Natural,
    -- | The unit of time in which to measure the __Interval__. For example, to
    -- deprecate a cross-Region AMI copy after 3 months, specify @Interval=3@
    -- and @IntervalUnit=MONTHS@.
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
-- 'intervalUnit', 'crossRegionCopyDeprecateRule_intervalUnit' - The unit of time in which to measure the __Interval__. For example, to
-- deprecate a cross-Region AMI copy after 3 months, specify @Interval=3@
-- and @IntervalUnit=MONTHS@.
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

-- | The unit of time in which to measure the __Interval__. For example, to
-- deprecate a cross-Region AMI copy after 3 months, specify @Interval=3@
-- and @IntervalUnit=MONTHS@.
crossRegionCopyDeprecateRule_intervalUnit :: Lens.Lens' CrossRegionCopyDeprecateRule (Prelude.Maybe RetentionIntervalUnitValues)
crossRegionCopyDeprecateRule_intervalUnit = Lens.lens (\CrossRegionCopyDeprecateRule' {intervalUnit} -> intervalUnit) (\s@CrossRegionCopyDeprecateRule' {} a -> s {intervalUnit = a} :: CrossRegionCopyDeprecateRule)

instance Data.FromJSON CrossRegionCopyDeprecateRule where
  parseJSON =
    Data.withObject
      "CrossRegionCopyDeprecateRule"
      ( \x ->
          CrossRegionCopyDeprecateRule'
            Prelude.<$> (x Data..:? "Interval")
            Prelude.<*> (x Data..:? "IntervalUnit")
      )

instance
  Prelude.Hashable
    CrossRegionCopyDeprecateRule
  where
  hashWithSalt _salt CrossRegionCopyDeprecateRule' {..} =
    _salt
      `Prelude.hashWithSalt` interval
      `Prelude.hashWithSalt` intervalUnit

instance Prelude.NFData CrossRegionCopyDeprecateRule where
  rnf CrossRegionCopyDeprecateRule' {..} =
    Prelude.rnf interval
      `Prelude.seq` Prelude.rnf intervalUnit

instance Data.ToJSON CrossRegionCopyDeprecateRule where
  toJSON CrossRegionCopyDeprecateRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Interval" Data..=) Prelude.<$> interval,
            ("IntervalUnit" Data..=) Prelude.<$> intervalUnit
          ]
      )
