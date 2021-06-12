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
-- Module      : Network.AWS.DeviceFarm.Types.RecurringCharge
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.RecurringCharge where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types.MonetaryAmount
import Network.AWS.DeviceFarm.Types.RecurringChargeFrequency
import qualified Network.AWS.Lens as Lens

-- | Specifies whether charges for devices are recurring.
--
-- /See:/ 'newRecurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { -- | The cost of the recurring charge.
    cost :: Core.Maybe MonetaryAmount,
    -- | The frequency in which charges recur.
    frequency :: Core.Maybe RecurringChargeFrequency
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RecurringCharge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cost', 'recurringCharge_cost' - The cost of the recurring charge.
--
-- 'frequency', 'recurringCharge_frequency' - The frequency in which charges recur.
newRecurringCharge ::
  RecurringCharge
newRecurringCharge =
  RecurringCharge'
    { cost = Core.Nothing,
      frequency = Core.Nothing
    }

-- | The cost of the recurring charge.
recurringCharge_cost :: Lens.Lens' RecurringCharge (Core.Maybe MonetaryAmount)
recurringCharge_cost = Lens.lens (\RecurringCharge' {cost} -> cost) (\s@RecurringCharge' {} a -> s {cost = a} :: RecurringCharge)

-- | The frequency in which charges recur.
recurringCharge_frequency :: Lens.Lens' RecurringCharge (Core.Maybe RecurringChargeFrequency)
recurringCharge_frequency = Lens.lens (\RecurringCharge' {frequency} -> frequency) (\s@RecurringCharge' {} a -> s {frequency = a} :: RecurringCharge)

instance Core.FromJSON RecurringCharge where
  parseJSON =
    Core.withObject
      "RecurringCharge"
      ( \x ->
          RecurringCharge'
            Core.<$> (x Core..:? "cost")
            Core.<*> (x Core..:? "frequency")
      )

instance Core.Hashable RecurringCharge

instance Core.NFData RecurringCharge
