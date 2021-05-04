{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DeviceFarm.Types.MonetaryAmount
import Network.AWS.DeviceFarm.Types.RecurringChargeFrequency
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies whether charges for devices are recurring.
--
-- /See:/ 'newRecurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { -- | The cost of the recurring charge.
    cost :: Prelude.Maybe MonetaryAmount,
    -- | The frequency in which charges recur.
    frequency :: Prelude.Maybe RecurringChargeFrequency
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { cost = Prelude.Nothing,
      frequency = Prelude.Nothing
    }

-- | The cost of the recurring charge.
recurringCharge_cost :: Lens.Lens' RecurringCharge (Prelude.Maybe MonetaryAmount)
recurringCharge_cost = Lens.lens (\RecurringCharge' {cost} -> cost) (\s@RecurringCharge' {} a -> s {cost = a} :: RecurringCharge)

-- | The frequency in which charges recur.
recurringCharge_frequency :: Lens.Lens' RecurringCharge (Prelude.Maybe RecurringChargeFrequency)
recurringCharge_frequency = Lens.lens (\RecurringCharge' {frequency} -> frequency) (\s@RecurringCharge' {} a -> s {frequency = a} :: RecurringCharge)

instance Prelude.FromJSON RecurringCharge where
  parseJSON =
    Prelude.withObject
      "RecurringCharge"
      ( \x ->
          RecurringCharge'
            Prelude.<$> (x Prelude..:? "cost")
            Prelude.<*> (x Prelude..:? "frequency")
      )

instance Prelude.Hashable RecurringCharge

instance Prelude.NFData RecurringCharge
