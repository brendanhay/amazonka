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
-- Module      : Network.AWS.EC2.Types.RecurringCharge
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RecurringCharge where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.RecurringChargeFrequency
import qualified Network.AWS.Lens as Lens

-- | Describes a recurring charge.
--
-- /See:/ 'newRecurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { -- | The amount of the recurring charge.
    amount :: Core.Maybe Core.Double,
    -- | The frequency of the recurring charge.
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
-- 'amount', 'recurringCharge_amount' - The amount of the recurring charge.
--
-- 'frequency', 'recurringCharge_frequency' - The frequency of the recurring charge.
newRecurringCharge ::
  RecurringCharge
newRecurringCharge =
  RecurringCharge'
    { amount = Core.Nothing,
      frequency = Core.Nothing
    }

-- | The amount of the recurring charge.
recurringCharge_amount :: Lens.Lens' RecurringCharge (Core.Maybe Core.Double)
recurringCharge_amount = Lens.lens (\RecurringCharge' {amount} -> amount) (\s@RecurringCharge' {} a -> s {amount = a} :: RecurringCharge)

-- | The frequency of the recurring charge.
recurringCharge_frequency :: Lens.Lens' RecurringCharge (Core.Maybe RecurringChargeFrequency)
recurringCharge_frequency = Lens.lens (\RecurringCharge' {frequency} -> frequency) (\s@RecurringCharge' {} a -> s {frequency = a} :: RecurringCharge)

instance Core.FromXML RecurringCharge where
  parseXML x =
    RecurringCharge'
      Core.<$> (x Core..@? "amount")
      Core.<*> (x Core..@? "frequency")

instance Core.Hashable RecurringCharge

instance Core.NFData RecurringCharge
