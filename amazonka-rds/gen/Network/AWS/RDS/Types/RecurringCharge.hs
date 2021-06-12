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
-- Module      : Network.AWS.RDS.Types.RecurringCharge
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.RecurringCharge where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | This data type is used as a response element in the
-- @DescribeReservedDBInstances@ and @DescribeReservedDBInstancesOfferings@
-- actions.
--
-- /See:/ 'newRecurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { -- | The frequency of the recurring charge.
    recurringChargeFrequency :: Core.Maybe Core.Text,
    -- | The amount of the recurring charge.
    recurringChargeAmount :: Core.Maybe Core.Double
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
-- 'recurringChargeFrequency', 'recurringCharge_recurringChargeFrequency' - The frequency of the recurring charge.
--
-- 'recurringChargeAmount', 'recurringCharge_recurringChargeAmount' - The amount of the recurring charge.
newRecurringCharge ::
  RecurringCharge
newRecurringCharge =
  RecurringCharge'
    { recurringChargeFrequency =
        Core.Nothing,
      recurringChargeAmount = Core.Nothing
    }

-- | The frequency of the recurring charge.
recurringCharge_recurringChargeFrequency :: Lens.Lens' RecurringCharge (Core.Maybe Core.Text)
recurringCharge_recurringChargeFrequency = Lens.lens (\RecurringCharge' {recurringChargeFrequency} -> recurringChargeFrequency) (\s@RecurringCharge' {} a -> s {recurringChargeFrequency = a} :: RecurringCharge)

-- | The amount of the recurring charge.
recurringCharge_recurringChargeAmount :: Lens.Lens' RecurringCharge (Core.Maybe Core.Double)
recurringCharge_recurringChargeAmount = Lens.lens (\RecurringCharge' {recurringChargeAmount} -> recurringChargeAmount) (\s@RecurringCharge' {} a -> s {recurringChargeAmount = a} :: RecurringCharge)

instance Core.FromXML RecurringCharge where
  parseXML x =
    RecurringCharge'
      Core.<$> (x Core..@? "RecurringChargeFrequency")
      Core.<*> (x Core..@? "RecurringChargeAmount")

instance Core.Hashable RecurringCharge

instance Core.NFData RecurringCharge
