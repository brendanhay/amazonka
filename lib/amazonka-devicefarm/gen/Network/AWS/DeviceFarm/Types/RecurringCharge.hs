-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.RecurringCharge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.RecurringCharge
  ( RecurringCharge (..),

    -- * Smart constructor
    mkRecurringCharge,

    -- * Lenses
    rcFrequency,
    rcCost,
  )
where

import Network.AWS.DeviceFarm.Types.MonetaryAmount
import Network.AWS.DeviceFarm.Types.RecurringChargeFrequency
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies whether charges for devices are recurring.
--
-- /See:/ 'mkRecurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { frequency ::
      Lude.Maybe RecurringChargeFrequency,
    cost :: Lude.Maybe MonetaryAmount
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecurringCharge' with the minimum fields required to make a request.
--
-- * 'cost' - The cost of the recurring charge.
-- * 'frequency' - The frequency in which charges recur.
mkRecurringCharge ::
  RecurringCharge
mkRecurringCharge =
  RecurringCharge' {frequency = Lude.Nothing, cost = Lude.Nothing}

-- | The frequency in which charges recur.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcFrequency :: Lens.Lens' RecurringCharge (Lude.Maybe RecurringChargeFrequency)
rcFrequency = Lens.lens (frequency :: RecurringCharge -> Lude.Maybe RecurringChargeFrequency) (\s a -> s {frequency = a} :: RecurringCharge)
{-# DEPRECATED rcFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | The cost of the recurring charge.
--
-- /Note:/ Consider using 'cost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCost :: Lens.Lens' RecurringCharge (Lude.Maybe MonetaryAmount)
rcCost = Lens.lens (cost :: RecurringCharge -> Lude.Maybe MonetaryAmount) (\s a -> s {cost = a} :: RecurringCharge)
{-# DEPRECATED rcCost "Use generic-lens or generic-optics with 'cost' instead." #-}

instance Lude.FromJSON RecurringCharge where
  parseJSON =
    Lude.withObject
      "RecurringCharge"
      ( \x ->
          RecurringCharge'
            Lude.<$> (x Lude..:? "frequency") Lude.<*> (x Lude..:? "cost")
      )
