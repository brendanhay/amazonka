{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RecurringCharge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RecurringCharge
  ( RecurringCharge (..),

    -- * Smart constructor
    mkRecurringCharge,

    -- * Lenses
    rcAmount,
    rcFrequency,
  )
where

import Network.AWS.EC2.Types.RecurringChargeFrequency
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a recurring charge.
--
-- /See:/ 'mkRecurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { -- | The amount of the recurring charge.
    amount :: Lude.Maybe Lude.Double,
    -- | The frequency of the recurring charge.
    frequency :: Lude.Maybe RecurringChargeFrequency
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecurringCharge' with the minimum fields required to make a request.
--
-- * 'amount' - The amount of the recurring charge.
-- * 'frequency' - The frequency of the recurring charge.
mkRecurringCharge ::
  RecurringCharge
mkRecurringCharge =
  RecurringCharge' {amount = Lude.Nothing, frequency = Lude.Nothing}

-- | The amount of the recurring charge.
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcAmount :: Lens.Lens' RecurringCharge (Lude.Maybe Lude.Double)
rcAmount = Lens.lens (amount :: RecurringCharge -> Lude.Maybe Lude.Double) (\s a -> s {amount = a} :: RecurringCharge)
{-# DEPRECATED rcAmount "Use generic-lens or generic-optics with 'amount' instead." #-}

-- | The frequency of the recurring charge.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcFrequency :: Lens.Lens' RecurringCharge (Lude.Maybe RecurringChargeFrequency)
rcFrequency = Lens.lens (frequency :: RecurringCharge -> Lude.Maybe RecurringChargeFrequency) (\s a -> s {frequency = a} :: RecurringCharge)
{-# DEPRECATED rcFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

instance Lude.FromXML RecurringCharge where
  parseXML x =
    RecurringCharge'
      Lude.<$> (x Lude..@? "amount") Lude.<*> (x Lude..@? "frequency")
