{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.RecurringCharge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.RecurringCharge
  ( RecurringCharge (..),

    -- * Smart constructor
    mkRecurringCharge,

    -- * Lenses
    rcRecurringChargeFrequency,
    rcRecurringChargeAmount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes a recurring charge.
--
-- /See:/ 'mkRecurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { recurringChargeFrequency ::
      Lude.Maybe Lude.Text,
    recurringChargeAmount :: Lude.Maybe Lude.Double
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
-- * 'recurringChargeAmount' - The amount charged per the period of time specified by the recurring charge frequency.
-- * 'recurringChargeFrequency' - The frequency at which the recurring charge amount is applied.
mkRecurringCharge ::
  RecurringCharge
mkRecurringCharge =
  RecurringCharge'
    { recurringChargeFrequency = Lude.Nothing,
      recurringChargeAmount = Lude.Nothing
    }

-- | The frequency at which the recurring charge amount is applied.
--
-- /Note:/ Consider using 'recurringChargeFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRecurringChargeFrequency :: Lens.Lens' RecurringCharge (Lude.Maybe Lude.Text)
rcRecurringChargeFrequency = Lens.lens (recurringChargeFrequency :: RecurringCharge -> Lude.Maybe Lude.Text) (\s a -> s {recurringChargeFrequency = a} :: RecurringCharge)
{-# DEPRECATED rcRecurringChargeFrequency "Use generic-lens or generic-optics with 'recurringChargeFrequency' instead." #-}

-- | The amount charged per the period of time specified by the recurring charge frequency.
--
-- /Note:/ Consider using 'recurringChargeAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRecurringChargeAmount :: Lens.Lens' RecurringCharge (Lude.Maybe Lude.Double)
rcRecurringChargeAmount = Lens.lens (recurringChargeAmount :: RecurringCharge -> Lude.Maybe Lude.Double) (\s a -> s {recurringChargeAmount = a} :: RecurringCharge)
{-# DEPRECATED rcRecurringChargeAmount "Use generic-lens or generic-optics with 'recurringChargeAmount' instead." #-}

instance Lude.FromXML RecurringCharge where
  parseXML x =
    RecurringCharge'
      Lude.<$> (x Lude..@? "RecurringChargeFrequency")
      Lude.<*> (x Lude..@? "RecurringChargeAmount")
