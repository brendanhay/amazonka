{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.RecurringCharge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.RecurringCharge
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

-- | Contains the specific price and frequency of a recurring charges for a reserved Elasticsearch instance, or for a reserved Elasticsearch instance offering.
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
-- * 'recurringChargeAmount' - The monetary amount of the recurring charge.
-- * 'recurringChargeFrequency' - The frequency of the recurring charge.
mkRecurringCharge ::
  RecurringCharge
mkRecurringCharge =
  RecurringCharge'
    { recurringChargeFrequency = Lude.Nothing,
      recurringChargeAmount = Lude.Nothing
    }

-- | The frequency of the recurring charge.
--
-- /Note:/ Consider using 'recurringChargeFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRecurringChargeFrequency :: Lens.Lens' RecurringCharge (Lude.Maybe Lude.Text)
rcRecurringChargeFrequency = Lens.lens (recurringChargeFrequency :: RecurringCharge -> Lude.Maybe Lude.Text) (\s a -> s {recurringChargeFrequency = a} :: RecurringCharge)
{-# DEPRECATED rcRecurringChargeFrequency "Use generic-lens or generic-optics with 'recurringChargeFrequency' instead." #-}

-- | The monetary amount of the recurring charge.
--
-- /Note:/ Consider using 'recurringChargeAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRecurringChargeAmount :: Lens.Lens' RecurringCharge (Lude.Maybe Lude.Double)
rcRecurringChargeAmount = Lens.lens (recurringChargeAmount :: RecurringCharge -> Lude.Maybe Lude.Double) (\s a -> s {recurringChargeAmount = a} :: RecurringCharge)
{-# DEPRECATED rcRecurringChargeAmount "Use generic-lens or generic-optics with 'recurringChargeAmount' instead." #-}

instance Lude.FromJSON RecurringCharge where
  parseJSON =
    Lude.withObject
      "RecurringCharge"
      ( \x ->
          RecurringCharge'
            Lude.<$> (x Lude..:? "RecurringChargeFrequency")
            Lude.<*> (x Lude..:? "RecurringChargeAmount")
      )
