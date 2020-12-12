{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansSavings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansSavings
  ( SavingsPlansSavings (..),

    -- * Smart constructor
    mkSavingsPlansSavings,

    -- * Lenses
    spsNetSavings,
    spsOnDemandCostEquivalent,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The amount of savings you're accumulating, against the public On-Demand rate of the usage accrued in an account.
--
-- /See:/ 'mkSavingsPlansSavings' smart constructor.
data SavingsPlansSavings = SavingsPlansSavings'
  { netSavings ::
      Lude.Maybe Lude.Text,
    onDemandCostEquivalent :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SavingsPlansSavings' with the minimum fields required to make a request.
--
-- * 'netSavings' - The savings amount that you are accumulating for the usage that is covered by a Savings Plans, when compared to the On-Demand equivalent of the same usage.
-- * 'onDemandCostEquivalent' - How much the amount that the usage would have cost if it was accrued at the On-Demand rate.
mkSavingsPlansSavings ::
  SavingsPlansSavings
mkSavingsPlansSavings =
  SavingsPlansSavings'
    { netSavings = Lude.Nothing,
      onDemandCostEquivalent = Lude.Nothing
    }

-- | The savings amount that you are accumulating for the usage that is covered by a Savings Plans, when compared to the On-Demand equivalent of the same usage.
--
-- /Note:/ Consider using 'netSavings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsNetSavings :: Lens.Lens' SavingsPlansSavings (Lude.Maybe Lude.Text)
spsNetSavings = Lens.lens (netSavings :: SavingsPlansSavings -> Lude.Maybe Lude.Text) (\s a -> s {netSavings = a} :: SavingsPlansSavings)
{-# DEPRECATED spsNetSavings "Use generic-lens or generic-optics with 'netSavings' instead." #-}

-- | How much the amount that the usage would have cost if it was accrued at the On-Demand rate.
--
-- /Note:/ Consider using 'onDemandCostEquivalent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsOnDemandCostEquivalent :: Lens.Lens' SavingsPlansSavings (Lude.Maybe Lude.Text)
spsOnDemandCostEquivalent = Lens.lens (onDemandCostEquivalent :: SavingsPlansSavings -> Lude.Maybe Lude.Text) (\s a -> s {onDemandCostEquivalent = a} :: SavingsPlansSavings)
{-# DEPRECATED spsOnDemandCostEquivalent "Use generic-lens or generic-optics with 'onDemandCostEquivalent' instead." #-}

instance Lude.FromJSON SavingsPlansSavings where
  parseJSON =
    Lude.withObject
      "SavingsPlansSavings"
      ( \x ->
          SavingsPlansSavings'
            Lude.<$> (x Lude..:? "NetSavings")
            Lude.<*> (x Lude..:? "OnDemandCostEquivalent")
      )
