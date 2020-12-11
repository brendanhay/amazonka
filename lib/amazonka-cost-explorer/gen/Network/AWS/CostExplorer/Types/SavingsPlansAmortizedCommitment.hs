-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
  ( SavingsPlansAmortizedCommitment (..),

    -- * Smart constructor
    mkSavingsPlansAmortizedCommitment,

    -- * Lenses
    spacAmortizedUpfrontCommitment,
    spacTotalAmortizedCommitment,
    spacAmortizedRecurringCommitment,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The amortized amount of Savings Plans purchased in a specific account during a specific time interval.
--
-- /See:/ 'mkSavingsPlansAmortizedCommitment' smart constructor.
data SavingsPlansAmortizedCommitment = SavingsPlansAmortizedCommitment'
  { amortizedUpfrontCommitment ::
      Lude.Maybe Lude.Text,
    totalAmortizedCommitment ::
      Lude.Maybe Lude.Text,
    amortizedRecurringCommitment ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SavingsPlansAmortizedCommitment' with the minimum fields required to make a request.
--
-- * 'amortizedRecurringCommitment' - The amortized amount of your Savings Plans commitment that was purchased with either a @Partial@ or a @NoUpfront@ .
-- * 'amortizedUpfrontCommitment' - The amortized amount of your Savings Plans commitment that was purchased with an @Upfront@ or @PartialUpfront@ Savings Plans.
-- * 'totalAmortizedCommitment' - The total amortized amount of your Savings Plans commitment, regardless of your Savings Plans purchase method.
mkSavingsPlansAmortizedCommitment ::
  SavingsPlansAmortizedCommitment
mkSavingsPlansAmortizedCommitment =
  SavingsPlansAmortizedCommitment'
    { amortizedUpfrontCommitment =
        Lude.Nothing,
      totalAmortizedCommitment = Lude.Nothing,
      amortizedRecurringCommitment = Lude.Nothing
    }

-- | The amortized amount of your Savings Plans commitment that was purchased with an @Upfront@ or @PartialUpfront@ Savings Plans.
--
-- /Note:/ Consider using 'amortizedUpfrontCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spacAmortizedUpfrontCommitment :: Lens.Lens' SavingsPlansAmortizedCommitment (Lude.Maybe Lude.Text)
spacAmortizedUpfrontCommitment = Lens.lens (amortizedUpfrontCommitment :: SavingsPlansAmortizedCommitment -> Lude.Maybe Lude.Text) (\s a -> s {amortizedUpfrontCommitment = a} :: SavingsPlansAmortizedCommitment)
{-# DEPRECATED spacAmortizedUpfrontCommitment "Use generic-lens or generic-optics with 'amortizedUpfrontCommitment' instead." #-}

-- | The total amortized amount of your Savings Plans commitment, regardless of your Savings Plans purchase method.
--
-- /Note:/ Consider using 'totalAmortizedCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spacTotalAmortizedCommitment :: Lens.Lens' SavingsPlansAmortizedCommitment (Lude.Maybe Lude.Text)
spacTotalAmortizedCommitment = Lens.lens (totalAmortizedCommitment :: SavingsPlansAmortizedCommitment -> Lude.Maybe Lude.Text) (\s a -> s {totalAmortizedCommitment = a} :: SavingsPlansAmortizedCommitment)
{-# DEPRECATED spacTotalAmortizedCommitment "Use generic-lens or generic-optics with 'totalAmortizedCommitment' instead." #-}

-- | The amortized amount of your Savings Plans commitment that was purchased with either a @Partial@ or a @NoUpfront@ .
--
-- /Note:/ Consider using 'amortizedRecurringCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spacAmortizedRecurringCommitment :: Lens.Lens' SavingsPlansAmortizedCommitment (Lude.Maybe Lude.Text)
spacAmortizedRecurringCommitment = Lens.lens (amortizedRecurringCommitment :: SavingsPlansAmortizedCommitment -> Lude.Maybe Lude.Text) (\s a -> s {amortizedRecurringCommitment = a} :: SavingsPlansAmortizedCommitment)
{-# DEPRECATED spacAmortizedRecurringCommitment "Use generic-lens or generic-optics with 'amortizedRecurringCommitment' instead." #-}

instance Lude.FromJSON SavingsPlansAmortizedCommitment where
  parseJSON =
    Lude.withObject
      "SavingsPlansAmortizedCommitment"
      ( \x ->
          SavingsPlansAmortizedCommitment'
            Lude.<$> (x Lude..:? "AmortizedUpfrontCommitment")
            Lude.<*> (x Lude..:? "TotalAmortizedCommitment")
            Lude.<*> (x Lude..:? "AmortizedRecurringCommitment")
      )
