{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Spend
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.Spend
  ( Spend (..),

    -- * Smart constructor
    mkSpend,

    -- * Lenses
    sAmount,
    sUnit,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The amount of cost or usage that is measured for a budget.
--
-- For example, a @Spend@ for @3 GB@ of S3 usage would have the following parameters:
--
--     * An @Amount@ of @3@
--
--
--     * A @unit@ of @GB@
--
--
--
-- /See:/ 'mkSpend' smart constructor.
data Spend = Spend'
  { -- | The cost or usage amount that is associated with a budget forecast, actual spend, or budget threshold.
    amount :: Lude.Text,
    -- | The unit of measurement that is used for the budget forecast, actual spend, or budget threshold, such as dollars or GB.
    unit :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Spend' with the minimum fields required to make a request.
--
-- * 'amount' - The cost or usage amount that is associated with a budget forecast, actual spend, or budget threshold.
-- * 'unit' - The unit of measurement that is used for the budget forecast, actual spend, or budget threshold, such as dollars or GB.
mkSpend ::
  -- | 'amount'
  Lude.Text ->
  -- | 'unit'
  Lude.Text ->
  Spend
mkSpend pAmount_ pUnit_ = Spend' {amount = pAmount_, unit = pUnit_}

-- | The cost or usage amount that is associated with a budget forecast, actual spend, or budget threshold.
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAmount :: Lens.Lens' Spend Lude.Text
sAmount = Lens.lens (amount :: Spend -> Lude.Text) (\s a -> s {amount = a} :: Spend)
{-# DEPRECATED sAmount "Use generic-lens or generic-optics with 'amount' instead." #-}

-- | The unit of measurement that is used for the budget forecast, actual spend, or budget threshold, such as dollars or GB.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUnit :: Lens.Lens' Spend Lude.Text
sUnit = Lens.lens (unit :: Spend -> Lude.Text) (\s a -> s {unit = a} :: Spend)
{-# DEPRECATED sUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Lude.FromJSON Spend where
  parseJSON =
    Lude.withObject
      "Spend"
      ( \x ->
          Spend' Lude.<$> (x Lude..: "Amount") Lude.<*> (x Lude..: "Unit")
      )

instance Lude.ToJSON Spend where
  toJSON Spend' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Amount" Lude..= amount),
            Lude.Just ("Unit" Lude..= unit)
          ]
      )
