{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Total
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Total
  ( Total (..),

    -- * Smart constructor
    mkTotal,

    -- * Lenses
    tAmount,
    tUnit,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the total usage with the corresponding currency unit for that value.
--
-- /See:/ 'mkTotal' smart constructor.
data Total = Total'
  { -- | The total usage.
    amount :: Lude.Maybe Lude.Text,
    -- | The currency unit that the amount is given in.
    unit :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Total' with the minimum fields required to make a request.
--
-- * 'amount' - The total usage.
-- * 'unit' - The currency unit that the amount is given in.
mkTotal ::
  Total
mkTotal = Total' {amount = Lude.Nothing, unit = Lude.Nothing}

-- | The total usage.
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tAmount :: Lens.Lens' Total (Lude.Maybe Lude.Text)
tAmount = Lens.lens (amount :: Total -> Lude.Maybe Lude.Text) (\s a -> s {amount = a} :: Total)
{-# DEPRECATED tAmount "Use generic-lens or generic-optics with 'amount' instead." #-}

-- | The currency unit that the amount is given in.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tUnit :: Lens.Lens' Total (Lude.Maybe Lude.Text)
tUnit = Lens.lens (unit :: Total -> Lude.Maybe Lude.Text) (\s a -> s {unit = a} :: Total)
{-# DEPRECATED tUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Lude.FromJSON Total where
  parseJSON =
    Lude.withObject
      "Total"
      ( \x ->
          Total' Lude.<$> (x Lude..:? "amount") Lude.<*> (x Lude..:? "unit")
      )
