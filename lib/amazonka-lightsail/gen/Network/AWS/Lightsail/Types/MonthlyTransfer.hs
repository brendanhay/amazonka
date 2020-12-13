{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.MonthlyTransfer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.MonthlyTransfer
  ( MonthlyTransfer (..),

    -- * Smart constructor
    mkMonthlyTransfer,

    -- * Lenses
    mtGbPerMonthAllocated,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the monthly data transfer in and out of your virtual private server (or /instance/ ).
--
-- /See:/ 'mkMonthlyTransfer' smart constructor.
newtype MonthlyTransfer = MonthlyTransfer'
  { -- | The amount allocated per month (in GB).
    gbPerMonthAllocated :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonthlyTransfer' with the minimum fields required to make a request.
--
-- * 'gbPerMonthAllocated' - The amount allocated per month (in GB).
mkMonthlyTransfer ::
  MonthlyTransfer
mkMonthlyTransfer =
  MonthlyTransfer' {gbPerMonthAllocated = Lude.Nothing}

-- | The amount allocated per month (in GB).
--
-- /Note:/ Consider using 'gbPerMonthAllocated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtGbPerMonthAllocated :: Lens.Lens' MonthlyTransfer (Lude.Maybe Lude.Int)
mtGbPerMonthAllocated = Lens.lens (gbPerMonthAllocated :: MonthlyTransfer -> Lude.Maybe Lude.Int) (\s a -> s {gbPerMonthAllocated = a} :: MonthlyTransfer)
{-# DEPRECATED mtGbPerMonthAllocated "Use generic-lens or generic-optics with 'gbPerMonthAllocated' instead." #-}

instance Lude.FromJSON MonthlyTransfer where
  parseJSON =
    Lude.withObject
      "MonthlyTransfer"
      ( \x ->
          MonthlyTransfer' Lude.<$> (x Lude..:? "gbPerMonthAllocated")
      )
