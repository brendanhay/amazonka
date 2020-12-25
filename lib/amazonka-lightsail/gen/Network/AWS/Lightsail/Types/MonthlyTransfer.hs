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
import qualified Network.AWS.Prelude as Core

-- | Describes the monthly data transfer in and out of your virtual private server (or /instance/ ).
--
-- /See:/ 'mkMonthlyTransfer' smart constructor.
newtype MonthlyTransfer = MonthlyTransfer'
  { -- | The amount allocated per month (in GB).
    gbPerMonthAllocated :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MonthlyTransfer' value with any optional fields omitted.
mkMonthlyTransfer ::
  MonthlyTransfer
mkMonthlyTransfer =
  MonthlyTransfer' {gbPerMonthAllocated = Core.Nothing}

-- | The amount allocated per month (in GB).
--
-- /Note:/ Consider using 'gbPerMonthAllocated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtGbPerMonthAllocated :: Lens.Lens' MonthlyTransfer (Core.Maybe Core.Int)
mtGbPerMonthAllocated = Lens.field @"gbPerMonthAllocated"
{-# DEPRECATED mtGbPerMonthAllocated "Use generic-lens or generic-optics with 'gbPerMonthAllocated' instead." #-}

instance Core.FromJSON MonthlyTransfer where
  parseJSON =
    Core.withObject "MonthlyTransfer" Core.$
      \x -> MonthlyTransfer' Core.<$> (x Core..:? "gbPerMonthAllocated")
