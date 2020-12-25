{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TrialMinutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TrialMinutes
  ( TrialMinutes (..),

    -- * Smart constructor
    mkTrialMinutes,

    -- * Lenses
    tmRemaining,
    tmTotal,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about free trial device minutes for an AWS account.
--
-- /See:/ 'mkTrialMinutes' smart constructor.
data TrialMinutes = TrialMinutes'
  { -- | The number of free trial minutes remaining in the account.
    remaining :: Core.Maybe Core.Double,
    -- | The total number of free trial minutes that the account started with.
    total :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrialMinutes' value with any optional fields omitted.
mkTrialMinutes ::
  TrialMinutes
mkTrialMinutes =
  TrialMinutes' {remaining = Core.Nothing, total = Core.Nothing}

-- | The number of free trial minutes remaining in the account.
--
-- /Note:/ Consider using 'remaining' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmRemaining :: Lens.Lens' TrialMinutes (Core.Maybe Core.Double)
tmRemaining = Lens.field @"remaining"
{-# DEPRECATED tmRemaining "Use generic-lens or generic-optics with 'remaining' instead." #-}

-- | The total number of free trial minutes that the account started with.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmTotal :: Lens.Lens' TrialMinutes (Core.Maybe Core.Double)
tmTotal = Lens.field @"total"
{-# DEPRECATED tmTotal "Use generic-lens or generic-optics with 'total' instead." #-}

instance Core.FromJSON TrialMinutes where
  parseJSON =
    Core.withObject "TrialMinutes" Core.$
      \x ->
        TrialMinutes'
          Core.<$> (x Core..:? "remaining") Core.<*> (x Core..:? "total")
