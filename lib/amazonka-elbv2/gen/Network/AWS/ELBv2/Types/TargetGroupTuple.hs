{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetGroupTuple
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetGroupTuple
  ( TargetGroupTuple (..),

    -- * Smart constructor
    mkTargetGroupTuple,

    -- * Lenses
    tgtTargetGroupArn,
    tgtWeight,
  )
where

import qualified Network.AWS.ELBv2.Types.TargetGroupArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about how traffic will be distributed between multiple target groups in a forward rule.
--
-- /See:/ 'mkTargetGroupTuple' smart constructor.
data TargetGroupTuple = TargetGroupTuple'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Core.Maybe Types.TargetGroupArn,
    -- | The weight. The range is 0 to 999.
    weight :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetGroupTuple' value with any optional fields omitted.
mkTargetGroupTuple ::
  TargetGroupTuple
mkTargetGroupTuple =
  TargetGroupTuple'
    { targetGroupArn = Core.Nothing,
      weight = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgtTargetGroupArn :: Lens.Lens' TargetGroupTuple (Core.Maybe Types.TargetGroupArn)
tgtTargetGroupArn = Lens.field @"targetGroupArn"
{-# DEPRECATED tgtTargetGroupArn "Use generic-lens or generic-optics with 'targetGroupArn' instead." #-}

-- | The weight. The range is 0 to 999.
--
-- /Note:/ Consider using 'weight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgtWeight :: Lens.Lens' TargetGroupTuple (Core.Maybe Core.Int)
tgtWeight = Lens.field @"weight"
{-# DEPRECATED tgtWeight "Use generic-lens or generic-optics with 'weight' instead." #-}

instance Core.FromXML TargetGroupTuple where
  parseXML x =
    TargetGroupTuple'
      Core.<$> (x Core..@? "TargetGroupArn") Core.<*> (x Core..@? "Weight")
