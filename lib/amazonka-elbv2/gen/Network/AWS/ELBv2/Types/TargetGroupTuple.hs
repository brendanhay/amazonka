{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetGroupTuple
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.TargetGroupTuple
  ( TargetGroupTuple (..)
  -- * Smart constructor
  , mkTargetGroupTuple
  -- * Lenses
  , tgtTargetGroupArn
  , tgtWeight
  ) where

import qualified Network.AWS.ELBv2.Types.TargetGroupArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about how traffic will be distributed between multiple target groups in a forward rule.
--
-- /See:/ 'mkTargetGroupTuple' smart constructor.
data TargetGroupTuple = TargetGroupTuple'
  { targetGroupArn :: Core.Maybe Types.TargetGroupArn
    -- ^ The Amazon Resource Name (ARN) of the target group.
  , weight :: Core.Maybe Core.Int
    -- ^ The weight. The range is 0 to 999.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetGroupTuple' value with any optional fields omitted.
mkTargetGroupTuple
    :: TargetGroupTuple
mkTargetGroupTuple
  = TargetGroupTuple'{targetGroupArn = Core.Nothing,
                      weight = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgtTargetGroupArn :: Lens.Lens' TargetGroupTuple (Core.Maybe Types.TargetGroupArn)
tgtTargetGroupArn = Lens.field @"targetGroupArn"
{-# INLINEABLE tgtTargetGroupArn #-}
{-# DEPRECATED targetGroupArn "Use generic-lens or generic-optics with 'targetGroupArn' instead"  #-}

-- | The weight. The range is 0 to 999.
--
-- /Note:/ Consider using 'weight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgtWeight :: Lens.Lens' TargetGroupTuple (Core.Maybe Core.Int)
tgtWeight = Lens.field @"weight"
{-# INLINEABLE tgtWeight #-}
{-# DEPRECATED weight "Use generic-lens or generic-optics with 'weight' instead"  #-}

instance Core.ToQuery TargetGroupTuple where
        toQuery TargetGroupTuple{..}
          = Core.maybe Core.mempty (Core.toQueryPair "TargetGroupArn")
              targetGroupArn
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Weight") weight

instance Core.FromXML TargetGroupTuple where
        parseXML x
          = TargetGroupTuple' Core.<$>
              (x Core..@? "TargetGroupArn") Core.<*> x Core..@? "Weight"
