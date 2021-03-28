{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ScalingConstraints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ScalingConstraints
  ( ScalingConstraints (..)
  -- * Smart constructor
  , mkScalingConstraints
  -- * Lenses
  , scMinCapacity
  , scMaxCapacity
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activities triggered by automatic scaling rules will not cause an instance group to grow above or below these limits.
--
-- /See:/ 'mkScalingConstraints' smart constructor.
data ScalingConstraints = ScalingConstraints'
  { minCapacity :: Core.Int
    -- ^ The lower boundary of EC2 instances in an instance group below which scaling activities are not allowed to shrink. Scale-in activities will not terminate instances below this boundary.
  , maxCapacity :: Core.Int
    -- ^ The upper boundary of EC2 instances in an instance group beyond which scaling activities are not allowed to grow. Scale-out activities will not add instances beyond this boundary.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScalingConstraints' value with any optional fields omitted.
mkScalingConstraints
    :: Core.Int -- ^ 'minCapacity'
    -> Core.Int -- ^ 'maxCapacity'
    -> ScalingConstraints
mkScalingConstraints minCapacity maxCapacity
  = ScalingConstraints'{minCapacity, maxCapacity}

-- | The lower boundary of EC2 instances in an instance group below which scaling activities are not allowed to shrink. Scale-in activities will not terminate instances below this boundary.
--
-- /Note:/ Consider using 'minCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMinCapacity :: Lens.Lens' ScalingConstraints Core.Int
scMinCapacity = Lens.field @"minCapacity"
{-# INLINEABLE scMinCapacity #-}
{-# DEPRECATED minCapacity "Use generic-lens or generic-optics with 'minCapacity' instead"  #-}

-- | The upper boundary of EC2 instances in an instance group beyond which scaling activities are not allowed to grow. Scale-out activities will not add instances beyond this boundary.
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMaxCapacity :: Lens.Lens' ScalingConstraints Core.Int
scMaxCapacity = Lens.field @"maxCapacity"
{-# INLINEABLE scMaxCapacity #-}
{-# DEPRECATED maxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead"  #-}

instance Core.FromJSON ScalingConstraints where
        toJSON ScalingConstraints{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MinCapacity" Core..= minCapacity),
                  Core.Just ("MaxCapacity" Core..= maxCapacity)])

instance Core.FromJSON ScalingConstraints where
        parseJSON
          = Core.withObject "ScalingConstraints" Core.$
              \ x ->
                ScalingConstraints' Core.<$>
                  (x Core..: "MinCapacity") Core.<*> x Core..: "MaxCapacity"
