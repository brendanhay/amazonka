{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.AutoScalingPolicy
  ( AutoScalingPolicy (..)
  -- * Smart constructor
  , mkAutoScalingPolicy
  -- * Lenses
  , aspConstraints
  , aspRules
  ) where

import qualified Network.AWS.EMR.Types.ScalingConstraints as Types
import qualified Network.AWS.EMR.Types.ScalingRule as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. An automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See 'PutAutoScalingPolicy' .
--
-- /See:/ 'mkAutoScalingPolicy' smart constructor.
data AutoScalingPolicy = AutoScalingPolicy'
  { constraints :: Types.ScalingConstraints
    -- ^ The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
  , rules :: [Types.ScalingRule]
    -- ^ The scale-in and scale-out rules that comprise the automatic scaling policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoScalingPolicy' value with any optional fields omitted.
mkAutoScalingPolicy
    :: Types.ScalingConstraints -- ^ 'constraints'
    -> AutoScalingPolicy
mkAutoScalingPolicy constraints
  = AutoScalingPolicy'{constraints, rules = Core.mempty}

-- | The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
--
-- /Note:/ Consider using 'constraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspConstraints :: Lens.Lens' AutoScalingPolicy Types.ScalingConstraints
aspConstraints = Lens.field @"constraints"
{-# INLINEABLE aspConstraints #-}
{-# DEPRECATED constraints "Use generic-lens or generic-optics with 'constraints' instead"  #-}

-- | The scale-in and scale-out rules that comprise the automatic scaling policy.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspRules :: Lens.Lens' AutoScalingPolicy [Types.ScalingRule]
aspRules = Lens.field @"rules"
{-# INLINEABLE aspRules #-}
{-# DEPRECATED rules "Use generic-lens or generic-optics with 'rules' instead"  #-}

instance Core.FromJSON AutoScalingPolicy where
        toJSON AutoScalingPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Constraints" Core..= constraints),
                  Core.Just ("Rules" Core..= rules)])
