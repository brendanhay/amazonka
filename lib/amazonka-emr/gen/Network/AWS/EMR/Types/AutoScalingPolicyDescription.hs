{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicyDescription
  ( AutoScalingPolicyDescription (..),

    -- * Smart constructor
    mkAutoScalingPolicyDescription,

    -- * Lenses
    aspdConstraints,
    aspdRules,
    aspdStatus,
  )
where

import qualified Network.AWS.EMR.Types.AutoScalingPolicyStatus as Types
import qualified Network.AWS.EMR.Types.ScalingConstraints as Types
import qualified Network.AWS.EMR.Types.ScalingRule as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See 'PutAutoScalingPolicy' .
--
-- /See:/ 'mkAutoScalingPolicyDescription' smart constructor.
data AutoScalingPolicyDescription = AutoScalingPolicyDescription'
  { -- | The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
    constraints :: Core.Maybe Types.ScalingConstraints,
    -- | The scale-in and scale-out rules that comprise the automatic scaling policy.
    rules :: Core.Maybe [Types.ScalingRule],
    -- | The status of an automatic scaling policy.
    status :: Core.Maybe Types.AutoScalingPolicyStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoScalingPolicyDescription' value with any optional fields omitted.
mkAutoScalingPolicyDescription ::
  AutoScalingPolicyDescription
mkAutoScalingPolicyDescription =
  AutoScalingPolicyDescription'
    { constraints = Core.Nothing,
      rules = Core.Nothing,
      status = Core.Nothing
    }

-- | The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
--
-- /Note:/ Consider using 'constraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspdConstraints :: Lens.Lens' AutoScalingPolicyDescription (Core.Maybe Types.ScalingConstraints)
aspdConstraints = Lens.field @"constraints"
{-# DEPRECATED aspdConstraints "Use generic-lens or generic-optics with 'constraints' instead." #-}

-- | The scale-in and scale-out rules that comprise the automatic scaling policy.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspdRules :: Lens.Lens' AutoScalingPolicyDescription (Core.Maybe [Types.ScalingRule])
aspdRules = Lens.field @"rules"
{-# DEPRECATED aspdRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The status of an automatic scaling policy.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspdStatus :: Lens.Lens' AutoScalingPolicyDescription (Core.Maybe Types.AutoScalingPolicyStatus)
aspdStatus = Lens.field @"status"
{-# DEPRECATED aspdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON AutoScalingPolicyDescription where
  parseJSON =
    Core.withObject "AutoScalingPolicyDescription" Core.$
      \x ->
        AutoScalingPolicyDescription'
          Core.<$> (x Core..:? "Constraints")
          Core.<*> (x Core..:? "Rules")
          Core.<*> (x Core..:? "Status")
