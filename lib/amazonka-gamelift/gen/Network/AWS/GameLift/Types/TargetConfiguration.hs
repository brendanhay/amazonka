{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.TargetConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.TargetConfiguration
  ( TargetConfiguration (..),

    -- * Smart constructor
    mkTargetConfiguration,

    -- * Lenses
    tcTargetValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for a target-based scaling policy (see 'ScalingPolicy' . A target-based policy tracks a particular fleet metric specifies a target value for the metric. As player usage changes, the policy triggers Amazon GameLift to adjust capacity so that the metric returns to the target value. The target configuration specifies settings as needed for the target based policy, including the target value.
--
--
--     * 'DescribeFleetCapacity'
--
--
--     * 'UpdateFleetCapacity'
--
--
--     * 'DescribeEC2InstanceLimits'
--
--
--     * Manage scaling policies:
--
--     * 'PutScalingPolicy' (auto-scaling)
--
--
--     * 'DescribeScalingPolicies' (auto-scaling)
--
--
--     * 'DeleteScalingPolicy' (auto-scaling)
--
--
--
--
--     * Manage fleet actions:
--
--     * 'StartFleetActions'
--
--
--     * 'StopFleetActions'
--
--
--
--
--
-- /See:/ 'mkTargetConfiguration' smart constructor.
newtype TargetConfiguration = TargetConfiguration'
  { -- | Desired value to use with a target-based scaling policy. The value must be relevant for whatever metric the scaling policy is using. For example, in a policy using the metric PercentAvailableGameSessions, the target value should be the preferred size of the fleet's buffer (the percent of capacity that should be idle and ready for new game sessions).
    targetValue :: Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TargetConfiguration' value with any optional fields omitted.
mkTargetConfiguration ::
  -- | 'targetValue'
  Core.Double ->
  TargetConfiguration
mkTargetConfiguration targetValue =
  TargetConfiguration' {targetValue}

-- | Desired value to use with a target-based scaling policy. The value must be relevant for whatever metric the scaling policy is using. For example, in a policy using the metric PercentAvailableGameSessions, the target value should be the preferred size of the fleet's buffer (the percent of capacity that should be idle and ready for new game sessions).
--
-- /Note:/ Consider using 'targetValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTargetValue :: Lens.Lens' TargetConfiguration Core.Double
tcTargetValue = Lens.field @"targetValue"
{-# DEPRECATED tcTargetValue "Use generic-lens or generic-optics with 'targetValue' instead." #-}

instance Core.FromJSON TargetConfiguration where
  toJSON TargetConfiguration {..} =
    Core.object
      (Core.catMaybes [Core.Just ("TargetValue" Core..= targetValue)])

instance Core.FromJSON TargetConfiguration where
  parseJSON =
    Core.withObject "TargetConfiguration" Core.$
      \x -> TargetConfiguration' Core.<$> (x Core..: "TargetValue")
