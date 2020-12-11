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
import qualified Network.AWS.Prelude as Lude

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
  { targetValue ::
      Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetConfiguration' with the minimum fields required to make a request.
--
-- * 'targetValue' - Desired value to use with a target-based scaling policy. The value must be relevant for whatever metric the scaling policy is using. For example, in a policy using the metric PercentAvailableGameSessions, the target value should be the preferred size of the fleet's buffer (the percent of capacity that should be idle and ready for new game sessions).
mkTargetConfiguration ::
  -- | 'targetValue'
  Lude.Double ->
  TargetConfiguration
mkTargetConfiguration pTargetValue_ =
  TargetConfiguration' {targetValue = pTargetValue_}

-- | Desired value to use with a target-based scaling policy. The value must be relevant for whatever metric the scaling policy is using. For example, in a policy using the metric PercentAvailableGameSessions, the target value should be the preferred size of the fleet's buffer (the percent of capacity that should be idle and ready for new game sessions).
--
-- /Note:/ Consider using 'targetValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTargetValue :: Lens.Lens' TargetConfiguration Lude.Double
tcTargetValue = Lens.lens (targetValue :: TargetConfiguration -> Lude.Double) (\s a -> s {targetValue = a} :: TargetConfiguration)
{-# DEPRECATED tcTargetValue "Use generic-lens or generic-optics with 'targetValue' instead." #-}

instance Lude.FromJSON TargetConfiguration where
  parseJSON =
    Lude.withObject
      "TargetConfiguration"
      (\x -> TargetConfiguration' Lude.<$> (x Lude..: "TargetValue"))

instance Lude.ToJSON TargetConfiguration where
  toJSON TargetConfiguration' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TargetValue" Lude..= targetValue)])
