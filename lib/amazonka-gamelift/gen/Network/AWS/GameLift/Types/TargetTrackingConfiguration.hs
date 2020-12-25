{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.TargetTrackingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.TargetTrackingConfiguration
  ( TargetTrackingConfiguration (..),

    -- * Smart constructor
    mkTargetTrackingConfiguration,

    -- * Lenses
    ttcTargetValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | __This data type is used with the Amazon GameLift FleetIQ and game server groups.__
--
-- Settings for a target-based scaling policy as part of a 'GameServerGroupAutoScalingPolicy' . These settings are used to create a target-based policy that tracks the GameLift FleetIQ metric @"PercentUtilizedGameServers"@ and specifies a target value for the metric. As player usage changes, the policy triggers to adjust the game server group capacity so that the metric returns to the target value.
--
-- /See:/ 'mkTargetTrackingConfiguration' smart constructor.
newtype TargetTrackingConfiguration = TargetTrackingConfiguration'
  { -- | Desired value to use with a game server group target-based scaling policy.
    targetValue :: Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TargetTrackingConfiguration' value with any optional fields omitted.
mkTargetTrackingConfiguration ::
  -- | 'targetValue'
  Core.Double ->
  TargetTrackingConfiguration
mkTargetTrackingConfiguration targetValue =
  TargetTrackingConfiguration' {targetValue}

-- | Desired value to use with a game server group target-based scaling policy.
--
-- /Note:/ Consider using 'targetValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttcTargetValue :: Lens.Lens' TargetTrackingConfiguration Core.Double
ttcTargetValue = Lens.field @"targetValue"
{-# DEPRECATED ttcTargetValue "Use generic-lens or generic-optics with 'targetValue' instead." #-}

instance Core.FromJSON TargetTrackingConfiguration where
  toJSON TargetTrackingConfiguration {..} =
    Core.object
      (Core.catMaybes [Core.Just ("TargetValue" Core..= targetValue)])
