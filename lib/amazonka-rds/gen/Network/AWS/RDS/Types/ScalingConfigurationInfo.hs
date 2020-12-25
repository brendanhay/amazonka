{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ScalingConfigurationInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ScalingConfigurationInfo
  ( ScalingConfigurationInfo (..),

    -- * Smart constructor
    mkScalingConfigurationInfo,

    -- * Lenses
    sciAutoPause,
    sciMaxCapacity,
    sciMinCapacity,
    sciSecondsUntilAutoPause,
    sciTimeoutAction,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | Shows the scaling configuration for an Aurora DB cluster in @serverless@ DB engine mode.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
-- /See:/ 'mkScalingConfigurationInfo' smart constructor.
data ScalingConfigurationInfo = ScalingConfigurationInfo'
  { -- | A value that indicates whether automatic pause is allowed for the Aurora DB cluster in @serverless@ DB engine mode.
    --
    -- When the value is set to false for an Aurora Serverless DB cluster, the DB cluster automatically resumes.
    autoPause :: Core.Maybe Core.Bool,
    -- | The maximum capacity for an Aurora DB cluster in @serverless@ DB engine mode.
    maxCapacity :: Core.Maybe Core.Int,
    -- | The maximum capacity for the Aurora DB cluster in @serverless@ DB engine mode.
    minCapacity :: Core.Maybe Core.Int,
    -- | The remaining amount of time, in seconds, before the Aurora DB cluster in @serverless@ mode is paused. A DB cluster can be paused only when it's idle (it has no connections).
    secondsUntilAutoPause :: Core.Maybe Core.Int,
    -- | The timeout action of a call to @ModifyCurrentDBClusterCapacity@ , either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
    timeoutAction :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScalingConfigurationInfo' value with any optional fields omitted.
mkScalingConfigurationInfo ::
  ScalingConfigurationInfo
mkScalingConfigurationInfo =
  ScalingConfigurationInfo'
    { autoPause = Core.Nothing,
      maxCapacity = Core.Nothing,
      minCapacity = Core.Nothing,
      secondsUntilAutoPause = Core.Nothing,
      timeoutAction = Core.Nothing
    }

-- | A value that indicates whether automatic pause is allowed for the Aurora DB cluster in @serverless@ DB engine mode.
--
-- When the value is set to false for an Aurora Serverless DB cluster, the DB cluster automatically resumes.
--
-- /Note:/ Consider using 'autoPause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sciAutoPause :: Lens.Lens' ScalingConfigurationInfo (Core.Maybe Core.Bool)
sciAutoPause = Lens.field @"autoPause"
{-# DEPRECATED sciAutoPause "Use generic-lens or generic-optics with 'autoPause' instead." #-}

-- | The maximum capacity for an Aurora DB cluster in @serverless@ DB engine mode.
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sciMaxCapacity :: Lens.Lens' ScalingConfigurationInfo (Core.Maybe Core.Int)
sciMaxCapacity = Lens.field @"maxCapacity"
{-# DEPRECATED sciMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | The maximum capacity for the Aurora DB cluster in @serverless@ DB engine mode.
--
-- /Note:/ Consider using 'minCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sciMinCapacity :: Lens.Lens' ScalingConfigurationInfo (Core.Maybe Core.Int)
sciMinCapacity = Lens.field @"minCapacity"
{-# DEPRECATED sciMinCapacity "Use generic-lens or generic-optics with 'minCapacity' instead." #-}

-- | The remaining amount of time, in seconds, before the Aurora DB cluster in @serverless@ mode is paused. A DB cluster can be paused only when it's idle (it has no connections).
--
-- /Note:/ Consider using 'secondsUntilAutoPause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sciSecondsUntilAutoPause :: Lens.Lens' ScalingConfigurationInfo (Core.Maybe Core.Int)
sciSecondsUntilAutoPause = Lens.field @"secondsUntilAutoPause"
{-# DEPRECATED sciSecondsUntilAutoPause "Use generic-lens or generic-optics with 'secondsUntilAutoPause' instead." #-}

-- | The timeout action of a call to @ModifyCurrentDBClusterCapacity@ , either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
--
-- /Note:/ Consider using 'timeoutAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sciTimeoutAction :: Lens.Lens' ScalingConfigurationInfo (Core.Maybe Types.String)
sciTimeoutAction = Lens.field @"timeoutAction"
{-# DEPRECATED sciTimeoutAction "Use generic-lens or generic-optics with 'timeoutAction' instead." #-}

instance Core.FromXML ScalingConfigurationInfo where
  parseXML x =
    ScalingConfigurationInfo'
      Core.<$> (x Core..@? "AutoPause")
      Core.<*> (x Core..@? "MaxCapacity")
      Core.<*> (x Core..@? "MinCapacity")
      Core.<*> (x Core..@? "SecondsUntilAutoPause")
      Core.<*> (x Core..@? "TimeoutAction")
