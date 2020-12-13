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
    sciSecondsUntilAutoPause,
    sciTimeoutAction,
    sciAutoPause,
    sciMaxCapacity,
    sciMinCapacity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Shows the scaling configuration for an Aurora DB cluster in @serverless@ DB engine mode.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
-- /See:/ 'mkScalingConfigurationInfo' smart constructor.
data ScalingConfigurationInfo = ScalingConfigurationInfo'
  { -- | The remaining amount of time, in seconds, before the Aurora DB cluster in @serverless@ mode is paused. A DB cluster can be paused only when it's idle (it has no connections).
    secondsUntilAutoPause :: Lude.Maybe Lude.Int,
    -- | The timeout action of a call to @ModifyCurrentDBClusterCapacity@ , either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
    timeoutAction :: Lude.Maybe Lude.Text,
    -- | A value that indicates whether automatic pause is allowed for the Aurora DB cluster in @serverless@ DB engine mode.
    --
    -- When the value is set to false for an Aurora Serverless DB cluster, the DB cluster automatically resumes.
    autoPause :: Lude.Maybe Lude.Bool,
    -- | The maximum capacity for an Aurora DB cluster in @serverless@ DB engine mode.
    maxCapacity :: Lude.Maybe Lude.Int,
    -- | The maximum capacity for the Aurora DB cluster in @serverless@ DB engine mode.
    minCapacity :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScalingConfigurationInfo' with the minimum fields required to make a request.
--
-- * 'secondsUntilAutoPause' - The remaining amount of time, in seconds, before the Aurora DB cluster in @serverless@ mode is paused. A DB cluster can be paused only when it's idle (it has no connections).
-- * 'timeoutAction' - The timeout action of a call to @ModifyCurrentDBClusterCapacity@ , either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
-- * 'autoPause' - A value that indicates whether automatic pause is allowed for the Aurora DB cluster in @serverless@ DB engine mode.
--
-- When the value is set to false for an Aurora Serverless DB cluster, the DB cluster automatically resumes.
-- * 'maxCapacity' - The maximum capacity for an Aurora DB cluster in @serverless@ DB engine mode.
-- * 'minCapacity' - The maximum capacity for the Aurora DB cluster in @serverless@ DB engine mode.
mkScalingConfigurationInfo ::
  ScalingConfigurationInfo
mkScalingConfigurationInfo =
  ScalingConfigurationInfo'
    { secondsUntilAutoPause = Lude.Nothing,
      timeoutAction = Lude.Nothing,
      autoPause = Lude.Nothing,
      maxCapacity = Lude.Nothing,
      minCapacity = Lude.Nothing
    }

-- | The remaining amount of time, in seconds, before the Aurora DB cluster in @serverless@ mode is paused. A DB cluster can be paused only when it's idle (it has no connections).
--
-- /Note:/ Consider using 'secondsUntilAutoPause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sciSecondsUntilAutoPause :: Lens.Lens' ScalingConfigurationInfo (Lude.Maybe Lude.Int)
sciSecondsUntilAutoPause = Lens.lens (secondsUntilAutoPause :: ScalingConfigurationInfo -> Lude.Maybe Lude.Int) (\s a -> s {secondsUntilAutoPause = a} :: ScalingConfigurationInfo)
{-# DEPRECATED sciSecondsUntilAutoPause "Use generic-lens or generic-optics with 'secondsUntilAutoPause' instead." #-}

-- | The timeout action of a call to @ModifyCurrentDBClusterCapacity@ , either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
--
-- /Note:/ Consider using 'timeoutAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sciTimeoutAction :: Lens.Lens' ScalingConfigurationInfo (Lude.Maybe Lude.Text)
sciTimeoutAction = Lens.lens (timeoutAction :: ScalingConfigurationInfo -> Lude.Maybe Lude.Text) (\s a -> s {timeoutAction = a} :: ScalingConfigurationInfo)
{-# DEPRECATED sciTimeoutAction "Use generic-lens or generic-optics with 'timeoutAction' instead." #-}

-- | A value that indicates whether automatic pause is allowed for the Aurora DB cluster in @serverless@ DB engine mode.
--
-- When the value is set to false for an Aurora Serverless DB cluster, the DB cluster automatically resumes.
--
-- /Note:/ Consider using 'autoPause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sciAutoPause :: Lens.Lens' ScalingConfigurationInfo (Lude.Maybe Lude.Bool)
sciAutoPause = Lens.lens (autoPause :: ScalingConfigurationInfo -> Lude.Maybe Lude.Bool) (\s a -> s {autoPause = a} :: ScalingConfigurationInfo)
{-# DEPRECATED sciAutoPause "Use generic-lens or generic-optics with 'autoPause' instead." #-}

-- | The maximum capacity for an Aurora DB cluster in @serverless@ DB engine mode.
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sciMaxCapacity :: Lens.Lens' ScalingConfigurationInfo (Lude.Maybe Lude.Int)
sciMaxCapacity = Lens.lens (maxCapacity :: ScalingConfigurationInfo -> Lude.Maybe Lude.Int) (\s a -> s {maxCapacity = a} :: ScalingConfigurationInfo)
{-# DEPRECATED sciMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | The maximum capacity for the Aurora DB cluster in @serverless@ DB engine mode.
--
-- /Note:/ Consider using 'minCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sciMinCapacity :: Lens.Lens' ScalingConfigurationInfo (Lude.Maybe Lude.Int)
sciMinCapacity = Lens.lens (minCapacity :: ScalingConfigurationInfo -> Lude.Maybe Lude.Int) (\s a -> s {minCapacity = a} :: ScalingConfigurationInfo)
{-# DEPRECATED sciMinCapacity "Use generic-lens or generic-optics with 'minCapacity' instead." #-}

instance Lude.FromXML ScalingConfigurationInfo where
  parseXML x =
    ScalingConfigurationInfo'
      Lude.<$> (x Lude..@? "SecondsUntilAutoPause")
      Lude.<*> (x Lude..@? "TimeoutAction")
      Lude.<*> (x Lude..@? "AutoPause")
      Lude.<*> (x Lude..@? "MaxCapacity")
      Lude.<*> (x Lude..@? "MinCapacity")
