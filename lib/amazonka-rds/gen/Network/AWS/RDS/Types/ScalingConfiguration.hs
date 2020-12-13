{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ScalingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ScalingConfiguration
  ( ScalingConfiguration (..),

    -- * Smart constructor
    mkScalingConfiguration,

    -- * Lenses
    scSecondsUntilAutoPause,
    scTimeoutAction,
    scAutoPause,
    scMaxCapacity,
    scMinCapacity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the scaling configuration of an Aurora Serverless DB cluster.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
-- /See:/ 'mkScalingConfiguration' smart constructor.
data ScalingConfiguration = ScalingConfiguration'
  { -- | The time, in seconds, before an Aurora DB cluster in @serverless@ mode is paused.
    secondsUntilAutoPause :: Lude.Maybe Lude.Int,
    -- | The action to take when the timeout is reached, either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
    --
    -- @ForceApplyCapacityChange@ sets the capacity to the specified value as soon as possible.
    -- @RollbackCapacityChange@ , the default, ignores the capacity change if a scaling point isn't found in the timeout period.
    -- /Important:/ If you specify @ForceApplyCapacityChange@ , connections that prevent Aurora Serverless from finding a scaling point might be dropped.
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.how-it-works.html#aurora-serverless.how-it-works.auto-scaling Autoscaling for Aurora Serverless> in the /Amazon Aurora User Guide/ .
    timeoutAction :: Lude.Maybe Lude.Text,
    -- | A value that indicates whether to allow or disallow automatic pause for an Aurora DB cluster in @serverless@ DB engine mode. A DB cluster can be paused only when it's idle (it has no connections).
    autoPause :: Lude.Maybe Lude.Bool,
    -- | The maximum capacity for an Aurora DB cluster in @serverless@ DB engine mode.
    --
    -- For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ .
    -- For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ .
    -- The maximum capacity must be greater than or equal to the minimum capacity.
    maxCapacity :: Lude.Maybe Lude.Int,
    -- | The minimum capacity for an Aurora DB cluster in @serverless@ DB engine mode.
    --
    -- For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ .
    -- For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ .
    -- The minimum capacity must be less than or equal to the maximum capacity.
    minCapacity :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScalingConfiguration' with the minimum fields required to make a request.
--
-- * 'secondsUntilAutoPause' - The time, in seconds, before an Aurora DB cluster in @serverless@ mode is paused.
-- * 'timeoutAction' - The action to take when the timeout is reached, either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
--
-- @ForceApplyCapacityChange@ sets the capacity to the specified value as soon as possible.
-- @RollbackCapacityChange@ , the default, ignores the capacity change if a scaling point isn't found in the timeout period.
-- /Important:/ If you specify @ForceApplyCapacityChange@ , connections that prevent Aurora Serverless from finding a scaling point might be dropped.
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.how-it-works.html#aurora-serverless.how-it-works.auto-scaling Autoscaling for Aurora Serverless> in the /Amazon Aurora User Guide/ .
-- * 'autoPause' - A value that indicates whether to allow or disallow automatic pause for an Aurora DB cluster in @serverless@ DB engine mode. A DB cluster can be paused only when it's idle (it has no connections).
-- * 'maxCapacity' - The maximum capacity for an Aurora DB cluster in @serverless@ DB engine mode.
--
-- For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ .
-- For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ .
-- The maximum capacity must be greater than or equal to the minimum capacity.
-- * 'minCapacity' - The minimum capacity for an Aurora DB cluster in @serverless@ DB engine mode.
--
-- For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ .
-- For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ .
-- The minimum capacity must be less than or equal to the maximum capacity.
mkScalingConfiguration ::
  ScalingConfiguration
mkScalingConfiguration =
  ScalingConfiguration'
    { secondsUntilAutoPause = Lude.Nothing,
      timeoutAction = Lude.Nothing,
      autoPause = Lude.Nothing,
      maxCapacity = Lude.Nothing,
      minCapacity = Lude.Nothing
    }

-- | The time, in seconds, before an Aurora DB cluster in @serverless@ mode is paused.
--
-- /Note:/ Consider using 'secondsUntilAutoPause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSecondsUntilAutoPause :: Lens.Lens' ScalingConfiguration (Lude.Maybe Lude.Int)
scSecondsUntilAutoPause = Lens.lens (secondsUntilAutoPause :: ScalingConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {secondsUntilAutoPause = a} :: ScalingConfiguration)
{-# DEPRECATED scSecondsUntilAutoPause "Use generic-lens or generic-optics with 'secondsUntilAutoPause' instead." #-}

-- | The action to take when the timeout is reached, either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
--
-- @ForceApplyCapacityChange@ sets the capacity to the specified value as soon as possible.
-- @RollbackCapacityChange@ , the default, ignores the capacity change if a scaling point isn't found in the timeout period.
-- /Important:/ If you specify @ForceApplyCapacityChange@ , connections that prevent Aurora Serverless from finding a scaling point might be dropped.
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.how-it-works.html#aurora-serverless.how-it-works.auto-scaling Autoscaling for Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'timeoutAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scTimeoutAction :: Lens.Lens' ScalingConfiguration (Lude.Maybe Lude.Text)
scTimeoutAction = Lens.lens (timeoutAction :: ScalingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {timeoutAction = a} :: ScalingConfiguration)
{-# DEPRECATED scTimeoutAction "Use generic-lens or generic-optics with 'timeoutAction' instead." #-}

-- | A value that indicates whether to allow or disallow automatic pause for an Aurora DB cluster in @serverless@ DB engine mode. A DB cluster can be paused only when it's idle (it has no connections).
--
-- /Note:/ Consider using 'autoPause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scAutoPause :: Lens.Lens' ScalingConfiguration (Lude.Maybe Lude.Bool)
scAutoPause = Lens.lens (autoPause :: ScalingConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {autoPause = a} :: ScalingConfiguration)
{-# DEPRECATED scAutoPause "Use generic-lens or generic-optics with 'autoPause' instead." #-}

-- | The maximum capacity for an Aurora DB cluster in @serverless@ DB engine mode.
--
-- For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ .
-- For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ .
-- The maximum capacity must be greater than or equal to the minimum capacity.
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMaxCapacity :: Lens.Lens' ScalingConfiguration (Lude.Maybe Lude.Int)
scMaxCapacity = Lens.lens (maxCapacity :: ScalingConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {maxCapacity = a} :: ScalingConfiguration)
{-# DEPRECATED scMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | The minimum capacity for an Aurora DB cluster in @serverless@ DB engine mode.
--
-- For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ .
-- For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ .
-- The minimum capacity must be less than or equal to the maximum capacity.
--
-- /Note:/ Consider using 'minCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMinCapacity :: Lens.Lens' ScalingConfiguration (Lude.Maybe Lude.Int)
scMinCapacity = Lens.lens (minCapacity :: ScalingConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {minCapacity = a} :: ScalingConfiguration)
{-# DEPRECATED scMinCapacity "Use generic-lens or generic-optics with 'minCapacity' instead." #-}

instance Lude.ToQuery ScalingConfiguration where
  toQuery ScalingConfiguration' {..} =
    Lude.mconcat
      [ "SecondsUntilAutoPause" Lude.=: secondsUntilAutoPause,
        "TimeoutAction" Lude.=: timeoutAction,
        "AutoPause" Lude.=: autoPause,
        "MaxCapacity" Lude.=: maxCapacity,
        "MinCapacity" Lude.=: minCapacity
      ]
