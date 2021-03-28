{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ScalingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.ScalingConfiguration
  ( ScalingConfiguration (..)
  -- * Smart constructor
  , mkScalingConfiguration
  -- * Lenses
  , scAutoPause
  , scMaxCapacity
  , scMinCapacity
  , scSecondsUntilAutoPause
  , scTimeoutAction
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the scaling configuration of an Aurora Serverless DB cluster.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
-- /See:/ 'mkScalingConfiguration' smart constructor.
data ScalingConfiguration = ScalingConfiguration'
  { autoPause :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to allow or disallow automatic pause for an Aurora DB cluster in @serverless@ DB engine mode. A DB cluster can be paused only when it's idle (it has no connections).
  , maxCapacity :: Core.Maybe Core.Int
    -- ^ The maximum capacity for an Aurora DB cluster in @serverless@ DB engine mode.
--
-- For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ .
-- For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ .
-- The maximum capacity must be greater than or equal to the minimum capacity.
  , minCapacity :: Core.Maybe Core.Int
    -- ^ The minimum capacity for an Aurora DB cluster in @serverless@ DB engine mode.
--
-- For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ .
-- For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ .
-- The minimum capacity must be less than or equal to the maximum capacity.
  , secondsUntilAutoPause :: Core.Maybe Core.Int
    -- ^ The time, in seconds, before an Aurora DB cluster in @serverless@ mode is paused.
  , timeoutAction :: Core.Maybe Core.Text
    -- ^ The action to take when the timeout is reached, either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
--
-- @ForceApplyCapacityChange@ sets the capacity to the specified value as soon as possible.
-- @RollbackCapacityChange@ , the default, ignores the capacity change if a scaling point isn't found in the timeout period.
-- /Important:/ If you specify @ForceApplyCapacityChange@ , connections that prevent Aurora Serverless from finding a scaling point might be dropped.
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.how-it-works.html#aurora-serverless.how-it-works.auto-scaling Autoscaling for Aurora Serverless> in the /Amazon Aurora User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScalingConfiguration' value with any optional fields omitted.
mkScalingConfiguration
    :: ScalingConfiguration
mkScalingConfiguration
  = ScalingConfiguration'{autoPause = Core.Nothing,
                          maxCapacity = Core.Nothing, minCapacity = Core.Nothing,
                          secondsUntilAutoPause = Core.Nothing, timeoutAction = Core.Nothing}

-- | A value that indicates whether to allow or disallow automatic pause for an Aurora DB cluster in @serverless@ DB engine mode. A DB cluster can be paused only when it's idle (it has no connections).
--
-- /Note:/ Consider using 'autoPause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scAutoPause :: Lens.Lens' ScalingConfiguration (Core.Maybe Core.Bool)
scAutoPause = Lens.field @"autoPause"
{-# INLINEABLE scAutoPause #-}
{-# DEPRECATED autoPause "Use generic-lens or generic-optics with 'autoPause' instead"  #-}

-- | The maximum capacity for an Aurora DB cluster in @serverless@ DB engine mode.
--
-- For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ .
-- For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ .
-- The maximum capacity must be greater than or equal to the minimum capacity.
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMaxCapacity :: Lens.Lens' ScalingConfiguration (Core.Maybe Core.Int)
scMaxCapacity = Lens.field @"maxCapacity"
{-# INLINEABLE scMaxCapacity #-}
{-# DEPRECATED maxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead"  #-}

-- | The minimum capacity for an Aurora DB cluster in @serverless@ DB engine mode.
--
-- For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ .
-- For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ .
-- The minimum capacity must be less than or equal to the maximum capacity.
--
-- /Note:/ Consider using 'minCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMinCapacity :: Lens.Lens' ScalingConfiguration (Core.Maybe Core.Int)
scMinCapacity = Lens.field @"minCapacity"
{-# INLINEABLE scMinCapacity #-}
{-# DEPRECATED minCapacity "Use generic-lens or generic-optics with 'minCapacity' instead"  #-}

-- | The time, in seconds, before an Aurora DB cluster in @serverless@ mode is paused.
--
-- /Note:/ Consider using 'secondsUntilAutoPause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSecondsUntilAutoPause :: Lens.Lens' ScalingConfiguration (Core.Maybe Core.Int)
scSecondsUntilAutoPause = Lens.field @"secondsUntilAutoPause"
{-# INLINEABLE scSecondsUntilAutoPause #-}
{-# DEPRECATED secondsUntilAutoPause "Use generic-lens or generic-optics with 'secondsUntilAutoPause' instead"  #-}

-- | The action to take when the timeout is reached, either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
--
-- @ForceApplyCapacityChange@ sets the capacity to the specified value as soon as possible.
-- @RollbackCapacityChange@ , the default, ignores the capacity change if a scaling point isn't found in the timeout period.
-- /Important:/ If you specify @ForceApplyCapacityChange@ , connections that prevent Aurora Serverless from finding a scaling point might be dropped.
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.how-it-works.html#aurora-serverless.how-it-works.auto-scaling Autoscaling for Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'timeoutAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scTimeoutAction :: Lens.Lens' ScalingConfiguration (Core.Maybe Core.Text)
scTimeoutAction = Lens.field @"timeoutAction"
{-# INLINEABLE scTimeoutAction #-}
{-# DEPRECATED timeoutAction "Use generic-lens or generic-optics with 'timeoutAction' instead"  #-}

instance Core.ToQuery ScalingConfiguration where
        toQuery ScalingConfiguration{..}
          = Core.maybe Core.mempty (Core.toQueryPair "AutoPause") autoPause
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxCapacity") maxCapacity
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MinCapacity") minCapacity
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SecondsUntilAutoPause")
                secondsUntilAutoPause
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TimeoutAction")
                timeoutAction
