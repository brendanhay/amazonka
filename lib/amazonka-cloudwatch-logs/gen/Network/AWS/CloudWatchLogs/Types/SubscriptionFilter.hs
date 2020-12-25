{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.SubscriptionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.SubscriptionFilter
  ( SubscriptionFilter (..),

    -- * Smart constructor
    mkSubscriptionFilter,

    -- * Lenses
    sfCreationTime,
    sfDestinationArn,
    sfDistribution,
    sfFilterName,
    sfFilterPattern,
    sfLogGroupName,
    sfRoleArn,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types.DestinationArn as Types
import qualified Network.AWS.CloudWatchLogs.Types.Distribution as Types
import qualified Network.AWS.CloudWatchLogs.Types.FilterName as Types
import qualified Network.AWS.CloudWatchLogs.Types.FilterPattern as Types
import qualified Network.AWS.CloudWatchLogs.Types.LogGroupName as Types
import qualified Network.AWS.CloudWatchLogs.Types.RoleArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a subscription filter.
--
-- /See:/ 'mkSubscriptionFilter' smart constructor.
data SubscriptionFilter = SubscriptionFilter'
  { -- | The creation time of the subscription filter, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    creationTime :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the destination.
    destinationArn :: Core.Maybe Types.DestinationArn,
    distribution :: Core.Maybe Types.Distribution,
    -- | The name of the subscription filter.
    filterName :: Core.Maybe Types.FilterName,
    filterPattern :: Core.Maybe Types.FilterPattern,
    -- | The name of the log group.
    logGroupName :: Core.Maybe Types.LogGroupName,
    -- |
    roleArn :: Core.Maybe Types.RoleArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubscriptionFilter' value with any optional fields omitted.
mkSubscriptionFilter ::
  SubscriptionFilter
mkSubscriptionFilter =
  SubscriptionFilter'
    { creationTime = Core.Nothing,
      destinationArn = Core.Nothing,
      distribution = Core.Nothing,
      filterName = Core.Nothing,
      filterPattern = Core.Nothing,
      logGroupName = Core.Nothing,
      roleArn = Core.Nothing
    }

-- | The creation time of the subscription filter, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfCreationTime :: Lens.Lens' SubscriptionFilter (Core.Maybe Core.Natural)
sfCreationTime = Lens.field @"creationTime"
{-# DEPRECATED sfCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the destination.
--
-- /Note:/ Consider using 'destinationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDestinationArn :: Lens.Lens' SubscriptionFilter (Core.Maybe Types.DestinationArn)
sfDestinationArn = Lens.field @"destinationArn"
{-# DEPRECATED sfDestinationArn "Use generic-lens or generic-optics with 'destinationArn' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'distribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDistribution :: Lens.Lens' SubscriptionFilter (Core.Maybe Types.Distribution)
sfDistribution = Lens.field @"distribution"
{-# DEPRECATED sfDistribution "Use generic-lens or generic-optics with 'distribution' instead." #-}

-- | The name of the subscription filter.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfFilterName :: Lens.Lens' SubscriptionFilter (Core.Maybe Types.FilterName)
sfFilterName = Lens.field @"filterName"
{-# DEPRECATED sfFilterName "Use generic-lens or generic-optics with 'filterName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filterPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfFilterPattern :: Lens.Lens' SubscriptionFilter (Core.Maybe Types.FilterPattern)
sfFilterPattern = Lens.field @"filterPattern"
{-# DEPRECATED sfFilterPattern "Use generic-lens or generic-optics with 'filterPattern' instead." #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfLogGroupName :: Lens.Lens' SubscriptionFilter (Core.Maybe Types.LogGroupName)
sfLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED sfLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- |
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfRoleArn :: Lens.Lens' SubscriptionFilter (Core.Maybe Types.RoleArn)
sfRoleArn = Lens.field @"roleArn"
{-# DEPRECATED sfRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

instance Core.FromJSON SubscriptionFilter where
  parseJSON =
    Core.withObject "SubscriptionFilter" Core.$
      \x ->
        SubscriptionFilter'
          Core.<$> (x Core..:? "creationTime")
          Core.<*> (x Core..:? "destinationArn")
          Core.<*> (x Core..:? "distribution")
          Core.<*> (x Core..:? "filterName")
          Core.<*> (x Core..:? "filterPattern")
          Core.<*> (x Core..:? "logGroupName")
          Core.<*> (x Core..:? "roleArn")
