{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EntityAggregate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EntityAggregate
  ( EntityAggregate (..),

    -- * Smart constructor
    mkEntityAggregate,

    -- * Lenses
    eCount,
    eEventArn,
  )
where

import qualified Network.AWS.AWSHealth.Types.EventArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The number of entities that are affected by one or more events. Returned by the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEntityAggregates.html DescribeEntityAggregates> operation.
--
-- /See:/ 'mkEntityAggregate' smart constructor.
data EntityAggregate = EntityAggregate'
  { -- | The number of entities that match the criteria for the specified events.
    count :: Core.Maybe Core.Int,
    -- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    eventArn :: Core.Maybe Types.EventArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EntityAggregate' value with any optional fields omitted.
mkEntityAggregate ::
  EntityAggregate
mkEntityAggregate =
  EntityAggregate' {count = Core.Nothing, eventArn = Core.Nothing}

-- | The number of entities that match the criteria for the specified events.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCount :: Lens.Lens' EntityAggregate (Core.Maybe Core.Int)
eCount = Lens.field @"count"
{-# DEPRECATED eCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- /Note:/ Consider using 'eventArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventArn :: Lens.Lens' EntityAggregate (Core.Maybe Types.EventArn)
eEventArn = Lens.field @"eventArn"
{-# DEPRECATED eEventArn "Use generic-lens or generic-optics with 'eventArn' instead." #-}

instance Core.FromJSON EntityAggregate where
  parseJSON =
    Core.withObject "EntityAggregate" Core.$
      \x ->
        EntityAggregate'
          Core.<$> (x Core..:? "count") Core.<*> (x Core..:? "eventArn")
