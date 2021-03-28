{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.RoutingProfileQueueConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.RoutingProfileQueueConfig
  ( RoutingProfileQueueConfig (..)
  -- * Smart constructor
  , mkRoutingProfileQueueConfig
  -- * Lenses
  , rpqcQueueReference
  , rpqcPriority
  , rpqcDelay
  ) where

import qualified Network.AWS.Connect.Types.RoutingProfileQueueReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the queue and channel for which priority and delay can be set.
--
-- /See:/ 'mkRoutingProfileQueueConfig' smart constructor.
data RoutingProfileQueueConfig = RoutingProfileQueueConfig'
  { queueReference :: Types.RoutingProfileQueueReference
    -- ^ Contains information about a queue resource.
  , priority :: Core.Natural
    -- ^ The order in which contacts are to be handled for the queue. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> .
  , delay :: Core.Natural
    -- ^ The delay, in seconds, a contact should be in the queue before they are routed to an available agent. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> in the /Amazon Connect Administrator Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RoutingProfileQueueConfig' value with any optional fields omitted.
mkRoutingProfileQueueConfig
    :: Types.RoutingProfileQueueReference -- ^ 'queueReference'
    -> Core.Natural -- ^ 'priority'
    -> Core.Natural -- ^ 'delay'
    -> RoutingProfileQueueConfig
mkRoutingProfileQueueConfig queueReference priority delay
  = RoutingProfileQueueConfig'{queueReference, priority, delay}

-- | Contains information about a queue resource.
--
-- /Note:/ Consider using 'queueReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcQueueReference :: Lens.Lens' RoutingProfileQueueConfig Types.RoutingProfileQueueReference
rpqcQueueReference = Lens.field @"queueReference"
{-# INLINEABLE rpqcQueueReference #-}
{-# DEPRECATED queueReference "Use generic-lens or generic-optics with 'queueReference' instead"  #-}

-- | The order in which contacts are to be handled for the queue. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> .
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcPriority :: Lens.Lens' RoutingProfileQueueConfig Core.Natural
rpqcPriority = Lens.field @"priority"
{-# INLINEABLE rpqcPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | The delay, in seconds, a contact should be in the queue before they are routed to an available agent. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> in the /Amazon Connect Administrator Guide/ .
--
-- /Note:/ Consider using 'delay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcDelay :: Lens.Lens' RoutingProfileQueueConfig Core.Natural
rpqcDelay = Lens.field @"delay"
{-# INLINEABLE rpqcDelay #-}
{-# DEPRECATED delay "Use generic-lens or generic-optics with 'delay' instead"  #-}

instance Core.FromJSON RoutingProfileQueueConfig where
        toJSON RoutingProfileQueueConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("QueueReference" Core..= queueReference),
                  Core.Just ("Priority" Core..= priority),
                  Core.Just ("Delay" Core..= delay)])
