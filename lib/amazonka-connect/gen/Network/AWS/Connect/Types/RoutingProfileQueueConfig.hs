{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.RoutingProfileQueueConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.RoutingProfileQueueConfig
  ( RoutingProfileQueueConfig (..),

    -- * Smart constructor
    mkRoutingProfileQueueConfig,

    -- * Lenses
    rpqcPriority,
    rpqcQueueReference,
    rpqcDelay,
  )
where

import Network.AWS.Connect.Types.RoutingProfileQueueReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the queue and channel for which priority and delay can be set.
--
-- /See:/ 'mkRoutingProfileQueueConfig' smart constructor.
data RoutingProfileQueueConfig = RoutingProfileQueueConfig'
  { -- | The order in which contacts are to be handled for the queue. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> .
    priority :: Lude.Natural,
    -- | Contains information about a queue resource.
    queueReference :: RoutingProfileQueueReference,
    -- | The delay, in seconds, a contact should be in the queue before they are routed to an available agent. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> in the /Amazon Connect Administrator Guide/ .
    delay :: Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RoutingProfileQueueConfig' with the minimum fields required to make a request.
--
-- * 'priority' - The order in which contacts are to be handled for the queue. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> .
-- * 'queueReference' - Contains information about a queue resource.
-- * 'delay' - The delay, in seconds, a contact should be in the queue before they are routed to an available agent. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> in the /Amazon Connect Administrator Guide/ .
mkRoutingProfileQueueConfig ::
  -- | 'priority'
  Lude.Natural ->
  -- | 'queueReference'
  RoutingProfileQueueReference ->
  -- | 'delay'
  Lude.Natural ->
  RoutingProfileQueueConfig
mkRoutingProfileQueueConfig pPriority_ pQueueReference_ pDelay_ =
  RoutingProfileQueueConfig'
    { priority = pPriority_,
      queueReference = pQueueReference_,
      delay = pDelay_
    }

-- | The order in which contacts are to be handled for the queue. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> .
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcPriority :: Lens.Lens' RoutingProfileQueueConfig Lude.Natural
rpqcPriority = Lens.lens (priority :: RoutingProfileQueueConfig -> Lude.Natural) (\s a -> s {priority = a} :: RoutingProfileQueueConfig)
{-# DEPRECATED rpqcPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | Contains information about a queue resource.
--
-- /Note:/ Consider using 'queueReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcQueueReference :: Lens.Lens' RoutingProfileQueueConfig RoutingProfileQueueReference
rpqcQueueReference = Lens.lens (queueReference :: RoutingProfileQueueConfig -> RoutingProfileQueueReference) (\s a -> s {queueReference = a} :: RoutingProfileQueueConfig)
{-# DEPRECATED rpqcQueueReference "Use generic-lens or generic-optics with 'queueReference' instead." #-}

-- | The delay, in seconds, a contact should be in the queue before they are routed to an available agent. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> in the /Amazon Connect Administrator Guide/ .
--
-- /Note:/ Consider using 'delay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcDelay :: Lens.Lens' RoutingProfileQueueConfig Lude.Natural
rpqcDelay = Lens.lens (delay :: RoutingProfileQueueConfig -> Lude.Natural) (\s a -> s {delay = a} :: RoutingProfileQueueConfig)
{-# DEPRECATED rpqcDelay "Use generic-lens or generic-optics with 'delay' instead." #-}

instance Lude.ToJSON RoutingProfileQueueConfig where
  toJSON RoutingProfileQueueConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Priority" Lude..= priority),
            Lude.Just ("QueueReference" Lude..= queueReference),
            Lude.Just ("Delay" Lude..= delay)
          ]
      )
