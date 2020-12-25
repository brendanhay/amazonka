{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.RoutingProfileQueueConfigSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.RoutingProfileQueueConfigSummary
  ( RoutingProfileQueueConfigSummary (..),

    -- * Smart constructor
    mkRoutingProfileQueueConfigSummary,

    -- * Lenses
    rpqcsQueueId,
    rpqcsQueueArn,
    rpqcsQueueName,
    rpqcsPriority,
    rpqcsDelay,
    rpqcsChannel,
  )
where

import qualified Network.AWS.Connect.Types.ARN as Types
import qualified Network.AWS.Connect.Types.Channel as Types
import qualified Network.AWS.Connect.Types.QueueId as Types
import qualified Network.AWS.Connect.Types.QueueName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains summary information about a routing profile queue.
--
-- /See:/ 'mkRoutingProfileQueueConfigSummary' smart constructor.
data RoutingProfileQueueConfigSummary = RoutingProfileQueueConfigSummary'
  { -- | The identifier of the queue.
    queueId :: Types.QueueId,
    -- | The Amazon Resource Name (ARN) of the queue.
    queueArn :: Types.ARN,
    -- | The name of the queue.
    queueName :: Types.QueueName,
    -- | The order in which contacts are to be handled for the queue. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> .
    priority :: Core.Natural,
    -- | The delay, in seconds, that a contact should be in the queue before they are routed to an available agent. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> in the /Amazon Connect Administrator Guide/ .
    delay :: Core.Natural,
    -- | The channels this queue supports.
    channel :: Types.Channel
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RoutingProfileQueueConfigSummary' value with any optional fields omitted.
mkRoutingProfileQueueConfigSummary ::
  -- | 'queueId'
  Types.QueueId ->
  -- | 'queueArn'
  Types.ARN ->
  -- | 'queueName'
  Types.QueueName ->
  -- | 'priority'
  Core.Natural ->
  -- | 'delay'
  Core.Natural ->
  -- | 'channel'
  Types.Channel ->
  RoutingProfileQueueConfigSummary
mkRoutingProfileQueueConfigSummary
  queueId
  queueArn
  queueName
  priority
  delay
  channel =
    RoutingProfileQueueConfigSummary'
      { queueId,
        queueArn,
        queueName,
        priority,
        delay,
        channel
      }

-- | The identifier of the queue.
--
-- /Note:/ Consider using 'queueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsQueueId :: Lens.Lens' RoutingProfileQueueConfigSummary Types.QueueId
rpqcsQueueId = Lens.field @"queueId"
{-# DEPRECATED rpqcsQueueId "Use generic-lens or generic-optics with 'queueId' instead." #-}

-- | The Amazon Resource Name (ARN) of the queue.
--
-- /Note:/ Consider using 'queueArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsQueueArn :: Lens.Lens' RoutingProfileQueueConfigSummary Types.ARN
rpqcsQueueArn = Lens.field @"queueArn"
{-# DEPRECATED rpqcsQueueArn "Use generic-lens or generic-optics with 'queueArn' instead." #-}

-- | The name of the queue.
--
-- /Note:/ Consider using 'queueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsQueueName :: Lens.Lens' RoutingProfileQueueConfigSummary Types.QueueName
rpqcsQueueName = Lens.field @"queueName"
{-# DEPRECATED rpqcsQueueName "Use generic-lens or generic-optics with 'queueName' instead." #-}

-- | The order in which contacts are to be handled for the queue. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> .
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsPriority :: Lens.Lens' RoutingProfileQueueConfigSummary Core.Natural
rpqcsPriority = Lens.field @"priority"
{-# DEPRECATED rpqcsPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The delay, in seconds, that a contact should be in the queue before they are routed to an available agent. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> in the /Amazon Connect Administrator Guide/ .
--
-- /Note:/ Consider using 'delay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsDelay :: Lens.Lens' RoutingProfileQueueConfigSummary Core.Natural
rpqcsDelay = Lens.field @"delay"
{-# DEPRECATED rpqcsDelay "Use generic-lens or generic-optics with 'delay' instead." #-}

-- | The channels this queue supports.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsChannel :: Lens.Lens' RoutingProfileQueueConfigSummary Types.Channel
rpqcsChannel = Lens.field @"channel"
{-# DEPRECATED rpqcsChannel "Use generic-lens or generic-optics with 'channel' instead." #-}

instance Core.FromJSON RoutingProfileQueueConfigSummary where
  parseJSON =
    Core.withObject "RoutingProfileQueueConfigSummary" Core.$
      \x ->
        RoutingProfileQueueConfigSummary'
          Core.<$> (x Core..: "QueueId")
          Core.<*> (x Core..: "QueueArn")
          Core.<*> (x Core..: "QueueName")
          Core.<*> (x Core..: "Priority")
          Core.<*> (x Core..: "Delay")
          Core.<*> (x Core..: "Channel")
