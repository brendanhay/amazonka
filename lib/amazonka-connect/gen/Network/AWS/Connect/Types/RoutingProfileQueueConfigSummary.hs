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
    rpqcsPriority,
    rpqcsChannel,
    rpqcsQueueName,
    rpqcsQueueARN,
    rpqcsQueueId,
    rpqcsDelay,
  )
where

import Network.AWS.Connect.Types.Channel
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains summary information about a routing profile queue.
--
-- /See:/ 'mkRoutingProfileQueueConfigSummary' smart constructor.
data RoutingProfileQueueConfigSummary = RoutingProfileQueueConfigSummary'
  { -- | The order in which contacts are to be handled for the queue. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> .
    priority :: Lude.Natural,
    -- | The channels this queue supports.
    channel :: Channel,
    -- | The name of the queue.
    queueName :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the queue.
    queueARN :: Lude.Text,
    -- | The identifier of the queue.
    queueId :: Lude.Text,
    -- | The delay, in seconds, that a contact should be in the queue before they are routed to an available agent. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> in the /Amazon Connect Administrator Guide/ .
    delay :: Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RoutingProfileQueueConfigSummary' with the minimum fields required to make a request.
--
-- * 'priority' - The order in which contacts are to be handled for the queue. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> .
-- * 'channel' - The channels this queue supports.
-- * 'queueName' - The name of the queue.
-- * 'queueARN' - The Amazon Resource Name (ARN) of the queue.
-- * 'queueId' - The identifier of the queue.
-- * 'delay' - The delay, in seconds, that a contact should be in the queue before they are routed to an available agent. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> in the /Amazon Connect Administrator Guide/ .
mkRoutingProfileQueueConfigSummary ::
  -- | 'priority'
  Lude.Natural ->
  -- | 'channel'
  Channel ->
  -- | 'queueName'
  Lude.Text ->
  -- | 'queueARN'
  Lude.Text ->
  -- | 'queueId'
  Lude.Text ->
  -- | 'delay'
  Lude.Natural ->
  RoutingProfileQueueConfigSummary
mkRoutingProfileQueueConfigSummary
  pPriority_
  pChannel_
  pQueueName_
  pQueueARN_
  pQueueId_
  pDelay_ =
    RoutingProfileQueueConfigSummary'
      { priority = pPriority_,
        channel = pChannel_,
        queueName = pQueueName_,
        queueARN = pQueueARN_,
        queueId = pQueueId_,
        delay = pDelay_
      }

-- | The order in which contacts are to be handled for the queue. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> .
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsPriority :: Lens.Lens' RoutingProfileQueueConfigSummary Lude.Natural
rpqcsPriority = Lens.lens (priority :: RoutingProfileQueueConfigSummary -> Lude.Natural) (\s a -> s {priority = a} :: RoutingProfileQueueConfigSummary)
{-# DEPRECATED rpqcsPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The channels this queue supports.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsChannel :: Lens.Lens' RoutingProfileQueueConfigSummary Channel
rpqcsChannel = Lens.lens (channel :: RoutingProfileQueueConfigSummary -> Channel) (\s a -> s {channel = a} :: RoutingProfileQueueConfigSummary)
{-# DEPRECATED rpqcsChannel "Use generic-lens or generic-optics with 'channel' instead." #-}

-- | The name of the queue.
--
-- /Note:/ Consider using 'queueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsQueueName :: Lens.Lens' RoutingProfileQueueConfigSummary Lude.Text
rpqcsQueueName = Lens.lens (queueName :: RoutingProfileQueueConfigSummary -> Lude.Text) (\s a -> s {queueName = a} :: RoutingProfileQueueConfigSummary)
{-# DEPRECATED rpqcsQueueName "Use generic-lens or generic-optics with 'queueName' instead." #-}

-- | The Amazon Resource Name (ARN) of the queue.
--
-- /Note:/ Consider using 'queueARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsQueueARN :: Lens.Lens' RoutingProfileQueueConfigSummary Lude.Text
rpqcsQueueARN = Lens.lens (queueARN :: RoutingProfileQueueConfigSummary -> Lude.Text) (\s a -> s {queueARN = a} :: RoutingProfileQueueConfigSummary)
{-# DEPRECATED rpqcsQueueARN "Use generic-lens or generic-optics with 'queueARN' instead." #-}

-- | The identifier of the queue.
--
-- /Note:/ Consider using 'queueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsQueueId :: Lens.Lens' RoutingProfileQueueConfigSummary Lude.Text
rpqcsQueueId = Lens.lens (queueId :: RoutingProfileQueueConfigSummary -> Lude.Text) (\s a -> s {queueId = a} :: RoutingProfileQueueConfigSummary)
{-# DEPRECATED rpqcsQueueId "Use generic-lens or generic-optics with 'queueId' instead." #-}

-- | The delay, in seconds, that a contact should be in the queue before they are routed to an available agent. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> in the /Amazon Connect Administrator Guide/ .
--
-- /Note:/ Consider using 'delay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsDelay :: Lens.Lens' RoutingProfileQueueConfigSummary Lude.Natural
rpqcsDelay = Lens.lens (delay :: RoutingProfileQueueConfigSummary -> Lude.Natural) (\s a -> s {delay = a} :: RoutingProfileQueueConfigSummary)
{-# DEPRECATED rpqcsDelay "Use generic-lens or generic-optics with 'delay' instead." #-}

instance Lude.FromJSON RoutingProfileQueueConfigSummary where
  parseJSON =
    Lude.withObject
      "RoutingProfileQueueConfigSummary"
      ( \x ->
          RoutingProfileQueueConfigSummary'
            Lude.<$> (x Lude..: "Priority")
            Lude.<*> (x Lude..: "Channel")
            Lude.<*> (x Lude..: "QueueName")
            Lude.<*> (x Lude..: "QueueArn")
            Lude.<*> (x Lude..: "QueueId")
            Lude.<*> (x Lude..: "Delay")
      )
