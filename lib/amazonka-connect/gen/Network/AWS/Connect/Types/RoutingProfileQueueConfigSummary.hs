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
    rpqcsQueueARN,
    rpqcsQueueName,
    rpqcsPriority,
    rpqcsDelay,
    rpqcsChannel,
  )
where

import Network.AWS.Connect.Types.Channel
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains summary information about a routing profile queue.
--
-- /See:/ 'mkRoutingProfileQueueConfigSummary' smart constructor.
data RoutingProfileQueueConfigSummary = RoutingProfileQueueConfigSummary'
  { queueId ::
      Lude.Text,
    queueARN :: Lude.Text,
    queueName :: Lude.Text,
    priority :: Lude.Natural,
    delay :: Lude.Natural,
    channel :: Channel
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RoutingProfileQueueConfigSummary' with the minimum fields required to make a request.
--
-- * 'channel' - The channels this queue supports.
-- * 'delay' - The delay, in seconds, that a contact should be in the queue before they are routed to an available agent. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> in the /Amazon Connect Administrator Guide/ .
-- * 'priority' - The order in which contacts are to be handled for the queue. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> .
-- * 'queueARN' - The Amazon Resource Name (ARN) of the queue.
-- * 'queueId' - The identifier of the queue.
-- * 'queueName' - The name of the queue.
mkRoutingProfileQueueConfigSummary ::
  -- | 'queueId'
  Lude.Text ->
  -- | 'queueARN'
  Lude.Text ->
  -- | 'queueName'
  Lude.Text ->
  -- | 'priority'
  Lude.Natural ->
  -- | 'delay'
  Lude.Natural ->
  -- | 'channel'
  Channel ->
  RoutingProfileQueueConfigSummary
mkRoutingProfileQueueConfigSummary
  pQueueId_
  pQueueARN_
  pQueueName_
  pPriority_
  pDelay_
  pChannel_ =
    RoutingProfileQueueConfigSummary'
      { queueId = pQueueId_,
        queueARN = pQueueARN_,
        queueName = pQueueName_,
        priority = pPriority_,
        delay = pDelay_,
        channel = pChannel_
      }

-- | The identifier of the queue.
--
-- /Note:/ Consider using 'queueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsQueueId :: Lens.Lens' RoutingProfileQueueConfigSummary Lude.Text
rpqcsQueueId = Lens.lens (queueId :: RoutingProfileQueueConfigSummary -> Lude.Text) (\s a -> s {queueId = a} :: RoutingProfileQueueConfigSummary)
{-# DEPRECATED rpqcsQueueId "Use generic-lens or generic-optics with 'queueId' instead." #-}

-- | The Amazon Resource Name (ARN) of the queue.
--
-- /Note:/ Consider using 'queueARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsQueueARN :: Lens.Lens' RoutingProfileQueueConfigSummary Lude.Text
rpqcsQueueARN = Lens.lens (queueARN :: RoutingProfileQueueConfigSummary -> Lude.Text) (\s a -> s {queueARN = a} :: RoutingProfileQueueConfigSummary)
{-# DEPRECATED rpqcsQueueARN "Use generic-lens or generic-optics with 'queueARN' instead." #-}

-- | The name of the queue.
--
-- /Note:/ Consider using 'queueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsQueueName :: Lens.Lens' RoutingProfileQueueConfigSummary Lude.Text
rpqcsQueueName = Lens.lens (queueName :: RoutingProfileQueueConfigSummary -> Lude.Text) (\s a -> s {queueName = a} :: RoutingProfileQueueConfigSummary)
{-# DEPRECATED rpqcsQueueName "Use generic-lens or generic-optics with 'queueName' instead." #-}

-- | The order in which contacts are to be handled for the queue. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> .
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsPriority :: Lens.Lens' RoutingProfileQueueConfigSummary Lude.Natural
rpqcsPriority = Lens.lens (priority :: RoutingProfileQueueConfigSummary -> Lude.Natural) (\s a -> s {priority = a} :: RoutingProfileQueueConfigSummary)
{-# DEPRECATED rpqcsPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The delay, in seconds, that a contact should be in the queue before they are routed to an available agent. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> in the /Amazon Connect Administrator Guide/ .
--
-- /Note:/ Consider using 'delay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsDelay :: Lens.Lens' RoutingProfileQueueConfigSummary Lude.Natural
rpqcsDelay = Lens.lens (delay :: RoutingProfileQueueConfigSummary -> Lude.Natural) (\s a -> s {delay = a} :: RoutingProfileQueueConfigSummary)
{-# DEPRECATED rpqcsDelay "Use generic-lens or generic-optics with 'delay' instead." #-}

-- | The channels this queue supports.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqcsChannel :: Lens.Lens' RoutingProfileQueueConfigSummary Channel
rpqcsChannel = Lens.lens (channel :: RoutingProfileQueueConfigSummary -> Channel) (\s a -> s {channel = a} :: RoutingProfileQueueConfigSummary)
{-# DEPRECATED rpqcsChannel "Use generic-lens or generic-optics with 'channel' instead." #-}

instance Lude.FromJSON RoutingProfileQueueConfigSummary where
  parseJSON =
    Lude.withObject
      "RoutingProfileQueueConfigSummary"
      ( \x ->
          RoutingProfileQueueConfigSummary'
            Lude.<$> (x Lude..: "QueueId")
            Lude.<*> (x Lude..: "QueueArn")
            Lude.<*> (x Lude..: "QueueName")
            Lude.<*> (x Lude..: "Priority")
            Lude.<*> (x Lude..: "Delay")
            Lude.<*> (x Lude..: "Channel")
      )
