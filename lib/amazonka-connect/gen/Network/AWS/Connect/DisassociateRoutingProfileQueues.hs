{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DisassociateRoutingProfileQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a set of queues from a routing profile.
module Network.AWS.Connect.DisassociateRoutingProfileQueues
  ( -- * Creating a request
    DisassociateRoutingProfileQueues (..),
    mkDisassociateRoutingProfileQueues,

    -- ** Request lenses
    drpqInstanceId,
    drpqRoutingProfileId,
    drpqQueueReferences,

    -- * Destructuring the response
    DisassociateRoutingProfileQueuesResponse (..),
    mkDisassociateRoutingProfileQueuesResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateRoutingProfileQueues' smart constructor.
data DisassociateRoutingProfileQueues = DisassociateRoutingProfileQueues'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Lude.Text,
    -- | The queues to disassociate from this routing profile.
    queueReferences :: [RoutingProfileQueueReference]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateRoutingProfileQueues' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'routingProfileId' - The identifier of the routing profile.
-- * 'queueReferences' - The queues to disassociate from this routing profile.
mkDisassociateRoutingProfileQueues ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'routingProfileId'
  Lude.Text ->
  DisassociateRoutingProfileQueues
mkDisassociateRoutingProfileQueues pInstanceId_ pRoutingProfileId_ =
  DisassociateRoutingProfileQueues'
    { instanceId = pInstanceId_,
      routingProfileId = pRoutingProfileId_,
      queueReferences = Lude.mempty
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpqInstanceId :: Lens.Lens' DisassociateRoutingProfileQueues Lude.Text
drpqInstanceId = Lens.lens (instanceId :: DisassociateRoutingProfileQueues -> Lude.Text) (\s a -> s {instanceId = a} :: DisassociateRoutingProfileQueues)
{-# DEPRECATED drpqInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpqRoutingProfileId :: Lens.Lens' DisassociateRoutingProfileQueues Lude.Text
drpqRoutingProfileId = Lens.lens (routingProfileId :: DisassociateRoutingProfileQueues -> Lude.Text) (\s a -> s {routingProfileId = a} :: DisassociateRoutingProfileQueues)
{-# DEPRECATED drpqRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

-- | The queues to disassociate from this routing profile.
--
-- /Note:/ Consider using 'queueReferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpqQueueReferences :: Lens.Lens' DisassociateRoutingProfileQueues [RoutingProfileQueueReference]
drpqQueueReferences = Lens.lens (queueReferences :: DisassociateRoutingProfileQueues -> [RoutingProfileQueueReference]) (\s a -> s {queueReferences = a} :: DisassociateRoutingProfileQueues)
{-# DEPRECATED drpqQueueReferences "Use generic-lens or generic-optics with 'queueReferences' instead." #-}

instance Lude.AWSRequest DisassociateRoutingProfileQueues where
  type
    Rs DisassociateRoutingProfileQueues =
      DisassociateRoutingProfileQueuesResponse
  request = Req.postJSON connectService
  response =
    Res.receiveNull DisassociateRoutingProfileQueuesResponse'

instance Lude.ToHeaders DisassociateRoutingProfileQueues where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateRoutingProfileQueues where
  toJSON DisassociateRoutingProfileQueues' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("QueueReferences" Lude..= queueReferences)]
      )

instance Lude.ToPath DisassociateRoutingProfileQueues where
  toPath DisassociateRoutingProfileQueues' {..} =
    Lude.mconcat
      [ "/routing-profiles/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS routingProfileId,
        "/disassociate-queues"
      ]

instance Lude.ToQuery DisassociateRoutingProfileQueues where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateRoutingProfileQueuesResponse' smart constructor.
data DisassociateRoutingProfileQueuesResponse = DisassociateRoutingProfileQueuesResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateRoutingProfileQueuesResponse' with the minimum fields required to make a request.
mkDisassociateRoutingProfileQueuesResponse ::
  DisassociateRoutingProfileQueuesResponse
mkDisassociateRoutingProfileQueuesResponse =
  DisassociateRoutingProfileQueuesResponse'
