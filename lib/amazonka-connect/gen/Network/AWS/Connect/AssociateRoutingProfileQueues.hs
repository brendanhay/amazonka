{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.AssociateRoutingProfileQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a set of queues with a routing profile.
module Network.AWS.Connect.AssociateRoutingProfileQueues
  ( -- * Creating a request
    AssociateRoutingProfileQueues (..),
    mkAssociateRoutingProfileQueues,

    -- ** Request lenses
    arpqInstanceId,
    arpqRoutingProfileId,
    arpqQueueConfigs,

    -- * Destructuring the response
    AssociateRoutingProfileQueuesResponse (..),
    mkAssociateRoutingProfileQueuesResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateRoutingProfileQueues' smart constructor.
data AssociateRoutingProfileQueues = AssociateRoutingProfileQueues'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Lude.Text,
    -- | The queues to associate with this routing profile.
    queueConfigs :: Lude.NonEmpty RoutingProfileQueueConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateRoutingProfileQueues' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'routingProfileId' - The identifier of the routing profile.
-- * 'queueConfigs' - The queues to associate with this routing profile.
mkAssociateRoutingProfileQueues ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'routingProfileId'
  Lude.Text ->
  -- | 'queueConfigs'
  Lude.NonEmpty RoutingProfileQueueConfig ->
  AssociateRoutingProfileQueues
mkAssociateRoutingProfileQueues
  pInstanceId_
  pRoutingProfileId_
  pQueueConfigs_ =
    AssociateRoutingProfileQueues'
      { instanceId = pInstanceId_,
        routingProfileId = pRoutingProfileId_,
        queueConfigs = pQueueConfigs_
      }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpqInstanceId :: Lens.Lens' AssociateRoutingProfileQueues Lude.Text
arpqInstanceId = Lens.lens (instanceId :: AssociateRoutingProfileQueues -> Lude.Text) (\s a -> s {instanceId = a} :: AssociateRoutingProfileQueues)
{-# DEPRECATED arpqInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpqRoutingProfileId :: Lens.Lens' AssociateRoutingProfileQueues Lude.Text
arpqRoutingProfileId = Lens.lens (routingProfileId :: AssociateRoutingProfileQueues -> Lude.Text) (\s a -> s {routingProfileId = a} :: AssociateRoutingProfileQueues)
{-# DEPRECATED arpqRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

-- | The queues to associate with this routing profile.
--
-- /Note:/ Consider using 'queueConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpqQueueConfigs :: Lens.Lens' AssociateRoutingProfileQueues (Lude.NonEmpty RoutingProfileQueueConfig)
arpqQueueConfigs = Lens.lens (queueConfigs :: AssociateRoutingProfileQueues -> Lude.NonEmpty RoutingProfileQueueConfig) (\s a -> s {queueConfigs = a} :: AssociateRoutingProfileQueues)
{-# DEPRECATED arpqQueueConfigs "Use generic-lens or generic-optics with 'queueConfigs' instead." #-}

instance Lude.AWSRequest AssociateRoutingProfileQueues where
  type
    Rs AssociateRoutingProfileQueues =
      AssociateRoutingProfileQueuesResponse
  request = Req.postJSON connectService
  response = Res.receiveNull AssociateRoutingProfileQueuesResponse'

instance Lude.ToHeaders AssociateRoutingProfileQueues where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateRoutingProfileQueues where
  toJSON AssociateRoutingProfileQueues' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("QueueConfigs" Lude..= queueConfigs)])

instance Lude.ToPath AssociateRoutingProfileQueues where
  toPath AssociateRoutingProfileQueues' {..} =
    Lude.mconcat
      [ "/routing-profiles/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS routingProfileId,
        "/associate-queues"
      ]

instance Lude.ToQuery AssociateRoutingProfileQueues where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateRoutingProfileQueuesResponse' smart constructor.
data AssociateRoutingProfileQueuesResponse = AssociateRoutingProfileQueuesResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateRoutingProfileQueuesResponse' with the minimum fields required to make a request.
mkAssociateRoutingProfileQueuesResponse ::
  AssociateRoutingProfileQueuesResponse
mkAssociateRoutingProfileQueuesResponse =
  AssociateRoutingProfileQueuesResponse'
