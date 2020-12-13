{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateRoutingProfileQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the properties associated with a set of queues for a routing profile.
module Network.AWS.Connect.UpdateRoutingProfileQueues
  ( -- * Creating a request
    UpdateRoutingProfileQueues (..),
    mkUpdateRoutingProfileQueues,

    -- ** Request lenses
    urpqInstanceId,
    urpqRoutingProfileId,
    urpqQueueConfigs,

    -- * Destructuring the response
    UpdateRoutingProfileQueuesResponse (..),
    mkUpdateRoutingProfileQueuesResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateRoutingProfileQueues' smart constructor.
data UpdateRoutingProfileQueues = UpdateRoutingProfileQueues'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Lude.Text,
    -- | The queues to be updated for this routing profile.
    queueConfigs :: Lude.NonEmpty RoutingProfileQueueConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRoutingProfileQueues' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'routingProfileId' - The identifier of the routing profile.
-- * 'queueConfigs' - The queues to be updated for this routing profile.
mkUpdateRoutingProfileQueues ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'routingProfileId'
  Lude.Text ->
  -- | 'queueConfigs'
  Lude.NonEmpty RoutingProfileQueueConfig ->
  UpdateRoutingProfileQueues
mkUpdateRoutingProfileQueues
  pInstanceId_
  pRoutingProfileId_
  pQueueConfigs_ =
    UpdateRoutingProfileQueues'
      { instanceId = pInstanceId_,
        routingProfileId = pRoutingProfileId_,
        queueConfigs = pQueueConfigs_
      }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpqInstanceId :: Lens.Lens' UpdateRoutingProfileQueues Lude.Text
urpqInstanceId = Lens.lens (instanceId :: UpdateRoutingProfileQueues -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateRoutingProfileQueues)
{-# DEPRECATED urpqInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpqRoutingProfileId :: Lens.Lens' UpdateRoutingProfileQueues Lude.Text
urpqRoutingProfileId = Lens.lens (routingProfileId :: UpdateRoutingProfileQueues -> Lude.Text) (\s a -> s {routingProfileId = a} :: UpdateRoutingProfileQueues)
{-# DEPRECATED urpqRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

-- | The queues to be updated for this routing profile.
--
-- /Note:/ Consider using 'queueConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpqQueueConfigs :: Lens.Lens' UpdateRoutingProfileQueues (Lude.NonEmpty RoutingProfileQueueConfig)
urpqQueueConfigs = Lens.lens (queueConfigs :: UpdateRoutingProfileQueues -> Lude.NonEmpty RoutingProfileQueueConfig) (\s a -> s {queueConfigs = a} :: UpdateRoutingProfileQueues)
{-# DEPRECATED urpqQueueConfigs "Use generic-lens or generic-optics with 'queueConfigs' instead." #-}

instance Lude.AWSRequest UpdateRoutingProfileQueues where
  type
    Rs UpdateRoutingProfileQueues =
      UpdateRoutingProfileQueuesResponse
  request = Req.postJSON connectService
  response = Res.receiveNull UpdateRoutingProfileQueuesResponse'

instance Lude.ToHeaders UpdateRoutingProfileQueues where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRoutingProfileQueues where
  toJSON UpdateRoutingProfileQueues' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("QueueConfigs" Lude..= queueConfigs)])

instance Lude.ToPath UpdateRoutingProfileQueues where
  toPath UpdateRoutingProfileQueues' {..} =
    Lude.mconcat
      [ "/routing-profiles/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS routingProfileId,
        "/queues"
      ]

instance Lude.ToQuery UpdateRoutingProfileQueues where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRoutingProfileQueuesResponse' smart constructor.
data UpdateRoutingProfileQueuesResponse = UpdateRoutingProfileQueuesResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRoutingProfileQueuesResponse' with the minimum fields required to make a request.
mkUpdateRoutingProfileQueuesResponse ::
  UpdateRoutingProfileQueuesResponse
mkUpdateRoutingProfileQueuesResponse =
  UpdateRoutingProfileQueuesResponse'
