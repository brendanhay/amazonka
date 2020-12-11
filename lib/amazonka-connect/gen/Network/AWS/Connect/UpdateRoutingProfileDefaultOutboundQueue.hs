{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateRoutingProfileDefaultOutboundQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the default outbound queue of a routing profile.
module Network.AWS.Connect.UpdateRoutingProfileDefaultOutboundQueue
  ( -- * Creating a request
    UpdateRoutingProfileDefaultOutboundQueue (..),
    mkUpdateRoutingProfileDefaultOutboundQueue,

    -- ** Request lenses
    urpdoqInstanceId,
    urpdoqRoutingProfileId,
    urpdoqDefaultOutboundQueueId,

    -- * Destructuring the response
    UpdateRoutingProfileDefaultOutboundQueueResponse (..),
    mkUpdateRoutingProfileDefaultOutboundQueueResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateRoutingProfileDefaultOutboundQueue' smart constructor.
data UpdateRoutingProfileDefaultOutboundQueue = UpdateRoutingProfileDefaultOutboundQueue'
  { instanceId ::
      Lude.Text,
    routingProfileId ::
      Lude.Text,
    defaultOutboundQueueId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRoutingProfileDefaultOutboundQueue' with the minimum fields required to make a request.
--
-- * 'defaultOutboundQueueId' - The identifier for the default outbound queue.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'routingProfileId' - The identifier of the routing profile.
mkUpdateRoutingProfileDefaultOutboundQueue ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'routingProfileId'
  Lude.Text ->
  -- | 'defaultOutboundQueueId'
  Lude.Text ->
  UpdateRoutingProfileDefaultOutboundQueue
mkUpdateRoutingProfileDefaultOutboundQueue
  pInstanceId_
  pRoutingProfileId_
  pDefaultOutboundQueueId_ =
    UpdateRoutingProfileDefaultOutboundQueue'
      { instanceId =
          pInstanceId_,
        routingProfileId = pRoutingProfileId_,
        defaultOutboundQueueId = pDefaultOutboundQueueId_
      }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpdoqInstanceId :: Lens.Lens' UpdateRoutingProfileDefaultOutboundQueue Lude.Text
urpdoqInstanceId = Lens.lens (instanceId :: UpdateRoutingProfileDefaultOutboundQueue -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateRoutingProfileDefaultOutboundQueue)
{-# DEPRECATED urpdoqInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpdoqRoutingProfileId :: Lens.Lens' UpdateRoutingProfileDefaultOutboundQueue Lude.Text
urpdoqRoutingProfileId = Lens.lens (routingProfileId :: UpdateRoutingProfileDefaultOutboundQueue -> Lude.Text) (\s a -> s {routingProfileId = a} :: UpdateRoutingProfileDefaultOutboundQueue)
{-# DEPRECATED urpdoqRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

-- | The identifier for the default outbound queue.
--
-- /Note:/ Consider using 'defaultOutboundQueueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urpdoqDefaultOutboundQueueId :: Lens.Lens' UpdateRoutingProfileDefaultOutboundQueue Lude.Text
urpdoqDefaultOutboundQueueId = Lens.lens (defaultOutboundQueueId :: UpdateRoutingProfileDefaultOutboundQueue -> Lude.Text) (\s a -> s {defaultOutboundQueueId = a} :: UpdateRoutingProfileDefaultOutboundQueue)
{-# DEPRECATED urpdoqDefaultOutboundQueueId "Use generic-lens or generic-optics with 'defaultOutboundQueueId' instead." #-}

instance Lude.AWSRequest UpdateRoutingProfileDefaultOutboundQueue where
  type
    Rs UpdateRoutingProfileDefaultOutboundQueue =
      UpdateRoutingProfileDefaultOutboundQueueResponse
  request = Req.postJSON connectService
  response =
    Res.receiveNull UpdateRoutingProfileDefaultOutboundQueueResponse'

instance Lude.ToHeaders UpdateRoutingProfileDefaultOutboundQueue where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRoutingProfileDefaultOutboundQueue where
  toJSON UpdateRoutingProfileDefaultOutboundQueue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("DefaultOutboundQueueId" Lude..= defaultOutboundQueueId)
          ]
      )

instance Lude.ToPath UpdateRoutingProfileDefaultOutboundQueue where
  toPath UpdateRoutingProfileDefaultOutboundQueue' {..} =
    Lude.mconcat
      [ "/routing-profiles/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS routingProfileId,
        "/default-outbound-queue"
      ]

instance Lude.ToQuery UpdateRoutingProfileDefaultOutboundQueue where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRoutingProfileDefaultOutboundQueueResponse' smart constructor.
data UpdateRoutingProfileDefaultOutboundQueueResponse = UpdateRoutingProfileDefaultOutboundQueueResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'UpdateRoutingProfileDefaultOutboundQueueResponse' with the minimum fields required to make a request.
mkUpdateRoutingProfileDefaultOutboundQueueResponse ::
  UpdateRoutingProfileDefaultOutboundQueueResponse
mkUpdateRoutingProfileDefaultOutboundQueueResponse =
  UpdateRoutingProfileDefaultOutboundQueueResponse'
