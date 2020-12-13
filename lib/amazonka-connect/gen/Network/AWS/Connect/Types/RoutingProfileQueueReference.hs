{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.RoutingProfileQueueReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.RoutingProfileQueueReference
  ( RoutingProfileQueueReference (..),

    -- * Smart constructor
    mkRoutingProfileQueueReference,

    -- * Lenses
    rpqrChannel,
    rpqrQueueId,
  )
where

import Network.AWS.Connect.Types.Channel
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the channel and queue identifier for a routing profile.
--
-- /See:/ 'mkRoutingProfileQueueReference' smart constructor.
data RoutingProfileQueueReference = RoutingProfileQueueReference'
  { -- | The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
    channel :: Channel,
    -- | The identifier of the queue.
    queueId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RoutingProfileQueueReference' with the minimum fields required to make a request.
--
-- * 'channel' - The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
-- * 'queueId' - The identifier of the queue.
mkRoutingProfileQueueReference ::
  -- | 'channel'
  Channel ->
  -- | 'queueId'
  Lude.Text ->
  RoutingProfileQueueReference
mkRoutingProfileQueueReference pChannel_ pQueueId_ =
  RoutingProfileQueueReference'
    { channel = pChannel_,
      queueId = pQueueId_
    }

-- | The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqrChannel :: Lens.Lens' RoutingProfileQueueReference Channel
rpqrChannel = Lens.lens (channel :: RoutingProfileQueueReference -> Channel) (\s a -> s {channel = a} :: RoutingProfileQueueReference)
{-# DEPRECATED rpqrChannel "Use generic-lens or generic-optics with 'channel' instead." #-}

-- | The identifier of the queue.
--
-- /Note:/ Consider using 'queueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqrQueueId :: Lens.Lens' RoutingProfileQueueReference Lude.Text
rpqrQueueId = Lens.lens (queueId :: RoutingProfileQueueReference -> Lude.Text) (\s a -> s {queueId = a} :: RoutingProfileQueueReference)
{-# DEPRECATED rpqrQueueId "Use generic-lens or generic-optics with 'queueId' instead." #-}

instance Lude.ToJSON RoutingProfileQueueReference where
  toJSON RoutingProfileQueueReference' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Channel" Lude..= channel),
            Lude.Just ("QueueId" Lude..= queueId)
          ]
      )
