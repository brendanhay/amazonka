{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeDeliveryChannels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about the specified delivery channel. If a delivery channel is not specified, this action returns the details of all delivery channels associated with the account.
module Network.AWS.Config.DescribeDeliveryChannels
  ( -- * Creating a request
    DescribeDeliveryChannels (..),
    mkDescribeDeliveryChannels,

    -- ** Request lenses
    ddcDeliveryChannelNames,

    -- * Destructuring the response
    DescribeDeliveryChannelsResponse (..),
    mkDescribeDeliveryChannelsResponse,

    -- ** Response lenses
    ddcrsDeliveryChannels,
    ddcrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'DescribeDeliveryChannels' action.
--
-- /See:/ 'mkDescribeDeliveryChannels' smart constructor.
newtype DescribeDeliveryChannels = DescribeDeliveryChannels'
  { -- | A list of delivery channel names.
    deliveryChannelNames :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDeliveryChannels' with the minimum fields required to make a request.
--
-- * 'deliveryChannelNames' - A list of delivery channel names.
mkDescribeDeliveryChannels ::
  DescribeDeliveryChannels
mkDescribeDeliveryChannels =
  DescribeDeliveryChannels' {deliveryChannelNames = Lude.Nothing}

-- | A list of delivery channel names.
--
-- /Note:/ Consider using 'deliveryChannelNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDeliveryChannelNames :: Lens.Lens' DescribeDeliveryChannels (Lude.Maybe [Lude.Text])
ddcDeliveryChannelNames = Lens.lens (deliveryChannelNames :: DescribeDeliveryChannels -> Lude.Maybe [Lude.Text]) (\s a -> s {deliveryChannelNames = a} :: DescribeDeliveryChannels)
{-# DEPRECATED ddcDeliveryChannelNames "Use generic-lens or generic-optics with 'deliveryChannelNames' instead." #-}

instance Lude.AWSRequest DescribeDeliveryChannels where
  type Rs DescribeDeliveryChannels = DescribeDeliveryChannelsResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDeliveryChannelsResponse'
            Lude.<$> (x Lude..?> "DeliveryChannels" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDeliveryChannels where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeDeliveryChannels" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDeliveryChannels where
  toJSON DescribeDeliveryChannels' {..} =
    Lude.object
      ( Lude.catMaybes
          [("DeliveryChannelNames" Lude..=) Lude.<$> deliveryChannelNames]
      )

instance Lude.ToPath DescribeDeliveryChannels where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDeliveryChannels where
  toQuery = Lude.const Lude.mempty

-- | The output for the 'DescribeDeliveryChannels' action.
--
-- /See:/ 'mkDescribeDeliveryChannelsResponse' smart constructor.
data DescribeDeliveryChannelsResponse = DescribeDeliveryChannelsResponse'
  { -- | A list that contains the descriptions of the specified delivery channel.
    deliveryChannels :: Lude.Maybe [DeliveryChannel],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDeliveryChannelsResponse' with the minimum fields required to make a request.
--
-- * 'deliveryChannels' - A list that contains the descriptions of the specified delivery channel.
-- * 'responseStatus' - The response status code.
mkDescribeDeliveryChannelsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDeliveryChannelsResponse
mkDescribeDeliveryChannelsResponse pResponseStatus_ =
  DescribeDeliveryChannelsResponse'
    { deliveryChannels =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list that contains the descriptions of the specified delivery channel.
--
-- /Note:/ Consider using 'deliveryChannels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsDeliveryChannels :: Lens.Lens' DescribeDeliveryChannelsResponse (Lude.Maybe [DeliveryChannel])
ddcrsDeliveryChannels = Lens.lens (deliveryChannels :: DescribeDeliveryChannelsResponse -> Lude.Maybe [DeliveryChannel]) (\s a -> s {deliveryChannels = a} :: DescribeDeliveryChannelsResponse)
{-# DEPRECATED ddcrsDeliveryChannels "Use generic-lens or generic-optics with 'deliveryChannels' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsResponseStatus :: Lens.Lens' DescribeDeliveryChannelsResponse Lude.Int
ddcrsResponseStatus = Lens.lens (responseStatus :: DescribeDeliveryChannelsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDeliveryChannelsResponse)
{-# DEPRECATED ddcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
