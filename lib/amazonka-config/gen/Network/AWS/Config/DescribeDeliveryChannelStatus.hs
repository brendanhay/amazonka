{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeDeliveryChannelStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of the specified delivery channel. If a delivery channel is not specified, this action returns the current status of all delivery channels associated with the account.
module Network.AWS.Config.DescribeDeliveryChannelStatus
  ( -- * Creating a request
    DescribeDeliveryChannelStatus (..),
    mkDescribeDeliveryChannelStatus,

    -- ** Request lenses
    ddcsDeliveryChannelNames,

    -- * Destructuring the response
    DescribeDeliveryChannelStatusResponse (..),
    mkDescribeDeliveryChannelStatusResponse,

    -- ** Response lenses
    ddcsrsDeliveryChannelsStatus,
    ddcsrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'DeliveryChannelStatus' action.
--
-- /See:/ 'mkDescribeDeliveryChannelStatus' smart constructor.
newtype DescribeDeliveryChannelStatus = DescribeDeliveryChannelStatus'
  { deliveryChannelNames ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDeliveryChannelStatus' with the minimum fields required to make a request.
--
-- * 'deliveryChannelNames' - A list of delivery channel names.
mkDescribeDeliveryChannelStatus ::
  DescribeDeliveryChannelStatus
mkDescribeDeliveryChannelStatus =
  DescribeDeliveryChannelStatus'
    { deliveryChannelNames =
        Lude.Nothing
    }

-- | A list of delivery channel names.
--
-- /Note:/ Consider using 'deliveryChannelNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcsDeliveryChannelNames :: Lens.Lens' DescribeDeliveryChannelStatus (Lude.Maybe [Lude.Text])
ddcsDeliveryChannelNames = Lens.lens (deliveryChannelNames :: DescribeDeliveryChannelStatus -> Lude.Maybe [Lude.Text]) (\s a -> s {deliveryChannelNames = a} :: DescribeDeliveryChannelStatus)
{-# DEPRECATED ddcsDeliveryChannelNames "Use generic-lens or generic-optics with 'deliveryChannelNames' instead." #-}

instance Lude.AWSRequest DescribeDeliveryChannelStatus where
  type
    Rs DescribeDeliveryChannelStatus =
      DescribeDeliveryChannelStatusResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDeliveryChannelStatusResponse'
            Lude.<$> (x Lude..?> "DeliveryChannelsStatus" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDeliveryChannelStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DescribeDeliveryChannelStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDeliveryChannelStatus where
  toJSON DescribeDeliveryChannelStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [("DeliveryChannelNames" Lude..=) Lude.<$> deliveryChannelNames]
      )

instance Lude.ToPath DescribeDeliveryChannelStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDeliveryChannelStatus where
  toQuery = Lude.const Lude.mempty

-- | The output for the 'DescribeDeliveryChannelStatus' action.
--
-- /See:/ 'mkDescribeDeliveryChannelStatusResponse' smart constructor.
data DescribeDeliveryChannelStatusResponse = DescribeDeliveryChannelStatusResponse'
  { deliveryChannelsStatus ::
      Lude.Maybe
        [DeliveryChannelStatus],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDeliveryChannelStatusResponse' with the minimum fields required to make a request.
--
-- * 'deliveryChannelsStatus' - A list that contains the status of a specified delivery channel.
-- * 'responseStatus' - The response status code.
mkDescribeDeliveryChannelStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDeliveryChannelStatusResponse
mkDescribeDeliveryChannelStatusResponse pResponseStatus_ =
  DescribeDeliveryChannelStatusResponse'
    { deliveryChannelsStatus =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list that contains the status of a specified delivery channel.
--
-- /Note:/ Consider using 'deliveryChannelsStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcsrsDeliveryChannelsStatus :: Lens.Lens' DescribeDeliveryChannelStatusResponse (Lude.Maybe [DeliveryChannelStatus])
ddcsrsDeliveryChannelsStatus = Lens.lens (deliveryChannelsStatus :: DescribeDeliveryChannelStatusResponse -> Lude.Maybe [DeliveryChannelStatus]) (\s a -> s {deliveryChannelsStatus = a} :: DescribeDeliveryChannelStatusResponse)
{-# DEPRECATED ddcsrsDeliveryChannelsStatus "Use generic-lens or generic-optics with 'deliveryChannelsStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcsrsResponseStatus :: Lens.Lens' DescribeDeliveryChannelStatusResponse Lude.Int
ddcsrsResponseStatus = Lens.lens (responseStatus :: DescribeDeliveryChannelStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDeliveryChannelStatusResponse)
{-# DEPRECATED ddcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
