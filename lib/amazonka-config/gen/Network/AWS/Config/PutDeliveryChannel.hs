{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutDeliveryChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a delivery channel object to deliver configuration information to an Amazon S3 bucket and Amazon SNS topic.
--
-- Before you can create a delivery channel, you must create a configuration recorder.
-- You can use this action to change the Amazon S3 bucket or an Amazon SNS topic of the existing delivery channel. To change the Amazon S3 bucket or an Amazon SNS topic, call this action and specify the changed values for the S3 bucket and the SNS topic. If you specify a different value for either the S3 bucket or the SNS topic, this action will keep the existing value for the parameter that is not changed.
module Network.AWS.Config.PutDeliveryChannel
  ( -- * Creating a request
    PutDeliveryChannel (..),
    mkPutDeliveryChannel,

    -- ** Request lenses
    pdcDeliveryChannel,

    -- * Destructuring the response
    PutDeliveryChannelResponse (..),
    mkPutDeliveryChannelResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'PutDeliveryChannel' action.
--
-- /See:/ 'mkPutDeliveryChannel' smart constructor.
newtype PutDeliveryChannel = PutDeliveryChannel'
  { deliveryChannel ::
      DeliveryChannel
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutDeliveryChannel' with the minimum fields required to make a request.
--
-- * 'deliveryChannel' - The configuration delivery channel object that delivers the configuration information to an Amazon S3 bucket and to an Amazon SNS topic.
mkPutDeliveryChannel ::
  -- | 'deliveryChannel'
  DeliveryChannel ->
  PutDeliveryChannel
mkPutDeliveryChannel pDeliveryChannel_ =
  PutDeliveryChannel' {deliveryChannel = pDeliveryChannel_}

-- | The configuration delivery channel object that delivers the configuration information to an Amazon S3 bucket and to an Amazon SNS topic.
--
-- /Note:/ Consider using 'deliveryChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdcDeliveryChannel :: Lens.Lens' PutDeliveryChannel DeliveryChannel
pdcDeliveryChannel = Lens.lens (deliveryChannel :: PutDeliveryChannel -> DeliveryChannel) (\s a -> s {deliveryChannel = a} :: PutDeliveryChannel)
{-# DEPRECATED pdcDeliveryChannel "Use generic-lens or generic-optics with 'deliveryChannel' instead." #-}

instance Lude.AWSRequest PutDeliveryChannel where
  type Rs PutDeliveryChannel = PutDeliveryChannelResponse
  request = Req.postJSON configService
  response = Res.receiveNull PutDeliveryChannelResponse'

instance Lude.ToHeaders PutDeliveryChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StarlingDoveService.PutDeliveryChannel" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutDeliveryChannel where
  toJSON PutDeliveryChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("DeliveryChannel" Lude..= deliveryChannel)]
      )

instance Lude.ToPath PutDeliveryChannel where
  toPath = Lude.const "/"

instance Lude.ToQuery PutDeliveryChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutDeliveryChannelResponse' smart constructor.
data PutDeliveryChannelResponse = PutDeliveryChannelResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutDeliveryChannelResponse' with the minimum fields required to make a request.
mkPutDeliveryChannelResponse ::
  PutDeliveryChannelResponse
mkPutDeliveryChannelResponse = PutDeliveryChannelResponse'
