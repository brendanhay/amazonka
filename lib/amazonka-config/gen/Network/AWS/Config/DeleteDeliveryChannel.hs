{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteDeliveryChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the delivery channel.
--
-- Before you can delete the delivery channel, you must stop the configuration recorder by using the 'StopConfigurationRecorder' action.
module Network.AWS.Config.DeleteDeliveryChannel
  ( -- * Creating a request
    DeleteDeliveryChannel (..),
    mkDeleteDeliveryChannel,

    -- ** Request lenses
    ddcDeliveryChannelName,

    -- * Destructuring the response
    DeleteDeliveryChannelResponse (..),
    mkDeleteDeliveryChannelResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'DeleteDeliveryChannel' action. The action accepts the following data, in JSON format.
--
-- /See:/ 'mkDeleteDeliveryChannel' smart constructor.
newtype DeleteDeliveryChannel = DeleteDeliveryChannel'
  { -- | The name of the delivery channel to delete.
    deliveryChannelName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDeliveryChannel' with the minimum fields required to make a request.
--
-- * 'deliveryChannelName' - The name of the delivery channel to delete.
mkDeleteDeliveryChannel ::
  -- | 'deliveryChannelName'
  Lude.Text ->
  DeleteDeliveryChannel
mkDeleteDeliveryChannel pDeliveryChannelName_ =
  DeleteDeliveryChannel'
    { deliveryChannelName =
        pDeliveryChannelName_
    }

-- | The name of the delivery channel to delete.
--
-- /Note:/ Consider using 'deliveryChannelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDeliveryChannelName :: Lens.Lens' DeleteDeliveryChannel Lude.Text
ddcDeliveryChannelName = Lens.lens (deliveryChannelName :: DeleteDeliveryChannel -> Lude.Text) (\s a -> s {deliveryChannelName = a} :: DeleteDeliveryChannel)
{-# DEPRECATED ddcDeliveryChannelName "Use generic-lens or generic-optics with 'deliveryChannelName' instead." #-}

instance Lude.AWSRequest DeleteDeliveryChannel where
  type Rs DeleteDeliveryChannel = DeleteDeliveryChannelResponse
  request = Req.postJSON configService
  response = Res.receiveNull DeleteDeliveryChannelResponse'

instance Lude.ToHeaders DeleteDeliveryChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StarlingDoveService.DeleteDeliveryChannel" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDeliveryChannel where
  toJSON DeleteDeliveryChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("DeliveryChannelName" Lude..= deliveryChannelName)]
      )

instance Lude.ToPath DeleteDeliveryChannel where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDeliveryChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDeliveryChannelResponse' smart constructor.
data DeleteDeliveryChannelResponse = DeleteDeliveryChannelResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDeliveryChannelResponse' with the minimum fields required to make a request.
mkDeleteDeliveryChannelResponse ::
  DeleteDeliveryChannelResponse
mkDeleteDeliveryChannelResponse = DeleteDeliveryChannelResponse'
