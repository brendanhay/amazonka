{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteAPNSVoipChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the APNs VoIP channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteAPNSVoipChannel
  ( -- * Creating a request
    DeleteAPNSVoipChannel (..),
    mkDeleteAPNSVoipChannel,

    -- ** Request lenses
    davcApplicationId,

    -- * Destructuring the response
    DeleteAPNSVoipChannelResponse (..),
    mkDeleteAPNSVoipChannelResponse,

    -- ** Response lenses
    davcrsAPNSVoipChannelResponse,
    davcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAPNSVoipChannel' smart constructor.
newtype DeleteAPNSVoipChannel = DeleteAPNSVoipChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAPNSVoipChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkDeleteAPNSVoipChannel ::
  -- | 'applicationId'
  Lude.Text ->
  DeleteAPNSVoipChannel
mkDeleteAPNSVoipChannel pApplicationId_ =
  DeleteAPNSVoipChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davcApplicationId :: Lens.Lens' DeleteAPNSVoipChannel Lude.Text
davcApplicationId = Lens.lens (applicationId :: DeleteAPNSVoipChannel -> Lude.Text) (\s a -> s {applicationId = a} :: DeleteAPNSVoipChannel)
{-# DEPRECATED davcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest DeleteAPNSVoipChannel where
  type Rs DeleteAPNSVoipChannel = DeleteAPNSVoipChannelResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteAPNSVoipChannelResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAPNSVoipChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteAPNSVoipChannel where
  toPath DeleteAPNSVoipChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/apns_voip"]

instance Lude.ToQuery DeleteAPNSVoipChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAPNSVoipChannelResponse' smart constructor.
data DeleteAPNSVoipChannelResponse = DeleteAPNSVoipChannelResponse'
  { apnsVoipChannelResponse :: APNSVoipChannelResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAPNSVoipChannelResponse' with the minimum fields required to make a request.
--
-- * 'apnsVoipChannelResponse' -
-- * 'responseStatus' - The response status code.
mkDeleteAPNSVoipChannelResponse ::
  -- | 'apnsVoipChannelResponse'
  APNSVoipChannelResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAPNSVoipChannelResponse
mkDeleteAPNSVoipChannelResponse
  pAPNSVoipChannelResponse_
  pResponseStatus_ =
    DeleteAPNSVoipChannelResponse'
      { apnsVoipChannelResponse =
          pAPNSVoipChannelResponse_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsVoipChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davcrsAPNSVoipChannelResponse :: Lens.Lens' DeleteAPNSVoipChannelResponse APNSVoipChannelResponse
davcrsAPNSVoipChannelResponse = Lens.lens (apnsVoipChannelResponse :: DeleteAPNSVoipChannelResponse -> APNSVoipChannelResponse) (\s a -> s {apnsVoipChannelResponse = a} :: DeleteAPNSVoipChannelResponse)
{-# DEPRECATED davcrsAPNSVoipChannelResponse "Use generic-lens or generic-optics with 'apnsVoipChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davcrsResponseStatus :: Lens.Lens' DeleteAPNSVoipChannelResponse Lude.Int
davcrsResponseStatus = Lens.lens (responseStatus :: DeleteAPNSVoipChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAPNSVoipChannelResponse)
{-# DEPRECATED davcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
