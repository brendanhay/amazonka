{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteAPNSChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the APNs channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteAPNSChannel
  ( -- * Creating a request
    DeleteAPNSChannel (..),
    mkDeleteAPNSChannel,

    -- ** Request lenses
    dacApplicationId,

    -- * Destructuring the response
    DeleteAPNSChannelResponse (..),
    mkDeleteAPNSChannelResponse,

    -- ** Response lenses
    dacrsAPNSChannelResponse,
    dacrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAPNSChannel' smart constructor.
newtype DeleteAPNSChannel = DeleteAPNSChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAPNSChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkDeleteAPNSChannel ::
  -- | 'applicationId'
  Lude.Text ->
  DeleteAPNSChannel
mkDeleteAPNSChannel pApplicationId_ =
  DeleteAPNSChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacApplicationId :: Lens.Lens' DeleteAPNSChannel Lude.Text
dacApplicationId = Lens.lens (applicationId :: DeleteAPNSChannel -> Lude.Text) (\s a -> s {applicationId = a} :: DeleteAPNSChannel)
{-# DEPRECATED dacApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest DeleteAPNSChannel where
  type Rs DeleteAPNSChannel = DeleteAPNSChannelResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteAPNSChannelResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAPNSChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteAPNSChannel where
  toPath DeleteAPNSChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/apns"]

instance Lude.ToQuery DeleteAPNSChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAPNSChannelResponse' smart constructor.
data DeleteAPNSChannelResponse = DeleteAPNSChannelResponse'
  { apnsChannelResponse :: APNSChannelResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAPNSChannelResponse' with the minimum fields required to make a request.
--
-- * 'apnsChannelResponse' -
-- * 'responseStatus' - The response status code.
mkDeleteAPNSChannelResponse ::
  -- | 'apnsChannelResponse'
  APNSChannelResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAPNSChannelResponse
mkDeleteAPNSChannelResponse pAPNSChannelResponse_ pResponseStatus_ =
  DeleteAPNSChannelResponse'
    { apnsChannelResponse =
        pAPNSChannelResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacrsAPNSChannelResponse :: Lens.Lens' DeleteAPNSChannelResponse APNSChannelResponse
dacrsAPNSChannelResponse = Lens.lens (apnsChannelResponse :: DeleteAPNSChannelResponse -> APNSChannelResponse) (\s a -> s {apnsChannelResponse = a} :: DeleteAPNSChannelResponse)
{-# DEPRECATED dacrsAPNSChannelResponse "Use generic-lens or generic-optics with 'apnsChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacrsResponseStatus :: Lens.Lens' DeleteAPNSChannelResponse Lude.Int
dacrsResponseStatus = Lens.lens (responseStatus :: DeleteAPNSChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAPNSChannelResponse)
{-# DEPRECATED dacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
