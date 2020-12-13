{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteGCMChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the GCM channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteGCMChannel
  ( -- * Creating a request
    DeleteGCMChannel (..),
    mkDeleteGCMChannel,

    -- ** Request lenses
    dgcApplicationId,

    -- * Destructuring the response
    DeleteGCMChannelResponse (..),
    mkDeleteGCMChannelResponse,

    -- ** Response lenses
    dgcrsGCMChannelResponse,
    dgcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteGCMChannel' smart constructor.
newtype DeleteGCMChannel = DeleteGCMChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGCMChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkDeleteGCMChannel ::
  -- | 'applicationId'
  Lude.Text ->
  DeleteGCMChannel
mkDeleteGCMChannel pApplicationId_ =
  DeleteGCMChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcApplicationId :: Lens.Lens' DeleteGCMChannel Lude.Text
dgcApplicationId = Lens.lens (applicationId :: DeleteGCMChannel -> Lude.Text) (\s a -> s {applicationId = a} :: DeleteGCMChannel)
{-# DEPRECATED dgcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest DeleteGCMChannel where
  type Rs DeleteGCMChannel = DeleteGCMChannelResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteGCMChannelResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteGCMChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteGCMChannel where
  toPath DeleteGCMChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/gcm"]

instance Lude.ToQuery DeleteGCMChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteGCMChannelResponse' smart constructor.
data DeleteGCMChannelResponse = DeleteGCMChannelResponse'
  { gcmChannelResponse :: GCMChannelResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGCMChannelResponse' with the minimum fields required to make a request.
--
-- * 'gcmChannelResponse' -
-- * 'responseStatus' - The response status code.
mkDeleteGCMChannelResponse ::
  -- | 'gcmChannelResponse'
  GCMChannelResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteGCMChannelResponse
mkDeleteGCMChannelResponse pGCMChannelResponse_ pResponseStatus_ =
  DeleteGCMChannelResponse'
    { gcmChannelResponse =
        pGCMChannelResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gcmChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcrsGCMChannelResponse :: Lens.Lens' DeleteGCMChannelResponse GCMChannelResponse
dgcrsGCMChannelResponse = Lens.lens (gcmChannelResponse :: DeleteGCMChannelResponse -> GCMChannelResponse) (\s a -> s {gcmChannelResponse = a} :: DeleteGCMChannelResponse)
{-# DEPRECATED dgcrsGCMChannelResponse "Use generic-lens or generic-optics with 'gcmChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcrsResponseStatus :: Lens.Lens' DeleteGCMChannelResponse Lude.Int
dgcrsResponseStatus = Lens.lens (responseStatus :: DeleteGCMChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteGCMChannelResponse)
{-# DEPRECATED dgcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
