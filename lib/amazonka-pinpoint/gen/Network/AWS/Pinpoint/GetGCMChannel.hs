{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetGCMChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the GCM channel for an application.
module Network.AWS.Pinpoint.GetGCMChannel
  ( -- * Creating a request
    GetGCMChannel (..),
    mkGetGCMChannel,

    -- ** Request lenses
    ggcApplicationId,

    -- * Destructuring the response
    GetGCMChannelResponse (..),
    mkGetGCMChannelResponse,

    -- ** Response lenses
    ggcrsGCMChannelResponse,
    ggcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetGCMChannel' smart constructor.
newtype GetGCMChannel = GetGCMChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGCMChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkGetGCMChannel ::
  -- | 'applicationId'
  Lude.Text ->
  GetGCMChannel
mkGetGCMChannel pApplicationId_ =
  GetGCMChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcApplicationId :: Lens.Lens' GetGCMChannel Lude.Text
ggcApplicationId = Lens.lens (applicationId :: GetGCMChannel -> Lude.Text) (\s a -> s {applicationId = a} :: GetGCMChannel)
{-# DEPRECATED ggcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetGCMChannel where
  type Rs GetGCMChannel = GetGCMChannelResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetGCMChannelResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGCMChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetGCMChannel where
  toPath GetGCMChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/gcm"]

instance Lude.ToQuery GetGCMChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetGCMChannelResponse' smart constructor.
data GetGCMChannelResponse = GetGCMChannelResponse'
  { gcmChannelResponse :: GCMChannelResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGCMChannelResponse' with the minimum fields required to make a request.
--
-- * 'gcmChannelResponse' -
-- * 'responseStatus' - The response status code.
mkGetGCMChannelResponse ::
  -- | 'gcmChannelResponse'
  GCMChannelResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetGCMChannelResponse
mkGetGCMChannelResponse pGCMChannelResponse_ pResponseStatus_ =
  GetGCMChannelResponse'
    { gcmChannelResponse = pGCMChannelResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gcmChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcrsGCMChannelResponse :: Lens.Lens' GetGCMChannelResponse GCMChannelResponse
ggcrsGCMChannelResponse = Lens.lens (gcmChannelResponse :: GetGCMChannelResponse -> GCMChannelResponse) (\s a -> s {gcmChannelResponse = a} :: GetGCMChannelResponse)
{-# DEPRECATED ggcrsGCMChannelResponse "Use generic-lens or generic-optics with 'gcmChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcrsResponseStatus :: Lens.Lens' GetGCMChannelResponse Lude.Int
ggcrsResponseStatus = Lens.lens (responseStatus :: GetGCMChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGCMChannelResponse)
{-# DEPRECATED ggcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
