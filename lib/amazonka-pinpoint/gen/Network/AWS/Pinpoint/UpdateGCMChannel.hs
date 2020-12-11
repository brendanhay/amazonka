{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateGCMChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the GCM channel for an application or updates the status and settings of the GCM channel for an application.
module Network.AWS.Pinpoint.UpdateGCMChannel
  ( -- * Creating a request
    UpdateGCMChannel (..),
    mkUpdateGCMChannel,

    -- ** Request lenses
    ugcApplicationId,
    ugcGCMChannelRequest,

    -- * Destructuring the response
    UpdateGCMChannelResponse (..),
    mkUpdateGCMChannelResponse,

    -- ** Response lenses
    ugcrsResponseStatus,
    ugcrsGCMChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateGCMChannel' smart constructor.
data UpdateGCMChannel = UpdateGCMChannel'
  { applicationId ::
      Lude.Text,
    gcmChannelRequest :: GCMChannelRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGCMChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'gcmChannelRequest' - Undocumented field.
mkUpdateGCMChannel ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'gcmChannelRequest'
  GCMChannelRequest ->
  UpdateGCMChannel
mkUpdateGCMChannel pApplicationId_ pGCMChannelRequest_ =
  UpdateGCMChannel'
    { applicationId = pApplicationId_,
      gcmChannelRequest = pGCMChannelRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugcApplicationId :: Lens.Lens' UpdateGCMChannel Lude.Text
ugcApplicationId = Lens.lens (applicationId :: UpdateGCMChannel -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateGCMChannel)
{-# DEPRECATED ugcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gcmChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugcGCMChannelRequest :: Lens.Lens' UpdateGCMChannel GCMChannelRequest
ugcGCMChannelRequest = Lens.lens (gcmChannelRequest :: UpdateGCMChannel -> GCMChannelRequest) (\s a -> s {gcmChannelRequest = a} :: UpdateGCMChannel)
{-# DEPRECATED ugcGCMChannelRequest "Use generic-lens or generic-optics with 'gcmChannelRequest' instead." #-}

instance Lude.AWSRequest UpdateGCMChannel where
  type Rs UpdateGCMChannel = UpdateGCMChannelResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateGCMChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders UpdateGCMChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateGCMChannel where
  toJSON UpdateGCMChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("GCMChannelRequest" Lude..= gcmChannelRequest)]
      )

instance Lude.ToPath UpdateGCMChannel where
  toPath UpdateGCMChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/gcm"]

instance Lude.ToQuery UpdateGCMChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateGCMChannelResponse' smart constructor.
data UpdateGCMChannelResponse = UpdateGCMChannelResponse'
  { responseStatus ::
      Lude.Int,
    gcmChannelResponse :: GCMChannelResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGCMChannelResponse' with the minimum fields required to make a request.
--
-- * 'gcmChannelResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateGCMChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'gcmChannelResponse'
  GCMChannelResponse ->
  UpdateGCMChannelResponse
mkUpdateGCMChannelResponse pResponseStatus_ pGCMChannelResponse_ =
  UpdateGCMChannelResponse'
    { responseStatus = pResponseStatus_,
      gcmChannelResponse = pGCMChannelResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugcrsResponseStatus :: Lens.Lens' UpdateGCMChannelResponse Lude.Int
ugcrsResponseStatus = Lens.lens (responseStatus :: UpdateGCMChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGCMChannelResponse)
{-# DEPRECATED ugcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gcmChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugcrsGCMChannelResponse :: Lens.Lens' UpdateGCMChannelResponse GCMChannelResponse
ugcrsGCMChannelResponse = Lens.lens (gcmChannelResponse :: UpdateGCMChannelResponse -> GCMChannelResponse) (\s a -> s {gcmChannelResponse = a} :: UpdateGCMChannelResponse)
{-# DEPRECATED ugcrsGCMChannelResponse "Use generic-lens or generic-optics with 'gcmChannelResponse' instead." #-}
