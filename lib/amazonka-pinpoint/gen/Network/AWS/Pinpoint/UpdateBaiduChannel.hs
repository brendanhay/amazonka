{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateBaiduChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the Baidu channel for an application or updates the status and settings of the Baidu channel for an application.
module Network.AWS.Pinpoint.UpdateBaiduChannel
  ( -- * Creating a request
    UpdateBaiduChannel (..),
    mkUpdateBaiduChannel,

    -- ** Request lenses
    ubcApplicationId,
    ubcBaiduChannelRequest,

    -- * Destructuring the response
    UpdateBaiduChannelResponse (..),
    mkUpdateBaiduChannelResponse,

    -- ** Response lenses
    ubcrsBaiduChannelResponse,
    ubcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateBaiduChannel' smart constructor.
data UpdateBaiduChannel = UpdateBaiduChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    baiduChannelRequest :: BaiduChannelRequest
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateBaiduChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'baiduChannelRequest' -
mkUpdateBaiduChannel ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'baiduChannelRequest'
  BaiduChannelRequest ->
  UpdateBaiduChannel
mkUpdateBaiduChannel pApplicationId_ pBaiduChannelRequest_ =
  UpdateBaiduChannel'
    { applicationId = pApplicationId_,
      baiduChannelRequest = pBaiduChannelRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubcApplicationId :: Lens.Lens' UpdateBaiduChannel Lude.Text
ubcApplicationId = Lens.lens (applicationId :: UpdateBaiduChannel -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateBaiduChannel)
{-# DEPRECATED ubcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'baiduChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubcBaiduChannelRequest :: Lens.Lens' UpdateBaiduChannel BaiduChannelRequest
ubcBaiduChannelRequest = Lens.lens (baiduChannelRequest :: UpdateBaiduChannel -> BaiduChannelRequest) (\s a -> s {baiduChannelRequest = a} :: UpdateBaiduChannel)
{-# DEPRECATED ubcBaiduChannelRequest "Use generic-lens or generic-optics with 'baiduChannelRequest' instead." #-}

instance Lude.AWSRequest UpdateBaiduChannel where
  type Rs UpdateBaiduChannel = UpdateBaiduChannelResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateBaiduChannelResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateBaiduChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateBaiduChannel where
  toJSON UpdateBaiduChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("BaiduChannelRequest" Lude..= baiduChannelRequest)]
      )

instance Lude.ToPath UpdateBaiduChannel where
  toPath UpdateBaiduChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/baidu"]

instance Lude.ToQuery UpdateBaiduChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateBaiduChannelResponse' smart constructor.
data UpdateBaiduChannelResponse = UpdateBaiduChannelResponse'
  { baiduChannelResponse :: BaiduChannelResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateBaiduChannelResponse' with the minimum fields required to make a request.
--
-- * 'baiduChannelResponse' -
-- * 'responseStatus' - The response status code.
mkUpdateBaiduChannelResponse ::
  -- | 'baiduChannelResponse'
  BaiduChannelResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateBaiduChannelResponse
mkUpdateBaiduChannelResponse
  pBaiduChannelResponse_
  pResponseStatus_ =
    UpdateBaiduChannelResponse'
      { baiduChannelResponse =
          pBaiduChannelResponse_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'baiduChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubcrsBaiduChannelResponse :: Lens.Lens' UpdateBaiduChannelResponse BaiduChannelResponse
ubcrsBaiduChannelResponse = Lens.lens (baiduChannelResponse :: UpdateBaiduChannelResponse -> BaiduChannelResponse) (\s a -> s {baiduChannelResponse = a} :: UpdateBaiduChannelResponse)
{-# DEPRECATED ubcrsBaiduChannelResponse "Use generic-lens or generic-optics with 'baiduChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubcrsResponseStatus :: Lens.Lens' UpdateBaiduChannelResponse Lude.Int
ubcrsResponseStatus = Lens.lens (responseStatus :: UpdateBaiduChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateBaiduChannelResponse)
{-# DEPRECATED ubcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
