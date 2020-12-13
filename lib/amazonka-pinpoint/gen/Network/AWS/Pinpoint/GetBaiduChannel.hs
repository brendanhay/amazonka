{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetBaiduChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the Baidu channel for an application.
module Network.AWS.Pinpoint.GetBaiduChannel
  ( -- * Creating a request
    GetBaiduChannel (..),
    mkGetBaiduChannel,

    -- ** Request lenses
    gbcApplicationId,

    -- * Destructuring the response
    GetBaiduChannelResponse (..),
    mkGetBaiduChannelResponse,

    -- ** Response lenses
    gbcrsBaiduChannelResponse,
    gbcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetBaiduChannel' smart constructor.
newtype GetBaiduChannel = GetBaiduChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBaiduChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkGetBaiduChannel ::
  -- | 'applicationId'
  Lude.Text ->
  GetBaiduChannel
mkGetBaiduChannel pApplicationId_ =
  GetBaiduChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcApplicationId :: Lens.Lens' GetBaiduChannel Lude.Text
gbcApplicationId = Lens.lens (applicationId :: GetBaiduChannel -> Lude.Text) (\s a -> s {applicationId = a} :: GetBaiduChannel)
{-# DEPRECATED gbcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetBaiduChannel where
  type Rs GetBaiduChannel = GetBaiduChannelResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBaiduChannelResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBaiduChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetBaiduChannel where
  toPath GetBaiduChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/baidu"]

instance Lude.ToQuery GetBaiduChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetBaiduChannelResponse' smart constructor.
data GetBaiduChannelResponse = GetBaiduChannelResponse'
  { baiduChannelResponse :: BaiduChannelResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBaiduChannelResponse' with the minimum fields required to make a request.
--
-- * 'baiduChannelResponse' -
-- * 'responseStatus' - The response status code.
mkGetBaiduChannelResponse ::
  -- | 'baiduChannelResponse'
  BaiduChannelResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetBaiduChannelResponse
mkGetBaiduChannelResponse pBaiduChannelResponse_ pResponseStatus_ =
  GetBaiduChannelResponse'
    { baiduChannelResponse =
        pBaiduChannelResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'baiduChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcrsBaiduChannelResponse :: Lens.Lens' GetBaiduChannelResponse BaiduChannelResponse
gbcrsBaiduChannelResponse = Lens.lens (baiduChannelResponse :: GetBaiduChannelResponse -> BaiduChannelResponse) (\s a -> s {baiduChannelResponse = a} :: GetBaiduChannelResponse)
{-# DEPRECATED gbcrsBaiduChannelResponse "Use generic-lens or generic-optics with 'baiduChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcrsResponseStatus :: Lens.Lens' GetBaiduChannelResponse Lude.Int
gbcrsResponseStatus = Lens.lens (responseStatus :: GetBaiduChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBaiduChannelResponse)
{-# DEPRECATED gbcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
