{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteBaiduChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the Baidu channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteBaiduChannel
  ( -- * Creating a request
    DeleteBaiduChannel (..),
    mkDeleteBaiduChannel,

    -- ** Request lenses
    dbcApplicationId,

    -- * Destructuring the response
    DeleteBaiduChannelResponse (..),
    mkDeleteBaiduChannelResponse,

    -- ** Response lenses
    dbcrsBaiduChannelResponse,
    dbcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteBaiduChannel' smart constructor.
newtype DeleteBaiduChannel = DeleteBaiduChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBaiduChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkDeleteBaiduChannel ::
  -- | 'applicationId'
  Lude.Text ->
  DeleteBaiduChannel
mkDeleteBaiduChannel pApplicationId_ =
  DeleteBaiduChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcApplicationId :: Lens.Lens' DeleteBaiduChannel Lude.Text
dbcApplicationId = Lens.lens (applicationId :: DeleteBaiduChannel -> Lude.Text) (\s a -> s {applicationId = a} :: DeleteBaiduChannel)
{-# DEPRECATED dbcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest DeleteBaiduChannel where
  type Rs DeleteBaiduChannel = DeleteBaiduChannelResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteBaiduChannelResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteBaiduChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteBaiduChannel where
  toPath DeleteBaiduChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/baidu"]

instance Lude.ToQuery DeleteBaiduChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteBaiduChannelResponse' smart constructor.
data DeleteBaiduChannelResponse = DeleteBaiduChannelResponse'
  { baiduChannelResponse :: BaiduChannelResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBaiduChannelResponse' with the minimum fields required to make a request.
--
-- * 'baiduChannelResponse' -
-- * 'responseStatus' - The response status code.
mkDeleteBaiduChannelResponse ::
  -- | 'baiduChannelResponse'
  BaiduChannelResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteBaiduChannelResponse
mkDeleteBaiduChannelResponse
  pBaiduChannelResponse_
  pResponseStatus_ =
    DeleteBaiduChannelResponse'
      { baiduChannelResponse =
          pBaiduChannelResponse_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'baiduChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcrsBaiduChannelResponse :: Lens.Lens' DeleteBaiduChannelResponse BaiduChannelResponse
dbcrsBaiduChannelResponse = Lens.lens (baiduChannelResponse :: DeleteBaiduChannelResponse -> BaiduChannelResponse) (\s a -> s {baiduChannelResponse = a} :: DeleteBaiduChannelResponse)
{-# DEPRECATED dbcrsBaiduChannelResponse "Use generic-lens or generic-optics with 'baiduChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcrsResponseStatus :: Lens.Lens' DeleteBaiduChannelResponse Lude.Int
dbcrsResponseStatus = Lens.lens (responseStatus :: DeleteBaiduChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteBaiduChannelResponse)
{-# DEPRECATED dbcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
