{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteEmailChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the email channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteEmailChannel
  ( -- * Creating a request
    DeleteEmailChannel (..),
    mkDeleteEmailChannel,

    -- ** Request lenses
    decApplicationId,

    -- * Destructuring the response
    DeleteEmailChannelResponse (..),
    mkDeleteEmailChannelResponse,

    -- ** Response lenses
    decrsResponseStatus,
    decrsEmailChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteEmailChannel' smart constructor.
newtype DeleteEmailChannel = DeleteEmailChannel'
  { applicationId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEmailChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkDeleteEmailChannel ::
  -- | 'applicationId'
  Lude.Text ->
  DeleteEmailChannel
mkDeleteEmailChannel pApplicationId_ =
  DeleteEmailChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decApplicationId :: Lens.Lens' DeleteEmailChannel Lude.Text
decApplicationId = Lens.lens (applicationId :: DeleteEmailChannel -> Lude.Text) (\s a -> s {applicationId = a} :: DeleteEmailChannel)
{-# DEPRECATED decApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest DeleteEmailChannel where
  type Rs DeleteEmailChannel = DeleteEmailChannelResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteEmailChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders DeleteEmailChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteEmailChannel where
  toPath DeleteEmailChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/email"]

instance Lude.ToQuery DeleteEmailChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteEmailChannelResponse' smart constructor.
data DeleteEmailChannelResponse = DeleteEmailChannelResponse'
  { responseStatus ::
      Lude.Int,
    emailChannelResponse ::
      EmailChannelResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEmailChannelResponse' with the minimum fields required to make a request.
--
-- * 'emailChannelResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteEmailChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'emailChannelResponse'
  EmailChannelResponse ->
  DeleteEmailChannelResponse
mkDeleteEmailChannelResponse
  pResponseStatus_
  pEmailChannelResponse_ =
    DeleteEmailChannelResponse'
      { responseStatus = pResponseStatus_,
        emailChannelResponse = pEmailChannelResponse_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsResponseStatus :: Lens.Lens' DeleteEmailChannelResponse Lude.Int
decrsResponseStatus = Lens.lens (responseStatus :: DeleteEmailChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteEmailChannelResponse)
{-# DEPRECATED decrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'emailChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsEmailChannelResponse :: Lens.Lens' DeleteEmailChannelResponse EmailChannelResponse
decrsEmailChannelResponse = Lens.lens (emailChannelResponse :: DeleteEmailChannelResponse -> EmailChannelResponse) (\s a -> s {emailChannelResponse = a} :: DeleteEmailChannelResponse)
{-# DEPRECATED decrsEmailChannelResponse "Use generic-lens or generic-optics with 'emailChannelResponse' instead." #-}
