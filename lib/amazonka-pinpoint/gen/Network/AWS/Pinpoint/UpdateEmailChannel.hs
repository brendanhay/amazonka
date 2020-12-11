{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateEmailChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the email channel for an application or updates the status and settings of the email channel for an application.
module Network.AWS.Pinpoint.UpdateEmailChannel
  ( -- * Creating a request
    UpdateEmailChannel (..),
    mkUpdateEmailChannel,

    -- ** Request lenses
    uecApplicationId,
    uecEmailChannelRequest,

    -- * Destructuring the response
    UpdateEmailChannelResponse (..),
    mkUpdateEmailChannelResponse,

    -- ** Response lenses
    uecrsResponseStatus,
    uecrsEmailChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateEmailChannel' smart constructor.
data UpdateEmailChannel = UpdateEmailChannel'
  { applicationId ::
      Lude.Text,
    emailChannelRequest :: EmailChannelRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEmailChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'emailChannelRequest' - Undocumented field.
mkUpdateEmailChannel ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'emailChannelRequest'
  EmailChannelRequest ->
  UpdateEmailChannel
mkUpdateEmailChannel pApplicationId_ pEmailChannelRequest_ =
  UpdateEmailChannel'
    { applicationId = pApplicationId_,
      emailChannelRequest = pEmailChannelRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecApplicationId :: Lens.Lens' UpdateEmailChannel Lude.Text
uecApplicationId = Lens.lens (applicationId :: UpdateEmailChannel -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateEmailChannel)
{-# DEPRECATED uecApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'emailChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecEmailChannelRequest :: Lens.Lens' UpdateEmailChannel EmailChannelRequest
uecEmailChannelRequest = Lens.lens (emailChannelRequest :: UpdateEmailChannel -> EmailChannelRequest) (\s a -> s {emailChannelRequest = a} :: UpdateEmailChannel)
{-# DEPRECATED uecEmailChannelRequest "Use generic-lens or generic-optics with 'emailChannelRequest' instead." #-}

instance Lude.AWSRequest UpdateEmailChannel where
  type Rs UpdateEmailChannel = UpdateEmailChannelResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateEmailChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders UpdateEmailChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateEmailChannel where
  toJSON UpdateEmailChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("EmailChannelRequest" Lude..= emailChannelRequest)]
      )

instance Lude.ToPath UpdateEmailChannel where
  toPath UpdateEmailChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/email"]

instance Lude.ToQuery UpdateEmailChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateEmailChannelResponse' smart constructor.
data UpdateEmailChannelResponse = UpdateEmailChannelResponse'
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

-- | Creates a value of 'UpdateEmailChannelResponse' with the minimum fields required to make a request.
--
-- * 'emailChannelResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateEmailChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'emailChannelResponse'
  EmailChannelResponse ->
  UpdateEmailChannelResponse
mkUpdateEmailChannelResponse
  pResponseStatus_
  pEmailChannelResponse_ =
    UpdateEmailChannelResponse'
      { responseStatus = pResponseStatus_,
        emailChannelResponse = pEmailChannelResponse_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecrsResponseStatus :: Lens.Lens' UpdateEmailChannelResponse Lude.Int
uecrsResponseStatus = Lens.lens (responseStatus :: UpdateEmailChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateEmailChannelResponse)
{-# DEPRECATED uecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'emailChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecrsEmailChannelResponse :: Lens.Lens' UpdateEmailChannelResponse EmailChannelResponse
uecrsEmailChannelResponse = Lens.lens (emailChannelResponse :: UpdateEmailChannelResponse -> EmailChannelResponse) (\s a -> s {emailChannelResponse = a} :: UpdateEmailChannelResponse)
{-# DEPRECATED uecrsEmailChannelResponse "Use generic-lens or generic-optics with 'emailChannelResponse' instead." #-}
