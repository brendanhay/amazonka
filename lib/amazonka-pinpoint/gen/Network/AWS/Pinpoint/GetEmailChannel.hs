{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetEmailChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the email channel for an application.
module Network.AWS.Pinpoint.GetEmailChannel
  ( -- * Creating a request
    GetEmailChannel (..),
    mkGetEmailChannel,

    -- ** Request lenses
    gecApplicationId,

    -- * Destructuring the response
    GetEmailChannelResponse (..),
    mkGetEmailChannelResponse,

    -- ** Response lenses
    gecrsEmailChannelResponse,
    gecrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetEmailChannel' smart constructor.
newtype GetEmailChannel = GetEmailChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEmailChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkGetEmailChannel ::
  -- | 'applicationId'
  Lude.Text ->
  GetEmailChannel
mkGetEmailChannel pApplicationId_ =
  GetEmailChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gecApplicationId :: Lens.Lens' GetEmailChannel Lude.Text
gecApplicationId = Lens.lens (applicationId :: GetEmailChannel -> Lude.Text) (\s a -> s {applicationId = a} :: GetEmailChannel)
{-# DEPRECATED gecApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetEmailChannel where
  type Rs GetEmailChannel = GetEmailChannelResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetEmailChannelResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetEmailChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetEmailChannel where
  toPath GetEmailChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/email"]

instance Lude.ToQuery GetEmailChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetEmailChannelResponse' smart constructor.
data GetEmailChannelResponse = GetEmailChannelResponse'
  { emailChannelResponse :: EmailChannelResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEmailChannelResponse' with the minimum fields required to make a request.
--
-- * 'emailChannelResponse' -
-- * 'responseStatus' - The response status code.
mkGetEmailChannelResponse ::
  -- | 'emailChannelResponse'
  EmailChannelResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetEmailChannelResponse
mkGetEmailChannelResponse pEmailChannelResponse_ pResponseStatus_ =
  GetEmailChannelResponse'
    { emailChannelResponse =
        pEmailChannelResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'emailChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gecrsEmailChannelResponse :: Lens.Lens' GetEmailChannelResponse EmailChannelResponse
gecrsEmailChannelResponse = Lens.lens (emailChannelResponse :: GetEmailChannelResponse -> EmailChannelResponse) (\s a -> s {emailChannelResponse = a} :: GetEmailChannelResponse)
{-# DEPRECATED gecrsEmailChannelResponse "Use generic-lens or generic-optics with 'emailChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gecrsResponseStatus :: Lens.Lens' GetEmailChannelResponse Lude.Int
gecrsResponseStatus = Lens.lens (responseStatus :: GetEmailChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetEmailChannelResponse)
{-# DEPRECATED gecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
