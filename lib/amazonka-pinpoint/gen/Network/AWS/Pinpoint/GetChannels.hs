{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetChannels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the history and status of each channel for an application.
module Network.AWS.Pinpoint.GetChannels
  ( -- * Creating a request
    GetChannels (..),
    mkGetChannels,

    -- ** Request lenses
    gcApplicationId,

    -- * Destructuring the response
    GetChannelsResponse (..),
    mkGetChannelsResponse,

    -- ** Response lenses
    gcsrsChannelsResponse,
    gcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetChannels' smart constructor.
newtype GetChannels = GetChannels'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetChannels' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkGetChannels ::
  -- | 'applicationId'
  Lude.Text ->
  GetChannels
mkGetChannels pApplicationId_ =
  GetChannels' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcApplicationId :: Lens.Lens' GetChannels Lude.Text
gcApplicationId = Lens.lens (applicationId :: GetChannels -> Lude.Text) (\s a -> s {applicationId = a} :: GetChannels)
{-# DEPRECATED gcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetChannels where
  type Rs GetChannels = GetChannelsResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetChannelsResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetChannels where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetChannels where
  toPath GetChannels' {..} =
    Lude.mconcat ["/v1/apps/", Lude.toBS applicationId, "/channels"]

instance Lude.ToQuery GetChannels where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetChannelsResponse' smart constructor.
data GetChannelsResponse = GetChannelsResponse'
  { channelsResponse :: ChannelsResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetChannelsResponse' with the minimum fields required to make a request.
--
-- * 'channelsResponse' -
-- * 'responseStatus' - The response status code.
mkGetChannelsResponse ::
  -- | 'channelsResponse'
  ChannelsResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetChannelsResponse
mkGetChannelsResponse pChannelsResponse_ pResponseStatus_ =
  GetChannelsResponse'
    { channelsResponse = pChannelsResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'channelsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsChannelsResponse :: Lens.Lens' GetChannelsResponse ChannelsResponse
gcsrsChannelsResponse = Lens.lens (channelsResponse :: GetChannelsResponse -> ChannelsResponse) (\s a -> s {channelsResponse = a} :: GetChannelsResponse)
{-# DEPRECATED gcsrsChannelsResponse "Use generic-lens or generic-optics with 'channelsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsResponseStatus :: Lens.Lens' GetChannelsResponse Lude.Int
gcsrsResponseStatus = Lens.lens (responseStatus :: GetChannelsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetChannelsResponse)
{-# DEPRECATED gcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
