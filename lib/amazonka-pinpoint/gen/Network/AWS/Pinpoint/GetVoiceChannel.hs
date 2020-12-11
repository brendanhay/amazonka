{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetVoiceChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the voice channel for an application.
module Network.AWS.Pinpoint.GetVoiceChannel
  ( -- * Creating a request
    GetVoiceChannel (..),
    mkGetVoiceChannel,

    -- ** Request lenses
    gvcApplicationId,

    -- * Destructuring the response
    GetVoiceChannelResponse (..),
    mkGetVoiceChannelResponse,

    -- ** Response lenses
    gvcrsResponseStatus,
    gvcrsVoiceChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetVoiceChannel' smart constructor.
newtype GetVoiceChannel = GetVoiceChannel'
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

-- | Creates a value of 'GetVoiceChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkGetVoiceChannel ::
  -- | 'applicationId'
  Lude.Text ->
  GetVoiceChannel
mkGetVoiceChannel pApplicationId_ =
  GetVoiceChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvcApplicationId :: Lens.Lens' GetVoiceChannel Lude.Text
gvcApplicationId = Lens.lens (applicationId :: GetVoiceChannel -> Lude.Text) (\s a -> s {applicationId = a} :: GetVoiceChannel)
{-# DEPRECATED gvcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetVoiceChannel where
  type Rs GetVoiceChannel = GetVoiceChannelResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetVoiceChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders GetVoiceChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetVoiceChannel where
  toPath GetVoiceChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/voice"]

instance Lude.ToQuery GetVoiceChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetVoiceChannelResponse' smart constructor.
data GetVoiceChannelResponse = GetVoiceChannelResponse'
  { responseStatus ::
      Lude.Int,
    voiceChannelResponse ::
      VoiceChannelResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetVoiceChannelResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'voiceChannelResponse' - Undocumented field.
mkGetVoiceChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'voiceChannelResponse'
  VoiceChannelResponse ->
  GetVoiceChannelResponse
mkGetVoiceChannelResponse pResponseStatus_ pVoiceChannelResponse_ =
  GetVoiceChannelResponse'
    { responseStatus = pResponseStatus_,
      voiceChannelResponse = pVoiceChannelResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvcrsResponseStatus :: Lens.Lens' GetVoiceChannelResponse Lude.Int
gvcrsResponseStatus = Lens.lens (responseStatus :: GetVoiceChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetVoiceChannelResponse)
{-# DEPRECATED gvcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'voiceChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvcrsVoiceChannelResponse :: Lens.Lens' GetVoiceChannelResponse VoiceChannelResponse
gvcrsVoiceChannelResponse = Lens.lens (voiceChannelResponse :: GetVoiceChannelResponse -> VoiceChannelResponse) (\s a -> s {voiceChannelResponse = a} :: GetVoiceChannelResponse)
{-# DEPRECATED gvcrsVoiceChannelResponse "Use generic-lens or generic-optics with 'voiceChannelResponse' instead." #-}
