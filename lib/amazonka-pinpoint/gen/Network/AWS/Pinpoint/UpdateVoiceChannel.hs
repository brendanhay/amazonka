{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateVoiceChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the voice channel for an application or updates the status and settings of the voice channel for an application.
module Network.AWS.Pinpoint.UpdateVoiceChannel
  ( -- * Creating a request
    UpdateVoiceChannel (..),
    mkUpdateVoiceChannel,

    -- ** Request lenses
    uvcApplicationId,
    uvcVoiceChannelRequest,

    -- * Destructuring the response
    UpdateVoiceChannelResponse (..),
    mkUpdateVoiceChannelResponse,

    -- ** Response lenses
    uvcrsResponseStatus,
    uvcrsVoiceChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateVoiceChannel' smart constructor.
data UpdateVoiceChannel = UpdateVoiceChannel'
  { applicationId ::
      Lude.Text,
    voiceChannelRequest :: VoiceChannelRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateVoiceChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'voiceChannelRequest' - Undocumented field.
mkUpdateVoiceChannel ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'voiceChannelRequest'
  VoiceChannelRequest ->
  UpdateVoiceChannel
mkUpdateVoiceChannel pApplicationId_ pVoiceChannelRequest_ =
  UpdateVoiceChannel'
    { applicationId = pApplicationId_,
      voiceChannelRequest = pVoiceChannelRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvcApplicationId :: Lens.Lens' UpdateVoiceChannel Lude.Text
uvcApplicationId = Lens.lens (applicationId :: UpdateVoiceChannel -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateVoiceChannel)
{-# DEPRECATED uvcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'voiceChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvcVoiceChannelRequest :: Lens.Lens' UpdateVoiceChannel VoiceChannelRequest
uvcVoiceChannelRequest = Lens.lens (voiceChannelRequest :: UpdateVoiceChannel -> VoiceChannelRequest) (\s a -> s {voiceChannelRequest = a} :: UpdateVoiceChannel)
{-# DEPRECATED uvcVoiceChannelRequest "Use generic-lens or generic-optics with 'voiceChannelRequest' instead." #-}

instance Lude.AWSRequest UpdateVoiceChannel where
  type Rs UpdateVoiceChannel = UpdateVoiceChannelResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateVoiceChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders UpdateVoiceChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateVoiceChannel where
  toJSON UpdateVoiceChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("VoiceChannelRequest" Lude..= voiceChannelRequest)]
      )

instance Lude.ToPath UpdateVoiceChannel where
  toPath UpdateVoiceChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/voice"]

instance Lude.ToQuery UpdateVoiceChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateVoiceChannelResponse' smart constructor.
data UpdateVoiceChannelResponse = UpdateVoiceChannelResponse'
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

-- | Creates a value of 'UpdateVoiceChannelResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'voiceChannelResponse' - Undocumented field.
mkUpdateVoiceChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'voiceChannelResponse'
  VoiceChannelResponse ->
  UpdateVoiceChannelResponse
mkUpdateVoiceChannelResponse
  pResponseStatus_
  pVoiceChannelResponse_ =
    UpdateVoiceChannelResponse'
      { responseStatus = pResponseStatus_,
        voiceChannelResponse = pVoiceChannelResponse_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvcrsResponseStatus :: Lens.Lens' UpdateVoiceChannelResponse Lude.Int
uvcrsResponseStatus = Lens.lens (responseStatus :: UpdateVoiceChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateVoiceChannelResponse)
{-# DEPRECATED uvcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'voiceChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvcrsVoiceChannelResponse :: Lens.Lens' UpdateVoiceChannelResponse VoiceChannelResponse
uvcrsVoiceChannelResponse = Lens.lens (voiceChannelResponse :: UpdateVoiceChannelResponse -> VoiceChannelResponse) (\s a -> s {voiceChannelResponse = a} :: UpdateVoiceChannelResponse)
{-# DEPRECATED uvcrsVoiceChannelResponse "Use generic-lens or generic-optics with 'voiceChannelResponse' instead." #-}
