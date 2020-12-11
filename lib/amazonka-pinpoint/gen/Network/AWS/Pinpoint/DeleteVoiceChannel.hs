{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteVoiceChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the voice channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteVoiceChannel
  ( -- * Creating a request
    DeleteVoiceChannel (..),
    mkDeleteVoiceChannel,

    -- ** Request lenses
    dvcApplicationId,

    -- * Destructuring the response
    DeleteVoiceChannelResponse (..),
    mkDeleteVoiceChannelResponse,

    -- ** Response lenses
    dvcrsResponseStatus,
    dvcrsVoiceChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteVoiceChannel' smart constructor.
newtype DeleteVoiceChannel = DeleteVoiceChannel'
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

-- | Creates a value of 'DeleteVoiceChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkDeleteVoiceChannel ::
  -- | 'applicationId'
  Lude.Text ->
  DeleteVoiceChannel
mkDeleteVoiceChannel pApplicationId_ =
  DeleteVoiceChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcApplicationId :: Lens.Lens' DeleteVoiceChannel Lude.Text
dvcApplicationId = Lens.lens (applicationId :: DeleteVoiceChannel -> Lude.Text) (\s a -> s {applicationId = a} :: DeleteVoiceChannel)
{-# DEPRECATED dvcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest DeleteVoiceChannel where
  type Rs DeleteVoiceChannel = DeleteVoiceChannelResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteVoiceChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders DeleteVoiceChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteVoiceChannel where
  toPath DeleteVoiceChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/voice"]

instance Lude.ToQuery DeleteVoiceChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteVoiceChannelResponse' smart constructor.
data DeleteVoiceChannelResponse = DeleteVoiceChannelResponse'
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

-- | Creates a value of 'DeleteVoiceChannelResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'voiceChannelResponse' - Undocumented field.
mkDeleteVoiceChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'voiceChannelResponse'
  VoiceChannelResponse ->
  DeleteVoiceChannelResponse
mkDeleteVoiceChannelResponse
  pResponseStatus_
  pVoiceChannelResponse_ =
    DeleteVoiceChannelResponse'
      { responseStatus = pResponseStatus_,
        voiceChannelResponse = pVoiceChannelResponse_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcrsResponseStatus :: Lens.Lens' DeleteVoiceChannelResponse Lude.Int
dvcrsResponseStatus = Lens.lens (responseStatus :: DeleteVoiceChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteVoiceChannelResponse)
{-# DEPRECATED dvcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'voiceChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcrsVoiceChannelResponse :: Lens.Lens' DeleteVoiceChannelResponse VoiceChannelResponse
dvcrsVoiceChannelResponse = Lens.lens (voiceChannelResponse :: DeleteVoiceChannelResponse -> VoiceChannelResponse) (\s a -> s {voiceChannelResponse = a} :: DeleteVoiceChannelResponse)
{-# DEPRECATED dvcrsVoiceChannelResponse "Use generic-lens or generic-optics with 'voiceChannelResponse' instead." #-}
