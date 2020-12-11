{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteAPNSVoipSandboxChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the APNs VoIP sandbox channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteAPNSVoipSandboxChannel
  ( -- * Creating a request
    DeleteAPNSVoipSandboxChannel (..),
    mkDeleteAPNSVoipSandboxChannel,

    -- ** Request lenses
    davscApplicationId,

    -- * Destructuring the response
    DeleteAPNSVoipSandboxChannelResponse (..),
    mkDeleteAPNSVoipSandboxChannelResponse,

    -- ** Response lenses
    davscrsResponseStatus,
    davscrsAPNSVoipSandboxChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAPNSVoipSandboxChannel' smart constructor.
newtype DeleteAPNSVoipSandboxChannel = DeleteAPNSVoipSandboxChannel'
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

-- | Creates a value of 'DeleteAPNSVoipSandboxChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkDeleteAPNSVoipSandboxChannel ::
  -- | 'applicationId'
  Lude.Text ->
  DeleteAPNSVoipSandboxChannel
mkDeleteAPNSVoipSandboxChannel pApplicationId_ =
  DeleteAPNSVoipSandboxChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davscApplicationId :: Lens.Lens' DeleteAPNSVoipSandboxChannel Lude.Text
davscApplicationId = Lens.lens (applicationId :: DeleteAPNSVoipSandboxChannel -> Lude.Text) (\s a -> s {applicationId = a} :: DeleteAPNSVoipSandboxChannel)
{-# DEPRECATED davscApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest DeleteAPNSVoipSandboxChannel where
  type
    Rs DeleteAPNSVoipSandboxChannel =
      DeleteAPNSVoipSandboxChannelResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteAPNSVoipSandboxChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders DeleteAPNSVoipSandboxChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteAPNSVoipSandboxChannel where
  toPath DeleteAPNSVoipSandboxChannel' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/channels/apns_voip_sandbox"
      ]

instance Lude.ToQuery DeleteAPNSVoipSandboxChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAPNSVoipSandboxChannelResponse' smart constructor.
data DeleteAPNSVoipSandboxChannelResponse = DeleteAPNSVoipSandboxChannelResponse'
  { responseStatus ::
      Lude.Int,
    apnsVoipSandboxChannelResponse ::
      APNSVoipSandboxChannelResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAPNSVoipSandboxChannelResponse' with the minimum fields required to make a request.
--
-- * 'apnsVoipSandboxChannelResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteAPNSVoipSandboxChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'apnsVoipSandboxChannelResponse'
  APNSVoipSandboxChannelResponse ->
  DeleteAPNSVoipSandboxChannelResponse
mkDeleteAPNSVoipSandboxChannelResponse
  pResponseStatus_
  pAPNSVoipSandboxChannelResponse_ =
    DeleteAPNSVoipSandboxChannelResponse'
      { responseStatus =
          pResponseStatus_,
        apnsVoipSandboxChannelResponse =
          pAPNSVoipSandboxChannelResponse_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davscrsResponseStatus :: Lens.Lens' DeleteAPNSVoipSandboxChannelResponse Lude.Int
davscrsResponseStatus = Lens.lens (responseStatus :: DeleteAPNSVoipSandboxChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAPNSVoipSandboxChannelResponse)
{-# DEPRECATED davscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsVoipSandboxChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davscrsAPNSVoipSandboxChannelResponse :: Lens.Lens' DeleteAPNSVoipSandboxChannelResponse APNSVoipSandboxChannelResponse
davscrsAPNSVoipSandboxChannelResponse = Lens.lens (apnsVoipSandboxChannelResponse :: DeleteAPNSVoipSandboxChannelResponse -> APNSVoipSandboxChannelResponse) (\s a -> s {apnsVoipSandboxChannelResponse = a} :: DeleteAPNSVoipSandboxChannelResponse)
{-# DEPRECATED davscrsAPNSVoipSandboxChannelResponse "Use generic-lens or generic-optics with 'apnsVoipSandboxChannelResponse' instead." #-}
