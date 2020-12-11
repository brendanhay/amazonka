{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteAPNSSandboxChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the APNs sandbox channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteAPNSSandboxChannel
  ( -- * Creating a request
    DeleteAPNSSandboxChannel (..),
    mkDeleteAPNSSandboxChannel,

    -- ** Request lenses
    dascApplicationId,

    -- * Destructuring the response
    DeleteAPNSSandboxChannelResponse (..),
    mkDeleteAPNSSandboxChannelResponse,

    -- ** Response lenses
    dascrsResponseStatus,
    dascrsAPNSSandboxChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAPNSSandboxChannel' smart constructor.
newtype DeleteAPNSSandboxChannel = DeleteAPNSSandboxChannel'
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

-- | Creates a value of 'DeleteAPNSSandboxChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkDeleteAPNSSandboxChannel ::
  -- | 'applicationId'
  Lude.Text ->
  DeleteAPNSSandboxChannel
mkDeleteAPNSSandboxChannel pApplicationId_ =
  DeleteAPNSSandboxChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dascApplicationId :: Lens.Lens' DeleteAPNSSandboxChannel Lude.Text
dascApplicationId = Lens.lens (applicationId :: DeleteAPNSSandboxChannel -> Lude.Text) (\s a -> s {applicationId = a} :: DeleteAPNSSandboxChannel)
{-# DEPRECATED dascApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest DeleteAPNSSandboxChannel where
  type Rs DeleteAPNSSandboxChannel = DeleteAPNSSandboxChannelResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteAPNSSandboxChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders DeleteAPNSSandboxChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteAPNSSandboxChannel where
  toPath DeleteAPNSSandboxChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/apns_sandbox"]

instance Lude.ToQuery DeleteAPNSSandboxChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAPNSSandboxChannelResponse' smart constructor.
data DeleteAPNSSandboxChannelResponse = DeleteAPNSSandboxChannelResponse'
  { responseStatus ::
      Lude.Int,
    apnsSandboxChannelResponse ::
      APNSSandboxChannelResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAPNSSandboxChannelResponse' with the minimum fields required to make a request.
--
-- * 'apnsSandboxChannelResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteAPNSSandboxChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'apnsSandboxChannelResponse'
  APNSSandboxChannelResponse ->
  DeleteAPNSSandboxChannelResponse
mkDeleteAPNSSandboxChannelResponse
  pResponseStatus_
  pAPNSSandboxChannelResponse_ =
    DeleteAPNSSandboxChannelResponse'
      { responseStatus =
          pResponseStatus_,
        apnsSandboxChannelResponse = pAPNSSandboxChannelResponse_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dascrsResponseStatus :: Lens.Lens' DeleteAPNSSandboxChannelResponse Lude.Int
dascrsResponseStatus = Lens.lens (responseStatus :: DeleteAPNSSandboxChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAPNSSandboxChannelResponse)
{-# DEPRECATED dascrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsSandboxChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dascrsAPNSSandboxChannelResponse :: Lens.Lens' DeleteAPNSSandboxChannelResponse APNSSandboxChannelResponse
dascrsAPNSSandboxChannelResponse = Lens.lens (apnsSandboxChannelResponse :: DeleteAPNSSandboxChannelResponse -> APNSSandboxChannelResponse) (\s a -> s {apnsSandboxChannelResponse = a} :: DeleteAPNSSandboxChannelResponse)
{-# DEPRECATED dascrsAPNSSandboxChannelResponse "Use generic-lens or generic-optics with 'apnsSandboxChannelResponse' instead." #-}
