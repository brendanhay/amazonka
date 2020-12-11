{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateAPNSChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the APNs channel for an application or updates the status and settings of the APNs channel for an application.
module Network.AWS.Pinpoint.UpdateAPNSChannel
  ( -- * Creating a request
    UpdateAPNSChannel (..),
    mkUpdateAPNSChannel,

    -- ** Request lenses
    uacApplicationId,
    uacAPNSChannelRequest,

    -- * Destructuring the response
    UpdateAPNSChannelResponse (..),
    mkUpdateAPNSChannelResponse,

    -- ** Response lenses
    uacrsResponseStatus,
    uacrsAPNSChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAPNSChannel' smart constructor.
data UpdateAPNSChannel = UpdateAPNSChannel'
  { applicationId ::
      Lude.Text,
    apnsChannelRequest :: APNSChannelRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAPNSChannel' with the minimum fields required to make a request.
--
-- * 'apnsChannelRequest' - Undocumented field.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkUpdateAPNSChannel ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'apnsChannelRequest'
  APNSChannelRequest ->
  UpdateAPNSChannel
mkUpdateAPNSChannel pApplicationId_ pAPNSChannelRequest_ =
  UpdateAPNSChannel'
    { applicationId = pApplicationId_,
      apnsChannelRequest = pAPNSChannelRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacApplicationId :: Lens.Lens' UpdateAPNSChannel Lude.Text
uacApplicationId = Lens.lens (applicationId :: UpdateAPNSChannel -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateAPNSChannel)
{-# DEPRECATED uacApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacAPNSChannelRequest :: Lens.Lens' UpdateAPNSChannel APNSChannelRequest
uacAPNSChannelRequest = Lens.lens (apnsChannelRequest :: UpdateAPNSChannel -> APNSChannelRequest) (\s a -> s {apnsChannelRequest = a} :: UpdateAPNSChannel)
{-# DEPRECATED uacAPNSChannelRequest "Use generic-lens or generic-optics with 'apnsChannelRequest' instead." #-}

instance Lude.AWSRequest UpdateAPNSChannel where
  type Rs UpdateAPNSChannel = UpdateAPNSChannelResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateAPNSChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders UpdateAPNSChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateAPNSChannel where
  toJSON UpdateAPNSChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("APNSChannelRequest" Lude..= apnsChannelRequest)]
      )

instance Lude.ToPath UpdateAPNSChannel where
  toPath UpdateAPNSChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/apns"]

instance Lude.ToQuery UpdateAPNSChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAPNSChannelResponse' smart constructor.
data UpdateAPNSChannelResponse = UpdateAPNSChannelResponse'
  { responseStatus ::
      Lude.Int,
    apnsChannelResponse ::
      APNSChannelResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAPNSChannelResponse' with the minimum fields required to make a request.
--
-- * 'apnsChannelResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateAPNSChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'apnsChannelResponse'
  APNSChannelResponse ->
  UpdateAPNSChannelResponse
mkUpdateAPNSChannelResponse pResponseStatus_ pAPNSChannelResponse_ =
  UpdateAPNSChannelResponse'
    { responseStatus = pResponseStatus_,
      apnsChannelResponse = pAPNSChannelResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacrsResponseStatus :: Lens.Lens' UpdateAPNSChannelResponse Lude.Int
uacrsResponseStatus = Lens.lens (responseStatus :: UpdateAPNSChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAPNSChannelResponse)
{-# DEPRECATED uacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacrsAPNSChannelResponse :: Lens.Lens' UpdateAPNSChannelResponse APNSChannelResponse
uacrsAPNSChannelResponse = Lens.lens (apnsChannelResponse :: UpdateAPNSChannelResponse -> APNSChannelResponse) (\s a -> s {apnsChannelResponse = a} :: UpdateAPNSChannelResponse)
{-# DEPRECATED uacrsAPNSChannelResponse "Use generic-lens or generic-optics with 'apnsChannelResponse' instead." #-}
