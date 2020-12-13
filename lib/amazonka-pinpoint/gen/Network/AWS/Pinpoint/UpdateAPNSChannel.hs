{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    uacAPNSChannelRequest,
    uacApplicationId,

    -- * Destructuring the response
    UpdateAPNSChannelResponse (..),
    mkUpdateAPNSChannelResponse,

    -- ** Response lenses
    uacrsAPNSChannelResponse,
    uacrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAPNSChannel' smart constructor.
data UpdateAPNSChannel = UpdateAPNSChannel'
  { apnsChannelRequest :: APNSChannelRequest,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAPNSChannel' with the minimum fields required to make a request.
--
-- * 'apnsChannelRequest' -
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkUpdateAPNSChannel ::
  -- | 'apnsChannelRequest'
  APNSChannelRequest ->
  -- | 'applicationId'
  Lude.Text ->
  UpdateAPNSChannel
mkUpdateAPNSChannel pAPNSChannelRequest_ pApplicationId_ =
  UpdateAPNSChannel'
    { apnsChannelRequest = pAPNSChannelRequest_,
      applicationId = pApplicationId_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacAPNSChannelRequest :: Lens.Lens' UpdateAPNSChannel APNSChannelRequest
uacAPNSChannelRequest = Lens.lens (apnsChannelRequest :: UpdateAPNSChannel -> APNSChannelRequest) (\s a -> s {apnsChannelRequest = a} :: UpdateAPNSChannel)
{-# DEPRECATED uacAPNSChannelRequest "Use generic-lens or generic-optics with 'apnsChannelRequest' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacApplicationId :: Lens.Lens' UpdateAPNSChannel Lude.Text
uacApplicationId = Lens.lens (applicationId :: UpdateAPNSChannel -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateAPNSChannel)
{-# DEPRECATED uacApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest UpdateAPNSChannel where
  type Rs UpdateAPNSChannel = UpdateAPNSChannelResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateAPNSChannelResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
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
  { apnsChannelResponse :: APNSChannelResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAPNSChannelResponse' with the minimum fields required to make a request.
--
-- * 'apnsChannelResponse' -
-- * 'responseStatus' - The response status code.
mkUpdateAPNSChannelResponse ::
  -- | 'apnsChannelResponse'
  APNSChannelResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateAPNSChannelResponse
mkUpdateAPNSChannelResponse pAPNSChannelResponse_ pResponseStatus_ =
  UpdateAPNSChannelResponse'
    { apnsChannelResponse =
        pAPNSChannelResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacrsAPNSChannelResponse :: Lens.Lens' UpdateAPNSChannelResponse APNSChannelResponse
uacrsAPNSChannelResponse = Lens.lens (apnsChannelResponse :: UpdateAPNSChannelResponse -> APNSChannelResponse) (\s a -> s {apnsChannelResponse = a} :: UpdateAPNSChannelResponse)
{-# DEPRECATED uacrsAPNSChannelResponse "Use generic-lens or generic-optics with 'apnsChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacrsResponseStatus :: Lens.Lens' UpdateAPNSChannelResponse Lude.Int
uacrsResponseStatus = Lens.lens (responseStatus :: UpdateAPNSChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAPNSChannelResponse)
{-# DEPRECATED uacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
