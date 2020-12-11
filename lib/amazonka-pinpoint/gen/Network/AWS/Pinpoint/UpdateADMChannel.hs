{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateADMChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the ADM channel for an application or updates the status and settings of the ADM channel for an application.
module Network.AWS.Pinpoint.UpdateADMChannel
  ( -- * Creating a request
    UpdateADMChannel (..),
    mkUpdateADMChannel,

    -- ** Request lenses
    uadmcApplicationId,
    uadmcADMChannelRequest,

    -- * Destructuring the response
    UpdateADMChannelResponse (..),
    mkUpdateADMChannelResponse,

    -- ** Response lenses
    uadmcrsResponseStatus,
    uadmcrsADMChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateADMChannel' smart constructor.
data UpdateADMChannel = UpdateADMChannel'
  { applicationId ::
      Lude.Text,
    aDMChannelRequest :: ADMChannelRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateADMChannel' with the minimum fields required to make a request.
--
-- * 'aDMChannelRequest' - Undocumented field.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkUpdateADMChannel ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'aDMChannelRequest'
  ADMChannelRequest ->
  UpdateADMChannel
mkUpdateADMChannel pApplicationId_ pADMChannelRequest_ =
  UpdateADMChannel'
    { applicationId = pApplicationId_,
      aDMChannelRequest = pADMChannelRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uadmcApplicationId :: Lens.Lens' UpdateADMChannel Lude.Text
uadmcApplicationId = Lens.lens (applicationId :: UpdateADMChannel -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateADMChannel)
{-# DEPRECATED uadmcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aDMChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uadmcADMChannelRequest :: Lens.Lens' UpdateADMChannel ADMChannelRequest
uadmcADMChannelRequest = Lens.lens (aDMChannelRequest :: UpdateADMChannel -> ADMChannelRequest) (\s a -> s {aDMChannelRequest = a} :: UpdateADMChannel)
{-# DEPRECATED uadmcADMChannelRequest "Use generic-lens or generic-optics with 'aDMChannelRequest' instead." #-}

instance Lude.AWSRequest UpdateADMChannel where
  type Rs UpdateADMChannel = UpdateADMChannelResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateADMChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders UpdateADMChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateADMChannel where
  toJSON UpdateADMChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ADMChannelRequest" Lude..= aDMChannelRequest)]
      )

instance Lude.ToPath UpdateADMChannel where
  toPath UpdateADMChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/adm"]

instance Lude.ToQuery UpdateADMChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateADMChannelResponse' smart constructor.
data UpdateADMChannelResponse = UpdateADMChannelResponse'
  { responseStatus ::
      Lude.Int,
    aDMChannelResponse :: ADMChannelResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateADMChannelResponse' with the minimum fields required to make a request.
--
-- * 'aDMChannelResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateADMChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'aDMChannelResponse'
  ADMChannelResponse ->
  UpdateADMChannelResponse
mkUpdateADMChannelResponse pResponseStatus_ pADMChannelResponse_ =
  UpdateADMChannelResponse'
    { responseStatus = pResponseStatus_,
      aDMChannelResponse = pADMChannelResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uadmcrsResponseStatus :: Lens.Lens' UpdateADMChannelResponse Lude.Int
uadmcrsResponseStatus = Lens.lens (responseStatus :: UpdateADMChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateADMChannelResponse)
{-# DEPRECATED uadmcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aDMChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uadmcrsADMChannelResponse :: Lens.Lens' UpdateADMChannelResponse ADMChannelResponse
uadmcrsADMChannelResponse = Lens.lens (aDMChannelResponse :: UpdateADMChannelResponse -> ADMChannelResponse) (\s a -> s {aDMChannelResponse = a} :: UpdateADMChannelResponse)
{-# DEPRECATED uadmcrsADMChannelResponse "Use generic-lens or generic-optics with 'aDMChannelResponse' instead." #-}
