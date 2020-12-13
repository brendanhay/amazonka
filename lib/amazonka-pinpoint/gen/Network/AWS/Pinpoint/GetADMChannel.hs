{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetADMChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the ADM channel for an application.
module Network.AWS.Pinpoint.GetADMChannel
  ( -- * Creating a request
    GetADMChannel (..),
    mkGetADMChannel,

    -- ** Request lenses
    gadmcApplicationId,

    -- * Destructuring the response
    GetADMChannelResponse (..),
    mkGetADMChannelResponse,

    -- ** Response lenses
    gadmcrsADMChannelResponse,
    gadmcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetADMChannel' smart constructor.
newtype GetADMChannel = GetADMChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetADMChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkGetADMChannel ::
  -- | 'applicationId'
  Lude.Text ->
  GetADMChannel
mkGetADMChannel pApplicationId_ =
  GetADMChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadmcApplicationId :: Lens.Lens' GetADMChannel Lude.Text
gadmcApplicationId = Lens.lens (applicationId :: GetADMChannel -> Lude.Text) (\s a -> s {applicationId = a} :: GetADMChannel)
{-# DEPRECATED gadmcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetADMChannel where
  type Rs GetADMChannel = GetADMChannelResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetADMChannelResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetADMChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetADMChannel where
  toPath GetADMChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/adm"]

instance Lude.ToQuery GetADMChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetADMChannelResponse' smart constructor.
data GetADMChannelResponse = GetADMChannelResponse'
  { aDMChannelResponse :: ADMChannelResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetADMChannelResponse' with the minimum fields required to make a request.
--
-- * 'aDMChannelResponse' -
-- * 'responseStatus' - The response status code.
mkGetADMChannelResponse ::
  -- | 'aDMChannelResponse'
  ADMChannelResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetADMChannelResponse
mkGetADMChannelResponse pADMChannelResponse_ pResponseStatus_ =
  GetADMChannelResponse'
    { aDMChannelResponse = pADMChannelResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'aDMChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadmcrsADMChannelResponse :: Lens.Lens' GetADMChannelResponse ADMChannelResponse
gadmcrsADMChannelResponse = Lens.lens (aDMChannelResponse :: GetADMChannelResponse -> ADMChannelResponse) (\s a -> s {aDMChannelResponse = a} :: GetADMChannelResponse)
{-# DEPRECATED gadmcrsADMChannelResponse "Use generic-lens or generic-optics with 'aDMChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadmcrsResponseStatus :: Lens.Lens' GetADMChannelResponse Lude.Int
gadmcrsResponseStatus = Lens.lens (responseStatus :: GetADMChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetADMChannelResponse)
{-# DEPRECATED gadmcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
