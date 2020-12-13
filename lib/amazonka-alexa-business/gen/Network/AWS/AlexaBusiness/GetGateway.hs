{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a gateway.
module Network.AWS.AlexaBusiness.GetGateway
  ( -- * Creating a request
    GetGateway (..),
    mkGetGateway,

    -- ** Request lenses
    ggGatewayARN,

    -- * Destructuring the response
    GetGatewayResponse (..),
    mkGetGatewayResponse,

    -- ** Response lenses
    ggrsGateway,
    ggrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetGateway' smart constructor.
newtype GetGateway = GetGateway'
  { -- | The ARN of the gateway to get.
    gatewayARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGateway' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - The ARN of the gateway to get.
mkGetGateway ::
  -- | 'gatewayARN'
  Lude.Text ->
  GetGateway
mkGetGateway pGatewayARN_ = GetGateway' {gatewayARN = pGatewayARN_}

-- | The ARN of the gateway to get.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggGatewayARN :: Lens.Lens' GetGateway Lude.Text
ggGatewayARN = Lens.lens (gatewayARN :: GetGateway -> Lude.Text) (\s a -> s {gatewayARN = a} :: GetGateway)
{-# DEPRECATED ggGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest GetGateway where
  type Rs GetGateway = GetGatewayResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetGatewayResponse'
            Lude.<$> (x Lude..?> "Gateway") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGateway where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.GetGateway" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetGateway where
  toJSON GetGateway' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayArn" Lude..= gatewayARN)])

instance Lude.ToPath GetGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery GetGateway where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetGatewayResponse' smart constructor.
data GetGatewayResponse = GetGatewayResponse'
  { -- | The details of the gateway.
    gateway :: Lude.Maybe Gateway,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGatewayResponse' with the minimum fields required to make a request.
--
-- * 'gateway' - The details of the gateway.
-- * 'responseStatus' - The response status code.
mkGetGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetGatewayResponse
mkGetGatewayResponse pResponseStatus_ =
  GetGatewayResponse'
    { gateway = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The details of the gateway.
--
-- /Note:/ Consider using 'gateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsGateway :: Lens.Lens' GetGatewayResponse (Lude.Maybe Gateway)
ggrsGateway = Lens.lens (gateway :: GetGatewayResponse -> Lude.Maybe Gateway) (\s a -> s {gateway = a} :: GetGatewayResponse)
{-# DEPRECATED ggrsGateway "Use generic-lens or generic-optics with 'gateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsResponseStatus :: Lens.Lens' GetGatewayResponse Lude.Int
ggrsResponseStatus = Lens.lens (responseStatus :: GetGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGatewayResponse)
{-# DEPRECATED ggrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
