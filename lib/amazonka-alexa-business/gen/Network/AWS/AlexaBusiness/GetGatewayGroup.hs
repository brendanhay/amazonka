{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetGatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a gateway group.
module Network.AWS.AlexaBusiness.GetGatewayGroup
  ( -- * Creating a request
    GetGatewayGroup (..),
    mkGetGatewayGroup,

    -- ** Request lenses
    gggGatewayGroupARN,

    -- * Destructuring the response
    GetGatewayGroupResponse (..),
    mkGetGatewayGroupResponse,

    -- ** Response lenses
    gggrsGatewayGroup,
    gggrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetGatewayGroup' smart constructor.
newtype GetGatewayGroup = GetGatewayGroup'
  { -- | The ARN of the gateway group to get.
    gatewayGroupARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGatewayGroup' with the minimum fields required to make a request.
--
-- * 'gatewayGroupARN' - The ARN of the gateway group to get.
mkGetGatewayGroup ::
  -- | 'gatewayGroupARN'
  Lude.Text ->
  GetGatewayGroup
mkGetGatewayGroup pGatewayGroupARN_ =
  GetGatewayGroup' {gatewayGroupARN = pGatewayGroupARN_}

-- | The ARN of the gateway group to get.
--
-- /Note:/ Consider using 'gatewayGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gggGatewayGroupARN :: Lens.Lens' GetGatewayGroup Lude.Text
gggGatewayGroupARN = Lens.lens (gatewayGroupARN :: GetGatewayGroup -> Lude.Text) (\s a -> s {gatewayGroupARN = a} :: GetGatewayGroup)
{-# DEPRECATED gggGatewayGroupARN "Use generic-lens or generic-optics with 'gatewayGroupARN' instead." #-}

instance Lude.AWSRequest GetGatewayGroup where
  type Rs GetGatewayGroup = GetGatewayGroupResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetGatewayGroupResponse'
            Lude.<$> (x Lude..?> "GatewayGroup") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGatewayGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.GetGatewayGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetGatewayGroup where
  toJSON GetGatewayGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("GatewayGroupArn" Lude..= gatewayGroupARN)]
      )

instance Lude.ToPath GetGatewayGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery GetGatewayGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetGatewayGroupResponse' smart constructor.
data GetGatewayGroupResponse = GetGatewayGroupResponse'
  { gatewayGroup :: Lude.Maybe GatewayGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGatewayGroupResponse' with the minimum fields required to make a request.
--
-- * 'gatewayGroup' -
-- * 'responseStatus' - The response status code.
mkGetGatewayGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetGatewayGroupResponse
mkGetGatewayGroupResponse pResponseStatus_ =
  GetGatewayGroupResponse'
    { gatewayGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gggrsGatewayGroup :: Lens.Lens' GetGatewayGroupResponse (Lude.Maybe GatewayGroup)
gggrsGatewayGroup = Lens.lens (gatewayGroup :: GetGatewayGroupResponse -> Lude.Maybe GatewayGroup) (\s a -> s {gatewayGroup = a} :: GetGatewayGroupResponse)
{-# DEPRECATED gggrsGatewayGroup "Use generic-lens or generic-optics with 'gatewayGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gggrsResponseStatus :: Lens.Lens' GetGatewayGroupResponse Lude.Int
gggrsResponseStatus = Lens.lens (responseStatus :: GetGatewayGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGatewayGroupResponse)
{-# DEPRECATED gggrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
