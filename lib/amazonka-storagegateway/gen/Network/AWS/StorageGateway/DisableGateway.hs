{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DisableGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables a tape gateway when the gateway is no longer functioning. For example, if your gateway VM is damaged, you can disable the gateway so you can recover virtual tapes.
--
-- Use this operation for a tape gateway that is not reachable or not functioning. This operation is only supported in the tape gateway type.
-- /Important:/ After a gateway is disabled, it cannot be enabled.
module Network.AWS.StorageGateway.DisableGateway
  ( -- * Creating a request
    DisableGateway (..),
    mkDisableGateway,

    -- ** Request lenses
    dGatewayARN,

    -- * Destructuring the response
    DisableGatewayResponse (..),
    mkDisableGatewayResponse,

    -- ** Response lenses
    dgrsGatewayARN,
    dgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | DisableGatewayInput
--
-- /See:/ 'mkDisableGateway' smart constructor.
newtype DisableGateway = DisableGateway'
  { gatewayARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableGateway' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
mkDisableGateway ::
  -- | 'gatewayARN'
  Lude.Text ->
  DisableGateway
mkDisableGateway pGatewayARN_ =
  DisableGateway' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGatewayARN :: Lens.Lens' DisableGateway Lude.Text
dGatewayARN = Lens.lens (gatewayARN :: DisableGateway -> Lude.Text) (\s a -> s {gatewayARN = a} :: DisableGateway)
{-# DEPRECATED dGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest DisableGateway where
  type Rs DisableGateway = DisableGatewayResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DisableGatewayResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableGateway where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.DisableGateway" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableGateway where
  toJSON DisableGateway' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath DisableGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableGateway where
  toQuery = Lude.const Lude.mempty

-- | DisableGatewayOutput
--
-- /See:/ 'mkDisableGatewayResponse' smart constructor.
data DisableGatewayResponse = DisableGatewayResponse'
  { -- | The unique Amazon Resource Name (ARN) of the disabled gateway.
    gatewayARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableGatewayResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - The unique Amazon Resource Name (ARN) of the disabled gateway.
-- * 'responseStatus' - The response status code.
mkDisableGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableGatewayResponse
mkDisableGatewayResponse pResponseStatus_ =
  DisableGatewayResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique Amazon Resource Name (ARN) of the disabled gateway.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrsGatewayARN :: Lens.Lens' DisableGatewayResponse (Lude.Maybe Lude.Text)
dgrsGatewayARN = Lens.lens (gatewayARN :: DisableGatewayResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: DisableGatewayResponse)
{-# DEPRECATED dgrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrsResponseStatus :: Lens.Lens' DisableGatewayResponse Lude.Int
dgrsResponseStatus = Lens.lens (responseStatus :: DisableGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableGatewayResponse)
{-# DEPRECATED dgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
