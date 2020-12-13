{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ShutdownGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shuts down a gateway. To specify which gateway to shut down, use the Amazon Resource Name (ARN) of the gateway in the body of your request.
--
-- The operation shuts down the gateway service component running in the gateway's virtual machine (VM) and not the host VM.
-- After the gateway is shutdown, you cannot call any other API except 'StartGateway' , 'DescribeGatewayInformation' , and 'ListGateways' . For more information, see 'ActivateGateway' . Your applications cannot read from or write to the gateway's storage volumes, and there are no snapshots taken.
-- If do not intend to use the gateway again, you must delete the gateway (using 'DeleteGateway' ) to no longer pay software charges associated with the gateway.
module Network.AWS.StorageGateway.ShutdownGateway
  ( -- * Creating a request
    ShutdownGateway (..),
    mkShutdownGateway,

    -- ** Request lenses
    sGatewayARN,

    -- * Destructuring the response
    ShutdownGatewayResponse (..),
    mkShutdownGatewayResponse,

    -- ** Response lenses
    srsGatewayARN,
    srsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway to shut down.
--
-- /See:/ 'mkShutdownGateway' smart constructor.
newtype ShutdownGateway = ShutdownGateway'
  { gatewayARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ShutdownGateway' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
mkShutdownGateway ::
  -- | 'gatewayARN'
  Lude.Text ->
  ShutdownGateway
mkShutdownGateway pGatewayARN_ =
  ShutdownGateway' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sGatewayARN :: Lens.Lens' ShutdownGateway Lude.Text
sGatewayARN = Lens.lens (gatewayARN :: ShutdownGateway -> Lude.Text) (\s a -> s {gatewayARN = a} :: ShutdownGateway)
{-# DEPRECATED sGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest ShutdownGateway where
  type Rs ShutdownGateway = ShutdownGatewayResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          ShutdownGatewayResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ShutdownGateway where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.ShutdownGateway" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ShutdownGateway where
  toJSON ShutdownGateway' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath ShutdownGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery ShutdownGateway where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway that was shut down.
--
-- /See:/ 'mkShutdownGatewayResponse' smart constructor.
data ShutdownGatewayResponse = ShutdownGatewayResponse'
  { gatewayARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ShutdownGatewayResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'responseStatus' - The response status code.
mkShutdownGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ShutdownGatewayResponse
mkShutdownGatewayResponse pResponseStatus_ =
  ShutdownGatewayResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsGatewayARN :: Lens.Lens' ShutdownGatewayResponse (Lude.Maybe Lude.Text)
srsGatewayARN = Lens.lens (gatewayARN :: ShutdownGatewayResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: ShutdownGatewayResponse)
{-# DEPRECATED srsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' ShutdownGatewayResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: ShutdownGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ShutdownGatewayResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
