{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.StartGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a gateway that you previously shut down (see 'ShutdownGateway' ). After the gateway starts, you can then make other API calls, your applications can read from or write to the gateway's storage volumes and you will be able to take snapshot backups.
--
-- To specify which gateway to start, use the Amazon Resource Name (ARN) of the gateway in your request.
module Network.AWS.StorageGateway.StartGateway
  ( -- * Creating a request
    StartGateway (..),
    mkStartGateway,

    -- ** Request lenses
    sgGatewayARN,

    -- * Destructuring the response
    StartGatewayResponse (..),
    mkStartGatewayResponse,

    -- ** Response lenses
    sgrsGatewayARN,
    sgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway to start.
--
-- /See:/ 'mkStartGateway' smart constructor.
newtype StartGateway = StartGateway'
  { gatewayARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartGateway' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
mkStartGateway ::
  -- | 'gatewayARN'
  Lude.Text ->
  StartGateway
mkStartGateway pGatewayARN_ =
  StartGateway' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgGatewayARN :: Lens.Lens' StartGateway Lude.Text
sgGatewayARN = Lens.lens (gatewayARN :: StartGateway -> Lude.Text) (\s a -> s {gatewayARN = a} :: StartGateway)
{-# DEPRECATED sgGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest StartGateway where
  type Rs StartGateway = StartGatewayResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartGatewayResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartGateway where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.StartGateway" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartGateway where
  toJSON StartGateway' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath StartGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery StartGateway where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway that was restarted.
--
-- /See:/ 'mkStartGatewayResponse' smart constructor.
data StartGatewayResponse = StartGatewayResponse'
  { gatewayARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartGatewayResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'responseStatus' - The response status code.
mkStartGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartGatewayResponse
mkStartGatewayResponse pResponseStatus_ =
  StartGatewayResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrsGatewayARN :: Lens.Lens' StartGatewayResponse (Lude.Maybe Lude.Text)
sgrsGatewayARN = Lens.lens (gatewayARN :: StartGatewayResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: StartGatewayResponse)
{-# DEPRECATED sgrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrsResponseStatus :: Lens.Lens' StartGatewayResponse Lude.Int
sgrsResponseStatus = Lens.lens (responseStatus :: StartGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartGatewayResponse)
{-# DEPRECATED sgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
