{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the gateway virtual machine (VM) software. The request immediately triggers the software update.
--
-- /Important:/ A software update forces a system restart of your gateway. You can minimize the chance of any disruption to your applications by increasing your iSCSI Initiators' timeouts. For more information about increasing iSCSI Initiator timeouts for Windows and Linux, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/ConfiguringiSCSIClientInitiatorWindowsClient.html#CustomizeWindowsiSCSISettings Customizing your Windows iSCSI settings> and <https://docs.aws.amazon.com/storagegateway/latest/userguide/ConfiguringiSCSIClientInitiatorRedHatClient.html#CustomizeLinuxiSCSISettings Customizing your Linux iSCSI settings> , respectively.
module Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
  ( -- * Creating a request
    UpdateGatewaySoftwareNow (..),
    mkUpdateGatewaySoftwareNow,

    -- ** Request lenses
    ugsnGatewayARN,

    -- * Destructuring the response
    UpdateGatewaySoftwareNowResponse (..),
    mkUpdateGatewaySoftwareNowResponse,

    -- ** Response lenses
    ugsnrsGatewayARN,
    ugsnrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway to update.
--
-- /See:/ 'mkUpdateGatewaySoftwareNow' smart constructor.
newtype UpdateGatewaySoftwareNow = UpdateGatewaySoftwareNow'
  { gatewayARN ::
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

-- | Creates a value of 'UpdateGatewaySoftwareNow' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
mkUpdateGatewaySoftwareNow ::
  -- | 'gatewayARN'
  Lude.Text ->
  UpdateGatewaySoftwareNow
mkUpdateGatewaySoftwareNow pGatewayARN_ =
  UpdateGatewaySoftwareNow' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsnGatewayARN :: Lens.Lens' UpdateGatewaySoftwareNow Lude.Text
ugsnGatewayARN = Lens.lens (gatewayARN :: UpdateGatewaySoftwareNow -> Lude.Text) (\s a -> s {gatewayARN = a} :: UpdateGatewaySoftwareNow)
{-# DEPRECATED ugsnGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest UpdateGatewaySoftwareNow where
  type Rs UpdateGatewaySoftwareNow = UpdateGatewaySoftwareNowResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateGatewaySoftwareNowResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGatewaySoftwareNow where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.UpdateGatewaySoftwareNow" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateGatewaySoftwareNow where
  toJSON UpdateGatewaySoftwareNow' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath UpdateGatewaySoftwareNow where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateGatewaySoftwareNow where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway that was updated.
--
-- /See:/ 'mkUpdateGatewaySoftwareNowResponse' smart constructor.
data UpdateGatewaySoftwareNowResponse = UpdateGatewaySoftwareNowResponse'
  { gatewayARN ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGatewaySoftwareNowResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateGatewaySoftwareNowResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGatewaySoftwareNowResponse
mkUpdateGatewaySoftwareNowResponse pResponseStatus_ =
  UpdateGatewaySoftwareNowResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsnrsGatewayARN :: Lens.Lens' UpdateGatewaySoftwareNowResponse (Lude.Maybe Lude.Text)
ugsnrsGatewayARN = Lens.lens (gatewayARN :: UpdateGatewaySoftwareNowResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: UpdateGatewaySoftwareNowResponse)
{-# DEPRECATED ugsnrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsnrsResponseStatus :: Lens.Lens' UpdateGatewaySoftwareNowResponse Lude.Int
ugsnrsResponseStatus = Lens.lens (responseStatus :: UpdateGatewaySoftwareNowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGatewaySoftwareNowResponse)
{-# DEPRECATED ugsnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
