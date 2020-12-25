{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    ugsnrrsGatewayARN,
    ugsnrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway to update.
--
-- /See:/ 'mkUpdateGatewaySoftwareNow' smart constructor.
newtype UpdateGatewaySoftwareNow = UpdateGatewaySoftwareNow'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGatewaySoftwareNow' value with any optional fields omitted.
mkUpdateGatewaySoftwareNow ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  UpdateGatewaySoftwareNow
mkUpdateGatewaySoftwareNow gatewayARN =
  UpdateGatewaySoftwareNow' {gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsnGatewayARN :: Lens.Lens' UpdateGatewaySoftwareNow Types.GatewayARN
ugsnGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED ugsnGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Core.FromJSON UpdateGatewaySoftwareNow where
  toJSON UpdateGatewaySoftwareNow {..} =
    Core.object
      (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest UpdateGatewaySoftwareNow where
  type Rs UpdateGatewaySoftwareNow = UpdateGatewaySoftwareNowResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StorageGateway_20130630.UpdateGatewaySoftwareNow"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGatewaySoftwareNowResponse'
            Core.<$> (x Core..:? "GatewayARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway that was updated.
--
-- /See:/ 'mkUpdateGatewaySoftwareNowResponse' smart constructor.
data UpdateGatewaySoftwareNowResponse = UpdateGatewaySoftwareNowResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGatewaySoftwareNowResponse' value with any optional fields omitted.
mkUpdateGatewaySoftwareNowResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateGatewaySoftwareNowResponse
mkUpdateGatewaySoftwareNowResponse responseStatus =
  UpdateGatewaySoftwareNowResponse'
    { gatewayARN = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsnrrsGatewayARN :: Lens.Lens' UpdateGatewaySoftwareNowResponse (Core.Maybe Types.GatewayARN)
ugsnrrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED ugsnrrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsnrrsResponseStatus :: Lens.Lens' UpdateGatewaySoftwareNowResponse Core.Int
ugsnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ugsnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
