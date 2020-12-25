{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateGatewayInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a gateway's metadata, which includes the gateway's name and time zone. To specify which gateway to update, use the Amazon Resource Name (ARN) of the gateway in your request.
module Network.AWS.StorageGateway.UpdateGatewayInformation
  ( -- * Creating a request
    UpdateGatewayInformation (..),
    mkUpdateGatewayInformation,

    -- ** Request lenses
    ugiGatewayARN,
    ugiCloudWatchLogGroupARN,
    ugiGatewayName,
    ugiGatewayTimezone,

    -- * Destructuring the response
    UpdateGatewayInformationResponse (..),
    mkUpdateGatewayInformationResponse,

    -- ** Response lenses
    ugirrsGatewayARN,
    ugirrsGatewayName,
    ugirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkUpdateGatewayInformation' smart constructor.
data UpdateGatewayInformation = UpdateGatewayInformation'
  { gatewayARN :: Types.GatewayARN,
    -- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that you want to use to monitor and log events in the gateway.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/WhatIsCloudWatchLogs.html What is Amazon CloudWatch Logs?>
    cloudWatchLogGroupARN :: Core.Maybe Types.CloudWatchLogGroupARN,
    gatewayName :: Core.Maybe Types.GatewayName,
    -- | A value that indicates the time zone of the gateway.
    gatewayTimezone :: Core.Maybe Types.GatewayTimezone
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGatewayInformation' value with any optional fields omitted.
mkUpdateGatewayInformation ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  UpdateGatewayInformation
mkUpdateGatewayInformation gatewayARN =
  UpdateGatewayInformation'
    { gatewayARN,
      cloudWatchLogGroupARN = Core.Nothing,
      gatewayName = Core.Nothing,
      gatewayTimezone = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugiGatewayARN :: Lens.Lens' UpdateGatewayInformation Types.GatewayARN
ugiGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED ugiGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that you want to use to monitor and log events in the gateway.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/WhatIsCloudWatchLogs.html What is Amazon CloudWatch Logs?>
--
-- /Note:/ Consider using 'cloudWatchLogGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugiCloudWatchLogGroupARN :: Lens.Lens' UpdateGatewayInformation (Core.Maybe Types.CloudWatchLogGroupARN)
ugiCloudWatchLogGroupARN = Lens.field @"cloudWatchLogGroupARN"
{-# DEPRECATED ugiCloudWatchLogGroupARN "Use generic-lens or generic-optics with 'cloudWatchLogGroupARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugiGatewayName :: Lens.Lens' UpdateGatewayInformation (Core.Maybe Types.GatewayName)
ugiGatewayName = Lens.field @"gatewayName"
{-# DEPRECATED ugiGatewayName "Use generic-lens or generic-optics with 'gatewayName' instead." #-}

-- | A value that indicates the time zone of the gateway.
--
-- /Note:/ Consider using 'gatewayTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugiGatewayTimezone :: Lens.Lens' UpdateGatewayInformation (Core.Maybe Types.GatewayTimezone)
ugiGatewayTimezone = Lens.field @"gatewayTimezone"
{-# DEPRECATED ugiGatewayTimezone "Use generic-lens or generic-optics with 'gatewayTimezone' instead." #-}

instance Core.FromJSON UpdateGatewayInformation where
  toJSON UpdateGatewayInformation {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            ("CloudWatchLogGroupARN" Core..=) Core.<$> cloudWatchLogGroupARN,
            ("GatewayName" Core..=) Core.<$> gatewayName,
            ("GatewayTimezone" Core..=) Core.<$> gatewayTimezone
          ]
      )

instance Core.AWSRequest UpdateGatewayInformation where
  type Rs UpdateGatewayInformation = UpdateGatewayInformationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StorageGateway_20130630.UpdateGatewayInformation"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGatewayInformationResponse'
            Core.<$> (x Core..:? "GatewayARN")
            Core.<*> (x Core..:? "GatewayName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway that was updated.
--
-- /See:/ 'mkUpdateGatewayInformationResponse' smart constructor.
data UpdateGatewayInformationResponse = UpdateGatewayInformationResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The name you configured for your gateway.
    gatewayName :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGatewayInformationResponse' value with any optional fields omitted.
mkUpdateGatewayInformationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateGatewayInformationResponse
mkUpdateGatewayInformationResponse responseStatus =
  UpdateGatewayInformationResponse'
    { gatewayARN = Core.Nothing,
      gatewayName = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugirrsGatewayARN :: Lens.Lens' UpdateGatewayInformationResponse (Core.Maybe Types.GatewayARN)
ugirrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED ugirrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The name you configured for your gateway.
--
-- /Note:/ Consider using 'gatewayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugirrsGatewayName :: Lens.Lens' UpdateGatewayInformationResponse (Core.Maybe Types.String)
ugirrsGatewayName = Lens.field @"gatewayName"
{-# DEPRECATED ugirrsGatewayName "Use generic-lens or generic-optics with 'gatewayName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugirrsResponseStatus :: Lens.Lens' UpdateGatewayInformationResponse Core.Int
ugirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ugirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
