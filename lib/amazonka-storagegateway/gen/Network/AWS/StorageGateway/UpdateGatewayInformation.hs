{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateGatewayInformation (..)
    , mkUpdateGatewayInformation
    -- ** Request lenses
    , ugiGatewayARN
    , ugiCloudWatchLogGroupARN
    , ugiGatewayName
    , ugiGatewayTimezone

    -- * Destructuring the response
    , UpdateGatewayInformationResponse (..)
    , mkUpdateGatewayInformationResponse
    -- ** Response lenses
    , ugirrsGatewayARN
    , ugirrsGatewayName
    , ugirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkUpdateGatewayInformation' smart constructor.
data UpdateGatewayInformation = UpdateGatewayInformation'
  { gatewayARN :: Types.GatewayARN
  , cloudWatchLogGroupARN :: Core.Maybe Types.CloudWatchLogGroupARN
    -- ^ The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that you want to use to monitor and log events in the gateway.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/WhatIsCloudWatchLogs.html What is Amazon CloudWatch Logs?> 
  , gatewayName :: Core.Maybe Types.GatewayName
  , gatewayTimezone :: Core.Maybe Types.GatewayTimezone
    -- ^ A value that indicates the time zone of the gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGatewayInformation' value with any optional fields omitted.
mkUpdateGatewayInformation
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> UpdateGatewayInformation
mkUpdateGatewayInformation gatewayARN
  = UpdateGatewayInformation'{gatewayARN,
                              cloudWatchLogGroupARN = Core.Nothing, gatewayName = Core.Nothing,
                              gatewayTimezone = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugiGatewayARN :: Lens.Lens' UpdateGatewayInformation Types.GatewayARN
ugiGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE ugiGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that you want to use to monitor and log events in the gateway.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/WhatIsCloudWatchLogs.html What is Amazon CloudWatch Logs?> 
--
-- /Note:/ Consider using 'cloudWatchLogGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugiCloudWatchLogGroupARN :: Lens.Lens' UpdateGatewayInformation (Core.Maybe Types.CloudWatchLogGroupARN)
ugiCloudWatchLogGroupARN = Lens.field @"cloudWatchLogGroupARN"
{-# INLINEABLE ugiCloudWatchLogGroupARN #-}
{-# DEPRECATED cloudWatchLogGroupARN "Use generic-lens or generic-optics with 'cloudWatchLogGroupARN' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugiGatewayName :: Lens.Lens' UpdateGatewayInformation (Core.Maybe Types.GatewayName)
ugiGatewayName = Lens.field @"gatewayName"
{-# INLINEABLE ugiGatewayName #-}
{-# DEPRECATED gatewayName "Use generic-lens or generic-optics with 'gatewayName' instead"  #-}

-- | A value that indicates the time zone of the gateway.
--
-- /Note:/ Consider using 'gatewayTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugiGatewayTimezone :: Lens.Lens' UpdateGatewayInformation (Core.Maybe Types.GatewayTimezone)
ugiGatewayTimezone = Lens.field @"gatewayTimezone"
{-# INLINEABLE ugiGatewayTimezone #-}
{-# DEPRECATED gatewayTimezone "Use generic-lens or generic-optics with 'gatewayTimezone' instead"  #-}

instance Core.ToQuery UpdateGatewayInformation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateGatewayInformation where
        toHeaders UpdateGatewayInformation{..}
          = Core.pure
              ("X-Amz-Target",
               "StorageGateway_20130630.UpdateGatewayInformation")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateGatewayInformation where
        toJSON UpdateGatewayInformation{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GatewayARN" Core..= gatewayARN),
                  ("CloudWatchLogGroupARN" Core..=) Core.<$> cloudWatchLogGroupARN,
                  ("GatewayName" Core..=) Core.<$> gatewayName,
                  ("GatewayTimezone" Core..=) Core.<$> gatewayTimezone])

instance Core.AWSRequest UpdateGatewayInformation where
        type Rs UpdateGatewayInformation = UpdateGatewayInformationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateGatewayInformationResponse' Core.<$>
                   (x Core..:? "GatewayARN") Core.<*> x Core..:? "GatewayName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway that was updated.
--
-- /See:/ 'mkUpdateGatewayInformationResponse' smart constructor.
data UpdateGatewayInformationResponse = UpdateGatewayInformationResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN
  , gatewayName :: Core.Maybe Core.Text
    -- ^ The name you configured for your gateway.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGatewayInformationResponse' value with any optional fields omitted.
mkUpdateGatewayInformationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateGatewayInformationResponse
mkUpdateGatewayInformationResponse responseStatus
  = UpdateGatewayInformationResponse'{gatewayARN = Core.Nothing,
                                      gatewayName = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugirrsGatewayARN :: Lens.Lens' UpdateGatewayInformationResponse (Core.Maybe Types.GatewayARN)
ugirrsGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE ugirrsGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The name you configured for your gateway.
--
-- /Note:/ Consider using 'gatewayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugirrsGatewayName :: Lens.Lens' UpdateGatewayInformationResponse (Core.Maybe Core.Text)
ugirrsGatewayName = Lens.field @"gatewayName"
{-# INLINEABLE ugirrsGatewayName #-}
{-# DEPRECATED gatewayName "Use generic-lens or generic-optics with 'gatewayName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugirrsResponseStatus :: Lens.Lens' UpdateGatewayInformationResponse Core.Int
ugirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ugirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
