{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.StartAvailabilityMonitorTest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start a test that verifies that the specified gateway is configured for High Availability monitoring in your host environment. This request only initiates the test and that a successful response only indicates that the test was started. It doesn't indicate that the test passed. For the status of the test, invoke the @DescribeAvailabilityMonitorTest@ API.
module Network.AWS.StorageGateway.StartAvailabilityMonitorTest
    (
    -- * Creating a request
      StartAvailabilityMonitorTest (..)
    , mkStartAvailabilityMonitorTest
    -- ** Request lenses
    , samtGatewayARN

    -- * Destructuring the response
    , StartAvailabilityMonitorTestResponse (..)
    , mkStartAvailabilityMonitorTestResponse
    -- ** Response lenses
    , samtrrsGatewayARN
    , samtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkStartAvailabilityMonitorTest' smart constructor.
newtype StartAvailabilityMonitorTest = StartAvailabilityMonitorTest'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartAvailabilityMonitorTest' value with any optional fields omitted.
mkStartAvailabilityMonitorTest
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> StartAvailabilityMonitorTest
mkStartAvailabilityMonitorTest gatewayARN
  = StartAvailabilityMonitorTest'{gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samtGatewayARN :: Lens.Lens' StartAvailabilityMonitorTest Types.GatewayARN
samtGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE samtGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

instance Core.ToQuery StartAvailabilityMonitorTest where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartAvailabilityMonitorTest where
        toHeaders StartAvailabilityMonitorTest{..}
          = Core.pure
              ("X-Amz-Target",
               "StorageGateway_20130630.StartAvailabilityMonitorTest")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartAvailabilityMonitorTest where
        toJSON StartAvailabilityMonitorTest{..}
          = Core.object
              (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest StartAvailabilityMonitorTest where
        type Rs StartAvailabilityMonitorTest =
             StartAvailabilityMonitorTestResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartAvailabilityMonitorTestResponse' Core.<$>
                   (x Core..:? "GatewayARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartAvailabilityMonitorTestResponse' smart constructor.
data StartAvailabilityMonitorTestResponse = StartAvailabilityMonitorTestResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartAvailabilityMonitorTestResponse' value with any optional fields omitted.
mkStartAvailabilityMonitorTestResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartAvailabilityMonitorTestResponse
mkStartAvailabilityMonitorTestResponse responseStatus
  = StartAvailabilityMonitorTestResponse'{gatewayARN = Core.Nothing,
                                          responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samtrrsGatewayARN :: Lens.Lens' StartAvailabilityMonitorTestResponse (Core.Maybe Types.GatewayARN)
samtrrsGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE samtrrsGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samtrrsResponseStatus :: Lens.Lens' StartAvailabilityMonitorTestResponse Core.Int
samtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE samtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
