{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.CreateReturnShippingLabel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a shipping label that will be used to return the Snow device to AWS.
module Network.AWS.Snowball.CreateReturnShippingLabel
    (
    -- * Creating a request
      CreateReturnShippingLabel (..)
    , mkCreateReturnShippingLabel
    -- ** Request lenses
    , crslJobId
    , crslShippingOption

    -- * Destructuring the response
    , CreateReturnShippingLabelResponse (..)
    , mkCreateReturnShippingLabelResponse
    -- ** Response lenses
    , crslrrsStatus
    , crslrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkCreateReturnShippingLabel' smart constructor.
data CreateReturnShippingLabel = CreateReturnShippingLabel'
  { jobId :: Types.JobId
    -- ^ The ID for a job that you want to create the return shipping label for. For example @JID123e4567-e89b-12d3-a456-426655440000@ .
  , shippingOption :: Core.Maybe Types.ShippingOption
    -- ^ The shipping speed for a particular job. This speed doesn't dictate how soon the device is returned to AWS. This speed represents how quickly it moves to its destination while in transit. Regional shipping speeds are as follows:
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReturnShippingLabel' value with any optional fields omitted.
mkCreateReturnShippingLabel
    :: Types.JobId -- ^ 'jobId'
    -> CreateReturnShippingLabel
mkCreateReturnShippingLabel jobId
  = CreateReturnShippingLabel'{jobId, shippingOption = Core.Nothing}

-- | The ID for a job that you want to create the return shipping label for. For example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crslJobId :: Lens.Lens' CreateReturnShippingLabel Types.JobId
crslJobId = Lens.field @"jobId"
{-# INLINEABLE crslJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The shipping speed for a particular job. This speed doesn't dictate how soon the device is returned to AWS. This speed represents how quickly it moves to its destination while in transit. Regional shipping speeds are as follows:
--
-- /Note:/ Consider using 'shippingOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crslShippingOption :: Lens.Lens' CreateReturnShippingLabel (Core.Maybe Types.ShippingOption)
crslShippingOption = Lens.field @"shippingOption"
{-# INLINEABLE crslShippingOption #-}
{-# DEPRECATED shippingOption "Use generic-lens or generic-optics with 'shippingOption' instead"  #-}

instance Core.ToQuery CreateReturnShippingLabel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateReturnShippingLabel where
        toHeaders CreateReturnShippingLabel{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSIESnowballJobManagementService.CreateReturnShippingLabel")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateReturnShippingLabel where
        toJSON CreateReturnShippingLabel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("JobId" Core..= jobId),
                  ("ShippingOption" Core..=) Core.<$> shippingOption])

instance Core.AWSRequest CreateReturnShippingLabel where
        type Rs CreateReturnShippingLabel =
             CreateReturnShippingLabelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateReturnShippingLabelResponse' Core.<$>
                   (x Core..:? "Status") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateReturnShippingLabelResponse' smart constructor.
data CreateReturnShippingLabelResponse = CreateReturnShippingLabelResponse'
  { status :: Core.Maybe Types.ShippingLabelStatus
    -- ^ The status information of the task on a Snow device that is being returned to AWS.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReturnShippingLabelResponse' value with any optional fields omitted.
mkCreateReturnShippingLabelResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateReturnShippingLabelResponse
mkCreateReturnShippingLabelResponse responseStatus
  = CreateReturnShippingLabelResponse'{status = Core.Nothing,
                                       responseStatus}

-- | The status information of the task on a Snow device that is being returned to AWS.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crslrrsStatus :: Lens.Lens' CreateReturnShippingLabelResponse (Core.Maybe Types.ShippingLabelStatus)
crslrrsStatus = Lens.field @"status"
{-# INLINEABLE crslrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crslrrsResponseStatus :: Lens.Lens' CreateReturnShippingLabelResponse Core.Int
crslrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crslrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
