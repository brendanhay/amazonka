{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelBundleTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a bundling operation for an instance store-backed Windows instance.
module Network.AWS.EC2.CancelBundleTask
    (
    -- * Creating a request
      CancelBundleTask (..)
    , mkCancelBundleTask
    -- ** Request lenses
    , cbtBundleId
    , cbtDryRun

    -- * Destructuring the response
    , CancelBundleTaskResponse (..)
    , mkCancelBundleTaskResponse
    -- ** Response lenses
    , cbtrrsBundleTask
    , cbtrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CancelBundleTask.
--
-- /See:/ 'mkCancelBundleTask' smart constructor.
data CancelBundleTask = CancelBundleTask'
  { bundleId :: Types.BundleId
    -- ^ The ID of the bundle task.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelBundleTask' value with any optional fields omitted.
mkCancelBundleTask
    :: Types.BundleId -- ^ 'bundleId'
    -> CancelBundleTask
mkCancelBundleTask bundleId
  = CancelBundleTask'{bundleId, dryRun = Core.Nothing}

-- | The ID of the bundle task.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtBundleId :: Lens.Lens' CancelBundleTask Types.BundleId
cbtBundleId = Lens.field @"bundleId"
{-# INLINEABLE cbtBundleId #-}
{-# DEPRECATED bundleId "Use generic-lens or generic-optics with 'bundleId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtDryRun :: Lens.Lens' CancelBundleTask (Core.Maybe Core.Bool)
cbtDryRun = Lens.field @"dryRun"
{-# INLINEABLE cbtDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery CancelBundleTask where
        toQuery CancelBundleTask{..}
          = Core.toQueryPair "Action" ("CancelBundleTask" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "BundleId" bundleId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders CancelBundleTask where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CancelBundleTask where
        type Rs CancelBundleTask = CancelBundleTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CancelBundleTaskResponse' Core.<$>
                   (x Core..@? "bundleInstanceTask") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of CancelBundleTask.
--
-- /See:/ 'mkCancelBundleTaskResponse' smart constructor.
data CancelBundleTaskResponse = CancelBundleTaskResponse'
  { bundleTask :: Core.Maybe Types.BundleTask
    -- ^ Information about the bundle task.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CancelBundleTaskResponse' value with any optional fields omitted.
mkCancelBundleTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelBundleTaskResponse
mkCancelBundleTaskResponse responseStatus
  = CancelBundleTaskResponse'{bundleTask = Core.Nothing,
                              responseStatus}

-- | Information about the bundle task.
--
-- /Note:/ Consider using 'bundleTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtrrsBundleTask :: Lens.Lens' CancelBundleTaskResponse (Core.Maybe Types.BundleTask)
cbtrrsBundleTask = Lens.field @"bundleTask"
{-# INLINEABLE cbtrrsBundleTask #-}
{-# DEPRECATED bundleTask "Use generic-lens or generic-optics with 'bundleTask' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtrrsResponseStatus :: Lens.Lens' CancelBundleTaskResponse Core.Int
cbtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cbtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
