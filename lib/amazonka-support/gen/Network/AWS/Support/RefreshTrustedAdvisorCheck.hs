{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.RefreshTrustedAdvisorCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Refreshes the AWS Trusted Advisor check that you specify using the check ID. You can get the check IDs by calling the 'DescribeTrustedAdvisorChecks' operation.
--
-- The response contains a 'TrustedAdvisorCheckRefreshStatus' object.
module Network.AWS.Support.RefreshTrustedAdvisorCheck
    (
    -- * Creating a request
      RefreshTrustedAdvisorCheck (..)
    , mkRefreshTrustedAdvisorCheck
    -- ** Request lenses
    , rtacCheckId

    -- * Destructuring the response
    , RefreshTrustedAdvisorCheckResponse (..)
    , mkRefreshTrustedAdvisorCheckResponse
    -- ** Response lenses
    , rtacrrsStatus
    , rtacrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Support.Types as Types

-- | 
--
-- /See:/ 'mkRefreshTrustedAdvisorCheck' smart constructor.
newtype RefreshTrustedAdvisorCheck = RefreshTrustedAdvisorCheck'
  { checkId :: Core.Text
    -- ^ The unique identifier for the Trusted Advisor check to refresh. __Note:__ Specifying the check ID of a check that is automatically refreshed causes an @InvalidParameterValue@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RefreshTrustedAdvisorCheck' value with any optional fields omitted.
mkRefreshTrustedAdvisorCheck
    :: Core.Text -- ^ 'checkId'
    -> RefreshTrustedAdvisorCheck
mkRefreshTrustedAdvisorCheck checkId
  = RefreshTrustedAdvisorCheck'{checkId}

-- | The unique identifier for the Trusted Advisor check to refresh. __Note:__ Specifying the check ID of a check that is automatically refreshed causes an @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'checkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacCheckId :: Lens.Lens' RefreshTrustedAdvisorCheck Core.Text
rtacCheckId = Lens.field @"checkId"
{-# INLINEABLE rtacCheckId #-}
{-# DEPRECATED checkId "Use generic-lens or generic-optics with 'checkId' instead"  #-}

instance Core.ToQuery RefreshTrustedAdvisorCheck where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RefreshTrustedAdvisorCheck where
        toHeaders RefreshTrustedAdvisorCheck{..}
          = Core.pure
              ("X-Amz-Target", "AWSSupport_20130415.RefreshTrustedAdvisorCheck")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RefreshTrustedAdvisorCheck where
        toJSON RefreshTrustedAdvisorCheck{..}
          = Core.object
              (Core.catMaybes [Core.Just ("checkId" Core..= checkId)])

instance Core.AWSRequest RefreshTrustedAdvisorCheck where
        type Rs RefreshTrustedAdvisorCheck =
             RefreshTrustedAdvisorCheckResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RefreshTrustedAdvisorCheckResponse' Core.<$>
                   (x Core..: "status") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The current refresh status of a Trusted Advisor check.
--
-- /See:/ 'mkRefreshTrustedAdvisorCheckResponse' smart constructor.
data RefreshTrustedAdvisorCheckResponse = RefreshTrustedAdvisorCheckResponse'
  { status :: Types.TrustedAdvisorCheckRefreshStatus
    -- ^ The current refresh status for a check, including the amount of time until the check is eligible for refresh.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RefreshTrustedAdvisorCheckResponse' value with any optional fields omitted.
mkRefreshTrustedAdvisorCheckResponse
    :: Types.TrustedAdvisorCheckRefreshStatus -- ^ 'status'
    -> Core.Int -- ^ 'responseStatus'
    -> RefreshTrustedAdvisorCheckResponse
mkRefreshTrustedAdvisorCheckResponse status responseStatus
  = RefreshTrustedAdvisorCheckResponse'{status, responseStatus}

-- | The current refresh status for a check, including the amount of time until the check is eligible for refresh.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacrrsStatus :: Lens.Lens' RefreshTrustedAdvisorCheckResponse Types.TrustedAdvisorCheckRefreshStatus
rtacrrsStatus = Lens.field @"status"
{-# INLINEABLE rtacrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacrrsResponseStatus :: Lens.Lens' RefreshTrustedAdvisorCheckResponse Core.Int
rtacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
