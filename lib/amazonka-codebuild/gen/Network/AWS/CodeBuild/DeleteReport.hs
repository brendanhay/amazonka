{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.DeleteReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a report. 
module Network.AWS.CodeBuild.DeleteReport
    (
    -- * Creating a request
      DeleteReport (..)
    , mkDeleteReport
    -- ** Request lenses
    , drArn

    -- * Destructuring the response
    , DeleteReportResponse (..)
    , mkDeleteReportResponse
    -- ** Response lenses
    , drrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteReport' smart constructor.
newtype DeleteReport = DeleteReport'
  { arn :: Types.Arn
    -- ^ The ARN of the report to delete. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReport' value with any optional fields omitted.
mkDeleteReport
    :: Types.Arn -- ^ 'arn'
    -> DeleteReport
mkDeleteReport arn = DeleteReport'{arn}

-- | The ARN of the report to delete. 
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drArn :: Lens.Lens' DeleteReport Types.Arn
drArn = Lens.field @"arn"
{-# INLINEABLE drArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery DeleteReport where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteReport where
        toHeaders DeleteReport{..}
          = Core.pure ("X-Amz-Target", "CodeBuild_20161006.DeleteReport")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteReport where
        toJSON DeleteReport{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest DeleteReport where
        type Rs DeleteReport = DeleteReportResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteReportResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteReportResponse' smart constructor.
newtype DeleteReportResponse = DeleteReportResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReportResponse' value with any optional fields omitted.
mkDeleteReportResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteReportResponse
mkDeleteReportResponse responseStatus
  = DeleteReportResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DeleteReportResponse Core.Int
drrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
