{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.DeleteReportGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a report group. Before you delete a report group, you must delete its reports. 
module Network.AWS.CodeBuild.DeleteReportGroup
    (
    -- * Creating a request
      DeleteReportGroup (..)
    , mkDeleteReportGroup
    -- ** Request lenses
    , drgArn
    , drgDeleteReports

    -- * Destructuring the response
    , DeleteReportGroupResponse (..)
    , mkDeleteReportGroupResponse
    -- ** Response lenses
    , drgrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteReportGroup' smart constructor.
data DeleteReportGroup = DeleteReportGroup'
  { arn :: Types.NonEmptyString
    -- ^ The ARN of the report group to delete. 
  , deleteReports :: Core.Maybe Core.Bool
    -- ^ If @true@ , deletes any reports that belong to a report group before deleting the report group. 
--
-- If @false@ , you must delete any reports in the report group. Use <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_ListReportsForReportGroup.html ListReportsForReportGroup> to get the reports in a report group. Use <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_DeleteReport.html DeleteReport> to delete the reports. If you call @DeleteReportGroup@ for a report group that contains one or more reports, an exception is thrown. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReportGroup' value with any optional fields omitted.
mkDeleteReportGroup
    :: Types.NonEmptyString -- ^ 'arn'
    -> DeleteReportGroup
mkDeleteReportGroup arn
  = DeleteReportGroup'{arn, deleteReports = Core.Nothing}

-- | The ARN of the report group to delete. 
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgArn :: Lens.Lens' DeleteReportGroup Types.NonEmptyString
drgArn = Lens.field @"arn"
{-# INLINEABLE drgArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | If @true@ , deletes any reports that belong to a report group before deleting the report group. 
--
-- If @false@ , you must delete any reports in the report group. Use <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_ListReportsForReportGroup.html ListReportsForReportGroup> to get the reports in a report group. Use <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_DeleteReport.html DeleteReport> to delete the reports. If you call @DeleteReportGroup@ for a report group that contains one or more reports, an exception is thrown. 
--
-- /Note:/ Consider using 'deleteReports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgDeleteReports :: Lens.Lens' DeleteReportGroup (Core.Maybe Core.Bool)
drgDeleteReports = Lens.field @"deleteReports"
{-# INLINEABLE drgDeleteReports #-}
{-# DEPRECATED deleteReports "Use generic-lens or generic-optics with 'deleteReports' instead"  #-}

instance Core.ToQuery DeleteReportGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteReportGroup where
        toHeaders DeleteReportGroup{..}
          = Core.pure
              ("X-Amz-Target", "CodeBuild_20161006.DeleteReportGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteReportGroup where
        toJSON DeleteReportGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("arn" Core..= arn),
                  ("deleteReports" Core..=) Core.<$> deleteReports])

instance Core.AWSRequest DeleteReportGroup where
        type Rs DeleteReportGroup = DeleteReportGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteReportGroupResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteReportGroupResponse' smart constructor.
newtype DeleteReportGroupResponse = DeleteReportGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReportGroupResponse' value with any optional fields omitted.
mkDeleteReportGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteReportGroupResponse
mkDeleteReportGroupResponse responseStatus
  = DeleteReportGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrrsResponseStatus :: Lens.Lens' DeleteReportGroupResponse Core.Int
drgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
