{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.UpdateReportGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a report group. 
module Network.AWS.CodeBuild.UpdateReportGroup
    (
    -- * Creating a request
      UpdateReportGroup (..)
    , mkUpdateReportGroup
    -- ** Request lenses
    , urgArn
    , urgExportConfig
    , urgTags

    -- * Destructuring the response
    , UpdateReportGroupResponse (..)
    , mkUpdateReportGroupResponse
    -- ** Response lenses
    , urgrrsReportGroup
    , urgrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateReportGroup' smart constructor.
data UpdateReportGroup = UpdateReportGroup'
  { arn :: Types.NonEmptyString
    -- ^ The ARN of the report group to update. 
  , exportConfig :: Core.Maybe Types.ReportExportConfig
    -- ^ Used to specify an updated export type. Valid values are: 
--
--
--     * @S3@ : The report results are exported to an S3 bucket. 
--
--
--     * @NO_EXPORT@ : The report results are not exported. 
--
--
  , tags :: Core.Maybe [Types.Tag]
    -- ^ An updated list of tag key and value pairs associated with this report group. 
--
-- These tags are available for use by AWS services that support AWS CodeBuild report group tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateReportGroup' value with any optional fields omitted.
mkUpdateReportGroup
    :: Types.NonEmptyString -- ^ 'arn'
    -> UpdateReportGroup
mkUpdateReportGroup arn
  = UpdateReportGroup'{arn, exportConfig = Core.Nothing,
                       tags = Core.Nothing}

-- | The ARN of the report group to update. 
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgArn :: Lens.Lens' UpdateReportGroup Types.NonEmptyString
urgArn = Lens.field @"arn"
{-# INLINEABLE urgArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | Used to specify an updated export type. Valid values are: 
--
--
--     * @S3@ : The report results are exported to an S3 bucket. 
--
--
--     * @NO_EXPORT@ : The report results are not exported. 
--
--
--
-- /Note:/ Consider using 'exportConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgExportConfig :: Lens.Lens' UpdateReportGroup (Core.Maybe Types.ReportExportConfig)
urgExportConfig = Lens.field @"exportConfig"
{-# INLINEABLE urgExportConfig #-}
{-# DEPRECATED exportConfig "Use generic-lens or generic-optics with 'exportConfig' instead"  #-}

-- | An updated list of tag key and value pairs associated with this report group. 
--
-- These tags are available for use by AWS services that support AWS CodeBuild report group tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgTags :: Lens.Lens' UpdateReportGroup (Core.Maybe [Types.Tag])
urgTags = Lens.field @"tags"
{-# INLINEABLE urgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery UpdateReportGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateReportGroup where
        toHeaders UpdateReportGroup{..}
          = Core.pure
              ("X-Amz-Target", "CodeBuild_20161006.UpdateReportGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateReportGroup where
        toJSON UpdateReportGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("arn" Core..= arn),
                  ("exportConfig" Core..=) Core.<$> exportConfig,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest UpdateReportGroup where
        type Rs UpdateReportGroup = UpdateReportGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateReportGroupResponse' Core.<$>
                   (x Core..:? "reportGroup") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateReportGroupResponse' smart constructor.
data UpdateReportGroupResponse = UpdateReportGroupResponse'
  { reportGroup :: Core.Maybe Types.ReportGroup
    -- ^ Information about the updated report group. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateReportGroupResponse' value with any optional fields omitted.
mkUpdateReportGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateReportGroupResponse
mkUpdateReportGroupResponse responseStatus
  = UpdateReportGroupResponse'{reportGroup = Core.Nothing,
                               responseStatus}

-- | Information about the updated report group. 
--
-- /Note:/ Consider using 'reportGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgrrsReportGroup :: Lens.Lens' UpdateReportGroupResponse (Core.Maybe Types.ReportGroup)
urgrrsReportGroup = Lens.field @"reportGroup"
{-# INLINEABLE urgrrsReportGroup #-}
{-# DEPRECATED reportGroup "Use generic-lens or generic-optics with 'reportGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgrrsResponseStatus :: Lens.Lens' UpdateReportGroupResponse Core.Int
urgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE urgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
