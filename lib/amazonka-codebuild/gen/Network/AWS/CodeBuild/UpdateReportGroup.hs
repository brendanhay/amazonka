{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateReportGroup (..),
    mkUpdateReportGroup,

    -- ** Request lenses
    urgArn,
    urgExportConfig,
    urgTags,

    -- * Destructuring the response
    UpdateReportGroupResponse (..),
    mkUpdateReportGroupResponse,

    -- ** Response lenses
    urgrrsReportGroup,
    urgrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateReportGroup' smart constructor.
data UpdateReportGroup = UpdateReportGroup'
  { -- | The ARN of the report group to update.
    arn :: Types.NonEmptyString,
    -- | Used to specify an updated export type. Valid values are:
    --
    --
    --     * @S3@ : The report results are exported to an S3 bucket.
    --
    --
    --     * @NO_EXPORT@ : The report results are not exported.
    exportConfig :: Core.Maybe Types.ReportExportConfig,
    -- | An updated list of tag key and value pairs associated with this report group.
    --
    -- These tags are available for use by AWS services that support AWS CodeBuild report group tags.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateReportGroup' value with any optional fields omitted.
mkUpdateReportGroup ::
  -- | 'arn'
  Types.NonEmptyString ->
  UpdateReportGroup
mkUpdateReportGroup arn =
  UpdateReportGroup'
    { arn,
      exportConfig = Core.Nothing,
      tags = Core.Nothing
    }

-- | The ARN of the report group to update.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgArn :: Lens.Lens' UpdateReportGroup Types.NonEmptyString
urgArn = Lens.field @"arn"
{-# DEPRECATED urgArn "Use generic-lens or generic-optics with 'arn' instead." #-}

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
{-# DEPRECATED urgExportConfig "Use generic-lens or generic-optics with 'exportConfig' instead." #-}

-- | An updated list of tag key and value pairs associated with this report group.
--
-- These tags are available for use by AWS services that support AWS CodeBuild report group tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgTags :: Lens.Lens' UpdateReportGroup (Core.Maybe [Types.Tag])
urgTags = Lens.field @"tags"
{-# DEPRECATED urgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON UpdateReportGroup where
  toJSON UpdateReportGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("arn" Core..= arn),
            ("exportConfig" Core..=) Core.<$> exportConfig,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest UpdateReportGroup where
  type Rs UpdateReportGroup = UpdateReportGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeBuild_20161006.UpdateReportGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateReportGroupResponse'
            Core.<$> (x Core..:? "reportGroup") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateReportGroupResponse' smart constructor.
data UpdateReportGroupResponse = UpdateReportGroupResponse'
  { -- | Information about the updated report group.
    reportGroup :: Core.Maybe Types.ReportGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateReportGroupResponse' value with any optional fields omitted.
mkUpdateReportGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateReportGroupResponse
mkUpdateReportGroupResponse responseStatus =
  UpdateReportGroupResponse'
    { reportGroup = Core.Nothing,
      responseStatus
    }

-- | Information about the updated report group.
--
-- /Note:/ Consider using 'reportGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgrrsReportGroup :: Lens.Lens' UpdateReportGroupResponse (Core.Maybe Types.ReportGroup)
urgrrsReportGroup = Lens.field @"reportGroup"
{-# DEPRECATED urgrrsReportGroup "Use generic-lens or generic-optics with 'reportGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgrrsResponseStatus :: Lens.Lens' UpdateReportGroupResponse Core.Int
urgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
