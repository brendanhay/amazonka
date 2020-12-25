{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.CreateReportGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a report group. A report group contains a collection of reports.
module Network.AWS.CodeBuild.CreateReportGroup
  ( -- * Creating a request
    CreateReportGroup (..),
    mkCreateReportGroup,

    -- ** Request lenses
    crgName,
    crgType,
    crgExportConfig,
    crgTags,

    -- * Destructuring the response
    CreateReportGroupResponse (..),
    mkCreateReportGroupResponse,

    -- ** Response lenses
    crgrrsReportGroup,
    crgrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateReportGroup' smart constructor.
data CreateReportGroup = CreateReportGroup'
  { -- | The name of the report group.
    name :: Types.Name,
    -- | The type of report group.
    type' :: Types.ReportType,
    -- | A @ReportExportConfig@ object that contains information about where the report group test results are exported.
    exportConfig :: Types.ReportExportConfig,
    -- | A list of tag key and value pairs associated with this report group.
    --
    -- These tags are available for use by AWS services that support AWS CodeBuild report group tags.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReportGroup' value with any optional fields omitted.
mkCreateReportGroup ::
  -- | 'name'
  Types.Name ->
  -- | 'type\''
  Types.ReportType ->
  -- | 'exportConfig'
  Types.ReportExportConfig ->
  CreateReportGroup
mkCreateReportGroup name type' exportConfig =
  CreateReportGroup'
    { name,
      type',
      exportConfig,
      tags = Core.Nothing
    }

-- | The name of the report group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgName :: Lens.Lens' CreateReportGroup Types.Name
crgName = Lens.field @"name"
{-# DEPRECATED crgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of report group.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgType :: Lens.Lens' CreateReportGroup Types.ReportType
crgType = Lens.field @"type'"
{-# DEPRECATED crgType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A @ReportExportConfig@ object that contains information about where the report group test results are exported.
--
-- /Note:/ Consider using 'exportConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgExportConfig :: Lens.Lens' CreateReportGroup Types.ReportExportConfig
crgExportConfig = Lens.field @"exportConfig"
{-# DEPRECATED crgExportConfig "Use generic-lens or generic-optics with 'exportConfig' instead." #-}

-- | A list of tag key and value pairs associated with this report group.
--
-- These tags are available for use by AWS services that support AWS CodeBuild report group tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgTags :: Lens.Lens' CreateReportGroup (Core.Maybe [Types.Tag])
crgTags = Lens.field @"tags"
{-# DEPRECATED crgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateReportGroup where
  toJSON CreateReportGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("type" Core..= type'),
            Core.Just ("exportConfig" Core..= exportConfig),
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateReportGroup where
  type Rs CreateReportGroup = CreateReportGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeBuild_20161006.CreateReportGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateReportGroupResponse'
            Core.<$> (x Core..:? "reportGroup") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateReportGroupResponse' smart constructor.
data CreateReportGroupResponse = CreateReportGroupResponse'
  { -- | Information about the report group that was created.
    reportGroup :: Core.Maybe Types.ReportGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateReportGroupResponse' value with any optional fields omitted.
mkCreateReportGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateReportGroupResponse
mkCreateReportGroupResponse responseStatus =
  CreateReportGroupResponse'
    { reportGroup = Core.Nothing,
      responseStatus
    }

-- | Information about the report group that was created.
--
-- /Note:/ Consider using 'reportGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrrsReportGroup :: Lens.Lens' CreateReportGroupResponse (Core.Maybe Types.ReportGroup)
crgrrsReportGroup = Lens.field @"reportGroup"
{-# DEPRECATED crgrrsReportGroup "Use generic-lens or generic-optics with 'reportGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrrsResponseStatus :: Lens.Lens' CreateReportGroupResponse Core.Int
crgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
