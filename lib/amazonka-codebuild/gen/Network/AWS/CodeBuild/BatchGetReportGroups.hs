{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.BatchGetReportGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of report groups.
module Network.AWS.CodeBuild.BatchGetReportGroups
  ( -- * Creating a request
    BatchGetReportGroups (..),
    mkBatchGetReportGroups,

    -- ** Request lenses
    bgrgReportGroupArns,

    -- * Destructuring the response
    BatchGetReportGroupsResponse (..),
    mkBatchGetReportGroupsResponse,

    -- ** Response lenses
    bgrgrrsReportGroups,
    bgrgrrsReportGroupsNotFound,
    bgrgrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetReportGroups' smart constructor.
newtype BatchGetReportGroups = BatchGetReportGroups'
  { -- | An array of report group ARNs that identify the report groups to return.
    reportGroupArns :: Core.NonEmpty Types.NonEmptyString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetReportGroups' value with any optional fields omitted.
mkBatchGetReportGroups ::
  -- | 'reportGroupArns'
  Core.NonEmpty Types.NonEmptyString ->
  BatchGetReportGroups
mkBatchGetReportGroups reportGroupArns =
  BatchGetReportGroups' {reportGroupArns}

-- | An array of report group ARNs that identify the report groups to return.
--
-- /Note:/ Consider using 'reportGroupArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrgReportGroupArns :: Lens.Lens' BatchGetReportGroups (Core.NonEmpty Types.NonEmptyString)
bgrgReportGroupArns = Lens.field @"reportGroupArns"
{-# DEPRECATED bgrgReportGroupArns "Use generic-lens or generic-optics with 'reportGroupArns' instead." #-}

instance Core.FromJSON BatchGetReportGroups where
  toJSON BatchGetReportGroups {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("reportGroupArns" Core..= reportGroupArns)]
      )

instance Core.AWSRequest BatchGetReportGroups where
  type Rs BatchGetReportGroups = BatchGetReportGroupsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeBuild_20161006.BatchGetReportGroups")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetReportGroupsResponse'
            Core.<$> (x Core..:? "reportGroups")
            Core.<*> (x Core..:? "reportGroupsNotFound")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchGetReportGroupsResponse' smart constructor.
data BatchGetReportGroupsResponse = BatchGetReportGroupsResponse'
  { -- | The array of report groups returned by @BatchGetReportGroups@ .
    reportGroups :: Core.Maybe (Core.NonEmpty Types.ReportGroup),
    -- | An array of ARNs passed to @BatchGetReportGroups@ that are not associated with a @ReportGroup@ .
    reportGroupsNotFound :: Core.Maybe (Core.NonEmpty Types.NonEmptyString),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchGetReportGroupsResponse' value with any optional fields omitted.
mkBatchGetReportGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchGetReportGroupsResponse
mkBatchGetReportGroupsResponse responseStatus =
  BatchGetReportGroupsResponse'
    { reportGroups = Core.Nothing,
      reportGroupsNotFound = Core.Nothing,
      responseStatus
    }

-- | The array of report groups returned by @BatchGetReportGroups@ .
--
-- /Note:/ Consider using 'reportGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrgrrsReportGroups :: Lens.Lens' BatchGetReportGroupsResponse (Core.Maybe (Core.NonEmpty Types.ReportGroup))
bgrgrrsReportGroups = Lens.field @"reportGroups"
{-# DEPRECATED bgrgrrsReportGroups "Use generic-lens or generic-optics with 'reportGroups' instead." #-}

-- | An array of ARNs passed to @BatchGetReportGroups@ that are not associated with a @ReportGroup@ .
--
-- /Note:/ Consider using 'reportGroupsNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrgrrsReportGroupsNotFound :: Lens.Lens' BatchGetReportGroupsResponse (Core.Maybe (Core.NonEmpty Types.NonEmptyString))
bgrgrrsReportGroupsNotFound = Lens.field @"reportGroupsNotFound"
{-# DEPRECATED bgrgrrsReportGroupsNotFound "Use generic-lens or generic-optics with 'reportGroupsNotFound' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrgrrsResponseStatus :: Lens.Lens' BatchGetReportGroupsResponse Core.Int
bgrgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bgrgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
