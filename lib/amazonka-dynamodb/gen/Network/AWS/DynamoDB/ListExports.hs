{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.ListExports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists completed exports within the past 90 days.
module Network.AWS.DynamoDB.ListExports
  ( -- * Creating a request
    ListExports (..),
    mkListExports,

    -- ** Request lenses
    leMaxResults,
    leNextToken,
    leTableArn,

    -- * Destructuring the response
    ListExportsResponse (..),
    mkListExportsResponse,

    -- ** Response lenses
    lerrsExportSummaries,
    lerrsNextToken,
    lerrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListExports' smart constructor.
data ListExports = ListExports'
  { -- | Maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | An optional string that, if supplied, must be copied from the output of a previous call to @ListExports@ . When provided in this manner, the API fetches the next page of results.
    nextToken :: Core.Maybe Types.ExportNextToken,
    -- | The Amazon Resource Name (ARN) associated with the exported table.
    tableArn :: Core.Maybe Types.TableArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListExports' value with any optional fields omitted.
mkListExports ::
  ListExports
mkListExports =
  ListExports'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      tableArn = Core.Nothing
    }

-- | Maximum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMaxResults :: Lens.Lens' ListExports (Core.Maybe Core.Natural)
leMaxResults = Lens.field @"maxResults"
{-# DEPRECATED leMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An optional string that, if supplied, must be copied from the output of a previous call to @ListExports@ . When provided in this manner, the API fetches the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leNextToken :: Lens.Lens' ListExports (Core.Maybe Types.ExportNextToken)
leNextToken = Lens.field @"nextToken"
{-# DEPRECATED leNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Amazon Resource Name (ARN) associated with the exported table.
--
-- /Note:/ Consider using 'tableArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leTableArn :: Lens.Lens' ListExports (Core.Maybe Types.TableArn)
leTableArn = Lens.field @"tableArn"
{-# DEPRECATED leTableArn "Use generic-lens or generic-optics with 'tableArn' instead." #-}

instance Core.FromJSON ListExports where
  toJSON ListExports {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("TableArn" Core..=) Core.<$> tableArn
          ]
      )

instance Core.AWSRequest ListExports where
  type Rs ListExports = ListExportsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDB_20120810.ListExports")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExportsResponse'
            Core.<$> (x Core..:? "ExportSummaries")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListExportsResponse' smart constructor.
data ListExportsResponse = ListExportsResponse'
  { -- | A list of @ExportSummary@ objects.
    exportSummaries :: Core.Maybe [Types.ExportSummary],
    -- | If this value is returned, there are additional results to be displayed. To retrieve them, call @ListExports@ again, with @NextToken@ set to this value.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListExportsResponse' value with any optional fields omitted.
mkListExportsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListExportsResponse
mkListExportsResponse responseStatus =
  ListExportsResponse'
    { exportSummaries = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of @ExportSummary@ objects.
--
-- /Note:/ Consider using 'exportSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsExportSummaries :: Lens.Lens' ListExportsResponse (Core.Maybe [Types.ExportSummary])
lerrsExportSummaries = Lens.field @"exportSummaries"
{-# DEPRECATED lerrsExportSummaries "Use generic-lens or generic-optics with 'exportSummaries' instead." #-}

-- | If this value is returned, there are additional results to be displayed. To retrieve them, call @ListExports@ again, with @NextToken@ set to this value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsNextToken :: Lens.Lens' ListExportsResponse (Core.Maybe Types.NextToken)
lerrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lerrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsResponseStatus :: Lens.Lens' ListExportsResponse Core.Int
lerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
