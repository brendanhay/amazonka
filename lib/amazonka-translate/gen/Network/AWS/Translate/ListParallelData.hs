{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.ListParallelData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of your parallel data resources in Amazon Translate.
module Network.AWS.Translate.ListParallelData
  ( -- * Creating a request
    ListParallelData (..),
    mkListParallelData,

    -- ** Request lenses
    lpdMaxResults,
    lpdNextToken,

    -- * Destructuring the response
    ListParallelDataResponse (..),
    mkListParallelDataResponse,

    -- ** Response lenses
    lpdrrsNextToken,
    lpdrrsParallelDataPropertiesList,
    lpdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Translate.Types as Types

-- | /See:/ 'mkListParallelData' smart constructor.
data ListParallelData = ListParallelData'
  { -- | The maximum number of parallel data resources returned for each request.
    maxResults :: Core.Maybe Core.Natural,
    -- | A string that specifies the next page of results to return in a paginated response.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListParallelData' value with any optional fields omitted.
mkListParallelData ::
  ListParallelData
mkListParallelData =
  ListParallelData'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of parallel data resources returned for each request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdMaxResults :: Lens.Lens' ListParallelData (Core.Maybe Core.Natural)
lpdMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lpdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A string that specifies the next page of results to return in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdNextToken :: Lens.Lens' ListParallelData (Core.Maybe Types.NextToken)
lpdNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListParallelData where
  toJSON ListParallelData {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListParallelData where
  type Rs ListParallelData = ListParallelDataResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSShineFrontendService_20170701.ListParallelData"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListParallelDataResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "ParallelDataPropertiesList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListParallelDataResponse' smart constructor.
data ListParallelDataResponse = ListParallelDataResponse'
  { -- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The properties of the parallel data resources returned by this request.
    parallelDataPropertiesList :: Core.Maybe [Types.ParallelDataProperties],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListParallelDataResponse' value with any optional fields omitted.
mkListParallelDataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListParallelDataResponse
mkListParallelDataResponse responseStatus =
  ListParallelDataResponse'
    { nextToken = Core.Nothing,
      parallelDataPropertiesList = Core.Nothing,
      responseStatus
    }

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdrrsNextToken :: Lens.Lens' ListParallelDataResponse (Core.Maybe Types.NextToken)
lpdrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpdrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The properties of the parallel data resources returned by this request.
--
-- /Note:/ Consider using 'parallelDataPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdrrsParallelDataPropertiesList :: Lens.Lens' ListParallelDataResponse (Core.Maybe [Types.ParallelDataProperties])
lpdrrsParallelDataPropertiesList = Lens.field @"parallelDataPropertiesList"
{-# DEPRECATED lpdrrsParallelDataPropertiesList "Use generic-lens or generic-optics with 'parallelDataPropertiesList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdrrsResponseStatus :: Lens.Lens' ListParallelDataResponse Core.Int
lpdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lpdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
