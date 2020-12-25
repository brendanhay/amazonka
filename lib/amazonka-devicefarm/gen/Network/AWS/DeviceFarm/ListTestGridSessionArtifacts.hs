{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListTestGridSessionArtifacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of artifacts created during the session.
module Network.AWS.DeviceFarm.ListTestGridSessionArtifacts
  ( -- * Creating a request
    ListTestGridSessionArtifacts (..),
    mkListTestGridSessionArtifacts,

    -- ** Request lenses
    lSessionArn,
    lMaxResult,
    lNextToken,
    lType,

    -- * Destructuring the response
    ListTestGridSessionArtifactsResponse (..),
    mkListTestGridSessionArtifactsResponse,

    -- ** Response lenses
    lrsArtifacts,
    lrsNextToken,
    lrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTestGridSessionArtifacts' smart constructor.
data ListTestGridSessionArtifacts = ListTestGridSessionArtifacts'
  { -- | The ARN of a 'TestGridSession' .
    sessionArn :: Types.DeviceFarmArn,
    -- | The maximum number of results to be returned by a request.
    maxResult :: Core.Maybe Core.Natural,
    -- | Pagination token.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | Limit results to a specified type of artifact.
    type' :: Core.Maybe Types.TestGridSessionArtifactCategory
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTestGridSessionArtifacts' value with any optional fields omitted.
mkListTestGridSessionArtifacts ::
  -- | 'sessionArn'
  Types.DeviceFarmArn ->
  ListTestGridSessionArtifacts
mkListTestGridSessionArtifacts sessionArn =
  ListTestGridSessionArtifacts'
    { sessionArn,
      maxResult = Core.Nothing,
      nextToken = Core.Nothing,
      type' = Core.Nothing
    }

-- | The ARN of a 'TestGridSession' .
--
-- /Note:/ Consider using 'sessionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lSessionArn :: Lens.Lens' ListTestGridSessionArtifacts Types.DeviceFarmArn
lSessionArn = Lens.field @"sessionArn"
{-# DEPRECATED lSessionArn "Use generic-lens or generic-optics with 'sessionArn' instead." #-}

-- | The maximum number of results to be returned by a request.
--
-- /Note:/ Consider using 'maxResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResult :: Lens.Lens' ListTestGridSessionArtifacts (Core.Maybe Core.Natural)
lMaxResult = Lens.field @"maxResult"
{-# DEPRECATED lMaxResult "Use generic-lens or generic-optics with 'maxResult' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListTestGridSessionArtifacts (Core.Maybe Types.PaginationToken)
lNextToken = Lens.field @"nextToken"
{-# DEPRECATED lNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Limit results to a specified type of artifact.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lType :: Lens.Lens' ListTestGridSessionArtifacts (Core.Maybe Types.TestGridSessionArtifactCategory)
lType = Lens.field @"type'"
{-# DEPRECATED lType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON ListTestGridSessionArtifacts where
  toJSON ListTestGridSessionArtifacts {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("sessionArn" Core..= sessionArn),
            ("maxResult" Core..=) Core.<$> maxResult,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("type" Core..=) Core.<$> type'
          ]
      )

instance Core.AWSRequest ListTestGridSessionArtifacts where
  type
    Rs ListTestGridSessionArtifacts =
      ListTestGridSessionArtifactsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "DeviceFarm_20150623.ListTestGridSessionArtifacts"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTestGridSessionArtifactsResponse'
            Core.<$> (x Core..:? "artifacts")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListTestGridSessionArtifactsResponse' smart constructor.
data ListTestGridSessionArtifactsResponse = ListTestGridSessionArtifactsResponse'
  { -- | A list of test grid session artifacts for a 'TestGridSession' .
    artifacts :: Core.Maybe [Types.TestGridSessionArtifact],
    -- | Pagination token.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTestGridSessionArtifactsResponse' value with any optional fields omitted.
mkListTestGridSessionArtifactsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTestGridSessionArtifactsResponse
mkListTestGridSessionArtifactsResponse responseStatus =
  ListTestGridSessionArtifactsResponse'
    { artifacts = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of test grid session artifacts for a 'TestGridSession' .
--
-- /Note:/ Consider using 'artifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsArtifacts :: Lens.Lens' ListTestGridSessionArtifactsResponse (Core.Maybe [Types.TestGridSessionArtifact])
lrsArtifacts = Lens.field @"artifacts"
{-# DEPRECATED lrsArtifacts "Use generic-lens or generic-optics with 'artifacts' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListTestGridSessionArtifactsResponse (Core.Maybe Types.PaginationToken)
lrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListTestGridSessionArtifactsResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
