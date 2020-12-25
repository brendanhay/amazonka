{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListV2LoggingLevels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists logging levels.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListV2LoggingLevels
  ( -- * Creating a request
    ListV2LoggingLevels (..),
    mkListV2LoggingLevels,

    -- ** Request lenses
    lvllMaxResults,
    lvllNextToken,
    lvllTargetType,

    -- * Destructuring the response
    ListV2LoggingLevelsResponse (..),
    mkListV2LoggingLevelsResponse,

    -- ** Response lenses
    lvllrrsLogTargetConfigurations,
    lvllrrsNextToken,
    lvllrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListV2LoggingLevels' smart constructor.
data ListV2LoggingLevels = ListV2LoggingLevels'
  { -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The type of resource for which you are configuring logging. Must be @THING_Group@ .
    targetType :: Core.Maybe Types.LogTargetType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListV2LoggingLevels' value with any optional fields omitted.
mkListV2LoggingLevels ::
  ListV2LoggingLevels
mkListV2LoggingLevels =
  ListV2LoggingLevels'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      targetType = Core.Nothing
    }

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvllMaxResults :: Lens.Lens' ListV2LoggingLevels (Core.Maybe Core.Natural)
lvllMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lvllMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvllNextToken :: Lens.Lens' ListV2LoggingLevels (Core.Maybe Types.NextToken)
lvllNextToken = Lens.field @"nextToken"
{-# DEPRECATED lvllNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The type of resource for which you are configuring logging. Must be @THING_Group@ .
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvllTargetType :: Lens.Lens' ListV2LoggingLevels (Core.Maybe Types.LogTargetType)
lvllTargetType = Lens.field @"targetType"
{-# DEPRECATED lvllTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

instance Core.AWSRequest ListV2LoggingLevels where
  type Rs ListV2LoggingLevels = ListV2LoggingLevelsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/v2LoggingLevel",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "targetType" Core.<$> targetType),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListV2LoggingLevelsResponse'
            Core.<$> (x Core..:? "logTargetConfigurations")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListV2LoggingLevels where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"logTargetConfigurations" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListV2LoggingLevelsResponse' smart constructor.
data ListV2LoggingLevelsResponse = ListV2LoggingLevelsResponse'
  { -- | The logging configuration for a target.
    logTargetConfigurations :: Core.Maybe [Types.LogTargetConfiguration],
    -- | The token to use to get the next set of results, or __null__ if there are no additional results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListV2LoggingLevelsResponse' value with any optional fields omitted.
mkListV2LoggingLevelsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListV2LoggingLevelsResponse
mkListV2LoggingLevelsResponse responseStatus =
  ListV2LoggingLevelsResponse'
    { logTargetConfigurations =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The logging configuration for a target.
--
-- /Note:/ Consider using 'logTargetConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvllrrsLogTargetConfigurations :: Lens.Lens' ListV2LoggingLevelsResponse (Core.Maybe [Types.LogTargetConfiguration])
lvllrrsLogTargetConfigurations = Lens.field @"logTargetConfigurations"
{-# DEPRECATED lvllrrsLogTargetConfigurations "Use generic-lens or generic-optics with 'logTargetConfigurations' instead." #-}

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvllrrsNextToken :: Lens.Lens' ListV2LoggingLevelsResponse (Core.Maybe Types.NextToken)
lvllrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lvllrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvllrrsResponseStatus :: Lens.Lens' ListV2LoggingLevelsResponse Core.Int
lvllrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lvllrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
