{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetConnectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the connectors registered with the AWS SMS.
--
-- This operation returns paginated results.
module Network.AWS.SMS.GetConnectors
  ( -- * Creating a request
    GetConnectors (..),
    mkGetConnectors,

    -- ** Request lenses
    gcMaxResults,
    gcNextToken,

    -- * Destructuring the response
    GetConnectorsResponse (..),
    mkGetConnectorsResponse,

    -- ** Response lenses
    gcrrsConnectorList,
    gcrrsNextToken,
    gcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkGetConnectors' smart constructor.
data GetConnectors = GetConnectors'
  { -- | The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
    maxResults :: Core.Maybe Core.Int,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConnectors' value with any optional fields omitted.
mkGetConnectors ::
  GetConnectors
mkGetConnectors =
  GetConnectors'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcMaxResults :: Lens.Lens' GetConnectors (Core.Maybe Core.Int)
gcMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcNextToken :: Lens.Lens' GetConnectors (Core.Maybe Types.NextToken)
gcNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetConnectors where
  toJSON GetConnectors {..} =
    Core.object
      ( Core.catMaybes
          [ ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetConnectors where
  type Rs GetConnectors = GetConnectorsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.GetConnectors"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectorsResponse'
            Core.<$> (x Core..:? "connectorList")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetConnectors where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"connectorList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetConnectorsResponse' smart constructor.
data GetConnectorsResponse = GetConnectorsResponse'
  { -- | Information about the registered connectors.
    connectorList :: Core.Maybe [Types.Connector],
    -- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetConnectorsResponse' value with any optional fields omitted.
mkGetConnectorsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetConnectorsResponse
mkGetConnectorsResponse responseStatus =
  GetConnectorsResponse'
    { connectorList = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the registered connectors.
--
-- /Note:/ Consider using 'connectorList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsConnectorList :: Lens.Lens' GetConnectorsResponse (Core.Maybe [Types.Connector])
gcrrsConnectorList = Lens.field @"connectorList"
{-# DEPRECATED gcrrsConnectorList "Use generic-lens or generic-optics with 'connectorList' instead." #-}

-- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsNextToken :: Lens.Lens' GetConnectorsResponse (Core.Maybe Types.NextToken)
gcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetConnectorsResponse Core.Int
gcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
