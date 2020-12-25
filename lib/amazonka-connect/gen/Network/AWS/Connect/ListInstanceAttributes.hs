{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListInstanceAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all attribute types for the given instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListInstanceAttributes
  ( -- * Creating a request
    ListInstanceAttributes (..),
    mkListInstanceAttributes,

    -- ** Request lenses
    liaInstanceId,
    liaMaxResults,
    liaNextToken,

    -- * Destructuring the response
    ListInstanceAttributesResponse (..),
    mkListInstanceAttributesResponse,

    -- ** Response lenses
    liarrsAttributes,
    liarrsNextToken,
    liarrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListInstanceAttributes' smart constructor.
data ListInstanceAttributes = ListInstanceAttributes'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The maximimum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInstanceAttributes' value with any optional fields omitted.
mkListInstanceAttributes ::
  -- | 'instanceId'
  Types.InstanceId ->
  ListInstanceAttributes
mkListInstanceAttributes instanceId =
  ListInstanceAttributes'
    { instanceId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liaInstanceId :: Lens.Lens' ListInstanceAttributes Types.InstanceId
liaInstanceId = Lens.field @"instanceId"
{-# DEPRECATED liaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liaMaxResults :: Lens.Lens' ListInstanceAttributes (Core.Maybe Core.Natural)
liaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED liaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liaNextToken :: Lens.Lens' ListInstanceAttributes (Core.Maybe Types.NextToken)
liaNextToken = Lens.field @"nextToken"
{-# DEPRECATED liaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListInstanceAttributes where
  type Rs ListInstanceAttributes = ListInstanceAttributesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/instance/" Core.<> (Core.toText instanceId)
                Core.<> ("/attributes")
            ),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstanceAttributesResponse'
            Core.<$> (x Core..:? "Attributes")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListInstanceAttributes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"attributes" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListInstanceAttributesResponse' smart constructor.
data ListInstanceAttributesResponse = ListInstanceAttributesResponse'
  { -- | The attribute types.
    attributes :: Core.Maybe [Types.Attribute],
    -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInstanceAttributesResponse' value with any optional fields omitted.
mkListInstanceAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListInstanceAttributesResponse
mkListInstanceAttributesResponse responseStatus =
  ListInstanceAttributesResponse'
    { attributes = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The attribute types.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liarrsAttributes :: Lens.Lens' ListInstanceAttributesResponse (Core.Maybe [Types.Attribute])
liarrsAttributes = Lens.field @"attributes"
{-# DEPRECATED liarrsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liarrsNextToken :: Lens.Lens' ListInstanceAttributesResponse (Core.Maybe Types.NextToken)
liarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED liarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liarrsResponseStatus :: Lens.Lens' ListInstanceAttributesResponse Core.Int
liarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED liarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
