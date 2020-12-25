{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.ListAssociatedFleets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the name of the fleet that is associated with the specified stack.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.ListAssociatedFleets
  ( -- * Creating a request
    ListAssociatedFleets (..),
    mkListAssociatedFleets,

    -- ** Request lenses
    lafStackName,
    lafNextToken,

    -- * Destructuring the response
    ListAssociatedFleetsResponse (..),
    mkListAssociatedFleetsResponse,

    -- ** Response lenses
    lafrrsNames,
    lafrrsNextToken,
    lafrrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAssociatedFleets' smart constructor.
data ListAssociatedFleets = ListAssociatedFleets'
  { -- | The name of the stack.
    stackName :: Types.String,
    -- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAssociatedFleets' value with any optional fields omitted.
mkListAssociatedFleets ::
  -- | 'stackName'
  Types.String ->
  ListAssociatedFleets
mkListAssociatedFleets stackName =
  ListAssociatedFleets' {stackName, nextToken = Core.Nothing}

-- | The name of the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafStackName :: Lens.Lens' ListAssociatedFleets Types.String
lafStackName = Lens.field @"stackName"
{-# DEPRECATED lafStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafNextToken :: Lens.Lens' ListAssociatedFleets (Core.Maybe Types.String)
lafNextToken = Lens.field @"nextToken"
{-# DEPRECATED lafNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListAssociatedFleets where
  toJSON ListAssociatedFleets {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StackName" Core..= stackName),
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListAssociatedFleets where
  type Rs ListAssociatedFleets = ListAssociatedFleetsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "PhotonAdminProxyService.ListAssociatedFleets")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociatedFleetsResponse'
            Core.<$> (x Core..:? "Names")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListAssociatedFleets where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"names" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListAssociatedFleetsResponse' smart constructor.
data ListAssociatedFleetsResponse = ListAssociatedFleetsResponse'
  { -- | The name of the fleet.
    names :: Core.Maybe [Types.String],
    -- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAssociatedFleetsResponse' value with any optional fields omitted.
mkListAssociatedFleetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAssociatedFleetsResponse
mkListAssociatedFleetsResponse responseStatus =
  ListAssociatedFleetsResponse'
    { names = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The name of the fleet.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafrrsNames :: Lens.Lens' ListAssociatedFleetsResponse (Core.Maybe [Types.String])
lafrrsNames = Lens.field @"names"
{-# DEPRECATED lafrrsNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafrrsNextToken :: Lens.Lens' ListAssociatedFleetsResponse (Core.Maybe Types.String)
lafrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lafrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafrrsResponseStatus :: Lens.Lens' ListAssociatedFleetsResponse Core.Int
lafrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lafrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
