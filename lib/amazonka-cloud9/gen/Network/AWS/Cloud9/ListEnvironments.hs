{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.ListEnvironments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of AWS Cloud9 development environment identifiers.
--
-- This operation returns paginated results.
module Network.AWS.Cloud9.ListEnvironments
  ( -- * Creating a request
    ListEnvironments (..),
    mkListEnvironments,

    -- ** Request lenses
    leMaxResults,
    leNextToken,

    -- * Destructuring the response
    ListEnvironmentsResponse (..),
    mkListEnvironmentsResponse,

    -- ** Response lenses
    lerrsEnvironmentIds,
    lerrsNextToken,
    lerrsResponseStatus,
  )
where

import qualified Network.AWS.Cloud9.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListEnvironments' smart constructor.
data ListEnvironments = ListEnvironments'
  { -- | The maximum number of environments to get identifiers for.
    maxResults :: Core.Maybe Core.Natural,
    -- | During a previous call, if there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListEnvironments' value with any optional fields omitted.
mkListEnvironments ::
  ListEnvironments
mkListEnvironments =
  ListEnvironments'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of environments to get identifiers for.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMaxResults :: Lens.Lens' ListEnvironments (Core.Maybe Core.Natural)
leMaxResults = Lens.field @"maxResults"
{-# DEPRECATED leMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | During a previous call, if there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leNextToken :: Lens.Lens' ListEnvironments (Core.Maybe Types.String)
leNextToken = Lens.field @"nextToken"
{-# DEPRECATED leNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListEnvironments where
  toJSON ListEnvironments {..} =
    Core.object
      ( Core.catMaybes
          [ ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListEnvironments where
  type Rs ListEnvironments = ListEnvironmentsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCloud9WorkspaceManagementService.ListEnvironments"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEnvironmentsResponse'
            Core.<$> (x Core..:? "environmentIds")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListEnvironments where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"environmentIds" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListEnvironmentsResponse' smart constructor.
data ListEnvironmentsResponse = ListEnvironmentsResponse'
  { -- | The list of environment identifiers.
    environmentIds :: Core.Maybe [Types.EnvironmentId],
    -- | If there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListEnvironmentsResponse' value with any optional fields omitted.
mkListEnvironmentsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListEnvironmentsResponse
mkListEnvironmentsResponse responseStatus =
  ListEnvironmentsResponse'
    { environmentIds = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of environment identifiers.
--
-- /Note:/ Consider using 'environmentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsEnvironmentIds :: Lens.Lens' ListEnvironmentsResponse (Core.Maybe [Types.EnvironmentId])
lerrsEnvironmentIds = Lens.field @"environmentIds"
{-# DEPRECATED lerrsEnvironmentIds "Use generic-lens or generic-optics with 'environmentIds' instead." #-}

-- | If there are more than 25 items in the list, only the first 25 items are returned, along with a unique string called a /next token/ . To get the next batch of items in the list, call this operation again, adding the next token to the call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsNextToken :: Lens.Lens' ListEnvironmentsResponse (Core.Maybe Types.String)
lerrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lerrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsResponseStatus :: Lens.Lens' ListEnvironmentsResponse Core.Int
lerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
