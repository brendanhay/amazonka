{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ListResourceServers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resource servers for a user pool.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListResourceServers
  ( -- * Creating a request
    ListResourceServers (..),
    mkListResourceServers,

    -- ** Request lenses
    lrsUserPoolId,
    lrsMaxResults,
    lrsNextToken,

    -- * Destructuring the response
    ListResourceServersResponse (..),
    mkListResourceServersResponse,

    -- ** Response lenses
    lrsrrsResourceServers,
    lrsrrsNextToken,
    lrsrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListResourceServers' smart constructor.
data ListResourceServers = ListResourceServers'
  { -- | The user pool ID for the user pool.
    userPoolId :: Types.UserPoolId,
    -- | The maximum number of resource servers to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | A pagination token.
    nextToken :: Core.Maybe Types.PaginationKeyType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceServers' value with any optional fields omitted.
mkListResourceServers ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  ListResourceServers
mkListResourceServers userPoolId =
  ListResourceServers'
    { userPoolId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsUserPoolId :: Lens.Lens' ListResourceServers Types.UserPoolId
lrsUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED lrsUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The maximum number of resource servers to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsMaxResults :: Lens.Lens' ListResourceServers (Core.Maybe Core.Natural)
lrsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lrsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListResourceServers (Core.Maybe Types.PaginationKeyType)
lrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListResourceServers where
  toJSON ListResourceServers {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListResourceServers where
  type Rs ListResourceServers = ListResourceServersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.ListResourceServers"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceServersResponse'
            Core.<$> (x Core..:? "ResourceServers" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListResourceServers where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"resourceServers") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListResourceServersResponse' smart constructor.
data ListResourceServersResponse = ListResourceServersResponse'
  { -- | The resource servers.
    resourceServers :: [Types.ResourceServerType],
    -- | A pagination token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceServersResponse' value with any optional fields omitted.
mkListResourceServersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListResourceServersResponse
mkListResourceServersResponse responseStatus =
  ListResourceServersResponse'
    { resourceServers = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The resource servers.
--
-- /Note:/ Consider using 'resourceServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsrrsResourceServers :: Lens.Lens' ListResourceServersResponse [Types.ResourceServerType]
lrsrrsResourceServers = Lens.field @"resourceServers"
{-# DEPRECATED lrsrrsResourceServers "Use generic-lens or generic-optics with 'resourceServers' instead." #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsrrsNextToken :: Lens.Lens' ListResourceServersResponse (Core.Maybe Types.NextToken)
lrsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsrrsResponseStatus :: Lens.Lens' ListResourceServersResponse Core.Int
lrsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
