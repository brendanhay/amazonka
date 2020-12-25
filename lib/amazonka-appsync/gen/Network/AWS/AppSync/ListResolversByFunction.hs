{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.ListResolversByFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the resolvers that are associated with a specific function.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListResolversByFunction
  ( -- * Creating a request
    ListResolversByFunction (..),
    mkListResolversByFunction,

    -- ** Request lenses
    lrbfApiId,
    lrbfFunctionId,
    lrbfMaxResults,
    lrbfNextToken,

    -- * Destructuring the response
    ListResolversByFunctionResponse (..),
    mkListResolversByFunctionResponse,

    -- ** Response lenses
    lrbfrrsNextToken,
    lrbfrrsResolvers,
    lrbfrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListResolversByFunction' smart constructor.
data ListResolversByFunction = ListResolversByFunction'
  { -- | The API ID.
    apiId :: Types.ApiId,
    -- | The Function ID.
    functionId :: Types.FunctionId,
    -- | The maximum number of results you want the request to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | An identifier that was returned from the previous call to this operation, which you can use to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResolversByFunction' value with any optional fields omitted.
mkListResolversByFunction ::
  -- | 'apiId'
  Types.ApiId ->
  -- | 'functionId'
  Types.FunctionId ->
  ListResolversByFunction
mkListResolversByFunction apiId functionId =
  ListResolversByFunction'
    { apiId,
      functionId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfApiId :: Lens.Lens' ListResolversByFunction Types.ApiId
lrbfApiId = Lens.field @"apiId"
{-# DEPRECATED lrbfApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The Function ID.
--
-- /Note:/ Consider using 'functionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfFunctionId :: Lens.Lens' ListResolversByFunction Types.FunctionId
lrbfFunctionId = Lens.field @"functionId"
{-# DEPRECATED lrbfFunctionId "Use generic-lens or generic-optics with 'functionId' instead." #-}

-- | The maximum number of results you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfMaxResults :: Lens.Lens' ListResolversByFunction (Core.Maybe Core.Natural)
lrbfMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lrbfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which you can use to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfNextToken :: Lens.Lens' ListResolversByFunction (Core.Maybe Types.PaginationToken)
lrbfNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrbfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListResolversByFunction where
  type Rs ListResolversByFunction = ListResolversByFunctionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apis/" Core.<> (Core.toText apiId) Core.<> ("/functions/")
                Core.<> (Core.toText functionId)
                Core.<> ("/resolvers")
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
          ListResolversByFunctionResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "resolvers")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListResolversByFunction where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"resolvers" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListResolversByFunctionResponse' smart constructor.
data ListResolversByFunctionResponse = ListResolversByFunctionResponse'
  { -- | An identifier that can be used to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The list of resolvers.
    resolvers :: Core.Maybe [Types.Resolver],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResolversByFunctionResponse' value with any optional fields omitted.
mkListResolversByFunctionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListResolversByFunctionResponse
mkListResolversByFunctionResponse responseStatus =
  ListResolversByFunctionResponse'
    { nextToken = Core.Nothing,
      resolvers = Core.Nothing,
      responseStatus
    }

-- | An identifier that can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfrrsNextToken :: Lens.Lens' ListResolversByFunctionResponse (Core.Maybe Types.PaginationToken)
lrbfrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrbfrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of resolvers.
--
-- /Note:/ Consider using 'resolvers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfrrsResolvers :: Lens.Lens' ListResolversByFunctionResponse (Core.Maybe [Types.Resolver])
lrbfrrsResolvers = Lens.field @"resolvers"
{-# DEPRECATED lrbfrrsResolvers "Use generic-lens or generic-optics with 'resolvers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfrrsResponseStatus :: Lens.Lens' ListResolversByFunctionResponse Core.Int
lrbfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrbfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
