{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.ListResolvers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resolvers for a given API and type.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListResolvers
  ( -- * Creating a request
    ListResolvers (..),
    mkListResolvers,

    -- ** Request lenses
    lrApiId,
    lrTypeName,
    lrMaxResults,
    lrNextToken,

    -- * Destructuring the response
    ListResolversResponse (..),
    mkListResolversResponse,

    -- ** Response lenses
    lrrrsNextToken,
    lrrrsResolvers,
    lrrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListResolvers' smart constructor.
data ListResolvers = ListResolvers'
  { -- | The API ID.
    apiId :: Types.String,
    -- | The type name.
    typeName :: Types.String,
    -- | The maximum number of results you want the request to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResolvers' value with any optional fields omitted.
mkListResolvers ::
  -- | 'apiId'
  Types.String ->
  -- | 'typeName'
  Types.String ->
  ListResolvers
mkListResolvers apiId typeName =
  ListResolvers'
    { apiId,
      typeName,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrApiId :: Lens.Lens' ListResolvers Types.String
lrApiId = Lens.field @"apiId"
{-# DEPRECATED lrApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrTypeName :: Lens.Lens' ListResolvers Types.String
lrTypeName = Lens.field @"typeName"
{-# DEPRECATED lrTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The maximum number of results you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaxResults :: Lens.Lens' ListResolvers (Core.Maybe Core.Natural)
lrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListResolvers (Core.Maybe Types.PaginationToken)
lrNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListResolvers where
  type Rs ListResolvers = ListResolversResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apis/" Core.<> (Core.toText apiId) Core.<> ("/types/")
                Core.<> (Core.toText typeName)
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
          ListResolversResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "resolvers")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListResolvers where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"resolvers" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListResolversResponse' smart constructor.
data ListResolversResponse = ListResolversResponse'
  { -- | An identifier to be passed in the next request to this operation to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The @Resolver@ objects.
    resolvers :: Core.Maybe [Types.Resolver],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResolversResponse' value with any optional fields omitted.
mkListResolversResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListResolversResponse
mkListResolversResponse responseStatus =
  ListResolversResponse'
    { nextToken = Core.Nothing,
      resolvers = Core.Nothing,
      responseStatus
    }

-- | An identifier to be passed in the next request to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsNextToken :: Lens.Lens' ListResolversResponse (Core.Maybe Types.PaginationToken)
lrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The @Resolver@ objects.
--
-- /Note:/ Consider using 'resolvers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsResolvers :: Lens.Lens' ListResolversResponse (Core.Maybe [Types.Resolver])
lrrrsResolvers = Lens.field @"resolvers"
{-# DEPRECATED lrrrsResolvers "Use generic-lens or generic-optics with 'resolvers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsResponseStatus :: Lens.Lens' ListResolversResponse Core.Int
lrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
