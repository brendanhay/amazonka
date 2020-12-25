{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.ListFunctions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List multiple functions.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListFunctions
  ( -- * Creating a request
    ListFunctions (..),
    mkListFunctions,

    -- ** Request lenses
    lfApiId,
    lfMaxResults,
    lfNextToken,

    -- * Destructuring the response
    ListFunctionsResponse (..),
    mkListFunctionsResponse,

    -- ** Response lenses
    lfrrsFunctions,
    lfrrsNextToken,
    lfrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListFunctions' smart constructor.
data ListFunctions = ListFunctions'
  { -- | The GraphQL API ID.
    apiId :: Types.String,
    -- | The maximum number of results you want the request to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFunctions' value with any optional fields omitted.
mkListFunctions ::
  -- | 'apiId'
  Types.String ->
  ListFunctions
mkListFunctions apiId =
  ListFunctions'
    { apiId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The GraphQL API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfApiId :: Lens.Lens' ListFunctions Types.String
lfApiId = Lens.field @"apiId"
{-# DEPRECATED lfApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The maximum number of results you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMaxResults :: Lens.Lens' ListFunctions (Core.Maybe Core.Natural)
lfMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfNextToken :: Lens.Lens' ListFunctions (Core.Maybe Types.PaginationToken)
lfNextToken = Lens.field @"nextToken"
{-# DEPRECATED lfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListFunctions where
  type Rs ListFunctions = ListFunctionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/v1/apis/" Core.<> (Core.toText apiId) Core.<> ("/functions")),
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
          ListFunctionsResponse'
            Core.<$> (x Core..:? "functions")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListFunctions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"functions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListFunctionsResponse' smart constructor.
data ListFunctionsResponse = ListFunctionsResponse'
  { -- | A list of @Function@ objects.
    functions :: Core.Maybe [Types.FunctionConfiguration],
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFunctionsResponse' value with any optional fields omitted.
mkListFunctionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListFunctionsResponse
mkListFunctionsResponse responseStatus =
  ListFunctionsResponse'
    { functions = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of @Function@ objects.
--
-- /Note:/ Consider using 'functions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsFunctions :: Lens.Lens' ListFunctionsResponse (Core.Maybe [Types.FunctionConfiguration])
lfrrsFunctions = Lens.field @"functions"
{-# DEPRECATED lfrrsFunctions "Use generic-lens or generic-optics with 'functions' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsNextToken :: Lens.Lens' ListFunctionsResponse (Core.Maybe Types.PaginationToken)
lfrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lfrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsResponseStatus :: Lens.Lens' ListFunctionsResponse Core.Int
lfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
