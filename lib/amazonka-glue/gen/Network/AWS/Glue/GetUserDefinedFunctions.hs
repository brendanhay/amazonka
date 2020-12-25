{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetUserDefinedFunctions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves multiple function definitions from the Data Catalog.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetUserDefinedFunctions
  ( -- * Creating a request
    GetUserDefinedFunctions (..),
    mkGetUserDefinedFunctions,

    -- ** Request lenses
    gudfPattern,
    gudfCatalogId,
    gudfDatabaseName,
    gudfMaxResults,
    gudfNextToken,

    -- * Destructuring the response
    GetUserDefinedFunctionsResponse (..),
    mkGetUserDefinedFunctionsResponse,

    -- ** Response lenses
    gudfrrsNextToken,
    gudfrrsUserDefinedFunctions,
    gudfrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetUserDefinedFunctions' smart constructor.
data GetUserDefinedFunctions = GetUserDefinedFunctions'
  { -- | An optional function-name pattern string that filters the function definitions returned.
    pattern' :: Types.NameString,
    -- | The ID of the Data Catalog where the functions to be retrieved are located. If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogIdString,
    -- | The name of the catalog database where the functions are located. If none is provided, functions from all the databases across the catalog will be returned.
    databaseName :: Core.Maybe Types.NameString,
    -- | The maximum number of functions to return in one response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Types.Token
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUserDefinedFunctions' value with any optional fields omitted.
mkGetUserDefinedFunctions ::
  -- | 'pattern\''
  Types.NameString ->
  GetUserDefinedFunctions
mkGetUserDefinedFunctions pattern' =
  GetUserDefinedFunctions'
    { pattern',
      catalogId = Core.Nothing,
      databaseName = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | An optional function-name pattern string that filters the function definitions returned.
--
-- /Note:/ Consider using 'pattern'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfPattern :: Lens.Lens' GetUserDefinedFunctions Types.NameString
gudfPattern = Lens.field @"pattern'"
{-# DEPRECATED gudfPattern "Use generic-lens or generic-optics with 'pattern'' instead." #-}

-- | The ID of the Data Catalog where the functions to be retrieved are located. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfCatalogId :: Lens.Lens' GetUserDefinedFunctions (Core.Maybe Types.CatalogIdString)
gudfCatalogId = Lens.field @"catalogId"
{-# DEPRECATED gudfCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database where the functions are located. If none is provided, functions from all the databases across the catalog will be returned.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfDatabaseName :: Lens.Lens' GetUserDefinedFunctions (Core.Maybe Types.NameString)
gudfDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED gudfDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The maximum number of functions to return in one response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfMaxResults :: Lens.Lens' GetUserDefinedFunctions (Core.Maybe Core.Natural)
gudfMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gudfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfNextToken :: Lens.Lens' GetUserDefinedFunctions (Core.Maybe Types.Token)
gudfNextToken = Lens.field @"nextToken"
{-# DEPRECATED gudfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetUserDefinedFunctions where
  toJSON GetUserDefinedFunctions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Pattern" Core..= pattern'),
            ("CatalogId" Core..=) Core.<$> catalogId,
            ("DatabaseName" Core..=) Core.<$> databaseName,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetUserDefinedFunctions where
  type Rs GetUserDefinedFunctions = GetUserDefinedFunctionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetUserDefinedFunctions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUserDefinedFunctionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "UserDefinedFunctions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetUserDefinedFunctions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"userDefinedFunctions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetUserDefinedFunctionsResponse' smart constructor.
data GetUserDefinedFunctionsResponse = GetUserDefinedFunctionsResponse'
  { -- | A continuation token, if the list of functions returned does not include the last requested function.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of requested function definitions.
    userDefinedFunctions :: Core.Maybe [Types.UserDefinedFunction],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetUserDefinedFunctionsResponse' value with any optional fields omitted.
mkGetUserDefinedFunctionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetUserDefinedFunctionsResponse
mkGetUserDefinedFunctionsResponse responseStatus =
  GetUserDefinedFunctionsResponse'
    { nextToken = Core.Nothing,
      userDefinedFunctions = Core.Nothing,
      responseStatus
    }

-- | A continuation token, if the list of functions returned does not include the last requested function.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfrrsNextToken :: Lens.Lens' GetUserDefinedFunctionsResponse (Core.Maybe Types.NextToken)
gudfrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gudfrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of requested function definitions.
--
-- /Note:/ Consider using 'userDefinedFunctions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfrrsUserDefinedFunctions :: Lens.Lens' GetUserDefinedFunctionsResponse (Core.Maybe [Types.UserDefinedFunction])
gudfrrsUserDefinedFunctions = Lens.field @"userDefinedFunctions"
{-# DEPRECATED gudfrrsUserDefinedFunctions "Use generic-lens or generic-optics with 'userDefinedFunctions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gudfrrsResponseStatus :: Lens.Lens' GetUserDefinedFunctionsResponse Core.Int
gudfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gudfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
