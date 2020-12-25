{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Select
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @Select@ operation returns a set of attributes for @ItemNames@ that match the select expression. @Select@ is similar to the standard SQL SELECT statement.
--
-- The total size of the response cannot exceed 1 MB in total size. Amazon SimpleDB automatically adjusts the number of items returned per page to enforce this limit. For example, if the client asks to retrieve 2500 items, but each individual item is 10 kB in size, the system returns 100 items and an appropriate @NextToken@ so the client can access the next page of results.
-- For information on how to construct select expressions, see Using Select to Create Amazon SimpleDB Queries in the Developer Guide.
--
-- This operation returns paginated results.
module Network.AWS.SDB.Select
  ( -- * Creating a request
    Select (..),
    mkSelect,

    -- ** Request lenses
    sSelectExpression,
    sConsistentRead,
    sNextToken,

    -- * Destructuring the response
    SelectResponse (..),
    mkSelectResponse,

    -- ** Response lenses
    srrsItems,
    srrsNextToken,
    srrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SDB.Types as Types

-- | /See:/ 'mkSelect' smart constructor.
data Select = Select'
  { -- | The expression used to query the domain.
    selectExpression :: Types.String,
    -- | @true@
    consistentRead :: Core.Maybe Core.Bool,
    -- | @ItemNames@
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Select' value with any optional fields omitted.
mkSelect ::
  -- | 'selectExpression'
  Types.String ->
  Select
mkSelect selectExpression =
  Select'
    { selectExpression,
      consistentRead = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The expression used to query the domain.
--
-- /Note:/ Consider using 'selectExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSelectExpression :: Lens.Lens' Select Types.String
sSelectExpression = Lens.field @"selectExpression"
{-# DEPRECATED sSelectExpression "Use generic-lens or generic-optics with 'selectExpression' instead." #-}

-- | @true@
--
-- /Note:/ Consider using 'consistentRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sConsistentRead :: Lens.Lens' Select (Core.Maybe Core.Bool)
sConsistentRead = Lens.field @"consistentRead"
{-# DEPRECATED sConsistentRead "Use generic-lens or generic-optics with 'consistentRead' instead." #-}

-- | @ItemNames@
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNextToken :: Lens.Lens' Select (Core.Maybe Types.String)
sNextToken = Lens.field @"nextToken"
{-# DEPRECATED sNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest Select where
  type Rs Select = SelectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "Select")
                Core.<> (Core.pure ("Version", "2009-04-15"))
                Core.<> (Core.toQueryValue "SelectExpression" selectExpression)
                Core.<> (Core.toQueryValue "ConsistentRead" Core.<$> consistentRead)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "SelectResult"
      ( \s h x ->
          SelectResponse'
            Core.<$> (x Core..@? "Item")
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager Select where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkSelectResponse' smart constructor.
data SelectResponse = SelectResponse'
  { -- | A list of items that match the select expression.
    items :: Core.Maybe [Types.Item],
    -- | @MaxNumberOfItems@
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SelectResponse' value with any optional fields omitted.
mkSelectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SelectResponse
mkSelectResponse responseStatus =
  SelectResponse'
    { items = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of items that match the select expression.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsItems :: Lens.Lens' SelectResponse (Core.Maybe [Types.Item])
srrsItems = Lens.field @"items"
{-# DEPRECATED srrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | @MaxNumberOfItems@
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsNextToken :: Lens.Lens' SelectResponse (Core.Maybe Types.String)
srrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED srrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsResponseStatus :: Lens.Lens' SelectResponse Core.Int
srrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
