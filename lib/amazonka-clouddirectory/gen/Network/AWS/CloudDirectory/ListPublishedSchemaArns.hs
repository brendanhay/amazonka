{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListPublishedSchemaArns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the major version families of each published schema. If a major version ARN is provided as @SchemaArn@ , the minor version revisions in that family are listed instead.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListPublishedSchemaArns
    (
    -- * Creating a request
      ListPublishedSchemaArns (..)
    , mkListPublishedSchemaArns
    -- ** Request lenses
    , lpsaMaxResults
    , lpsaNextToken
    , lpsaSchemaArn

    -- * Destructuring the response
    , ListPublishedSchemaArnsResponse (..)
    , mkListPublishedSchemaArnsResponse
    -- ** Response lenses
    , lpsarrsNextToken
    , lpsarrsSchemaArns
    , lpsarrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPublishedSchemaArns' smart constructor.
data ListPublishedSchemaArns = ListPublishedSchemaArns'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to retrieve.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  , schemaArn :: Core.Maybe Types.SchemaArn
    -- ^ The response for @ListPublishedSchemaArns@ when this parameter is used will list all minor version ARNs for a major version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPublishedSchemaArns' value with any optional fields omitted.
mkListPublishedSchemaArns
    :: ListPublishedSchemaArns
mkListPublishedSchemaArns
  = ListPublishedSchemaArns'{maxResults = Core.Nothing,
                             nextToken = Core.Nothing, schemaArn = Core.Nothing}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsaMaxResults :: Lens.Lens' ListPublishedSchemaArns (Core.Maybe Core.Natural)
lpsaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lpsaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsaNextToken :: Lens.Lens' ListPublishedSchemaArns (Core.Maybe Types.NextToken)
lpsaNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpsaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response for @ListPublishedSchemaArns@ when this parameter is used will list all minor version ARNs for a major version.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsaSchemaArn :: Lens.Lens' ListPublishedSchemaArns (Core.Maybe Types.SchemaArn)
lpsaSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE lpsaSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

instance Core.ToQuery ListPublishedSchemaArns where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListPublishedSchemaArns where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON ListPublishedSchemaArns where
        toJSON ListPublishedSchemaArns{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SchemaArn" Core..=) Core.<$> schemaArn])

instance Core.AWSRequest ListPublishedSchemaArns where
        type Rs ListPublishedSchemaArns = ListPublishedSchemaArnsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/schema/published",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPublishedSchemaArnsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "SchemaArns" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPublishedSchemaArns where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"schemaArns" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListPublishedSchemaArnsResponse' smart constructor.
data ListPublishedSchemaArnsResponse = ListPublishedSchemaArnsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  , schemaArns :: Core.Maybe [Types.Arn]
    -- ^ The ARNs of published schemas.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPublishedSchemaArnsResponse' value with any optional fields omitted.
mkListPublishedSchemaArnsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPublishedSchemaArnsResponse
mkListPublishedSchemaArnsResponse responseStatus
  = ListPublishedSchemaArnsResponse'{nextToken = Core.Nothing,
                                     schemaArns = Core.Nothing, responseStatus}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsarrsNextToken :: Lens.Lens' ListPublishedSchemaArnsResponse (Core.Maybe Types.NextToken)
lpsarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpsarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The ARNs of published schemas.
--
-- /Note:/ Consider using 'schemaArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsarrsSchemaArns :: Lens.Lens' ListPublishedSchemaArnsResponse (Core.Maybe [Types.Arn])
lpsarrsSchemaArns = Lens.field @"schemaArns"
{-# INLINEABLE lpsarrsSchemaArns #-}
{-# DEPRECATED schemaArns "Use generic-lens or generic-optics with 'schemaArns' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsarrsResponseStatus :: Lens.Lens' ListPublishedSchemaArnsResponse Core.Int
lpsarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lpsarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
