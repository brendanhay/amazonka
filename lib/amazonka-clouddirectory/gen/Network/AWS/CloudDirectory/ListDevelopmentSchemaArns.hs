{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListDevelopmentSchemaArns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves each Amazon Resource Name (ARN) of schemas in the development state.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListDevelopmentSchemaArns
    (
    -- * Creating a request
      ListDevelopmentSchemaArns (..)
    , mkListDevelopmentSchemaArns
    -- ** Request lenses
    , ldsaMaxResults
    , ldsaNextToken

    -- * Destructuring the response
    , ListDevelopmentSchemaArnsResponse (..)
    , mkListDevelopmentSchemaArnsResponse
    -- ** Response lenses
    , ldsarrsNextToken
    , ldsarrsSchemaArns
    , ldsarrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDevelopmentSchemaArns' smart constructor.
data ListDevelopmentSchemaArns = ListDevelopmentSchemaArns'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to retrieve.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDevelopmentSchemaArns' value with any optional fields omitted.
mkListDevelopmentSchemaArns
    :: ListDevelopmentSchemaArns
mkListDevelopmentSchemaArns
  = ListDevelopmentSchemaArns'{maxResults = Core.Nothing,
                               nextToken = Core.Nothing}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsaMaxResults :: Lens.Lens' ListDevelopmentSchemaArns (Core.Maybe Core.Natural)
ldsaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ldsaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsaNextToken :: Lens.Lens' ListDevelopmentSchemaArns (Core.Maybe Types.NextToken)
ldsaNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldsaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListDevelopmentSchemaArns where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListDevelopmentSchemaArns where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON ListDevelopmentSchemaArns where
        toJSON ListDevelopmentSchemaArns{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListDevelopmentSchemaArns where
        type Rs ListDevelopmentSchemaArns =
             ListDevelopmentSchemaArnsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/amazonclouddirectory/2017-01-11/schema/development",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListDevelopmentSchemaArnsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "SchemaArns" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListDevelopmentSchemaArns where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"schemaArns" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListDevelopmentSchemaArnsResponse' smart constructor.
data ListDevelopmentSchemaArnsResponse = ListDevelopmentSchemaArnsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  , schemaArns :: Core.Maybe [Types.Arn]
    -- ^ The ARNs of retrieved development schemas.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDevelopmentSchemaArnsResponse' value with any optional fields omitted.
mkListDevelopmentSchemaArnsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDevelopmentSchemaArnsResponse
mkListDevelopmentSchemaArnsResponse responseStatus
  = ListDevelopmentSchemaArnsResponse'{nextToken = Core.Nothing,
                                       schemaArns = Core.Nothing, responseStatus}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsarrsNextToken :: Lens.Lens' ListDevelopmentSchemaArnsResponse (Core.Maybe Types.NextToken)
ldsarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldsarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The ARNs of retrieved development schemas.
--
-- /Note:/ Consider using 'schemaArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsarrsSchemaArns :: Lens.Lens' ListDevelopmentSchemaArnsResponse (Core.Maybe [Types.Arn])
ldsarrsSchemaArns = Lens.field @"schemaArns"
{-# INLINEABLE ldsarrsSchemaArns #-}
{-# DEPRECATED schemaArns "Use generic-lens or generic-optics with 'schemaArns' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsarrsResponseStatus :: Lens.Lens' ListDevelopmentSchemaArnsResponse Core.Int
ldsarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldsarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
