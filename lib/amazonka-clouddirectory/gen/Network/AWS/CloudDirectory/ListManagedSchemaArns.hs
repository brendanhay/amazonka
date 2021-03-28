{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListManagedSchemaArns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the major version families of each managed schema. If a major version ARN is provided as SchemaArn, the minor version revisions in that family are listed instead.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListManagedSchemaArns
    (
    -- * Creating a request
      ListManagedSchemaArns (..)
    , mkListManagedSchemaArns
    -- ** Request lenses
    , lmsaMaxResults
    , lmsaNextToken
    , lmsaSchemaArn

    -- * Destructuring the response
    , ListManagedSchemaArnsResponse (..)
    , mkListManagedSchemaArnsResponse
    -- ** Response lenses
    , lmsarrsNextToken
    , lmsarrsSchemaArns
    , lmsarrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListManagedSchemaArns' smart constructor.
data ListManagedSchemaArns = ListManagedSchemaArns'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to retrieve.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  , schemaArn :: Core.Maybe Types.Arn
    -- ^ The response for ListManagedSchemaArns. When this parameter is used, all minor version ARNs for a major version are listed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListManagedSchemaArns' value with any optional fields omitted.
mkListManagedSchemaArns
    :: ListManagedSchemaArns
mkListManagedSchemaArns
  = ListManagedSchemaArns'{maxResults = Core.Nothing,
                           nextToken = Core.Nothing, schemaArn = Core.Nothing}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsaMaxResults :: Lens.Lens' ListManagedSchemaArns (Core.Maybe Core.Natural)
lmsaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lmsaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsaNextToken :: Lens.Lens' ListManagedSchemaArns (Core.Maybe Types.NextToken)
lmsaNextToken = Lens.field @"nextToken"
{-# INLINEABLE lmsaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response for ListManagedSchemaArns. When this parameter is used, all minor version ARNs for a major version are listed.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsaSchemaArn :: Lens.Lens' ListManagedSchemaArns (Core.Maybe Types.Arn)
lmsaSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE lmsaSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

instance Core.ToQuery ListManagedSchemaArns where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListManagedSchemaArns where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON ListManagedSchemaArns where
        toJSON ListManagedSchemaArns{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SchemaArn" Core..=) Core.<$> schemaArn])

instance Core.AWSRequest ListManagedSchemaArns where
        type Rs ListManagedSchemaArns = ListManagedSchemaArnsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/schema/managed",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListManagedSchemaArnsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "SchemaArns" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListManagedSchemaArns where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"schemaArns" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListManagedSchemaArnsResponse' smart constructor.
data ListManagedSchemaArnsResponse = ListManagedSchemaArnsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  , schemaArns :: Core.Maybe [Types.Arn]
    -- ^ The ARNs for all AWS managed schemas.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListManagedSchemaArnsResponse' value with any optional fields omitted.
mkListManagedSchemaArnsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListManagedSchemaArnsResponse
mkListManagedSchemaArnsResponse responseStatus
  = ListManagedSchemaArnsResponse'{nextToken = Core.Nothing,
                                   schemaArns = Core.Nothing, responseStatus}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsarrsNextToken :: Lens.Lens' ListManagedSchemaArnsResponse (Core.Maybe Types.NextToken)
lmsarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lmsarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The ARNs for all AWS managed schemas.
--
-- /Note:/ Consider using 'schemaArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsarrsSchemaArns :: Lens.Lens' ListManagedSchemaArnsResponse (Core.Maybe [Types.Arn])
lmsarrsSchemaArns = Lens.field @"schemaArns"
{-# INLINEABLE lmsarrsSchemaArns #-}
{-# DEPRECATED schemaArns "Use generic-lens or generic-optics with 'schemaArns' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsarrsResponseStatus :: Lens.Lens' ListManagedSchemaArnsResponse Core.Int
lmsarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lmsarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
