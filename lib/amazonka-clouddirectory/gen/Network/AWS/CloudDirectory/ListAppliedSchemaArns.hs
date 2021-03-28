{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListAppliedSchemaArns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists schema major versions applied to a directory. If @SchemaArn@ is provided, lists the minor version.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListAppliedSchemaArns
    (
    -- * Creating a request
      ListAppliedSchemaArns (..)
    , mkListAppliedSchemaArns
    -- ** Request lenses
    , lasaDirectoryArn
    , lasaMaxResults
    , lasaNextToken
    , lasaSchemaArn

    -- * Destructuring the response
    , ListAppliedSchemaArnsResponse (..)
    , mkListAppliedSchemaArnsResponse
    -- ** Response lenses
    , lasarrsNextToken
    , lasarrsSchemaArns
    , lasarrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAppliedSchemaArns' smart constructor.
data ListAppliedSchemaArns = ListAppliedSchemaArns'
  { directoryArn :: Types.Arn
    -- ^ The ARN of the directory you are listing.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to retrieve.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  , schemaArn :: Core.Maybe Types.Arn
    -- ^ The response for @ListAppliedSchemaArns@ when this parameter is used will list all minor version ARNs for a major version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAppliedSchemaArns' value with any optional fields omitted.
mkListAppliedSchemaArns
    :: Types.Arn -- ^ 'directoryArn'
    -> ListAppliedSchemaArns
mkListAppliedSchemaArns directoryArn
  = ListAppliedSchemaArns'{directoryArn, maxResults = Core.Nothing,
                           nextToken = Core.Nothing, schemaArn = Core.Nothing}

-- | The ARN of the directory you are listing.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasaDirectoryArn :: Lens.Lens' ListAppliedSchemaArns Types.Arn
lasaDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE lasaDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasaMaxResults :: Lens.Lens' ListAppliedSchemaArns (Core.Maybe Core.Natural)
lasaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lasaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasaNextToken :: Lens.Lens' ListAppliedSchemaArns (Core.Maybe Types.NextToken)
lasaNextToken = Lens.field @"nextToken"
{-# INLINEABLE lasaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response for @ListAppliedSchemaArns@ when this parameter is used will list all minor version ARNs for a major version.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasaSchemaArn :: Lens.Lens' ListAppliedSchemaArns (Core.Maybe Types.Arn)
lasaSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE lasaSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

instance Core.ToQuery ListAppliedSchemaArns where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListAppliedSchemaArns where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON ListAppliedSchemaArns where
        toJSON ListAppliedSchemaArns{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryArn" Core..= directoryArn),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SchemaArn" Core..=) Core.<$> schemaArn])

instance Core.AWSRequest ListAppliedSchemaArns where
        type Rs ListAppliedSchemaArns = ListAppliedSchemaArnsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/schema/applied",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAppliedSchemaArnsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "SchemaArns" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAppliedSchemaArns where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"schemaArns" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListAppliedSchemaArnsResponse' smart constructor.
data ListAppliedSchemaArnsResponse = ListAppliedSchemaArnsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  , schemaArns :: Core.Maybe [Types.Arn]
    -- ^ The ARNs of schemas that are applied to the directory.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAppliedSchemaArnsResponse' value with any optional fields omitted.
mkListAppliedSchemaArnsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAppliedSchemaArnsResponse
mkListAppliedSchemaArnsResponse responseStatus
  = ListAppliedSchemaArnsResponse'{nextToken = Core.Nothing,
                                   schemaArns = Core.Nothing, responseStatus}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasarrsNextToken :: Lens.Lens' ListAppliedSchemaArnsResponse (Core.Maybe Types.NextToken)
lasarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lasarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The ARNs of schemas that are applied to the directory.
--
-- /Note:/ Consider using 'schemaArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasarrsSchemaArns :: Lens.Lens' ListAppliedSchemaArnsResponse (Core.Maybe [Types.Arn])
lasarrsSchemaArns = Lens.field @"schemaArns"
{-# INLINEABLE lasarrsSchemaArns #-}
{-# DEPRECATED schemaArns "Use generic-lens or generic-optics with 'schemaArns' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasarrsResponseStatus :: Lens.Lens' ListAppliedSchemaArnsResponse Core.Int
lasarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lasarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
