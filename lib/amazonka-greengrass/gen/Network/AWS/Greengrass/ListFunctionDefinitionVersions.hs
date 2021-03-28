{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListFunctionDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a Lambda function definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListFunctionDefinitionVersions
    (
    -- * Creating a request
      ListFunctionDefinitionVersions (..)
    , mkListFunctionDefinitionVersions
    -- ** Request lenses
    , lfdvFunctionDefinitionId
    , lfdvMaxResults
    , lfdvNextToken

    -- * Destructuring the response
    , ListFunctionDefinitionVersionsResponse (..)
    , mkListFunctionDefinitionVersionsResponse
    -- ** Response lenses
    , lfdvrrsNextToken
    , lfdvrrsVersions
    , lfdvrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListFunctionDefinitionVersions' smart constructor.
data ListFunctionDefinitionVersions = ListFunctionDefinitionVersions'
  { functionDefinitionId :: Core.Text
    -- ^ The ID of the Lambda function definition.
  , maxResults :: Core.Maybe Core.Text
    -- ^ The maximum number of results to be returned per request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFunctionDefinitionVersions' value with any optional fields omitted.
mkListFunctionDefinitionVersions
    :: Core.Text -- ^ 'functionDefinitionId'
    -> ListFunctionDefinitionVersions
mkListFunctionDefinitionVersions functionDefinitionId
  = ListFunctionDefinitionVersions'{functionDefinitionId,
                                    maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the Lambda function definition.
--
-- /Note:/ Consider using 'functionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvFunctionDefinitionId :: Lens.Lens' ListFunctionDefinitionVersions Core.Text
lfdvFunctionDefinitionId = Lens.field @"functionDefinitionId"
{-# INLINEABLE lfdvFunctionDefinitionId #-}
{-# DEPRECATED functionDefinitionId "Use generic-lens or generic-optics with 'functionDefinitionId' instead"  #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvMaxResults :: Lens.Lens' ListFunctionDefinitionVersions (Core.Maybe Core.Text)
lfdvMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lfdvMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvNextToken :: Lens.Lens' ListFunctionDefinitionVersions (Core.Maybe Core.Text)
lfdvNextToken = Lens.field @"nextToken"
{-# INLINEABLE lfdvNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListFunctionDefinitionVersions where
        toQuery ListFunctionDefinitionVersions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListFunctionDefinitionVersions where
        toHeaders ListFunctionDefinitionVersions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListFunctionDefinitionVersions where
        type Rs ListFunctionDefinitionVersions =
             ListFunctionDefinitionVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/definition/functions/" Core.<>
                             Core.toText functionDefinitionId
                             Core.<> "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListFunctionDefinitionVersionsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Versions" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListFunctionDefinitionVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListFunctionDefinitionVersionsResponse' smart constructor.
data ListFunctionDefinitionVersionsResponse = ListFunctionDefinitionVersionsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  , versions :: Core.Maybe [Types.VersionInformation]
    -- ^ Information about a version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFunctionDefinitionVersionsResponse' value with any optional fields omitted.
mkListFunctionDefinitionVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListFunctionDefinitionVersionsResponse
mkListFunctionDefinitionVersionsResponse responseStatus
  = ListFunctionDefinitionVersionsResponse'{nextToken = Core.Nothing,
                                            versions = Core.Nothing, responseStatus}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvrrsNextToken :: Lens.Lens' ListFunctionDefinitionVersionsResponse (Core.Maybe Core.Text)
lfdvrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lfdvrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvrrsVersions :: Lens.Lens' ListFunctionDefinitionVersionsResponse (Core.Maybe [Types.VersionInformation])
lfdvrrsVersions = Lens.field @"versions"
{-# INLINEABLE lfdvrrsVersions #-}
{-# DEPRECATED versions "Use generic-lens or generic-optics with 'versions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvrrsResponseStatus :: Lens.Lens' ListFunctionDefinitionVersionsResponse Core.Int
lfdvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lfdvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
