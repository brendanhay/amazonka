{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.SearchIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The query search index.
module Network.AWS.IoT.SearchIndex
    (
    -- * Creating a request
      SearchIndex (..)
    , mkSearchIndex
    -- ** Request lenses
    , siQueryString
    , siIndexName
    , siMaxResults
    , siNextToken
    , siQueryVersion

    -- * Destructuring the response
    , SearchIndexResponse (..)
    , mkSearchIndexResponse
    -- ** Response lenses
    , sirrsNextToken
    , sirrsThingGroups
    , sirrsThings
    , sirrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSearchIndex' smart constructor.
data SearchIndex = SearchIndex'
  { queryString :: Types.QueryString
    -- ^ The search query string.
  , indexName :: Core.Maybe Types.IndexName
    -- ^ The search index name.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return at one time.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token used to get the next set of results, or @null@ if there are no additional results.
  , queryVersion :: Core.Maybe Types.QueryVersion
    -- ^ The query version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchIndex' value with any optional fields omitted.
mkSearchIndex
    :: Types.QueryString -- ^ 'queryString'
    -> SearchIndex
mkSearchIndex queryString
  = SearchIndex'{queryString, indexName = Core.Nothing,
                 maxResults = Core.Nothing, nextToken = Core.Nothing,
                 queryVersion = Core.Nothing}

-- | The search query string.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siQueryString :: Lens.Lens' SearchIndex Types.QueryString
siQueryString = Lens.field @"queryString"
{-# INLINEABLE siQueryString #-}
{-# DEPRECATED queryString "Use generic-lens or generic-optics with 'queryString' instead"  #-}

-- | The search index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siIndexName :: Lens.Lens' SearchIndex (Core.Maybe Types.IndexName)
siIndexName = Lens.field @"indexName"
{-# INLINEABLE siIndexName #-}
{-# DEPRECATED indexName "Use generic-lens or generic-optics with 'indexName' instead"  #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siMaxResults :: Lens.Lens' SearchIndex (Core.Maybe Core.Natural)
siMaxResults = Lens.field @"maxResults"
{-# INLINEABLE siMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token used to get the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siNextToken :: Lens.Lens' SearchIndex (Core.Maybe Types.NextToken)
siNextToken = Lens.field @"nextToken"
{-# INLINEABLE siNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The query version.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siQueryVersion :: Lens.Lens' SearchIndex (Core.Maybe Types.QueryVersion)
siQueryVersion = Lens.field @"queryVersion"
{-# INLINEABLE siQueryVersion #-}
{-# DEPRECATED queryVersion "Use generic-lens or generic-optics with 'queryVersion' instead"  #-}

instance Core.ToQuery SearchIndex where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SearchIndex where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON SearchIndex where
        toJSON SearchIndex{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("queryString" Core..= queryString),
                  ("indexName" Core..=) Core.<$> indexName,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("queryVersion" Core..=) Core.<$> queryVersion])

instance Core.AWSRequest SearchIndex where
        type Rs SearchIndex = SearchIndexResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/indices/search",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SearchIndexResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "thingGroups" Core.<*>
                     x Core..:? "things"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSearchIndexResponse' smart constructor.
data SearchIndexResponse = SearchIndexResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token used to get the next set of results, or @null@ if there are no additional results.
  , thingGroups :: Core.Maybe [Types.ThingGroupDocument]
    -- ^ The thing groups that match the search query.
  , things :: Core.Maybe [Types.ThingDocument]
    -- ^ The things that match the search query.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchIndexResponse' value with any optional fields omitted.
mkSearchIndexResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SearchIndexResponse
mkSearchIndexResponse responseStatus
  = SearchIndexResponse'{nextToken = Core.Nothing,
                         thingGroups = Core.Nothing, things = Core.Nothing, responseStatus}

-- | The token used to get the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsNextToken :: Lens.Lens' SearchIndexResponse (Core.Maybe Types.NextToken)
sirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE sirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The thing groups that match the search query.
--
-- /Note:/ Consider using 'thingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsThingGroups :: Lens.Lens' SearchIndexResponse (Core.Maybe [Types.ThingGroupDocument])
sirrsThingGroups = Lens.field @"thingGroups"
{-# INLINEABLE sirrsThingGroups #-}
{-# DEPRECATED thingGroups "Use generic-lens or generic-optics with 'thingGroups' instead"  #-}

-- | The things that match the search query.
--
-- /Note:/ Consider using 'things' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsThings :: Lens.Lens' SearchIndexResponse (Core.Maybe [Types.ThingDocument])
sirrsThings = Lens.field @"things"
{-# INLINEABLE sirrsThings #-}
{-# DEPRECATED things "Use generic-lens or generic-optics with 'things' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsResponseStatus :: Lens.Lens' SearchIndexResponse Core.Int
sirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
