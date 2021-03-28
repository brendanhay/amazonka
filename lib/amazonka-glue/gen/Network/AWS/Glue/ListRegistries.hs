{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListRegistries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of registries that you have created, with minimal registry information. Registries in the @Deleting@ status will not be included in the results. Empty results will be returned if there are no registries available.
--
-- This operation returns paginated results.
module Network.AWS.Glue.ListRegistries
    (
    -- * Creating a request
      ListRegistries (..)
    , mkListRegistries
    -- ** Request lenses
    , lrMaxResults
    , lrNextToken

    -- * Destructuring the response
    , ListRegistriesResponse (..)
    , mkListRegistriesResponse
    -- ** Response lenses
    , lrrrsNextToken
    , lrrrsRegistries
    , lrrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListRegistries' smart constructor.
data ListRegistries = ListRegistries'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token, if this is a continuation call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRegistries' value with any optional fields omitted.
mkListRegistries
    :: ListRegistries
mkListRegistries
  = ListRegistries'{maxResults = Core.Nothing,
                    nextToken = Core.Nothing}

-- | Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaxResults :: Lens.Lens' ListRegistries (Core.Maybe Core.Natural)
lrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListRegistries (Core.Maybe Types.NextToken)
lrNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListRegistries where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListRegistries where
        toHeaders ListRegistries{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.ListRegistries") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListRegistries where
        toJSON ListRegistries{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListRegistries where
        type Rs ListRegistries = ListRegistriesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListRegistriesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Registries" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListRegistries where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"registries" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListRegistriesResponse' smart constructor.
data ListRegistriesResponse = ListRegistriesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
  , registries :: Core.Maybe [Types.RegistryListItem]
    -- ^ An array of @RegistryDetailedListItem@ objects containing minimal details of each registry.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRegistriesResponse' value with any optional fields omitted.
mkListRegistriesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListRegistriesResponse
mkListRegistriesResponse responseStatus
  = ListRegistriesResponse'{nextToken = Core.Nothing,
                            registries = Core.Nothing, responseStatus}

-- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsNextToken :: Lens.Lens' ListRegistriesResponse (Core.Maybe Types.NextToken)
lrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of @RegistryDetailedListItem@ objects containing minimal details of each registry.
--
-- /Note:/ Consider using 'registries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsRegistries :: Lens.Lens' ListRegistriesResponse (Core.Maybe [Types.RegistryListItem])
lrrrsRegistries = Lens.field @"registries"
{-# INLINEABLE lrrrsRegistries #-}
{-# DEPRECATED registries "Use generic-lens or generic-optics with 'registries' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsResponseStatus :: Lens.Lens' ListRegistriesResponse Core.Int
lrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
