{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListConferenceProviders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists conference providers under a specific AWS account.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListConferenceProviders
    (
    -- * Creating a request
      ListConferenceProviders (..)
    , mkListConferenceProviders
    -- ** Request lenses
    , lcpMaxResults
    , lcpNextToken

    -- * Destructuring the response
    , ListConferenceProvidersResponse (..)
    , mkListConferenceProvidersResponse
    -- ** Response lenses
    , lcprrsConferenceProviders
    , lcprrsNextToken
    , lcprrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListConferenceProviders' smart constructor.
data ListConferenceProviders = ListConferenceProviders'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of conference providers to be returned, per paginated calls.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The tokens used for pagination.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListConferenceProviders' value with any optional fields omitted.
mkListConferenceProviders
    :: ListConferenceProviders
mkListConferenceProviders
  = ListConferenceProviders'{maxResults = Core.Nothing,
                             nextToken = Core.Nothing}

-- | The maximum number of conference providers to be returned, per paginated calls.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcpMaxResults :: Lens.Lens' ListConferenceProviders (Core.Maybe Core.Natural)
lcpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The tokens used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcpNextToken :: Lens.Lens' ListConferenceProviders (Core.Maybe Types.NextToken)
lcpNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListConferenceProviders where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListConferenceProviders where
        toHeaders ListConferenceProviders{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.ListConferenceProviders")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListConferenceProviders where
        toJSON ListConferenceProviders{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListConferenceProviders where
        type Rs ListConferenceProviders = ListConferenceProvidersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListConferenceProvidersResponse' Core.<$>
                   (x Core..:? "ConferenceProviders") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListConferenceProviders where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"conferenceProviders" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListConferenceProvidersResponse' smart constructor.
data ListConferenceProvidersResponse = ListConferenceProvidersResponse'
  { conferenceProviders :: Core.Maybe [Types.ConferenceProvider]
    -- ^ The conference providers.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The tokens used for pagination.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListConferenceProvidersResponse' value with any optional fields omitted.
mkListConferenceProvidersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListConferenceProvidersResponse
mkListConferenceProvidersResponse responseStatus
  = ListConferenceProvidersResponse'{conferenceProviders =
                                       Core.Nothing,
                                     nextToken = Core.Nothing, responseStatus}

-- | The conference providers.
--
-- /Note:/ Consider using 'conferenceProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcprrsConferenceProviders :: Lens.Lens' ListConferenceProvidersResponse (Core.Maybe [Types.ConferenceProvider])
lcprrsConferenceProviders = Lens.field @"conferenceProviders"
{-# INLINEABLE lcprrsConferenceProviders #-}
{-# DEPRECATED conferenceProviders "Use generic-lens or generic-optics with 'conferenceProviders' instead"  #-}

-- | The tokens used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcprrsNextToken :: Lens.Lens' ListConferenceProvidersResponse (Core.Maybe Types.NextToken)
lcprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcprrsResponseStatus :: Lens.Lens' ListConferenceProvidersResponse Core.Int
lcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
