{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ListPresets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListPresets operation gets a list of the default presets included with Elastic Transcoder and the presets that you've added in an AWS region.
--
-- This operation returns paginated results.
module Network.AWS.ElasticTranscoder.ListPresets
    (
    -- * Creating a request
      ListPresets (..)
    , mkListPresets
    -- ** Request lenses
    , lAscending
    , lPageToken

    -- * Destructuring the response
    , ListPresetsResponse (..)
    , mkListPresetsResponse
    -- ** Response lenses
    , lrsNextPageToken
    , lrsPresets
    , lrsResponseStatus
    ) where

import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @ListPresetsRequest@ structure.
--
-- /See:/ 'mkListPresets' smart constructor.
data ListPresets = ListPresets'
  { ascending :: Core.Maybe Types.Ascending
    -- ^ To list presets in chronological order by the date and time that they were created, enter @true@ . To list presets in reverse chronological order, enter @false@ .
  , pageToken :: Core.Maybe Types.PageToken
    -- ^ When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPresets' value with any optional fields omitted.
mkListPresets
    :: ListPresets
mkListPresets
  = ListPresets'{ascending = Core.Nothing, pageToken = Core.Nothing}

-- | To list presets in chronological order by the date and time that they were created, enter @true@ . To list presets in reverse chronological order, enter @false@ .
--
-- /Note:/ Consider using 'ascending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAscending :: Lens.Lens' ListPresets (Core.Maybe Types.Ascending)
lAscending = Lens.field @"ascending"
{-# INLINEABLE lAscending #-}
{-# DEPRECATED ascending "Use generic-lens or generic-optics with 'ascending' instead"  #-}

-- | When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results. 
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lPageToken :: Lens.Lens' ListPresets (Core.Maybe Types.PageToken)
lPageToken = Lens.field @"pageToken"
{-# INLINEABLE lPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery ListPresets where
        toQuery ListPresets{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Ascending") ascending
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PageToken") pageToken

instance Core.ToHeaders ListPresets where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListPresets where
        type Rs ListPresets = ListPresetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/2012-09-25/presets",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPresetsResponse' Core.<$>
                   (x Core..:? "NextPageToken") Core.<*> x Core..:? "Presets" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPresets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"presets" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | The @ListPresetsResponse@ structure.
--
-- /See:/ 'mkListPresetsResponse' smart constructor.
data ListPresetsResponse = ListPresetsResponse'
  { nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ A value that you use to access the second and subsequent pages of results, if any. When the presets fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
  , presets :: Core.Maybe [Types.Preset]
    -- ^ An array of @Preset@ objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPresetsResponse' value with any optional fields omitted.
mkListPresetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPresetsResponse
mkListPresetsResponse responseStatus
  = ListPresetsResponse'{nextPageToken = Core.Nothing,
                         presets = Core.Nothing, responseStatus}

-- | A value that you use to access the second and subsequent pages of results, if any. When the presets fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextPageToken :: Lens.Lens' ListPresetsResponse (Core.Maybe Types.NextPageToken)
lrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE lrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | An array of @Preset@ objects.
--
-- /Note:/ Consider using 'presets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsPresets :: Lens.Lens' ListPresetsResponse (Core.Maybe [Types.Preset])
lrsPresets = Lens.field @"presets"
{-# INLINEABLE lrsPresets #-}
{-# DEPRECATED presets "Use generic-lens or generic-optics with 'presets' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListPresetsResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
