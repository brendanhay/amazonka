{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.ListPresets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JSON array of up to twenty of your presets. This will return the presets themselves, not just a list of them. To retrieve the next twenty presets, use the nextToken string returned with the array.
--
-- This operation returns paginated results.
module Network.AWS.MediaConvert.ListPresets
    (
    -- * Creating a request
      ListPresets (..)
    , mkListPresets
    -- ** Request lenses
    , lpCategory
    , lpListBy
    , lpMaxResults
    , lpNextToken
    , lpOrder

    -- * Destructuring the response
    , ListPresetsResponse (..)
    , mkListPresetsResponse
    -- ** Response lenses
    , lprrsNextToken
    , lprrsPresets
    , lprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPresets' smart constructor.
data ListPresets = ListPresets'
  { category :: Core.Maybe Core.Text
    -- ^ Optionally, specify a preset category to limit responses to only presets from that category.
  , listBy :: Core.Maybe Types.PresetListBy
    -- ^ Optional. When you request a list of presets, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ Optional. Number of presets, up to twenty, that will be returned at one time
  , nextToken :: Core.Maybe Core.Text
    -- ^ Use this string, provided with the response to a previous request, to request the next batch of presets.
  , order :: Core.Maybe Types.Order
    -- ^ Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPresets' value with any optional fields omitted.
mkListPresets
    :: ListPresets
mkListPresets
  = ListPresets'{category = Core.Nothing, listBy = Core.Nothing,
                 maxResults = Core.Nothing, nextToken = Core.Nothing,
                 order = Core.Nothing}

-- | Optionally, specify a preset category to limit responses to only presets from that category.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpCategory :: Lens.Lens' ListPresets (Core.Maybe Core.Text)
lpCategory = Lens.field @"category"
{-# INLINEABLE lpCategory #-}
{-# DEPRECATED category "Use generic-lens or generic-optics with 'category' instead"  #-}

-- | Optional. When you request a list of presets, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
--
-- /Note:/ Consider using 'listBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpListBy :: Lens.Lens' ListPresets (Core.Maybe Types.PresetListBy)
lpListBy = Lens.field @"listBy"
{-# INLINEABLE lpListBy #-}
{-# DEPRECATED listBy "Use generic-lens or generic-optics with 'listBy' instead"  #-}

-- | Optional. Number of presets, up to twenty, that will be returned at one time
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxResults :: Lens.Lens' ListPresets (Core.Maybe Core.Natural)
lpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Use this string, provided with the response to a previous request, to request the next batch of presets.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListPresets (Core.Maybe Core.Text)
lpNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpOrder :: Lens.Lens' ListPresets (Core.Maybe Types.Order)
lpOrder = Lens.field @"order"
{-# INLINEABLE lpOrder #-}
{-# DEPRECATED order "Use generic-lens or generic-optics with 'order' instead"  #-}

instance Core.ToQuery ListPresets where
        toQuery ListPresets{..}
          = Core.maybe Core.mempty (Core.toQueryPair "category") category
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "listBy") listBy
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "order") order

instance Core.ToHeaders ListPresets where
        toHeaders ListPresets{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListPresets where
        type Rs ListPresets = ListPresetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/2017-08-29/presets",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPresetsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "presets" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPresets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"presets" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListPresetsResponse' smart constructor.
data ListPresetsResponse = ListPresetsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ Use this string to request the next batch of presets.
  , presets :: Core.Maybe [Types.Preset]
    -- ^ List of presets
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListPresetsResponse' value with any optional fields omitted.
mkListPresetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPresetsResponse
mkListPresetsResponse responseStatus
  = ListPresetsResponse'{nextToken = Core.Nothing,
                         presets = Core.Nothing, responseStatus}

-- | Use this string to request the next batch of presets.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsNextToken :: Lens.Lens' ListPresetsResponse (Core.Maybe Core.Text)
lprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | List of presets
--
-- /Note:/ Consider using 'presets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsPresets :: Lens.Lens' ListPresetsResponse (Core.Maybe [Types.Preset])
lprrsPresets = Lens.field @"presets"
{-# INLINEABLE lprrsPresets #-}
{-# DEPRECATED presets "Use generic-lens or generic-optics with 'presets' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' ListPresetsResponse Core.Int
lprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
