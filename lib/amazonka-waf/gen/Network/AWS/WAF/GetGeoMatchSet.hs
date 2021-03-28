{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetGeoMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'GeoMatchSet' that is specified by @GeoMatchSetId@ .
module Network.AWS.WAF.GetGeoMatchSet
    (
    -- * Creating a request
      GetGeoMatchSet (..)
    , mkGetGeoMatchSet
    -- ** Request lenses
    , ggmsGeoMatchSetId

    -- * Destructuring the response
    , GetGeoMatchSetResponse (..)
    , mkGetGeoMatchSetResponse
    -- ** Response lenses
    , ggmsrrsGeoMatchSet
    , ggmsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkGetGeoMatchSet' smart constructor.
newtype GetGeoMatchSet = GetGeoMatchSet'
  { geoMatchSetId :: Types.ResourceId
    -- ^ The @GeoMatchSetId@ of the 'GeoMatchSet' that you want to get. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetGeoMatchSet' value with any optional fields omitted.
mkGetGeoMatchSet
    :: Types.ResourceId -- ^ 'geoMatchSetId'
    -> GetGeoMatchSet
mkGetGeoMatchSet geoMatchSetId = GetGeoMatchSet'{geoMatchSetId}

-- | The @GeoMatchSetId@ of the 'GeoMatchSet' that you want to get. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
--
-- /Note:/ Consider using 'geoMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggmsGeoMatchSetId :: Lens.Lens' GetGeoMatchSet Types.ResourceId
ggmsGeoMatchSetId = Lens.field @"geoMatchSetId"
{-# INLINEABLE ggmsGeoMatchSetId #-}
{-# DEPRECATED geoMatchSetId "Use generic-lens or generic-optics with 'geoMatchSetId' instead"  #-}

instance Core.ToQuery GetGeoMatchSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetGeoMatchSet where
        toHeaders GetGeoMatchSet{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_20150824.GetGeoMatchSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetGeoMatchSet where
        toJSON GetGeoMatchSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GeoMatchSetId" Core..= geoMatchSetId)])

instance Core.AWSRequest GetGeoMatchSet where
        type Rs GetGeoMatchSet = GetGeoMatchSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetGeoMatchSetResponse' Core.<$>
                   (x Core..:? "GeoMatchSet") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetGeoMatchSetResponse' smart constructor.
data GetGeoMatchSetResponse = GetGeoMatchSetResponse'
  { geoMatchSet :: Core.Maybe Types.GeoMatchSet
    -- ^ Information about the 'GeoMatchSet' that you specified in the @GetGeoMatchSet@ request. This includes the @Type@ , which for a @GeoMatchContraint@ is always @Country@ , as well as the @Value@ , which is the identifier for a specific country.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGeoMatchSetResponse' value with any optional fields omitted.
mkGetGeoMatchSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetGeoMatchSetResponse
mkGetGeoMatchSetResponse responseStatus
  = GetGeoMatchSetResponse'{geoMatchSet = Core.Nothing,
                            responseStatus}

-- | Information about the 'GeoMatchSet' that you specified in the @GetGeoMatchSet@ request. This includes the @Type@ , which for a @GeoMatchContraint@ is always @Country@ , as well as the @Value@ , which is the identifier for a specific country.
--
-- /Note:/ Consider using 'geoMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggmsrrsGeoMatchSet :: Lens.Lens' GetGeoMatchSetResponse (Core.Maybe Types.GeoMatchSet)
ggmsrrsGeoMatchSet = Lens.field @"geoMatchSet"
{-# INLINEABLE ggmsrrsGeoMatchSet #-}
{-# DEPRECATED geoMatchSet "Use generic-lens or generic-optics with 'geoMatchSet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggmsrrsResponseStatus :: Lens.Lens' GetGeoMatchSetResponse Core.Int
ggmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ggmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
