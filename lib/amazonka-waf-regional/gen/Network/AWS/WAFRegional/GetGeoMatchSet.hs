{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetGeoMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'GeoMatchSet' that is specified by @GeoMatchSetId@ .
module Network.AWS.WAFRegional.GetGeoMatchSet
  ( -- * Creating a request
    GetGeoMatchSet (..),
    mkGetGeoMatchSet,

    -- ** Request lenses
    ggmsGeoMatchSetId,

    -- * Destructuring the response
    GetGeoMatchSetResponse (..),
    mkGetGeoMatchSetResponse,

    -- ** Response lenses
    ggmsrrsGeoMatchSet,
    ggmsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkGetGeoMatchSet' smart constructor.
newtype GetGeoMatchSet = GetGeoMatchSet'
  { -- | The @GeoMatchSetId@ of the 'GeoMatchSet' that you want to get. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
    geoMatchSetId :: Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetGeoMatchSet' value with any optional fields omitted.
mkGetGeoMatchSet ::
  -- | 'geoMatchSetId'
  Types.ResourceId ->
  GetGeoMatchSet
mkGetGeoMatchSet geoMatchSetId = GetGeoMatchSet' {geoMatchSetId}

-- | The @GeoMatchSetId@ of the 'GeoMatchSet' that you want to get. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
--
-- /Note:/ Consider using 'geoMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggmsGeoMatchSetId :: Lens.Lens' GetGeoMatchSet Types.ResourceId
ggmsGeoMatchSetId = Lens.field @"geoMatchSetId"
{-# DEPRECATED ggmsGeoMatchSetId "Use generic-lens or generic-optics with 'geoMatchSetId' instead." #-}

instance Core.FromJSON GetGeoMatchSet where
  toJSON GetGeoMatchSet {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GeoMatchSetId" Core..= geoMatchSetId)]
      )

instance Core.AWSRequest GetGeoMatchSet where
  type Rs GetGeoMatchSet = GetGeoMatchSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSWAF_Regional_20161128.GetGeoMatchSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGeoMatchSetResponse'
            Core.<$> (x Core..:? "GeoMatchSet") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetGeoMatchSetResponse' smart constructor.
data GetGeoMatchSetResponse = GetGeoMatchSetResponse'
  { -- | Information about the 'GeoMatchSet' that you specified in the @GetGeoMatchSet@ request. This includes the @Type@ , which for a @GeoMatchContraint@ is always @Country@ , as well as the @Value@ , which is the identifier for a specific country.
    geoMatchSet :: Core.Maybe Types.GeoMatchSet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGeoMatchSetResponse' value with any optional fields omitted.
mkGetGeoMatchSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetGeoMatchSetResponse
mkGetGeoMatchSetResponse responseStatus =
  GetGeoMatchSetResponse'
    { geoMatchSet = Core.Nothing,
      responseStatus
    }

-- | Information about the 'GeoMatchSet' that you specified in the @GetGeoMatchSet@ request. This includes the @Type@ , which for a @GeoMatchContraint@ is always @Country@ , as well as the @Value@ , which is the identifier for a specific country.
--
-- /Note:/ Consider using 'geoMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggmsrrsGeoMatchSet :: Lens.Lens' GetGeoMatchSetResponse (Core.Maybe Types.GeoMatchSet)
ggmsrrsGeoMatchSet = Lens.field @"geoMatchSet"
{-# DEPRECATED ggmsrrsGeoMatchSet "Use generic-lens or generic-optics with 'geoMatchSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggmsrrsResponseStatus :: Lens.Lens' GetGeoMatchSetResponse Core.Int
ggmsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ggmsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
