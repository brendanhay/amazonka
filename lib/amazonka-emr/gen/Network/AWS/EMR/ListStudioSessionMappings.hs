{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListStudioSessionMappings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all user or group session mappings for the EMR Studio specified by @StudioId@ .
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListStudioSessionMappings
    (
    -- * Creating a request
      ListStudioSessionMappings (..)
    , mkListStudioSessionMappings
    -- ** Request lenses
    , lssmIdentityType
    , lssmMarker
    , lssmStudioId

    -- * Destructuring the response
    , ListStudioSessionMappingsResponse (..)
    , mkListStudioSessionMappingsResponse
    -- ** Response lenses
    , lssmrrsMarker
    , lssmrrsSessionMappings
    , lssmrrsResponseStatus
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListStudioSessionMappings' smart constructor.
data ListStudioSessionMappings = ListStudioSessionMappings'
  { identityType :: Core.Maybe Types.IdentityType
    -- ^ Specifies whether to return session mappings for users or groups. If not specified, the results include session mapping details for both users and groups.
  , marker :: Core.Maybe Types.Marker
    -- ^ The pagination token that indicates the set of results to retrieve.
  , studioId :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ The ID of the Amazon EMR Studio.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStudioSessionMappings' value with any optional fields omitted.
mkListStudioSessionMappings
    :: ListStudioSessionMappings
mkListStudioSessionMappings
  = ListStudioSessionMappings'{identityType = Core.Nothing,
                               marker = Core.Nothing, studioId = Core.Nothing}

-- | Specifies whether to return session mappings for users or groups. If not specified, the results include session mapping details for both users and groups.
--
-- /Note:/ Consider using 'identityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssmIdentityType :: Lens.Lens' ListStudioSessionMappings (Core.Maybe Types.IdentityType)
lssmIdentityType = Lens.field @"identityType"
{-# INLINEABLE lssmIdentityType #-}
{-# DEPRECATED identityType "Use generic-lens or generic-optics with 'identityType' instead"  #-}

-- | The pagination token that indicates the set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssmMarker :: Lens.Lens' ListStudioSessionMappings (Core.Maybe Types.Marker)
lssmMarker = Lens.field @"marker"
{-# INLINEABLE lssmMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The ID of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssmStudioId :: Lens.Lens' ListStudioSessionMappings (Core.Maybe Types.XmlStringMaxLen256)
lssmStudioId = Lens.field @"studioId"
{-# INLINEABLE lssmStudioId #-}
{-# DEPRECATED studioId "Use generic-lens or generic-optics with 'studioId' instead"  #-}

instance Core.ToQuery ListStudioSessionMappings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListStudioSessionMappings where
        toHeaders ListStudioSessionMappings{..}
          = Core.pure
              ("X-Amz-Target", "ElasticMapReduce.ListStudioSessionMappings")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListStudioSessionMappings where
        toJSON ListStudioSessionMappings{..}
          = Core.object
              (Core.catMaybes
                 [("IdentityType" Core..=) Core.<$> identityType,
                  ("Marker" Core..=) Core.<$> marker,
                  ("StudioId" Core..=) Core.<$> studioId])

instance Core.AWSRequest ListStudioSessionMappings where
        type Rs ListStudioSessionMappings =
             ListStudioSessionMappingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListStudioSessionMappingsResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*> x Core..:? "SessionMappings"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListStudioSessionMappings where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"sessionMappings" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkListStudioSessionMappingsResponse' smart constructor.
data ListStudioSessionMappingsResponse = ListStudioSessionMappingsResponse'
  { marker :: Core.Maybe Types.Marker
    -- ^ The pagination token that indicates the next set of results to retrieve.
  , sessionMappings :: Core.Maybe [Types.SessionMappingSummary]
    -- ^ A list of session mapping summary objects. Each object includes session mapping details such as creation time, identity type (user or group), and Studio ID.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListStudioSessionMappingsResponse' value with any optional fields omitted.
mkListStudioSessionMappingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListStudioSessionMappingsResponse
mkListStudioSessionMappingsResponse responseStatus
  = ListStudioSessionMappingsResponse'{marker = Core.Nothing,
                                       sessionMappings = Core.Nothing, responseStatus}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssmrrsMarker :: Lens.Lens' ListStudioSessionMappingsResponse (Core.Maybe Types.Marker)
lssmrrsMarker = Lens.field @"marker"
{-# INLINEABLE lssmrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | A list of session mapping summary objects. Each object includes session mapping details such as creation time, identity type (user or group), and Studio ID.
--
-- /Note:/ Consider using 'sessionMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssmrrsSessionMappings :: Lens.Lens' ListStudioSessionMappingsResponse (Core.Maybe [Types.SessionMappingSummary])
lssmrrsSessionMappings = Lens.field @"sessionMappings"
{-# INLINEABLE lssmrrsSessionMappings #-}
{-# DEPRECATED sessionMappings "Use generic-lens or generic-optics with 'sessionMappings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssmrrsResponseStatus :: Lens.Lens' ListStudioSessionMappingsResponse Core.Int
lssmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lssmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
