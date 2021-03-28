{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the filter specified by the filter name.
module Network.AWS.GuardDuty.GetFilter
    (
    -- * Creating a request
      GetFilter (..)
    , mkGetFilter
    -- ** Request lenses
    , gDetectorId
    , gFilterName

    -- * Destructuring the response
    , GetFilterResponse (..)
    , mkGetFilterResponse
    -- ** Response lenses
    , gfrrsName
    , gfrrsAction
    , gfrrsFindingCriteria
    , gfrrsDescription
    , gfrrsRank
    , gfrrsTags
    , gfrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFilter' smart constructor.
data GetFilter = GetFilter'
  { detectorId :: Types.DetectorId
    -- ^ The unique ID of the detector that the filter is associated with.
  , filterName :: Core.Text
    -- ^ The name of the filter you want to get.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFilter' value with any optional fields omitted.
mkGetFilter
    :: Types.DetectorId -- ^ 'detectorId'
    -> Core.Text -- ^ 'filterName'
    -> GetFilter
mkGetFilter detectorId filterName
  = GetFilter'{detectorId, filterName}

-- | The unique ID of the detector that the filter is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gDetectorId :: Lens.Lens' GetFilter Types.DetectorId
gDetectorId = Lens.field @"detectorId"
{-# INLINEABLE gDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The name of the filter you want to get.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gFilterName :: Lens.Lens' GetFilter Core.Text
gFilterName = Lens.field @"filterName"
{-# INLINEABLE gFilterName #-}
{-# DEPRECATED filterName "Use generic-lens or generic-optics with 'filterName' instead"  #-}

instance Core.ToQuery GetFilter where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetFilter where
        toHeaders GetFilter{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetFilter where
        type Rs GetFilter = GetFilterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<> "/filter/"
                             Core.<> Core.toText filterName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetFilterResponse' Core.<$>
                   (x Core..: "name") Core.<*> x Core..: "action" Core.<*>
                     x Core..: "findingCriteria"
                     Core.<*> x Core..:? "description"
                     Core.<*> x Core..:? "rank"
                     Core.<*> x Core..:? "tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetFilterResponse' smart constructor.
data GetFilterResponse = GetFilterResponse'
  { name :: Types.FilterName
    -- ^ The name of the filter.
  , action :: Types.FilterAction
    -- ^ Specifies the action that is to be applied to the findings that match the filter.
  , findingCriteria :: Types.FindingCriteria
    -- ^ Represents the criteria to be used in the filter for querying findings.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the filter.
  , rank :: Core.Maybe Core.Natural
    -- ^ Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags of the filter resource.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFilterResponse' value with any optional fields omitted.
mkGetFilterResponse
    :: Types.FilterName -- ^ 'name'
    -> Types.FilterAction -- ^ 'action'
    -> Types.FindingCriteria -- ^ 'findingCriteria'
    -> Core.Int -- ^ 'responseStatus'
    -> GetFilterResponse
mkGetFilterResponse name action findingCriteria responseStatus
  = GetFilterResponse'{name, action, findingCriteria,
                       description = Core.Nothing, rank = Core.Nothing,
                       tags = Core.Nothing, responseStatus}

-- | The name of the filter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsName :: Lens.Lens' GetFilterResponse Types.FilterName
gfrrsName = Lens.field @"name"
{-# INLINEABLE gfrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Specifies the action that is to be applied to the findings that match the filter.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsAction :: Lens.Lens' GetFilterResponse Types.FilterAction
gfrrsAction = Lens.field @"action"
{-# INLINEABLE gfrrsAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | Represents the criteria to be used in the filter for querying findings.
--
-- /Note:/ Consider using 'findingCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsFindingCriteria :: Lens.Lens' GetFilterResponse Types.FindingCriteria
gfrrsFindingCriteria = Lens.field @"findingCriteria"
{-# INLINEABLE gfrrsFindingCriteria #-}
{-# DEPRECATED findingCriteria "Use generic-lens or generic-optics with 'findingCriteria' instead"  #-}

-- | The description of the filter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsDescription :: Lens.Lens' GetFilterResponse (Core.Maybe Types.Description)
gfrrsDescription = Lens.field @"description"
{-# INLINEABLE gfrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
--
-- /Note:/ Consider using 'rank' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsRank :: Lens.Lens' GetFilterResponse (Core.Maybe Core.Natural)
gfrrsRank = Lens.field @"rank"
{-# INLINEABLE gfrrsRank #-}
{-# DEPRECATED rank "Use generic-lens or generic-optics with 'rank' instead"  #-}

-- | The tags of the filter resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsTags :: Lens.Lens' GetFilterResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
gfrrsTags = Lens.field @"tags"
{-# INLINEABLE gfrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsResponseStatus :: Lens.Lens' GetFilterResponse Core.Int
gfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
