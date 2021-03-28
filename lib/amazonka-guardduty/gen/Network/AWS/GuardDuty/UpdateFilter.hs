{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UpdateFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the filter specified by the filter name.
module Network.AWS.GuardDuty.UpdateFilter
    (
    -- * Creating a request
      UpdateFilter (..)
    , mkUpdateFilter
    -- ** Request lenses
    , ufDetectorId
    , ufFilterName
    , ufAction
    , ufDescription
    , ufFindingCriteria
    , ufRank

    -- * Destructuring the response
    , UpdateFilterResponse (..)
    , mkUpdateFilterResponse
    -- ** Response lenses
    , ufrrsName
    , ufrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateFilter' smart constructor.
data UpdateFilter = UpdateFilter'
  { detectorId :: Types.DetectorId
    -- ^ The unique ID of the detector that specifies the GuardDuty service where you want to update a filter.
  , filterName :: Core.Text
    -- ^ The name of the filter.
  , action :: Core.Maybe Types.FilterAction
    -- ^ Specifies the action that is to be applied to the findings that match the filter.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the filter.
  , findingCriteria :: Core.Maybe Types.FindingCriteria
    -- ^ Represents the criteria to be used in the filter for querying findings.
  , rank :: Core.Maybe Core.Natural
    -- ^ Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFilter' value with any optional fields omitted.
mkUpdateFilter
    :: Types.DetectorId -- ^ 'detectorId'
    -> Core.Text -- ^ 'filterName'
    -> UpdateFilter
mkUpdateFilter detectorId filterName
  = UpdateFilter'{detectorId, filterName, action = Core.Nothing,
                  description = Core.Nothing, findingCriteria = Core.Nothing,
                  rank = Core.Nothing}

-- | The unique ID of the detector that specifies the GuardDuty service where you want to update a filter.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufDetectorId :: Lens.Lens' UpdateFilter Types.DetectorId
ufDetectorId = Lens.field @"detectorId"
{-# INLINEABLE ufDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The name of the filter.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufFilterName :: Lens.Lens' UpdateFilter Core.Text
ufFilterName = Lens.field @"filterName"
{-# INLINEABLE ufFilterName #-}
{-# DEPRECATED filterName "Use generic-lens or generic-optics with 'filterName' instead"  #-}

-- | Specifies the action that is to be applied to the findings that match the filter.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufAction :: Lens.Lens' UpdateFilter (Core.Maybe Types.FilterAction)
ufAction = Lens.field @"action"
{-# INLINEABLE ufAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The description of the filter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufDescription :: Lens.Lens' UpdateFilter (Core.Maybe Types.Description)
ufDescription = Lens.field @"description"
{-# INLINEABLE ufDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Represents the criteria to be used in the filter for querying findings.
--
-- /Note:/ Consider using 'findingCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufFindingCriteria :: Lens.Lens' UpdateFilter (Core.Maybe Types.FindingCriteria)
ufFindingCriteria = Lens.field @"findingCriteria"
{-# INLINEABLE ufFindingCriteria #-}
{-# DEPRECATED findingCriteria "Use generic-lens or generic-optics with 'findingCriteria' instead"  #-}

-- | Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
--
-- /Note:/ Consider using 'rank' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufRank :: Lens.Lens' UpdateFilter (Core.Maybe Core.Natural)
ufRank = Lens.field @"rank"
{-# INLINEABLE ufRank #-}
{-# DEPRECATED rank "Use generic-lens or generic-optics with 'rank' instead"  #-}

instance Core.ToQuery UpdateFilter where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateFilter where
        toHeaders UpdateFilter{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateFilter where
        toJSON UpdateFilter{..}
          = Core.object
              (Core.catMaybes
                 [("action" Core..=) Core.<$> action,
                  ("description" Core..=) Core.<$> description,
                  ("findingCriteria" Core..=) Core.<$> findingCriteria,
                  ("rank" Core..=) Core.<$> rank])

instance Core.AWSRequest UpdateFilter where
        type Rs UpdateFilter = UpdateFilterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<> "/filter/"
                             Core.<> Core.toText filterName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateFilterResponse' Core.<$>
                   (x Core..: "name") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateFilterResponse' smart constructor.
data UpdateFilterResponse = UpdateFilterResponse'
  { name :: Types.FilterName
    -- ^ The name of the filter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFilterResponse' value with any optional fields omitted.
mkUpdateFilterResponse
    :: Types.FilterName -- ^ 'name'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateFilterResponse
mkUpdateFilterResponse name responseStatus
  = UpdateFilterResponse'{name, responseStatus}

-- | The name of the filter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufrrsName :: Lens.Lens' UpdateFilterResponse Types.FilterName
ufrrsName = Lens.field @"name"
{-# INLINEABLE ufrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufrrsResponseStatus :: Lens.Lens' UpdateFilterResponse Core.Int
ufrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ufrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
