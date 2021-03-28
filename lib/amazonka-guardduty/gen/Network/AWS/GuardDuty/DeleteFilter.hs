{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DeleteFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the filter specified by the filter name.
module Network.AWS.GuardDuty.DeleteFilter
    (
    -- * Creating a request
      DeleteFilter (..)
    , mkDeleteFilter
    -- ** Request lenses
    , dfDetectorId
    , dfFilterName

    -- * Destructuring the response
    , DeleteFilterResponse (..)
    , mkDeleteFilterResponse
    -- ** Response lenses
    , dfrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFilter' smart constructor.
data DeleteFilter = DeleteFilter'
  { detectorId :: Types.DetectorId
    -- ^ The unique ID of the detector that the filter is associated with.
  , filterName :: Core.Text
    -- ^ The name of the filter that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFilter' value with any optional fields omitted.
mkDeleteFilter
    :: Types.DetectorId -- ^ 'detectorId'
    -> Core.Text -- ^ 'filterName'
    -> DeleteFilter
mkDeleteFilter detectorId filterName
  = DeleteFilter'{detectorId, filterName}

-- | The unique ID of the detector that the filter is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfDetectorId :: Lens.Lens' DeleteFilter Types.DetectorId
dfDetectorId = Lens.field @"detectorId"
{-# INLINEABLE dfDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The name of the filter that you want to delete.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFilterName :: Lens.Lens' DeleteFilter Core.Text
dfFilterName = Lens.field @"filterName"
{-# INLINEABLE dfFilterName #-}
{-# DEPRECATED filterName "Use generic-lens or generic-optics with 'filterName' instead"  #-}

instance Core.ToQuery DeleteFilter where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteFilter where
        toHeaders DeleteFilter{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteFilter where
        type Rs DeleteFilter = DeleteFilterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<> "/filter/"
                             Core.<> Core.toText filterName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteFilterResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteFilterResponse' smart constructor.
newtype DeleteFilterResponse = DeleteFilterResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFilterResponse' value with any optional fields omitted.
mkDeleteFilterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteFilterResponse
mkDeleteFilterResponse responseStatus
  = DeleteFilterResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsResponseStatus :: Lens.Lens' DeleteFilterResponse Core.Int
dfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
