{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DeleteDetector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon GuardDuty detector that is specified by the detector ID.
module Network.AWS.GuardDuty.DeleteDetector
    (
    -- * Creating a request
      DeleteDetector (..)
    , mkDeleteDetector
    -- ** Request lenses
    , ddDetectorId

    -- * Destructuring the response
    , DeleteDetectorResponse (..)
    , mkDeleteDetectorResponse
    -- ** Response lenses
    , ddrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDetector' smart constructor.
newtype DeleteDetector = DeleteDetector'
  { detectorId :: Types.DetectorId
    -- ^ The unique ID of the detector that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDetector' value with any optional fields omitted.
mkDeleteDetector
    :: Types.DetectorId -- ^ 'detectorId'
    -> DeleteDetector
mkDeleteDetector detectorId = DeleteDetector'{detectorId}

-- | The unique ID of the detector that you want to delete.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDetectorId :: Lens.Lens' DeleteDetector Types.DetectorId
ddDetectorId = Lens.field @"detectorId"
{-# INLINEABLE ddDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

instance Core.ToQuery DeleteDetector where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDetector where
        toHeaders DeleteDetector{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteDetector where
        type Rs DeleteDetector = DeleteDetectorResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/detector/" Core.<> Core.toText detectorId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteDetectorResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDetectorResponse' smart constructor.
newtype DeleteDetectorResponse = DeleteDetectorResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDetectorResponse' value with any optional fields omitted.
mkDeleteDetectorResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDetectorResponse
mkDeleteDetectorResponse responseStatus
  = DeleteDetectorResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DeleteDetectorResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
