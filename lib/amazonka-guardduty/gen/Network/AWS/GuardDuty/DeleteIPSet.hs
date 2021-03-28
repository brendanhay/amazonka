{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DeleteIPSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the IPSet specified by the @ipSetId@ . IPSets are called trusted IP lists in the console user interface.
module Network.AWS.GuardDuty.DeleteIPSet
    (
    -- * Creating a request
      DeleteIPSet (..)
    , mkDeleteIPSet
    -- ** Request lenses
    , dipsDetectorId
    , dipsIpSetId

    -- * Destructuring the response
    , DeleteIPSetResponse (..)
    , mkDeleteIPSetResponse
    -- ** Response lenses
    , dipsrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteIPSet' smart constructor.
data DeleteIPSet = DeleteIPSet'
  { detectorId :: Types.DetectorId
    -- ^ The unique ID of the detector associated with the IPSet.
  , ipSetId :: Core.Text
    -- ^ The unique ID of the IPSet to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIPSet' value with any optional fields omitted.
mkDeleteIPSet
    :: Types.DetectorId -- ^ 'detectorId'
    -> Core.Text -- ^ 'ipSetId'
    -> DeleteIPSet
mkDeleteIPSet detectorId ipSetId
  = DeleteIPSet'{detectorId, ipSetId}

-- | The unique ID of the detector associated with the IPSet.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsDetectorId :: Lens.Lens' DeleteIPSet Types.DetectorId
dipsDetectorId = Lens.field @"detectorId"
{-# INLINEABLE dipsDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The unique ID of the IPSet to delete.
--
-- /Note:/ Consider using 'ipSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsIpSetId :: Lens.Lens' DeleteIPSet Core.Text
dipsIpSetId = Lens.field @"ipSetId"
{-# INLINEABLE dipsIpSetId #-}
{-# DEPRECATED ipSetId "Use generic-lens or generic-optics with 'ipSetId' instead"  #-}

instance Core.ToQuery DeleteIPSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteIPSet where
        toHeaders DeleteIPSet{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteIPSet where
        type Rs DeleteIPSet = DeleteIPSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<> "/ipset/"
                             Core.<> Core.toText ipSetId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteIPSetResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteIPSetResponse' smart constructor.
newtype DeleteIPSetResponse = DeleteIPSetResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIPSetResponse' value with any optional fields omitted.
mkDeleteIPSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteIPSetResponse
mkDeleteIPSetResponse responseStatus
  = DeleteIPSetResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsrrsResponseStatus :: Lens.Lens' DeleteIPSetResponse Core.Int
dipsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dipsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
