{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.UpdateHITReviewStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateHITReviewStatus@ operation updates the status of a HIT. If the status is Reviewable, this operation can update the status to Reviewing, or it can revert a Reviewing HIT back to the Reviewable status. 
module Network.AWS.MechanicalTurk.UpdateHITReviewStatus
    (
    -- * Creating a request
      UpdateHITReviewStatus (..)
    , mkUpdateHITReviewStatus
    -- ** Request lenses
    , uhitrsHITId
    , uhitrsRevert

    -- * Destructuring the response
    , UpdateHITReviewStatusResponse (..)
    , mkUpdateHITReviewStatusResponse
    -- ** Response lenses
    , uhitrsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateHITReviewStatus' smart constructor.
data UpdateHITReviewStatus = UpdateHITReviewStatus'
  { hITId :: Types.HITId
    -- ^ The ID of the HIT to update. 
  , revert :: Core.Maybe Core.Bool
    -- ^ Specifies how to update the HIT status. Default is @False@ . 
--
--
--     * Setting this to false will only transition a HIT from @Reviewable@ to @Reviewing@ 
--
--
--     * Setting this to true will only transition a HIT from @Reviewing@ to @Reviewable@ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateHITReviewStatus' value with any optional fields omitted.
mkUpdateHITReviewStatus
    :: Types.HITId -- ^ 'hITId'
    -> UpdateHITReviewStatus
mkUpdateHITReviewStatus hITId
  = UpdateHITReviewStatus'{hITId, revert = Core.Nothing}

-- | The ID of the HIT to update. 
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhitrsHITId :: Lens.Lens' UpdateHITReviewStatus Types.HITId
uhitrsHITId = Lens.field @"hITId"
{-# INLINEABLE uhitrsHITId #-}
{-# DEPRECATED hITId "Use generic-lens or generic-optics with 'hITId' instead"  #-}

-- | Specifies how to update the HIT status. Default is @False@ . 
--
--
--     * Setting this to false will only transition a HIT from @Reviewable@ to @Reviewing@ 
--
--
--     * Setting this to true will only transition a HIT from @Reviewing@ to @Reviewable@ 
--
--
--
-- /Note:/ Consider using 'revert' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhitrsRevert :: Lens.Lens' UpdateHITReviewStatus (Core.Maybe Core.Bool)
uhitrsRevert = Lens.field @"revert"
{-# INLINEABLE uhitrsRevert #-}
{-# DEPRECATED revert "Use generic-lens or generic-optics with 'revert' instead"  #-}

instance Core.ToQuery UpdateHITReviewStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateHITReviewStatus where
        toHeaders UpdateHITReviewStatus{..}
          = Core.pure
              ("X-Amz-Target",
               "MTurkRequesterServiceV20170117.UpdateHITReviewStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateHITReviewStatus where
        toJSON UpdateHITReviewStatus{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("HITId" Core..= hITId),
                  ("Revert" Core..=) Core.<$> revert])

instance Core.AWSRequest UpdateHITReviewStatus where
        type Rs UpdateHITReviewStatus = UpdateHITReviewStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateHITReviewStatusResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateHITReviewStatusResponse' smart constructor.
newtype UpdateHITReviewStatusResponse = UpdateHITReviewStatusResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateHITReviewStatusResponse' value with any optional fields omitted.
mkUpdateHITReviewStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateHITReviewStatusResponse
mkUpdateHITReviewStatusResponse responseStatus
  = UpdateHITReviewStatusResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhitrsrrsResponseStatus :: Lens.Lens' UpdateHITReviewStatusResponse Core.Int
uhitrsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uhitrsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
