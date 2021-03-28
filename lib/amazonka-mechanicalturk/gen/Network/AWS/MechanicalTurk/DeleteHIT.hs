{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.DeleteHIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteHIT@ operation is used to delete HIT that is no longer needed. Only the Requester who created the HIT can delete it. 
--
-- You can only dispose of HITs that are in the @Reviewable@ state, with all of their submitted assignments already either approved or rejected. If you call the DeleteHIT operation on a HIT that is not in the @Reviewable@ state (for example, that has not expired, or still has active assignments), or on a HIT that is Reviewable but without all of its submitted assignments already approved or rejected, the service will return an error. 
module Network.AWS.MechanicalTurk.DeleteHIT
    (
    -- * Creating a request
      DeleteHIT (..)
    , mkDeleteHIT
    -- ** Request lenses
    , dhitHITId

    -- * Destructuring the response
    , DeleteHITResponse (..)
    , mkDeleteHITResponse
    -- ** Response lenses
    , dhitrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteHIT' smart constructor.
newtype DeleteHIT = DeleteHIT'
  { hITId :: Types.HITId
    -- ^ The ID of the HIT to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHIT' value with any optional fields omitted.
mkDeleteHIT
    :: Types.HITId -- ^ 'hITId'
    -> DeleteHIT
mkDeleteHIT hITId = DeleteHIT'{hITId}

-- | The ID of the HIT to be deleted.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhitHITId :: Lens.Lens' DeleteHIT Types.HITId
dhitHITId = Lens.field @"hITId"
{-# INLINEABLE dhitHITId #-}
{-# DEPRECATED hITId "Use generic-lens or generic-optics with 'hITId' instead"  #-}

instance Core.ToQuery DeleteHIT where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteHIT where
        toHeaders DeleteHIT{..}
          = Core.pure
              ("X-Amz-Target", "MTurkRequesterServiceV20170117.DeleteHIT")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteHIT where
        toJSON DeleteHIT{..}
          = Core.object (Core.catMaybes [Core.Just ("HITId" Core..= hITId)])

instance Core.AWSRequest DeleteHIT where
        type Rs DeleteHIT = DeleteHITResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteHITResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteHITResponse' smart constructor.
newtype DeleteHITResponse = DeleteHITResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHITResponse' value with any optional fields omitted.
mkDeleteHITResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteHITResponse
mkDeleteHITResponse responseStatus
  = DeleteHITResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhitrrsResponseStatus :: Lens.Lens' DeleteHITResponse Core.Int
dhitrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dhitrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
