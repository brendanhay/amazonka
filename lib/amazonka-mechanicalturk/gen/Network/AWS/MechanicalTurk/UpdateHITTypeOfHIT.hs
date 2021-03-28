{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.UpdateHITTypeOfHIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateHITTypeOfHIT@ operation allows you to change the HITType properties of a HIT. This operation disassociates the HIT from its old HITType properties and associates it with the new HITType properties. The HIT takes on the properties of the new HITType in place of the old ones. 
module Network.AWS.MechanicalTurk.UpdateHITTypeOfHIT
    (
    -- * Creating a request
      UpdateHITTypeOfHIT (..)
    , mkUpdateHITTypeOfHIT
    -- ** Request lenses
    , uhittohitHITId
    , uhittohitHITTypeId

    -- * Destructuring the response
    , UpdateHITTypeOfHITResponse (..)
    , mkUpdateHITTypeOfHITResponse
    -- ** Response lenses
    , uhittohitrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateHITTypeOfHIT' smart constructor.
data UpdateHITTypeOfHIT = UpdateHITTypeOfHIT'
  { hITId :: Types.EntityId
    -- ^ The HIT to update.
  , hITTypeId :: Types.EntityId
    -- ^ The ID of the new HIT type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateHITTypeOfHIT' value with any optional fields omitted.
mkUpdateHITTypeOfHIT
    :: Types.EntityId -- ^ 'hITId'
    -> Types.EntityId -- ^ 'hITTypeId'
    -> UpdateHITTypeOfHIT
mkUpdateHITTypeOfHIT hITId hITTypeId
  = UpdateHITTypeOfHIT'{hITId, hITTypeId}

-- | The HIT to update.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhittohitHITId :: Lens.Lens' UpdateHITTypeOfHIT Types.EntityId
uhittohitHITId = Lens.field @"hITId"
{-# INLINEABLE uhittohitHITId #-}
{-# DEPRECATED hITId "Use generic-lens or generic-optics with 'hITId' instead"  #-}

-- | The ID of the new HIT type.
--
-- /Note:/ Consider using 'hITTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhittohitHITTypeId :: Lens.Lens' UpdateHITTypeOfHIT Types.EntityId
uhittohitHITTypeId = Lens.field @"hITTypeId"
{-# INLINEABLE uhittohitHITTypeId #-}
{-# DEPRECATED hITTypeId "Use generic-lens or generic-optics with 'hITTypeId' instead"  #-}

instance Core.ToQuery UpdateHITTypeOfHIT where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateHITTypeOfHIT where
        toHeaders UpdateHITTypeOfHIT{..}
          = Core.pure
              ("X-Amz-Target",
               "MTurkRequesterServiceV20170117.UpdateHITTypeOfHIT")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateHITTypeOfHIT where
        toJSON UpdateHITTypeOfHIT{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("HITId" Core..= hITId),
                  Core.Just ("HITTypeId" Core..= hITTypeId)])

instance Core.AWSRequest UpdateHITTypeOfHIT where
        type Rs UpdateHITTypeOfHIT = UpdateHITTypeOfHITResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateHITTypeOfHITResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateHITTypeOfHITResponse' smart constructor.
newtype UpdateHITTypeOfHITResponse = UpdateHITTypeOfHITResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateHITTypeOfHITResponse' value with any optional fields omitted.
mkUpdateHITTypeOfHITResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateHITTypeOfHITResponse
mkUpdateHITTypeOfHITResponse responseStatus
  = UpdateHITTypeOfHITResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uhittohitrrsResponseStatus :: Lens.Lens' UpdateHITTypeOfHITResponse Core.Int
uhittohitrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uhittohitrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
