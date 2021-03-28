{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.UpdateExpirationForHIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateExpirationForHIT@ operation allows you update the expiration time of a HIT. If you update it to a time in the past, the HIT will be immediately expired. 
module Network.AWS.MechanicalTurk.UpdateExpirationForHIT
    (
    -- * Creating a request
      UpdateExpirationForHIT (..)
    , mkUpdateExpirationForHIT
    -- ** Request lenses
    , uefhitHITId
    , uefhitExpireAt

    -- * Destructuring the response
    , UpdateExpirationForHITResponse (..)
    , mkUpdateExpirationForHITResponse
    -- ** Response lenses
    , uefhitrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateExpirationForHIT' smart constructor.
data UpdateExpirationForHIT = UpdateExpirationForHIT'
  { hITId :: Types.HITId
    -- ^ The HIT to update. 
  , expireAt :: Core.NominalDiffTime
    -- ^ The date and time at which you want the HIT to expire 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateExpirationForHIT' value with any optional fields omitted.
mkUpdateExpirationForHIT
    :: Types.HITId -- ^ 'hITId'
    -> Core.NominalDiffTime -- ^ 'expireAt'
    -> UpdateExpirationForHIT
mkUpdateExpirationForHIT hITId expireAt
  = UpdateExpirationForHIT'{hITId, expireAt}

-- | The HIT to update. 
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uefhitHITId :: Lens.Lens' UpdateExpirationForHIT Types.HITId
uefhitHITId = Lens.field @"hITId"
{-# INLINEABLE uefhitHITId #-}
{-# DEPRECATED hITId "Use generic-lens or generic-optics with 'hITId' instead"  #-}

-- | The date and time at which you want the HIT to expire 
--
-- /Note:/ Consider using 'expireAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uefhitExpireAt :: Lens.Lens' UpdateExpirationForHIT Core.NominalDiffTime
uefhitExpireAt = Lens.field @"expireAt"
{-# INLINEABLE uefhitExpireAt #-}
{-# DEPRECATED expireAt "Use generic-lens or generic-optics with 'expireAt' instead"  #-}

instance Core.ToQuery UpdateExpirationForHIT where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateExpirationForHIT where
        toHeaders UpdateExpirationForHIT{..}
          = Core.pure
              ("X-Amz-Target",
               "MTurkRequesterServiceV20170117.UpdateExpirationForHIT")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateExpirationForHIT where
        toJSON UpdateExpirationForHIT{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("HITId" Core..= hITId),
                  Core.Just ("ExpireAt" Core..= expireAt)])

instance Core.AWSRequest UpdateExpirationForHIT where
        type Rs UpdateExpirationForHIT = UpdateExpirationForHITResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateExpirationForHITResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateExpirationForHITResponse' smart constructor.
newtype UpdateExpirationForHITResponse = UpdateExpirationForHITResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateExpirationForHITResponse' value with any optional fields omitted.
mkUpdateExpirationForHITResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateExpirationForHITResponse
mkUpdateExpirationForHITResponse responseStatus
  = UpdateExpirationForHITResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uefhitrrsResponseStatus :: Lens.Lens' UpdateExpirationForHITResponse Core.Int
uefhitrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uefhitrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
