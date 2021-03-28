{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.CancelHandshake
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a handshake. Canceling a handshake sets the handshake state to @CANCELED@ .
--
-- This operation can be called only from the account that originated the handshake. The recipient of the handshake can't cancel it, but can use 'DeclineHandshake' instead. After a handshake is canceled, the recipient can no longer respond to that handshake.
-- After you cancel a handshake, it continues to appear in the results of relevant APIs for only 30 days. After that, it's deleted.
module Network.AWS.Organizations.CancelHandshake
    (
    -- * Creating a request
      CancelHandshake (..)
    , mkCancelHandshake
    -- ** Request lenses
    , chHandshakeId

    -- * Destructuring the response
    , CancelHandshakeResponse (..)
    , mkCancelHandshakeResponse
    -- ** Response lenses
    , chrrsHandshake
    , chrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelHandshake' smart constructor.
newtype CancelHandshake = CancelHandshake'
  { handshakeId :: Types.HandshakeId
    -- ^ The unique identifier (ID) of the handshake that you want to cancel. You can get the ID from the 'ListHandshakesForOrganization' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelHandshake' value with any optional fields omitted.
mkCancelHandshake
    :: Types.HandshakeId -- ^ 'handshakeId'
    -> CancelHandshake
mkCancelHandshake handshakeId = CancelHandshake'{handshakeId}

-- | The unique identifier (ID) of the handshake that you want to cancel. You can get the ID from the 'ListHandshakesForOrganization' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'handshakeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chHandshakeId :: Lens.Lens' CancelHandshake Types.HandshakeId
chHandshakeId = Lens.field @"handshakeId"
{-# INLINEABLE chHandshakeId #-}
{-# DEPRECATED handshakeId "Use generic-lens or generic-optics with 'handshakeId' instead"  #-}

instance Core.ToQuery CancelHandshake where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CancelHandshake where
        toHeaders CancelHandshake{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.CancelHandshake")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CancelHandshake where
        toJSON CancelHandshake{..}
          = Core.object
              (Core.catMaybes [Core.Just ("HandshakeId" Core..= handshakeId)])

instance Core.AWSRequest CancelHandshake where
        type Rs CancelHandshake = CancelHandshakeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CancelHandshakeResponse' Core.<$>
                   (x Core..:? "Handshake") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCancelHandshakeResponse' smart constructor.
data CancelHandshakeResponse = CancelHandshakeResponse'
  { handshake :: Core.Maybe Types.Handshake
    -- ^ A structure that contains details about the handshake that you canceled.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CancelHandshakeResponse' value with any optional fields omitted.
mkCancelHandshakeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelHandshakeResponse
mkCancelHandshakeResponse responseStatus
  = CancelHandshakeResponse'{handshake = Core.Nothing,
                             responseStatus}

-- | A structure that contains details about the handshake that you canceled.
--
-- /Note:/ Consider using 'handshake' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chrrsHandshake :: Lens.Lens' CancelHandshakeResponse (Core.Maybe Types.Handshake)
chrrsHandshake = Lens.field @"handshake"
{-# INLINEABLE chrrsHandshake #-}
{-# DEPRECATED handshake "Use generic-lens or generic-optics with 'handshake' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chrrsResponseStatus :: Lens.Lens' CancelHandshakeResponse Core.Int
chrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE chrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
