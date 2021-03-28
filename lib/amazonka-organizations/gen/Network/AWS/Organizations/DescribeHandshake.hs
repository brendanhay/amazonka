{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DescribeHandshake
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a previously requested handshake. The handshake ID comes from the response to the original 'InviteAccountToOrganization' operation that generated the handshake.
--
-- You can access handshakes that are @ACCEPTED@ , @DECLINED@ , or @CANCELED@ for only 30 days after they change to that state. They're then deleted and no longer accessible.
-- This operation can be called from any account in the organization.
module Network.AWS.Organizations.DescribeHandshake
    (
    -- * Creating a request
      DescribeHandshake (..)
    , mkDescribeHandshake
    -- ** Request lenses
    , dhHandshakeId

    -- * Destructuring the response
    , DescribeHandshakeResponse (..)
    , mkDescribeHandshakeResponse
    -- ** Response lenses
    , dhrrsHandshake
    , dhrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeHandshake' smart constructor.
newtype DescribeHandshake = DescribeHandshake'
  { handshakeId :: Types.HandshakeId
    -- ^ The unique identifier (ID) of the handshake that you want information about. You can get the ID from the original call to 'InviteAccountToOrganization' , or from a call to 'ListHandshakesForAccount' or 'ListHandshakesForOrganization' .
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHandshake' value with any optional fields omitted.
mkDescribeHandshake
    :: Types.HandshakeId -- ^ 'handshakeId'
    -> DescribeHandshake
mkDescribeHandshake handshakeId = DescribeHandshake'{handshakeId}

-- | The unique identifier (ID) of the handshake that you want information about. You can get the ID from the original call to 'InviteAccountToOrganization' , or from a call to 'ListHandshakesForAccount' or 'ListHandshakesForOrganization' .
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'handshakeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhHandshakeId :: Lens.Lens' DescribeHandshake Types.HandshakeId
dhHandshakeId = Lens.field @"handshakeId"
{-# INLINEABLE dhHandshakeId #-}
{-# DEPRECATED handshakeId "Use generic-lens or generic-optics with 'handshakeId' instead"  #-}

instance Core.ToQuery DescribeHandshake where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeHandshake where
        toHeaders DescribeHandshake{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.DescribeHandshake")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeHandshake where
        toJSON DescribeHandshake{..}
          = Core.object
              (Core.catMaybes [Core.Just ("HandshakeId" Core..= handshakeId)])

instance Core.AWSRequest DescribeHandshake where
        type Rs DescribeHandshake = DescribeHandshakeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeHandshakeResponse' Core.<$>
                   (x Core..:? "Handshake") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeHandshakeResponse' smart constructor.
data DescribeHandshakeResponse = DescribeHandshakeResponse'
  { handshake :: Core.Maybe Types.Handshake
    -- ^ A structure that contains information about the specified handshake.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeHandshakeResponse' value with any optional fields omitted.
mkDescribeHandshakeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeHandshakeResponse
mkDescribeHandshakeResponse responseStatus
  = DescribeHandshakeResponse'{handshake = Core.Nothing,
                               responseStatus}

-- | A structure that contains information about the specified handshake.
--
-- /Note:/ Consider using 'handshake' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrrsHandshake :: Lens.Lens' DescribeHandshakeResponse (Core.Maybe Types.Handshake)
dhrrsHandshake = Lens.field @"handshake"
{-# INLINEABLE dhrrsHandshake #-}
{-# DEPRECATED handshake "Use generic-lens or generic-optics with 'handshake' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrrsResponseStatus :: Lens.Lens' DescribeHandshakeResponse Core.Int
dhrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dhrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
