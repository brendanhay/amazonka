{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.ExpireSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Immediately stops the specified streaming session.
module Network.AWS.AppStream.ExpireSession
    (
    -- * Creating a request
      ExpireSession (..)
    , mkExpireSession
    -- ** Request lenses
    , esSessionId

    -- * Destructuring the response
    , ExpireSessionResponse (..)
    , mkExpireSessionResponse
    -- ** Response lenses
    , esrrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkExpireSession' smart constructor.
newtype ExpireSession = ExpireSession'
  { sessionId :: Core.Text
    -- ^ The identifier of the streaming session.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ExpireSession' value with any optional fields omitted.
mkExpireSession
    :: Core.Text -- ^ 'sessionId'
    -> ExpireSession
mkExpireSession sessionId = ExpireSession'{sessionId}

-- | The identifier of the streaming session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSessionId :: Lens.Lens' ExpireSession Core.Text
esSessionId = Lens.field @"sessionId"
{-# INLINEABLE esSessionId #-}
{-# DEPRECATED sessionId "Use generic-lens or generic-optics with 'sessionId' instead"  #-}

instance Core.ToQuery ExpireSession where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ExpireSession where
        toHeaders ExpireSession{..}
          = Core.pure
              ("X-Amz-Target", "PhotonAdminProxyService.ExpireSession")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ExpireSession where
        toJSON ExpireSession{..}
          = Core.object
              (Core.catMaybes [Core.Just ("SessionId" Core..= sessionId)])

instance Core.AWSRequest ExpireSession where
        type Rs ExpireSession = ExpireSessionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ExpireSessionResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkExpireSessionResponse' smart constructor.
newtype ExpireSessionResponse = ExpireSessionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ExpireSessionResponse' value with any optional fields omitted.
mkExpireSessionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ExpireSessionResponse
mkExpireSessionResponse responseStatus
  = ExpireSessionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrrsResponseStatus :: Lens.Lens' ExpireSessionResponse Core.Int
esrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE esrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
