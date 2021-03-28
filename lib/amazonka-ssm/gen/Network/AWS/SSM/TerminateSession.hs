{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.TerminateSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently ends a session and closes the data connection between the Session Manager client and SSM Agent on the instance. A terminated session cannot be resumed.
module Network.AWS.SSM.TerminateSession
    (
    -- * Creating a request
      TerminateSession (..)
    , mkTerminateSession
    -- ** Request lenses
    , tsSessionId

    -- * Destructuring the response
    , TerminateSessionResponse (..)
    , mkTerminateSessionResponse
    -- ** Response lenses
    , tsrrsSessionId
    , tsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkTerminateSession' smart constructor.
newtype TerminateSession = TerminateSession'
  { sessionId :: Types.SessionId
    -- ^ The ID of the session to terminate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateSession' value with any optional fields omitted.
mkTerminateSession
    :: Types.SessionId -- ^ 'sessionId'
    -> TerminateSession
mkTerminateSession sessionId = TerminateSession'{sessionId}

-- | The ID of the session to terminate.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsSessionId :: Lens.Lens' TerminateSession Types.SessionId
tsSessionId = Lens.field @"sessionId"
{-# INLINEABLE tsSessionId #-}
{-# DEPRECATED sessionId "Use generic-lens or generic-optics with 'sessionId' instead"  #-}

instance Core.ToQuery TerminateSession where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TerminateSession where
        toHeaders TerminateSession{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.TerminateSession") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON TerminateSession where
        toJSON TerminateSession{..}
          = Core.object
              (Core.catMaybes [Core.Just ("SessionId" Core..= sessionId)])

instance Core.AWSRequest TerminateSession where
        type Rs TerminateSession = TerminateSessionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 TerminateSessionResponse' Core.<$>
                   (x Core..:? "SessionId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTerminateSessionResponse' smart constructor.
data TerminateSessionResponse = TerminateSessionResponse'
  { sessionId :: Core.Maybe Types.SessionId
    -- ^ The ID of the session that has been terminated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateSessionResponse' value with any optional fields omitted.
mkTerminateSessionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TerminateSessionResponse
mkTerminateSessionResponse responseStatus
  = TerminateSessionResponse'{sessionId = Core.Nothing,
                              responseStatus}

-- | The ID of the session that has been terminated.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsrrsSessionId :: Lens.Lens' TerminateSessionResponse (Core.Maybe Types.SessionId)
tsrrsSessionId = Lens.field @"sessionId"
{-# INLINEABLE tsrrsSessionId #-}
{-# DEPRECATED sessionId "Use generic-lens or generic-optics with 'sessionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsrrsResponseStatus :: Lens.Lens' TerminateSessionResponse Core.Int
tsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE tsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
