{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetTestGridSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A session is an instance of a browser created through a @RemoteWebDriver@ with the URL from 'CreateTestGridUrlResult$url' . You can use the following to look up sessions:
--
--
--     * The session ARN ('GetTestGridSessionRequest$sessionArn' ).
--
--
--     * The project ARN and a session ID ('GetTestGridSessionRequest$projectArn' and 'GetTestGridSessionRequest$sessionId' ).
--
--
--
module Network.AWS.DeviceFarm.GetTestGridSession
    (
    -- * Creating a request
      GetTestGridSession (..)
    , mkGetTestGridSession
    -- ** Request lenses
    , gtgsProjectArn
    , gtgsSessionArn
    , gtgsSessionId

    -- * Destructuring the response
    , GetTestGridSessionResponse (..)
    , mkGetTestGridSessionResponse
    -- ** Response lenses
    , gtgsrrsTestGridSession
    , gtgsrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTestGridSession' smart constructor.
data GetTestGridSession = GetTestGridSession'
  { projectArn :: Core.Maybe Types.ProjectArn
    -- ^ The ARN for the project that this session belongs to. See 'CreateTestGridProject' and 'ListTestGridProjects' .
  , sessionArn :: Core.Maybe Types.SessionArn
    -- ^ An ARN that uniquely identifies a 'TestGridSession' .
  , sessionId :: Core.Maybe Types.SessionId
    -- ^ An ID associated with this session.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTestGridSession' value with any optional fields omitted.
mkGetTestGridSession
    :: GetTestGridSession
mkGetTestGridSession
  = GetTestGridSession'{projectArn = Core.Nothing,
                        sessionArn = Core.Nothing, sessionId = Core.Nothing}

-- | The ARN for the project that this session belongs to. See 'CreateTestGridProject' and 'ListTestGridProjects' .
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsProjectArn :: Lens.Lens' GetTestGridSession (Core.Maybe Types.ProjectArn)
gtgsProjectArn = Lens.field @"projectArn"
{-# INLINEABLE gtgsProjectArn #-}
{-# DEPRECATED projectArn "Use generic-lens or generic-optics with 'projectArn' instead"  #-}

-- | An ARN that uniquely identifies a 'TestGridSession' .
--
-- /Note:/ Consider using 'sessionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsSessionArn :: Lens.Lens' GetTestGridSession (Core.Maybe Types.SessionArn)
gtgsSessionArn = Lens.field @"sessionArn"
{-# INLINEABLE gtgsSessionArn #-}
{-# DEPRECATED sessionArn "Use generic-lens or generic-optics with 'sessionArn' instead"  #-}

-- | An ID associated with this session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsSessionId :: Lens.Lens' GetTestGridSession (Core.Maybe Types.SessionId)
gtgsSessionId = Lens.field @"sessionId"
{-# INLINEABLE gtgsSessionId #-}
{-# DEPRECATED sessionId "Use generic-lens or generic-optics with 'sessionId' instead"  #-}

instance Core.ToQuery GetTestGridSession where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTestGridSession where
        toHeaders GetTestGridSession{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.GetTestGridSession")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetTestGridSession where
        toJSON GetTestGridSession{..}
          = Core.object
              (Core.catMaybes
                 [("projectArn" Core..=) Core.<$> projectArn,
                  ("sessionArn" Core..=) Core.<$> sessionArn,
                  ("sessionId" Core..=) Core.<$> sessionId])

instance Core.AWSRequest GetTestGridSession where
        type Rs GetTestGridSession = GetTestGridSessionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTestGridSessionResponse' Core.<$>
                   (x Core..:? "testGridSession") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetTestGridSessionResponse' smart constructor.
data GetTestGridSessionResponse = GetTestGridSessionResponse'
  { testGridSession :: Core.Maybe Types.TestGridSession
    -- ^ The 'TestGridSession' that was requested.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetTestGridSessionResponse' value with any optional fields omitted.
mkGetTestGridSessionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTestGridSessionResponse
mkGetTestGridSessionResponse responseStatus
  = GetTestGridSessionResponse'{testGridSession = Core.Nothing,
                                responseStatus}

-- | The 'TestGridSession' that was requested.
--
-- /Note:/ Consider using 'testGridSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsrrsTestGridSession :: Lens.Lens' GetTestGridSessionResponse (Core.Maybe Types.TestGridSession)
gtgsrrsTestGridSession = Lens.field @"testGridSession"
{-# INLINEABLE gtgsrrsTestGridSession #-}
{-# DEPRECATED testGridSession "Use generic-lens or generic-optics with 'testGridSession' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsrrsResponseStatus :: Lens.Lens' GetTestGridSessionResponse Core.Int
gtgsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtgsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
