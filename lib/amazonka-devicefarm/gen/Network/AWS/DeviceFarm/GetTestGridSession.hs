{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.DeviceFarm.GetTestGridSession
  ( -- * Creating a request
    GetTestGridSession (..),
    mkGetTestGridSession,

    -- ** Request lenses
    gtgsProjectArn,
    gtgsSessionArn,
    gtgsSessionId,

    -- * Destructuring the response
    GetTestGridSessionResponse (..),
    mkGetTestGridSessionResponse,

    -- ** Response lenses
    gtgsrrsTestGridSession,
    gtgsrrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTestGridSession' smart constructor.
data GetTestGridSession = GetTestGridSession'
  { -- | The ARN for the project that this session belongs to. See 'CreateTestGridProject' and 'ListTestGridProjects' .
    projectArn :: Core.Maybe Types.ProjectArn,
    -- | An ARN that uniquely identifies a 'TestGridSession' .
    sessionArn :: Core.Maybe Types.SessionArn,
    -- | An ID associated with this session.
    sessionId :: Core.Maybe Types.SessionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTestGridSession' value with any optional fields omitted.
mkGetTestGridSession ::
  GetTestGridSession
mkGetTestGridSession =
  GetTestGridSession'
    { projectArn = Core.Nothing,
      sessionArn = Core.Nothing,
      sessionId = Core.Nothing
    }

-- | The ARN for the project that this session belongs to. See 'CreateTestGridProject' and 'ListTestGridProjects' .
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsProjectArn :: Lens.Lens' GetTestGridSession (Core.Maybe Types.ProjectArn)
gtgsProjectArn = Lens.field @"projectArn"
{-# DEPRECATED gtgsProjectArn "Use generic-lens or generic-optics with 'projectArn' instead." #-}

-- | An ARN that uniquely identifies a 'TestGridSession' .
--
-- /Note:/ Consider using 'sessionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsSessionArn :: Lens.Lens' GetTestGridSession (Core.Maybe Types.SessionArn)
gtgsSessionArn = Lens.field @"sessionArn"
{-# DEPRECATED gtgsSessionArn "Use generic-lens or generic-optics with 'sessionArn' instead." #-}

-- | An ID associated with this session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsSessionId :: Lens.Lens' GetTestGridSession (Core.Maybe Types.SessionId)
gtgsSessionId = Lens.field @"sessionId"
{-# DEPRECATED gtgsSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

instance Core.FromJSON GetTestGridSession where
  toJSON GetTestGridSession {..} =
    Core.object
      ( Core.catMaybes
          [ ("projectArn" Core..=) Core.<$> projectArn,
            ("sessionArn" Core..=) Core.<$> sessionArn,
            ("sessionId" Core..=) Core.<$> sessionId
          ]
      )

instance Core.AWSRequest GetTestGridSession where
  type Rs GetTestGridSession = GetTestGridSessionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.GetTestGridSession")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTestGridSessionResponse'
            Core.<$> (x Core..:? "testGridSession")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetTestGridSessionResponse' smart constructor.
data GetTestGridSessionResponse = GetTestGridSessionResponse'
  { -- | The 'TestGridSession' that was requested.
    testGridSession :: Core.Maybe Types.TestGridSession,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetTestGridSessionResponse' value with any optional fields omitted.
mkGetTestGridSessionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTestGridSessionResponse
mkGetTestGridSessionResponse responseStatus =
  GetTestGridSessionResponse'
    { testGridSession = Core.Nothing,
      responseStatus
    }

-- | The 'TestGridSession' that was requested.
--
-- /Note:/ Consider using 'testGridSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsrrsTestGridSession :: Lens.Lens' GetTestGridSessionResponse (Core.Maybe Types.TestGridSession)
gtgsrrsTestGridSession = Lens.field @"testGridSession"
{-# DEPRECATED gtgsrrsTestGridSession "Use generic-lens or generic-optics with 'testGridSession' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgsrrsResponseStatus :: Lens.Lens' GetTestGridSessionResponse Core.Int
gtgsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtgsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
