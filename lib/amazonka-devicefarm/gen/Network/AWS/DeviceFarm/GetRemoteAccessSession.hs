{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetRemoteAccessSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a link to a currently running remote access session.
module Network.AWS.DeviceFarm.GetRemoteAccessSession
  ( -- * Creating a request
    GetRemoteAccessSession (..),
    mkGetRemoteAccessSession,

    -- ** Request lenses
    grasArn,

    -- * Destructuring the response
    GetRemoteAccessSessionResponse (..),
    mkGetRemoteAccessSessionResponse,

    -- ** Response lenses
    grasrrsRemoteAccessSession,
    grasrrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to get information about the specified remote access session.
--
-- /See:/ 'mkGetRemoteAccessSession' smart constructor.
newtype GetRemoteAccessSession = GetRemoteAccessSession'
  { -- | The Amazon Resource Name (ARN) of the remote access session about which you want to get session information.
    arn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRemoteAccessSession' value with any optional fields omitted.
mkGetRemoteAccessSession ::
  -- | 'arn'
  Types.Arn ->
  GetRemoteAccessSession
mkGetRemoteAccessSession arn = GetRemoteAccessSession' {arn}

-- | The Amazon Resource Name (ARN) of the remote access session about which you want to get session information.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grasArn :: Lens.Lens' GetRemoteAccessSession Types.Arn
grasArn = Lens.field @"arn"
{-# DEPRECATED grasArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromJSON GetRemoteAccessSession where
  toJSON GetRemoteAccessSession {..} =
    Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest GetRemoteAccessSession where
  type Rs GetRemoteAccessSession = GetRemoteAccessSessionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.GetRemoteAccessSession")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRemoteAccessSessionResponse'
            Core.<$> (x Core..:? "remoteAccessSession")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the response from the server that lists detailed information about the remote access session.
--
-- /See:/ 'mkGetRemoteAccessSessionResponse' smart constructor.
data GetRemoteAccessSessionResponse = GetRemoteAccessSessionResponse'
  { -- | A container that lists detailed information about the remote access session.
    remoteAccessSession :: Core.Maybe Types.RemoteAccessSession,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetRemoteAccessSessionResponse' value with any optional fields omitted.
mkGetRemoteAccessSessionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRemoteAccessSessionResponse
mkGetRemoteAccessSessionResponse responseStatus =
  GetRemoteAccessSessionResponse'
    { remoteAccessSession =
        Core.Nothing,
      responseStatus
    }

-- | A container that lists detailed information about the remote access session.
--
-- /Note:/ Consider using 'remoteAccessSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grasrrsRemoteAccessSession :: Lens.Lens' GetRemoteAccessSessionResponse (Core.Maybe Types.RemoteAccessSession)
grasrrsRemoteAccessSession = Lens.field @"remoteAccessSession"
{-# DEPRECATED grasrrsRemoteAccessSession "Use generic-lens or generic-optics with 'remoteAccessSession' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grasrrsResponseStatus :: Lens.Lens' GetRemoteAccessSessionResponse Core.Int
grasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
