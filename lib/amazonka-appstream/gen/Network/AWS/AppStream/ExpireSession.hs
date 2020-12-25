{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ExpireSession (..),
    mkExpireSession,

    -- ** Request lenses
    esSessionId,

    -- * Destructuring the response
    ExpireSessionResponse (..),
    mkExpireSessionResponse,

    -- ** Response lenses
    esrrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkExpireSession' smart constructor.
newtype ExpireSession = ExpireSession'
  { -- | The identifier of the streaming session.
    sessionId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ExpireSession' value with any optional fields omitted.
mkExpireSession ::
  -- | 'sessionId'
  Types.String ->
  ExpireSession
mkExpireSession sessionId = ExpireSession' {sessionId}

-- | The identifier of the streaming session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSessionId :: Lens.Lens' ExpireSession Types.String
esSessionId = Lens.field @"sessionId"
{-# DEPRECATED esSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

instance Core.FromJSON ExpireSession where
  toJSON ExpireSession {..} =
    Core.object
      (Core.catMaybes [Core.Just ("SessionId" Core..= sessionId)])

instance Core.AWSRequest ExpireSession where
  type Rs ExpireSession = ExpireSessionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "PhotonAdminProxyService.ExpireSession")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ExpireSessionResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkExpireSessionResponse' smart constructor.
newtype ExpireSessionResponse = ExpireSessionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ExpireSessionResponse' value with any optional fields omitted.
mkExpireSessionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ExpireSessionResponse
mkExpireSessionResponse responseStatus =
  ExpireSessionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrrsResponseStatus :: Lens.Lens' ExpireSessionResponse Core.Int
esrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED esrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
