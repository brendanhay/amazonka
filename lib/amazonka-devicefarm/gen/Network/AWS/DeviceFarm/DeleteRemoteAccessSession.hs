{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteRemoteAccessSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a completed remote access session and its results.
module Network.AWS.DeviceFarm.DeleteRemoteAccessSession
  ( -- * Creating a request
    DeleteRemoteAccessSession (..),
    mkDeleteRemoteAccessSession,

    -- ** Request lenses
    drasArn,

    -- * Destructuring the response
    DeleteRemoteAccessSessionResponse (..),
    mkDeleteRemoteAccessSessionResponse,

    -- ** Response lenses
    drasrrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to delete the specified remote access session.
--
-- /See:/ 'mkDeleteRemoteAccessSession' smart constructor.
newtype DeleteRemoteAccessSession = DeleteRemoteAccessSession'
  { -- | The Amazon Resource Name (ARN) of the session for which you want to delete remote access.
    arn :: Types.AmazonResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRemoteAccessSession' value with any optional fields omitted.
mkDeleteRemoteAccessSession ::
  -- | 'arn'
  Types.AmazonResourceName ->
  DeleteRemoteAccessSession
mkDeleteRemoteAccessSession arn = DeleteRemoteAccessSession' {arn}

-- | The Amazon Resource Name (ARN) of the session for which you want to delete remote access.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drasArn :: Lens.Lens' DeleteRemoteAccessSession Types.AmazonResourceName
drasArn = Lens.field @"arn"
{-# DEPRECATED drasArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromJSON DeleteRemoteAccessSession where
  toJSON DeleteRemoteAccessSession {..} =
    Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest DeleteRemoteAccessSession where
  type
    Rs DeleteRemoteAccessSession =
      DeleteRemoteAccessSessionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.DeleteRemoteAccessSession")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRemoteAccessSessionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | The response from the server when a request is made to delete the remote access session.
--
-- /See:/ 'mkDeleteRemoteAccessSessionResponse' smart constructor.
newtype DeleteRemoteAccessSessionResponse = DeleteRemoteAccessSessionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRemoteAccessSessionResponse' value with any optional fields omitted.
mkDeleteRemoteAccessSessionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteRemoteAccessSessionResponse
mkDeleteRemoteAccessSessionResponse responseStatus =
  DeleteRemoteAccessSessionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drasrrsResponseStatus :: Lens.Lens' DeleteRemoteAccessSessionResponse Core.Int
drasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
