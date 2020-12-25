{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.DeleteChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing Channel.
module Network.AWS.MediaPackage.DeleteChannel
  ( -- * Creating a request
    DeleteChannel (..),
    mkDeleteChannel,

    -- ** Request lenses
    dcId,

    -- * Destructuring the response
    DeleteChannelResponse (..),
    mkDeleteChannelResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteChannel' smart constructor.
newtype DeleteChannel = DeleteChannel'
  { -- | The ID of the Channel to delete.
    id :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteChannel' value with any optional fields omitted.
mkDeleteChannel ::
  -- | 'id'
  Core.Text ->
  DeleteChannel
mkDeleteChannel id = DeleteChannel' {id}

-- | The ID of the Channel to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcId :: Lens.Lens' DeleteChannel Core.Text
dcId = Lens.field @"id"
{-# DEPRECATED dcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest DeleteChannel where
  type Rs DeleteChannel = DeleteChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath ("/channels/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteChannelResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteChannelResponse' smart constructor.
newtype DeleteChannelResponse = DeleteChannelResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteChannelResponse' value with any optional fields omitted.
mkDeleteChannelResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteChannelResponse
mkDeleteChannelResponse responseStatus =
  DeleteChannelResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteChannelResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
