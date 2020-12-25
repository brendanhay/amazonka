{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.DeleteSignalingChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified signaling channel. @DeleteSignalingChannel@ is an asynchronous operation. If you don't specify the channel's current version, the most recent version is deleted.
module Network.AWS.KinesisVideo.DeleteSignalingChannel
  ( -- * Creating a request
    DeleteSignalingChannel (..),
    mkDeleteSignalingChannel,

    -- ** Request lenses
    dscChannelARN,
    dscCurrentVersion,

    -- * Destructuring the response
    DeleteSignalingChannelResponse (..),
    mkDeleteSignalingChannelResponse,

    -- ** Response lenses
    dscrrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSignalingChannel' smart constructor.
data DeleteSignalingChannel = DeleteSignalingChannel'
  { -- | The Amazon Resource Name (ARN) of the signaling channel that you want to delete.
    channelARN :: Types.ChannelARN,
    -- | The current version of the signaling channel that you want to delete. You can obtain the current version by invoking the @DescribeSignalingChannel@ or @ListSignalingChannels@ API operations.
    currentVersion :: Core.Maybe Types.CurrentVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSignalingChannel' value with any optional fields omitted.
mkDeleteSignalingChannel ::
  -- | 'channelARN'
  Types.ChannelARN ->
  DeleteSignalingChannel
mkDeleteSignalingChannel channelARN =
  DeleteSignalingChannel'
    { channelARN,
      currentVersion = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the signaling channel that you want to delete.
--
-- /Note:/ Consider using 'channelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscChannelARN :: Lens.Lens' DeleteSignalingChannel Types.ChannelARN
dscChannelARN = Lens.field @"channelARN"
{-# DEPRECATED dscChannelARN "Use generic-lens or generic-optics with 'channelARN' instead." #-}

-- | The current version of the signaling channel that you want to delete. You can obtain the current version by invoking the @DescribeSignalingChannel@ or @ListSignalingChannels@ API operations.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscCurrentVersion :: Lens.Lens' DeleteSignalingChannel (Core.Maybe Types.CurrentVersion)
dscCurrentVersion = Lens.field @"currentVersion"
{-# DEPRECATED dscCurrentVersion "Use generic-lens or generic-optics with 'currentVersion' instead." #-}

instance Core.FromJSON DeleteSignalingChannel where
  toJSON DeleteSignalingChannel {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ChannelARN" Core..= channelARN),
            ("CurrentVersion" Core..=) Core.<$> currentVersion
          ]
      )

instance Core.AWSRequest DeleteSignalingChannel where
  type Rs DeleteSignalingChannel = DeleteSignalingChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/deleteSignalingChannel",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSignalingChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteSignalingChannelResponse' smart constructor.
newtype DeleteSignalingChannelResponse = DeleteSignalingChannelResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSignalingChannelResponse' value with any optional fields omitted.
mkDeleteSignalingChannelResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteSignalingChannelResponse
mkDeleteSignalingChannelResponse responseStatus =
  DeleteSignalingChannelResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrrsResponseStatus :: Lens.Lens' DeleteSignalingChannelResponse Core.Int
dscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
