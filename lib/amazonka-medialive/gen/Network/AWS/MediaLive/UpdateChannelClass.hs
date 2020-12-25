{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.UpdateChannelClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the class of the channel.
module Network.AWS.MediaLive.UpdateChannelClass
  ( -- * Creating a request
    UpdateChannelClass (..),
    mkUpdateChannelClass,

    -- ** Request lenses
    uccChannelId,
    uccChannelClass,
    uccDestinations,

    -- * Destructuring the response
    UpdateChannelClassResponse (..),
    mkUpdateChannelClassResponse,

    -- ** Response lenses
    uccrrsChannel,
    uccrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Channel class that the channel should be updated to.
--
-- /See:/ 'mkUpdateChannelClass' smart constructor.
data UpdateChannelClass = UpdateChannelClass'
  { -- | Channel Id of the channel whose class should be updated.
    channelId :: Core.Text,
    -- | The channel class that you wish to update this channel to use.
    channelClass :: Types.ChannelClass,
    -- | A list of output destinations for this channel.
    destinations :: Core.Maybe [Types.OutputDestination]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateChannelClass' value with any optional fields omitted.
mkUpdateChannelClass ::
  -- | 'channelId'
  Core.Text ->
  -- | 'channelClass'
  Types.ChannelClass ->
  UpdateChannelClass
mkUpdateChannelClass channelId channelClass =
  UpdateChannelClass'
    { channelId,
      channelClass,
      destinations = Core.Nothing
    }

-- | Channel Id of the channel whose class should be updated.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccChannelId :: Lens.Lens' UpdateChannelClass Core.Text
uccChannelId = Lens.field @"channelId"
{-# DEPRECATED uccChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | The channel class that you wish to update this channel to use.
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccChannelClass :: Lens.Lens' UpdateChannelClass Types.ChannelClass
uccChannelClass = Lens.field @"channelClass"
{-# DEPRECATED uccChannelClass "Use generic-lens or generic-optics with 'channelClass' instead." #-}

-- | A list of output destinations for this channel.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccDestinations :: Lens.Lens' UpdateChannelClass (Core.Maybe [Types.OutputDestination])
uccDestinations = Lens.field @"destinations"
{-# DEPRECATED uccDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

instance Core.FromJSON UpdateChannelClass where
  toJSON UpdateChannelClass {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("channelClass" Core..= channelClass),
            ("destinations" Core..=) Core.<$> destinations
          ]
      )

instance Core.AWSRequest UpdateChannelClass where
  type Rs UpdateChannelClass = UpdateChannelClassResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/prod/channels/" Core.<> (Core.toText channelId)
                Core.<> ("/channelClass")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateChannelClassResponse'
            Core.<$> (x Core..:? "channel") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for UpdateChannelClassResponse
--
-- /See:/ 'mkUpdateChannelClassResponse' smart constructor.
data UpdateChannelClassResponse = UpdateChannelClassResponse'
  { channel :: Core.Maybe Types.Channel,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateChannelClassResponse' value with any optional fields omitted.
mkUpdateChannelClassResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateChannelClassResponse
mkUpdateChannelClassResponse responseStatus =
  UpdateChannelClassResponse'
    { channel = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccrrsChannel :: Lens.Lens' UpdateChannelClassResponse (Core.Maybe Types.Channel)
uccrrsChannel = Lens.field @"channel"
{-# DEPRECATED uccrrsChannel "Use generic-lens or generic-optics with 'channel' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccrrsResponseStatus :: Lens.Lens' UpdateChannelClassResponse Core.Int
uccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
