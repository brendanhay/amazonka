{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.UpdateSignalingChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the existing signaling channel. This is an asynchronous operation and takes time to complete.
--
-- If the @MessageTtlSeconds@ value is updated (either increased or reduced), it only applies to new messages sent via this channel after it's been updated. Existing messages are still expired as per the previous @MessageTtlSeconds@ value.
module Network.AWS.KinesisVideo.UpdateSignalingChannel
  ( -- * Creating a request
    UpdateSignalingChannel (..),
    mkUpdateSignalingChannel,

    -- ** Request lenses
    uscChannelARN,
    uscCurrentVersion,
    uscSingleMasterConfiguration,

    -- * Destructuring the response
    UpdateSignalingChannelResponse (..),
    mkUpdateSignalingChannelResponse,

    -- ** Response lenses
    uscrrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateSignalingChannel' smart constructor.
data UpdateSignalingChannel = UpdateSignalingChannel'
  { -- | The Amazon Resource Name (ARN) of the signaling channel that you want to update.
    channelARN :: Types.ChannelARN,
    -- | The current version of the signaling channel that you want to update.
    currentVersion :: Types.CurrentVersion,
    -- | The structure containing the configuration for the @SINGLE_MASTER@ type of the signaling channel that you want to update.
    singleMasterConfiguration :: Core.Maybe Types.SingleMasterConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSignalingChannel' value with any optional fields omitted.
mkUpdateSignalingChannel ::
  -- | 'channelARN'
  Types.ChannelARN ->
  -- | 'currentVersion'
  Types.CurrentVersion ->
  UpdateSignalingChannel
mkUpdateSignalingChannel channelARN currentVersion =
  UpdateSignalingChannel'
    { channelARN,
      currentVersion,
      singleMasterConfiguration = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the signaling channel that you want to update.
--
-- /Note:/ Consider using 'channelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscChannelARN :: Lens.Lens' UpdateSignalingChannel Types.ChannelARN
uscChannelARN = Lens.field @"channelARN"
{-# DEPRECATED uscChannelARN "Use generic-lens or generic-optics with 'channelARN' instead." #-}

-- | The current version of the signaling channel that you want to update.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscCurrentVersion :: Lens.Lens' UpdateSignalingChannel Types.CurrentVersion
uscCurrentVersion = Lens.field @"currentVersion"
{-# DEPRECATED uscCurrentVersion "Use generic-lens or generic-optics with 'currentVersion' instead." #-}

-- | The structure containing the configuration for the @SINGLE_MASTER@ type of the signaling channel that you want to update.
--
-- /Note:/ Consider using 'singleMasterConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscSingleMasterConfiguration :: Lens.Lens' UpdateSignalingChannel (Core.Maybe Types.SingleMasterConfiguration)
uscSingleMasterConfiguration = Lens.field @"singleMasterConfiguration"
{-# DEPRECATED uscSingleMasterConfiguration "Use generic-lens or generic-optics with 'singleMasterConfiguration' instead." #-}

instance Core.FromJSON UpdateSignalingChannel where
  toJSON UpdateSignalingChannel {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ChannelARN" Core..= channelARN),
            Core.Just ("CurrentVersion" Core..= currentVersion),
            ("SingleMasterConfiguration" Core..=)
              Core.<$> singleMasterConfiguration
          ]
      )

instance Core.AWSRequest UpdateSignalingChannel where
  type Rs UpdateSignalingChannel = UpdateSignalingChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/updateSignalingChannel",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateSignalingChannelResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateSignalingChannelResponse' smart constructor.
newtype UpdateSignalingChannelResponse = UpdateSignalingChannelResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSignalingChannelResponse' value with any optional fields omitted.
mkUpdateSignalingChannelResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateSignalingChannelResponse
mkUpdateSignalingChannelResponse responseStatus =
  UpdateSignalingChannelResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrrsResponseStatus :: Lens.Lens' UpdateSignalingChannelResponse Core.Int
uscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
