{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.CreateSignalingChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a signaling channel.
--
-- @CreateSignalingChannel@ is an asynchronous operation.
module Network.AWS.KinesisVideo.CreateSignalingChannel
  ( -- * Creating a request
    CreateSignalingChannel (..),
    mkCreateSignalingChannel,

    -- ** Request lenses
    cscChannelName,
    cscChannelType,
    cscSingleMasterConfiguration,
    cscTags,

    -- * Destructuring the response
    CreateSignalingChannelResponse (..),
    mkCreateSignalingChannelResponse,

    -- ** Response lenses
    cscrrsChannelARN,
    cscrrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSignalingChannel' smart constructor.
data CreateSignalingChannel = CreateSignalingChannel'
  { -- | A name for the signaling channel that you are creating. It must be unique for each AWS account and AWS Region.
    channelName :: Types.ChannelName,
    -- | A type of the signaling channel that you are creating. Currently, @SINGLE_MASTER@ is the only supported channel type.
    channelType :: Core.Maybe Types.ChannelType,
    -- | A structure containing the configuration for the @SINGLE_MASTER@ channel type.
    singleMasterConfiguration :: Core.Maybe Types.SingleMasterConfiguration,
    -- | A set of tags (key-value pairs) that you want to associate with this channel.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSignalingChannel' value with any optional fields omitted.
mkCreateSignalingChannel ::
  -- | 'channelName'
  Types.ChannelName ->
  CreateSignalingChannel
mkCreateSignalingChannel channelName =
  CreateSignalingChannel'
    { channelName,
      channelType = Core.Nothing,
      singleMasterConfiguration = Core.Nothing,
      tags = Core.Nothing
    }

-- | A name for the signaling channel that you are creating. It must be unique for each AWS account and AWS Region.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscChannelName :: Lens.Lens' CreateSignalingChannel Types.ChannelName
cscChannelName = Lens.field @"channelName"
{-# DEPRECATED cscChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

-- | A type of the signaling channel that you are creating. Currently, @SINGLE_MASTER@ is the only supported channel type.
--
-- /Note:/ Consider using 'channelType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscChannelType :: Lens.Lens' CreateSignalingChannel (Core.Maybe Types.ChannelType)
cscChannelType = Lens.field @"channelType"
{-# DEPRECATED cscChannelType "Use generic-lens or generic-optics with 'channelType' instead." #-}

-- | A structure containing the configuration for the @SINGLE_MASTER@ channel type.
--
-- /Note:/ Consider using 'singleMasterConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscSingleMasterConfiguration :: Lens.Lens' CreateSignalingChannel (Core.Maybe Types.SingleMasterConfiguration)
cscSingleMasterConfiguration = Lens.field @"singleMasterConfiguration"
{-# DEPRECATED cscSingleMasterConfiguration "Use generic-lens or generic-optics with 'singleMasterConfiguration' instead." #-}

-- | A set of tags (key-value pairs) that you want to associate with this channel.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscTags :: Lens.Lens' CreateSignalingChannel (Core.Maybe [Types.Tag])
cscTags = Lens.field @"tags"
{-# DEPRECATED cscTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateSignalingChannel where
  toJSON CreateSignalingChannel {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ChannelName" Core..= channelName),
            ("ChannelType" Core..=) Core.<$> channelType,
            ("SingleMasterConfiguration" Core..=)
              Core.<$> singleMasterConfiguration,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateSignalingChannel where
  type Rs CreateSignalingChannel = CreateSignalingChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/createSignalingChannel",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSignalingChannelResponse'
            Core.<$> (x Core..:? "ChannelARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateSignalingChannelResponse' smart constructor.
data CreateSignalingChannelResponse = CreateSignalingChannelResponse'
  { -- | The Amazon Resource Name (ARN) of the created channel.
    channelARN :: Core.Maybe Types.ResourceARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSignalingChannelResponse' value with any optional fields omitted.
mkCreateSignalingChannelResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSignalingChannelResponse
mkCreateSignalingChannelResponse responseStatus =
  CreateSignalingChannelResponse'
    { channelARN = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the created channel.
--
-- /Note:/ Consider using 'channelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrrsChannelARN :: Lens.Lens' CreateSignalingChannelResponse (Core.Maybe Types.ResourceARN)
cscrrsChannelARN = Lens.field @"channelARN"
{-# DEPRECATED cscrrsChannelARN "Use generic-lens or generic-optics with 'channelARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrrsResponseStatus :: Lens.Lens' CreateSignalingChannelResponse Core.Int
cscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
