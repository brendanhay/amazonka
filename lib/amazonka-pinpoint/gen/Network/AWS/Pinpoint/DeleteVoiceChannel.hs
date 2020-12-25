{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteVoiceChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the voice channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteVoiceChannel
  ( -- * Creating a request
    DeleteVoiceChannel (..),
    mkDeleteVoiceChannel,

    -- ** Request lenses
    dvcApplicationId,

    -- * Destructuring the response
    DeleteVoiceChannelResponse (..),
    mkDeleteVoiceChannelResponse,

    -- ** Response lenses
    dvcrrsVoiceChannelResponse,
    dvcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteVoiceChannel' smart constructor.
newtype DeleteVoiceChannel = DeleteVoiceChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVoiceChannel' value with any optional fields omitted.
mkDeleteVoiceChannel ::
  -- | 'applicationId'
  Core.Text ->
  DeleteVoiceChannel
mkDeleteVoiceChannel applicationId =
  DeleteVoiceChannel' {applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcApplicationId :: Lens.Lens' DeleteVoiceChannel Core.Text
dvcApplicationId = Lens.field @"applicationId"
{-# DEPRECATED dvcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Core.AWSRequest DeleteVoiceChannel where
  type Rs DeleteVoiceChannel = DeleteVoiceChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/channels/voice")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVoiceChannelResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteVoiceChannelResponse' smart constructor.
data DeleteVoiceChannelResponse = DeleteVoiceChannelResponse'
  { voiceChannelResponse :: Types.VoiceChannelResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVoiceChannelResponse' value with any optional fields omitted.
mkDeleteVoiceChannelResponse ::
  -- | 'voiceChannelResponse'
  Types.VoiceChannelResponse ->
  -- | 'responseStatus'
  Core.Int ->
  DeleteVoiceChannelResponse
mkDeleteVoiceChannelResponse voiceChannelResponse responseStatus =
  DeleteVoiceChannelResponse' {voiceChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'voiceChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcrrsVoiceChannelResponse :: Lens.Lens' DeleteVoiceChannelResponse Types.VoiceChannelResponse
dvcrrsVoiceChannelResponse = Lens.field @"voiceChannelResponse"
{-# DEPRECATED dvcrrsVoiceChannelResponse "Use generic-lens or generic-optics with 'voiceChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcrrsResponseStatus :: Lens.Lens' DeleteVoiceChannelResponse Core.Int
dvcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dvcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
