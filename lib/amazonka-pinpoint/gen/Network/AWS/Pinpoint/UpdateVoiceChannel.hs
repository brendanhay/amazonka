{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateVoiceChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the voice channel for an application or updates the status and settings of the voice channel for an application.
module Network.AWS.Pinpoint.UpdateVoiceChannel
  ( -- * Creating a request
    UpdateVoiceChannel (..),
    mkUpdateVoiceChannel,

    -- ** Request lenses
    uvcApplicationId,
    uvcVoiceChannelRequest,

    -- * Destructuring the response
    UpdateVoiceChannelResponse (..),
    mkUpdateVoiceChannelResponse,

    -- ** Response lenses
    uvcrrsVoiceChannelResponse,
    uvcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateVoiceChannel' smart constructor.
data UpdateVoiceChannel = UpdateVoiceChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    voiceChannelRequest :: Types.VoiceChannelRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateVoiceChannel' value with any optional fields omitted.
mkUpdateVoiceChannel ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'voiceChannelRequest'
  Types.VoiceChannelRequest ->
  UpdateVoiceChannel
mkUpdateVoiceChannel applicationId voiceChannelRequest =
  UpdateVoiceChannel' {applicationId, voiceChannelRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvcApplicationId :: Lens.Lens' UpdateVoiceChannel Core.Text
uvcApplicationId = Lens.field @"applicationId"
{-# DEPRECATED uvcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'voiceChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvcVoiceChannelRequest :: Lens.Lens' UpdateVoiceChannel Types.VoiceChannelRequest
uvcVoiceChannelRequest = Lens.field @"voiceChannelRequest"
{-# DEPRECATED uvcVoiceChannelRequest "Use generic-lens or generic-optics with 'voiceChannelRequest' instead." #-}

instance Core.FromJSON UpdateVoiceChannel where
  toJSON UpdateVoiceChannel {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("VoiceChannelRequest" Core..= voiceChannelRequest)]
      )

instance Core.AWSRequest UpdateVoiceChannel where
  type Rs UpdateVoiceChannel = UpdateVoiceChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/channels/voice")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVoiceChannelResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateVoiceChannelResponse' smart constructor.
data UpdateVoiceChannelResponse = UpdateVoiceChannelResponse'
  { voiceChannelResponse :: Types.VoiceChannelResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateVoiceChannelResponse' value with any optional fields omitted.
mkUpdateVoiceChannelResponse ::
  -- | 'voiceChannelResponse'
  Types.VoiceChannelResponse ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateVoiceChannelResponse
mkUpdateVoiceChannelResponse voiceChannelResponse responseStatus =
  UpdateVoiceChannelResponse' {voiceChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'voiceChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvcrrsVoiceChannelResponse :: Lens.Lens' UpdateVoiceChannelResponse Types.VoiceChannelResponse
uvcrrsVoiceChannelResponse = Lens.field @"voiceChannelResponse"
{-# DEPRECATED uvcrrsVoiceChannelResponse "Use generic-lens or generic-optics with 'voiceChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvcrrsResponseStatus :: Lens.Lens' UpdateVoiceChannelResponse Core.Int
uvcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uvcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
