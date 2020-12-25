{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetVoiceChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the voice channel for an application.
module Network.AWS.Pinpoint.GetVoiceChannel
  ( -- * Creating a request
    GetVoiceChannel (..),
    mkGetVoiceChannel,

    -- ** Request lenses
    gvcApplicationId,

    -- * Destructuring the response
    GetVoiceChannelResponse (..),
    mkGetVoiceChannelResponse,

    -- ** Response lenses
    gvcrrsVoiceChannelResponse,
    gvcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetVoiceChannel' smart constructor.
newtype GetVoiceChannel = GetVoiceChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetVoiceChannel' value with any optional fields omitted.
mkGetVoiceChannel ::
  -- | 'applicationId'
  Core.Text ->
  GetVoiceChannel
mkGetVoiceChannel applicationId = GetVoiceChannel' {applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvcApplicationId :: Lens.Lens' GetVoiceChannel Core.Text
gvcApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gvcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Core.AWSRequest GetVoiceChannel where
  type Rs GetVoiceChannel = GetVoiceChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
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
          GetVoiceChannelResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetVoiceChannelResponse' smart constructor.
data GetVoiceChannelResponse = GetVoiceChannelResponse'
  { voiceChannelResponse :: Types.VoiceChannelResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetVoiceChannelResponse' value with any optional fields omitted.
mkGetVoiceChannelResponse ::
  -- | 'voiceChannelResponse'
  Types.VoiceChannelResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetVoiceChannelResponse
mkGetVoiceChannelResponse voiceChannelResponse responseStatus =
  GetVoiceChannelResponse' {voiceChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'voiceChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvcrrsVoiceChannelResponse :: Lens.Lens' GetVoiceChannelResponse Types.VoiceChannelResponse
gvcrrsVoiceChannelResponse = Lens.field @"voiceChannelResponse"
{-# DEPRECATED gvcrrsVoiceChannelResponse "Use generic-lens or generic-optics with 'voiceChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvcrrsResponseStatus :: Lens.Lens' GetVoiceChannelResponse Core.Int
gvcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gvcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
