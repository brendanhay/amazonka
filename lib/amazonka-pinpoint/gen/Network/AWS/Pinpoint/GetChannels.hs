{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetChannels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the history and status of each channel for an application.
module Network.AWS.Pinpoint.GetChannels
  ( -- * Creating a request
    GetChannels (..),
    mkGetChannels,

    -- ** Request lenses
    gcsApplicationId,

    -- * Destructuring the response
    GetChannelsResponse (..),
    mkGetChannelsResponse,

    -- ** Response lenses
    gcrfrsChannelsResponse,
    gcrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetChannels' smart constructor.
newtype GetChannels = GetChannels'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetChannels' value with any optional fields omitted.
mkGetChannels ::
  -- | 'applicationId'
  Core.Text ->
  GetChannels
mkGetChannels applicationId = GetChannels' {applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsApplicationId :: Lens.Lens' GetChannels Core.Text
gcsApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gcsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Core.AWSRequest GetChannels where
  type Rs GetChannels = GetChannelsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/channels")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChannelsResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetChannelsResponse' smart constructor.
data GetChannelsResponse = GetChannelsResponse'
  { channelsResponse :: Types.ChannelsResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetChannelsResponse' value with any optional fields omitted.
mkGetChannelsResponse ::
  -- | 'channelsResponse'
  Types.ChannelsResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetChannelsResponse
mkGetChannelsResponse channelsResponse responseStatus =
  GetChannelsResponse' {channelsResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'channelsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrfrsChannelsResponse :: Lens.Lens' GetChannelsResponse Types.ChannelsResponse
gcrfrsChannelsResponse = Lens.field @"channelsResponse"
{-# DEPRECATED gcrfrsChannelsResponse "Use generic-lens or generic-optics with 'channelsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrfrsResponseStatus :: Lens.Lens' GetChannelsResponse Core.Int
gcrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
