{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetEmailChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the email channel for an application.
module Network.AWS.Pinpoint.GetEmailChannel
  ( -- * Creating a request
    GetEmailChannel (..),
    mkGetEmailChannel,

    -- ** Request lenses
    gecApplicationId,

    -- * Destructuring the response
    GetEmailChannelResponse (..),
    mkGetEmailChannelResponse,

    -- ** Response lenses
    gecrrsEmailChannelResponse,
    gecrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetEmailChannel' smart constructor.
newtype GetEmailChannel = GetEmailChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetEmailChannel' value with any optional fields omitted.
mkGetEmailChannel ::
  -- | 'applicationId'
  Core.Text ->
  GetEmailChannel
mkGetEmailChannel applicationId = GetEmailChannel' {applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gecApplicationId :: Lens.Lens' GetEmailChannel Core.Text
gecApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gecApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Core.AWSRequest GetEmailChannel where
  type Rs GetEmailChannel = GetEmailChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/channels/email")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEmailChannelResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetEmailChannelResponse' smart constructor.
data GetEmailChannelResponse = GetEmailChannelResponse'
  { emailChannelResponse :: Types.EmailChannelResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetEmailChannelResponse' value with any optional fields omitted.
mkGetEmailChannelResponse ::
  -- | 'emailChannelResponse'
  Types.EmailChannelResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetEmailChannelResponse
mkGetEmailChannelResponse emailChannelResponse responseStatus =
  GetEmailChannelResponse' {emailChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'emailChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gecrrsEmailChannelResponse :: Lens.Lens' GetEmailChannelResponse Types.EmailChannelResponse
gecrrsEmailChannelResponse = Lens.field @"emailChannelResponse"
{-# DEPRECATED gecrrsEmailChannelResponse "Use generic-lens or generic-optics with 'emailChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gecrrsResponseStatus :: Lens.Lens' GetEmailChannelResponse Core.Int
gecrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gecrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
