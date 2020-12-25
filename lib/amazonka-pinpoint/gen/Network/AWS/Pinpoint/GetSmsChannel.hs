{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSmsChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the SMS channel for an application.
module Network.AWS.Pinpoint.GetSmsChannel
  ( -- * Creating a request
    GetSmsChannel (..),
    mkGetSmsChannel,

    -- ** Request lenses
    gscApplicationId,

    -- * Destructuring the response
    GetSmsChannelResponse (..),
    mkGetSmsChannelResponse,

    -- ** Response lenses
    gscrrsSMSChannelResponse,
    gscrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSmsChannel' smart constructor.
newtype GetSmsChannel = GetSmsChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSmsChannel' value with any optional fields omitted.
mkGetSmsChannel ::
  -- | 'applicationId'
  Core.Text ->
  GetSmsChannel
mkGetSmsChannel applicationId = GetSmsChannel' {applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscApplicationId :: Lens.Lens' GetSmsChannel Core.Text
gscApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gscApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Core.AWSRequest GetSmsChannel where
  type Rs GetSmsChannel = GetSmsChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/channels/sms")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSmsChannelResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSmsChannelResponse' smart constructor.
data GetSmsChannelResponse = GetSmsChannelResponse'
  { sMSChannelResponse :: Types.SMSChannelResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSmsChannelResponse' value with any optional fields omitted.
mkGetSmsChannelResponse ::
  -- | 'sMSChannelResponse'
  Types.SMSChannelResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetSmsChannelResponse
mkGetSmsChannelResponse sMSChannelResponse responseStatus =
  GetSmsChannelResponse' {sMSChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sMSChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrrsSMSChannelResponse :: Lens.Lens' GetSmsChannelResponse Types.SMSChannelResponse
gscrrsSMSChannelResponse = Lens.field @"sMSChannelResponse"
{-# DEPRECATED gscrrsSMSChannelResponse "Use generic-lens or generic-optics with 'sMSChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrrsResponseStatus :: Lens.Lens' GetSmsChannelResponse Core.Int
gscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
