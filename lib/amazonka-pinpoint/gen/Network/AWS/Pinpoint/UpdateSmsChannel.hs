{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateSmsChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the SMS channel for an application or updates the status and settings of the SMS channel for an application.
module Network.AWS.Pinpoint.UpdateSmsChannel
  ( -- * Creating a request
    UpdateSmsChannel (..),
    mkUpdateSmsChannel,

    -- ** Request lenses
    uscApplicationId,
    uscSMSChannelRequest,

    -- * Destructuring the response
    UpdateSmsChannelResponse (..),
    mkUpdateSmsChannelResponse,

    -- ** Response lenses
    uscrrsSMSChannelResponse,
    uscrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateSmsChannel' smart constructor.
data UpdateSmsChannel = UpdateSmsChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    sMSChannelRequest :: Types.SMSChannelRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSmsChannel' value with any optional fields omitted.
mkUpdateSmsChannel ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'sMSChannelRequest'
  Types.SMSChannelRequest ->
  UpdateSmsChannel
mkUpdateSmsChannel applicationId sMSChannelRequest =
  UpdateSmsChannel' {applicationId, sMSChannelRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscApplicationId :: Lens.Lens' UpdateSmsChannel Core.Text
uscApplicationId = Lens.field @"applicationId"
{-# DEPRECATED uscApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sMSChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscSMSChannelRequest :: Lens.Lens' UpdateSmsChannel Types.SMSChannelRequest
uscSMSChannelRequest = Lens.field @"sMSChannelRequest"
{-# DEPRECATED uscSMSChannelRequest "Use generic-lens or generic-optics with 'sMSChannelRequest' instead." #-}

instance Core.FromJSON UpdateSmsChannel where
  toJSON UpdateSmsChannel {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SMSChannelRequest" Core..= sMSChannelRequest)]
      )

instance Core.AWSRequest UpdateSmsChannel where
  type Rs UpdateSmsChannel = UpdateSmsChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/channels/sms")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSmsChannelResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateSmsChannelResponse' smart constructor.
data UpdateSmsChannelResponse = UpdateSmsChannelResponse'
  { sMSChannelResponse :: Types.SMSChannelResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSmsChannelResponse' value with any optional fields omitted.
mkUpdateSmsChannelResponse ::
  -- | 'sMSChannelResponse'
  Types.SMSChannelResponse ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateSmsChannelResponse
mkUpdateSmsChannelResponse sMSChannelResponse responseStatus =
  UpdateSmsChannelResponse' {sMSChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sMSChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrrsSMSChannelResponse :: Lens.Lens' UpdateSmsChannelResponse Types.SMSChannelResponse
uscrrsSMSChannelResponse = Lens.field @"sMSChannelResponse"
{-# DEPRECATED uscrrsSMSChannelResponse "Use generic-lens or generic-optics with 'sMSChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrrsResponseStatus :: Lens.Lens' UpdateSmsChannelResponse Core.Int
uscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
