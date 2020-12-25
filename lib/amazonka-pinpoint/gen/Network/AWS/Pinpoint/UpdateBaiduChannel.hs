{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateBaiduChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the Baidu channel for an application or updates the status and settings of the Baidu channel for an application.
module Network.AWS.Pinpoint.UpdateBaiduChannel
  ( -- * Creating a request
    UpdateBaiduChannel (..),
    mkUpdateBaiduChannel,

    -- ** Request lenses
    ubcApplicationId,
    ubcBaiduChannelRequest,

    -- * Destructuring the response
    UpdateBaiduChannelResponse (..),
    mkUpdateBaiduChannelResponse,

    -- ** Response lenses
    ubcrrsBaiduChannelResponse,
    ubcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateBaiduChannel' smart constructor.
data UpdateBaiduChannel = UpdateBaiduChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    baiduChannelRequest :: Types.BaiduChannelRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBaiduChannel' value with any optional fields omitted.
mkUpdateBaiduChannel ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'baiduChannelRequest'
  Types.BaiduChannelRequest ->
  UpdateBaiduChannel
mkUpdateBaiduChannel applicationId baiduChannelRequest =
  UpdateBaiduChannel' {applicationId, baiduChannelRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubcApplicationId :: Lens.Lens' UpdateBaiduChannel Core.Text
ubcApplicationId = Lens.field @"applicationId"
{-# DEPRECATED ubcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'baiduChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubcBaiduChannelRequest :: Lens.Lens' UpdateBaiduChannel Types.BaiduChannelRequest
ubcBaiduChannelRequest = Lens.field @"baiduChannelRequest"
{-# DEPRECATED ubcBaiduChannelRequest "Use generic-lens or generic-optics with 'baiduChannelRequest' instead." #-}

instance Core.FromJSON UpdateBaiduChannel where
  toJSON UpdateBaiduChannel {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("BaiduChannelRequest" Core..= baiduChannelRequest)]
      )

instance Core.AWSRequest UpdateBaiduChannel where
  type Rs UpdateBaiduChannel = UpdateBaiduChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/channels/baidu")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBaiduChannelResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateBaiduChannelResponse' smart constructor.
data UpdateBaiduChannelResponse = UpdateBaiduChannelResponse'
  { baiduChannelResponse :: Types.BaiduChannelResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBaiduChannelResponse' value with any optional fields omitted.
mkUpdateBaiduChannelResponse ::
  -- | 'baiduChannelResponse'
  Types.BaiduChannelResponse ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateBaiduChannelResponse
mkUpdateBaiduChannelResponse baiduChannelResponse responseStatus =
  UpdateBaiduChannelResponse' {baiduChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'baiduChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubcrrsBaiduChannelResponse :: Lens.Lens' UpdateBaiduChannelResponse Types.BaiduChannelResponse
ubcrrsBaiduChannelResponse = Lens.field @"baiduChannelResponse"
{-# DEPRECATED ubcrrsBaiduChannelResponse "Use generic-lens or generic-optics with 'baiduChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubcrrsResponseStatus :: Lens.Lens' UpdateBaiduChannelResponse Core.Int
ubcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ubcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
