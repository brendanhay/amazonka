{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteBaiduChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the Baidu channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteBaiduChannel
  ( -- * Creating a request
    DeleteBaiduChannel (..),
    mkDeleteBaiduChannel,

    -- ** Request lenses
    dbcApplicationId,

    -- * Destructuring the response
    DeleteBaiduChannelResponse (..),
    mkDeleteBaiduChannelResponse,

    -- ** Response lenses
    dbcrrsBaiduChannelResponse,
    dbcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBaiduChannel' smart constructor.
newtype DeleteBaiduChannel = DeleteBaiduChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBaiduChannel' value with any optional fields omitted.
mkDeleteBaiduChannel ::
  -- | 'applicationId'
  Core.Text ->
  DeleteBaiduChannel
mkDeleteBaiduChannel applicationId =
  DeleteBaiduChannel' {applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcApplicationId :: Lens.Lens' DeleteBaiduChannel Core.Text
dbcApplicationId = Lens.field @"applicationId"
{-# DEPRECATED dbcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Core.AWSRequest DeleteBaiduChannel where
  type Rs DeleteBaiduChannel = DeleteBaiduChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/channels/baidu")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBaiduChannelResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteBaiduChannelResponse' smart constructor.
data DeleteBaiduChannelResponse = DeleteBaiduChannelResponse'
  { baiduChannelResponse :: Types.BaiduChannelResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBaiduChannelResponse' value with any optional fields omitted.
mkDeleteBaiduChannelResponse ::
  -- | 'baiduChannelResponse'
  Types.BaiduChannelResponse ->
  -- | 'responseStatus'
  Core.Int ->
  DeleteBaiduChannelResponse
mkDeleteBaiduChannelResponse baiduChannelResponse responseStatus =
  DeleteBaiduChannelResponse' {baiduChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'baiduChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcrrsBaiduChannelResponse :: Lens.Lens' DeleteBaiduChannelResponse Types.BaiduChannelResponse
dbcrrsBaiduChannelResponse = Lens.field @"baiduChannelResponse"
{-# DEPRECATED dbcrrsBaiduChannelResponse "Use generic-lens or generic-optics with 'baiduChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcrrsResponseStatus :: Lens.Lens' DeleteBaiduChannelResponse Core.Int
dbcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
