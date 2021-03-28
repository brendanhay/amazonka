{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateBaiduChannel (..)
    , mkUpdateBaiduChannel
    -- ** Request lenses
    , ubcApplicationId
    , ubcBaiduChannelRequest

    -- * Destructuring the response
    , UpdateBaiduChannelResponse (..)
    , mkUpdateBaiduChannelResponse
    -- ** Response lenses
    , ubcrrsBaiduChannelResponse
    , ubcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateBaiduChannel' smart constructor.
data UpdateBaiduChannel = UpdateBaiduChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , baiduChannelRequest :: Types.BaiduChannelRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBaiduChannel' value with any optional fields omitted.
mkUpdateBaiduChannel
    :: Core.Text -- ^ 'applicationId'
    -> Types.BaiduChannelRequest -- ^ 'baiduChannelRequest'
    -> UpdateBaiduChannel
mkUpdateBaiduChannel applicationId baiduChannelRequest
  = UpdateBaiduChannel'{applicationId, baiduChannelRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubcApplicationId :: Lens.Lens' UpdateBaiduChannel Core.Text
ubcApplicationId = Lens.field @"applicationId"
{-# INLINEABLE ubcApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'baiduChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubcBaiduChannelRequest :: Lens.Lens' UpdateBaiduChannel Types.BaiduChannelRequest
ubcBaiduChannelRequest = Lens.field @"baiduChannelRequest"
{-# INLINEABLE ubcBaiduChannelRequest #-}
{-# DEPRECATED baiduChannelRequest "Use generic-lens or generic-optics with 'baiduChannelRequest' instead"  #-}

instance Core.ToQuery UpdateBaiduChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateBaiduChannel where
        toHeaders UpdateBaiduChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateBaiduChannel where
        toJSON UpdateBaiduChannel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("BaiduChannelRequest" Core..= baiduChannelRequest)])

instance Core.AWSRequest UpdateBaiduChannel where
        type Rs UpdateBaiduChannel = UpdateBaiduChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/baidu",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateBaiduChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateBaiduChannelResponse' smart constructor.
data UpdateBaiduChannelResponse = UpdateBaiduChannelResponse'
  { baiduChannelResponse :: Types.BaiduChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBaiduChannelResponse' value with any optional fields omitted.
mkUpdateBaiduChannelResponse
    :: Types.BaiduChannelResponse -- ^ 'baiduChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateBaiduChannelResponse
mkUpdateBaiduChannelResponse baiduChannelResponse responseStatus
  = UpdateBaiduChannelResponse'{baiduChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'baiduChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubcrrsBaiduChannelResponse :: Lens.Lens' UpdateBaiduChannelResponse Types.BaiduChannelResponse
ubcrrsBaiduChannelResponse = Lens.field @"baiduChannelResponse"
{-# INLINEABLE ubcrrsBaiduChannelResponse #-}
{-# DEPRECATED baiduChannelResponse "Use generic-lens or generic-optics with 'baiduChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubcrrsResponseStatus :: Lens.Lens' UpdateBaiduChannelResponse Core.Int
ubcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ubcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
