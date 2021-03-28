{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetBaiduChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the Baidu channel for an application.
module Network.AWS.Pinpoint.GetBaiduChannel
    (
    -- * Creating a request
      GetBaiduChannel (..)
    , mkGetBaiduChannel
    -- ** Request lenses
    , gbcApplicationId

    -- * Destructuring the response
    , GetBaiduChannelResponse (..)
    , mkGetBaiduChannelResponse
    -- ** Response lenses
    , gbcrrsBaiduChannelResponse
    , gbcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBaiduChannel' smart constructor.
newtype GetBaiduChannel = GetBaiduChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetBaiduChannel' value with any optional fields omitted.
mkGetBaiduChannel
    :: Core.Text -- ^ 'applicationId'
    -> GetBaiduChannel
mkGetBaiduChannel applicationId = GetBaiduChannel'{applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcApplicationId :: Lens.Lens' GetBaiduChannel Core.Text
gbcApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gbcApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery GetBaiduChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetBaiduChannel where
        toHeaders GetBaiduChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetBaiduChannel where
        type Rs GetBaiduChannel = GetBaiduChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/baidu",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetBaiduChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetBaiduChannelResponse' smart constructor.
data GetBaiduChannelResponse = GetBaiduChannelResponse'
  { baiduChannelResponse :: Types.BaiduChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBaiduChannelResponse' value with any optional fields omitted.
mkGetBaiduChannelResponse
    :: Types.BaiduChannelResponse -- ^ 'baiduChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetBaiduChannelResponse
mkGetBaiduChannelResponse baiduChannelResponse responseStatus
  = GetBaiduChannelResponse'{baiduChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'baiduChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcrrsBaiduChannelResponse :: Lens.Lens' GetBaiduChannelResponse Types.BaiduChannelResponse
gbcrrsBaiduChannelResponse = Lens.field @"baiduChannelResponse"
{-# INLINEABLE gbcrrsBaiduChannelResponse #-}
{-# DEPRECATED baiduChannelResponse "Use generic-lens or generic-optics with 'baiduChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcrrsResponseStatus :: Lens.Lens' GetBaiduChannelResponse Core.Int
gbcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gbcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
