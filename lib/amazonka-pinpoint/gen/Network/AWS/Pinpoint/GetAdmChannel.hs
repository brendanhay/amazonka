{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetAdmChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the ADM channel for an application.
module Network.AWS.Pinpoint.GetAdmChannel
    (
    -- * Creating a request
      GetAdmChannel (..)
    , mkGetAdmChannel
    -- ** Request lenses
    , gacApplicationId

    -- * Destructuring the response
    , GetAdmChannelResponse (..)
    , mkGetAdmChannelResponse
    -- ** Response lenses
    , gacrfrsADMChannelResponse
    , gacrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAdmChannel' smart constructor.
newtype GetAdmChannel = GetAdmChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetAdmChannel' value with any optional fields omitted.
mkGetAdmChannel
    :: Core.Text -- ^ 'applicationId'
    -> GetAdmChannel
mkGetAdmChannel applicationId = GetAdmChannel'{applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacApplicationId :: Lens.Lens' GetAdmChannel Core.Text
gacApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gacApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery GetAdmChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAdmChannel where
        toHeaders GetAdmChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetAdmChannel where
        type Rs GetAdmChannel = GetAdmChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/adm",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAdmChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAdmChannelResponse' smart constructor.
data GetAdmChannelResponse = GetAdmChannelResponse'
  { aDMChannelResponse :: Types.ADMChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAdmChannelResponse' value with any optional fields omitted.
mkGetAdmChannelResponse
    :: Types.ADMChannelResponse -- ^ 'aDMChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetAdmChannelResponse
mkGetAdmChannelResponse aDMChannelResponse responseStatus
  = GetAdmChannelResponse'{aDMChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aDMChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrfrsADMChannelResponse :: Lens.Lens' GetAdmChannelResponse Types.ADMChannelResponse
gacrfrsADMChannelResponse = Lens.field @"aDMChannelResponse"
{-# INLINEABLE gacrfrsADMChannelResponse #-}
{-# DEPRECATED aDMChannelResponse "Use generic-lens or generic-optics with 'aDMChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrfrsResponseStatus :: Lens.Lens' GetAdmChannelResponse Core.Int
gacrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gacrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
