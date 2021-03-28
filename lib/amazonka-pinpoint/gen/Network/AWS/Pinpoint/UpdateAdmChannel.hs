{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateAdmChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the ADM channel for an application or updates the status and settings of the ADM channel for an application.
module Network.AWS.Pinpoint.UpdateAdmChannel
    (
    -- * Creating a request
      UpdateAdmChannel (..)
    , mkUpdateAdmChannel
    -- ** Request lenses
    , uacApplicationId
    , uacADMChannelRequest

    -- * Destructuring the response
    , UpdateAdmChannelResponse (..)
    , mkUpdateAdmChannelResponse
    -- ** Response lenses
    , uacrrsADMChannelResponse
    , uacrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateAdmChannel' smart constructor.
data UpdateAdmChannel = UpdateAdmChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , aDMChannelRequest :: Types.ADMChannelRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAdmChannel' value with any optional fields omitted.
mkUpdateAdmChannel
    :: Core.Text -- ^ 'applicationId'
    -> Types.ADMChannelRequest -- ^ 'aDMChannelRequest'
    -> UpdateAdmChannel
mkUpdateAdmChannel applicationId aDMChannelRequest
  = UpdateAdmChannel'{applicationId, aDMChannelRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacApplicationId :: Lens.Lens' UpdateAdmChannel Core.Text
uacApplicationId = Lens.field @"applicationId"
{-# INLINEABLE uacApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aDMChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacADMChannelRequest :: Lens.Lens' UpdateAdmChannel Types.ADMChannelRequest
uacADMChannelRequest = Lens.field @"aDMChannelRequest"
{-# INLINEABLE uacADMChannelRequest #-}
{-# DEPRECATED aDMChannelRequest "Use generic-lens or generic-optics with 'aDMChannelRequest' instead"  #-}

instance Core.ToQuery UpdateAdmChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateAdmChannel where
        toHeaders UpdateAdmChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateAdmChannel where
        toJSON UpdateAdmChannel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ADMChannelRequest" Core..= aDMChannelRequest)])

instance Core.AWSRequest UpdateAdmChannel where
        type Rs UpdateAdmChannel = UpdateAdmChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/adm",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateAdmChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateAdmChannelResponse' smart constructor.
data UpdateAdmChannelResponse = UpdateAdmChannelResponse'
  { aDMChannelResponse :: Types.ADMChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAdmChannelResponse' value with any optional fields omitted.
mkUpdateAdmChannelResponse
    :: Types.ADMChannelResponse -- ^ 'aDMChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateAdmChannelResponse
mkUpdateAdmChannelResponse aDMChannelResponse responseStatus
  = UpdateAdmChannelResponse'{aDMChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aDMChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacrrsADMChannelResponse :: Lens.Lens' UpdateAdmChannelResponse Types.ADMChannelResponse
uacrrsADMChannelResponse = Lens.field @"aDMChannelResponse"
{-# INLINEABLE uacrrsADMChannelResponse #-}
{-# DEPRECATED aDMChannelResponse "Use generic-lens or generic-optics with 'aDMChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacrrsResponseStatus :: Lens.Lens' UpdateAdmChannelResponse Core.Int
uacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
