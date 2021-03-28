{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteAdmChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the ADM channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteAdmChannel
    (
    -- * Creating a request
      DeleteAdmChannel (..)
    , mkDeleteAdmChannel
    -- ** Request lenses
    , dacApplicationId

    -- * Destructuring the response
    , DeleteAdmChannelResponse (..)
    , mkDeleteAdmChannelResponse
    -- ** Response lenses
    , dacrrsADMChannelResponse
    , dacrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAdmChannel' smart constructor.
newtype DeleteAdmChannel = DeleteAdmChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAdmChannel' value with any optional fields omitted.
mkDeleteAdmChannel
    :: Core.Text -- ^ 'applicationId'
    -> DeleteAdmChannel
mkDeleteAdmChannel applicationId = DeleteAdmChannel'{applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacApplicationId :: Lens.Lens' DeleteAdmChannel Core.Text
dacApplicationId = Lens.field @"applicationId"
{-# INLINEABLE dacApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery DeleteAdmChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteAdmChannel where
        toHeaders DeleteAdmChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteAdmChannel where
        type Rs DeleteAdmChannel = DeleteAdmChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/adm",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteAdmChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAdmChannelResponse' smart constructor.
data DeleteAdmChannelResponse = DeleteAdmChannelResponse'
  { aDMChannelResponse :: Types.ADMChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAdmChannelResponse' value with any optional fields omitted.
mkDeleteAdmChannelResponse
    :: Types.ADMChannelResponse -- ^ 'aDMChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteAdmChannelResponse
mkDeleteAdmChannelResponse aDMChannelResponse responseStatus
  = DeleteAdmChannelResponse'{aDMChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aDMChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacrrsADMChannelResponse :: Lens.Lens' DeleteAdmChannelResponse Types.ADMChannelResponse
dacrrsADMChannelResponse = Lens.field @"aDMChannelResponse"
{-# INLINEABLE dacrrsADMChannelResponse #-}
{-# DEPRECATED aDMChannelResponse "Use generic-lens or generic-optics with 'aDMChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacrrsResponseStatus :: Lens.Lens' DeleteAdmChannelResponse Core.Int
dacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
