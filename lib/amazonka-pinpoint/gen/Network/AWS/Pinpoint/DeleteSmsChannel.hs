{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteSmsChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the SMS channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteSmsChannel
    (
    -- * Creating a request
      DeleteSmsChannel (..)
    , mkDeleteSmsChannel
    -- ** Request lenses
    , dscApplicationId

    -- * Destructuring the response
    , DeleteSmsChannelResponse (..)
    , mkDeleteSmsChannelResponse
    -- ** Response lenses
    , dscrrsSMSChannelResponse
    , dscrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSmsChannel' smart constructor.
newtype DeleteSmsChannel = DeleteSmsChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSmsChannel' value with any optional fields omitted.
mkDeleteSmsChannel
    :: Core.Text -- ^ 'applicationId'
    -> DeleteSmsChannel
mkDeleteSmsChannel applicationId = DeleteSmsChannel'{applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscApplicationId :: Lens.Lens' DeleteSmsChannel Core.Text
dscApplicationId = Lens.field @"applicationId"
{-# INLINEABLE dscApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery DeleteSmsChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteSmsChannel where
        toHeaders DeleteSmsChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteSmsChannel where
        type Rs DeleteSmsChannel = DeleteSmsChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/sms",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteSmsChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSmsChannelResponse' smart constructor.
data DeleteSmsChannelResponse = DeleteSmsChannelResponse'
  { sMSChannelResponse :: Types.SMSChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSmsChannelResponse' value with any optional fields omitted.
mkDeleteSmsChannelResponse
    :: Types.SMSChannelResponse -- ^ 'sMSChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteSmsChannelResponse
mkDeleteSmsChannelResponse sMSChannelResponse responseStatus
  = DeleteSmsChannelResponse'{sMSChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sMSChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrrsSMSChannelResponse :: Lens.Lens' DeleteSmsChannelResponse Types.SMSChannelResponse
dscrrsSMSChannelResponse = Lens.field @"sMSChannelResponse"
{-# INLINEABLE dscrrsSMSChannelResponse #-}
{-# DEPRECATED sMSChannelResponse "Use generic-lens or generic-optics with 'sMSChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrrsResponseStatus :: Lens.Lens' DeleteSmsChannelResponse Core.Int
dscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
