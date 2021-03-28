{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetApnsSandboxChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the APNs sandbox channel for an application.
module Network.AWS.Pinpoint.GetApnsSandboxChannel
    (
    -- * Creating a request
      GetApnsSandboxChannel (..)
    , mkGetApnsSandboxChannel
    -- ** Request lenses
    , gascApplicationId

    -- * Destructuring the response
    , GetApnsSandboxChannelResponse (..)
    , mkGetApnsSandboxChannelResponse
    -- ** Response lenses
    , gascrrsAPNSSandboxChannelResponse
    , gascrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetApnsSandboxChannel' smart constructor.
newtype GetApnsSandboxChannel = GetApnsSandboxChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetApnsSandboxChannel' value with any optional fields omitted.
mkGetApnsSandboxChannel
    :: Core.Text -- ^ 'applicationId'
    -> GetApnsSandboxChannel
mkGetApnsSandboxChannel applicationId
  = GetApnsSandboxChannel'{applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gascApplicationId :: Lens.Lens' GetApnsSandboxChannel Core.Text
gascApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gascApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery GetApnsSandboxChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetApnsSandboxChannel where
        toHeaders GetApnsSandboxChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetApnsSandboxChannel where
        type Rs GetApnsSandboxChannel = GetApnsSandboxChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/apns_sandbox",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetApnsSandboxChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetApnsSandboxChannelResponse' smart constructor.
data GetApnsSandboxChannelResponse = GetApnsSandboxChannelResponse'
  { aPNSSandboxChannelResponse :: Types.APNSSandboxChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetApnsSandboxChannelResponse' value with any optional fields omitted.
mkGetApnsSandboxChannelResponse
    :: Types.APNSSandboxChannelResponse -- ^ 'aPNSSandboxChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetApnsSandboxChannelResponse
mkGetApnsSandboxChannelResponse aPNSSandboxChannelResponse
  responseStatus
  = GetApnsSandboxChannelResponse'{aPNSSandboxChannelResponse,
                                   responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPNSSandboxChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gascrrsAPNSSandboxChannelResponse :: Lens.Lens' GetApnsSandboxChannelResponse Types.APNSSandboxChannelResponse
gascrrsAPNSSandboxChannelResponse = Lens.field @"aPNSSandboxChannelResponse"
{-# INLINEABLE gascrrsAPNSSandboxChannelResponse #-}
{-# DEPRECATED aPNSSandboxChannelResponse "Use generic-lens or generic-optics with 'aPNSSandboxChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gascrrsResponseStatus :: Lens.Lens' GetApnsSandboxChannelResponse Core.Int
gascrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gascrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
