{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetVoiceChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the voice channel for an application.
module Network.AWS.Pinpoint.GetVoiceChannel
    (
    -- * Creating a request
      GetVoiceChannel (..)
    , mkGetVoiceChannel
    -- ** Request lenses
    , gvcApplicationId

    -- * Destructuring the response
    , GetVoiceChannelResponse (..)
    , mkGetVoiceChannelResponse
    -- ** Response lenses
    , gvcrrsVoiceChannelResponse
    , gvcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetVoiceChannel' smart constructor.
newtype GetVoiceChannel = GetVoiceChannel'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetVoiceChannel' value with any optional fields omitted.
mkGetVoiceChannel
    :: Core.Text -- ^ 'applicationId'
    -> GetVoiceChannel
mkGetVoiceChannel applicationId = GetVoiceChannel'{applicationId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvcApplicationId :: Lens.Lens' GetVoiceChannel Core.Text
gvcApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gvcApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery GetVoiceChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetVoiceChannel where
        toHeaders GetVoiceChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetVoiceChannel where
        type Rs GetVoiceChannel = GetVoiceChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/channels/voice",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetVoiceChannelResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetVoiceChannelResponse' smart constructor.
data GetVoiceChannelResponse = GetVoiceChannelResponse'
  { voiceChannelResponse :: Types.VoiceChannelResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetVoiceChannelResponse' value with any optional fields omitted.
mkGetVoiceChannelResponse
    :: Types.VoiceChannelResponse -- ^ 'voiceChannelResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetVoiceChannelResponse
mkGetVoiceChannelResponse voiceChannelResponse responseStatus
  = GetVoiceChannelResponse'{voiceChannelResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'voiceChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvcrrsVoiceChannelResponse :: Lens.Lens' GetVoiceChannelResponse Types.VoiceChannelResponse
gvcrrsVoiceChannelResponse = Lens.field @"voiceChannelResponse"
{-# INLINEABLE gvcrrsVoiceChannelResponse #-}
{-# DEPRECATED voiceChannelResponse "Use generic-lens or generic-optics with 'voiceChannelResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvcrrsResponseStatus :: Lens.Lens' GetVoiceChannelResponse Core.Int
gvcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gvcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
