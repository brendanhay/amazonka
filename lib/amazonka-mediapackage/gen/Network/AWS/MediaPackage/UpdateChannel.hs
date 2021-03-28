{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.UpdateChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Channel.
module Network.AWS.MediaPackage.UpdateChannel
    (
    -- * Creating a request
      UpdateChannel (..)
    , mkUpdateChannel
    -- ** Request lenses
    , ucId
    , ucDescription

    -- * Destructuring the response
    , UpdateChannelResponse (..)
    , mkUpdateChannelResponse
    -- ** Response lenses
    , ucrrsArn
    , ucrrsDescription
    , ucrrsEgressAccessLogs
    , ucrrsHlsIngest
    , ucrrsId
    , ucrrsIngressAccessLogs
    , ucrrsTags
    , ucrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Configuration parameters used to update the Channel.
--
-- /See:/ 'mkUpdateChannel' smart constructor.
data UpdateChannel = UpdateChannel'
  { id :: Core.Text
    -- ^ The ID of the Channel to update.
  , description :: Core.Maybe Core.Text
    -- ^ A short text description of the Channel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateChannel' value with any optional fields omitted.
mkUpdateChannel
    :: Core.Text -- ^ 'id'
    -> UpdateChannel
mkUpdateChannel id = UpdateChannel'{id, description = Core.Nothing}

-- | The ID of the Channel to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucId :: Lens.Lens' UpdateChannel Core.Text
ucId = Lens.field @"id"
{-# INLINEABLE ucId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A short text description of the Channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDescription :: Lens.Lens' UpdateChannel (Core.Maybe Core.Text)
ucDescription = Lens.field @"description"
{-# INLINEABLE ucDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery UpdateChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateChannel where
        toHeaders UpdateChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateChannel where
        toJSON UpdateChannel{..}
          = Core.object
              (Core.catMaybes [("description" Core..=) Core.<$> description])

instance Core.AWSRequest UpdateChannel where
        type Rs UpdateChannel = UpdateChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/channels/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateChannelResponse' Core.<$>
                   (x Core..:? "arn") Core.<*> x Core..:? "description" Core.<*>
                     x Core..:? "egressAccessLogs"
                     Core.<*> x Core..:? "hlsIngest"
                     Core.<*> x Core..:? "id"
                     Core.<*> x Core..:? "ingressAccessLogs"
                     Core.<*> x Core..:? "tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateChannelResponse' smart constructor.
data UpdateChannelResponse = UpdateChannelResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) assigned to the Channel.
  , description :: Core.Maybe Core.Text
    -- ^ A short text description of the Channel.
  , egressAccessLogs :: Core.Maybe Types.EgressAccessLogs
  , hlsIngest :: Core.Maybe Types.HlsIngest
  , id :: Core.Maybe Core.Text
    -- ^ The ID of the Channel.
  , ingressAccessLogs :: Core.Maybe Types.IngressAccessLogs
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateChannelResponse' value with any optional fields omitted.
mkUpdateChannelResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateChannelResponse
mkUpdateChannelResponse responseStatus
  = UpdateChannelResponse'{arn = Core.Nothing,
                           description = Core.Nothing, egressAccessLogs = Core.Nothing,
                           hlsIngest = Core.Nothing, id = Core.Nothing,
                           ingressAccessLogs = Core.Nothing, tags = Core.Nothing,
                           responseStatus}

-- | The Amazon Resource Name (ARN) assigned to the Channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsArn :: Lens.Lens' UpdateChannelResponse (Core.Maybe Core.Text)
ucrrsArn = Lens.field @"arn"
{-# INLINEABLE ucrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | A short text description of the Channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsDescription :: Lens.Lens' UpdateChannelResponse (Core.Maybe Core.Text)
ucrrsDescription = Lens.field @"description"
{-# INLINEABLE ucrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'egressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsEgressAccessLogs :: Lens.Lens' UpdateChannelResponse (Core.Maybe Types.EgressAccessLogs)
ucrrsEgressAccessLogs = Lens.field @"egressAccessLogs"
{-# INLINEABLE ucrrsEgressAccessLogs #-}
{-# DEPRECATED egressAccessLogs "Use generic-lens or generic-optics with 'egressAccessLogs' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsIngest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsHlsIngest :: Lens.Lens' UpdateChannelResponse (Core.Maybe Types.HlsIngest)
ucrrsHlsIngest = Lens.field @"hlsIngest"
{-# INLINEABLE ucrrsHlsIngest #-}
{-# DEPRECATED hlsIngest "Use generic-lens or generic-optics with 'hlsIngest' instead"  #-}

-- | The ID of the Channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsId :: Lens.Lens' UpdateChannelResponse (Core.Maybe Core.Text)
ucrrsId = Lens.field @"id"
{-# INLINEABLE ucrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ingressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsIngressAccessLogs :: Lens.Lens' UpdateChannelResponse (Core.Maybe Types.IngressAccessLogs)
ucrrsIngressAccessLogs = Lens.field @"ingressAccessLogs"
{-# INLINEABLE ucrrsIngressAccessLogs #-}
{-# DEPRECATED ingressAccessLogs "Use generic-lens or generic-optics with 'ingressAccessLogs' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsTags :: Lens.Lens' UpdateChannelResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
ucrrsTags = Lens.field @"tags"
{-# INLINEABLE ucrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsResponseStatus :: Lens.Lens' UpdateChannelResponse Core.Int
ucrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
