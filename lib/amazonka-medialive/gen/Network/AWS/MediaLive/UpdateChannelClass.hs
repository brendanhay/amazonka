{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.UpdateChannelClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the class of the channel.
module Network.AWS.MediaLive.UpdateChannelClass
    (
    -- * Creating a request
      UpdateChannelClass (..)
    , mkUpdateChannelClass
    -- ** Request lenses
    , uccChannelId
    , uccChannelClass
    , uccDestinations

    -- * Destructuring the response
    , UpdateChannelClassResponse (..)
    , mkUpdateChannelClassResponse
    -- ** Response lenses
    , uccrrsChannel
    , uccrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Channel class that the channel should be updated to.
--
-- /See:/ 'mkUpdateChannelClass' smart constructor.
data UpdateChannelClass = UpdateChannelClass'
  { channelId :: Core.Text
    -- ^ Channel Id of the channel whose class should be updated.
  , channelClass :: Types.ChannelClass
    -- ^ The channel class that you wish to update this channel to use.
  , destinations :: Core.Maybe [Types.OutputDestination]
    -- ^ A list of output destinations for this channel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateChannelClass' value with any optional fields omitted.
mkUpdateChannelClass
    :: Core.Text -- ^ 'channelId'
    -> Types.ChannelClass -- ^ 'channelClass'
    -> UpdateChannelClass
mkUpdateChannelClass channelId channelClass
  = UpdateChannelClass'{channelId, channelClass,
                        destinations = Core.Nothing}

-- | Channel Id of the channel whose class should be updated.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccChannelId :: Lens.Lens' UpdateChannelClass Core.Text
uccChannelId = Lens.field @"channelId"
{-# INLINEABLE uccChannelId #-}
{-# DEPRECATED channelId "Use generic-lens or generic-optics with 'channelId' instead"  #-}

-- | The channel class that you wish to update this channel to use.
--
-- /Note:/ Consider using 'channelClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccChannelClass :: Lens.Lens' UpdateChannelClass Types.ChannelClass
uccChannelClass = Lens.field @"channelClass"
{-# INLINEABLE uccChannelClass #-}
{-# DEPRECATED channelClass "Use generic-lens or generic-optics with 'channelClass' instead"  #-}

-- | A list of output destinations for this channel.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccDestinations :: Lens.Lens' UpdateChannelClass (Core.Maybe [Types.OutputDestination])
uccDestinations = Lens.field @"destinations"
{-# INLINEABLE uccDestinations #-}
{-# DEPRECATED destinations "Use generic-lens or generic-optics with 'destinations' instead"  #-}

instance Core.ToQuery UpdateChannelClass where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateChannelClass where
        toHeaders UpdateChannelClass{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateChannelClass where
        toJSON UpdateChannelClass{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("channelClass" Core..= channelClass),
                  ("destinations" Core..=) Core.<$> destinations])

instance Core.AWSRequest UpdateChannelClass where
        type Rs UpdateChannelClass = UpdateChannelClassResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/prod/channels/" Core.<> Core.toText channelId Core.<>
                             "/channelClass",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateChannelClassResponse' Core.<$>
                   (x Core..:? "channel") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for UpdateChannelClassResponse
--
-- /See:/ 'mkUpdateChannelClassResponse' smart constructor.
data UpdateChannelClassResponse = UpdateChannelClassResponse'
  { channel :: Core.Maybe Types.Channel
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateChannelClassResponse' value with any optional fields omitted.
mkUpdateChannelClassResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateChannelClassResponse
mkUpdateChannelClassResponse responseStatus
  = UpdateChannelClassResponse'{channel = Core.Nothing,
                                responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccrrsChannel :: Lens.Lens' UpdateChannelClassResponse (Core.Maybe Types.Channel)
uccrrsChannel = Lens.field @"channel"
{-# INLINEABLE uccrrsChannel #-}
{-# DEPRECATED channel "Use generic-lens or generic-optics with 'channel' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccrrsResponseStatus :: Lens.Lens' UpdateChannelClassResponse Core.Int
uccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
