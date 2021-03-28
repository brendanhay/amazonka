{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeDeliveryChannels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about the specified delivery channel. If a delivery channel is not specified, this action returns the details of all delivery channels associated with the account.
module Network.AWS.Config.DescribeDeliveryChannels
    (
    -- * Creating a request
      DescribeDeliveryChannels (..)
    , mkDescribeDeliveryChannels
    -- ** Request lenses
    , ddcDeliveryChannelNames

    -- * Destructuring the response
    , DescribeDeliveryChannelsResponse (..)
    , mkDescribeDeliveryChannelsResponse
    -- ** Response lenses
    , ddcrrsDeliveryChannels
    , ddcrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'DescribeDeliveryChannels' action.
--
-- /See:/ 'mkDescribeDeliveryChannels' smart constructor.
newtype DescribeDeliveryChannels = DescribeDeliveryChannels'
  { deliveryChannelNames :: Core.Maybe [Types.ChannelName]
    -- ^ A list of delivery channel names.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDeliveryChannels' value with any optional fields omitted.
mkDescribeDeliveryChannels
    :: DescribeDeliveryChannels
mkDescribeDeliveryChannels
  = DescribeDeliveryChannels'{deliveryChannelNames = Core.Nothing}

-- | A list of delivery channel names.
--
-- /Note:/ Consider using 'deliveryChannelNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDeliveryChannelNames :: Lens.Lens' DescribeDeliveryChannels (Core.Maybe [Types.ChannelName])
ddcDeliveryChannelNames = Lens.field @"deliveryChannelNames"
{-# INLINEABLE ddcDeliveryChannelNames #-}
{-# DEPRECATED deliveryChannelNames "Use generic-lens or generic-optics with 'deliveryChannelNames' instead"  #-}

instance Core.ToQuery DescribeDeliveryChannels where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeDeliveryChannels where
        toHeaders DescribeDeliveryChannels{..}
          = Core.pure
              ("X-Amz-Target", "StarlingDoveService.DescribeDeliveryChannels")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeDeliveryChannels where
        toJSON DescribeDeliveryChannels{..}
          = Core.object
              (Core.catMaybes
                 [("DeliveryChannelNames" Core..=) Core.<$> deliveryChannelNames])

instance Core.AWSRequest DescribeDeliveryChannels where
        type Rs DescribeDeliveryChannels = DescribeDeliveryChannelsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeDeliveryChannelsResponse' Core.<$>
                   (x Core..:? "DeliveryChannels") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output for the 'DescribeDeliveryChannels' action.
--
-- /See:/ 'mkDescribeDeliveryChannelsResponse' smart constructor.
data DescribeDeliveryChannelsResponse = DescribeDeliveryChannelsResponse'
  { deliveryChannels :: Core.Maybe [Types.DeliveryChannel]
    -- ^ A list that contains the descriptions of the specified delivery channel.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDeliveryChannelsResponse' value with any optional fields omitted.
mkDescribeDeliveryChannelsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDeliveryChannelsResponse
mkDescribeDeliveryChannelsResponse responseStatus
  = DescribeDeliveryChannelsResponse'{deliveryChannels =
                                        Core.Nothing,
                                      responseStatus}

-- | A list that contains the descriptions of the specified delivery channel.
--
-- /Note:/ Consider using 'deliveryChannels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsDeliveryChannels :: Lens.Lens' DescribeDeliveryChannelsResponse (Core.Maybe [Types.DeliveryChannel])
ddcrrsDeliveryChannels = Lens.field @"deliveryChannels"
{-# INLINEABLE ddcrrsDeliveryChannels #-}
{-# DEPRECATED deliveryChannels "Use generic-lens or generic-optics with 'deliveryChannels' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsResponseStatus :: Lens.Lens' DescribeDeliveryChannelsResponse Core.Int
ddcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
