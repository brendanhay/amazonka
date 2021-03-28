{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeDeliveryChannelStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of the specified delivery channel. If a delivery channel is not specified, this action returns the current status of all delivery channels associated with the account.
module Network.AWS.Config.DescribeDeliveryChannelStatus
    (
    -- * Creating a request
      DescribeDeliveryChannelStatus (..)
    , mkDescribeDeliveryChannelStatus
    -- ** Request lenses
    , ddcsDeliveryChannelNames

    -- * Destructuring the response
    , DescribeDeliveryChannelStatusResponse (..)
    , mkDescribeDeliveryChannelStatusResponse
    -- ** Response lenses
    , ddcsrrsDeliveryChannelsStatus
    , ddcsrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'DeliveryChannelStatus' action.
--
-- /See:/ 'mkDescribeDeliveryChannelStatus' smart constructor.
newtype DescribeDeliveryChannelStatus = DescribeDeliveryChannelStatus'
  { deliveryChannelNames :: Core.Maybe [Types.ChannelName]
    -- ^ A list of delivery channel names.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDeliveryChannelStatus' value with any optional fields omitted.
mkDescribeDeliveryChannelStatus
    :: DescribeDeliveryChannelStatus
mkDescribeDeliveryChannelStatus
  = DescribeDeliveryChannelStatus'{deliveryChannelNames =
                                     Core.Nothing}

-- | A list of delivery channel names.
--
-- /Note:/ Consider using 'deliveryChannelNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcsDeliveryChannelNames :: Lens.Lens' DescribeDeliveryChannelStatus (Core.Maybe [Types.ChannelName])
ddcsDeliveryChannelNames = Lens.field @"deliveryChannelNames"
{-# INLINEABLE ddcsDeliveryChannelNames #-}
{-# DEPRECATED deliveryChannelNames "Use generic-lens or generic-optics with 'deliveryChannelNames' instead"  #-}

instance Core.ToQuery DescribeDeliveryChannelStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeDeliveryChannelStatus where
        toHeaders DescribeDeliveryChannelStatus{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.DescribeDeliveryChannelStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeDeliveryChannelStatus where
        toJSON DescribeDeliveryChannelStatus{..}
          = Core.object
              (Core.catMaybes
                 [("DeliveryChannelNames" Core..=) Core.<$> deliveryChannelNames])

instance Core.AWSRequest DescribeDeliveryChannelStatus where
        type Rs DescribeDeliveryChannelStatus =
             DescribeDeliveryChannelStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeDeliveryChannelStatusResponse' Core.<$>
                   (x Core..:? "DeliveryChannelsStatus") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output for the 'DescribeDeliveryChannelStatus' action.
--
-- /See:/ 'mkDescribeDeliveryChannelStatusResponse' smart constructor.
data DescribeDeliveryChannelStatusResponse = DescribeDeliveryChannelStatusResponse'
  { deliveryChannelsStatus :: Core.Maybe [Types.DeliveryChannelStatus]
    -- ^ A list that contains the status of a specified delivery channel.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeDeliveryChannelStatusResponse' value with any optional fields omitted.
mkDescribeDeliveryChannelStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDeliveryChannelStatusResponse
mkDescribeDeliveryChannelStatusResponse responseStatus
  = DescribeDeliveryChannelStatusResponse'{deliveryChannelsStatus =
                                             Core.Nothing,
                                           responseStatus}

-- | A list that contains the status of a specified delivery channel.
--
-- /Note:/ Consider using 'deliveryChannelsStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcsrrsDeliveryChannelsStatus :: Lens.Lens' DescribeDeliveryChannelStatusResponse (Core.Maybe [Types.DeliveryChannelStatus])
ddcsrrsDeliveryChannelsStatus = Lens.field @"deliveryChannelsStatus"
{-# INLINEABLE ddcsrrsDeliveryChannelsStatus #-}
{-# DEPRECATED deliveryChannelsStatus "Use generic-lens or generic-optics with 'deliveryChannelsStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcsrrsResponseStatus :: Lens.Lens' DescribeDeliveryChannelStatusResponse Core.Int
ddcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
