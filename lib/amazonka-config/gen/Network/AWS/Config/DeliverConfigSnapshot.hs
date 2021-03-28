{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeliverConfigSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules delivery of a configuration snapshot to the Amazon S3 bucket in the specified delivery channel. After the delivery has started, AWS Config sends the following notifications using an Amazon SNS topic that you have specified.
--
--
--     * Notification of the start of the delivery.
--
--
--     * Notification of the completion of the delivery, if the delivery was successfully completed.
--
--
--     * Notification of delivery failure, if the delivery failed.
--
--
module Network.AWS.Config.DeliverConfigSnapshot
    (
    -- * Creating a request
      DeliverConfigSnapshot (..)
    , mkDeliverConfigSnapshot
    -- ** Request lenses
    , dcsDeliveryChannelName

    -- * Destructuring the response
    , DeliverConfigSnapshotResponse (..)
    , mkDeliverConfigSnapshotResponse
    -- ** Response lenses
    , dcsrrsConfigSnapshotId
    , dcsrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'DeliverConfigSnapshot' action.
--
-- /See:/ 'mkDeliverConfigSnapshot' smart constructor.
newtype DeliverConfigSnapshot = DeliverConfigSnapshot'
  { deliveryChannelName :: Types.DeliveryChannelName
    -- ^ The name of the delivery channel through which the snapshot is delivered.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeliverConfigSnapshot' value with any optional fields omitted.
mkDeliverConfigSnapshot
    :: Types.DeliveryChannelName -- ^ 'deliveryChannelName'
    -> DeliverConfigSnapshot
mkDeliverConfigSnapshot deliveryChannelName
  = DeliverConfigSnapshot'{deliveryChannelName}

-- | The name of the delivery channel through which the snapshot is delivered.
--
-- /Note:/ Consider using 'deliveryChannelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsDeliveryChannelName :: Lens.Lens' DeliverConfigSnapshot Types.DeliveryChannelName
dcsDeliveryChannelName = Lens.field @"deliveryChannelName"
{-# INLINEABLE dcsDeliveryChannelName #-}
{-# DEPRECATED deliveryChannelName "Use generic-lens or generic-optics with 'deliveryChannelName' instead"  #-}

instance Core.ToQuery DeliverConfigSnapshot where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeliverConfigSnapshot where
        toHeaders DeliverConfigSnapshot{..}
          = Core.pure
              ("X-Amz-Target", "StarlingDoveService.DeliverConfigSnapshot")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeliverConfigSnapshot where
        toJSON DeliverConfigSnapshot{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("deliveryChannelName" Core..= deliveryChannelName)])

instance Core.AWSRequest DeliverConfigSnapshot where
        type Rs DeliverConfigSnapshot = DeliverConfigSnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeliverConfigSnapshotResponse' Core.<$>
                   (x Core..:? "configSnapshotId") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output for the 'DeliverConfigSnapshot' action, in JSON format.
--
-- /See:/ 'mkDeliverConfigSnapshotResponse' smart constructor.
data DeliverConfigSnapshotResponse = DeliverConfigSnapshotResponse'
  { configSnapshotId :: Core.Maybe Core.Text
    -- ^ The ID of the snapshot that is being created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeliverConfigSnapshotResponse' value with any optional fields omitted.
mkDeliverConfigSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeliverConfigSnapshotResponse
mkDeliverConfigSnapshotResponse responseStatus
  = DeliverConfigSnapshotResponse'{configSnapshotId = Core.Nothing,
                                   responseStatus}

-- | The ID of the snapshot that is being created.
--
-- /Note:/ Consider using 'configSnapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrrsConfigSnapshotId :: Lens.Lens' DeliverConfigSnapshotResponse (Core.Maybe Core.Text)
dcsrrsConfigSnapshotId = Lens.field @"configSnapshotId"
{-# INLINEABLE dcsrrsConfigSnapshotId #-}
{-# DEPRECATED configSnapshotId "Use generic-lens or generic-optics with 'configSnapshotId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrrsResponseStatus :: Lens.Lens' DeliverConfigSnapshotResponse Core.Int
dcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
