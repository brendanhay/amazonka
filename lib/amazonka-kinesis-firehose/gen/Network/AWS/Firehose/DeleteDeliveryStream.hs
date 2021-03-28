{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.DeleteDeliveryStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a delivery stream and its data.
--
-- To check the state of a delivery stream, use 'DescribeDeliveryStream' . You can delete a delivery stream only if it is in one of the following states: @ACTIVE@ , @DELETING@ , @CREATING_FAILED@ , or @DELETING_FAILED@ . You can't delete a delivery stream that is in the @CREATING@ state. While the deletion request is in process, the delivery stream is in the @DELETING@ state.
-- While the delivery stream is in the @DELETING@ state, the service might continue to accept records, but it doesn't make any guarantees with respect to delivering the data. Therefore, as a best practice, first stop any applications that are sending records before you delete a delivery stream.
module Network.AWS.Firehose.DeleteDeliveryStream
    (
    -- * Creating a request
      DeleteDeliveryStream (..)
    , mkDeleteDeliveryStream
    -- ** Request lenses
    , dDeliveryStreamName
    , dAllowForceDelete

    -- * Destructuring the response
    , DeleteDeliveryStreamResponse (..)
    , mkDeleteDeliveryStreamResponse
    -- ** Response lenses
    , drsResponseStatus
    ) where

import qualified Network.AWS.Firehose.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDeliveryStream' smart constructor.
data DeleteDeliveryStream = DeleteDeliveryStream'
  { deliveryStreamName :: Types.DeliveryStreamName
    -- ^ The name of the delivery stream.
  , allowForceDelete :: Core.Maybe Core.Bool
    -- ^ Set this to true if you want to delete the delivery stream even if Kinesis Data Firehose is unable to retire the grant for the CMK. Kinesis Data Firehose might be unable to retire the grant due to a customer error, such as when the CMK or the grant are in an invalid state. If you force deletion, you can then use the <https://docs.aws.amazon.com/kms/latest/APIReference/API_RevokeGrant.html RevokeGrant> operation to revoke the grant you gave to Kinesis Data Firehose. If a failure to retire the grant happens due to an AWS KMS issue, Kinesis Data Firehose keeps retrying the delete operation.
--
-- The default value is false.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeliveryStream' value with any optional fields omitted.
mkDeleteDeliveryStream
    :: Types.DeliveryStreamName -- ^ 'deliveryStreamName'
    -> DeleteDeliveryStream
mkDeleteDeliveryStream deliveryStreamName
  = DeleteDeliveryStream'{deliveryStreamName,
                          allowForceDelete = Core.Nothing}

-- | The name of the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeliveryStreamName :: Lens.Lens' DeleteDeliveryStream Types.DeliveryStreamName
dDeliveryStreamName = Lens.field @"deliveryStreamName"
{-# INLINEABLE dDeliveryStreamName #-}
{-# DEPRECATED deliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead"  #-}

-- | Set this to true if you want to delete the delivery stream even if Kinesis Data Firehose is unable to retire the grant for the CMK. Kinesis Data Firehose might be unable to retire the grant due to a customer error, such as when the CMK or the grant are in an invalid state. If you force deletion, you can then use the <https://docs.aws.amazon.com/kms/latest/APIReference/API_RevokeGrant.html RevokeGrant> operation to revoke the grant you gave to Kinesis Data Firehose. If a failure to retire the grant happens due to an AWS KMS issue, Kinesis Data Firehose keeps retrying the delete operation.
--
-- The default value is false.
--
-- /Note:/ Consider using 'allowForceDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAllowForceDelete :: Lens.Lens' DeleteDeliveryStream (Core.Maybe Core.Bool)
dAllowForceDelete = Lens.field @"allowForceDelete"
{-# INLINEABLE dAllowForceDelete #-}
{-# DEPRECATED allowForceDelete "Use generic-lens or generic-optics with 'allowForceDelete' instead"  #-}

instance Core.ToQuery DeleteDeliveryStream where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDeliveryStream where
        toHeaders DeleteDeliveryStream{..}
          = Core.pure
              ("X-Amz-Target", "Firehose_20150804.DeleteDeliveryStream")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteDeliveryStream where
        toJSON DeleteDeliveryStream{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DeliveryStreamName" Core..= deliveryStreamName),
                  ("AllowForceDelete" Core..=) Core.<$> allowForceDelete])

instance Core.AWSRequest DeleteDeliveryStream where
        type Rs DeleteDeliveryStream = DeleteDeliveryStreamResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteDeliveryStreamResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDeliveryStreamResponse' smart constructor.
newtype DeleteDeliveryStreamResponse = DeleteDeliveryStreamResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeliveryStreamResponse' value with any optional fields omitted.
mkDeleteDeliveryStreamResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDeliveryStreamResponse
mkDeleteDeliveryStreamResponse responseStatus
  = DeleteDeliveryStreamResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteDeliveryStreamResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
