{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.StopDeliveryStreamEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables server-side encryption (SSE) for the delivery stream. 
--
-- This operation is asynchronous. It returns immediately. When you invoke it, Kinesis Data Firehose first sets the encryption status of the stream to @DISABLING@ , and then to @DISABLED@ . You can continue to read and write data to your stream while its status is @DISABLING@ . It can take up to 5 seconds after the encryption status changes to @DISABLED@ before all records written to the delivery stream are no longer subject to encryption. To find out whether a record or a batch of records was encrypted, check the response elements 'PutRecordOutput$Encrypted' and 'PutRecordBatchOutput$Encrypted' , respectively.
-- To check the encryption state of a delivery stream, use 'DescribeDeliveryStream' . 
-- If SSE is enabled using a customer managed CMK and then you invoke @StopDeliveryStreamEncryption@ , Kinesis Data Firehose schedules the related KMS grant for retirement and then retires it after it ensures that it is finished delivering records to the destination.
-- The @StartDeliveryStreamEncryption@ and @StopDeliveryStreamEncryption@ operations have a combined limit of 25 calls per delivery stream per 24 hours. For example, you reach the limit if you call @StartDeliveryStreamEncryption@ 13 times and @StopDeliveryStreamEncryption@ 12 times for the same delivery stream in a 24-hour period.
module Network.AWS.Firehose.StopDeliveryStreamEncryption
    (
    -- * Creating a request
      StopDeliveryStreamEncryption (..)
    , mkStopDeliveryStreamEncryption
    -- ** Request lenses
    , sdseDeliveryStreamName

    -- * Destructuring the response
    , StopDeliveryStreamEncryptionResponse (..)
    , mkStopDeliveryStreamEncryptionResponse
    -- ** Response lenses
    , sdserrsResponseStatus
    ) where

import qualified Network.AWS.Firehose.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopDeliveryStreamEncryption' smart constructor.
newtype StopDeliveryStreamEncryption = StopDeliveryStreamEncryption'
  { deliveryStreamName :: Types.DeliveryStreamName
    -- ^ The name of the delivery stream for which you want to disable server-side encryption (SSE).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopDeliveryStreamEncryption' value with any optional fields omitted.
mkStopDeliveryStreamEncryption
    :: Types.DeliveryStreamName -- ^ 'deliveryStreamName'
    -> StopDeliveryStreamEncryption
mkStopDeliveryStreamEncryption deliveryStreamName
  = StopDeliveryStreamEncryption'{deliveryStreamName}

-- | The name of the delivery stream for which you want to disable server-side encryption (SSE).
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdseDeliveryStreamName :: Lens.Lens' StopDeliveryStreamEncryption Types.DeliveryStreamName
sdseDeliveryStreamName = Lens.field @"deliveryStreamName"
{-# INLINEABLE sdseDeliveryStreamName #-}
{-# DEPRECATED deliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead"  #-}

instance Core.ToQuery StopDeliveryStreamEncryption where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopDeliveryStreamEncryption where
        toHeaders StopDeliveryStreamEncryption{..}
          = Core.pure
              ("X-Amz-Target", "Firehose_20150804.StopDeliveryStreamEncryption")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopDeliveryStreamEncryption where
        toJSON StopDeliveryStreamEncryption{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DeliveryStreamName" Core..= deliveryStreamName)])

instance Core.AWSRequest StopDeliveryStreamEncryption where
        type Rs StopDeliveryStreamEncryption =
             StopDeliveryStreamEncryptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StopDeliveryStreamEncryptionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopDeliveryStreamEncryptionResponse' smart constructor.
newtype StopDeliveryStreamEncryptionResponse = StopDeliveryStreamEncryptionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopDeliveryStreamEncryptionResponse' value with any optional fields omitted.
mkStopDeliveryStreamEncryptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopDeliveryStreamEncryptionResponse
mkStopDeliveryStreamEncryptionResponse responseStatus
  = StopDeliveryStreamEncryptionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdserrsResponseStatus :: Lens.Lens' StopDeliveryStreamEncryptionResponse Core.Int
sdserrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sdserrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
