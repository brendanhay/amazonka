{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.StartDeliveryStreamEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables server-side encryption (SSE) for the delivery stream. 
--
-- This operation is asynchronous. It returns immediately. When you invoke it, Kinesis Data Firehose first sets the encryption status of the stream to @ENABLING@ , and then to @ENABLED@ . The encryption status of a delivery stream is the @Status@ property in 'DeliveryStreamEncryptionConfiguration' . If the operation fails, the encryption status changes to @ENABLING_FAILED@ . You can continue to read and write data to your delivery stream while the encryption status is @ENABLING@ , but the data is not encrypted. It can take up to 5 seconds after the encryption status changes to @ENABLED@ before all records written to the delivery stream are encrypted. To find out whether a record or a batch of records was encrypted, check the response elements 'PutRecordOutput$Encrypted' and 'PutRecordBatchOutput$Encrypted' , respectively.
-- To check the encryption status of a delivery stream, use 'DescribeDeliveryStream' .
-- Even if encryption is currently enabled for a delivery stream, you can still invoke this operation on it to change the ARN of the CMK or both its type and ARN. If you invoke this method to change the CMK, and the old CMK is of type @CUSTOMER_MANAGED_CMK@ , Kinesis Data Firehose schedules the grant it had on the old CMK for retirement. If the new CMK is of type @CUSTOMER_MANAGED_CMK@ , Kinesis Data Firehose creates a grant that enables it to use the new CMK to encrypt and decrypt data and to manage the grant.
-- If a delivery stream already has encryption enabled and then you invoke this operation to change the ARN of the CMK or both its type and ARN and you get @ENABLING_FAILED@ , this only means that the attempt to change the CMK failed. In this case, encryption remains enabled with the old CMK.
-- If the encryption status of your delivery stream is @ENABLING_FAILED@ , you can invoke this operation again with a valid CMK. The CMK must be enabled and the key policy mustn't explicitly deny the permission for Kinesis Data Firehose to invoke KMS encrypt and decrypt operations.
-- You can enable SSE for a delivery stream only if it's a delivery stream that uses @DirectPut@ as its source. 
-- The @StartDeliveryStreamEncryption@ and @StopDeliveryStreamEncryption@ operations have a combined limit of 25 calls per delivery stream per 24 hours. For example, you reach the limit if you call @StartDeliveryStreamEncryption@ 13 times and @StopDeliveryStreamEncryption@ 12 times for the same delivery stream in a 24-hour period.
module Network.AWS.Firehose.StartDeliveryStreamEncryption
    (
    -- * Creating a request
      StartDeliveryStreamEncryption (..)
    , mkStartDeliveryStreamEncryption
    -- ** Request lenses
    , sDeliveryStreamName
    , sDeliveryStreamEncryptionConfigurationInput

    -- * Destructuring the response
    , StartDeliveryStreamEncryptionResponse (..)
    , mkStartDeliveryStreamEncryptionResponse
    -- ** Response lenses
    , srsResponseStatus
    ) where

import qualified Network.AWS.Firehose.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartDeliveryStreamEncryption' smart constructor.
data StartDeliveryStreamEncryption = StartDeliveryStreamEncryption'
  { deliveryStreamName :: Types.DeliveryStreamName
    -- ^ The name of the delivery stream for which you want to enable server-side encryption (SSE).
  , deliveryStreamEncryptionConfigurationInput :: Core.Maybe Types.DeliveryStreamEncryptionConfigurationInput
    -- ^ Used to specify the type and Amazon Resource Name (ARN) of the KMS key needed for Server-Side Encryption (SSE).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartDeliveryStreamEncryption' value with any optional fields omitted.
mkStartDeliveryStreamEncryption
    :: Types.DeliveryStreamName -- ^ 'deliveryStreamName'
    -> StartDeliveryStreamEncryption
mkStartDeliveryStreamEncryption deliveryStreamName
  = StartDeliveryStreamEncryption'{deliveryStreamName,
                                   deliveryStreamEncryptionConfigurationInput = Core.Nothing}

-- | The name of the delivery stream for which you want to enable server-side encryption (SSE).
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDeliveryStreamName :: Lens.Lens' StartDeliveryStreamEncryption Types.DeliveryStreamName
sDeliveryStreamName = Lens.field @"deliveryStreamName"
{-# INLINEABLE sDeliveryStreamName #-}
{-# DEPRECATED deliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead"  #-}

-- | Used to specify the type and Amazon Resource Name (ARN) of the KMS key needed for Server-Side Encryption (SSE).
--
-- /Note:/ Consider using 'deliveryStreamEncryptionConfigurationInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDeliveryStreamEncryptionConfigurationInput :: Lens.Lens' StartDeliveryStreamEncryption (Core.Maybe Types.DeliveryStreamEncryptionConfigurationInput)
sDeliveryStreamEncryptionConfigurationInput = Lens.field @"deliveryStreamEncryptionConfigurationInput"
{-# INLINEABLE sDeliveryStreamEncryptionConfigurationInput #-}
{-# DEPRECATED deliveryStreamEncryptionConfigurationInput "Use generic-lens or generic-optics with 'deliveryStreamEncryptionConfigurationInput' instead"  #-}

instance Core.ToQuery StartDeliveryStreamEncryption where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartDeliveryStreamEncryption where
        toHeaders StartDeliveryStreamEncryption{..}
          = Core.pure
              ("X-Amz-Target", "Firehose_20150804.StartDeliveryStreamEncryption")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartDeliveryStreamEncryption where
        toJSON StartDeliveryStreamEncryption{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DeliveryStreamName" Core..= deliveryStreamName),
                  ("DeliveryStreamEncryptionConfigurationInput" Core..=) Core.<$>
                    deliveryStreamEncryptionConfigurationInput])

instance Core.AWSRequest StartDeliveryStreamEncryption where
        type Rs StartDeliveryStreamEncryption =
             StartDeliveryStreamEncryptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StartDeliveryStreamEncryptionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartDeliveryStreamEncryptionResponse' smart constructor.
newtype StartDeliveryStreamEncryptionResponse = StartDeliveryStreamEncryptionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartDeliveryStreamEncryptionResponse' value with any optional fields omitted.
mkStartDeliveryStreamEncryptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartDeliveryStreamEncryptionResponse
mkStartDeliveryStreamEncryptionResponse responseStatus
  = StartDeliveryStreamEncryptionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartDeliveryStreamEncryptionResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
