{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    StartDeliveryStreamEncryption (..),
    mkStartDeliveryStreamEncryption,

    -- ** Request lenses
    sDeliveryStreamEncryptionConfigurationInput,
    sDeliveryStreamName,

    -- * Destructuring the response
    StartDeliveryStreamEncryptionResponse (..),
    mkStartDeliveryStreamEncryptionResponse,

    -- ** Response lenses
    srsResponseStatus,
  )
where

import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartDeliveryStreamEncryption' smart constructor.
data StartDeliveryStreamEncryption = StartDeliveryStreamEncryption'
  { deliveryStreamEncryptionConfigurationInput ::
      Lude.Maybe
        DeliveryStreamEncryptionConfigurationInput,
    deliveryStreamName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartDeliveryStreamEncryption' with the minimum fields required to make a request.
--
-- * 'deliveryStreamEncryptionConfigurationInput' - Used to specify the type and Amazon Resource Name (ARN) of the KMS key needed for Server-Side Encryption (SSE).
-- * 'deliveryStreamName' - The name of the delivery stream for which you want to enable server-side encryption (SSE).
mkStartDeliveryStreamEncryption ::
  -- | 'deliveryStreamName'
  Lude.Text ->
  StartDeliveryStreamEncryption
mkStartDeliveryStreamEncryption pDeliveryStreamName_ =
  StartDeliveryStreamEncryption'
    { deliveryStreamEncryptionConfigurationInput =
        Lude.Nothing,
      deliveryStreamName = pDeliveryStreamName_
    }

-- | Used to specify the type and Amazon Resource Name (ARN) of the KMS key needed for Server-Side Encryption (SSE).
--
-- /Note:/ Consider using 'deliveryStreamEncryptionConfigurationInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDeliveryStreamEncryptionConfigurationInput :: Lens.Lens' StartDeliveryStreamEncryption (Lude.Maybe DeliveryStreamEncryptionConfigurationInput)
sDeliveryStreamEncryptionConfigurationInput = Lens.lens (deliveryStreamEncryptionConfigurationInput :: StartDeliveryStreamEncryption -> Lude.Maybe DeliveryStreamEncryptionConfigurationInput) (\s a -> s {deliveryStreamEncryptionConfigurationInput = a} :: StartDeliveryStreamEncryption)
{-# DEPRECATED sDeliveryStreamEncryptionConfigurationInput "Use generic-lens or generic-optics with 'deliveryStreamEncryptionConfigurationInput' instead." #-}

-- | The name of the delivery stream for which you want to enable server-side encryption (SSE).
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDeliveryStreamName :: Lens.Lens' StartDeliveryStreamEncryption Lude.Text
sDeliveryStreamName = Lens.lens (deliveryStreamName :: StartDeliveryStreamEncryption -> Lude.Text) (\s a -> s {deliveryStreamName = a} :: StartDeliveryStreamEncryption)
{-# DEPRECATED sDeliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead." #-}

instance Lude.AWSRequest StartDeliveryStreamEncryption where
  type
    Rs StartDeliveryStreamEncryption =
      StartDeliveryStreamEncryptionResponse
  request = Req.postJSON firehoseService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StartDeliveryStreamEncryptionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartDeliveryStreamEncryption where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Firehose_20150804.StartDeliveryStreamEncryption" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartDeliveryStreamEncryption where
  toJSON StartDeliveryStreamEncryption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeliveryStreamEncryptionConfigurationInput" Lude..=)
              Lude.<$> deliveryStreamEncryptionConfigurationInput,
            Lude.Just ("DeliveryStreamName" Lude..= deliveryStreamName)
          ]
      )

instance Lude.ToPath StartDeliveryStreamEncryption where
  toPath = Lude.const "/"

instance Lude.ToQuery StartDeliveryStreamEncryption where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartDeliveryStreamEncryptionResponse' smart constructor.
newtype StartDeliveryStreamEncryptionResponse = StartDeliveryStreamEncryptionResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartDeliveryStreamEncryptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStartDeliveryStreamEncryptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartDeliveryStreamEncryptionResponse
mkStartDeliveryStreamEncryptionResponse pResponseStatus_ =
  StartDeliveryStreamEncryptionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartDeliveryStreamEncryptionResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StartDeliveryStreamEncryptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartDeliveryStreamEncryptionResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
