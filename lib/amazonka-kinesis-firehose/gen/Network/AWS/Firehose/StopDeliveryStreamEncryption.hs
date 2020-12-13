{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    StopDeliveryStreamEncryption (..),
    mkStopDeliveryStreamEncryption,

    -- ** Request lenses
    sdseDeliveryStreamName,

    -- * Destructuring the response
    StopDeliveryStreamEncryptionResponse (..),
    mkStopDeliveryStreamEncryptionResponse,

    -- ** Response lenses
    srsResponseStatus,
  )
where

import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopDeliveryStreamEncryption' smart constructor.
newtype StopDeliveryStreamEncryption = StopDeliveryStreamEncryption'
  { -- | The name of the delivery stream for which you want to disable server-side encryption (SSE).
    deliveryStreamName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopDeliveryStreamEncryption' with the minimum fields required to make a request.
--
-- * 'deliveryStreamName' - The name of the delivery stream for which you want to disable server-side encryption (SSE).
mkStopDeliveryStreamEncryption ::
  -- | 'deliveryStreamName'
  Lude.Text ->
  StopDeliveryStreamEncryption
mkStopDeliveryStreamEncryption pDeliveryStreamName_ =
  StopDeliveryStreamEncryption'
    { deliveryStreamName =
        pDeliveryStreamName_
    }

-- | The name of the delivery stream for which you want to disable server-side encryption (SSE).
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdseDeliveryStreamName :: Lens.Lens' StopDeliveryStreamEncryption Lude.Text
sdseDeliveryStreamName = Lens.lens (deliveryStreamName :: StopDeliveryStreamEncryption -> Lude.Text) (\s a -> s {deliveryStreamName = a} :: StopDeliveryStreamEncryption)
{-# DEPRECATED sdseDeliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead." #-}

instance Lude.AWSRequest StopDeliveryStreamEncryption where
  type
    Rs StopDeliveryStreamEncryption =
      StopDeliveryStreamEncryptionResponse
  request = Req.postJSON firehoseService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopDeliveryStreamEncryptionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopDeliveryStreamEncryption where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Firehose_20150804.StopDeliveryStreamEncryption" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopDeliveryStreamEncryption where
  toJSON StopDeliveryStreamEncryption' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("DeliveryStreamName" Lude..= deliveryStreamName)]
      )

instance Lude.ToPath StopDeliveryStreamEncryption where
  toPath = Lude.const "/"

instance Lude.ToQuery StopDeliveryStreamEncryption where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopDeliveryStreamEncryptionResponse' smart constructor.
newtype StopDeliveryStreamEncryptionResponse = StopDeliveryStreamEncryptionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopDeliveryStreamEncryptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopDeliveryStreamEncryptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopDeliveryStreamEncryptionResponse
mkStopDeliveryStreamEncryptionResponse pResponseStatus_ =
  StopDeliveryStreamEncryptionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopDeliveryStreamEncryptionResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StopDeliveryStreamEncryptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopDeliveryStreamEncryptionResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
