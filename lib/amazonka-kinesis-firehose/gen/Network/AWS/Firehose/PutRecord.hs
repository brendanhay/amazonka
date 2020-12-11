{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.PutRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Writes a single data record into an Amazon Kinesis Data Firehose delivery stream. To write multiple data records into a delivery stream, use 'PutRecordBatch' . Applications using these operations are referred to as producers.
--
-- By default, each delivery stream can take in up to 2,000 transactions per second, 5,000 records per second, or 5 MB per second. If you use 'PutRecord' and 'PutRecordBatch' , the limits are an aggregate across these two operations for each delivery stream. For more information about limits and how to request an increase, see <https://docs.aws.amazon.com/firehose/latest/dev/limits.html Amazon Kinesis Data Firehose Limits> .
-- You must specify the name of the delivery stream and the data record when using 'PutRecord' . The data record consists of a data blob that can be up to 1,000 KB in size, and any kind of data. For example, it can be a segment from a log file, geographic location data, website clickstream data, and so on.
-- Kinesis Data Firehose buffers records before delivering them to the destination. To disambiguate the data blobs at the destination, a common solution is to use delimiters in the data, such as a newline (@\n@ ) or some other character unique within the data. This allows the consumer application to parse individual data items when reading the data from the destination.
-- The @PutRecord@ operation returns a @RecordId@ , which is a unique string assigned to each record. Producer applications can use this ID for purposes such as auditability and investigation.
-- If the @PutRecord@ operation throws a @ServiceUnavailableException@ , back off and retry. If the exception persists, it is possible that the throughput limits have been exceeded for the delivery stream.
-- Data records sent to Kinesis Data Firehose are stored for 24 hours from the time they are added to a delivery stream as it tries to send the records to the destination. If the destination is unreachable for more than 24 hours, the data is no longer available.
-- /Important:/ Don't concatenate two or more base64 strings to form the data fields of your records. Instead, concatenate the raw data, then perform base64 encoding.
module Network.AWS.Firehose.PutRecord
  ( -- * Creating a request
    PutRecord (..),
    mkPutRecord,

    -- ** Request lenses
    prDeliveryStreamName,
    prRecord,

    -- * Destructuring the response
    PutRecordResponse (..),
    mkPutRecordResponse,

    -- ** Response lenses
    prrsEncrypted,
    prrsResponseStatus,
    prrsRecordId,
  )
where

import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutRecord' smart constructor.
data PutRecord = PutRecord'
  { deliveryStreamName :: Lude.Text,
    record :: Record
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRecord' with the minimum fields required to make a request.
--
-- * 'deliveryStreamName' - The name of the delivery stream.
-- * 'record' - The record.
mkPutRecord ::
  -- | 'deliveryStreamName'
  Lude.Text ->
  -- | 'record'
  Record ->
  PutRecord
mkPutRecord pDeliveryStreamName_ pRecord_ =
  PutRecord'
    { deliveryStreamName = pDeliveryStreamName_,
      record = pRecord_
    }

-- | The name of the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prDeliveryStreamName :: Lens.Lens' PutRecord Lude.Text
prDeliveryStreamName = Lens.lens (deliveryStreamName :: PutRecord -> Lude.Text) (\s a -> s {deliveryStreamName = a} :: PutRecord)
{-# DEPRECATED prDeliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead." #-}

-- | The record.
--
-- /Note:/ Consider using 'record' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prRecord :: Lens.Lens' PutRecord Record
prRecord = Lens.lens (record :: PutRecord -> Record) (\s a -> s {record = a} :: PutRecord)
{-# DEPRECATED prRecord "Use generic-lens or generic-optics with 'record' instead." #-}

instance Lude.AWSRequest PutRecord where
  type Rs PutRecord = PutRecordResponse
  request = Req.postJSON firehoseService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutRecordResponse'
            Lude.<$> (x Lude..?> "Encrypted")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "RecordId")
      )

instance Lude.ToHeaders PutRecord where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Firehose_20150804.PutRecord" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutRecord where
  toJSON PutRecord' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DeliveryStreamName" Lude..= deliveryStreamName),
            Lude.Just ("Record" Lude..= record)
          ]
      )

instance Lude.ToPath PutRecord where
  toPath = Lude.const "/"

instance Lude.ToQuery PutRecord where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutRecordResponse' smart constructor.
data PutRecordResponse = PutRecordResponse'
  { encrypted ::
      Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int,
    recordId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRecordResponse' with the minimum fields required to make a request.
--
-- * 'encrypted' - Indicates whether server-side encryption (SSE) was enabled during this operation.
-- * 'recordId' - The ID of the record.
-- * 'responseStatus' - The response status code.
mkPutRecordResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'recordId'
  Lude.Text ->
  PutRecordResponse
mkPutRecordResponse pResponseStatus_ pRecordId_ =
  PutRecordResponse'
    { encrypted = Lude.Nothing,
      responseStatus = pResponseStatus_,
      recordId = pRecordId_
    }

-- | Indicates whether server-side encryption (SSE) was enabled during this operation.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrsEncrypted :: Lens.Lens' PutRecordResponse (Lude.Maybe Lude.Bool)
prrsEncrypted = Lens.lens (encrypted :: PutRecordResponse -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: PutRecordResponse)
{-# DEPRECATED prrsEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrsResponseStatus :: Lens.Lens' PutRecordResponse Lude.Int
prrsResponseStatus = Lens.lens (responseStatus :: PutRecordResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutRecordResponse)
{-# DEPRECATED prrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The ID of the record.
--
-- /Note:/ Consider using 'recordId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrsRecordId :: Lens.Lens' PutRecordResponse Lude.Text
prrsRecordId = Lens.lens (recordId :: PutRecordResponse -> Lude.Text) (\s a -> s {recordId = a} :: PutRecordResponse)
{-# DEPRECATED prrsRecordId "Use generic-lens or generic-optics with 'recordId' instead." #-}
