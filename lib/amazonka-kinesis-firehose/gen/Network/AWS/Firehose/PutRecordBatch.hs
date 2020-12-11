{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.PutRecordBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Writes multiple data records into a delivery stream in a single call, which can achieve higher throughput per producer than when writing single records. To write single data records into a delivery stream, use 'PutRecord' . Applications using these operations are referred to as producers.
--
-- For information about service quota, see <https://docs.aws.amazon.com/firehose/latest/dev/limits.html Amazon Kinesis Data Firehose Quota> .
-- Each 'PutRecordBatch' request supports up to 500 records. Each record in the request can be as large as 1,000 KB (before 64-bit encoding), up to a limit of 4 MB for the entire request. These limits cannot be changed.
-- You must specify the name of the delivery stream and the data record when using 'PutRecord' . The data record consists of a data blob that can be up to 1,000 KB in size, and any kind of data. For example, it could be a segment from a log file, geographic location data, website clickstream data, and so on.
-- Kinesis Data Firehose buffers records before delivering them to the destination. To disambiguate the data blobs at the destination, a common solution is to use delimiters in the data, such as a newline (@\n@ ) or some other character unique within the data. This allows the consumer application to parse individual data items when reading the data from the destination.
-- The 'PutRecordBatch' response includes a count of failed records, @FailedPutCount@ , and an array of responses, @RequestResponses@ . Even if the 'PutRecordBatch' call succeeds, the value of @FailedPutCount@ may be greater than 0, indicating that there are records for which the operation didn't succeed. Each entry in the @RequestResponses@ array provides additional information about the processed record. It directly correlates with a record in the request array using the same ordering, from the top to the bottom. The response array always includes the same number of records as the request array. @RequestResponses@ includes both successfully and unsuccessfully processed records. Kinesis Data Firehose tries to process all records in each 'PutRecordBatch' request. A single record failure does not stop the processing of subsequent records.
-- A successfully processed record includes a @RecordId@ value, which is unique for the record. An unsuccessfully processed record includes @ErrorCode@ and @ErrorMessage@ values. @ErrorCode@ reflects the type of error, and is one of the following values: @ServiceUnavailableException@ or @InternalFailure@ . @ErrorMessage@ provides more detailed information about the error.
-- If there is an internal server error or a timeout, the write might have completed or it might have failed. If @FailedPutCount@ is greater than 0, retry the request, resending only those records that might have failed processing. This minimizes the possible duplicate records and also reduces the total bytes sent (and corresponding charges). We recommend that you handle any duplicates at the destination.
-- If 'PutRecordBatch' throws @ServiceUnavailableException@ , back off and retry. If the exception persists, it is possible that the throughput limits have been exceeded for the delivery stream.
-- Data records sent to Kinesis Data Firehose are stored for 24 hours from the time they are added to a delivery stream as it attempts to send the records to the destination. If the destination is unreachable for more than 24 hours, the data is no longer available.
-- /Important:/ Don't concatenate two or more base64 strings to form the data fields of your records. Instead, concatenate the raw data, then perform base64 encoding.
module Network.AWS.Firehose.PutRecordBatch
  ( -- * Creating a request
    PutRecordBatch (..),
    mkPutRecordBatch,

    -- ** Request lenses
    prbDeliveryStreamName,
    prbRecords,

    -- * Destructuring the response
    PutRecordBatchResponse (..),
    mkPutRecordBatchResponse,

    -- ** Response lenses
    prbrsEncrypted,
    prbrsResponseStatus,
    prbrsFailedPutCount,
    prbrsRequestResponses,
  )
where

import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutRecordBatch' smart constructor.
data PutRecordBatch = PutRecordBatch'
  { deliveryStreamName ::
      Lude.Text,
    records :: Lude.NonEmpty Record
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRecordBatch' with the minimum fields required to make a request.
--
-- * 'deliveryStreamName' - The name of the delivery stream.
-- * 'records' - One or more records.
mkPutRecordBatch ::
  -- | 'deliveryStreamName'
  Lude.Text ->
  -- | 'records'
  Lude.NonEmpty Record ->
  PutRecordBatch
mkPutRecordBatch pDeliveryStreamName_ pRecords_ =
  PutRecordBatch'
    { deliveryStreamName = pDeliveryStreamName_,
      records = pRecords_
    }

-- | The name of the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbDeliveryStreamName :: Lens.Lens' PutRecordBatch Lude.Text
prbDeliveryStreamName = Lens.lens (deliveryStreamName :: PutRecordBatch -> Lude.Text) (\s a -> s {deliveryStreamName = a} :: PutRecordBatch)
{-# DEPRECATED prbDeliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead." #-}

-- | One or more records.
--
-- /Note:/ Consider using 'records' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbRecords :: Lens.Lens' PutRecordBatch (Lude.NonEmpty Record)
prbRecords = Lens.lens (records :: PutRecordBatch -> Lude.NonEmpty Record) (\s a -> s {records = a} :: PutRecordBatch)
{-# DEPRECATED prbRecords "Use generic-lens or generic-optics with 'records' instead." #-}

instance Lude.AWSRequest PutRecordBatch where
  type Rs PutRecordBatch = PutRecordBatchResponse
  request = Req.postJSON firehoseService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutRecordBatchResponse'
            Lude.<$> (x Lude..?> "Encrypted")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "FailedPutCount")
            Lude.<*> (x Lude..:> "RequestResponses")
      )

instance Lude.ToHeaders PutRecordBatch where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Firehose_20150804.PutRecordBatch" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutRecordBatch where
  toJSON PutRecordBatch' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DeliveryStreamName" Lude..= deliveryStreamName),
            Lude.Just ("Records" Lude..= records)
          ]
      )

instance Lude.ToPath PutRecordBatch where
  toPath = Lude.const "/"

instance Lude.ToQuery PutRecordBatch where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutRecordBatchResponse' smart constructor.
data PutRecordBatchResponse = PutRecordBatchResponse'
  { encrypted ::
      Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int,
    failedPutCount :: Lude.Natural,
    requestResponses ::
      Lude.NonEmpty PutRecordBatchResponseEntry
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRecordBatchResponse' with the minimum fields required to make a request.
--
-- * 'encrypted' - Indicates whether server-side encryption (SSE) was enabled during this operation.
-- * 'failedPutCount' - The number of records that might have failed processing. This number might be greater than 0 even if the 'PutRecordBatch' call succeeds. Check @FailedPutCount@ to determine whether there are records that you need to resend.
-- * 'requestResponses' - The results array. For each record, the index of the response element is the same as the index used in the request array.
-- * 'responseStatus' - The response status code.
mkPutRecordBatchResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'failedPutCount'
  Lude.Natural ->
  -- | 'requestResponses'
  Lude.NonEmpty PutRecordBatchResponseEntry ->
  PutRecordBatchResponse
mkPutRecordBatchResponse
  pResponseStatus_
  pFailedPutCount_
  pRequestResponses_ =
    PutRecordBatchResponse'
      { encrypted = Lude.Nothing,
        responseStatus = pResponseStatus_,
        failedPutCount = pFailedPutCount_,
        requestResponses = pRequestResponses_
      }

-- | Indicates whether server-side encryption (SSE) was enabled during this operation.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbrsEncrypted :: Lens.Lens' PutRecordBatchResponse (Lude.Maybe Lude.Bool)
prbrsEncrypted = Lens.lens (encrypted :: PutRecordBatchResponse -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: PutRecordBatchResponse)
{-# DEPRECATED prbrsEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbrsResponseStatus :: Lens.Lens' PutRecordBatchResponse Lude.Int
prbrsResponseStatus = Lens.lens (responseStatus :: PutRecordBatchResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutRecordBatchResponse)
{-# DEPRECATED prbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The number of records that might have failed processing. This number might be greater than 0 even if the 'PutRecordBatch' call succeeds. Check @FailedPutCount@ to determine whether there are records that you need to resend.
--
-- /Note:/ Consider using 'failedPutCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbrsFailedPutCount :: Lens.Lens' PutRecordBatchResponse Lude.Natural
prbrsFailedPutCount = Lens.lens (failedPutCount :: PutRecordBatchResponse -> Lude.Natural) (\s a -> s {failedPutCount = a} :: PutRecordBatchResponse)
{-# DEPRECATED prbrsFailedPutCount "Use generic-lens or generic-optics with 'failedPutCount' instead." #-}

-- | The results array. For each record, the index of the response element is the same as the index used in the request array.
--
-- /Note:/ Consider using 'requestResponses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbrsRequestResponses :: Lens.Lens' PutRecordBatchResponse (Lude.NonEmpty PutRecordBatchResponseEntry)
prbrsRequestResponses = Lens.lens (requestResponses :: PutRecordBatchResponse -> Lude.NonEmpty PutRecordBatchResponseEntry) (\s a -> s {requestResponses = a} :: PutRecordBatchResponse)
{-# DEPRECATED prbrsRequestResponses "Use generic-lens or generic-optics with 'requestResponses' instead." #-}
