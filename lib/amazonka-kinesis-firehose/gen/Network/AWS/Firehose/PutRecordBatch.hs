{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      PutRecordBatch (..)
    , mkPutRecordBatch
    -- ** Request lenses
    , prbDeliveryStreamName
    , prbRecords

    -- * Destructuring the response
    , PutRecordBatchResponse (..)
    , mkPutRecordBatchResponse
    -- ** Response lenses
    , prbrrsFailedPutCount
    , prbrrsRequestResponses
    , prbrrsEncrypted
    , prbrrsResponseStatus
    ) where

import qualified Network.AWS.Firehose.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutRecordBatch' smart constructor.
data PutRecordBatch = PutRecordBatch'
  { deliveryStreamName :: Types.DeliveryStreamName
    -- ^ The name of the delivery stream.
  , records :: Core.NonEmpty Types.Record
    -- ^ One or more records.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRecordBatch' value with any optional fields omitted.
mkPutRecordBatch
    :: Types.DeliveryStreamName -- ^ 'deliveryStreamName'
    -> Core.NonEmpty Types.Record -- ^ 'records'
    -> PutRecordBatch
mkPutRecordBatch deliveryStreamName records
  = PutRecordBatch'{deliveryStreamName, records}

-- | The name of the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbDeliveryStreamName :: Lens.Lens' PutRecordBatch Types.DeliveryStreamName
prbDeliveryStreamName = Lens.field @"deliveryStreamName"
{-# INLINEABLE prbDeliveryStreamName #-}
{-# DEPRECATED deliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead"  #-}

-- | One or more records.
--
-- /Note:/ Consider using 'records' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbRecords :: Lens.Lens' PutRecordBatch (Core.NonEmpty Types.Record)
prbRecords = Lens.field @"records"
{-# INLINEABLE prbRecords #-}
{-# DEPRECATED records "Use generic-lens or generic-optics with 'records' instead"  #-}

instance Core.ToQuery PutRecordBatch where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutRecordBatch where
        toHeaders PutRecordBatch{..}
          = Core.pure ("X-Amz-Target", "Firehose_20150804.PutRecordBatch")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutRecordBatch where
        toJSON PutRecordBatch{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DeliveryStreamName" Core..= deliveryStreamName),
                  Core.Just ("Records" Core..= records)])

instance Core.AWSRequest PutRecordBatch where
        type Rs PutRecordBatch = PutRecordBatchResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutRecordBatchResponse' Core.<$>
                   (x Core..: "FailedPutCount") Core.<*> x Core..: "RequestResponses"
                     Core.<*> x Core..:? "Encrypted"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutRecordBatchResponse' smart constructor.
data PutRecordBatchResponse = PutRecordBatchResponse'
  { failedPutCount :: Core.Natural
    -- ^ The number of records that might have failed processing. This number might be greater than 0 even if the 'PutRecordBatch' call succeeds. Check @FailedPutCount@ to determine whether there are records that you need to resend.
  , requestResponses :: Core.NonEmpty Types.PutRecordBatchResponseEntry
    -- ^ The results array. For each record, the index of the response element is the same as the index used in the request array.
  , encrypted :: Core.Maybe Core.Bool
    -- ^ Indicates whether server-side encryption (SSE) was enabled during this operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRecordBatchResponse' value with any optional fields omitted.
mkPutRecordBatchResponse
    :: Core.Natural -- ^ 'failedPutCount'
    -> Core.NonEmpty Types.PutRecordBatchResponseEntry -- ^ 'requestResponses'
    -> Core.Int -- ^ 'responseStatus'
    -> PutRecordBatchResponse
mkPutRecordBatchResponse failedPutCount requestResponses
  responseStatus
  = PutRecordBatchResponse'{failedPutCount, requestResponses,
                            encrypted = Core.Nothing, responseStatus}

-- | The number of records that might have failed processing. This number might be greater than 0 even if the 'PutRecordBatch' call succeeds. Check @FailedPutCount@ to determine whether there are records that you need to resend.
--
-- /Note:/ Consider using 'failedPutCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbrrsFailedPutCount :: Lens.Lens' PutRecordBatchResponse Core.Natural
prbrrsFailedPutCount = Lens.field @"failedPutCount"
{-# INLINEABLE prbrrsFailedPutCount #-}
{-# DEPRECATED failedPutCount "Use generic-lens or generic-optics with 'failedPutCount' instead"  #-}

-- | The results array. For each record, the index of the response element is the same as the index used in the request array.
--
-- /Note:/ Consider using 'requestResponses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbrrsRequestResponses :: Lens.Lens' PutRecordBatchResponse (Core.NonEmpty Types.PutRecordBatchResponseEntry)
prbrrsRequestResponses = Lens.field @"requestResponses"
{-# INLINEABLE prbrrsRequestResponses #-}
{-# DEPRECATED requestResponses "Use generic-lens or generic-optics with 'requestResponses' instead"  #-}

-- | Indicates whether server-side encryption (SSE) was enabled during this operation.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbrrsEncrypted :: Lens.Lens' PutRecordBatchResponse (Core.Maybe Core.Bool)
prbrrsEncrypted = Lens.field @"encrypted"
{-# INLINEABLE prbrrsEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbrrsResponseStatus :: Lens.Lens' PutRecordBatchResponse Core.Int
prbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE prbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
