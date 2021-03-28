{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.PutRecords
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Writes multiple data records into a Kinesis data stream in a single call (also referred to as a @PutRecords@ request). Use this operation to send data into the stream for data ingestion and processing. 
--
-- Each @PutRecords@ request can support up to 500 records. Each record in the request can be as large as 1 MiB, up to a limit of 5 MiB for the entire request, including partition keys. Each shard can support writes up to 1,000 records per second, up to a maximum data write total of 1 MiB per second.
-- You must specify the name of the stream that captures, stores, and transports the data; and an array of request @Records@ , with each record in the array requiring a partition key and data blob. The record size limit applies to the total size of the partition key and data blob.
-- The data blob can be any type of data; for example, a segment from a log file, geographic/location data, website clickstream data, and so on.
-- The partition key is used by Kinesis Data Streams as input to a hash function that maps the partition key and associated data to a specific shard. An MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards. As a result of this hashing mechanism, all data records with the same partition key map to the same shard within the stream. For more information, see <https://docs.aws.amazon.com/kinesis/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-add-data-to-stream Adding Data to a Stream> in the /Amazon Kinesis Data Streams Developer Guide/ .
-- Each record in the @Records@ array may include an optional parameter, @ExplicitHashKey@ , which overrides the partition key to shard mapping. This parameter allows a data producer to determine explicitly the shard where the record is stored. For more information, see <https://docs.aws.amazon.com/kinesis/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-putrecords Adding Multiple Records with PutRecords> in the /Amazon Kinesis Data Streams Developer Guide/ .
-- The @PutRecords@ response includes an array of response @Records@ . Each record in the response array directly correlates with a record in the request array using natural ordering, from the top to the bottom of the request and response. The response @Records@ array always includes the same number of records as the request array.
-- The response @Records@ array includes both successfully and unsuccessfully processed records. Kinesis Data Streams attempts to process all records in each @PutRecords@ request. A single record failure does not stop the processing of subsequent records. As a result, PutRecords doesn't guarantee the ordering of records. If you need to read records in the same order they are written to the stream, use 'PutRecord' instead of @PutRecords@ , and write to the same shard.
-- A successfully processed record includes @ShardId@ and @SequenceNumber@ values. The @ShardId@ parameter identifies the shard in the stream where the record is stored. The @SequenceNumber@ parameter is an identifier assigned to the put record, unique to all records in the stream.
-- An unsuccessfully processed record includes @ErrorCode@ and @ErrorMessage@ values. @ErrorCode@ reflects the type of error and can be one of the following values: @ProvisionedThroughputExceededException@ or @InternalFailure@ . @ErrorMessage@ provides more detailed information about the @ProvisionedThroughputExceededException@ exception including the account ID, stream name, and shard ID of the record that was throttled. For more information about partially successful responses, see <https://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-add-data-to-stream.html#kinesis-using-sdk-java-putrecords Adding Multiple Records with PutRecords> in the /Amazon Kinesis Data Streams Developer Guide/ .
-- /Important:/ After you write a record to a stream, you cannot modify that record or its order within the stream.
-- By default, data records are accessible for 24 hours from the time that they are added to a stream. You can use 'IncreaseStreamRetentionPeriod' or 'DecreaseStreamRetentionPeriod' to modify this retention period.
module Network.AWS.Kinesis.PutRecords
    (
    -- * Creating a request
      PutRecords (..)
    , mkPutRecords
    -- ** Request lenses
    , pRecordEntries
    , pStreamName

    -- * Destructuring the response
    , PutRecordsResponse (..)
    , mkPutRecordsResponse
    -- ** Response lenses
    , prsRecords
    , prsEncryptionType
    , prsFailedRecordCount
    , prsResponseStatus
    ) where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A @PutRecords@ request.
--
-- /See:/ 'mkPutRecords' smart constructor.
data PutRecords = PutRecords'
  { recordEntries :: Core.NonEmpty Types.PutRecordsRequestEntry
    -- ^ The records associated with the request.
  , streamName :: Types.StreamName
    -- ^ The stream name associated with the request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRecords' value with any optional fields omitted.
mkPutRecords
    :: Core.NonEmpty Types.PutRecordsRequestEntry -- ^ 'recordEntries'
    -> Types.StreamName -- ^ 'streamName'
    -> PutRecords
mkPutRecords recordEntries streamName
  = PutRecords'{recordEntries, streamName}

-- | The records associated with the request.
--
-- /Note:/ Consider using 'recordEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRecordEntries :: Lens.Lens' PutRecords (Core.NonEmpty Types.PutRecordsRequestEntry)
pRecordEntries = Lens.field @"recordEntries"
{-# INLINEABLE pRecordEntries #-}
{-# DEPRECATED recordEntries "Use generic-lens or generic-optics with 'recordEntries' instead"  #-}

-- | The stream name associated with the request.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pStreamName :: Lens.Lens' PutRecords Types.StreamName
pStreamName = Lens.field @"streamName"
{-# INLINEABLE pStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

instance Core.ToQuery PutRecords where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutRecords where
        toHeaders PutRecords{..}
          = Core.pure ("X-Amz-Target", "Kinesis_20131202.PutRecords") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutRecords where
        toJSON PutRecords{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Records" Core..= recordEntries),
                  Core.Just ("StreamName" Core..= streamName)])

instance Core.AWSRequest PutRecords where
        type Rs PutRecords = PutRecordsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutRecordsResponse' Core.<$>
                   (x Core..: "Records") Core.<*> x Core..:? "EncryptionType" Core.<*>
                     x Core..:? "FailedRecordCount"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | @PutRecords@ results.
--
-- /See:/ 'mkPutRecordsResponse' smart constructor.
data PutRecordsResponse = PutRecordsResponse'
  { records :: Core.NonEmpty Types.PutRecordsResultEntry
    -- ^ An array of successfully and unsuccessfully processed record results, correlated with the request by natural ordering. A record that is successfully added to a stream includes @SequenceNumber@ and @ShardId@ in the result. A record that fails to be added to a stream includes @ErrorCode@ and @ErrorMessage@ in the result.
  , encryptionType :: Core.Maybe Types.EncryptionType
    -- ^ The encryption type used on the records. This parameter can be one of the following values:
--
--
--     * @NONE@ : Do not encrypt the records.
--
--
--     * @KMS@ : Use server-side encryption on the records using a customer-managed AWS KMS key.
--
--
  , failedRecordCount :: Core.Maybe Core.Natural
    -- ^ The number of unsuccessfully processed records in a @PutRecords@ request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRecordsResponse' value with any optional fields omitted.
mkPutRecordsResponse
    :: Core.NonEmpty Types.PutRecordsResultEntry -- ^ 'records'
    -> Core.Int -- ^ 'responseStatus'
    -> PutRecordsResponse
mkPutRecordsResponse records responseStatus
  = PutRecordsResponse'{records, encryptionType = Core.Nothing,
                        failedRecordCount = Core.Nothing, responseStatus}

-- | An array of successfully and unsuccessfully processed record results, correlated with the request by natural ordering. A record that is successfully added to a stream includes @SequenceNumber@ and @ShardId@ in the result. A record that fails to be added to a stream includes @ErrorCode@ and @ErrorMessage@ in the result.
--
-- /Note:/ Consider using 'records' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsRecords :: Lens.Lens' PutRecordsResponse (Core.NonEmpty Types.PutRecordsResultEntry)
prsRecords = Lens.field @"records"
{-# INLINEABLE prsRecords #-}
{-# DEPRECATED records "Use generic-lens or generic-optics with 'records' instead"  #-}

-- | The encryption type used on the records. This parameter can be one of the following values:
--
--
--     * @NONE@ : Do not encrypt the records.
--
--
--     * @KMS@ : Use server-side encryption on the records using a customer-managed AWS KMS key.
--
--
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsEncryptionType :: Lens.Lens' PutRecordsResponse (Core.Maybe Types.EncryptionType)
prsEncryptionType = Lens.field @"encryptionType"
{-# INLINEABLE prsEncryptionType #-}
{-# DEPRECATED encryptionType "Use generic-lens or generic-optics with 'encryptionType' instead"  #-}

-- | The number of unsuccessfully processed records in a @PutRecords@ request.
--
-- /Note:/ Consider using 'failedRecordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsFailedRecordCount :: Lens.Lens' PutRecordsResponse (Core.Maybe Core.Natural)
prsFailedRecordCount = Lens.field @"failedRecordCount"
{-# INLINEABLE prsFailedRecordCount #-}
{-# DEPRECATED failedRecordCount "Use generic-lens or generic-optics with 'failedRecordCount' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsResponseStatus :: Lens.Lens' PutRecordsResponse Core.Int
prsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE prsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
