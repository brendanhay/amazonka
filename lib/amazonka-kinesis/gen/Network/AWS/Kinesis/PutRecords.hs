{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PutRecords (..),
    mkPutRecords,

    -- ** Request lenses
    pRecordEntries,
    pStreamName,

    -- * Destructuring the response
    PutRecordsResponse (..),
    mkPutRecordsResponse,

    -- ** Response lenses
    prsEncryptionType,
    prsFailedRecordCount,
    prsResponseStatus,
    prsRecords,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A @PutRecords@ request.
--
-- /See:/ 'mkPutRecords' smart constructor.
data PutRecords = PutRecords'
  { recordEntries ::
      Lude.NonEmpty PutRecordsRequestEntry,
    streamName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRecords' with the minimum fields required to make a request.
--
-- * 'recordEntries' - The records associated with the request.
-- * 'streamName' - The stream name associated with the request.
mkPutRecords ::
  -- | 'recordEntries'
  Lude.NonEmpty PutRecordsRequestEntry ->
  -- | 'streamName'
  Lude.Text ->
  PutRecords
mkPutRecords pRecordEntries_ pStreamName_ =
  PutRecords'
    { recordEntries = pRecordEntries_,
      streamName = pStreamName_
    }

-- | The records associated with the request.
--
-- /Note:/ Consider using 'recordEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRecordEntries :: Lens.Lens' PutRecords (Lude.NonEmpty PutRecordsRequestEntry)
pRecordEntries = Lens.lens (recordEntries :: PutRecords -> Lude.NonEmpty PutRecordsRequestEntry) (\s a -> s {recordEntries = a} :: PutRecords)
{-# DEPRECATED pRecordEntries "Use generic-lens or generic-optics with 'recordEntries' instead." #-}

-- | The stream name associated with the request.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pStreamName :: Lens.Lens' PutRecords Lude.Text
pStreamName = Lens.lens (streamName :: PutRecords -> Lude.Text) (\s a -> s {streamName = a} :: PutRecords)
{-# DEPRECATED pStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest PutRecords where
  type Rs PutRecords = PutRecordsResponse
  request = Req.postJSON kinesisService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutRecordsResponse'
            Lude.<$> (x Lude..?> "EncryptionType")
            Lude.<*> (x Lude..?> "FailedRecordCount")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "Records")
      )

instance Lude.ToHeaders PutRecords where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.PutRecords" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutRecords where
  toJSON PutRecords' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Records" Lude..= recordEntries),
            Lude.Just ("StreamName" Lude..= streamName)
          ]
      )

instance Lude.ToPath PutRecords where
  toPath = Lude.const "/"

instance Lude.ToQuery PutRecords where
  toQuery = Lude.const Lude.mempty

-- | @PutRecords@ results.
--
-- /See:/ 'mkPutRecordsResponse' smart constructor.
data PutRecordsResponse = PutRecordsResponse'
  { encryptionType ::
      Lude.Maybe EncryptionType,
    failedRecordCount :: Lude.Maybe Lude.Natural,
    responseStatus :: Lude.Int,
    records :: Lude.NonEmpty PutRecordsResultEntry
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRecordsResponse' with the minimum fields required to make a request.
--
-- * 'encryptionType' - The encryption type used on the records. This parameter can be one of the following values:
--
--
--     * @NONE@ : Do not encrypt the records.
--
--
--     * @KMS@ : Use server-side encryption on the records using a customer-managed AWS KMS key.
--
--
-- * 'failedRecordCount' - The number of unsuccessfully processed records in a @PutRecords@ request.
-- * 'records' - An array of successfully and unsuccessfully processed record results, correlated with the request by natural ordering. A record that is successfully added to a stream includes @SequenceNumber@ and @ShardId@ in the result. A record that fails to be added to a stream includes @ErrorCode@ and @ErrorMessage@ in the result.
-- * 'responseStatus' - The response status code.
mkPutRecordsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'records'
  Lude.NonEmpty PutRecordsResultEntry ->
  PutRecordsResponse
mkPutRecordsResponse pResponseStatus_ pRecords_ =
  PutRecordsResponse'
    { encryptionType = Lude.Nothing,
      failedRecordCount = Lude.Nothing,
      responseStatus = pResponseStatus_,
      records = pRecords_
    }

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
prsEncryptionType :: Lens.Lens' PutRecordsResponse (Lude.Maybe EncryptionType)
prsEncryptionType = Lens.lens (encryptionType :: PutRecordsResponse -> Lude.Maybe EncryptionType) (\s a -> s {encryptionType = a} :: PutRecordsResponse)
{-# DEPRECATED prsEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

-- | The number of unsuccessfully processed records in a @PutRecords@ request.
--
-- /Note:/ Consider using 'failedRecordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsFailedRecordCount :: Lens.Lens' PutRecordsResponse (Lude.Maybe Lude.Natural)
prsFailedRecordCount = Lens.lens (failedRecordCount :: PutRecordsResponse -> Lude.Maybe Lude.Natural) (\s a -> s {failedRecordCount = a} :: PutRecordsResponse)
{-# DEPRECATED prsFailedRecordCount "Use generic-lens or generic-optics with 'failedRecordCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsResponseStatus :: Lens.Lens' PutRecordsResponse Lude.Int
prsResponseStatus = Lens.lens (responseStatus :: PutRecordsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutRecordsResponse)
{-# DEPRECATED prsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | An array of successfully and unsuccessfully processed record results, correlated with the request by natural ordering. A record that is successfully added to a stream includes @SequenceNumber@ and @ShardId@ in the result. A record that fails to be added to a stream includes @ErrorCode@ and @ErrorMessage@ in the result.
--
-- /Note:/ Consider using 'records' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsRecords :: Lens.Lens' PutRecordsResponse (Lude.NonEmpty PutRecordsResultEntry)
prsRecords = Lens.lens (records :: PutRecordsResponse -> Lude.NonEmpty PutRecordsResultEntry) (\s a -> s {records = a} :: PutRecordsResponse)
{-# DEPRECATED prsRecords "Use generic-lens or generic-optics with 'records' instead." #-}
