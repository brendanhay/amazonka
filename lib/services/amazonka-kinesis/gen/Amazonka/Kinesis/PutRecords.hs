{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Kinesis.PutRecords
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Writes multiple data records into a Kinesis data stream in a single call
-- (also referred to as a @PutRecords@ request). Use this operation to send
-- data into the stream for data ingestion and processing.
--
-- When invoking this API, it is recommended you use the @StreamARN@ input
-- parameter rather than the @StreamName@ input parameter.
--
-- Each @PutRecords@ request can support up to 500 records. Each record in
-- the request can be as large as 1 MiB, up to a limit of 5 MiB for the
-- entire request, including partition keys. Each shard can support writes
-- up to 1,000 records per second, up to a maximum data write total of 1
-- MiB per second.
--
-- You must specify the name of the stream that captures, stores, and
-- transports the data; and an array of request @Records@, with each record
-- in the array requiring a partition key and data blob. The record size
-- limit applies to the total size of the partition key and data blob.
--
-- The data blob can be any type of data; for example, a segment from a log
-- file, geographic\/location data, website clickstream data, and so on.
--
-- The partition key is used by Kinesis Data Streams as input to a hash
-- function that maps the partition key and associated data to a specific
-- shard. An MD5 hash function is used to map partition keys to 128-bit
-- integer values and to map associated data records to shards. As a result
-- of this hashing mechanism, all data records with the same partition key
-- map to the same shard within the stream. For more information, see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-add-data-to-stream Adding Data to a Stream>
-- in the /Amazon Kinesis Data Streams Developer Guide/.
--
-- Each record in the @Records@ array may include an optional parameter,
-- @ExplicitHashKey@, which overrides the partition key to shard mapping.
-- This parameter allows a data producer to determine explicitly the shard
-- where the record is stored. For more information, see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-putrecords Adding Multiple Records with PutRecords>
-- in the /Amazon Kinesis Data Streams Developer Guide/.
--
-- The @PutRecords@ response includes an array of response @Records@. Each
-- record in the response array directly correlates with a record in the
-- request array using natural ordering, from the top to the bottom of the
-- request and response. The response @Records@ array always includes the
-- same number of records as the request array.
--
-- The response @Records@ array includes both successfully and
-- unsuccessfully processed records. Kinesis Data Streams attempts to
-- process all records in each @PutRecords@ request. A single record
-- failure does not stop the processing of subsequent records. As a result,
-- PutRecords doesn\'t guarantee the ordering of records. If you need to
-- read records in the same order they are written to the stream, use
-- PutRecord instead of @PutRecords@, and write to the same shard.
--
-- A successfully processed record includes @ShardId@ and @SequenceNumber@
-- values. The @ShardId@ parameter identifies the shard in the stream where
-- the record is stored. The @SequenceNumber@ parameter is an identifier
-- assigned to the put record, unique to all records in the stream.
--
-- An unsuccessfully processed record includes @ErrorCode@ and
-- @ErrorMessage@ values. @ErrorCode@ reflects the type of error and can be
-- one of the following values: @ProvisionedThroughputExceededException@ or
-- @InternalFailure@. @ErrorMessage@ provides more detailed information
-- about the @ProvisionedThroughputExceededException@ exception including
-- the account ID, stream name, and shard ID of the record that was
-- throttled. For more information about partially successful responses,
-- see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-add-data-to-stream.html#kinesis-using-sdk-java-putrecords Adding Multiple Records with PutRecords>
-- in the /Amazon Kinesis Data Streams Developer Guide/.
--
-- After you write a record to a stream, you cannot modify that record or
-- its order within the stream.
--
-- By default, data records are accessible for 24 hours from the time that
-- they are added to a stream. You can use IncreaseStreamRetentionPeriod or
-- DecreaseStreamRetentionPeriod to modify this retention period.
module Amazonka.Kinesis.PutRecords
  ( -- * Creating a Request
    PutRecords (..),
    newPutRecords,

    -- * Request Lenses
    putRecords_streamARN,
    putRecords_streamName,
    putRecords_records,

    -- * Destructuring the Response
    PutRecordsResponse (..),
    newPutRecordsResponse,

    -- * Response Lenses
    putRecordsResponse_encryptionType,
    putRecordsResponse_failedRecordCount,
    putRecordsResponse_httpStatus,
    putRecordsResponse_records,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A @PutRecords@ request.
--
-- /See:/ 'newPutRecords' smart constructor.
data PutRecords = PutRecords'
  { -- | The ARN of the stream.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The stream name associated with the request.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | The records associated with the request.
    records :: Prelude.NonEmpty PutRecordsRequestEntry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRecords' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'putRecords_streamARN' - The ARN of the stream.
--
-- 'streamName', 'putRecords_streamName' - The stream name associated with the request.
--
-- 'records', 'putRecords_records' - The records associated with the request.
newPutRecords ::
  -- | 'records'
  Prelude.NonEmpty PutRecordsRequestEntry ->
  PutRecords
newPutRecords pRecords_ =
  PutRecords'
    { streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing,
      records = Lens.coerced Lens.# pRecords_
    }

-- | The ARN of the stream.
putRecords_streamARN :: Lens.Lens' PutRecords (Prelude.Maybe Prelude.Text)
putRecords_streamARN = Lens.lens (\PutRecords' {streamARN} -> streamARN) (\s@PutRecords' {} a -> s {streamARN = a} :: PutRecords)

-- | The stream name associated with the request.
putRecords_streamName :: Lens.Lens' PutRecords (Prelude.Maybe Prelude.Text)
putRecords_streamName = Lens.lens (\PutRecords' {streamName} -> streamName) (\s@PutRecords' {} a -> s {streamName = a} :: PutRecords)

-- | The records associated with the request.
putRecords_records :: Lens.Lens' PutRecords (Prelude.NonEmpty PutRecordsRequestEntry)
putRecords_records = Lens.lens (\PutRecords' {records} -> records) (\s@PutRecords' {} a -> s {records = a} :: PutRecords) Prelude.. Lens.coerced

instance Core.AWSRequest PutRecords where
  type AWSResponse PutRecords = PutRecordsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRecordsResponse'
            Prelude.<$> (x Data..?> "EncryptionType")
            Prelude.<*> (x Data..?> "FailedRecordCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Records")
      )

instance Prelude.Hashable PutRecords where
  hashWithSalt _salt PutRecords' {..} =
    _salt
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` records

instance Prelude.NFData PutRecords where
  rnf PutRecords' {..} =
    Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf records

instance Data.ToHeaders PutRecords where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Kinesis_20131202.PutRecords" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutRecords where
  toJSON PutRecords' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName,
            Prelude.Just ("Records" Data..= records)
          ]
      )

instance Data.ToPath PutRecords where
  toPath = Prelude.const "/"

instance Data.ToQuery PutRecords where
  toQuery = Prelude.const Prelude.mempty

-- | @PutRecords@ results.
--
-- /See:/ 'newPutRecordsResponse' smart constructor.
data PutRecordsResponse = PutRecordsResponse'
  { -- | The encryption type used on the records. This parameter can be one of
    -- the following values:
    --
    -- -   @NONE@: Do not encrypt the records.
    --
    -- -   @KMS@: Use server-side encryption on the records using a
    --     customer-managed Amazon Web Services KMS key.
    encryptionType :: Prelude.Maybe EncryptionType,
    -- | The number of unsuccessfully processed records in a @PutRecords@
    -- request.
    failedRecordCount :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of successfully and unsuccessfully processed record results. A
    -- record that is successfully added to a stream includes @SequenceNumber@
    -- and @ShardId@ in the result. A record that fails to be added to a stream
    -- includes @ErrorCode@ and @ErrorMessage@ in the result.
    records :: Prelude.NonEmpty PutRecordsResultEntry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRecordsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionType', 'putRecordsResponse_encryptionType' - The encryption type used on the records. This parameter can be one of
-- the following values:
--
-- -   @NONE@: Do not encrypt the records.
--
-- -   @KMS@: Use server-side encryption on the records using a
--     customer-managed Amazon Web Services KMS key.
--
-- 'failedRecordCount', 'putRecordsResponse_failedRecordCount' - The number of unsuccessfully processed records in a @PutRecords@
-- request.
--
-- 'httpStatus', 'putRecordsResponse_httpStatus' - The response's http status code.
--
-- 'records', 'putRecordsResponse_records' - An array of successfully and unsuccessfully processed record results. A
-- record that is successfully added to a stream includes @SequenceNumber@
-- and @ShardId@ in the result. A record that fails to be added to a stream
-- includes @ErrorCode@ and @ErrorMessage@ in the result.
newPutRecordsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'records'
  Prelude.NonEmpty PutRecordsResultEntry ->
  PutRecordsResponse
newPutRecordsResponse pHttpStatus_ pRecords_ =
  PutRecordsResponse'
    { encryptionType =
        Prelude.Nothing,
      failedRecordCount = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      records = Lens.coerced Lens.# pRecords_
    }

-- | The encryption type used on the records. This parameter can be one of
-- the following values:
--
-- -   @NONE@: Do not encrypt the records.
--
-- -   @KMS@: Use server-side encryption on the records using a
--     customer-managed Amazon Web Services KMS key.
putRecordsResponse_encryptionType :: Lens.Lens' PutRecordsResponse (Prelude.Maybe EncryptionType)
putRecordsResponse_encryptionType = Lens.lens (\PutRecordsResponse' {encryptionType} -> encryptionType) (\s@PutRecordsResponse' {} a -> s {encryptionType = a} :: PutRecordsResponse)

-- | The number of unsuccessfully processed records in a @PutRecords@
-- request.
putRecordsResponse_failedRecordCount :: Lens.Lens' PutRecordsResponse (Prelude.Maybe Prelude.Natural)
putRecordsResponse_failedRecordCount = Lens.lens (\PutRecordsResponse' {failedRecordCount} -> failedRecordCount) (\s@PutRecordsResponse' {} a -> s {failedRecordCount = a} :: PutRecordsResponse)

-- | The response's http status code.
putRecordsResponse_httpStatus :: Lens.Lens' PutRecordsResponse Prelude.Int
putRecordsResponse_httpStatus = Lens.lens (\PutRecordsResponse' {httpStatus} -> httpStatus) (\s@PutRecordsResponse' {} a -> s {httpStatus = a} :: PutRecordsResponse)

-- | An array of successfully and unsuccessfully processed record results. A
-- record that is successfully added to a stream includes @SequenceNumber@
-- and @ShardId@ in the result. A record that fails to be added to a stream
-- includes @ErrorCode@ and @ErrorMessage@ in the result.
putRecordsResponse_records :: Lens.Lens' PutRecordsResponse (Prelude.NonEmpty PutRecordsResultEntry)
putRecordsResponse_records = Lens.lens (\PutRecordsResponse' {records} -> records) (\s@PutRecordsResponse' {} a -> s {records = a} :: PutRecordsResponse) Prelude.. Lens.coerced

instance Prelude.NFData PutRecordsResponse where
  rnf PutRecordsResponse' {..} =
    Prelude.rnf encryptionType
      `Prelude.seq` Prelude.rnf failedRecordCount
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf records
