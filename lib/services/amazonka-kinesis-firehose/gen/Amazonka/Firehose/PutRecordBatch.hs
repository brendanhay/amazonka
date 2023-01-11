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
-- Module      : Amazonka.Firehose.PutRecordBatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Writes multiple data records into a delivery stream in a single call,
-- which can achieve higher throughput per producer than when writing
-- single records. To write single data records into a delivery stream, use
-- PutRecord. Applications using these operations are referred to as
-- producers.
--
-- For information about service quota, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/limits.html Amazon Kinesis Data Firehose Quota>.
--
-- Each PutRecordBatch request supports up to 500 records. Each record in
-- the request can be as large as 1,000 KB (before base64 encoding), up to
-- a limit of 4 MB for the entire request. These limits cannot be changed.
--
-- You must specify the name of the delivery stream and the data record
-- when using PutRecord. The data record consists of a data blob that can
-- be up to 1,000 KB in size, and any kind of data. For example, it could
-- be a segment from a log file, geographic location data, website
-- clickstream data, and so on.
--
-- Kinesis Data Firehose buffers records before delivering them to the
-- destination. To disambiguate the data blobs at the destination, a common
-- solution is to use delimiters in the data, such as a newline (@\\n@) or
-- some other character unique within the data. This allows the consumer
-- application to parse individual data items when reading the data from
-- the destination.
--
-- The PutRecordBatch response includes a count of failed records,
-- @FailedPutCount@, and an array of responses, @RequestResponses@. Even if
-- the PutRecordBatch call succeeds, the value of @FailedPutCount@ may be
-- greater than 0, indicating that there are records for which the
-- operation didn\'t succeed. Each entry in the @RequestResponses@ array
-- provides additional information about the processed record. It directly
-- correlates with a record in the request array using the same ordering,
-- from the top to the bottom. The response array always includes the same
-- number of records as the request array. @RequestResponses@ includes both
-- successfully and unsuccessfully processed records. Kinesis Data Firehose
-- tries to process all records in each PutRecordBatch request. A single
-- record failure does not stop the processing of subsequent records.
--
-- A successfully processed record includes a @RecordId@ value, which is
-- unique for the record. An unsuccessfully processed record includes
-- @ErrorCode@ and @ErrorMessage@ values. @ErrorCode@ reflects the type of
-- error, and is one of the following values: @ServiceUnavailableException@
-- or @InternalFailure@. @ErrorMessage@ provides more detailed information
-- about the error.
--
-- If there is an internal server error or a timeout, the write might have
-- completed or it might have failed. If @FailedPutCount@ is greater than
-- 0, retry the request, resending only those records that might have
-- failed processing. This minimizes the possible duplicate records and
-- also reduces the total bytes sent (and corresponding charges). We
-- recommend that you handle any duplicates at the destination.
--
-- If PutRecordBatch throws @ServiceUnavailableException@, back off and
-- retry. If the exception persists, it is possible that the throughput
-- limits have been exceeded for the delivery stream.
--
-- Data records sent to Kinesis Data Firehose are stored for 24 hours from
-- the time they are added to a delivery stream as it attempts to send the
-- records to the destination. If the destination is unreachable for more
-- than 24 hours, the data is no longer available.
--
-- Don\'t concatenate two or more base64 strings to form the data fields of
-- your records. Instead, concatenate the raw data, then perform base64
-- encoding.
module Amazonka.Firehose.PutRecordBatch
  ( -- * Creating a Request
    PutRecordBatch (..),
    newPutRecordBatch,

    -- * Request Lenses
    putRecordBatch_deliveryStreamName,
    putRecordBatch_records,

    -- * Destructuring the Response
    PutRecordBatchResponse (..),
    newPutRecordBatchResponse,

    -- * Response Lenses
    putRecordBatchResponse_encrypted,
    putRecordBatchResponse_httpStatus,
    putRecordBatchResponse_failedPutCount,
    putRecordBatchResponse_requestResponses,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutRecordBatch' smart constructor.
data PutRecordBatch = PutRecordBatch'
  { -- | The name of the delivery stream.
    deliveryStreamName :: Prelude.Text,
    -- | One or more records.
    records :: Prelude.NonEmpty Record
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRecordBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryStreamName', 'putRecordBatch_deliveryStreamName' - The name of the delivery stream.
--
-- 'records', 'putRecordBatch_records' - One or more records.
newPutRecordBatch ::
  -- | 'deliveryStreamName'
  Prelude.Text ->
  -- | 'records'
  Prelude.NonEmpty Record ->
  PutRecordBatch
newPutRecordBatch pDeliveryStreamName_ pRecords_ =
  PutRecordBatch'
    { deliveryStreamName =
        pDeliveryStreamName_,
      records = Lens.coerced Lens.# pRecords_
    }

-- | The name of the delivery stream.
putRecordBatch_deliveryStreamName :: Lens.Lens' PutRecordBatch Prelude.Text
putRecordBatch_deliveryStreamName = Lens.lens (\PutRecordBatch' {deliveryStreamName} -> deliveryStreamName) (\s@PutRecordBatch' {} a -> s {deliveryStreamName = a} :: PutRecordBatch)

-- | One or more records.
putRecordBatch_records :: Lens.Lens' PutRecordBatch (Prelude.NonEmpty Record)
putRecordBatch_records = Lens.lens (\PutRecordBatch' {records} -> records) (\s@PutRecordBatch' {} a -> s {records = a} :: PutRecordBatch) Prelude.. Lens.coerced

instance Core.AWSRequest PutRecordBatch where
  type
    AWSResponse PutRecordBatch =
      PutRecordBatchResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRecordBatchResponse'
            Prelude.<$> (x Data..?> "Encrypted")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "FailedPutCount")
            Prelude.<*> (x Data..:> "RequestResponses")
      )

instance Prelude.Hashable PutRecordBatch where
  hashWithSalt _salt PutRecordBatch' {..} =
    _salt `Prelude.hashWithSalt` deliveryStreamName
      `Prelude.hashWithSalt` records

instance Prelude.NFData PutRecordBatch where
  rnf PutRecordBatch' {..} =
    Prelude.rnf deliveryStreamName
      `Prelude.seq` Prelude.rnf records

instance Data.ToHeaders PutRecordBatch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Firehose_20150804.PutRecordBatch" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutRecordBatch where
  toJSON PutRecordBatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DeliveryStreamName" Data..= deliveryStreamName),
            Prelude.Just ("Records" Data..= records)
          ]
      )

instance Data.ToPath PutRecordBatch where
  toPath = Prelude.const "/"

instance Data.ToQuery PutRecordBatch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRecordBatchResponse' smart constructor.
data PutRecordBatchResponse = PutRecordBatchResponse'
  { -- | Indicates whether server-side encryption (SSE) was enabled during this
    -- operation.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The number of records that might have failed processing. This number
    -- might be greater than 0 even if the PutRecordBatch call succeeds. Check
    -- @FailedPutCount@ to determine whether there are records that you need to
    -- resend.
    failedPutCount :: Prelude.Natural,
    -- | The results array. For each record, the index of the response element is
    -- the same as the index used in the request array.
    requestResponses :: Prelude.NonEmpty PutRecordBatchResponseEntry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRecordBatchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encrypted', 'putRecordBatchResponse_encrypted' - Indicates whether server-side encryption (SSE) was enabled during this
-- operation.
--
-- 'httpStatus', 'putRecordBatchResponse_httpStatus' - The response's http status code.
--
-- 'failedPutCount', 'putRecordBatchResponse_failedPutCount' - The number of records that might have failed processing. This number
-- might be greater than 0 even if the PutRecordBatch call succeeds. Check
-- @FailedPutCount@ to determine whether there are records that you need to
-- resend.
--
-- 'requestResponses', 'putRecordBatchResponse_requestResponses' - The results array. For each record, the index of the response element is
-- the same as the index used in the request array.
newPutRecordBatchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'failedPutCount'
  Prelude.Natural ->
  -- | 'requestResponses'
  Prelude.NonEmpty PutRecordBatchResponseEntry ->
  PutRecordBatchResponse
newPutRecordBatchResponse
  pHttpStatus_
  pFailedPutCount_
  pRequestResponses_ =
    PutRecordBatchResponse'
      { encrypted =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        failedPutCount = pFailedPutCount_,
        requestResponses =
          Lens.coerced Lens.# pRequestResponses_
      }

-- | Indicates whether server-side encryption (SSE) was enabled during this
-- operation.
putRecordBatchResponse_encrypted :: Lens.Lens' PutRecordBatchResponse (Prelude.Maybe Prelude.Bool)
putRecordBatchResponse_encrypted = Lens.lens (\PutRecordBatchResponse' {encrypted} -> encrypted) (\s@PutRecordBatchResponse' {} a -> s {encrypted = a} :: PutRecordBatchResponse)

-- | The response's http status code.
putRecordBatchResponse_httpStatus :: Lens.Lens' PutRecordBatchResponse Prelude.Int
putRecordBatchResponse_httpStatus = Lens.lens (\PutRecordBatchResponse' {httpStatus} -> httpStatus) (\s@PutRecordBatchResponse' {} a -> s {httpStatus = a} :: PutRecordBatchResponse)

-- | The number of records that might have failed processing. This number
-- might be greater than 0 even if the PutRecordBatch call succeeds. Check
-- @FailedPutCount@ to determine whether there are records that you need to
-- resend.
putRecordBatchResponse_failedPutCount :: Lens.Lens' PutRecordBatchResponse Prelude.Natural
putRecordBatchResponse_failedPutCount = Lens.lens (\PutRecordBatchResponse' {failedPutCount} -> failedPutCount) (\s@PutRecordBatchResponse' {} a -> s {failedPutCount = a} :: PutRecordBatchResponse)

-- | The results array. For each record, the index of the response element is
-- the same as the index used in the request array.
putRecordBatchResponse_requestResponses :: Lens.Lens' PutRecordBatchResponse (Prelude.NonEmpty PutRecordBatchResponseEntry)
putRecordBatchResponse_requestResponses = Lens.lens (\PutRecordBatchResponse' {requestResponses} -> requestResponses) (\s@PutRecordBatchResponse' {} a -> s {requestResponses = a} :: PutRecordBatchResponse) Prelude.. Lens.coerced

instance Prelude.NFData PutRecordBatchResponse where
  rnf PutRecordBatchResponse' {..} =
    Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf failedPutCount
      `Prelude.seq` Prelude.rnf requestResponses
