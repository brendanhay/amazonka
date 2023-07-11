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
-- Module      : Amazonka.Firehose.PutRecord
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Writes a single data record into an Amazon Kinesis Data Firehose
-- delivery stream. To write multiple data records into a delivery stream,
-- use PutRecordBatch. Applications using these operations are referred to
-- as producers.
--
-- By default, each delivery stream can take in up to 2,000 transactions
-- per second, 5,000 records per second, or 5 MB per second. If you use
-- PutRecord and PutRecordBatch, the limits are an aggregate across these
-- two operations for each delivery stream. For more information about
-- limits and how to request an increase, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/limits.html Amazon Kinesis Data Firehose Limits>.
--
-- You must specify the name of the delivery stream and the data record
-- when using PutRecord. The data record consists of a data blob that can
-- be up to 1,000 KiB in size, and any kind of data. For example, it can be
-- a segment from a log file, geographic location data, website clickstream
-- data, and so on.
--
-- Kinesis Data Firehose buffers records before delivering them to the
-- destination. To disambiguate the data blobs at the destination, a common
-- solution is to use delimiters in the data, such as a newline (@\\n@) or
-- some other character unique within the data. This allows the consumer
-- application to parse individual data items when reading the data from
-- the destination.
--
-- The @PutRecord@ operation returns a @RecordId@, which is a unique string
-- assigned to each record. Producer applications can use this ID for
-- purposes such as auditability and investigation.
--
-- If the @PutRecord@ operation throws a @ServiceUnavailableException@,
-- back off and retry. If the exception persists, it is possible that the
-- throughput limits have been exceeded for the delivery stream.
--
-- Data records sent to Kinesis Data Firehose are stored for 24 hours from
-- the time they are added to a delivery stream as it tries to send the
-- records to the destination. If the destination is unreachable for more
-- than 24 hours, the data is no longer available.
--
-- Don\'t concatenate two or more base64 strings to form the data fields of
-- your records. Instead, concatenate the raw data, then perform base64
-- encoding.
module Amazonka.Firehose.PutRecord
  ( -- * Creating a Request
    PutRecord (..),
    newPutRecord,

    -- * Request Lenses
    putRecord_deliveryStreamName,
    putRecord_record,

    -- * Destructuring the Response
    PutRecordResponse (..),
    newPutRecordResponse,

    -- * Response Lenses
    putRecordResponse_encrypted,
    putRecordResponse_httpStatus,
    putRecordResponse_recordId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutRecord' smart constructor.
data PutRecord = PutRecord'
  { -- | The name of the delivery stream.
    deliveryStreamName :: Prelude.Text,
    -- | The record.
    record :: Record
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryStreamName', 'putRecord_deliveryStreamName' - The name of the delivery stream.
--
-- 'record', 'putRecord_record' - The record.
newPutRecord ::
  -- | 'deliveryStreamName'
  Prelude.Text ->
  -- | 'record'
  Record ->
  PutRecord
newPutRecord pDeliveryStreamName_ pRecord_ =
  PutRecord'
    { deliveryStreamName =
        pDeliveryStreamName_,
      record = pRecord_
    }

-- | The name of the delivery stream.
putRecord_deliveryStreamName :: Lens.Lens' PutRecord Prelude.Text
putRecord_deliveryStreamName = Lens.lens (\PutRecord' {deliveryStreamName} -> deliveryStreamName) (\s@PutRecord' {} a -> s {deliveryStreamName = a} :: PutRecord)

-- | The record.
putRecord_record :: Lens.Lens' PutRecord Record
putRecord_record = Lens.lens (\PutRecord' {record} -> record) (\s@PutRecord' {} a -> s {record = a} :: PutRecord)

instance Core.AWSRequest PutRecord where
  type AWSResponse PutRecord = PutRecordResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRecordResponse'
            Prelude.<$> (x Data..?> "Encrypted")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "RecordId")
      )

instance Prelude.Hashable PutRecord where
  hashWithSalt _salt PutRecord' {..} =
    _salt
      `Prelude.hashWithSalt` deliveryStreamName
      `Prelude.hashWithSalt` record

instance Prelude.NFData PutRecord where
  rnf PutRecord' {..} =
    Prelude.rnf deliveryStreamName
      `Prelude.seq` Prelude.rnf record

instance Data.ToHeaders PutRecord where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Firehose_20150804.PutRecord" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutRecord where
  toJSON PutRecord' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DeliveryStreamName" Data..= deliveryStreamName),
            Prelude.Just ("Record" Data..= record)
          ]
      )

instance Data.ToPath PutRecord where
  toPath = Prelude.const "/"

instance Data.ToQuery PutRecord where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRecordResponse' smart constructor.
data PutRecordResponse = PutRecordResponse'
  { -- | Indicates whether server-side encryption (SSE) was enabled during this
    -- operation.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the record.
    recordId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRecordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encrypted', 'putRecordResponse_encrypted' - Indicates whether server-side encryption (SSE) was enabled during this
-- operation.
--
-- 'httpStatus', 'putRecordResponse_httpStatus' - The response's http status code.
--
-- 'recordId', 'putRecordResponse_recordId' - The ID of the record.
newPutRecordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'recordId'
  Prelude.Text ->
  PutRecordResponse
newPutRecordResponse pHttpStatus_ pRecordId_ =
  PutRecordResponse'
    { encrypted = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      recordId = pRecordId_
    }

-- | Indicates whether server-side encryption (SSE) was enabled during this
-- operation.
putRecordResponse_encrypted :: Lens.Lens' PutRecordResponse (Prelude.Maybe Prelude.Bool)
putRecordResponse_encrypted = Lens.lens (\PutRecordResponse' {encrypted} -> encrypted) (\s@PutRecordResponse' {} a -> s {encrypted = a} :: PutRecordResponse)

-- | The response's http status code.
putRecordResponse_httpStatus :: Lens.Lens' PutRecordResponse Prelude.Int
putRecordResponse_httpStatus = Lens.lens (\PutRecordResponse' {httpStatus} -> httpStatus) (\s@PutRecordResponse' {} a -> s {httpStatus = a} :: PutRecordResponse)

-- | The ID of the record.
putRecordResponse_recordId :: Lens.Lens' PutRecordResponse Prelude.Text
putRecordResponse_recordId = Lens.lens (\PutRecordResponse' {recordId} -> recordId) (\s@PutRecordResponse' {} a -> s {recordId = a} :: PutRecordResponse)

instance Prelude.NFData PutRecordResponse where
  rnf PutRecordResponse' {..} =
    Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf recordId
