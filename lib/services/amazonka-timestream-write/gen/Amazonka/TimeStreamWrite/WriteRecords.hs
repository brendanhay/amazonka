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
-- Module      : Amazonka.TimeStreamWrite.WriteRecords
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The WriteRecords operation enables you to write your time series data
-- into Timestream. You can specify a single data point or a batch of data
-- points to be inserted into the system. Timestream offers you with a
-- flexible schema that auto detects the column names and data types for
-- your Timestream tables based on the dimension names and data types of
-- the data points you specify when invoking writes into the database.
-- Timestream support eventual consistency read semantics. This means that
-- when you query data immediately after writing a batch of data into
-- Timestream, the query results might not reflect the results of a
-- recently completed write operation. The results may also include some
-- stale data. If you repeat the query request after a short time, the
-- results should return the latest data.
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/ts-limits.html Service quotas apply>.
--
-- See
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/code-samples.write.html code sample>
-- for details.
--
-- __Upserts__
--
-- You can use the @Version@ parameter in a @WriteRecords@ request to
-- update data points. Timestream tracks a version number with each record.
-- @Version@ defaults to @1@ when not specified for the record in the
-- request. Timestream will update an existing record’s measure value along
-- with its @Version@ upon receiving a write request with a higher
-- @Version@ number for that record. Upon receiving an update request where
-- the measure value is the same as that of the existing record, Timestream
-- still updates @Version@, if it is greater than the existing value of
-- @Version@. You can update a data point as many times as desired, as long
-- as the value of @Version@ continuously increases.
--
-- For example, suppose you write a new record without indicating @Version@
-- in the request. Timestream will store this record, and set @Version@ to
-- @1@. Now, suppose you try to update this record with a @WriteRecords@
-- request of the same record with a different measure value but, like
-- before, do not provide @Version@. In this case, Timestream will reject
-- this update with a @RejectedRecordsException@ since the updated record’s
-- version is not greater than the existing value of Version. However, if
-- you were to resend the update request with @Version@ set to @2@,
-- Timestream would then succeed in updating the record’s value, and the
-- @Version@ would be set to @2@. Next, suppose you sent a @WriteRecords@
-- request with this same record and an identical measure value, but with
-- @Version@ set to @3@. In this case, Timestream would only update
-- @Version@ to @3@. Any further updates would need to send a version
-- number greater than @3@, or the update requests would receive a
-- @RejectedRecordsException@.
module Amazonka.TimeStreamWrite.WriteRecords
  ( -- * Creating a Request
    WriteRecords (..),
    newWriteRecords,

    -- * Request Lenses
    writeRecords_commonAttributes,
    writeRecords_databaseName,
    writeRecords_tableName,
    writeRecords_records,

    -- * Destructuring the Response
    WriteRecordsResponse (..),
    newWriteRecordsResponse,

    -- * Response Lenses
    writeRecordsResponse_recordsIngested,
    writeRecordsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamWrite.Types

-- | /See:/ 'newWriteRecords' smart constructor.
data WriteRecords = WriteRecords'
  { -- | A record containing the common measure, dimension, time, and version
    -- attributes shared across all the records in the request. The measure and
    -- dimension attributes specified will be merged with the measure and
    -- dimension attributes in the records object when the data is written into
    -- Timestream. Dimensions may not overlap, or a @ValidationException@ will
    -- be thrown. In other words, a record must contain dimensions with unique
    -- names.
    commonAttributes :: Prelude.Maybe Record,
    -- | The name of the Timestream database.
    databaseName :: Prelude.Text,
    -- | The name of the Timestream table.
    tableName :: Prelude.Text,
    -- | An array of records containing the unique measure, dimension, time, and
    -- version attributes for each time series data point.
    records :: Prelude.NonEmpty Record
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WriteRecords' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commonAttributes', 'writeRecords_commonAttributes' - A record containing the common measure, dimension, time, and version
-- attributes shared across all the records in the request. The measure and
-- dimension attributes specified will be merged with the measure and
-- dimension attributes in the records object when the data is written into
-- Timestream. Dimensions may not overlap, or a @ValidationException@ will
-- be thrown. In other words, a record must contain dimensions with unique
-- names.
--
-- 'databaseName', 'writeRecords_databaseName' - The name of the Timestream database.
--
-- 'tableName', 'writeRecords_tableName' - The name of the Timestream table.
--
-- 'records', 'writeRecords_records' - An array of records containing the unique measure, dimension, time, and
-- version attributes for each time series data point.
newWriteRecords ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'records'
  Prelude.NonEmpty Record ->
  WriteRecords
newWriteRecords pDatabaseName_ pTableName_ pRecords_ =
  WriteRecords'
    { commonAttributes = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      records = Lens.coerced Lens.# pRecords_
    }

-- | A record containing the common measure, dimension, time, and version
-- attributes shared across all the records in the request. The measure and
-- dimension attributes specified will be merged with the measure and
-- dimension attributes in the records object when the data is written into
-- Timestream. Dimensions may not overlap, or a @ValidationException@ will
-- be thrown. In other words, a record must contain dimensions with unique
-- names.
writeRecords_commonAttributes :: Lens.Lens' WriteRecords (Prelude.Maybe Record)
writeRecords_commonAttributes = Lens.lens (\WriteRecords' {commonAttributes} -> commonAttributes) (\s@WriteRecords' {} a -> s {commonAttributes = a} :: WriteRecords)

-- | The name of the Timestream database.
writeRecords_databaseName :: Lens.Lens' WriteRecords Prelude.Text
writeRecords_databaseName = Lens.lens (\WriteRecords' {databaseName} -> databaseName) (\s@WriteRecords' {} a -> s {databaseName = a} :: WriteRecords)

-- | The name of the Timestream table.
writeRecords_tableName :: Lens.Lens' WriteRecords Prelude.Text
writeRecords_tableName = Lens.lens (\WriteRecords' {tableName} -> tableName) (\s@WriteRecords' {} a -> s {tableName = a} :: WriteRecords)

-- | An array of records containing the unique measure, dimension, time, and
-- version attributes for each time series data point.
writeRecords_records :: Lens.Lens' WriteRecords (Prelude.NonEmpty Record)
writeRecords_records = Lens.lens (\WriteRecords' {records} -> records) (\s@WriteRecords' {} a -> s {records = a} :: WriteRecords) Prelude.. Lens.coerced

instance Core.AWSRequest WriteRecords where
  type AWSResponse WriteRecords = WriteRecordsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          WriteRecordsResponse'
            Prelude.<$> (x Data..?> "RecordsIngested")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable WriteRecords where
  hashWithSalt _salt WriteRecords' {..} =
    _salt `Prelude.hashWithSalt` commonAttributes
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` records

instance Prelude.NFData WriteRecords where
  rnf WriteRecords' {..} =
    Prelude.rnf commonAttributes
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf records

instance Data.ToHeaders WriteRecords where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.WriteRecords" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON WriteRecords where
  toJSON WriteRecords' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CommonAttributes" Data..=)
              Prelude.<$> commonAttributes,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just ("Records" Data..= records)
          ]
      )

instance Data.ToPath WriteRecords where
  toPath = Prelude.const "/"

instance Data.ToQuery WriteRecords where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newWriteRecordsResponse' smart constructor.
data WriteRecordsResponse = WriteRecordsResponse'
  { -- | Information on the records ingested by this request.
    recordsIngested :: Prelude.Maybe RecordsIngested,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WriteRecordsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordsIngested', 'writeRecordsResponse_recordsIngested' - Information on the records ingested by this request.
--
-- 'httpStatus', 'writeRecordsResponse_httpStatus' - The response's http status code.
newWriteRecordsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  WriteRecordsResponse
newWriteRecordsResponse pHttpStatus_ =
  WriteRecordsResponse'
    { recordsIngested =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information on the records ingested by this request.
writeRecordsResponse_recordsIngested :: Lens.Lens' WriteRecordsResponse (Prelude.Maybe RecordsIngested)
writeRecordsResponse_recordsIngested = Lens.lens (\WriteRecordsResponse' {recordsIngested} -> recordsIngested) (\s@WriteRecordsResponse' {} a -> s {recordsIngested = a} :: WriteRecordsResponse)

-- | The response's http status code.
writeRecordsResponse_httpStatus :: Lens.Lens' WriteRecordsResponse Prelude.Int
writeRecordsResponse_httpStatus = Lens.lens (\WriteRecordsResponse' {httpStatus} -> httpStatus) (\s@WriteRecordsResponse' {} a -> s {httpStatus = a} :: WriteRecordsResponse)

instance Prelude.NFData WriteRecordsResponse where
  rnf WriteRecordsResponse' {..} =
    Prelude.rnf recordsIngested
      `Prelude.seq` Prelude.rnf httpStatus
