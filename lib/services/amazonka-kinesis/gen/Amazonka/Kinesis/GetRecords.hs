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
-- Module      : Amazonka.Kinesis.GetRecords
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets data records from a Kinesis data stream\'s shard.
--
-- Specify a shard iterator using the @ShardIterator@ parameter. The shard
-- iterator specifies the position in the shard from which you want to
-- start reading data records sequentially. If there are no records
-- available in the portion of the shard that the iterator points to,
-- GetRecords returns an empty list. It might take multiple calls to get to
-- a portion of the shard that contains records.
--
-- You can scale by provisioning multiple shards per stream while
-- considering service limits (for more information, see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Amazon Kinesis Data Streams Limits>
-- in the /Amazon Kinesis Data Streams Developer Guide/). Your application
-- should have one thread per shard, each reading continuously from its
-- stream. To read from a stream continually, call GetRecords in a loop.
-- Use GetShardIterator to get the shard iterator to specify in the first
-- GetRecords call. GetRecords returns a new shard iterator in
-- @NextShardIterator@. Specify the shard iterator returned in
-- @NextShardIterator@ in subsequent calls to GetRecords. If the shard has
-- been closed, the shard iterator can\'t return more data and GetRecords
-- returns @null@ in @NextShardIterator@. You can terminate the loop when
-- the shard is closed, or when the shard iterator reaches the record with
-- the sequence number or other attribute that marks it as the last record
-- to process.
--
-- Each data record can be up to 1 MiB in size, and each shard can read up
-- to 2 MiB per second. You can ensure that your calls don\'t exceed the
-- maximum supported size or throughput by using the @Limit@ parameter to
-- specify the maximum number of records that GetRecords can return.
-- Consider your average record size when determining this limit. The
-- maximum number of records that can be returned per call is 10,000.
--
-- The size of the data returned by GetRecords varies depending on the
-- utilization of the shard. It is recommended that consumer applications
-- retrieve records via the @GetRecords@ command using the 5 TPS limit to
-- remain caught up. Retrieving records less frequently can lead to
-- consumer applications falling behind. The maximum size of data that
-- GetRecords can return is 10 MiB. If a call returns this amount of data,
-- subsequent calls made within the next 5 seconds throw
-- @ProvisionedThroughputExceededException@. If there is insufficient
-- provisioned throughput on the stream, subsequent calls made within the
-- next 1 second throw @ProvisionedThroughputExceededException@. GetRecords
-- doesn\'t return any data when it throws an exception. For this reason,
-- we recommend that you wait 1 second between calls to GetRecords.
-- However, it\'s possible that the application will get exceptions for
-- longer than 1 second.
--
-- To detect whether the application is falling behind in processing, you
-- can use the @MillisBehindLatest@ response attribute. You can also
-- monitor the stream using CloudWatch metrics and other mechanisms (see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/monitoring.html Monitoring>
-- in the /Amazon Kinesis Data Streams Developer Guide/).
--
-- Each Amazon Kinesis record includes a value,
-- @ApproximateArrivalTimestamp@, that is set when a stream successfully
-- receives and stores a record. This is commonly referred to as a
-- server-side time stamp, whereas a client-side time stamp is set when a
-- data producer creates or sends the record to a stream (a data producer
-- is any data source putting data records into a stream, for example with
-- PutRecords). The time stamp has millisecond precision. There are no
-- guarantees about the time stamp accuracy, or that the time stamp is
-- always increasing. For example, records in a shard or across a stream
-- might have time stamps that are out of order.
--
-- This operation has a limit of five transactions per second per shard.
module Amazonka.Kinesis.GetRecords
  ( -- * Creating a Request
    GetRecords (..),
    newGetRecords,

    -- * Request Lenses
    getRecords_limit,
    getRecords_shardIterator,

    -- * Destructuring the Response
    GetRecordsResponse (..),
    newGetRecordsResponse,

    -- * Response Lenses
    getRecordsResponse_millisBehindLatest,
    getRecordsResponse_nextShardIterator,
    getRecordsResponse_childShards,
    getRecordsResponse_httpStatus,
    getRecordsResponse_records,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for GetRecords.
--
-- /See:/ 'newGetRecords' smart constructor.
data GetRecords = GetRecords'
  { -- | The maximum number of records to return. Specify a value of up to
    -- 10,000. If you specify a value that is greater than 10,000, GetRecords
    -- throws @InvalidArgumentException@. The default value is 10,000.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The position in the shard from which you want to start sequentially
    -- reading data records. A shard iterator specifies this position using the
    -- sequence number of a data record in the shard.
    shardIterator :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecords' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'getRecords_limit' - The maximum number of records to return. Specify a value of up to
-- 10,000. If you specify a value that is greater than 10,000, GetRecords
-- throws @InvalidArgumentException@. The default value is 10,000.
--
-- 'shardIterator', 'getRecords_shardIterator' - The position in the shard from which you want to start sequentially
-- reading data records. A shard iterator specifies this position using the
-- sequence number of a data record in the shard.
newGetRecords ::
  -- | 'shardIterator'
  Prelude.Text ->
  GetRecords
newGetRecords pShardIterator_ =
  GetRecords'
    { limit = Prelude.Nothing,
      shardIterator = pShardIterator_
    }

-- | The maximum number of records to return. Specify a value of up to
-- 10,000. If you specify a value that is greater than 10,000, GetRecords
-- throws @InvalidArgumentException@. The default value is 10,000.
getRecords_limit :: Lens.Lens' GetRecords (Prelude.Maybe Prelude.Natural)
getRecords_limit = Lens.lens (\GetRecords' {limit} -> limit) (\s@GetRecords' {} a -> s {limit = a} :: GetRecords)

-- | The position in the shard from which you want to start sequentially
-- reading data records. A shard iterator specifies this position using the
-- sequence number of a data record in the shard.
getRecords_shardIterator :: Lens.Lens' GetRecords Prelude.Text
getRecords_shardIterator = Lens.lens (\GetRecords' {shardIterator} -> shardIterator) (\s@GetRecords' {} a -> s {shardIterator = a} :: GetRecords)

instance Core.AWSRequest GetRecords where
  type AWSResponse GetRecords = GetRecordsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRecordsResponse'
            Prelude.<$> (x Data..?> "MillisBehindLatest")
            Prelude.<*> (x Data..?> "NextShardIterator")
            Prelude.<*> (x Data..?> "ChildShards" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Records" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetRecords where
  hashWithSalt _salt GetRecords' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` shardIterator

instance Prelude.NFData GetRecords where
  rnf GetRecords' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf shardIterator

instance Data.ToHeaders GetRecords where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Kinesis_20131202.GetRecords" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRecords where
  toJSON GetRecords' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            Prelude.Just
              ("ShardIterator" Data..= shardIterator)
          ]
      )

instance Data.ToPath GetRecords where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRecords where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output for GetRecords.
--
-- /See:/ 'newGetRecordsResponse' smart constructor.
data GetRecordsResponse = GetRecordsResponse'
  { -- | The number of milliseconds the GetRecords response is from the tip of
    -- the stream, indicating how far behind current time the consumer is. A
    -- value of zero indicates that record processing is caught up, and there
    -- are no new records to process at this moment.
    millisBehindLatest :: Prelude.Maybe Prelude.Natural,
    -- | The next position in the shard from which to start sequentially reading
    -- data records. If set to @null@, the shard has been closed and the
    -- requested iterator does not return any more data.
    nextShardIterator :: Prelude.Maybe Prelude.Text,
    -- | The list of the current shard\'s child shards, returned in the
    -- @GetRecords@ API\'s response only when the end of the current shard is
    -- reached.
    childShards :: Prelude.Maybe [ChildShard],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The data records retrieved from the shard.
    records :: [Record]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecordsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'millisBehindLatest', 'getRecordsResponse_millisBehindLatest' - The number of milliseconds the GetRecords response is from the tip of
-- the stream, indicating how far behind current time the consumer is. A
-- value of zero indicates that record processing is caught up, and there
-- are no new records to process at this moment.
--
-- 'nextShardIterator', 'getRecordsResponse_nextShardIterator' - The next position in the shard from which to start sequentially reading
-- data records. If set to @null@, the shard has been closed and the
-- requested iterator does not return any more data.
--
-- 'childShards', 'getRecordsResponse_childShards' - The list of the current shard\'s child shards, returned in the
-- @GetRecords@ API\'s response only when the end of the current shard is
-- reached.
--
-- 'httpStatus', 'getRecordsResponse_httpStatus' - The response's http status code.
--
-- 'records', 'getRecordsResponse_records' - The data records retrieved from the shard.
newGetRecordsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRecordsResponse
newGetRecordsResponse pHttpStatus_ =
  GetRecordsResponse'
    { millisBehindLatest =
        Prelude.Nothing,
      nextShardIterator = Prelude.Nothing,
      childShards = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      records = Prelude.mempty
    }

-- | The number of milliseconds the GetRecords response is from the tip of
-- the stream, indicating how far behind current time the consumer is. A
-- value of zero indicates that record processing is caught up, and there
-- are no new records to process at this moment.
getRecordsResponse_millisBehindLatest :: Lens.Lens' GetRecordsResponse (Prelude.Maybe Prelude.Natural)
getRecordsResponse_millisBehindLatest = Lens.lens (\GetRecordsResponse' {millisBehindLatest} -> millisBehindLatest) (\s@GetRecordsResponse' {} a -> s {millisBehindLatest = a} :: GetRecordsResponse)

-- | The next position in the shard from which to start sequentially reading
-- data records. If set to @null@, the shard has been closed and the
-- requested iterator does not return any more data.
getRecordsResponse_nextShardIterator :: Lens.Lens' GetRecordsResponse (Prelude.Maybe Prelude.Text)
getRecordsResponse_nextShardIterator = Lens.lens (\GetRecordsResponse' {nextShardIterator} -> nextShardIterator) (\s@GetRecordsResponse' {} a -> s {nextShardIterator = a} :: GetRecordsResponse)

-- | The list of the current shard\'s child shards, returned in the
-- @GetRecords@ API\'s response only when the end of the current shard is
-- reached.
getRecordsResponse_childShards :: Lens.Lens' GetRecordsResponse (Prelude.Maybe [ChildShard])
getRecordsResponse_childShards = Lens.lens (\GetRecordsResponse' {childShards} -> childShards) (\s@GetRecordsResponse' {} a -> s {childShards = a} :: GetRecordsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRecordsResponse_httpStatus :: Lens.Lens' GetRecordsResponse Prelude.Int
getRecordsResponse_httpStatus = Lens.lens (\GetRecordsResponse' {httpStatus} -> httpStatus) (\s@GetRecordsResponse' {} a -> s {httpStatus = a} :: GetRecordsResponse)

-- | The data records retrieved from the shard.
getRecordsResponse_records :: Lens.Lens' GetRecordsResponse [Record]
getRecordsResponse_records = Lens.lens (\GetRecordsResponse' {records} -> records) (\s@GetRecordsResponse' {} a -> s {records = a} :: GetRecordsResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetRecordsResponse where
  rnf GetRecordsResponse' {..} =
    Prelude.rnf millisBehindLatest
      `Prelude.seq` Prelude.rnf nextShardIterator
      `Prelude.seq` Prelude.rnf childShards
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf records
