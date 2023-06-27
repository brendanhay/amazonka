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
-- Module      : Amazonka.Kinesis.GetShardIterator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an Amazon Kinesis shard iterator. A shard iterator expires 5
-- minutes after it is returned to the requester.
--
-- When invoking this API, it is recommended you use the @StreamARN@ input
-- parameter rather than the @StreamName@ input parameter.
--
-- A shard iterator specifies the shard position from which to start
-- reading data records sequentially. The position is specified using the
-- sequence number of a data record in a shard. A sequence number is the
-- identifier associated with every record ingested in the stream, and is
-- assigned when a record is put into the stream. Each stream has one or
-- more shards.
--
-- You must specify the shard iterator type. For example, you can set the
-- @ShardIteratorType@ parameter to read exactly from the position denoted
-- by a specific sequence number by using the @AT_SEQUENCE_NUMBER@ shard
-- iterator type. Alternatively, the parameter can read right after the
-- sequence number by using the @AFTER_SEQUENCE_NUMBER@ shard iterator
-- type, using sequence numbers returned by earlier calls to PutRecord,
-- PutRecords, GetRecords, or DescribeStream. In the request, you can
-- specify the shard iterator type @AT_TIMESTAMP@ to read records from an
-- arbitrary point in time, @TRIM_HORIZON@ to cause @ShardIterator@ to
-- point to the last untrimmed record in the shard in the system (the
-- oldest data record in the shard), or @LATEST@ so that you always read
-- the most recent data in the shard.
--
-- When you read repeatedly from a stream, use a GetShardIterator request
-- to get the first shard iterator for use in your first GetRecords request
-- and for subsequent reads use the shard iterator returned by the
-- GetRecords request in @NextShardIterator@. A new shard iterator is
-- returned by every GetRecords request in @NextShardIterator@, which you
-- use in the @ShardIterator@ parameter of the next GetRecords request.
--
-- If a GetShardIterator request is made too often, you receive a
-- @ProvisionedThroughputExceededException@. For more information about
-- throughput limits, see GetRecords, and
-- <https://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Streams Limits>
-- in the /Amazon Kinesis Data Streams Developer Guide/.
--
-- If the shard is closed, GetShardIterator returns a valid iterator for
-- the last sequence number of the shard. A shard can be closed as a result
-- of using SplitShard or MergeShards.
--
-- GetShardIterator has a limit of five transactions per second per account
-- per open shard.
module Amazonka.Kinesis.GetShardIterator
  ( -- * Creating a Request
    GetShardIterator (..),
    newGetShardIterator,

    -- * Request Lenses
    getShardIterator_startingSequenceNumber,
    getShardIterator_streamARN,
    getShardIterator_streamName,
    getShardIterator_timestamp,
    getShardIterator_shardId,
    getShardIterator_shardIteratorType,

    -- * Destructuring the Response
    GetShardIteratorResponse (..),
    newGetShardIteratorResponse,

    -- * Response Lenses
    getShardIteratorResponse_shardIterator,
    getShardIteratorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for @GetShardIterator@.
--
-- /See:/ 'newGetShardIterator' smart constructor.
data GetShardIterator = GetShardIterator'
  { -- | The sequence number of the data record in the shard from which to start
    -- reading. Used with shard iterator type AT_SEQUENCE_NUMBER and
    -- AFTER_SEQUENCE_NUMBER.
    startingSequenceNumber :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the stream.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Kinesis data stream.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | The time stamp of the data record from which to start reading. Used with
    -- shard iterator type AT_TIMESTAMP. A time stamp is the Unix epoch date
    -- with precision in milliseconds. For example,
    -- @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@. If a record with
    -- this exact time stamp does not exist, the iterator returned is for the
    -- next (later) record. If the time stamp is older than the current trim
    -- horizon, the iterator returned is for the oldest untrimmed data record
    -- (TRIM_HORIZON).
    timestamp :: Prelude.Maybe Data.POSIX,
    -- | The shard ID of the Kinesis Data Streams shard to get the iterator for.
    shardId :: Prelude.Text,
    -- | Determines how the shard iterator is used to start reading data records
    -- from the shard.
    --
    -- The following are the valid Amazon Kinesis shard iterator types:
    --
    -- -   AT_SEQUENCE_NUMBER - Start reading from the position denoted by a
    --     specific sequence number, provided in the value
    --     @StartingSequenceNumber@.
    --
    -- -   AFTER_SEQUENCE_NUMBER - Start reading right after the position
    --     denoted by a specific sequence number, provided in the value
    --     @StartingSequenceNumber@.
    --
    -- -   AT_TIMESTAMP - Start reading from the position denoted by a specific
    --     time stamp, provided in the value @Timestamp@.
    --
    -- -   TRIM_HORIZON - Start reading at the last untrimmed record in the
    --     shard in the system, which is the oldest data record in the shard.
    --
    -- -   LATEST - Start reading just after the most recent record in the
    --     shard, so that you always read the most recent data in the shard.
    shardIteratorType :: ShardIteratorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetShardIterator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startingSequenceNumber', 'getShardIterator_startingSequenceNumber' - The sequence number of the data record in the shard from which to start
-- reading. Used with shard iterator type AT_SEQUENCE_NUMBER and
-- AFTER_SEQUENCE_NUMBER.
--
-- 'streamARN', 'getShardIterator_streamARN' - The ARN of the stream.
--
-- 'streamName', 'getShardIterator_streamName' - The name of the Amazon Kinesis data stream.
--
-- 'timestamp', 'getShardIterator_timestamp' - The time stamp of the data record from which to start reading. Used with
-- shard iterator type AT_TIMESTAMP. A time stamp is the Unix epoch date
-- with precision in milliseconds. For example,
-- @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@. If a record with
-- this exact time stamp does not exist, the iterator returned is for the
-- next (later) record. If the time stamp is older than the current trim
-- horizon, the iterator returned is for the oldest untrimmed data record
-- (TRIM_HORIZON).
--
-- 'shardId', 'getShardIterator_shardId' - The shard ID of the Kinesis Data Streams shard to get the iterator for.
--
-- 'shardIteratorType', 'getShardIterator_shardIteratorType' - Determines how the shard iterator is used to start reading data records
-- from the shard.
--
-- The following are the valid Amazon Kinesis shard iterator types:
--
-- -   AT_SEQUENCE_NUMBER - Start reading from the position denoted by a
--     specific sequence number, provided in the value
--     @StartingSequenceNumber@.
--
-- -   AFTER_SEQUENCE_NUMBER - Start reading right after the position
--     denoted by a specific sequence number, provided in the value
--     @StartingSequenceNumber@.
--
-- -   AT_TIMESTAMP - Start reading from the position denoted by a specific
--     time stamp, provided in the value @Timestamp@.
--
-- -   TRIM_HORIZON - Start reading at the last untrimmed record in the
--     shard in the system, which is the oldest data record in the shard.
--
-- -   LATEST - Start reading just after the most recent record in the
--     shard, so that you always read the most recent data in the shard.
newGetShardIterator ::
  -- | 'shardId'
  Prelude.Text ->
  -- | 'shardIteratorType'
  ShardIteratorType ->
  GetShardIterator
newGetShardIterator pShardId_ pShardIteratorType_ =
  GetShardIterator'
    { startingSequenceNumber =
        Prelude.Nothing,
      streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      shardId = pShardId_,
      shardIteratorType = pShardIteratorType_
    }

-- | The sequence number of the data record in the shard from which to start
-- reading. Used with shard iterator type AT_SEQUENCE_NUMBER and
-- AFTER_SEQUENCE_NUMBER.
getShardIterator_startingSequenceNumber :: Lens.Lens' GetShardIterator (Prelude.Maybe Prelude.Text)
getShardIterator_startingSequenceNumber = Lens.lens (\GetShardIterator' {startingSequenceNumber} -> startingSequenceNumber) (\s@GetShardIterator' {} a -> s {startingSequenceNumber = a} :: GetShardIterator)

-- | The ARN of the stream.
getShardIterator_streamARN :: Lens.Lens' GetShardIterator (Prelude.Maybe Prelude.Text)
getShardIterator_streamARN = Lens.lens (\GetShardIterator' {streamARN} -> streamARN) (\s@GetShardIterator' {} a -> s {streamARN = a} :: GetShardIterator)

-- | The name of the Amazon Kinesis data stream.
getShardIterator_streamName :: Lens.Lens' GetShardIterator (Prelude.Maybe Prelude.Text)
getShardIterator_streamName = Lens.lens (\GetShardIterator' {streamName} -> streamName) (\s@GetShardIterator' {} a -> s {streamName = a} :: GetShardIterator)

-- | The time stamp of the data record from which to start reading. Used with
-- shard iterator type AT_TIMESTAMP. A time stamp is the Unix epoch date
-- with precision in milliseconds. For example,
-- @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@. If a record with
-- this exact time stamp does not exist, the iterator returned is for the
-- next (later) record. If the time stamp is older than the current trim
-- horizon, the iterator returned is for the oldest untrimmed data record
-- (TRIM_HORIZON).
getShardIterator_timestamp :: Lens.Lens' GetShardIterator (Prelude.Maybe Prelude.UTCTime)
getShardIterator_timestamp = Lens.lens (\GetShardIterator' {timestamp} -> timestamp) (\s@GetShardIterator' {} a -> s {timestamp = a} :: GetShardIterator) Prelude.. Lens.mapping Data._Time

-- | The shard ID of the Kinesis Data Streams shard to get the iterator for.
getShardIterator_shardId :: Lens.Lens' GetShardIterator Prelude.Text
getShardIterator_shardId = Lens.lens (\GetShardIterator' {shardId} -> shardId) (\s@GetShardIterator' {} a -> s {shardId = a} :: GetShardIterator)

-- | Determines how the shard iterator is used to start reading data records
-- from the shard.
--
-- The following are the valid Amazon Kinesis shard iterator types:
--
-- -   AT_SEQUENCE_NUMBER - Start reading from the position denoted by a
--     specific sequence number, provided in the value
--     @StartingSequenceNumber@.
--
-- -   AFTER_SEQUENCE_NUMBER - Start reading right after the position
--     denoted by a specific sequence number, provided in the value
--     @StartingSequenceNumber@.
--
-- -   AT_TIMESTAMP - Start reading from the position denoted by a specific
--     time stamp, provided in the value @Timestamp@.
--
-- -   TRIM_HORIZON - Start reading at the last untrimmed record in the
--     shard in the system, which is the oldest data record in the shard.
--
-- -   LATEST - Start reading just after the most recent record in the
--     shard, so that you always read the most recent data in the shard.
getShardIterator_shardIteratorType :: Lens.Lens' GetShardIterator ShardIteratorType
getShardIterator_shardIteratorType = Lens.lens (\GetShardIterator' {shardIteratorType} -> shardIteratorType) (\s@GetShardIterator' {} a -> s {shardIteratorType = a} :: GetShardIterator)

instance Core.AWSRequest GetShardIterator where
  type
    AWSResponse GetShardIterator =
      GetShardIteratorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetShardIteratorResponse'
            Prelude.<$> (x Data..?> "ShardIterator")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetShardIterator where
  hashWithSalt _salt GetShardIterator' {..} =
    _salt
      `Prelude.hashWithSalt` startingSequenceNumber
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` shardId
      `Prelude.hashWithSalt` shardIteratorType

instance Prelude.NFData GetShardIterator where
  rnf GetShardIterator' {..} =
    Prelude.rnf startingSequenceNumber
      `Prelude.seq` Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf shardId
      `Prelude.seq` Prelude.rnf shardIteratorType

instance Data.ToHeaders GetShardIterator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Kinesis_20131202.GetShardIterator" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetShardIterator where
  toJSON GetShardIterator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StartingSequenceNumber" Data..=)
              Prelude.<$> startingSequenceNumber,
            ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName,
            ("Timestamp" Data..=) Prelude.<$> timestamp,
            Prelude.Just ("ShardId" Data..= shardId),
            Prelude.Just
              ("ShardIteratorType" Data..= shardIteratorType)
          ]
      )

instance Data.ToPath GetShardIterator where
  toPath = Prelude.const "/"

instance Data.ToQuery GetShardIterator where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output for @GetShardIterator@.
--
-- /See:/ 'newGetShardIteratorResponse' smart constructor.
data GetShardIteratorResponse = GetShardIteratorResponse'
  { -- | The position in the shard from which to start reading data records
    -- sequentially. A shard iterator specifies this position using the
    -- sequence number of a data record in a shard.
    shardIterator :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetShardIteratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shardIterator', 'getShardIteratorResponse_shardIterator' - The position in the shard from which to start reading data records
-- sequentially. A shard iterator specifies this position using the
-- sequence number of a data record in a shard.
--
-- 'httpStatus', 'getShardIteratorResponse_httpStatus' - The response's http status code.
newGetShardIteratorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetShardIteratorResponse
newGetShardIteratorResponse pHttpStatus_ =
  GetShardIteratorResponse'
    { shardIterator =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The position in the shard from which to start reading data records
-- sequentially. A shard iterator specifies this position using the
-- sequence number of a data record in a shard.
getShardIteratorResponse_shardIterator :: Lens.Lens' GetShardIteratorResponse (Prelude.Maybe Prelude.Text)
getShardIteratorResponse_shardIterator = Lens.lens (\GetShardIteratorResponse' {shardIterator} -> shardIterator) (\s@GetShardIteratorResponse' {} a -> s {shardIterator = a} :: GetShardIteratorResponse)

-- | The response's http status code.
getShardIteratorResponse_httpStatus :: Lens.Lens' GetShardIteratorResponse Prelude.Int
getShardIteratorResponse_httpStatus = Lens.lens (\GetShardIteratorResponse' {httpStatus} -> httpStatus) (\s@GetShardIteratorResponse' {} a -> s {httpStatus = a} :: GetShardIteratorResponse)

instance Prelude.NFData GetShardIteratorResponse where
  rnf GetShardIteratorResponse' {..} =
    Prelude.rnf shardIterator
      `Prelude.seq` Prelude.rnf httpStatus
