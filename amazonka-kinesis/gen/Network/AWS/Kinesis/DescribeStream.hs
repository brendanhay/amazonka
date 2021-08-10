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
-- Module      : Network.AWS.Kinesis.DescribeStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Kinesis data stream.
--
-- The information returned includes the stream name, Amazon Resource Name
-- (ARN), creation time, enhanced metric configuration, and shard map. The
-- shard map is an array of shard objects. For each shard object, there is
-- the hash key and sequence number ranges that the shard spans, and the
-- IDs of any earlier shards that played in a role in creating the shard.
-- Every record ingested in the stream is identified by a sequence number,
-- which is assigned when the record is put into the stream.
--
-- You can limit the number of shards returned by each call. For more
-- information, see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-retrieve-shards.html Retrieving Shards from a Stream>
-- in the /Amazon Kinesis Data Streams Developer Guide/.
--
-- There are no guarantees about the chronological order shards returned.
-- To process shards in chronological order, use the ID of the parent shard
-- to track the lineage to the oldest shard.
--
-- This operation has a limit of 10 transactions per second per account.
--
-- This operation returns paginated results.
module Network.AWS.Kinesis.DescribeStream
  ( -- * Creating a Request
    DescribeStream (..),
    newDescribeStream,

    -- * Request Lenses
    describeStream_exclusiveStartShardId,
    describeStream_limit,
    describeStream_streamName,

    -- * Destructuring the Response
    DescribeStreamResponse (..),
    newDescribeStreamResponse,

    -- * Response Lenses
    describeStreamResponse_httpStatus,
    describeStreamResponse_streamDescription,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for @DescribeStream@.
--
-- /See:/ 'newDescribeStream' smart constructor.
data DescribeStream = DescribeStream'
  { -- | The shard ID of the shard to start with.
    exclusiveStartShardId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of shards to return in a single call. The default
    -- value is 100. If you specify a value greater than 100, at most 100
    -- shards are returned.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of the stream to describe.
    streamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exclusiveStartShardId', 'describeStream_exclusiveStartShardId' - The shard ID of the shard to start with.
--
-- 'limit', 'describeStream_limit' - The maximum number of shards to return in a single call. The default
-- value is 100. If you specify a value greater than 100, at most 100
-- shards are returned.
--
-- 'streamName', 'describeStream_streamName' - The name of the stream to describe.
newDescribeStream ::
  -- | 'streamName'
  Prelude.Text ->
  DescribeStream
newDescribeStream pStreamName_ =
  DescribeStream'
    { exclusiveStartShardId =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      streamName = pStreamName_
    }

-- | The shard ID of the shard to start with.
describeStream_exclusiveStartShardId :: Lens.Lens' DescribeStream (Prelude.Maybe Prelude.Text)
describeStream_exclusiveStartShardId = Lens.lens (\DescribeStream' {exclusiveStartShardId} -> exclusiveStartShardId) (\s@DescribeStream' {} a -> s {exclusiveStartShardId = a} :: DescribeStream)

-- | The maximum number of shards to return in a single call. The default
-- value is 100. If you specify a value greater than 100, at most 100
-- shards are returned.
describeStream_limit :: Lens.Lens' DescribeStream (Prelude.Maybe Prelude.Natural)
describeStream_limit = Lens.lens (\DescribeStream' {limit} -> limit) (\s@DescribeStream' {} a -> s {limit = a} :: DescribeStream)

-- | The name of the stream to describe.
describeStream_streamName :: Lens.Lens' DescribeStream Prelude.Text
describeStream_streamName = Lens.lens (\DescribeStream' {streamName} -> streamName) (\s@DescribeStream' {} a -> s {streamName = a} :: DescribeStream)

instance Core.AWSPager DescribeStream where
  page rq rs
    | Core.stop
        ( rs
            Lens.^. describeStreamResponse_streamDescription
              Prelude.. streamDescription_hasMoreShards
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? describeStreamResponse_streamDescription
              Prelude.. streamDescription_shards
              Prelude.. Lens._last
              Prelude.. shard_shardId
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeStream_exclusiveStartShardId
          Lens..~ rs
          Lens.^? describeStreamResponse_streamDescription
            Prelude.. streamDescription_shards
            Prelude.. Lens._last
            Prelude.. shard_shardId

instance Core.AWSRequest DescribeStream where
  type
    AWSResponse DescribeStream =
      DescribeStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "StreamDescription")
      )

instance Prelude.Hashable DescribeStream

instance Prelude.NFData DescribeStream

instance Core.ToHeaders DescribeStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Kinesis_20131202.DescribeStream" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeStream where
  toJSON DescribeStream' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ExclusiveStartShardId" Core..=)
              Prelude.<$> exclusiveStartShardId,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just ("StreamName" Core..= streamName)
          ]
      )

instance Core.ToPath DescribeStream where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeStream where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output for @DescribeStream@.
--
-- /See:/ 'newDescribeStreamResponse' smart constructor.
data DescribeStreamResponse = DescribeStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The current status of the stream, the stream Amazon Resource Name (ARN),
    -- an array of shard objects that comprise the stream, and whether there
    -- are more shards available.
    streamDescription :: StreamDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeStreamResponse_httpStatus' - The response's http status code.
--
-- 'streamDescription', 'describeStreamResponse_streamDescription' - The current status of the stream, the stream Amazon Resource Name (ARN),
-- an array of shard objects that comprise the stream, and whether there
-- are more shards available.
newDescribeStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'streamDescription'
  StreamDescription ->
  DescribeStreamResponse
newDescribeStreamResponse
  pHttpStatus_
  pStreamDescription_ =
    DescribeStreamResponse'
      { httpStatus = pHttpStatus_,
        streamDescription = pStreamDescription_
      }

-- | The response's http status code.
describeStreamResponse_httpStatus :: Lens.Lens' DescribeStreamResponse Prelude.Int
describeStreamResponse_httpStatus = Lens.lens (\DescribeStreamResponse' {httpStatus} -> httpStatus) (\s@DescribeStreamResponse' {} a -> s {httpStatus = a} :: DescribeStreamResponse)

-- | The current status of the stream, the stream Amazon Resource Name (ARN),
-- an array of shard objects that comprise the stream, and whether there
-- are more shards available.
describeStreamResponse_streamDescription :: Lens.Lens' DescribeStreamResponse StreamDescription
describeStreamResponse_streamDescription = Lens.lens (\DescribeStreamResponse' {streamDescription} -> streamDescription) (\s@DescribeStreamResponse' {} a -> s {streamDescription = a} :: DescribeStreamResponse)

instance Prelude.NFData DescribeStreamResponse
