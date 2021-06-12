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
-- Module      : Network.AWS.DynamoDBStreams.DescribeStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a stream, including the current status of the
-- stream, its Amazon Resource Name (ARN), the composition of its shards,
-- and its corresponding DynamoDB table.
--
-- You can call @DescribeStream@ at a maximum rate of 10 times per second.
--
-- Each shard in the stream has a @SequenceNumberRange@ associated with it.
-- If the @SequenceNumberRange@ has a @StartingSequenceNumber@ but no
-- @EndingSequenceNumber@, then the shard is still open (able to receive
-- more stream records). If both @StartingSequenceNumber@ and
-- @EndingSequenceNumber@ are present, then that shard is closed and can no
-- longer receive more data.
module Network.AWS.DynamoDBStreams.DescribeStream
  ( -- * Creating a Request
    DescribeStream (..),
    newDescribeStream,

    -- * Request Lenses
    describeStream_exclusiveStartShardId,
    describeStream_limit,
    describeStream_streamArn,

    -- * Destructuring the Response
    DescribeStreamResponse (..),
    newDescribeStreamResponse,

    -- * Response Lenses
    describeStreamResponse_streamDescription,
    describeStreamResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDBStreams.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeStream@ operation.
--
-- /See:/ 'newDescribeStream' smart constructor.
data DescribeStream = DescribeStream'
  { -- | The shard ID of the first item that this operation will evaluate. Use
    -- the value that was returned for @LastEvaluatedShardId@ in the previous
    -- operation.
    exclusiveStartShardId :: Core.Maybe Core.Text,
    -- | The maximum number of shard objects to return. The upper limit is 100.
    limit :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) for the stream.
    streamArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exclusiveStartShardId', 'describeStream_exclusiveStartShardId' - The shard ID of the first item that this operation will evaluate. Use
-- the value that was returned for @LastEvaluatedShardId@ in the previous
-- operation.
--
-- 'limit', 'describeStream_limit' - The maximum number of shard objects to return. The upper limit is 100.
--
-- 'streamArn', 'describeStream_streamArn' - The Amazon Resource Name (ARN) for the stream.
newDescribeStream ::
  -- | 'streamArn'
  Core.Text ->
  DescribeStream
newDescribeStream pStreamArn_ =
  DescribeStream'
    { exclusiveStartShardId =
        Core.Nothing,
      limit = Core.Nothing,
      streamArn = pStreamArn_
    }

-- | The shard ID of the first item that this operation will evaluate. Use
-- the value that was returned for @LastEvaluatedShardId@ in the previous
-- operation.
describeStream_exclusiveStartShardId :: Lens.Lens' DescribeStream (Core.Maybe Core.Text)
describeStream_exclusiveStartShardId = Lens.lens (\DescribeStream' {exclusiveStartShardId} -> exclusiveStartShardId) (\s@DescribeStream' {} a -> s {exclusiveStartShardId = a} :: DescribeStream)

-- | The maximum number of shard objects to return. The upper limit is 100.
describeStream_limit :: Lens.Lens' DescribeStream (Core.Maybe Core.Natural)
describeStream_limit = Lens.lens (\DescribeStream' {limit} -> limit) (\s@DescribeStream' {} a -> s {limit = a} :: DescribeStream)

-- | The Amazon Resource Name (ARN) for the stream.
describeStream_streamArn :: Lens.Lens' DescribeStream Core.Text
describeStream_streamArn = Lens.lens (\DescribeStream' {streamArn} -> streamArn) (\s@DescribeStream' {} a -> s {streamArn = a} :: DescribeStream)

instance Core.AWSRequest DescribeStream where
  type
    AWSResponse DescribeStream =
      DescribeStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStreamResponse'
            Core.<$> (x Core..?> "StreamDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeStream

instance Core.NFData DescribeStream

instance Core.ToHeaders DescribeStream where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDBStreams_20120810.DescribeStream" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeStream where
  toJSON DescribeStream' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExclusiveStartShardId" Core..=)
              Core.<$> exclusiveStartShardId,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just ("StreamArn" Core..= streamArn)
          ]
      )

instance Core.ToPath DescribeStream where
  toPath = Core.const "/"

instance Core.ToQuery DescribeStream where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @DescribeStream@ operation.
--
-- /See:/ 'newDescribeStreamResponse' smart constructor.
data DescribeStreamResponse = DescribeStreamResponse'
  { -- | A complete description of the stream, including its creation date and
    -- time, the DynamoDB table associated with the stream, the shard IDs
    -- within the stream, and the beginning and ending sequence numbers of
    -- stream records within the shards.
    streamDescription :: Core.Maybe StreamDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamDescription', 'describeStreamResponse_streamDescription' - A complete description of the stream, including its creation date and
-- time, the DynamoDB table associated with the stream, the shard IDs
-- within the stream, and the beginning and ending sequence numbers of
-- stream records within the shards.
--
-- 'httpStatus', 'describeStreamResponse_httpStatus' - The response's http status code.
newDescribeStreamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeStreamResponse
newDescribeStreamResponse pHttpStatus_ =
  DescribeStreamResponse'
    { streamDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A complete description of the stream, including its creation date and
-- time, the DynamoDB table associated with the stream, the shard IDs
-- within the stream, and the beginning and ending sequence numbers of
-- stream records within the shards.
describeStreamResponse_streamDescription :: Lens.Lens' DescribeStreamResponse (Core.Maybe StreamDescription)
describeStreamResponse_streamDescription = Lens.lens (\DescribeStreamResponse' {streamDescription} -> streamDescription) (\s@DescribeStreamResponse' {} a -> s {streamDescription = a} :: DescribeStreamResponse)

-- | The response's http status code.
describeStreamResponse_httpStatus :: Lens.Lens' DescribeStreamResponse Core.Int
describeStreamResponse_httpStatus = Lens.lens (\DescribeStreamResponse' {httpStatus} -> httpStatus) (\s@DescribeStreamResponse' {} a -> s {httpStatus = a} :: DescribeStreamResponse)

instance Core.NFData DescribeStreamResponse
