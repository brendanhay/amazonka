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
-- Module      : Amazonka.Kinesis.CreateStream
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Kinesis data stream. A stream captures and transports data
-- records that are continuously emitted from different data sources or
-- /producers/. Scale-out within a stream is explicitly supported by means
-- of shards, which are uniquely identified groups of data records in a
-- stream.
--
-- You specify and control the number of shards that a stream is composed
-- of. Each shard can support reads up to five transactions per second, up
-- to a maximum data read total of 2 MiB per second. Each shard can support
-- writes up to 1,000 records per second, up to a maximum data write total
-- of 1 MiB per second. If the amount of data input increases or decreases,
-- you can add or remove shards.
--
-- The stream name identifies the stream. The name is scoped to the Amazon
-- Web Services account used by the application. It is also scoped by
-- Amazon Web Services Region. That is, two streams in two different
-- accounts can have the same name, and two streams in the same account,
-- but in two different Regions, can have the same name.
--
-- @CreateStream@ is an asynchronous operation. Upon receiving a
-- @CreateStream@ request, Kinesis Data Streams immediately returns and
-- sets the stream status to @CREATING@. After the stream is created,
-- Kinesis Data Streams sets the stream status to @ACTIVE@. You should
-- perform read and write operations only on an @ACTIVE@ stream.
--
-- You receive a @LimitExceededException@ when making a @CreateStream@
-- request when you try to do one of the following:
--
-- -   Have more than five streams in the @CREATING@ state at any point in
--     time.
--
-- -   Create more shards than are authorized for your account.
--
-- For the default shard limit for an Amazon Web Services account, see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Amazon Kinesis Data Streams Limits>
-- in the /Amazon Kinesis Data Streams Developer Guide/. To increase this
-- limit,
-- <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html contact Amazon Web Services Support>.
--
-- You can use DescribeStreamSummary to check the stream status, which is
-- returned in @StreamStatus@.
--
-- CreateStream has a limit of five transactions per second per account.
module Amazonka.Kinesis.CreateStream
  ( -- * Creating a Request
    CreateStream (..),
    newCreateStream,

    -- * Request Lenses
    createStream_shardCount,
    createStream_streamModeDetails,
    createStream_streamName,

    -- * Destructuring the Response
    CreateStreamResponse (..),
    newCreateStreamResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for @CreateStream@.
--
-- /See:/ 'newCreateStream' smart constructor.
data CreateStream = CreateStream'
  { -- | The number of shards that the stream will use. The throughput of the
    -- stream is a function of the number of shards; more shards are required
    -- for greater provisioned throughput.
    shardCount :: Prelude.Maybe Prelude.Natural,
    -- | Indicates the capacity mode of the data stream. Currently, in Kinesis
    -- Data Streams, you can choose between an __on-demand__ capacity mode and
    -- a __provisioned__ capacity mode for your data streams.
    streamModeDetails :: Prelude.Maybe StreamModeDetails,
    -- | A name to identify the stream. The stream name is scoped to the Amazon
    -- Web Services account used by the application that creates the stream. It
    -- is also scoped by Amazon Web Services Region. That is, two streams in
    -- two different Amazon Web Services accounts can have the same name. Two
    -- streams in the same Amazon Web Services account but in two different
    -- Regions can also have the same name.
    streamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shardCount', 'createStream_shardCount' - The number of shards that the stream will use. The throughput of the
-- stream is a function of the number of shards; more shards are required
-- for greater provisioned throughput.
--
-- 'streamModeDetails', 'createStream_streamModeDetails' - Indicates the capacity mode of the data stream. Currently, in Kinesis
-- Data Streams, you can choose between an __on-demand__ capacity mode and
-- a __provisioned__ capacity mode for your data streams.
--
-- 'streamName', 'createStream_streamName' - A name to identify the stream. The stream name is scoped to the Amazon
-- Web Services account used by the application that creates the stream. It
-- is also scoped by Amazon Web Services Region. That is, two streams in
-- two different Amazon Web Services accounts can have the same name. Two
-- streams in the same Amazon Web Services account but in two different
-- Regions can also have the same name.
newCreateStream ::
  -- | 'streamName'
  Prelude.Text ->
  CreateStream
newCreateStream pStreamName_ =
  CreateStream'
    { shardCount = Prelude.Nothing,
      streamModeDetails = Prelude.Nothing,
      streamName = pStreamName_
    }

-- | The number of shards that the stream will use. The throughput of the
-- stream is a function of the number of shards; more shards are required
-- for greater provisioned throughput.
createStream_shardCount :: Lens.Lens' CreateStream (Prelude.Maybe Prelude.Natural)
createStream_shardCount = Lens.lens (\CreateStream' {shardCount} -> shardCount) (\s@CreateStream' {} a -> s {shardCount = a} :: CreateStream)

-- | Indicates the capacity mode of the data stream. Currently, in Kinesis
-- Data Streams, you can choose between an __on-demand__ capacity mode and
-- a __provisioned__ capacity mode for your data streams.
createStream_streamModeDetails :: Lens.Lens' CreateStream (Prelude.Maybe StreamModeDetails)
createStream_streamModeDetails = Lens.lens (\CreateStream' {streamModeDetails} -> streamModeDetails) (\s@CreateStream' {} a -> s {streamModeDetails = a} :: CreateStream)

-- | A name to identify the stream. The stream name is scoped to the Amazon
-- Web Services account used by the application that creates the stream. It
-- is also scoped by Amazon Web Services Region. That is, two streams in
-- two different Amazon Web Services accounts can have the same name. Two
-- streams in the same Amazon Web Services account but in two different
-- Regions can also have the same name.
createStream_streamName :: Lens.Lens' CreateStream Prelude.Text
createStream_streamName = Lens.lens (\CreateStream' {streamName} -> streamName) (\s@CreateStream' {} a -> s {streamName = a} :: CreateStream)

instance Core.AWSRequest CreateStream where
  type AWSResponse CreateStream = CreateStreamResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull CreateStreamResponse'

instance Prelude.Hashable CreateStream where
  hashWithSalt _salt CreateStream' {..} =
    _salt `Prelude.hashWithSalt` shardCount
      `Prelude.hashWithSalt` streamModeDetails
      `Prelude.hashWithSalt` streamName

instance Prelude.NFData CreateStream where
  rnf CreateStream' {..} =
    Prelude.rnf shardCount
      `Prelude.seq` Prelude.rnf streamModeDetails
      `Prelude.seq` Prelude.rnf streamName

instance Data.ToHeaders CreateStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Kinesis_20131202.CreateStream" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateStream where
  toJSON CreateStream' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ShardCount" Data..=) Prelude.<$> shardCount,
            ("StreamModeDetails" Data..=)
              Prelude.<$> streamModeDetails,
            Prelude.Just ("StreamName" Data..= streamName)
          ]
      )

instance Data.ToPath CreateStream where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStreamResponse' smart constructor.
data CreateStreamResponse = CreateStreamResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateStreamResponse ::
  CreateStreamResponse
newCreateStreamResponse = CreateStreamResponse'

instance Prelude.NFData CreateStreamResponse where
  rnf _ = ()
