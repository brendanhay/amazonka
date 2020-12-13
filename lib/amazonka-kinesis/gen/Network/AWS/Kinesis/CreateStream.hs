{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.CreateStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Kinesis data stream. A stream captures and transports data records that are continuously emitted from different data sources or /producers/ . Scale-out within a stream is explicitly supported by means of shards, which are uniquely identified groups of data records in a stream.
--
-- You specify and control the number of shards that a stream is composed of. Each shard can support reads up to five transactions per second, up to a maximum data read total of 2 MiB per second. Each shard can support writes up to 1,000 records per second, up to a maximum data write total of 1 MiB per second. If the amount of data input increases or decreases, you can add or remove shards.
-- The stream name identifies the stream. The name is scoped to the AWS account used by the application. It is also scoped by AWS Region. That is, two streams in two different accounts can have the same name, and two streams in the same account, but in two different Regions, can have the same name.
-- @CreateStream@ is an asynchronous operation. Upon receiving a @CreateStream@ request, Kinesis Data Streams immediately returns and sets the stream status to @CREATING@ . After the stream is created, Kinesis Data Streams sets the stream status to @ACTIVE@ . You should perform read and write operations only on an @ACTIVE@ stream.
-- You receive a @LimitExceededException@ when making a @CreateStream@ request when you try to do one of the following:
--
--     * Have more than five streams in the @CREATING@ state at any point in time.
--
--
--     * Create more shards than are authorized for your account.
--
--
-- For the default shard limit for an AWS account, see <https://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Amazon Kinesis Data Streams Limits> in the /Amazon Kinesis Data Streams Developer Guide/ . To increase this limit, <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html contact AWS Support> .
-- You can use @DescribeStream@ to check the stream status, which is returned in @StreamStatus@ .
-- 'CreateStream' has a limit of five transactions per second per account.
module Network.AWS.Kinesis.CreateStream
  ( -- * Creating a request
    CreateStream (..),
    mkCreateStream,

    -- ** Request lenses
    csShardCount,
    csStreamName,

    -- * Destructuring the response
    CreateStreamResponse (..),
    mkCreateStreamResponse,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for @CreateStream@ .
--
-- /See:/ 'mkCreateStream' smart constructor.
data CreateStream = CreateStream'
  { -- | The number of shards that the stream will use. The throughput of the stream is a function of the number of shards; more shards are required for greater provisioned throughput.
    shardCount :: Lude.Natural,
    -- | A name to identify the stream. The stream name is scoped to the AWS account used by the application that creates the stream. It is also scoped by AWS Region. That is, two streams in two different AWS accounts can have the same name. Two streams in the same AWS account but in two different Regions can also have the same name.
    streamName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStream' with the minimum fields required to make a request.
--
-- * 'shardCount' - The number of shards that the stream will use. The throughput of the stream is a function of the number of shards; more shards are required for greater provisioned throughput.
-- * 'streamName' - A name to identify the stream. The stream name is scoped to the AWS account used by the application that creates the stream. It is also scoped by AWS Region. That is, two streams in two different AWS accounts can have the same name. Two streams in the same AWS account but in two different Regions can also have the same name.
mkCreateStream ::
  -- | 'shardCount'
  Lude.Natural ->
  -- | 'streamName'
  Lude.Text ->
  CreateStream
mkCreateStream pShardCount_ pStreamName_ =
  CreateStream'
    { shardCount = pShardCount_,
      streamName = pStreamName_
    }

-- | The number of shards that the stream will use. The throughput of the stream is a function of the number of shards; more shards are required for greater provisioned throughput.
--
-- /Note:/ Consider using 'shardCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csShardCount :: Lens.Lens' CreateStream Lude.Natural
csShardCount = Lens.lens (shardCount :: CreateStream -> Lude.Natural) (\s a -> s {shardCount = a} :: CreateStream)
{-# DEPRECATED csShardCount "Use generic-lens or generic-optics with 'shardCount' instead." #-}

-- | A name to identify the stream. The stream name is scoped to the AWS account used by the application that creates the stream. It is also scoped by AWS Region. That is, two streams in two different AWS accounts can have the same name. Two streams in the same AWS account but in two different Regions can also have the same name.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStreamName :: Lens.Lens' CreateStream Lude.Text
csStreamName = Lens.lens (streamName :: CreateStream -> Lude.Text) (\s a -> s {streamName = a} :: CreateStream)
{-# DEPRECATED csStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest CreateStream where
  type Rs CreateStream = CreateStreamResponse
  request = Req.postJSON kinesisService
  response = Res.receiveNull CreateStreamResponse'

instance Lude.ToHeaders CreateStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.CreateStream" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateStream where
  toJSON CreateStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ShardCount" Lude..= shardCount),
            Lude.Just ("StreamName" Lude..= streamName)
          ]
      )

instance Lude.ToPath CreateStream where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateStreamResponse' smart constructor.
data CreateStreamResponse = CreateStreamResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStreamResponse' with the minimum fields required to make a request.
mkCreateStreamResponse ::
  CreateStreamResponse
mkCreateStreamResponse = CreateStreamResponse'
