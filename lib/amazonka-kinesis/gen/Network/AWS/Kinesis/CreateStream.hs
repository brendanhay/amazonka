{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateStream (..)
    , mkCreateStream
    -- ** Request lenses
    , csStreamName
    , csShardCount

    -- * Destructuring the response
    , CreateStreamResponse (..)
    , mkCreateStreamResponse
    ) where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for @CreateStream@ .
--
-- /See:/ 'mkCreateStream' smart constructor.
data CreateStream = CreateStream'
  { streamName :: Types.StreamName
    -- ^ A name to identify the stream. The stream name is scoped to the AWS account used by the application that creates the stream. It is also scoped by AWS Region. That is, two streams in two different AWS accounts can have the same name. Two streams in the same AWS account but in two different Regions can also have the same name.
  , shardCount :: Core.Natural
    -- ^ The number of shards that the stream will use. The throughput of the stream is a function of the number of shards; more shards are required for greater provisioned throughput.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStream' value with any optional fields omitted.
mkCreateStream
    :: Types.StreamName -- ^ 'streamName'
    -> Core.Natural -- ^ 'shardCount'
    -> CreateStream
mkCreateStream streamName shardCount
  = CreateStream'{streamName, shardCount}

-- | A name to identify the stream. The stream name is scoped to the AWS account used by the application that creates the stream. It is also scoped by AWS Region. That is, two streams in two different AWS accounts can have the same name. Two streams in the same AWS account but in two different Regions can also have the same name.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStreamName :: Lens.Lens' CreateStream Types.StreamName
csStreamName = Lens.field @"streamName"
{-# INLINEABLE csStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

-- | The number of shards that the stream will use. The throughput of the stream is a function of the number of shards; more shards are required for greater provisioned throughput.
--
-- /Note:/ Consider using 'shardCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csShardCount :: Lens.Lens' CreateStream Core.Natural
csShardCount = Lens.field @"shardCount"
{-# INLINEABLE csShardCount #-}
{-# DEPRECATED shardCount "Use generic-lens or generic-optics with 'shardCount' instead"  #-}

instance Core.ToQuery CreateStream where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateStream where
        toHeaders CreateStream{..}
          = Core.pure ("X-Amz-Target", "Kinesis_20131202.CreateStream")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateStream where
        toJSON CreateStream{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StreamName" Core..= streamName),
                  Core.Just ("ShardCount" Core..= shardCount)])

instance Core.AWSRequest CreateStream where
        type Rs CreateStream = CreateStreamResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull CreateStreamResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateStreamResponse' smart constructor.
data CreateStreamResponse = CreateStreamResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStreamResponse' value with any optional fields omitted.
mkCreateStreamResponse
    :: CreateStreamResponse
mkCreateStreamResponse = CreateStreamResponse'
