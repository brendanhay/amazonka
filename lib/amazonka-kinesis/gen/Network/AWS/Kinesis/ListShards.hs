{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.ListShards
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the shards in a stream and provides information about each shard. This operation has a limit of 100 transactions per second per data stream.
--
-- /Important:/ This API is a new operation that is used by the Amazon Kinesis Client Library (KCL). If you have a fine-grained IAM policy that only allows specific operations, you must update your policy to allow calls to this API. For more information, see <https://docs.aws.amazon.com/streams/latest/dev/controlling-access.html Controlling Access to Amazon Kinesis Data Streams Resources Using IAM> .
--
-- This operation returns paginated results.
module Network.AWS.Kinesis.ListShards
  ( -- * Creating a request
    ListShards (..),
    mkListShards,

    -- ** Request lenses
    lsExclusiveStartShardId,
    lsMaxResults,
    lsNextToken,
    lsShardFilter,
    lsStreamCreationTimestamp,
    lsStreamName,

    -- * Destructuring the response
    ListShardsResponse (..),
    mkListShardsResponse,

    -- ** Response lenses
    lsrrsNextToken,
    lsrrsShards,
    lsrrsResponseStatus,
  )
where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListShards' smart constructor.
data ListShards = ListShards'
  { -- | Specify this parameter to indicate that you want to list the shards starting with the shard whose ID immediately follows @ExclusiveStartShardId@ .
    --
    -- If you don't specify this parameter, the default behavior is for @ListShards@ to list the shards starting with the first one in the stream.
    -- You cannot specify this parameter if you specify @NextToken@ .
    exclusiveStartShardId :: Core.Maybe Types.ExclusiveStartShardId,
    -- | The maximum number of shards to return in a single call to @ListShards@ . The minimum value you can specify for this parameter is 1, and the maximum is 10,000, which is also the default.
    --
    -- When the number of shards to be listed is greater than the value of @MaxResults@ , the response contains a @NextToken@ value that you can use in a subsequent call to @ListShards@ to list the next set of shards.
    maxResults :: Core.Maybe Core.Natural,
    -- | When the number of shards in the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of shards in the data stream, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListShards@ to list the next set of shards.
    --
    -- Don't specify @StreamName@ or @StreamCreationTimestamp@ if you specify @NextToken@ because the latter unambiguously identifies the stream.
    -- You can optionally specify a value for the @MaxResults@ parameter when you specify @NextToken@ . If you specify a @MaxResults@ value that is less than the number of shards that the operation returns if you don't specify @MaxResults@ , the response will contain a new @NextToken@ value. You can use the new @NextToken@ value in a subsequent call to the @ListShards@ operation.
    -- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListShards@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListShards@ , you get @ExpiredNextTokenException@ .
    nextToken :: Core.Maybe Types.NextToken,
    shardFilter :: Core.Maybe Types.ShardFilter,
    -- | Specify this input parameter to distinguish data streams that have the same name. For example, if you create a data stream and then delete it, and you later create another data stream with the same name, you can use this input parameter to specify which of the two streams you want to list the shards for.
    --
    -- You cannot specify this parameter if you specify the @NextToken@ parameter.
    streamCreationTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the data stream whose shards you want to list.
    --
    -- You cannot specify this parameter if you specify the @NextToken@ parameter.
    streamName :: Core.Maybe Types.StreamName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListShards' value with any optional fields omitted.
mkListShards ::
  ListShards
mkListShards =
  ListShards'
    { exclusiveStartShardId = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      shardFilter = Core.Nothing,
      streamCreationTimestamp = Core.Nothing,
      streamName = Core.Nothing
    }

-- | Specify this parameter to indicate that you want to list the shards starting with the shard whose ID immediately follows @ExclusiveStartShardId@ .
--
-- If you don't specify this parameter, the default behavior is for @ListShards@ to list the shards starting with the first one in the stream.
-- You cannot specify this parameter if you specify @NextToken@ .
--
-- /Note:/ Consider using 'exclusiveStartShardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsExclusiveStartShardId :: Lens.Lens' ListShards (Core.Maybe Types.ExclusiveStartShardId)
lsExclusiveStartShardId = Lens.field @"exclusiveStartShardId"
{-# DEPRECATED lsExclusiveStartShardId "Use generic-lens or generic-optics with 'exclusiveStartShardId' instead." #-}

-- | The maximum number of shards to return in a single call to @ListShards@ . The minimum value you can specify for this parameter is 1, and the maximum is 10,000, which is also the default.
--
-- When the number of shards to be listed is greater than the value of @MaxResults@ , the response contains a @NextToken@ value that you can use in a subsequent call to @ListShards@ to list the next set of shards.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMaxResults :: Lens.Lens' ListShards (Core.Maybe Core.Natural)
lsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | When the number of shards in the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of shards in the data stream, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListShards@ to list the next set of shards.
--
-- Don't specify @StreamName@ or @StreamCreationTimestamp@ if you specify @NextToken@ because the latter unambiguously identifies the stream.
-- You can optionally specify a value for the @MaxResults@ parameter when you specify @NextToken@ . If you specify a @MaxResults@ value that is less than the number of shards that the operation returns if you don't specify @MaxResults@ , the response will contain a new @NextToken@ value. You can use the new @NextToken@ value in a subsequent call to the @ListShards@ operation.
-- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListShards@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListShards@ , you get @ExpiredNextTokenException@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListShards (Core.Maybe Types.NextToken)
lsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'shardFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsShardFilter :: Lens.Lens' ListShards (Core.Maybe Types.ShardFilter)
lsShardFilter = Lens.field @"shardFilter"
{-# DEPRECATED lsShardFilter "Use generic-lens or generic-optics with 'shardFilter' instead." #-}

-- | Specify this input parameter to distinguish data streams that have the same name. For example, if you create a data stream and then delete it, and you later create another data stream with the same name, you can use this input parameter to specify which of the two streams you want to list the shards for.
--
-- You cannot specify this parameter if you specify the @NextToken@ parameter.
--
-- /Note:/ Consider using 'streamCreationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsStreamCreationTimestamp :: Lens.Lens' ListShards (Core.Maybe Core.NominalDiffTime)
lsStreamCreationTimestamp = Lens.field @"streamCreationTimestamp"
{-# DEPRECATED lsStreamCreationTimestamp "Use generic-lens or generic-optics with 'streamCreationTimestamp' instead." #-}

-- | The name of the data stream whose shards you want to list.
--
-- You cannot specify this parameter if you specify the @NextToken@ parameter.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsStreamName :: Lens.Lens' ListShards (Core.Maybe Types.StreamName)
lsStreamName = Lens.field @"streamName"
{-# DEPRECATED lsStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Core.FromJSON ListShards where
  toJSON ListShards {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExclusiveStartShardId" Core..=) Core.<$> exclusiveStartShardId,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("ShardFilter" Core..=) Core.<$> shardFilter,
            ("StreamCreationTimestamp" Core..=)
              Core.<$> streamCreationTimestamp,
            ("StreamName" Core..=) Core.<$> streamName
          ]
      )

instance Core.AWSRequest ListShards where
  type Rs ListShards = ListShardsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Kinesis_20131202.ListShards")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListShardsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Shards")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListShards where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"shards" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListShardsResponse' smart constructor.
data ListShardsResponse = ListShardsResponse'
  { -- | When the number of shards in the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of shards in the data stream, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListShards@ to list the next set of shards. For more information about the use of this pagination token when calling the @ListShards@ operation, see 'ListShardsInput$NextToken' .
    --
    -- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListShards@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListShards@ , you get @ExpiredNextTokenException@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | An array of JSON objects. Each object represents one shard and specifies the IDs of the shard, the shard's parent, and the shard that's adjacent to the shard's parent. Each object also contains the starting and ending hash keys and the starting and ending sequence numbers for the shard.
    shards :: Core.Maybe [Types.Shard],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListShardsResponse' value with any optional fields omitted.
mkListShardsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListShardsResponse
mkListShardsResponse responseStatus =
  ListShardsResponse'
    { nextToken = Core.Nothing,
      shards = Core.Nothing,
      responseStatus
    }

-- | When the number of shards in the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of shards in the data stream, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListShards@ to list the next set of shards. For more information about the use of this pagination token when calling the @ListShards@ operation, see 'ListShardsInput$NextToken' .
--
-- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListShards@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListShards@ , you get @ExpiredNextTokenException@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsNextToken :: Lens.Lens' ListShardsResponse (Core.Maybe Types.NextToken)
lsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of JSON objects. Each object represents one shard and specifies the IDs of the shard, the shard's parent, and the shard that's adjacent to the shard's parent. Each object also contains the starting and ending hash keys and the starting and ending sequence numbers for the shard.
--
-- /Note:/ Consider using 'shards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsShards :: Lens.Lens' ListShardsResponse (Core.Maybe [Types.Shard])
lsrrsShards = Lens.field @"shards"
{-# DEPRECATED lsrrsShards "Use generic-lens or generic-optics with 'shards' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsResponseStatus :: Lens.Lens' ListShardsResponse Core.Int
lsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
