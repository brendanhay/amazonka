{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    lsShardFilter,
    lsNextToken,
    lsExclusiveStartShardId,
    lsStreamCreationTimestamp,
    lsStreamName,
    lsMaxResults,

    -- * Destructuring the response
    ListShardsResponse (..),
    mkListShardsResponse,

    -- ** Response lenses
    lrsNextToken,
    lrsShards,
    lrsResponseStatus,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListShards' smart constructor.
data ListShards = ListShards'
  { shardFilter ::
      Lude.Maybe ShardFilter,
    nextToken :: Lude.Maybe Lude.Text,
    exclusiveStartShardId :: Lude.Maybe Lude.Text,
    streamCreationTimestamp :: Lude.Maybe Lude.Timestamp,
    streamName :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListShards' with the minimum fields required to make a request.
--
-- * 'exclusiveStartShardId' - Specify this parameter to indicate that you want to list the shards starting with the shard whose ID immediately follows @ExclusiveStartShardId@ .
--
-- If you don't specify this parameter, the default behavior is for @ListShards@ to list the shards starting with the first one in the stream.
-- You cannot specify this parameter if you specify @NextToken@ .
-- * 'maxResults' - The maximum number of shards to return in a single call to @ListShards@ . The minimum value you can specify for this parameter is 1, and the maximum is 10,000, which is also the default.
--
-- When the number of shards to be listed is greater than the value of @MaxResults@ , the response contains a @NextToken@ value that you can use in a subsequent call to @ListShards@ to list the next set of shards.
-- * 'nextToken' - When the number of shards in the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of shards in the data stream, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListShards@ to list the next set of shards.
--
-- Don't specify @StreamName@ or @StreamCreationTimestamp@ if you specify @NextToken@ because the latter unambiguously identifies the stream.
-- You can optionally specify a value for the @MaxResults@ parameter when you specify @NextToken@ . If you specify a @MaxResults@ value that is less than the number of shards that the operation returns if you don't specify @MaxResults@ , the response will contain a new @NextToken@ value. You can use the new @NextToken@ value in a subsequent call to the @ListShards@ operation.
-- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListShards@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListShards@ , you get @ExpiredNextTokenException@ .
-- * 'shardFilter' - Undocumented field.
-- * 'streamCreationTimestamp' - Specify this input parameter to distinguish data streams that have the same name. For example, if you create a data stream and then delete it, and you later create another data stream with the same name, you can use this input parameter to specify which of the two streams you want to list the shards for.
--
-- You cannot specify this parameter if you specify the @NextToken@ parameter.
-- * 'streamName' - The name of the data stream whose shards you want to list.
--
-- You cannot specify this parameter if you specify the @NextToken@ parameter.
mkListShards ::
  ListShards
mkListShards =
  ListShards'
    { shardFilter = Lude.Nothing,
      nextToken = Lude.Nothing,
      exclusiveStartShardId = Lude.Nothing,
      streamCreationTimestamp = Lude.Nothing,
      streamName = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'shardFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsShardFilter :: Lens.Lens' ListShards (Lude.Maybe ShardFilter)
lsShardFilter = Lens.lens (shardFilter :: ListShards -> Lude.Maybe ShardFilter) (\s a -> s {shardFilter = a} :: ListShards)
{-# DEPRECATED lsShardFilter "Use generic-lens or generic-optics with 'shardFilter' instead." #-}

-- | When the number of shards in the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of shards in the data stream, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListShards@ to list the next set of shards.
--
-- Don't specify @StreamName@ or @StreamCreationTimestamp@ if you specify @NextToken@ because the latter unambiguously identifies the stream.
-- You can optionally specify a value for the @MaxResults@ parameter when you specify @NextToken@ . If you specify a @MaxResults@ value that is less than the number of shards that the operation returns if you don't specify @MaxResults@ , the response will contain a new @NextToken@ value. You can use the new @NextToken@ value in a subsequent call to the @ListShards@ operation.
-- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListShards@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListShards@ , you get @ExpiredNextTokenException@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListShards (Lude.Maybe Lude.Text)
lsNextToken = Lens.lens (nextToken :: ListShards -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListShards)
{-# DEPRECATED lsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specify this parameter to indicate that you want to list the shards starting with the shard whose ID immediately follows @ExclusiveStartShardId@ .
--
-- If you don't specify this parameter, the default behavior is for @ListShards@ to list the shards starting with the first one in the stream.
-- You cannot specify this parameter if you specify @NextToken@ .
--
-- /Note:/ Consider using 'exclusiveStartShardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsExclusiveStartShardId :: Lens.Lens' ListShards (Lude.Maybe Lude.Text)
lsExclusiveStartShardId = Lens.lens (exclusiveStartShardId :: ListShards -> Lude.Maybe Lude.Text) (\s a -> s {exclusiveStartShardId = a} :: ListShards)
{-# DEPRECATED lsExclusiveStartShardId "Use generic-lens or generic-optics with 'exclusiveStartShardId' instead." #-}

-- | Specify this input parameter to distinguish data streams that have the same name. For example, if you create a data stream and then delete it, and you later create another data stream with the same name, you can use this input parameter to specify which of the two streams you want to list the shards for.
--
-- You cannot specify this parameter if you specify the @NextToken@ parameter.
--
-- /Note:/ Consider using 'streamCreationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsStreamCreationTimestamp :: Lens.Lens' ListShards (Lude.Maybe Lude.Timestamp)
lsStreamCreationTimestamp = Lens.lens (streamCreationTimestamp :: ListShards -> Lude.Maybe Lude.Timestamp) (\s a -> s {streamCreationTimestamp = a} :: ListShards)
{-# DEPRECATED lsStreamCreationTimestamp "Use generic-lens or generic-optics with 'streamCreationTimestamp' instead." #-}

-- | The name of the data stream whose shards you want to list.
--
-- You cannot specify this parameter if you specify the @NextToken@ parameter.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsStreamName :: Lens.Lens' ListShards (Lude.Maybe Lude.Text)
lsStreamName = Lens.lens (streamName :: ListShards -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: ListShards)
{-# DEPRECATED lsStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The maximum number of shards to return in a single call to @ListShards@ . The minimum value you can specify for this parameter is 1, and the maximum is 10,000, which is also the default.
--
-- When the number of shards to be listed is greater than the value of @MaxResults@ , the response contains a @NextToken@ value that you can use in a subsequent call to @ListShards@ to list the next set of shards.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMaxResults :: Lens.Lens' ListShards (Lude.Maybe Lude.Natural)
lsMaxResults = Lens.lens (maxResults :: ListShards -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListShards)
{-# DEPRECATED lsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListShards where
  page rq rs
    | Page.stop (rs Lens.^. lrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrsShards) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsNextToken Lens..~ rs Lens.^. lrsNextToken

instance Lude.AWSRequest ListShards where
  type Rs ListShards = ListShardsResponse
  request = Req.postJSON kinesisService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListShardsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Shards" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListShards where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.ListShards" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListShards where
  toJSON ListShards' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ShardFilter" Lude..=) Lude.<$> shardFilter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("ExclusiveStartShardId" Lude..=) Lude.<$> exclusiveStartShardId,
            ("StreamCreationTimestamp" Lude..=)
              Lude.<$> streamCreationTimestamp,
            ("StreamName" Lude..=) Lude.<$> streamName,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListShards where
  toPath = Lude.const "/"

instance Lude.ToQuery ListShards where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListShardsResponse' smart constructor.
data ListShardsResponse = ListShardsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    shards :: Lude.Maybe [Shard],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListShardsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - When the number of shards in the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of shards in the data stream, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListShards@ to list the next set of shards. For more information about the use of this pagination token when calling the @ListShards@ operation, see 'ListShardsInput$NextToken' .
--
-- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListShards@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListShards@ , you get @ExpiredNextTokenException@ .
-- * 'responseStatus' - The response status code.
-- * 'shards' - An array of JSON objects. Each object represents one shard and specifies the IDs of the shard, the shard's parent, and the shard that's adjacent to the shard's parent. Each object also contains the starting and ending hash keys and the starting and ending sequence numbers for the shard.
mkListShardsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListShardsResponse
mkListShardsResponse pResponseStatus_ =
  ListShardsResponse'
    { nextToken = Lude.Nothing,
      shards = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | When the number of shards in the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of shards in the data stream, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListShards@ to list the next set of shards. For more information about the use of this pagination token when calling the @ListShards@ operation, see 'ListShardsInput$NextToken' .
--
-- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListShards@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListShards@ , you get @ExpiredNextTokenException@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListShardsResponse (Lude.Maybe Lude.Text)
lrsNextToken = Lens.lens (nextToken :: ListShardsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListShardsResponse)
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of JSON objects. Each object represents one shard and specifies the IDs of the shard, the shard's parent, and the shard that's adjacent to the shard's parent. Each object also contains the starting and ending hash keys and the starting and ending sequence numbers for the shard.
--
-- /Note:/ Consider using 'shards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsShards :: Lens.Lens' ListShardsResponse (Lude.Maybe [Shard])
lrsShards = Lens.lens (shards :: ListShardsResponse -> Lude.Maybe [Shard]) (\s a -> s {shards = a} :: ListShardsResponse)
{-# DEPRECATED lrsShards "Use generic-lens or generic-optics with 'shards' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListShardsResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListShardsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListShardsResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
