{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.GetRecords
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the stream records from a given shard.
--
-- Specify a shard iterator using the @ShardIterator@ parameter. The shard iterator specifies the position in the shard from which you want to start reading stream records sequentially. If there are no stream records available in the portion of the shard that the iterator points to, @GetRecords@ returns an empty list. Note that it might take multiple calls to get to a portion of the shard that contains stream records.
module Network.AWS.DynamoDBStreams.GetRecords
  ( -- * Creating a request
    GetRecords (..),
    mkGetRecords,

    -- ** Request lenses
    grShardIterator,
    grLimit,

    -- * Destructuring the response
    GetRecordsResponse (..),
    mkGetRecordsResponse,

    -- ** Response lenses
    grrsRecords,
    grrsNextShardIterator,
    grrsResponseStatus,
  )
where

import Network.AWS.DynamoDBStreams.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @GetRecords@ operation.
--
-- /See:/ 'mkGetRecords' smart constructor.
data GetRecords = GetRecords'
  { -- | A shard iterator that was retrieved from a previous GetShardIterator operation. This iterator can be used to access the stream records in this shard.
    shardIterator :: Lude.Text,
    -- | The maximum number of records to return from the shard. The upper limit is 1000.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRecords' with the minimum fields required to make a request.
--
-- * 'shardIterator' - A shard iterator that was retrieved from a previous GetShardIterator operation. This iterator can be used to access the stream records in this shard.
-- * 'limit' - The maximum number of records to return from the shard. The upper limit is 1000.
mkGetRecords ::
  -- | 'shardIterator'
  Lude.Text ->
  GetRecords
mkGetRecords pShardIterator_ =
  GetRecords'
    { shardIterator = pShardIterator_,
      limit = Lude.Nothing
    }

-- | A shard iterator that was retrieved from a previous GetShardIterator operation. This iterator can be used to access the stream records in this shard.
--
-- /Note:/ Consider using 'shardIterator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grShardIterator :: Lens.Lens' GetRecords Lude.Text
grShardIterator = Lens.lens (shardIterator :: GetRecords -> Lude.Text) (\s a -> s {shardIterator = a} :: GetRecords)
{-# DEPRECATED grShardIterator "Use generic-lens or generic-optics with 'shardIterator' instead." #-}

-- | The maximum number of records to return from the shard. The upper limit is 1000.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grLimit :: Lens.Lens' GetRecords (Lude.Maybe Lude.Natural)
grLimit = Lens.lens (limit :: GetRecords -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: GetRecords)
{-# DEPRECATED grLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest GetRecords where
  type Rs GetRecords = GetRecordsResponse
  request = Req.postJSON dynamoDBStreamsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRecordsResponse'
            Lude.<$> (x Lude..?> "Records" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextShardIterator")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRecords where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDBStreams_20120810.GetRecords" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRecords where
  toJSON GetRecords' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ShardIterator" Lude..= shardIterator),
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath GetRecords where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRecords where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetRecords@ operation.
--
-- /See:/ 'mkGetRecordsResponse' smart constructor.
data GetRecordsResponse = GetRecordsResponse'
  { -- | The stream records from the shard, which were retrieved using the shard iterator.
    records :: Lude.Maybe [Record],
    -- | The next position in the shard from which to start sequentially reading stream records. If set to @null@ , the shard has been closed and the requested iterator will not return any more data.
    nextShardIterator :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRecordsResponse' with the minimum fields required to make a request.
--
-- * 'records' - The stream records from the shard, which were retrieved using the shard iterator.
-- * 'nextShardIterator' - The next position in the shard from which to start sequentially reading stream records. If set to @null@ , the shard has been closed and the requested iterator will not return any more data.
-- * 'responseStatus' - The response status code.
mkGetRecordsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRecordsResponse
mkGetRecordsResponse pResponseStatus_ =
  GetRecordsResponse'
    { records = Lude.Nothing,
      nextShardIterator = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The stream records from the shard, which were retrieved using the shard iterator.
--
-- /Note:/ Consider using 'records' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsRecords :: Lens.Lens' GetRecordsResponse (Lude.Maybe [Record])
grrsRecords = Lens.lens (records :: GetRecordsResponse -> Lude.Maybe [Record]) (\s a -> s {records = a} :: GetRecordsResponse)
{-# DEPRECATED grrsRecords "Use generic-lens or generic-optics with 'records' instead." #-}

-- | The next position in the shard from which to start sequentially reading stream records. If set to @null@ , the shard has been closed and the requested iterator will not return any more data.
--
-- /Note:/ Consider using 'nextShardIterator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsNextShardIterator :: Lens.Lens' GetRecordsResponse (Lude.Maybe Lude.Text)
grrsNextShardIterator = Lens.lens (nextShardIterator :: GetRecordsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextShardIterator = a} :: GetRecordsResponse)
{-# DEPRECATED grrsNextShardIterator "Use generic-lens or generic-optics with 'nextShardIterator' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsResponseStatus :: Lens.Lens' GetRecordsResponse Lude.Int
grrsResponseStatus = Lens.lens (responseStatus :: GetRecordsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRecordsResponse)
{-# DEPRECATED grrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
