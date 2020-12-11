{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.ListStreams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of stream ARNs associated with the current account and endpoint. If the @TableName@ parameter is present, then @ListStreams@ will return only the streams ARNs for that table.
module Network.AWS.DynamoDBStreams.ListStreams
  ( -- * Creating a request
    ListStreams (..),
    mkListStreams,

    -- ** Request lenses
    lsExclusiveStartStreamARN,
    lsLimit,
    lsTableName,

    -- * Destructuring the response
    ListStreamsResponse (..),
    mkListStreamsResponse,

    -- ** Response lenses
    lsrsLastEvaluatedStreamARN,
    lsrsStreams,
    lsrsResponseStatus,
  )
where

import Network.AWS.DynamoDBStreams.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @ListStreams@ operation.
--
-- /See:/ 'mkListStreams' smart constructor.
data ListStreams = ListStreams'
  { exclusiveStartStreamARN ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    tableName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStreams' with the minimum fields required to make a request.
--
-- * 'exclusiveStartStreamARN' - The ARN (Amazon Resource Name) of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedStreamArn@ in the previous operation.
-- * 'limit' - The maximum number of streams to return. The upper limit is 100.
-- * 'tableName' - If this parameter is provided, then only the streams associated with this table name are returned.
mkListStreams ::
  ListStreams
mkListStreams =
  ListStreams'
    { exclusiveStartStreamARN = Lude.Nothing,
      limit = Lude.Nothing,
      tableName = Lude.Nothing
    }

-- | The ARN (Amazon Resource Name) of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedStreamArn@ in the previous operation.
--
-- /Note:/ Consider using 'exclusiveStartStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsExclusiveStartStreamARN :: Lens.Lens' ListStreams (Lude.Maybe Lude.Text)
lsExclusiveStartStreamARN = Lens.lens (exclusiveStartStreamARN :: ListStreams -> Lude.Maybe Lude.Text) (\s a -> s {exclusiveStartStreamARN = a} :: ListStreams)
{-# DEPRECATED lsExclusiveStartStreamARN "Use generic-lens or generic-optics with 'exclusiveStartStreamARN' instead." #-}

-- | The maximum number of streams to return. The upper limit is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLimit :: Lens.Lens' ListStreams (Lude.Maybe Lude.Natural)
lsLimit = Lens.lens (limit :: ListStreams -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListStreams)
{-# DEPRECATED lsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | If this parameter is provided, then only the streams associated with this table name are returned.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsTableName :: Lens.Lens' ListStreams (Lude.Maybe Lude.Text)
lsTableName = Lens.lens (tableName :: ListStreams -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: ListStreams)
{-# DEPRECATED lsTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest ListStreams where
  type Rs ListStreams = ListStreamsResponse
  request = Req.postJSON dynamoDBStreamsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListStreamsResponse'
            Lude.<$> (x Lude..?> "LastEvaluatedStreamArn")
            Lude.<*> (x Lude..?> "Streams" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListStreams where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDBStreams_20120810.ListStreams" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListStreams where
  toJSON ListStreams' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExclusiveStartStreamArn" Lude..=)
              Lude.<$> exclusiveStartStreamARN,
            ("Limit" Lude..=) Lude.<$> limit,
            ("TableName" Lude..=) Lude.<$> tableName
          ]
      )

instance Lude.ToPath ListStreams where
  toPath = Lude.const "/"

instance Lude.ToQuery ListStreams where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @ListStreams@ operation.
--
-- /See:/ 'mkListStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { lastEvaluatedStreamARN ::
      Lude.Maybe Lude.Text,
    streams :: Lude.Maybe [Stream],
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

-- | Creates a value of 'ListStreamsResponse' with the minimum fields required to make a request.
--
-- * 'lastEvaluatedStreamARN' - The stream ARN of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request.
--
-- If @LastEvaluatedStreamArn@ is empty, then the "last page" of results has been processed and there is no more data to be retrieved.
-- If @LastEvaluatedStreamArn@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedStreamArn@ is empty.
-- * 'responseStatus' - The response status code.
-- * 'streams' - A list of stream descriptors associated with the current account and endpoint.
mkListStreamsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListStreamsResponse
mkListStreamsResponse pResponseStatus_ =
  ListStreamsResponse'
    { lastEvaluatedStreamARN = Lude.Nothing,
      streams = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The stream ARN of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request.
--
-- If @LastEvaluatedStreamArn@ is empty, then the "last page" of results has been processed and there is no more data to be retrieved.
-- If @LastEvaluatedStreamArn@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedStreamArn@ is empty.
--
-- /Note:/ Consider using 'lastEvaluatedStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsLastEvaluatedStreamARN :: Lens.Lens' ListStreamsResponse (Lude.Maybe Lude.Text)
lsrsLastEvaluatedStreamARN = Lens.lens (lastEvaluatedStreamARN :: ListStreamsResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastEvaluatedStreamARN = a} :: ListStreamsResponse)
{-# DEPRECATED lsrsLastEvaluatedStreamARN "Use generic-lens or generic-optics with 'lastEvaluatedStreamARN' instead." #-}

-- | A list of stream descriptors associated with the current account and endpoint.
--
-- /Note:/ Consider using 'streams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsStreams :: Lens.Lens' ListStreamsResponse (Lude.Maybe [Stream])
lsrsStreams = Lens.lens (streams :: ListStreamsResponse -> Lude.Maybe [Stream]) (\s a -> s {streams = a} :: ListStreamsResponse)
{-# DEPRECATED lsrsStreams "Use generic-lens or generic-optics with 'streams' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsResponseStatus :: Lens.Lens' ListStreamsResponse Lude.Int
lsrsResponseStatus = Lens.lens (responseStatus :: ListStreamsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListStreamsResponse)
{-# DEPRECATED lsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
