{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.ListStreams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your Kinesis data streams.
--
-- The number of streams may be too large to return from a single call to @ListStreams@ . You can limit the number of returned streams using the @Limit@ parameter. If you do not specify a value for the @Limit@ parameter, Kinesis Data Streams uses the default limit, which is currently 10.
-- You can detect if there are more streams available to list by using the @HasMoreStreams@ flag from the returned output. If there are more streams available, you can request more streams by using the name of the last stream returned by the @ListStreams@ request in the @ExclusiveStartStreamName@ parameter in a subsequent request to @ListStreams@ . The group of stream names returned by the subsequent request is then added to the list. You can continue this process until all the stream names have been collected in the list.
-- 'ListStreams' has a limit of five transactions per second per account.
--
-- This operation returns paginated results.
module Network.AWS.Kinesis.ListStreams
  ( -- * Creating a request
    ListStreams (..),
    mkListStreams,

    -- ** Request lenses
    lsLimit,
    lsExclusiveStartStreamName,

    -- * Destructuring the response
    ListStreamsResponse (..),
    mkListStreamsResponse,

    -- ** Response lenses
    lsrsHasMoreStreams,
    lsrsStreamNames,
    lsrsResponseStatus,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for @ListStreams@ .
--
-- /See:/ 'mkListStreams' smart constructor.
data ListStreams = ListStreams'
  { -- | The maximum number of streams to list.
    limit :: Lude.Maybe Lude.Natural,
    -- | The name of the stream to start the list with.
    exclusiveStartStreamName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStreams' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of streams to list.
-- * 'exclusiveStartStreamName' - The name of the stream to start the list with.
mkListStreams ::
  ListStreams
mkListStreams =
  ListStreams'
    { limit = Lude.Nothing,
      exclusiveStartStreamName = Lude.Nothing
    }

-- | The maximum number of streams to list.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLimit :: Lens.Lens' ListStreams (Lude.Maybe Lude.Natural)
lsLimit = Lens.lens (limit :: ListStreams -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListStreams)
{-# DEPRECATED lsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The name of the stream to start the list with.
--
-- /Note:/ Consider using 'exclusiveStartStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsExclusiveStartStreamName :: Lens.Lens' ListStreams (Lude.Maybe Lude.Text)
lsExclusiveStartStreamName = Lens.lens (exclusiveStartStreamName :: ListStreams -> Lude.Maybe Lude.Text) (\s a -> s {exclusiveStartStreamName = a} :: ListStreams)
{-# DEPRECATED lsExclusiveStartStreamName "Use generic-lens or generic-optics with 'exclusiveStartStreamName' instead." #-}

instance Page.AWSPager ListStreams where
  page rq rs
    | Page.stop (rs Lens.^. lsrsHasMoreStreams) = Lude.Nothing
    | Lude.isNothing (rs Lens.^? lsrsStreamNames Lude.. Lens._last) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsExclusiveStartStreamName
          Lens..~ rs Lens.^? lsrsStreamNames Lude.. Lens._last

instance Lude.AWSRequest ListStreams where
  type Rs ListStreams = ListStreamsResponse
  request = Req.postJSON kinesisService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListStreamsResponse'
            Lude.<$> (x Lude..:> "HasMoreStreams")
            Lude.<*> (x Lude..?> "StreamNames" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListStreams where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.ListStreams" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListStreams where
  toJSON ListStreams' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Limit" Lude..=) Lude.<$> limit,
            ("ExclusiveStartStreamName" Lude..=)
              Lude.<$> exclusiveStartStreamName
          ]
      )

instance Lude.ToPath ListStreams where
  toPath = Lude.const "/"

instance Lude.ToQuery ListStreams where
  toQuery = Lude.const Lude.mempty

-- | Represents the output for @ListStreams@ .
--
-- /See:/ 'mkListStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { -- | If set to @true@ , there are more streams available to list.
    hasMoreStreams :: Lude.Bool,
    -- | The names of the streams that are associated with the AWS account making the @ListStreams@ request.
    streamNames :: [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStreamsResponse' with the minimum fields required to make a request.
--
-- * 'hasMoreStreams' - If set to @true@ , there are more streams available to list.
-- * 'streamNames' - The names of the streams that are associated with the AWS account making the @ListStreams@ request.
-- * 'responseStatus' - The response status code.
mkListStreamsResponse ::
  -- | 'hasMoreStreams'
  Lude.Bool ->
  -- | 'responseStatus'
  Lude.Int ->
  ListStreamsResponse
mkListStreamsResponse pHasMoreStreams_ pResponseStatus_ =
  ListStreamsResponse'
    { hasMoreStreams = pHasMoreStreams_,
      streamNames = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | If set to @true@ , there are more streams available to list.
--
-- /Note:/ Consider using 'hasMoreStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsHasMoreStreams :: Lens.Lens' ListStreamsResponse Lude.Bool
lsrsHasMoreStreams = Lens.lens (hasMoreStreams :: ListStreamsResponse -> Lude.Bool) (\s a -> s {hasMoreStreams = a} :: ListStreamsResponse)
{-# DEPRECATED lsrsHasMoreStreams "Use generic-lens or generic-optics with 'hasMoreStreams' instead." #-}

-- | The names of the streams that are associated with the AWS account making the @ListStreams@ request.
--
-- /Note:/ Consider using 'streamNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsStreamNames :: Lens.Lens' ListStreamsResponse [Lude.Text]
lsrsStreamNames = Lens.lens (streamNames :: ListStreamsResponse -> [Lude.Text]) (\s a -> s {streamNames = a} :: ListStreamsResponse)
{-# DEPRECATED lsrsStreamNames "Use generic-lens or generic-optics with 'streamNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsResponseStatus :: Lens.Lens' ListStreamsResponse Lude.Int
lsrsResponseStatus = Lens.lens (responseStatus :: ListStreamsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListStreamsResponse)
{-# DEPRECATED lsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
