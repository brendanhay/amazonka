{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.ListStreamConsumers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the consumers registered to receive data from a stream using enhanced fan-out, and provides information about each consumer.
--
-- This operation has a limit of 5 transactions per second per stream.
--
-- This operation returns paginated results.
module Network.AWS.Kinesis.ListStreamConsumers
  ( -- * Creating a request
    ListStreamConsumers (..),
    mkListStreamConsumers,

    -- ** Request lenses
    lscStreamARN,
    lscNextToken,
    lscStreamCreationTimestamp,
    lscMaxResults,

    -- * Destructuring the response
    ListStreamConsumersResponse (..),
    mkListStreamConsumersResponse,

    -- ** Response lenses
    lscrsNextToken,
    lscrsConsumers,
    lscrsResponseStatus,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListStreamConsumers' smart constructor.
data ListStreamConsumers = ListStreamConsumers'
  { -- | The ARN of the Kinesis data stream for which you want to list the registered consumers. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    streamARN :: Lude.Text,
    -- | When the number of consumers that are registered with the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of consumers that are registered with the data stream, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListStreamConsumers@ to list the next set of registered consumers.
    --
    -- Don't specify @StreamName@ or @StreamCreationTimestamp@ if you specify @NextToken@ because the latter unambiguously identifies the stream.
    -- You can optionally specify a value for the @MaxResults@ parameter when you specify @NextToken@ . If you specify a @MaxResults@ value that is less than the number of consumers that the operation returns if you don't specify @MaxResults@ , the response will contain a new @NextToken@ value. You can use the new @NextToken@ value in a subsequent call to the @ListStreamConsumers@ operation to list the next set of consumers.
    -- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListStreamConsumers@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListStreamConsumers@ , you get @ExpiredNextTokenException@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | Specify this input parameter to distinguish data streams that have the same name. For example, if you create a data stream and then delete it, and you later create another data stream with the same name, you can use this input parameter to specify which of the two streams you want to list the consumers for.
    --
    -- You can't specify this parameter if you specify the NextToken parameter.
    streamCreationTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | The maximum number of consumers that you want a single call of @ListStreamConsumers@ to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStreamConsumers' with the minimum fields required to make a request.
--
-- * 'streamARN' - The ARN of the Kinesis data stream for which you want to list the registered consumers. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 'nextToken' - When the number of consumers that are registered with the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of consumers that are registered with the data stream, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListStreamConsumers@ to list the next set of registered consumers.
--
-- Don't specify @StreamName@ or @StreamCreationTimestamp@ if you specify @NextToken@ because the latter unambiguously identifies the stream.
-- You can optionally specify a value for the @MaxResults@ parameter when you specify @NextToken@ . If you specify a @MaxResults@ value that is less than the number of consumers that the operation returns if you don't specify @MaxResults@ , the response will contain a new @NextToken@ value. You can use the new @NextToken@ value in a subsequent call to the @ListStreamConsumers@ operation to list the next set of consumers.
-- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListStreamConsumers@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListStreamConsumers@ , you get @ExpiredNextTokenException@ .
-- * 'streamCreationTimestamp' - Specify this input parameter to distinguish data streams that have the same name. For example, if you create a data stream and then delete it, and you later create another data stream with the same name, you can use this input parameter to specify which of the two streams you want to list the consumers for.
--
-- You can't specify this parameter if you specify the NextToken parameter.
-- * 'maxResults' - The maximum number of consumers that you want a single call of @ListStreamConsumers@ to return.
mkListStreamConsumers ::
  -- | 'streamARN'
  Lude.Text ->
  ListStreamConsumers
mkListStreamConsumers pStreamARN_ =
  ListStreamConsumers'
    { streamARN = pStreamARN_,
      nextToken = Lude.Nothing,
      streamCreationTimestamp = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ARN of the Kinesis data stream for which you want to list the registered consumers. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscStreamARN :: Lens.Lens' ListStreamConsumers Lude.Text
lscStreamARN = Lens.lens (streamARN :: ListStreamConsumers -> Lude.Text) (\s a -> s {streamARN = a} :: ListStreamConsumers)
{-# DEPRECATED lscStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | When the number of consumers that are registered with the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of consumers that are registered with the data stream, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListStreamConsumers@ to list the next set of registered consumers.
--
-- Don't specify @StreamName@ or @StreamCreationTimestamp@ if you specify @NextToken@ because the latter unambiguously identifies the stream.
-- You can optionally specify a value for the @MaxResults@ parameter when you specify @NextToken@ . If you specify a @MaxResults@ value that is less than the number of consumers that the operation returns if you don't specify @MaxResults@ , the response will contain a new @NextToken@ value. You can use the new @NextToken@ value in a subsequent call to the @ListStreamConsumers@ operation to list the next set of consumers.
-- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListStreamConsumers@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListStreamConsumers@ , you get @ExpiredNextTokenException@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscNextToken :: Lens.Lens' ListStreamConsumers (Lude.Maybe Lude.Text)
lscNextToken = Lens.lens (nextToken :: ListStreamConsumers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStreamConsumers)
{-# DEPRECATED lscNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specify this input parameter to distinguish data streams that have the same name. For example, if you create a data stream and then delete it, and you later create another data stream with the same name, you can use this input parameter to specify which of the two streams you want to list the consumers for.
--
-- You can't specify this parameter if you specify the NextToken parameter.
--
-- /Note:/ Consider using 'streamCreationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscStreamCreationTimestamp :: Lens.Lens' ListStreamConsumers (Lude.Maybe Lude.Timestamp)
lscStreamCreationTimestamp = Lens.lens (streamCreationTimestamp :: ListStreamConsumers -> Lude.Maybe Lude.Timestamp) (\s a -> s {streamCreationTimestamp = a} :: ListStreamConsumers)
{-# DEPRECATED lscStreamCreationTimestamp "Use generic-lens or generic-optics with 'streamCreationTimestamp' instead." #-}

-- | The maximum number of consumers that you want a single call of @ListStreamConsumers@ to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscMaxResults :: Lens.Lens' ListStreamConsumers (Lude.Maybe Lude.Natural)
lscMaxResults = Lens.lens (maxResults :: ListStreamConsumers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListStreamConsumers)
{-# DEPRECATED lscMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListStreamConsumers where
  page rq rs
    | Page.stop (rs Lens.^. lscrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lscrsConsumers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lscNextToken Lens..~ rs Lens.^. lscrsNextToken

instance Lude.AWSRequest ListStreamConsumers where
  type Rs ListStreamConsumers = ListStreamConsumersResponse
  request = Req.postJSON kinesisService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListStreamConsumersResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Consumers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListStreamConsumers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.ListStreamConsumers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListStreamConsumers where
  toJSON ListStreamConsumers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StreamARN" Lude..= streamARN),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("StreamCreationTimestamp" Lude..=)
              Lude.<$> streamCreationTimestamp,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListStreamConsumers where
  toPath = Lude.const "/"

instance Lude.ToQuery ListStreamConsumers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListStreamConsumersResponse' smart constructor.
data ListStreamConsumersResponse = ListStreamConsumersResponse'
  { -- | When the number of consumers that are registered with the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of registered consumers, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListStreamConsumers@ to list the next set of registered consumers. For more information about the use of this pagination token when calling the @ListStreamConsumers@ operation, see 'ListStreamConsumersInput$NextToken' .
    --
    -- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListStreamConsumers@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListStreamConsumers@ , you get @ExpiredNextTokenException@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | An array of JSON objects. Each object represents one registered consumer.
    consumers :: Lude.Maybe [Consumer],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStreamConsumersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - When the number of consumers that are registered with the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of registered consumers, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListStreamConsumers@ to list the next set of registered consumers. For more information about the use of this pagination token when calling the @ListStreamConsumers@ operation, see 'ListStreamConsumersInput$NextToken' .
--
-- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListStreamConsumers@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListStreamConsumers@ , you get @ExpiredNextTokenException@ .
-- * 'consumers' - An array of JSON objects. Each object represents one registered consumer.
-- * 'responseStatus' - The response status code.
mkListStreamConsumersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListStreamConsumersResponse
mkListStreamConsumersResponse pResponseStatus_ =
  ListStreamConsumersResponse'
    { nextToken = Lude.Nothing,
      consumers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | When the number of consumers that are registered with the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of registered consumers, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListStreamConsumers@ to list the next set of registered consumers. For more information about the use of this pagination token when calling the @ListStreamConsumers@ operation, see 'ListStreamConsumersInput$NextToken' .
--
-- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListStreamConsumers@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListStreamConsumers@ , you get @ExpiredNextTokenException@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrsNextToken :: Lens.Lens' ListStreamConsumersResponse (Lude.Maybe Lude.Text)
lscrsNextToken = Lens.lens (nextToken :: ListStreamConsumersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStreamConsumersResponse)
{-# DEPRECATED lscrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of JSON objects. Each object represents one registered consumer.
--
-- /Note:/ Consider using 'consumers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrsConsumers :: Lens.Lens' ListStreamConsumersResponse (Lude.Maybe [Consumer])
lscrsConsumers = Lens.lens (consumers :: ListStreamConsumersResponse -> Lude.Maybe [Consumer]) (\s a -> s {consumers = a} :: ListStreamConsumersResponse)
{-# DEPRECATED lscrsConsumers "Use generic-lens or generic-optics with 'consumers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrsResponseStatus :: Lens.Lens' ListStreamConsumersResponse Lude.Int
lscrsResponseStatus = Lens.lens (responseStatus :: ListStreamConsumersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListStreamConsumersResponse)
{-# DEPRECATED lscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
