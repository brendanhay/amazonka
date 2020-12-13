{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListEventSourceMappings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists event source mappings. Specify an @EventSourceArn@ to only show event source mappings for a single event source.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListEventSourceMappings
  ( -- * Creating a request
    ListEventSourceMappings (..),
    mkListEventSourceMappings,

    -- ** Request lenses
    lesmEventSourceARN,
    lesmMarker,
    lesmMaxItems,
    lesmFunctionName,

    -- * Destructuring the response
    ListEventSourceMappingsResponse (..),
    mkListEventSourceMappingsResponse,

    -- ** Response lenses
    lesmrsEventSourceMappings,
    lesmrsNextMarker,
    lesmrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListEventSourceMappings' smart constructor.
data ListEventSourceMappings = ListEventSourceMappings'
  { -- | The Amazon Resource Name (ARN) of the event source.
    --
    --
    --     * __Amazon Kinesis__ - The ARN of the data stream or a stream consumer.
    --
    --
    --     * __Amazon DynamoDB Streams__ - The ARN of the stream.
    --
    --
    --     * __Amazon Simple Queue Service__ - The ARN of the queue.
    --
    --
    --     * __Amazon Managed Streaming for Apache Kafka__ - The ARN of the cluster.
    eventSourceARN :: Lude.Maybe Lude.Text,
    -- | A pagination token returned by a previous call.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of event source mappings to return.
    maxItems :: Lude.Maybe Lude.Natural,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    --     * __Function name__ - @MyFunction@ .
    --
    --
    --     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
    --
    --
    --     * __Version or Alias ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@ .
    --
    --
    --     * __Partial ARN__ - @123456789012:function:MyFunction@ .
    --
    --
    -- The length constraint applies only to the full ARN. If you specify only the function name, it's limited to 64 characters in length.
    functionName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListEventSourceMappings' with the minimum fields required to make a request.
--
-- * 'eventSourceARN' - The Amazon Resource Name (ARN) of the event source.
--
--
--     * __Amazon Kinesis__ - The ARN of the data stream or a stream consumer.
--
--
--     * __Amazon DynamoDB Streams__ - The ARN of the stream.
--
--
--     * __Amazon Simple Queue Service__ - The ARN of the queue.
--
--
--     * __Amazon Managed Streaming for Apache Kafka__ - The ARN of the cluster.
--
--
-- * 'marker' - A pagination token returned by a previous call.
-- * 'maxItems' - The maximum number of event source mappings to return.
-- * 'functionName' - The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Version or Alias ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it's limited to 64 characters in length.
mkListEventSourceMappings ::
  ListEventSourceMappings
mkListEventSourceMappings =
  ListEventSourceMappings'
    { eventSourceARN = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      functionName = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the event source.
--
--
--     * __Amazon Kinesis__ - The ARN of the data stream or a stream consumer.
--
--
--     * __Amazon DynamoDB Streams__ - The ARN of the stream.
--
--
--     * __Amazon Simple Queue Service__ - The ARN of the queue.
--
--
--     * __Amazon Managed Streaming for Apache Kafka__ - The ARN of the cluster.
--
--
--
-- /Note:/ Consider using 'eventSourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesmEventSourceARN :: Lens.Lens' ListEventSourceMappings (Lude.Maybe Lude.Text)
lesmEventSourceARN = Lens.lens (eventSourceARN :: ListEventSourceMappings -> Lude.Maybe Lude.Text) (\s a -> s {eventSourceARN = a} :: ListEventSourceMappings)
{-# DEPRECATED lesmEventSourceARN "Use generic-lens or generic-optics with 'eventSourceARN' instead." #-}

-- | A pagination token returned by a previous call.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesmMarker :: Lens.Lens' ListEventSourceMappings (Lude.Maybe Lude.Text)
lesmMarker = Lens.lens (marker :: ListEventSourceMappings -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListEventSourceMappings)
{-# DEPRECATED lesmMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of event source mappings to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesmMaxItems :: Lens.Lens' ListEventSourceMappings (Lude.Maybe Lude.Natural)
lesmMaxItems = Lens.lens (maxItems :: ListEventSourceMappings -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListEventSourceMappings)
{-# DEPRECATED lesmMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Version or Alias ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it's limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesmFunctionName :: Lens.Lens' ListEventSourceMappings (Lude.Maybe Lude.Text)
lesmFunctionName = Lens.lens (functionName :: ListEventSourceMappings -> Lude.Maybe Lude.Text) (\s a -> s {functionName = a} :: ListEventSourceMappings)
{-# DEPRECATED lesmFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Page.AWSPager ListEventSourceMappings where
  page rq rs
    | Page.stop (rs Lens.^. lesmrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lesmrsEventSourceMappings) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lesmMarker Lens..~ rs Lens.^. lesmrsNextMarker

instance Lude.AWSRequest ListEventSourceMappings where
  type Rs ListEventSourceMappings = ListEventSourceMappingsResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListEventSourceMappingsResponse'
            Lude.<$> (x Lude..?> "EventSourceMappings" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListEventSourceMappings where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListEventSourceMappings where
  toPath = Lude.const "/2015-03-31/event-source-mappings/"

instance Lude.ToQuery ListEventSourceMappings where
  toQuery ListEventSourceMappings' {..} =
    Lude.mconcat
      [ "EventSourceArn" Lude.=: eventSourceARN,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems,
        "FunctionName" Lude.=: functionName
      ]

-- | /See:/ 'mkListEventSourceMappingsResponse' smart constructor.
data ListEventSourceMappingsResponse = ListEventSourceMappingsResponse'
  { -- | A list of event source mappings.
    eventSourceMappings :: Lude.Maybe [EventSourceMappingConfiguration],
    -- | A pagination token that's returned when the response doesn't contain all event source mappings.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListEventSourceMappingsResponse' with the minimum fields required to make a request.
--
-- * 'eventSourceMappings' - A list of event source mappings.
-- * 'nextMarker' - A pagination token that's returned when the response doesn't contain all event source mappings.
-- * 'responseStatus' - The response status code.
mkListEventSourceMappingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListEventSourceMappingsResponse
mkListEventSourceMappingsResponse pResponseStatus_ =
  ListEventSourceMappingsResponse'
    { eventSourceMappings =
        Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of event source mappings.
--
-- /Note:/ Consider using 'eventSourceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesmrsEventSourceMappings :: Lens.Lens' ListEventSourceMappingsResponse (Lude.Maybe [EventSourceMappingConfiguration])
lesmrsEventSourceMappings = Lens.lens (eventSourceMappings :: ListEventSourceMappingsResponse -> Lude.Maybe [EventSourceMappingConfiguration]) (\s a -> s {eventSourceMappings = a} :: ListEventSourceMappingsResponse)
{-# DEPRECATED lesmrsEventSourceMappings "Use generic-lens or generic-optics with 'eventSourceMappings' instead." #-}

-- | A pagination token that's returned when the response doesn't contain all event source mappings.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesmrsNextMarker :: Lens.Lens' ListEventSourceMappingsResponse (Lude.Maybe Lude.Text)
lesmrsNextMarker = Lens.lens (nextMarker :: ListEventSourceMappingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListEventSourceMappingsResponse)
{-# DEPRECATED lesmrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesmrsResponseStatus :: Lens.Lens' ListEventSourceMappingsResponse Lude.Int
lesmrsResponseStatus = Lens.lens (responseStatus :: ListEventSourceMappingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListEventSourceMappingsResponse)
{-# DEPRECATED lesmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
