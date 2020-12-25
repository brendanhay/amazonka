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
    lscMaxResults,
    lscNextToken,
    lscStreamCreationTimestamp,

    -- * Destructuring the response
    ListStreamConsumersResponse (..),
    mkListStreamConsumersResponse,

    -- ** Response lenses
    lscrrsConsumers,
    lscrrsNextToken,
    lscrrsResponseStatus,
  )
where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListStreamConsumers' smart constructor.
data ListStreamConsumers = ListStreamConsumers'
  { -- | The ARN of the Kinesis data stream for which you want to list the registered consumers. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    streamARN :: Types.StreamARN,
    -- | The maximum number of consumers that you want a single call of @ListStreamConsumers@ to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | When the number of consumers that are registered with the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of consumers that are registered with the data stream, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListStreamConsumers@ to list the next set of registered consumers.
    --
    -- Don't specify @StreamName@ or @StreamCreationTimestamp@ if you specify @NextToken@ because the latter unambiguously identifies the stream.
    -- You can optionally specify a value for the @MaxResults@ parameter when you specify @NextToken@ . If you specify a @MaxResults@ value that is less than the number of consumers that the operation returns if you don't specify @MaxResults@ , the response will contain a new @NextToken@ value. You can use the new @NextToken@ value in a subsequent call to the @ListStreamConsumers@ operation to list the next set of consumers.
    -- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListStreamConsumers@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListStreamConsumers@ , you get @ExpiredNextTokenException@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | Specify this input parameter to distinguish data streams that have the same name. For example, if you create a data stream and then delete it, and you later create another data stream with the same name, you can use this input parameter to specify which of the two streams you want to list the consumers for.
    --
    -- You can't specify this parameter if you specify the NextToken parameter.
    streamCreationTimestamp :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListStreamConsumers' value with any optional fields omitted.
mkListStreamConsumers ::
  -- | 'streamARN'
  Types.StreamARN ->
  ListStreamConsumers
mkListStreamConsumers streamARN =
  ListStreamConsumers'
    { streamARN,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      streamCreationTimestamp = Core.Nothing
    }

-- | The ARN of the Kinesis data stream for which you want to list the registered consumers. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscStreamARN :: Lens.Lens' ListStreamConsumers Types.StreamARN
lscStreamARN = Lens.field @"streamARN"
{-# DEPRECATED lscStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The maximum number of consumers that you want a single call of @ListStreamConsumers@ to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscMaxResults :: Lens.Lens' ListStreamConsumers (Core.Maybe Core.Natural)
lscMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lscMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | When the number of consumers that are registered with the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of consumers that are registered with the data stream, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListStreamConsumers@ to list the next set of registered consumers.
--
-- Don't specify @StreamName@ or @StreamCreationTimestamp@ if you specify @NextToken@ because the latter unambiguously identifies the stream.
-- You can optionally specify a value for the @MaxResults@ parameter when you specify @NextToken@ . If you specify a @MaxResults@ value that is less than the number of consumers that the operation returns if you don't specify @MaxResults@ , the response will contain a new @NextToken@ value. You can use the new @NextToken@ value in a subsequent call to the @ListStreamConsumers@ operation to list the next set of consumers.
-- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListStreamConsumers@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListStreamConsumers@ , you get @ExpiredNextTokenException@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscNextToken :: Lens.Lens' ListStreamConsumers (Core.Maybe Types.NextToken)
lscNextToken = Lens.field @"nextToken"
{-# DEPRECATED lscNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specify this input parameter to distinguish data streams that have the same name. For example, if you create a data stream and then delete it, and you later create another data stream with the same name, you can use this input parameter to specify which of the two streams you want to list the consumers for.
--
-- You can't specify this parameter if you specify the NextToken parameter.
--
-- /Note:/ Consider using 'streamCreationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscStreamCreationTimestamp :: Lens.Lens' ListStreamConsumers (Core.Maybe Core.NominalDiffTime)
lscStreamCreationTimestamp = Lens.field @"streamCreationTimestamp"
{-# DEPRECATED lscStreamCreationTimestamp "Use generic-lens or generic-optics with 'streamCreationTimestamp' instead." #-}

instance Core.FromJSON ListStreamConsumers where
  toJSON ListStreamConsumers {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StreamARN" Core..= streamARN),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("StreamCreationTimestamp" Core..=)
              Core.<$> streamCreationTimestamp
          ]
      )

instance Core.AWSRequest ListStreamConsumers where
  type Rs ListStreamConsumers = ListStreamConsumersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Kinesis_20131202.ListStreamConsumers")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStreamConsumersResponse'
            Core.<$> (x Core..:? "Consumers")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListStreamConsumers where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"consumers" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListStreamConsumersResponse' smart constructor.
data ListStreamConsumersResponse = ListStreamConsumersResponse'
  { -- | An array of JSON objects. Each object represents one registered consumer.
    consumers :: Core.Maybe [Types.Consumer],
    -- | When the number of consumers that are registered with the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of registered consumers, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListStreamConsumers@ to list the next set of registered consumers. For more information about the use of this pagination token when calling the @ListStreamConsumers@ operation, see 'ListStreamConsumersInput$NextToken' .
    --
    -- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListStreamConsumers@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListStreamConsumers@ , you get @ExpiredNextTokenException@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListStreamConsumersResponse' value with any optional fields omitted.
mkListStreamConsumersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListStreamConsumersResponse
mkListStreamConsumersResponse responseStatus =
  ListStreamConsumersResponse'
    { consumers = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of JSON objects. Each object represents one registered consumer.
--
-- /Note:/ Consider using 'consumers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrrsConsumers :: Lens.Lens' ListStreamConsumersResponse (Core.Maybe [Types.Consumer])
lscrrsConsumers = Lens.field @"consumers"
{-# DEPRECATED lscrrsConsumers "Use generic-lens or generic-optics with 'consumers' instead." #-}

-- | When the number of consumers that are registered with the data stream is greater than the default value for the @MaxResults@ parameter, or if you explicitly specify a value for @MaxResults@ that is less than the number of registered consumers, the response includes a pagination token named @NextToken@ . You can specify this @NextToken@ value in a subsequent call to @ListStreamConsumers@ to list the next set of registered consumers. For more information about the use of this pagination token when calling the @ListStreamConsumers@ operation, see 'ListStreamConsumersInput$NextToken' .
--
-- /Important:/ Tokens expire after 300 seconds. When you obtain a value for @NextToken@ in the response to a call to @ListStreamConsumers@ , you have 300 seconds to use that value. If you specify an expired token in a call to @ListStreamConsumers@ , you get @ExpiredNextTokenException@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrrsNextToken :: Lens.Lens' ListStreamConsumersResponse (Core.Maybe Types.NextToken)
lscrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lscrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrrsResponseStatus :: Lens.Lens' ListStreamConsumersResponse Core.Int
lscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
