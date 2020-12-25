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
    lsExclusiveStartStreamName,
    lsLimit,

    -- * Destructuring the response
    ListStreamsResponse (..),
    mkListStreamsResponse,

    -- ** Response lenses
    lrsStreamNames,
    lrsHasMoreStreams,
    lrsResponseStatus,
  )
where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for @ListStreams@ .
--
-- /See:/ 'mkListStreams' smart constructor.
data ListStreams = ListStreams'
  { -- | The name of the stream to start the list with.
    exclusiveStartStreamName :: Core.Maybe Types.ExclusiveStartStreamName,
    -- | The maximum number of streams to list.
    limit :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStreams' value with any optional fields omitted.
mkListStreams ::
  ListStreams
mkListStreams =
  ListStreams'
    { exclusiveStartStreamName = Core.Nothing,
      limit = Core.Nothing
    }

-- | The name of the stream to start the list with.
--
-- /Note:/ Consider using 'exclusiveStartStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsExclusiveStartStreamName :: Lens.Lens' ListStreams (Core.Maybe Types.ExclusiveStartStreamName)
lsExclusiveStartStreamName = Lens.field @"exclusiveStartStreamName"
{-# DEPRECATED lsExclusiveStartStreamName "Use generic-lens or generic-optics with 'exclusiveStartStreamName' instead." #-}

-- | The maximum number of streams to list.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLimit :: Lens.Lens' ListStreams (Core.Maybe Core.Natural)
lsLimit = Lens.field @"limit"
{-# DEPRECATED lsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Core.FromJSON ListStreams where
  toJSON ListStreams {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExclusiveStartStreamName" Core..=)
              Core.<$> exclusiveStartStreamName,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.AWSRequest ListStreams where
  type Rs ListStreams = ListStreamsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Kinesis_20131202.ListStreams")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStreamsResponse'
            Core.<$> (x Core..:? "StreamNames" Core..!= Core.mempty)
            Core.<*> (x Core..: "HasMoreStreams")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListStreams where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"hasMoreStreams") =
      Core.Nothing
    | Core.isNothing
        (rs Lens.^? Lens.field @"streamNames" Core.. Lens._last) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"exclusiveStartStreamName"
            Lens..~ rs Lens.^? Lens.field @"streamNames" Core.. Lens._last
        )

-- | Represents the output for @ListStreams@ .
--
-- /See:/ 'mkListStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { -- | The names of the streams that are associated with the AWS account making the @ListStreams@ request.
    streamNames :: [Types.StreamName],
    -- | If set to @true@ , there are more streams available to list.
    hasMoreStreams :: Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStreamsResponse' value with any optional fields omitted.
mkListStreamsResponse ::
  -- | 'hasMoreStreams'
  Core.Bool ->
  -- | 'responseStatus'
  Core.Int ->
  ListStreamsResponse
mkListStreamsResponse hasMoreStreams responseStatus =
  ListStreamsResponse'
    { streamNames = Core.mempty,
      hasMoreStreams,
      responseStatus
    }

-- | The names of the streams that are associated with the AWS account making the @ListStreams@ request.
--
-- /Note:/ Consider using 'streamNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsStreamNames :: Lens.Lens' ListStreamsResponse [Types.StreamName]
lrsStreamNames = Lens.field @"streamNames"
{-# DEPRECATED lrsStreamNames "Use generic-lens or generic-optics with 'streamNames' instead." #-}

-- | If set to @true@ , there are more streams available to list.
--
-- /Note:/ Consider using 'hasMoreStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsHasMoreStreams :: Lens.Lens' ListStreamsResponse Core.Bool
lrsHasMoreStreams = Lens.field @"hasMoreStreams"
{-# DEPRECATED lrsHasMoreStreams "Use generic-lens or generic-optics with 'hasMoreStreams' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListStreamsResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
