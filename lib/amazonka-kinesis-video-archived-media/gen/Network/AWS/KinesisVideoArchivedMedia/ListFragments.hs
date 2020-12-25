{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.ListFragments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of 'Fragment' objects from the specified stream and timestamp range within the archived data.
--
-- Listing fragments is eventually consistent. This means that even if the producer receives an acknowledgment that a fragment is persisted, the result might not be returned immediately from a request to @ListFragments@ . However, results are typically available in less than one second.
-- /Important:/ If an error is thrown after invoking a Kinesis Video Streams archived media API, in addition to the HTTP status code and the response body, it includes the following pieces of information:
--
--     * @x-amz-ErrorType@ HTTP header – contains a more specific error type in addition to what the HTTP status code provides.
--
--
--     * @x-amz-RequestId@ HTTP header – if you want to report an issue to AWS, the support team can better diagnose the problem if given the Request Id.
--
--
-- Both the HTTP status code and the ErrorType header can be utilized to make programmatic decisions about whether errors are retry-able and under what conditions, as well as provide information on what actions the client programmer might need to take in order to successfully try again.
-- For more information, see the __Errors__ section at the bottom of this topic, as well as <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/CommonErrors.html Common Errors> .
--
-- This operation returns paginated results.
module Network.AWS.KinesisVideoArchivedMedia.ListFragments
  ( -- * Creating a request
    ListFragments (..),
    mkListFragments,

    -- ** Request lenses
    lfStreamName,
    lfFragmentSelector,
    lfMaxResults,
    lfNextToken,

    -- * Destructuring the response
    ListFragmentsResponse (..),
    mkListFragmentsResponse,

    -- ** Response lenses
    lfrrsFragments,
    lfrrsNextToken,
    lfrrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisVideoArchivedMedia.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListFragments' smart constructor.
data ListFragments = ListFragments'
  { -- | The name of the stream from which to retrieve a fragment list.
    streamName :: Types.StreamName,
    -- | Describes the timestamp range and timestamp origin for the range of fragments to return.
    fragmentSelector :: Core.Maybe Types.FragmentSelector,
    -- | The total number of fragments to return. If the total number of fragments available is more than the value specified in @max-results@ , then a 'ListFragmentsOutput$NextToken' is provided in the output that you can use to resume pagination.
    maxResults :: Core.Maybe Core.Natural,
    -- | A token to specify where to start paginating. This is the 'ListFragmentsOutput$NextToken' from a previously truncated response.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListFragments' value with any optional fields omitted.
mkListFragments ::
  -- | 'streamName'
  Types.StreamName ->
  ListFragments
mkListFragments streamName =
  ListFragments'
    { streamName,
      fragmentSelector = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the stream from which to retrieve a fragment list.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfStreamName :: Lens.Lens' ListFragments Types.StreamName
lfStreamName = Lens.field @"streamName"
{-# DEPRECATED lfStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | Describes the timestamp range and timestamp origin for the range of fragments to return.
--
-- /Note:/ Consider using 'fragmentSelector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfFragmentSelector :: Lens.Lens' ListFragments (Core.Maybe Types.FragmentSelector)
lfFragmentSelector = Lens.field @"fragmentSelector"
{-# DEPRECATED lfFragmentSelector "Use generic-lens or generic-optics with 'fragmentSelector' instead." #-}

-- | The total number of fragments to return. If the total number of fragments available is more than the value specified in @max-results@ , then a 'ListFragmentsOutput$NextToken' is provided in the output that you can use to resume pagination.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMaxResults :: Lens.Lens' ListFragments (Core.Maybe Core.Natural)
lfMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A token to specify where to start paginating. This is the 'ListFragmentsOutput$NextToken' from a previously truncated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfNextToken :: Lens.Lens' ListFragments (Core.Maybe Types.NextToken)
lfNextToken = Lens.field @"nextToken"
{-# DEPRECATED lfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListFragments where
  toJSON ListFragments {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StreamName" Core..= streamName),
            ("FragmentSelector" Core..=) Core.<$> fragmentSelector,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListFragments where
  type Rs ListFragments = ListFragmentsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/listFragments",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFragmentsResponse'
            Core.<$> (x Core..:? "Fragments")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListFragments where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"fragments" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListFragmentsResponse' smart constructor.
data ListFragmentsResponse = ListFragmentsResponse'
  { -- | A list of archived 'Fragment' objects from the stream that meet the selector criteria. Results are in no specific order, even across pages.
    fragments :: Core.Maybe [Types.Fragment],
    -- | If the returned list is truncated, the operation returns this token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListFragmentsResponse' value with any optional fields omitted.
mkListFragmentsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListFragmentsResponse
mkListFragmentsResponse responseStatus =
  ListFragmentsResponse'
    { fragments = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of archived 'Fragment' objects from the stream that meet the selector criteria. Results are in no specific order, even across pages.
--
-- /Note:/ Consider using 'fragments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsFragments :: Lens.Lens' ListFragmentsResponse (Core.Maybe [Types.Fragment])
lfrrsFragments = Lens.field @"fragments"
{-# DEPRECATED lfrrsFragments "Use generic-lens or generic-optics with 'fragments' instead." #-}

-- | If the returned list is truncated, the operation returns this token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsNextToken :: Lens.Lens' ListFragmentsResponse (Core.Maybe Types.NextToken)
lfrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lfrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsResponseStatus :: Lens.Lens' ListFragmentsResponse Core.Int
lfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
