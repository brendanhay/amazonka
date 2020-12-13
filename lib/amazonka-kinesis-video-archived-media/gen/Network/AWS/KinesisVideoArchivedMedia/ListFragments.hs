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
    lfFragmentSelector,
    lfNextToken,
    lfStreamName,
    lfMaxResults,

    -- * Destructuring the response
    ListFragmentsResponse (..),
    mkListFragmentsResponse,

    -- ** Response lenses
    lfrsNextToken,
    lfrsFragments,
    lfrsResponseStatus,
  )
where

import Network.AWS.KinesisVideoArchivedMedia.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListFragments' smart constructor.
data ListFragments = ListFragments'
  { -- | Describes the timestamp range and timestamp origin for the range of fragments to return.
    fragmentSelector :: Lude.Maybe FragmentSelector,
    -- | A token to specify where to start paginating. This is the 'ListFragmentsOutput$NextToken' from a previously truncated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the stream from which to retrieve a fragment list.
    streamName :: Lude.Text,
    -- | The total number of fragments to return. If the total number of fragments available is more than the value specified in @max-results@ , then a 'ListFragmentsOutput$NextToken' is provided in the output that you can use to resume pagination.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFragments' with the minimum fields required to make a request.
--
-- * 'fragmentSelector' - Describes the timestamp range and timestamp origin for the range of fragments to return.
-- * 'nextToken' - A token to specify where to start paginating. This is the 'ListFragmentsOutput$NextToken' from a previously truncated response.
-- * 'streamName' - The name of the stream from which to retrieve a fragment list.
-- * 'maxResults' - The total number of fragments to return. If the total number of fragments available is more than the value specified in @max-results@ , then a 'ListFragmentsOutput$NextToken' is provided in the output that you can use to resume pagination.
mkListFragments ::
  -- | 'streamName'
  Lude.Text ->
  ListFragments
mkListFragments pStreamName_ =
  ListFragments'
    { fragmentSelector = Lude.Nothing,
      nextToken = Lude.Nothing,
      streamName = pStreamName_,
      maxResults = Lude.Nothing
    }

-- | Describes the timestamp range and timestamp origin for the range of fragments to return.
--
-- /Note:/ Consider using 'fragmentSelector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfFragmentSelector :: Lens.Lens' ListFragments (Lude.Maybe FragmentSelector)
lfFragmentSelector = Lens.lens (fragmentSelector :: ListFragments -> Lude.Maybe FragmentSelector) (\s a -> s {fragmentSelector = a} :: ListFragments)
{-# DEPRECATED lfFragmentSelector "Use generic-lens or generic-optics with 'fragmentSelector' instead." #-}

-- | A token to specify where to start paginating. This is the 'ListFragmentsOutput$NextToken' from a previously truncated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfNextToken :: Lens.Lens' ListFragments (Lude.Maybe Lude.Text)
lfNextToken = Lens.lens (nextToken :: ListFragments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFragments)
{-# DEPRECATED lfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the stream from which to retrieve a fragment list.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfStreamName :: Lens.Lens' ListFragments Lude.Text
lfStreamName = Lens.lens (streamName :: ListFragments -> Lude.Text) (\s a -> s {streamName = a} :: ListFragments)
{-# DEPRECATED lfStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The total number of fragments to return. If the total number of fragments available is more than the value specified in @max-results@ , then a 'ListFragmentsOutput$NextToken' is provided in the output that you can use to resume pagination.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMaxResults :: Lens.Lens' ListFragments (Lude.Maybe Lude.Natural)
lfMaxResults = Lens.lens (maxResults :: ListFragments -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListFragments)
{-# DEPRECATED lfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListFragments where
  page rq rs
    | Page.stop (rs Lens.^. lfrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lfrsFragments) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lfNextToken Lens..~ rs Lens.^. lfrsNextToken

instance Lude.AWSRequest ListFragments where
  type Rs ListFragments = ListFragmentsResponse
  request = Req.postJSON kinesisVideoArchivedMediaService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListFragmentsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Fragments" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListFragments where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ListFragments where
  toJSON ListFragments' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FragmentSelector" Lude..=) Lude.<$> fragmentSelector,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("StreamName" Lude..= streamName),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListFragments where
  toPath = Lude.const "/listFragments"

instance Lude.ToQuery ListFragments where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListFragmentsResponse' smart constructor.
data ListFragmentsResponse = ListFragmentsResponse'
  { -- | If the returned list is truncated, the operation returns this token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of archived 'Fragment' objects from the stream that meet the selector criteria. Results are in no specific order, even across pages.
    fragments :: Lude.Maybe [Fragment],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFragmentsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the returned list is truncated, the operation returns this token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'fragments' - A list of archived 'Fragment' objects from the stream that meet the selector criteria. Results are in no specific order, even across pages.
-- * 'responseStatus' - The response status code.
mkListFragmentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListFragmentsResponse
mkListFragmentsResponse pResponseStatus_ =
  ListFragmentsResponse'
    { nextToken = Lude.Nothing,
      fragments = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the returned list is truncated, the operation returns this token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsNextToken :: Lens.Lens' ListFragmentsResponse (Lude.Maybe Lude.Text)
lfrsNextToken = Lens.lens (nextToken :: ListFragmentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFragmentsResponse)
{-# DEPRECATED lfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of archived 'Fragment' objects from the stream that meet the selector criteria. Results are in no specific order, even across pages.
--
-- /Note:/ Consider using 'fragments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsFragments :: Lens.Lens' ListFragmentsResponse (Lude.Maybe [Fragment])
lfrsFragments = Lens.lens (fragments :: ListFragmentsResponse -> Lude.Maybe [Fragment]) (\s a -> s {fragments = a} :: ListFragmentsResponse)
{-# DEPRECATED lfrsFragments "Use generic-lens or generic-optics with 'fragments' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsResponseStatus :: Lens.Lens' ListFragmentsResponse Lude.Int
lfrsResponseStatus = Lens.lens (responseStatus :: ListFragmentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListFragmentsResponse)
{-# DEPRECATED lfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
