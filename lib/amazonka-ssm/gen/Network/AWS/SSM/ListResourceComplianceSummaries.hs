{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListResourceComplianceSummaries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a resource-level summary count. The summary includes information about compliant and non-compliant statuses and detailed compliance-item severity counts, according to the filter criteria you specify.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListResourceComplianceSummaries
  ( -- * Creating a request
    ListResourceComplianceSummaries (..),
    mkListResourceComplianceSummaries,

    -- ** Request lenses
    lrcsFilters,
    lrcsNextToken,
    lrcsMaxResults,

    -- * Destructuring the response
    ListResourceComplianceSummariesResponse (..),
    mkListResourceComplianceSummariesResponse,

    -- ** Response lenses
    lrcsrsResourceComplianceSummaryItems,
    lrcsrsNextToken,
    lrcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkListResourceComplianceSummaries' smart constructor.
data ListResourceComplianceSummaries = ListResourceComplianceSummaries'
  { filters ::
      Lude.Maybe
        [ComplianceStringFilter],
    nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourceComplianceSummaries' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters. Use a filter to return a more specific list of results.
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - A token to start the list. Use this token to get the next set of results.
mkListResourceComplianceSummaries ::
  ListResourceComplianceSummaries
mkListResourceComplianceSummaries =
  ListResourceComplianceSummaries'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters. Use a filter to return a more specific list of results.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrcsFilters :: Lens.Lens' ListResourceComplianceSummaries (Lude.Maybe [ComplianceStringFilter])
lrcsFilters = Lens.lens (filters :: ListResourceComplianceSummaries -> Lude.Maybe [ComplianceStringFilter]) (\s a -> s {filters = a} :: ListResourceComplianceSummaries)
{-# DEPRECATED lrcsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrcsNextToken :: Lens.Lens' ListResourceComplianceSummaries (Lude.Maybe Lude.Text)
lrcsNextToken = Lens.lens (nextToken :: ListResourceComplianceSummaries -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResourceComplianceSummaries)
{-# DEPRECATED lrcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrcsMaxResults :: Lens.Lens' ListResourceComplianceSummaries (Lude.Maybe Lude.Natural)
lrcsMaxResults = Lens.lens (maxResults :: ListResourceComplianceSummaries -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListResourceComplianceSummaries)
{-# DEPRECATED lrcsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListResourceComplianceSummaries where
  page rq rs
    | Page.stop (rs Lens.^. lrcsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrcsrsResourceComplianceSummaryItems) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrcsNextToken Lens..~ rs Lens.^. lrcsrsNextToken

instance Lude.AWSRequest ListResourceComplianceSummaries where
  type
    Rs ListResourceComplianceSummaries =
      ListResourceComplianceSummariesResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListResourceComplianceSummariesResponse'
            Lude.<$> (x Lude..?> "ResourceComplianceSummaryItems" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListResourceComplianceSummaries where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.ListResourceComplianceSummaries" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListResourceComplianceSummaries where
  toJSON ListResourceComplianceSummaries' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListResourceComplianceSummaries where
  toPath = Lude.const "/"

instance Lude.ToQuery ListResourceComplianceSummaries where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListResourceComplianceSummariesResponse' smart constructor.
data ListResourceComplianceSummariesResponse = ListResourceComplianceSummariesResponse'
  { resourceComplianceSummaryItems ::
      Lude.Maybe
        [ResourceComplianceSummaryItem],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourceComplianceSummariesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of items to return. Use this token to get the next set of results.
-- * 'resourceComplianceSummaryItems' - A summary count for specified or targeted managed instances. Summary count includes information about compliant and non-compliant State Manager associations, patch status, or custom items according to the filter criteria that you specify.
-- * 'responseStatus' - The response status code.
mkListResourceComplianceSummariesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListResourceComplianceSummariesResponse
mkListResourceComplianceSummariesResponse pResponseStatus_ =
  ListResourceComplianceSummariesResponse'
    { resourceComplianceSummaryItems =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A summary count for specified or targeted managed instances. Summary count includes information about compliant and non-compliant State Manager associations, patch status, or custom items according to the filter criteria that you specify.
--
-- /Note:/ Consider using 'resourceComplianceSummaryItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrcsrsResourceComplianceSummaryItems :: Lens.Lens' ListResourceComplianceSummariesResponse (Lude.Maybe [ResourceComplianceSummaryItem])
lrcsrsResourceComplianceSummaryItems = Lens.lens (resourceComplianceSummaryItems :: ListResourceComplianceSummariesResponse -> Lude.Maybe [ResourceComplianceSummaryItem]) (\s a -> s {resourceComplianceSummaryItems = a} :: ListResourceComplianceSummariesResponse)
{-# DEPRECATED lrcsrsResourceComplianceSummaryItems "Use generic-lens or generic-optics with 'resourceComplianceSummaryItems' instead." #-}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrcsrsNextToken :: Lens.Lens' ListResourceComplianceSummariesResponse (Lude.Maybe Lude.Text)
lrcsrsNextToken = Lens.lens (nextToken :: ListResourceComplianceSummariesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResourceComplianceSummariesResponse)
{-# DEPRECATED lrcsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrcsrsResponseStatus :: Lens.Lens' ListResourceComplianceSummariesResponse Lude.Int
lrcsrsResponseStatus = Lens.lens (responseStatus :: ListResourceComplianceSummariesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListResourceComplianceSummariesResponse)
{-# DEPRECATED lrcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
