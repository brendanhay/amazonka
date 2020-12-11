{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListComplianceSummaries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a summary count of compliant and non-compliant resources for a compliance type. For example, this call can return State Manager associations, patches, or custom compliance types according to the filter criteria that you specify.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListComplianceSummaries
  ( -- * Creating a request
    ListComplianceSummaries (..),
    mkListComplianceSummaries,

    -- ** Request lenses
    lcsFilters,
    lcsNextToken,
    lcsMaxResults,

    -- * Destructuring the response
    ListComplianceSummariesResponse (..),
    mkListComplianceSummariesResponse,

    -- ** Response lenses
    lcsrsNextToken,
    lcsrsComplianceSummaryItems,
    lcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkListComplianceSummaries' smart constructor.
data ListComplianceSummaries = ListComplianceSummaries'
  { filters ::
      Lude.Maybe [ComplianceStringFilter],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListComplianceSummaries' with the minimum fields required to make a request.
--
-- * 'filters' - One or more compliance or inventory filters. Use a filter to return a more specific list of results.
-- * 'maxResults' - The maximum number of items to return for this call. Currently, you can specify null or 50. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - A token to start the list. Use this token to get the next set of results.
mkListComplianceSummaries ::
  ListComplianceSummaries
mkListComplianceSummaries =
  ListComplianceSummaries'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more compliance or inventory filters. Use a filter to return a more specific list of results.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsFilters :: Lens.Lens' ListComplianceSummaries (Lude.Maybe [ComplianceStringFilter])
lcsFilters = Lens.lens (filters :: ListComplianceSummaries -> Lude.Maybe [ComplianceStringFilter]) (\s a -> s {filters = a} :: ListComplianceSummaries)
{-# DEPRECATED lcsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsNextToken :: Lens.Lens' ListComplianceSummaries (Lude.Maybe Lude.Text)
lcsNextToken = Lens.lens (nextToken :: ListComplianceSummaries -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListComplianceSummaries)
{-# DEPRECATED lcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. Currently, you can specify null or 50. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsMaxResults :: Lens.Lens' ListComplianceSummaries (Lude.Maybe Lude.Natural)
lcsMaxResults = Lens.lens (maxResults :: ListComplianceSummaries -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListComplianceSummaries)
{-# DEPRECATED lcsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListComplianceSummaries where
  page rq rs
    | Page.stop (rs Lens.^. lcsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcsrsComplianceSummaryItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcsNextToken Lens..~ rs Lens.^. lcsrsNextToken

instance Lude.AWSRequest ListComplianceSummaries where
  type Rs ListComplianceSummaries = ListComplianceSummariesResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListComplianceSummariesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ComplianceSummaryItems" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListComplianceSummaries where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.ListComplianceSummaries" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListComplianceSummaries where
  toJSON ListComplianceSummaries' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListComplianceSummaries where
  toPath = Lude.const "/"

instance Lude.ToQuery ListComplianceSummaries where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListComplianceSummariesResponse' smart constructor.
data ListComplianceSummariesResponse = ListComplianceSummariesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    complianceSummaryItems ::
      Lude.Maybe
        [ComplianceSummaryItem],
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

-- | Creates a value of 'ListComplianceSummariesResponse' with the minimum fields required to make a request.
--
-- * 'complianceSummaryItems' - A list of compliant and non-compliant summary counts based on compliance types. For example, this call returns State Manager associations, patches, or custom compliance types according to the filter criteria that you specified.
-- * 'nextToken' - The token for the next set of items to return. Use this token to get the next set of results.
-- * 'responseStatus' - The response status code.
mkListComplianceSummariesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListComplianceSummariesResponse
mkListComplianceSummariesResponse pResponseStatus_ =
  ListComplianceSummariesResponse'
    { nextToken = Lude.Nothing,
      complianceSummaryItems = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrsNextToken :: Lens.Lens' ListComplianceSummariesResponse (Lude.Maybe Lude.Text)
lcsrsNextToken = Lens.lens (nextToken :: ListComplianceSummariesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListComplianceSummariesResponse)
{-# DEPRECATED lcsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of compliant and non-compliant summary counts based on compliance types. For example, this call returns State Manager associations, patches, or custom compliance types according to the filter criteria that you specified.
--
-- /Note:/ Consider using 'complianceSummaryItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrsComplianceSummaryItems :: Lens.Lens' ListComplianceSummariesResponse (Lude.Maybe [ComplianceSummaryItem])
lcsrsComplianceSummaryItems = Lens.lens (complianceSummaryItems :: ListComplianceSummariesResponse -> Lude.Maybe [ComplianceSummaryItem]) (\s a -> s {complianceSummaryItems = a} :: ListComplianceSummariesResponse)
{-# DEPRECATED lcsrsComplianceSummaryItems "Use generic-lens or generic-optics with 'complianceSummaryItems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrsResponseStatus :: Lens.Lens' ListComplianceSummariesResponse Lude.Int
lcsrsResponseStatus = Lens.lens (responseStatus :: ListComplianceSummariesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListComplianceSummariesResponse)
{-# DEPRECATED lcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
