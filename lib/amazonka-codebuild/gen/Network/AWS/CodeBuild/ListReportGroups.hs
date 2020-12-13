{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListReportGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list ARNs for the report groups in the current AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListReportGroups
  ( -- * Creating a request
    ListReportGroups (..),
    mkListReportGroups,

    -- ** Request lenses
    lrgSortOrder,
    lrgNextToken,
    lrgMaxResults,
    lrgSortBy,

    -- * Destructuring the response
    ListReportGroupsResponse (..),
    mkListReportGroupsResponse,

    -- ** Response lenses
    lrgrsNextToken,
    lrgrsReportGroups,
    lrgrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListReportGroups' smart constructor.
data ListReportGroups = ListReportGroups'
  { -- | Used to specify the order to sort the list of returned report groups. Valid values are @ASCENDING@ and @DESCENDING@ .
    sortOrder :: Lude.Maybe SortOrderType,
    -- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of paginated report groups returned per response. Use @nextToken@ to iterate pages in the list of returned @ReportGroup@ objects. The default value is 100.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The criterion to be used to list build report groups. Valid values include:
    --
    --
    --     * @CREATED_TIME@ : List based on when each report group was created.
    --
    --
    --     * @LAST_MODIFIED_TIME@ : List based on when each report group was last changed.
    --
    --
    --     * @NAME@ : List based on each report group's name.
    sortBy :: Lude.Maybe ReportGroupSortByType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReportGroups' with the minimum fields required to make a request.
--
-- * 'sortOrder' - Used to specify the order to sort the list of returned report groups. Valid values are @ASCENDING@ and @DESCENDING@ .
-- * 'nextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
-- * 'maxResults' - The maximum number of paginated report groups returned per response. Use @nextToken@ to iterate pages in the list of returned @ReportGroup@ objects. The default value is 100.
-- * 'sortBy' - The criterion to be used to list build report groups. Valid values include:
--
--
--     * @CREATED_TIME@ : List based on when each report group was created.
--
--
--     * @LAST_MODIFIED_TIME@ : List based on when each report group was last changed.
--
--
--     * @NAME@ : List based on each report group's name.
mkListReportGroups ::
  ListReportGroups
mkListReportGroups =
  ListReportGroups'
    { sortOrder = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | Used to specify the order to sort the list of returned report groups. Valid values are @ASCENDING@ and @DESCENDING@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgSortOrder :: Lens.Lens' ListReportGroups (Lude.Maybe SortOrderType)
lrgSortOrder = Lens.lens (sortOrder :: ListReportGroups -> Lude.Maybe SortOrderType) (\s a -> s {sortOrder = a} :: ListReportGroups)
{-# DEPRECATED lrgSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgNextToken :: Lens.Lens' ListReportGroups (Lude.Maybe Lude.Text)
lrgNextToken = Lens.lens (nextToken :: ListReportGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListReportGroups)
{-# DEPRECATED lrgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of paginated report groups returned per response. Use @nextToken@ to iterate pages in the list of returned @ReportGroup@ objects. The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgMaxResults :: Lens.Lens' ListReportGroups (Lude.Maybe Lude.Natural)
lrgMaxResults = Lens.lens (maxResults :: ListReportGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListReportGroups)
{-# DEPRECATED lrgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The criterion to be used to list build report groups. Valid values include:
--
--
--     * @CREATED_TIME@ : List based on when each report group was created.
--
--
--     * @LAST_MODIFIED_TIME@ : List based on when each report group was last changed.
--
--
--     * @NAME@ : List based on each report group's name.
--
--
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgSortBy :: Lens.Lens' ListReportGroups (Lude.Maybe ReportGroupSortByType)
lrgSortBy = Lens.lens (sortBy :: ListReportGroups -> Lude.Maybe ReportGroupSortByType) (\s a -> s {sortBy = a} :: ListReportGroups)
{-# DEPRECATED lrgSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListReportGroups where
  page rq rs
    | Page.stop (rs Lens.^. lrgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrgrsReportGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrgNextToken Lens..~ rs Lens.^. lrgrsNextToken

instance Lude.AWSRequest ListReportGroups where
  type Rs ListReportGroups = ListReportGroupsResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListReportGroupsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "reportGroups")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListReportGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.ListReportGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListReportGroups where
  toJSON ListReportGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sortOrder" Lude..=) Lude.<$> sortOrder,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            ("sortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListReportGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery ListReportGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListReportGroupsResponse' smart constructor.
data ListReportGroupsResponse = ListReportGroupsResponse'
  { -- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The list of ARNs for the report groups in the current AWS account.
    reportGroups :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReportGroupsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
-- * 'reportGroups' - The list of ARNs for the report groups in the current AWS account.
-- * 'responseStatus' - The response status code.
mkListReportGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListReportGroupsResponse
mkListReportGroupsResponse pResponseStatus_ =
  ListReportGroupsResponse'
    { nextToken = Lude.Nothing,
      reportGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgrsNextToken :: Lens.Lens' ListReportGroupsResponse (Lude.Maybe Lude.Text)
lrgrsNextToken = Lens.lens (nextToken :: ListReportGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListReportGroupsResponse)
{-# DEPRECATED lrgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of ARNs for the report groups in the current AWS account.
--
-- /Note:/ Consider using 'reportGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgrsReportGroups :: Lens.Lens' ListReportGroupsResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
lrgrsReportGroups = Lens.lens (reportGroups :: ListReportGroupsResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {reportGroups = a} :: ListReportGroupsResponse)
{-# DEPRECATED lrgrsReportGroups "Use generic-lens or generic-optics with 'reportGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgrsResponseStatus :: Lens.Lens' ListReportGroupsResponse Lude.Int
lrgrsResponseStatus = Lens.lens (responseStatus :: ListReportGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListReportGroupsResponse)
{-# DEPRECATED lrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
