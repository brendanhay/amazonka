{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListSharedReportGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of report groups that are shared with other AWS accounts or users.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListSharedReportGroups
  ( -- * Creating a request
    ListSharedReportGroups (..),
    mkListSharedReportGroups,

    -- ** Request lenses
    lsrgSortOrder,
    lsrgNextToken,
    lsrgMaxResults,
    lsrgSortBy,

    -- * Destructuring the response
    ListSharedReportGroupsResponse (..),
    mkListSharedReportGroupsResponse,

    -- ** Response lenses
    lsrgrsNextToken,
    lsrgrsReportGroups,
    lsrgrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSharedReportGroups' smart constructor.
data ListSharedReportGroups = ListSharedReportGroups'
  { -- | The order in which to list shared report groups. Valid values include:
    --
    --
    --     * @ASCENDING@ : List in ascending order.
    --
    --
    --     * @DESCENDING@ : List in descending order.
    sortOrder :: Lude.Maybe SortOrderType,
    -- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of paginated shared report groups per response. Use @nextToken@ to iterate pages in the list of returned @ReportGroup@ objects. The default value is 100.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The criterion to be used to list report groups shared with the current AWS account or user. Valid values include:
    --
    --
    --     * @ARN@ : List based on the ARN.
    --
    --
    --     * @MODIFIED_TIME@ : List based on when information about the shared report group was last changed.
    sortBy :: Lude.Maybe SharedResourceSortByType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSharedReportGroups' with the minimum fields required to make a request.
--
-- * 'sortOrder' - The order in which to list shared report groups. Valid values include:
--
--
--     * @ASCENDING@ : List in ascending order.
--
--
--     * @DESCENDING@ : List in descending order.
--
--
-- * 'nextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
-- * 'maxResults' - The maximum number of paginated shared report groups per response. Use @nextToken@ to iterate pages in the list of returned @ReportGroup@ objects. The default value is 100.
-- * 'sortBy' - The criterion to be used to list report groups shared with the current AWS account or user. Valid values include:
--
--
--     * @ARN@ : List based on the ARN.
--
--
--     * @MODIFIED_TIME@ : List based on when information about the shared report group was last changed.
mkListSharedReportGroups ::
  ListSharedReportGroups
mkListSharedReportGroups =
  ListSharedReportGroups'
    { sortOrder = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | The order in which to list shared report groups. Valid values include:
--
--
--     * @ASCENDING@ : List in ascending order.
--
--
--     * @DESCENDING@ : List in descending order.
--
--
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgSortOrder :: Lens.Lens' ListSharedReportGroups (Lude.Maybe SortOrderType)
lsrgSortOrder = Lens.lens (sortOrder :: ListSharedReportGroups -> Lude.Maybe SortOrderType) (\s a -> s {sortOrder = a} :: ListSharedReportGroups)
{-# DEPRECATED lsrgSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgNextToken :: Lens.Lens' ListSharedReportGroups (Lude.Maybe Lude.Text)
lsrgNextToken = Lens.lens (nextToken :: ListSharedReportGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSharedReportGroups)
{-# DEPRECATED lsrgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of paginated shared report groups per response. Use @nextToken@ to iterate pages in the list of returned @ReportGroup@ objects. The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgMaxResults :: Lens.Lens' ListSharedReportGroups (Lude.Maybe Lude.Natural)
lsrgMaxResults = Lens.lens (maxResults :: ListSharedReportGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSharedReportGroups)
{-# DEPRECATED lsrgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The criterion to be used to list report groups shared with the current AWS account or user. Valid values include:
--
--
--     * @ARN@ : List based on the ARN.
--
--
--     * @MODIFIED_TIME@ : List based on when information about the shared report group was last changed.
--
--
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgSortBy :: Lens.Lens' ListSharedReportGroups (Lude.Maybe SharedResourceSortByType)
lsrgSortBy = Lens.lens (sortBy :: ListSharedReportGroups -> Lude.Maybe SharedResourceSortByType) (\s a -> s {sortBy = a} :: ListSharedReportGroups)
{-# DEPRECATED lsrgSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListSharedReportGroups where
  page rq rs
    | Page.stop (rs Lens.^. lsrgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsrgrsReportGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsrgNextToken Lens..~ rs Lens.^. lsrgrsNextToken

instance Lude.AWSRequest ListSharedReportGroups where
  type Rs ListSharedReportGroups = ListSharedReportGroupsResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSharedReportGroupsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "reportGroups")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSharedReportGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.ListSharedReportGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSharedReportGroups where
  toJSON ListSharedReportGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sortOrder" Lude..=) Lude.<$> sortOrder,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            ("sortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListSharedReportGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSharedReportGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSharedReportGroupsResponse' smart constructor.
data ListSharedReportGroupsResponse = ListSharedReportGroupsResponse'
  { -- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The list of ARNs for the report groups shared with the current AWS account or user.
    reportGroups :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSharedReportGroupsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
-- * 'reportGroups' - The list of ARNs for the report groups shared with the current AWS account or user.
-- * 'responseStatus' - The response status code.
mkListSharedReportGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSharedReportGroupsResponse
mkListSharedReportGroupsResponse pResponseStatus_ =
  ListSharedReportGroupsResponse'
    { nextToken = Lude.Nothing,
      reportGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgrsNextToken :: Lens.Lens' ListSharedReportGroupsResponse (Lude.Maybe Lude.Text)
lsrgrsNextToken = Lens.lens (nextToken :: ListSharedReportGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSharedReportGroupsResponse)
{-# DEPRECATED lsrgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of ARNs for the report groups shared with the current AWS account or user.
--
-- /Note:/ Consider using 'reportGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgrsReportGroups :: Lens.Lens' ListSharedReportGroupsResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
lsrgrsReportGroups = Lens.lens (reportGroups :: ListSharedReportGroupsResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {reportGroups = a} :: ListSharedReportGroupsResponse)
{-# DEPRECATED lsrgrsReportGroups "Use generic-lens or generic-optics with 'reportGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgrsResponseStatus :: Lens.Lens' ListSharedReportGroupsResponse Lude.Int
lsrgrsResponseStatus = Lens.lens (responseStatus :: ListSharedReportGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSharedReportGroupsResponse)
{-# DEPRECATED lsrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
