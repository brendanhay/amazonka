{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeImportImageTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays details about an import virtual machine or import snapshot tasks that are already created.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeImportImageTasks
  ( -- * Creating a request
    DescribeImportImageTasks (..),
    mkDescribeImportImageTasks,

    -- ** Request lenses
    diitFilters,
    diitImportTaskIds,
    diitNextToken,
    diitDryRun,
    diitMaxResults,

    -- * Destructuring the response
    DescribeImportImageTasksResponse (..),
    mkDescribeImportImageTasksResponse,

    -- ** Response lenses
    diitrsNextToken,
    diitrsImportImageTasks,
    diitrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeImportImageTasks' smart constructor.
data DescribeImportImageTasks = DescribeImportImageTasks'
  { -- | Filter tasks using the @task-state@ filter and one of the following values: @active@ , @completed@ , @deleting@ , or @deleted@ .
    filters :: Lude.Maybe [Filter],
    -- | The IDs of the import image tasks.
    importTaskIds :: Lude.Maybe [Lude.Text],
    -- | A token that indicates the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return in a single call.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImportImageTasks' with the minimum fields required to make a request.
--
-- * 'filters' - Filter tasks using the @task-state@ filter and one of the following values: @active@ , @completed@ , @deleting@ , or @deleted@ .
-- * 'importTaskIds' - The IDs of the import image tasks.
-- * 'nextToken' - A token that indicates the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return in a single call.
mkDescribeImportImageTasks ::
  DescribeImportImageTasks
mkDescribeImportImageTasks =
  DescribeImportImageTasks'
    { filters = Lude.Nothing,
      importTaskIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Filter tasks using the @task-state@ filter and one of the following values: @active@ , @completed@ , @deleting@ , or @deleted@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitFilters :: Lens.Lens' DescribeImportImageTasks (Lude.Maybe [Filter])
diitFilters = Lens.lens (filters :: DescribeImportImageTasks -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeImportImageTasks)
{-# DEPRECATED diitFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The IDs of the import image tasks.
--
-- /Note:/ Consider using 'importTaskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitImportTaskIds :: Lens.Lens' DescribeImportImageTasks (Lude.Maybe [Lude.Text])
diitImportTaskIds = Lens.lens (importTaskIds :: DescribeImportImageTasks -> Lude.Maybe [Lude.Text]) (\s a -> s {importTaskIds = a} :: DescribeImportImageTasks)
{-# DEPRECATED diitImportTaskIds "Use generic-lens or generic-optics with 'importTaskIds' instead." #-}

-- | A token that indicates the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitNextToken :: Lens.Lens' DescribeImportImageTasks (Lude.Maybe Lude.Text)
diitNextToken = Lens.lens (nextToken :: DescribeImportImageTasks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeImportImageTasks)
{-# DEPRECATED diitNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitDryRun :: Lens.Lens' DescribeImportImageTasks (Lude.Maybe Lude.Bool)
diitDryRun = Lens.lens (dryRun :: DescribeImportImageTasks -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeImportImageTasks)
{-# DEPRECATED diitDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitMaxResults :: Lens.Lens' DescribeImportImageTasks (Lude.Maybe Lude.Int)
diitMaxResults = Lens.lens (maxResults :: DescribeImportImageTasks -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeImportImageTasks)
{-# DEPRECATED diitMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeImportImageTasks where
  page rq rs
    | Page.stop (rs Lens.^. diitrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. diitrsImportImageTasks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& diitNextToken Lens..~ rs Lens.^. diitrsNextToken

instance Lude.AWSRequest DescribeImportImageTasks where
  type Rs DescribeImportImageTasks = DescribeImportImageTasksResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeImportImageTasksResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "importImageTaskSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeImportImageTasks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeImportImageTasks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeImportImageTasks where
  toQuery DescribeImportImageTasks' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeImportImageTasks" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filters" Lude.<$> filters),
        Lude.toQuery
          (Lude.toQueryList "ImportTaskId" Lude.<$> importTaskIds),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeImportImageTasksResponse' smart constructor.
data DescribeImportImageTasksResponse = DescribeImportImageTasksResponse'
  { -- | The token to use to get the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of zero or more import image tasks that are currently active or were completed or canceled in the previous 7 days.
    importImageTasks :: Lude.Maybe [ImportImageTask],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImportImageTasksResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to get the next page of results. This value is @null@ when there are no more results to return.
-- * 'importImageTasks' - A list of zero or more import image tasks that are currently active or were completed or canceled in the previous 7 days.
-- * 'responseStatus' - The response status code.
mkDescribeImportImageTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeImportImageTasksResponse
mkDescribeImportImageTasksResponse pResponseStatus_ =
  DescribeImportImageTasksResponse'
    { nextToken = Lude.Nothing,
      importImageTasks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to get the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitrsNextToken :: Lens.Lens' DescribeImportImageTasksResponse (Lude.Maybe Lude.Text)
diitrsNextToken = Lens.lens (nextToken :: DescribeImportImageTasksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeImportImageTasksResponse)
{-# DEPRECATED diitrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of zero or more import image tasks that are currently active or were completed or canceled in the previous 7 days.
--
-- /Note:/ Consider using 'importImageTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitrsImportImageTasks :: Lens.Lens' DescribeImportImageTasksResponse (Lude.Maybe [ImportImageTask])
diitrsImportImageTasks = Lens.lens (importImageTasks :: DescribeImportImageTasksResponse -> Lude.Maybe [ImportImageTask]) (\s a -> s {importImageTasks = a} :: DescribeImportImageTasksResponse)
{-# DEPRECATED diitrsImportImageTasks "Use generic-lens or generic-optics with 'importImageTasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diitrsResponseStatus :: Lens.Lens' DescribeImportImageTasksResponse Lude.Int
diitrsResponseStatus = Lens.lens (responseStatus :: DescribeImportImageTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeImportImageTasksResponse)
{-# DEPRECATED diitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
