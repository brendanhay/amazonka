{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeExportImageTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified export image tasks or all of your export image tasks.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeExportImageTasks
  ( -- * Creating a request
    DescribeExportImageTasks (..),
    mkDescribeExportImageTasks,

    -- ** Request lenses
    deitExportImageTaskIds,
    deitFilters,
    deitNextToken,
    deitDryRun,
    deitMaxResults,

    -- * Destructuring the response
    DescribeExportImageTasksResponse (..),
    mkDescribeExportImageTasksResponse,

    -- ** Response lenses
    deitrsExportImageTasks,
    deitrsNextToken,
    deitrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeExportImageTasks' smart constructor.
data DescribeExportImageTasks = DescribeExportImageTasks'
  { exportImageTaskIds ::
      Lude.Maybe [Lude.Text],
    filters :: Lude.Maybe [Filter],
    nextToken :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'DescribeExportImageTasks' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'exportImageTaskIds' - The IDs of the export image tasks.
-- * 'filters' - Filter tasks using the @task-state@ filter and one of the following values: @active@ , @completed@ , @deleting@ , or @deleted@ .
-- * 'maxResults' - The maximum number of results to return in a single call.
-- * 'nextToken' - A token that indicates the next page of results.
mkDescribeExportImageTasks ::
  DescribeExportImageTasks
mkDescribeExportImageTasks =
  DescribeExportImageTasks'
    { exportImageTaskIds = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The IDs of the export image tasks.
--
-- /Note:/ Consider using 'exportImageTaskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitExportImageTaskIds :: Lens.Lens' DescribeExportImageTasks (Lude.Maybe [Lude.Text])
deitExportImageTaskIds = Lens.lens (exportImageTaskIds :: DescribeExportImageTasks -> Lude.Maybe [Lude.Text]) (\s a -> s {exportImageTaskIds = a} :: DescribeExportImageTasks)
{-# DEPRECATED deitExportImageTaskIds "Use generic-lens or generic-optics with 'exportImageTaskIds' instead." #-}

-- | Filter tasks using the @task-state@ filter and one of the following values: @active@ , @completed@ , @deleting@ , or @deleted@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitFilters :: Lens.Lens' DescribeExportImageTasks (Lude.Maybe [Filter])
deitFilters = Lens.lens (filters :: DescribeExportImageTasks -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeExportImageTasks)
{-# DEPRECATED deitFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | A token that indicates the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitNextToken :: Lens.Lens' DescribeExportImageTasks (Lude.Maybe Lude.Text)
deitNextToken = Lens.lens (nextToken :: DescribeExportImageTasks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeExportImageTasks)
{-# DEPRECATED deitNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitDryRun :: Lens.Lens' DescribeExportImageTasks (Lude.Maybe Lude.Bool)
deitDryRun = Lens.lens (dryRun :: DescribeExportImageTasks -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeExportImageTasks)
{-# DEPRECATED deitDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitMaxResults :: Lens.Lens' DescribeExportImageTasks (Lude.Maybe Lude.Natural)
deitMaxResults = Lens.lens (maxResults :: DescribeExportImageTasks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeExportImageTasks)
{-# DEPRECATED deitMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeExportImageTasks where
  page rq rs
    | Page.stop (rs Lens.^. deitrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. deitrsExportImageTasks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& deitNextToken Lens..~ rs Lens.^. deitrsNextToken

instance Lude.AWSRequest DescribeExportImageTasks where
  type Rs DescribeExportImageTasks = DescribeExportImageTasksResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeExportImageTasksResponse'
            Lude.<$> ( x Lude..@? "exportImageTaskSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeExportImageTasks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeExportImageTasks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeExportImageTasks where
  toQuery DescribeExportImageTasks' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeExportImageTasks" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "ExportImageTaskId" Lude.<$> exportImageTaskIds),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeExportImageTasksResponse' smart constructor.
data DescribeExportImageTasksResponse = DescribeExportImageTasksResponse'
  { exportImageTasks ::
      Lude.Maybe
        [ExportImageTask],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeExportImageTasksResponse' with the minimum fields required to make a request.
--
-- * 'exportImageTasks' - Information about the export image tasks.
-- * 'nextToken' - The token to use to get the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeExportImageTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeExportImageTasksResponse
mkDescribeExportImageTasksResponse pResponseStatus_ =
  DescribeExportImageTasksResponse'
    { exportImageTasks =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the export image tasks.
--
-- /Note:/ Consider using 'exportImageTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitrsExportImageTasks :: Lens.Lens' DescribeExportImageTasksResponse (Lude.Maybe [ExportImageTask])
deitrsExportImageTasks = Lens.lens (exportImageTasks :: DescribeExportImageTasksResponse -> Lude.Maybe [ExportImageTask]) (\s a -> s {exportImageTasks = a} :: DescribeExportImageTasksResponse)
{-# DEPRECATED deitrsExportImageTasks "Use generic-lens or generic-optics with 'exportImageTasks' instead." #-}

-- | The token to use to get the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitrsNextToken :: Lens.Lens' DescribeExportImageTasksResponse (Lude.Maybe Lude.Text)
deitrsNextToken = Lens.lens (nextToken :: DescribeExportImageTasksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeExportImageTasksResponse)
{-# DEPRECATED deitrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deitrsResponseStatus :: Lens.Lens' DescribeExportImageTasksResponse Lude.Int
deitrsResponseStatus = Lens.lens (responseStatus :: DescribeExportImageTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeExportImageTasksResponse)
{-# DEPRECATED deitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
