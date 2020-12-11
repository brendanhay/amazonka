{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeImportSnapshotTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your import snapshot tasks.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeImportSnapshotTasks
  ( -- * Creating a request
    DescribeImportSnapshotTasks (..),
    mkDescribeImportSnapshotTasks,

    -- ** Request lenses
    distFilters,
    distImportTaskIds,
    distNextToken,
    distDryRun,
    distMaxResults,

    -- * Destructuring the response
    DescribeImportSnapshotTasksResponse (..),
    mkDescribeImportSnapshotTasksResponse,

    -- ** Response lenses
    distrsNextToken,
    distrsImportSnapshotTasks,
    distrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeImportSnapshotTasks' smart constructor.
data DescribeImportSnapshotTasks = DescribeImportSnapshotTasks'
  { filters ::
      Lude.Maybe [Filter],
    importTaskIds ::
      Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImportSnapshotTasks' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - The filters.
-- * 'importTaskIds' - A list of import snapshot task IDs.
-- * 'maxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value.
-- * 'nextToken' - A token that indicates the next page of results.
mkDescribeImportSnapshotTasks ::
  DescribeImportSnapshotTasks
mkDescribeImportSnapshotTasks =
  DescribeImportSnapshotTasks'
    { filters = Lude.Nothing,
      importTaskIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
distFilters :: Lens.Lens' DescribeImportSnapshotTasks (Lude.Maybe [Filter])
distFilters = Lens.lens (filters :: DescribeImportSnapshotTasks -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeImportSnapshotTasks)
{-# DEPRECATED distFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | A list of import snapshot task IDs.
--
-- /Note:/ Consider using 'importTaskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
distImportTaskIds :: Lens.Lens' DescribeImportSnapshotTasks (Lude.Maybe [Lude.Text])
distImportTaskIds = Lens.lens (importTaskIds :: DescribeImportSnapshotTasks -> Lude.Maybe [Lude.Text]) (\s a -> s {importTaskIds = a} :: DescribeImportSnapshotTasks)
{-# DEPRECATED distImportTaskIds "Use generic-lens or generic-optics with 'importTaskIds' instead." #-}

-- | A token that indicates the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
distNextToken :: Lens.Lens' DescribeImportSnapshotTasks (Lude.Maybe Lude.Text)
distNextToken = Lens.lens (nextToken :: DescribeImportSnapshotTasks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeImportSnapshotTasks)
{-# DEPRECATED distNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
distDryRun :: Lens.Lens' DescribeImportSnapshotTasks (Lude.Maybe Lude.Bool)
distDryRun = Lens.lens (dryRun :: DescribeImportSnapshotTasks -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeImportSnapshotTasks)
{-# DEPRECATED distDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
distMaxResults :: Lens.Lens' DescribeImportSnapshotTasks (Lude.Maybe Lude.Int)
distMaxResults = Lens.lens (maxResults :: DescribeImportSnapshotTasks -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeImportSnapshotTasks)
{-# DEPRECATED distMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeImportSnapshotTasks where
  page rq rs
    | Page.stop (rs Lens.^. distrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. distrsImportSnapshotTasks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& distNextToken Lens..~ rs Lens.^. distrsNextToken

instance Lude.AWSRequest DescribeImportSnapshotTasks where
  type
    Rs DescribeImportSnapshotTasks =
      DescribeImportSnapshotTasksResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeImportSnapshotTasksResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "importSnapshotTaskSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeImportSnapshotTasks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeImportSnapshotTasks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeImportSnapshotTasks where
  toQuery DescribeImportSnapshotTasks' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeImportSnapshotTasks" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filters" Lude.<$> filters),
        Lude.toQuery
          (Lude.toQueryList "ImportTaskId" Lude.<$> importTaskIds),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeImportSnapshotTasksResponse' smart constructor.
data DescribeImportSnapshotTasksResponse = DescribeImportSnapshotTasksResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    importSnapshotTasks ::
      Lude.Maybe
        [ImportSnapshotTask],
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

-- | Creates a value of 'DescribeImportSnapshotTasksResponse' with the minimum fields required to make a request.
--
-- * 'importSnapshotTasks' - A list of zero or more import snapshot tasks that are currently active or were completed or canceled in the previous 7 days.
-- * 'nextToken' - The token to use to get the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeImportSnapshotTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeImportSnapshotTasksResponse
mkDescribeImportSnapshotTasksResponse pResponseStatus_ =
  DescribeImportSnapshotTasksResponse'
    { nextToken = Lude.Nothing,
      importSnapshotTasks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to get the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
distrsNextToken :: Lens.Lens' DescribeImportSnapshotTasksResponse (Lude.Maybe Lude.Text)
distrsNextToken = Lens.lens (nextToken :: DescribeImportSnapshotTasksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeImportSnapshotTasksResponse)
{-# DEPRECATED distrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of zero or more import snapshot tasks that are currently active or were completed or canceled in the previous 7 days.
--
-- /Note:/ Consider using 'importSnapshotTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
distrsImportSnapshotTasks :: Lens.Lens' DescribeImportSnapshotTasksResponse (Lude.Maybe [ImportSnapshotTask])
distrsImportSnapshotTasks = Lens.lens (importSnapshotTasks :: DescribeImportSnapshotTasksResponse -> Lude.Maybe [ImportSnapshotTask]) (\s a -> s {importSnapshotTasks = a} :: DescribeImportSnapshotTasksResponse)
{-# DEPRECATED distrsImportSnapshotTasks "Use generic-lens or generic-optics with 'importSnapshotTasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
distrsResponseStatus :: Lens.Lens' DescribeImportSnapshotTasksResponse Lude.Int
distrsResponseStatus = Lens.lens (responseStatus :: DescribeImportSnapshotTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeImportSnapshotTasksResponse)
{-# DEPRECATED distrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
