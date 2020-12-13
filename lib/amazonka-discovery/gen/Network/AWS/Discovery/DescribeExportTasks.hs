{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DescribeExportTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve status of one or more export tasks. You can retrieve the status of up to 100 export tasks.
--
-- This operation returns paginated results.
module Network.AWS.Discovery.DescribeExportTasks
  ( -- * Creating a request
    DescribeExportTasks (..),
    mkDescribeExportTasks,

    -- ** Request lenses
    detFilters,
    detNextToken,
    detExportIds,
    detMaxResults,

    -- * Destructuring the response
    DescribeExportTasksResponse (..),
    mkDescribeExportTasksResponse,

    -- ** Response lenses
    detrsNextToken,
    detrsExportsInfo,
    detrsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { -- | One or more filters.
    --
    --
    --     * @AgentId@ - ID of the agent whose collected data will be exported
    filters :: Lude.Maybe [ExportFilter],
    -- | The @nextToken@ value returned from a previous paginated @DescribeExportTasks@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is null when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | One or more unique identifiers used to query the status of an export request.
    exportIds :: Lude.Maybe [Lude.Text],
    -- | The maximum number of volume results returned by @DescribeExportTasks@ in paginated output. When this parameter is used, @DescribeExportTasks@ only returns @maxResults@ results in a single page along with a @nextToken@ response element.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeExportTasks' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
--
--
--     * @AgentId@ - ID of the agent whose collected data will be exported
--
--
-- * 'nextToken' - The @nextToken@ value returned from a previous paginated @DescribeExportTasks@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is null when there are no more results to return.
-- * 'exportIds' - One or more unique identifiers used to query the status of an export request.
-- * 'maxResults' - The maximum number of volume results returned by @DescribeExportTasks@ in paginated output. When this parameter is used, @DescribeExportTasks@ only returns @maxResults@ results in a single page along with a @nextToken@ response element.
mkDescribeExportTasks ::
  DescribeExportTasks
mkDescribeExportTasks =
  DescribeExportTasks'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      exportIds = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @AgentId@ - ID of the agent whose collected data will be exported
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detFilters :: Lens.Lens' DescribeExportTasks (Lude.Maybe [ExportFilter])
detFilters = Lens.lens (filters :: DescribeExportTasks -> Lude.Maybe [ExportFilter]) (\s a -> s {filters = a} :: DescribeExportTasks)
{-# DEPRECATED detFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The @nextToken@ value returned from a previous paginated @DescribeExportTasks@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detNextToken :: Lens.Lens' DescribeExportTasks (Lude.Maybe Lude.Text)
detNextToken = Lens.lens (nextToken :: DescribeExportTasks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeExportTasks)
{-# DEPRECATED detNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more unique identifiers used to query the status of an export request.
--
-- /Note:/ Consider using 'exportIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detExportIds :: Lens.Lens' DescribeExportTasks (Lude.Maybe [Lude.Text])
detExportIds = Lens.lens (exportIds :: DescribeExportTasks -> Lude.Maybe [Lude.Text]) (\s a -> s {exportIds = a} :: DescribeExportTasks)
{-# DEPRECATED detExportIds "Use generic-lens or generic-optics with 'exportIds' instead." #-}

-- | The maximum number of volume results returned by @DescribeExportTasks@ in paginated output. When this parameter is used, @DescribeExportTasks@ only returns @maxResults@ results in a single page along with a @nextToken@ response element.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detMaxResults :: Lens.Lens' DescribeExportTasks (Lude.Maybe Lude.Int)
detMaxResults = Lens.lens (maxResults :: DescribeExportTasks -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeExportTasks)
{-# DEPRECATED detMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeExportTasks where
  page rq rs
    | Page.stop (rs Lens.^. detrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. detrsExportsInfo) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& detNextToken Lens..~ rs Lens.^. detrsNextToken

instance Lude.AWSRequest DescribeExportTasks where
  type Rs DescribeExportTasks = DescribeExportTasksResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeExportTasksResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "exportsInfo" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeExportTasks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.DescribeExportTasks" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeExportTasks where
  toJSON DescribeExportTasks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("filters" Lude..=) Lude.<$> filters,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("exportIds" Lude..=) Lude.<$> exportIds,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeExportTasks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeExportTasks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { -- | The @nextToken@ value to include in a future @DescribeExportTasks@ request. When the results of a @DescribeExportTasks@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is null when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Contains one or more sets of export request details. When the status of a request is @SUCCEEDED@ , the response includes a URL for an Amazon S3 bucket where you can view the data in a CSV file.
    exportsInfo :: Lude.Maybe [ExportInfo],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeExportTasksResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @nextToken@ value to include in a future @DescribeExportTasks@ request. When the results of a @DescribeExportTasks@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is null when there are no more results to return.
-- * 'exportsInfo' - Contains one or more sets of export request details. When the status of a request is @SUCCEEDED@ , the response includes a URL for an Amazon S3 bucket where you can view the data in a CSV file.
-- * 'responseStatus' - The response status code.
mkDescribeExportTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeExportTasksResponse
mkDescribeExportTasksResponse pResponseStatus_ =
  DescribeExportTasksResponse'
    { nextToken = Lude.Nothing,
      exportsInfo = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @nextToken@ value to include in a future @DescribeExportTasks@ request. When the results of a @DescribeExportTasks@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsNextToken :: Lens.Lens' DescribeExportTasksResponse (Lude.Maybe Lude.Text)
detrsNextToken = Lens.lens (nextToken :: DescribeExportTasksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeExportTasksResponse)
{-# DEPRECATED detrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Contains one or more sets of export request details. When the status of a request is @SUCCEEDED@ , the response includes a URL for an Amazon S3 bucket where you can view the data in a CSV file.
--
-- /Note:/ Consider using 'exportsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsExportsInfo :: Lens.Lens' DescribeExportTasksResponse (Lude.Maybe [ExportInfo])
detrsExportsInfo = Lens.lens (exportsInfo :: DescribeExportTasksResponse -> Lude.Maybe [ExportInfo]) (\s a -> s {exportsInfo = a} :: DescribeExportTasksResponse)
{-# DEPRECATED detrsExportsInfo "Use generic-lens or generic-optics with 'exportsInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsResponseStatus :: Lens.Lens' DescribeExportTasksResponse Lude.Int
detrsResponseStatus = Lens.lens (responseStatus :: DescribeExportTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeExportTasksResponse)
{-# DEPRECATED detrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
