{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DescribeImportTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of import tasks for your account, including status information, times, IDs, the Amazon S3 Object URL for the import file, and more.
module Network.AWS.Discovery.DescribeImportTasks
  ( -- * Creating a request
    DescribeImportTasks (..),
    mkDescribeImportTasks,

    -- ** Request lenses
    ditFilters,
    ditNextToken,
    ditMaxResults,

    -- * Destructuring the response
    DescribeImportTasksResponse (..),
    mkDescribeImportTasksResponse,

    -- ** Response lenses
    ditrsTasks,
    ditrsNextToken,
    ditrsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeImportTasks' smart constructor.
data DescribeImportTasks = DescribeImportTasks'
  { -- | An array of name-value pairs that you provide to filter the results for the @DescribeImportTask@ request to a specific subset of results. Currently, wildcard values aren't supported for filters.
    filters :: Lude.Maybe [ImportTaskFilter],
    -- | The token to request a specific page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results that you want this request to return, up to 100.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImportTasks' with the minimum fields required to make a request.
--
-- * 'filters' - An array of name-value pairs that you provide to filter the results for the @DescribeImportTask@ request to a specific subset of results. Currently, wildcard values aren't supported for filters.
-- * 'nextToken' - The token to request a specific page of results.
-- * 'maxResults' - The maximum number of results that you want this request to return, up to 100.
mkDescribeImportTasks ::
  DescribeImportTasks
mkDescribeImportTasks =
  DescribeImportTasks'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | An array of name-value pairs that you provide to filter the results for the @DescribeImportTask@ request to a specific subset of results. Currently, wildcard values aren't supported for filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditFilters :: Lens.Lens' DescribeImportTasks (Lude.Maybe [ImportTaskFilter])
ditFilters = Lens.lens (filters :: DescribeImportTasks -> Lude.Maybe [ImportTaskFilter]) (\s a -> s {filters = a} :: DescribeImportTasks)
{-# DEPRECATED ditFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to request a specific page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditNextToken :: Lens.Lens' DescribeImportTasks (Lude.Maybe Lude.Text)
ditNextToken = Lens.lens (nextToken :: DescribeImportTasks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeImportTasks)
{-# DEPRECATED ditNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results that you want this request to return, up to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditMaxResults :: Lens.Lens' DescribeImportTasks (Lude.Maybe Lude.Natural)
ditMaxResults = Lens.lens (maxResults :: DescribeImportTasks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeImportTasks)
{-# DEPRECATED ditMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest DescribeImportTasks where
  type Rs DescribeImportTasks = DescribeImportTasksResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeImportTasksResponse'
            Lude.<$> (x Lude..?> "tasks" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeImportTasks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.DescribeImportTasks" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeImportTasks where
  toJSON DescribeImportTasks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("filters" Lude..=) Lude.<$> filters,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeImportTasks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeImportTasks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeImportTasksResponse' smart constructor.
data DescribeImportTasksResponse = DescribeImportTasksResponse'
  { -- | A returned array of import tasks that match any applied filters, up to the specified number of maximum results.
    tasks :: Lude.Maybe [ImportTask],
    -- | The token to request the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImportTasksResponse' with the minimum fields required to make a request.
--
-- * 'tasks' - A returned array of import tasks that match any applied filters, up to the specified number of maximum results.
-- * 'nextToken' - The token to request the next page of results.
-- * 'responseStatus' - The response status code.
mkDescribeImportTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeImportTasksResponse
mkDescribeImportTasksResponse pResponseStatus_ =
  DescribeImportTasksResponse'
    { tasks = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A returned array of import tasks that match any applied filters, up to the specified number of maximum results.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditrsTasks :: Lens.Lens' DescribeImportTasksResponse (Lude.Maybe [ImportTask])
ditrsTasks = Lens.lens (tasks :: DescribeImportTasksResponse -> Lude.Maybe [ImportTask]) (\s a -> s {tasks = a} :: DescribeImportTasksResponse)
{-# DEPRECATED ditrsTasks "Use generic-lens or generic-optics with 'tasks' instead." #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditrsNextToken :: Lens.Lens' DescribeImportTasksResponse (Lude.Maybe Lude.Text)
ditrsNextToken = Lens.lens (nextToken :: DescribeImportTasksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeImportTasksResponse)
{-# DEPRECATED ditrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditrsResponseStatus :: Lens.Lens' DescribeImportTasksResponse Lude.Int
ditrsResponseStatus = Lens.lens (responseStatus :: DescribeImportTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeImportTasksResponse)
{-# DEPRECATED ditrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
