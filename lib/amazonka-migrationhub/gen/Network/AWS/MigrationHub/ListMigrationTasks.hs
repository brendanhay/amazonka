{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.ListMigrationTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all, or filtered by resource name, migration tasks associated with the user account making this call. This API has the following traits:
--
--
--     * Can show a summary list of the most recent migration tasks.
--
--
--     * Can show a summary list of migration tasks associated with a given discovered resource.
--
--
--     * Lists migration tasks in a paginated interface.
--
--
--
-- This operation returns paginated results.
module Network.AWS.MigrationHub.ListMigrationTasks
  ( -- * Creating a request
    ListMigrationTasks (..),
    mkListMigrationTasks,

    -- ** Request lenses
    lmtResourceName,
    lmtNextToken,
    lmtMaxResults,

    -- * Destructuring the response
    ListMigrationTasksResponse (..),
    mkListMigrationTasksResponse,

    -- ** Response lenses
    lmtrsMigrationTaskSummaryList,
    lmtrsNextToken,
    lmtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListMigrationTasks' smart constructor.
data ListMigrationTasks = ListMigrationTasks'
  { -- | Filter migration tasks by discovered resource name.
    resourceName :: Lude.Maybe Lude.Text,
    -- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | Value to specify how many results are returned per page.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMigrationTasks' with the minimum fields required to make a request.
--
-- * 'resourceName' - Filter migration tasks by discovered resource name.
-- * 'nextToken' - If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
-- * 'maxResults' - Value to specify how many results are returned per page.
mkListMigrationTasks ::
  ListMigrationTasks
mkListMigrationTasks =
  ListMigrationTasks'
    { resourceName = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Filter migration tasks by discovered resource name.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtResourceName :: Lens.Lens' ListMigrationTasks (Lude.Maybe Lude.Text)
lmtResourceName = Lens.lens (resourceName :: ListMigrationTasks -> Lude.Maybe Lude.Text) (\s a -> s {resourceName = a} :: ListMigrationTasks)
{-# DEPRECATED lmtResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtNextToken :: Lens.Lens' ListMigrationTasks (Lude.Maybe Lude.Text)
lmtNextToken = Lens.lens (nextToken :: ListMigrationTasks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMigrationTasks)
{-# DEPRECATED lmtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Value to specify how many results are returned per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtMaxResults :: Lens.Lens' ListMigrationTasks (Lude.Maybe Lude.Natural)
lmtMaxResults = Lens.lens (maxResults :: ListMigrationTasks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListMigrationTasks)
{-# DEPRECATED lmtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListMigrationTasks where
  page rq rs
    | Page.stop (rs Lens.^. lmtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lmtrsMigrationTaskSummaryList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lmtNextToken Lens..~ rs Lens.^. lmtrsNextToken

instance Lude.AWSRequest ListMigrationTasks where
  type Rs ListMigrationTasks = ListMigrationTasksResponse
  request = Req.postJSON migrationHubService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListMigrationTasksResponse'
            Lude.<$> (x Lude..?> "MigrationTaskSummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListMigrationTasks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMigrationHub.ListMigrationTasks" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListMigrationTasks where
  toJSON ListMigrationTasks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceName" Lude..=) Lude.<$> resourceName,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListMigrationTasks where
  toPath = Lude.const "/"

instance Lude.ToQuery ListMigrationTasks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListMigrationTasksResponse' smart constructor.
data ListMigrationTasksResponse = ListMigrationTasksResponse'
  { -- | Lists the migration task's summary which includes: @MigrationTaskName@ , @ProgressPercent@ , @ProgressUpdateStream@ , @Status@ , and the @UpdateDateTime@ for each task.
    migrationTaskSummaryList :: Lude.Maybe [MigrationTaskSummary],
    -- | If there are more migration tasks than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMigrationTasksResponse' with the minimum fields required to make a request.
--
-- * 'migrationTaskSummaryList' - Lists the migration task's summary which includes: @MigrationTaskName@ , @ProgressPercent@ , @ProgressUpdateStream@ , @Status@ , and the @UpdateDateTime@ for each task.
-- * 'nextToken' - If there are more migration tasks than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
-- * 'responseStatus' - The response status code.
mkListMigrationTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListMigrationTasksResponse
mkListMigrationTasksResponse pResponseStatus_ =
  ListMigrationTasksResponse'
    { migrationTaskSummaryList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Lists the migration task's summary which includes: @MigrationTaskName@ , @ProgressPercent@ , @ProgressUpdateStream@ , @Status@ , and the @UpdateDateTime@ for each task.
--
-- /Note:/ Consider using 'migrationTaskSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtrsMigrationTaskSummaryList :: Lens.Lens' ListMigrationTasksResponse (Lude.Maybe [MigrationTaskSummary])
lmtrsMigrationTaskSummaryList = Lens.lens (migrationTaskSummaryList :: ListMigrationTasksResponse -> Lude.Maybe [MigrationTaskSummary]) (\s a -> s {migrationTaskSummaryList = a} :: ListMigrationTasksResponse)
{-# DEPRECATED lmtrsMigrationTaskSummaryList "Use generic-lens or generic-optics with 'migrationTaskSummaryList' instead." #-}

-- | If there are more migration tasks than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtrsNextToken :: Lens.Lens' ListMigrationTasksResponse (Lude.Maybe Lude.Text)
lmtrsNextToken = Lens.lens (nextToken :: ListMigrationTasksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMigrationTasksResponse)
{-# DEPRECATED lmtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtrsResponseStatus :: Lens.Lens' ListMigrationTasksResponse Lude.Int
lmtrsResponseStatus = Lens.lens (responseStatus :: ListMigrationTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListMigrationTasksResponse)
{-# DEPRECATED lmtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
