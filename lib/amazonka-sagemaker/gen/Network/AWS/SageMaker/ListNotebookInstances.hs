{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListNotebookInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the Amazon SageMaker notebook instances in the requester's account in an AWS Region.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListNotebookInstances
  ( -- * Creating a request
    ListNotebookInstances (..),
    mkListNotebookInstances,

    -- ** Request lenses
    lniNameContains,
    lniDefaultCodeRepositoryContains,
    lniLastModifiedTimeBefore,
    lniNotebookInstanceLifecycleConfigNameContains,
    lniCreationTimeAfter,
    lniAdditionalCodeRepositoryEquals,
    lniNextToken,
    lniSortOrder,
    lniLastModifiedTimeAfter,
    lniCreationTimeBefore,
    lniStatusEquals,
    lniMaxResults,
    lniSortBy,

    -- * Destructuring the response
    ListNotebookInstancesResponse (..),
    mkListNotebookInstancesResponse,

    -- ** Response lenses
    lnirsNotebookInstances,
    lnirsNextToken,
    lnirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListNotebookInstances' smart constructor.
data ListNotebookInstances = ListNotebookInstances'
  { nameContains ::
      Lude.Maybe Lude.Text,
    defaultCodeRepositoryContains ::
      Lude.Maybe Lude.Text,
    lastModifiedTimeBefore ::
      Lude.Maybe Lude.Timestamp,
    notebookInstanceLifecycleConfigNameContains ::
      Lude.Maybe Lude.Text,
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    additionalCodeRepositoryEquals ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    sortOrder ::
      Lude.Maybe NotebookInstanceSortOrder,
    lastModifiedTimeAfter ::
      Lude.Maybe Lude.Timestamp,
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    statusEquals ::
      Lude.Maybe NotebookInstanceStatus,
    maxResults :: Lude.Maybe Lude.Natural,
    sortBy :: Lude.Maybe NotebookInstanceSortKey
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListNotebookInstances' with the minimum fields required to make a request.
--
-- * 'additionalCodeRepositoryEquals' - A filter that returns only notebook instances with associated with the specified git repository.
-- * 'creationTimeAfter' - A filter that returns only notebook instances that were created after the specified time (timestamp).
-- * 'creationTimeBefore' - A filter that returns only notebook instances that were created before the specified time (timestamp).
-- * 'defaultCodeRepositoryContains' - A string in the name or URL of a Git repository associated with this notebook instance. This filter returns only notebook instances associated with a git repository with a name that contains the specified string.
-- * 'lastModifiedTimeAfter' - A filter that returns only notebook instances that were modified after the specified time (timestamp).
-- * 'lastModifiedTimeBefore' - A filter that returns only notebook instances that were modified before the specified time (timestamp).
-- * 'maxResults' - The maximum number of notebook instances to return.
-- * 'nameContains' - A string in the notebook instances' name. This filter returns only notebook instances whose name contains the specified string.
-- * 'nextToken' - If the previous call to the @ListNotebookInstances@ is truncated, the response includes a @NextToken@ . You can use this token in your subsequent @ListNotebookInstances@ request to fetch the next set of notebook instances.
-- * 'notebookInstanceLifecycleConfigNameContains' - A string in the name of a notebook instances lifecycle configuration associated with this notebook instance. This filter returns only notebook instances associated with a lifecycle configuration with a name that contains the specified string.
-- * 'sortBy' - The field to sort results by. The default is @Name@ .
-- * 'sortOrder' - The sort order for results.
-- * 'statusEquals' - A filter that returns only notebook instances with the specified status.
mkListNotebookInstances ::
  ListNotebookInstances
mkListNotebookInstances =
  ListNotebookInstances'
    { nameContains = Lude.Nothing,
      defaultCodeRepositoryContains = Lude.Nothing,
      lastModifiedTimeBefore = Lude.Nothing,
      notebookInstanceLifecycleConfigNameContains = Lude.Nothing,
      creationTimeAfter = Lude.Nothing,
      additionalCodeRepositoryEquals = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      lastModifiedTimeAfter = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      statusEquals = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A string in the notebook instances' name. This filter returns only notebook instances whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniNameContains :: Lens.Lens' ListNotebookInstances (Lude.Maybe Lude.Text)
lniNameContains = Lens.lens (nameContains :: ListNotebookInstances -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListNotebookInstances)
{-# DEPRECATED lniNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A string in the name or URL of a Git repository associated with this notebook instance. This filter returns only notebook instances associated with a git repository with a name that contains the specified string.
--
-- /Note:/ Consider using 'defaultCodeRepositoryContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniDefaultCodeRepositoryContains :: Lens.Lens' ListNotebookInstances (Lude.Maybe Lude.Text)
lniDefaultCodeRepositoryContains = Lens.lens (defaultCodeRepositoryContains :: ListNotebookInstances -> Lude.Maybe Lude.Text) (\s a -> s {defaultCodeRepositoryContains = a} :: ListNotebookInstances)
{-# DEPRECATED lniDefaultCodeRepositoryContains "Use generic-lens or generic-optics with 'defaultCodeRepositoryContains' instead." #-}

-- | A filter that returns only notebook instances that were modified before the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniLastModifiedTimeBefore :: Lens.Lens' ListNotebookInstances (Lude.Maybe Lude.Timestamp)
lniLastModifiedTimeBefore = Lens.lens (lastModifiedTimeBefore :: ListNotebookInstances -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeBefore = a} :: ListNotebookInstances)
{-# DEPRECATED lniLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | A string in the name of a notebook instances lifecycle configuration associated with this notebook instance. This filter returns only notebook instances associated with a lifecycle configuration with a name that contains the specified string.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigNameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniNotebookInstanceLifecycleConfigNameContains :: Lens.Lens' ListNotebookInstances (Lude.Maybe Lude.Text)
lniNotebookInstanceLifecycleConfigNameContains = Lens.lens (notebookInstanceLifecycleConfigNameContains :: ListNotebookInstances -> Lude.Maybe Lude.Text) (\s a -> s {notebookInstanceLifecycleConfigNameContains = a} :: ListNotebookInstances)
{-# DEPRECATED lniNotebookInstanceLifecycleConfigNameContains "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigNameContains' instead." #-}

-- | A filter that returns only notebook instances that were created after the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniCreationTimeAfter :: Lens.Lens' ListNotebookInstances (Lude.Maybe Lude.Timestamp)
lniCreationTimeAfter = Lens.lens (creationTimeAfter :: ListNotebookInstances -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListNotebookInstances)
{-# DEPRECATED lniCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | A filter that returns only notebook instances with associated with the specified git repository.
--
-- /Note:/ Consider using 'additionalCodeRepositoryEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniAdditionalCodeRepositoryEquals :: Lens.Lens' ListNotebookInstances (Lude.Maybe Lude.Text)
lniAdditionalCodeRepositoryEquals = Lens.lens (additionalCodeRepositoryEquals :: ListNotebookInstances -> Lude.Maybe Lude.Text) (\s a -> s {additionalCodeRepositoryEquals = a} :: ListNotebookInstances)
{-# DEPRECATED lniAdditionalCodeRepositoryEquals "Use generic-lens or generic-optics with 'additionalCodeRepositoryEquals' instead." #-}

-- | If the previous call to the @ListNotebookInstances@ is truncated, the response includes a @NextToken@ . You can use this token in your subsequent @ListNotebookInstances@ request to fetch the next set of notebook instances.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniNextToken :: Lens.Lens' ListNotebookInstances (Lude.Maybe Lude.Text)
lniNextToken = Lens.lens (nextToken :: ListNotebookInstances -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListNotebookInstances)
{-# DEPRECATED lniNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for results.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniSortOrder :: Lens.Lens' ListNotebookInstances (Lude.Maybe NotebookInstanceSortOrder)
lniSortOrder = Lens.lens (sortOrder :: ListNotebookInstances -> Lude.Maybe NotebookInstanceSortOrder) (\s a -> s {sortOrder = a} :: ListNotebookInstances)
{-# DEPRECATED lniSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only notebook instances that were modified after the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniLastModifiedTimeAfter :: Lens.Lens' ListNotebookInstances (Lude.Maybe Lude.Timestamp)
lniLastModifiedTimeAfter = Lens.lens (lastModifiedTimeAfter :: ListNotebookInstances -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeAfter = a} :: ListNotebookInstances)
{-# DEPRECATED lniLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only notebook instances that were created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniCreationTimeBefore :: Lens.Lens' ListNotebookInstances (Lude.Maybe Lude.Timestamp)
lniCreationTimeBefore = Lens.lens (creationTimeBefore :: ListNotebookInstances -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListNotebookInstances)
{-# DEPRECATED lniCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that returns only notebook instances with the specified status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniStatusEquals :: Lens.Lens' ListNotebookInstances (Lude.Maybe NotebookInstanceStatus)
lniStatusEquals = Lens.lens (statusEquals :: ListNotebookInstances -> Lude.Maybe NotebookInstanceStatus) (\s a -> s {statusEquals = a} :: ListNotebookInstances)
{-# DEPRECATED lniStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

-- | The maximum number of notebook instances to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniMaxResults :: Lens.Lens' ListNotebookInstances (Lude.Maybe Lude.Natural)
lniMaxResults = Lens.lens (maxResults :: ListNotebookInstances -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListNotebookInstances)
{-# DEPRECATED lniMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The field to sort results by. The default is @Name@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lniSortBy :: Lens.Lens' ListNotebookInstances (Lude.Maybe NotebookInstanceSortKey)
lniSortBy = Lens.lens (sortBy :: ListNotebookInstances -> Lude.Maybe NotebookInstanceSortKey) (\s a -> s {sortBy = a} :: ListNotebookInstances)
{-# DEPRECATED lniSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListNotebookInstances where
  page rq rs
    | Page.stop (rs Lens.^. lnirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lnirsNotebookInstances) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lniNextToken Lens..~ rs Lens.^. lnirsNextToken

instance Lude.AWSRequest ListNotebookInstances where
  type Rs ListNotebookInstances = ListNotebookInstancesResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListNotebookInstancesResponse'
            Lude.<$> (x Lude..?> "NotebookInstances" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListNotebookInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListNotebookInstances" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListNotebookInstances where
  toJSON ListNotebookInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NameContains" Lude..=) Lude.<$> nameContains,
            ("DefaultCodeRepositoryContains" Lude..=)
              Lude.<$> defaultCodeRepositoryContains,
            ("LastModifiedTimeBefore" Lude..=) Lude.<$> lastModifiedTimeBefore,
            ("NotebookInstanceLifecycleConfigNameContains" Lude..=)
              Lude.<$> notebookInstanceLifecycleConfigNameContains,
            ("CreationTimeAfter" Lude..=) Lude.<$> creationTimeAfter,
            ("AdditionalCodeRepositoryEquals" Lude..=)
              Lude.<$> additionalCodeRepositoryEquals,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("LastModifiedTimeAfter" Lude..=) Lude.<$> lastModifiedTimeAfter,
            ("CreationTimeBefore" Lude..=) Lude.<$> creationTimeBefore,
            ("StatusEquals" Lude..=) Lude.<$> statusEquals,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListNotebookInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery ListNotebookInstances where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListNotebookInstancesResponse' smart constructor.
data ListNotebookInstancesResponse = ListNotebookInstancesResponse'
  { notebookInstances ::
      Lude.Maybe
        [NotebookInstanceSummary],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListNotebookInstancesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response to the previous @ListNotebookInstances@ request was truncated, Amazon SageMaker returns this token. To retrieve the next set of notebook instances, use the token in the next request.
-- * 'notebookInstances' - An array of @NotebookInstanceSummary@ objects, one for each notebook instance.
-- * 'responseStatus' - The response status code.
mkListNotebookInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListNotebookInstancesResponse
mkListNotebookInstancesResponse pResponseStatus_ =
  ListNotebookInstancesResponse'
    { notebookInstances = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @NotebookInstanceSummary@ objects, one for each notebook instance.
--
-- /Note:/ Consider using 'notebookInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnirsNotebookInstances :: Lens.Lens' ListNotebookInstancesResponse (Lude.Maybe [NotebookInstanceSummary])
lnirsNotebookInstances = Lens.lens (notebookInstances :: ListNotebookInstancesResponse -> Lude.Maybe [NotebookInstanceSummary]) (\s a -> s {notebookInstances = a} :: ListNotebookInstancesResponse)
{-# DEPRECATED lnirsNotebookInstances "Use generic-lens or generic-optics with 'notebookInstances' instead." #-}

-- | If the response to the previous @ListNotebookInstances@ request was truncated, Amazon SageMaker returns this token. To retrieve the next set of notebook instances, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnirsNextToken :: Lens.Lens' ListNotebookInstancesResponse (Lude.Maybe Lude.Text)
lnirsNextToken = Lens.lens (nextToken :: ListNotebookInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListNotebookInstancesResponse)
{-# DEPRECATED lnirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnirsResponseStatus :: Lens.Lens' ListNotebookInstancesResponse Lude.Int
lnirsResponseStatus = Lens.lens (responseStatus :: ListNotebookInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListNotebookInstancesResponse)
{-# DEPRECATED lnirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
