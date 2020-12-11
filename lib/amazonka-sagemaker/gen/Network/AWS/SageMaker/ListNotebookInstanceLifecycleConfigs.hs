{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListNotebookInstanceLifecycleConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists notebook instance lifestyle configurations created with the 'CreateNotebookInstanceLifecycleConfig' API.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListNotebookInstanceLifecycleConfigs
  ( -- * Creating a request
    ListNotebookInstanceLifecycleConfigs (..),
    mkListNotebookInstanceLifecycleConfigs,

    -- ** Request lenses
    lnilcNameContains,
    lnilcLastModifiedTimeBefore,
    lnilcCreationTimeAfter,
    lnilcNextToken,
    lnilcSortOrder,
    lnilcLastModifiedTimeAfter,
    lnilcCreationTimeBefore,
    lnilcMaxResults,
    lnilcSortBy,

    -- * Destructuring the response
    ListNotebookInstanceLifecycleConfigsResponse (..),
    mkListNotebookInstanceLifecycleConfigsResponse,

    -- ** Response lenses
    lnilcrsNextToken,
    lnilcrsNotebookInstanceLifecycleConfigs,
    lnilcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListNotebookInstanceLifecycleConfigs' smart constructor.
data ListNotebookInstanceLifecycleConfigs = ListNotebookInstanceLifecycleConfigs'
  { nameContains ::
      Lude.Maybe
        Lude.Text,
    lastModifiedTimeBefore ::
      Lude.Maybe
        Lude.Timestamp,
    creationTimeAfter ::
      Lude.Maybe
        Lude.Timestamp,
    nextToken ::
      Lude.Maybe
        Lude.Text,
    sortOrder ::
      Lude.Maybe
        NotebookInstanceLifecycleConfigSortOrder,
    lastModifiedTimeAfter ::
      Lude.Maybe
        Lude.Timestamp,
    creationTimeBefore ::
      Lude.Maybe
        Lude.Timestamp,
    maxResults ::
      Lude.Maybe
        Lude.Natural,
    sortBy ::
      Lude.Maybe
        NotebookInstanceLifecycleConfigSortKey
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListNotebookInstanceLifecycleConfigs' with the minimum fields required to make a request.
--
-- * 'creationTimeAfter' - A filter that returns only lifecycle configurations that were created after the specified time (timestamp).
-- * 'creationTimeBefore' - A filter that returns only lifecycle configurations that were created before the specified time (timestamp).
-- * 'lastModifiedTimeAfter' - A filter that returns only lifecycle configurations that were modified after the specified time (timestamp).
-- * 'lastModifiedTimeBefore' - A filter that returns only lifecycle configurations that were modified before the specified time (timestamp).
-- * 'maxResults' - The maximum number of lifecycle configurations to return in the response.
-- * 'nameContains' - A string in the lifecycle configuration name. This filter returns only lifecycle configurations whose name contains the specified string.
-- * 'nextToken' - If the result of a @ListNotebookInstanceLifecycleConfigs@ request was truncated, the response includes a @NextToken@ . To get the next set of lifecycle configurations, use the token in the next request.
-- * 'sortBy' - Sorts the list of results. The default is @CreationTime@ .
-- * 'sortOrder' - The sort order for results.
mkListNotebookInstanceLifecycleConfigs ::
  ListNotebookInstanceLifecycleConfigs
mkListNotebookInstanceLifecycleConfigs =
  ListNotebookInstanceLifecycleConfigs'
    { nameContains =
        Lude.Nothing,
      lastModifiedTimeBefore = Lude.Nothing,
      creationTimeAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      lastModifiedTimeAfter = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A string in the lifecycle configuration name. This filter returns only lifecycle configurations whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcNameContains :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Lude.Maybe Lude.Text)
lnilcNameContains = Lens.lens (nameContains :: ListNotebookInstanceLifecycleConfigs -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListNotebookInstanceLifecycleConfigs)
{-# DEPRECATED lnilcNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A filter that returns only lifecycle configurations that were modified before the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcLastModifiedTimeBefore :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Lude.Maybe Lude.Timestamp)
lnilcLastModifiedTimeBefore = Lens.lens (lastModifiedTimeBefore :: ListNotebookInstanceLifecycleConfigs -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeBefore = a} :: ListNotebookInstanceLifecycleConfigs)
{-# DEPRECATED lnilcLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | A filter that returns only lifecycle configurations that were created after the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcCreationTimeAfter :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Lude.Maybe Lude.Timestamp)
lnilcCreationTimeAfter = Lens.lens (creationTimeAfter :: ListNotebookInstanceLifecycleConfigs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListNotebookInstanceLifecycleConfigs)
{-# DEPRECATED lnilcCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the result of a @ListNotebookInstanceLifecycleConfigs@ request was truncated, the response includes a @NextToken@ . To get the next set of lifecycle configurations, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcNextToken :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Lude.Maybe Lude.Text)
lnilcNextToken = Lens.lens (nextToken :: ListNotebookInstanceLifecycleConfigs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListNotebookInstanceLifecycleConfigs)
{-# DEPRECATED lnilcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for results.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcSortOrder :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Lude.Maybe NotebookInstanceLifecycleConfigSortOrder)
lnilcSortOrder = Lens.lens (sortOrder :: ListNotebookInstanceLifecycleConfigs -> Lude.Maybe NotebookInstanceLifecycleConfigSortOrder) (\s a -> s {sortOrder = a} :: ListNotebookInstanceLifecycleConfigs)
{-# DEPRECATED lnilcSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only lifecycle configurations that were modified after the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcLastModifiedTimeAfter :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Lude.Maybe Lude.Timestamp)
lnilcLastModifiedTimeAfter = Lens.lens (lastModifiedTimeAfter :: ListNotebookInstanceLifecycleConfigs -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeAfter = a} :: ListNotebookInstanceLifecycleConfigs)
{-# DEPRECATED lnilcLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only lifecycle configurations that were created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcCreationTimeBefore :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Lude.Maybe Lude.Timestamp)
lnilcCreationTimeBefore = Lens.lens (creationTimeBefore :: ListNotebookInstanceLifecycleConfigs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListNotebookInstanceLifecycleConfigs)
{-# DEPRECATED lnilcCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | The maximum number of lifecycle configurations to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcMaxResults :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Lude.Maybe Lude.Natural)
lnilcMaxResults = Lens.lens (maxResults :: ListNotebookInstanceLifecycleConfigs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListNotebookInstanceLifecycleConfigs)
{-# DEPRECATED lnilcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Sorts the list of results. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcSortBy :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Lude.Maybe NotebookInstanceLifecycleConfigSortKey)
lnilcSortBy = Lens.lens (sortBy :: ListNotebookInstanceLifecycleConfigs -> Lude.Maybe NotebookInstanceLifecycleConfigSortKey) (\s a -> s {sortBy = a} :: ListNotebookInstanceLifecycleConfigs)
{-# DEPRECATED lnilcSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListNotebookInstanceLifecycleConfigs where
  page rq rs
    | Page.stop (rs Lens.^. lnilcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lnilcrsNotebookInstanceLifecycleConfigs) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lnilcNextToken Lens..~ rs Lens.^. lnilcrsNextToken

instance Lude.AWSRequest ListNotebookInstanceLifecycleConfigs where
  type
    Rs ListNotebookInstanceLifecycleConfigs =
      ListNotebookInstanceLifecycleConfigsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListNotebookInstanceLifecycleConfigsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> ( x Lude..?> "NotebookInstanceLifecycleConfigs"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListNotebookInstanceLifecycleConfigs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SageMaker.ListNotebookInstanceLifecycleConfigs" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListNotebookInstanceLifecycleConfigs where
  toJSON ListNotebookInstanceLifecycleConfigs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NameContains" Lude..=) Lude.<$> nameContains,
            ("LastModifiedTimeBefore" Lude..=) Lude.<$> lastModifiedTimeBefore,
            ("CreationTimeAfter" Lude..=) Lude.<$> creationTimeAfter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("LastModifiedTimeAfter" Lude..=) Lude.<$> lastModifiedTimeAfter,
            ("CreationTimeBefore" Lude..=) Lude.<$> creationTimeBefore,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListNotebookInstanceLifecycleConfigs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListNotebookInstanceLifecycleConfigs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListNotebookInstanceLifecycleConfigsResponse' smart constructor.
data ListNotebookInstanceLifecycleConfigsResponse = ListNotebookInstanceLifecycleConfigsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    notebookInstanceLifecycleConfigs ::
      Lude.Maybe
        [NotebookInstanceLifecycleConfigSummary],
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

-- | Creates a value of 'ListNotebookInstanceLifecycleConfigsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this token. To get the next set of lifecycle configurations, use it in the next request.
-- * 'notebookInstanceLifecycleConfigs' - An array of @NotebookInstanceLifecycleConfiguration@ objects, each listing a lifecycle configuration.
-- * 'responseStatus' - The response status code.
mkListNotebookInstanceLifecycleConfigsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListNotebookInstanceLifecycleConfigsResponse
mkListNotebookInstanceLifecycleConfigsResponse pResponseStatus_ =
  ListNotebookInstanceLifecycleConfigsResponse'
    { nextToken =
        Lude.Nothing,
      notebookInstanceLifecycleConfigs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To get the next set of lifecycle configurations, use it in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcrsNextToken :: Lens.Lens' ListNotebookInstanceLifecycleConfigsResponse (Lude.Maybe Lude.Text)
lnilcrsNextToken = Lens.lens (nextToken :: ListNotebookInstanceLifecycleConfigsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListNotebookInstanceLifecycleConfigsResponse)
{-# DEPRECATED lnilcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of @NotebookInstanceLifecycleConfiguration@ objects, each listing a lifecycle configuration.
--
-- /Note:/ Consider using 'notebookInstanceLifecycleConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcrsNotebookInstanceLifecycleConfigs :: Lens.Lens' ListNotebookInstanceLifecycleConfigsResponse (Lude.Maybe [NotebookInstanceLifecycleConfigSummary])
lnilcrsNotebookInstanceLifecycleConfigs = Lens.lens (notebookInstanceLifecycleConfigs :: ListNotebookInstanceLifecycleConfigsResponse -> Lude.Maybe [NotebookInstanceLifecycleConfigSummary]) (\s a -> s {notebookInstanceLifecycleConfigs = a} :: ListNotebookInstanceLifecycleConfigsResponse)
{-# DEPRECATED lnilcrsNotebookInstanceLifecycleConfigs "Use generic-lens or generic-optics with 'notebookInstanceLifecycleConfigs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnilcrsResponseStatus :: Lens.Lens' ListNotebookInstanceLifecycleConfigsResponse Lude.Int
lnilcrsResponseStatus = Lens.lens (responseStatus :: ListNotebookInstanceLifecycleConfigsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListNotebookInstanceLifecycleConfigsResponse)
{-# DEPRECATED lnilcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
