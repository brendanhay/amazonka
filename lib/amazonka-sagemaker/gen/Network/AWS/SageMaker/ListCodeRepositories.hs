{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListCodeRepositories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the Git repositories in your account.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListCodeRepositories
  ( -- * Creating a request
    ListCodeRepositories (..),
    mkListCodeRepositories,

    -- ** Request lenses
    lcrNameContains,
    lcrLastModifiedTimeBefore,
    lcrCreationTimeAfter,
    lcrNextToken,
    lcrSortOrder,
    lcrLastModifiedTimeAfter,
    lcrCreationTimeBefore,
    lcrMaxResults,
    lcrSortBy,

    -- * Destructuring the response
    ListCodeRepositoriesResponse (..),
    mkListCodeRepositoriesResponse,

    -- ** Response lenses
    lcrrsCodeRepositorySummaryList,
    lcrrsNextToken,
    lcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListCodeRepositories' smart constructor.
data ListCodeRepositories = ListCodeRepositories'
  { -- | A string in the Git repositories name. This filter returns only repositories whose name contains the specified string.
    nameContains :: Lude.Maybe Lude.Text,
    -- | A filter that returns only Git repositories that were last modified before the specified time.
    lastModifiedTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only Git repositories that were created after the specified time.
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | If the result of a @ListCodeRepositoriesOutput@ request was truncated, the response includes a @NextToken@ . To get the next set of Git repositories, use the token in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The sort order for results. The default is @Ascending@ .
    sortOrder :: Lude.Maybe CodeRepositorySortOrder,
    -- | A filter that returns only Git repositories that were last modified after the specified time.
    lastModifiedTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only Git repositories that were created before the specified time.
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | The maximum number of Git repositories to return in the response.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The field to sort results by. The default is @Name@ .
    sortBy :: Lude.Maybe CodeRepositorySortBy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCodeRepositories' with the minimum fields required to make a request.
--
-- * 'nameContains' - A string in the Git repositories name. This filter returns only repositories whose name contains the specified string.
-- * 'lastModifiedTimeBefore' - A filter that returns only Git repositories that were last modified before the specified time.
-- * 'creationTimeAfter' - A filter that returns only Git repositories that were created after the specified time.
-- * 'nextToken' - If the result of a @ListCodeRepositoriesOutput@ request was truncated, the response includes a @NextToken@ . To get the next set of Git repositories, use the token in the next request.
-- * 'sortOrder' - The sort order for results. The default is @Ascending@ .
-- * 'lastModifiedTimeAfter' - A filter that returns only Git repositories that were last modified after the specified time.
-- * 'creationTimeBefore' - A filter that returns only Git repositories that were created before the specified time.
-- * 'maxResults' - The maximum number of Git repositories to return in the response.
-- * 'sortBy' - The field to sort results by. The default is @Name@ .
mkListCodeRepositories ::
  ListCodeRepositories
mkListCodeRepositories =
  ListCodeRepositories'
    { nameContains = Lude.Nothing,
      lastModifiedTimeBefore = Lude.Nothing,
      creationTimeAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      lastModifiedTimeAfter = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A string in the Git repositories name. This filter returns only repositories whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrNameContains :: Lens.Lens' ListCodeRepositories (Lude.Maybe Lude.Text)
lcrNameContains = Lens.lens (nameContains :: ListCodeRepositories -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListCodeRepositories)
{-# DEPRECATED lcrNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A filter that returns only Git repositories that were last modified before the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrLastModifiedTimeBefore :: Lens.Lens' ListCodeRepositories (Lude.Maybe Lude.Timestamp)
lcrLastModifiedTimeBefore = Lens.lens (lastModifiedTimeBefore :: ListCodeRepositories -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeBefore = a} :: ListCodeRepositories)
{-# DEPRECATED lcrLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | A filter that returns only Git repositories that were created after the specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrCreationTimeAfter :: Lens.Lens' ListCodeRepositories (Lude.Maybe Lude.Timestamp)
lcrCreationTimeAfter = Lens.lens (creationTimeAfter :: ListCodeRepositories -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListCodeRepositories)
{-# DEPRECATED lcrCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the result of a @ListCodeRepositoriesOutput@ request was truncated, the response includes a @NextToken@ . To get the next set of Git repositories, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrNextToken :: Lens.Lens' ListCodeRepositories (Lude.Maybe Lude.Text)
lcrNextToken = Lens.lens (nextToken :: ListCodeRepositories -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCodeRepositories)
{-# DEPRECATED lcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrSortOrder :: Lens.Lens' ListCodeRepositories (Lude.Maybe CodeRepositorySortOrder)
lcrSortOrder = Lens.lens (sortOrder :: ListCodeRepositories -> Lude.Maybe CodeRepositorySortOrder) (\s a -> s {sortOrder = a} :: ListCodeRepositories)
{-# DEPRECATED lcrSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only Git repositories that were last modified after the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrLastModifiedTimeAfter :: Lens.Lens' ListCodeRepositories (Lude.Maybe Lude.Timestamp)
lcrLastModifiedTimeAfter = Lens.lens (lastModifiedTimeAfter :: ListCodeRepositories -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeAfter = a} :: ListCodeRepositories)
{-# DEPRECATED lcrLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only Git repositories that were created before the specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrCreationTimeBefore :: Lens.Lens' ListCodeRepositories (Lude.Maybe Lude.Timestamp)
lcrCreationTimeBefore = Lens.lens (creationTimeBefore :: ListCodeRepositories -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListCodeRepositories)
{-# DEPRECATED lcrCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | The maximum number of Git repositories to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrMaxResults :: Lens.Lens' ListCodeRepositories (Lude.Maybe Lude.Natural)
lcrMaxResults = Lens.lens (maxResults :: ListCodeRepositories -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListCodeRepositories)
{-# DEPRECATED lcrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The field to sort results by. The default is @Name@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrSortBy :: Lens.Lens' ListCodeRepositories (Lude.Maybe CodeRepositorySortBy)
lcrSortBy = Lens.lens (sortBy :: ListCodeRepositories -> Lude.Maybe CodeRepositorySortBy) (\s a -> s {sortBy = a} :: ListCodeRepositories)
{-# DEPRECATED lcrSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListCodeRepositories where
  page rq rs
    | Page.stop (rs Lens.^. lcrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcrrsCodeRepositorySummaryList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcrNextToken Lens..~ rs Lens.^. lcrrsNextToken

instance Lude.AWSRequest ListCodeRepositories where
  type Rs ListCodeRepositories = ListCodeRepositoriesResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCodeRepositoriesResponse'
            Lude.<$> (x Lude..?> "CodeRepositorySummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCodeRepositories where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListCodeRepositories" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListCodeRepositories where
  toJSON ListCodeRepositories' {..} =
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

instance Lude.ToPath ListCodeRepositories where
  toPath = Lude.const "/"

instance Lude.ToQuery ListCodeRepositories where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListCodeRepositoriesResponse' smart constructor.
data ListCodeRepositoriesResponse = ListCodeRepositoriesResponse'
  { -- | Gets a list of summaries of the Git repositories. Each summary specifies the following values for the repository:
    --
    --
    --     * Name
    --
    --
    --     * Amazon Resource Name (ARN)
    --
    --
    --     * Creation time
    --
    --
    --     * Last modified time
    --
    --
    --     * Configuration information, including the URL location of the repository and the ARN of the AWS Secrets Manager secret that contains the credentials used to access the repository.
    codeRepositorySummaryList :: [CodeRepositorySummary],
    -- | If the result of a @ListCodeRepositoriesOutput@ request was truncated, the response includes a @NextToken@ . To get the next set of Git repositories, use the token in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCodeRepositoriesResponse' with the minimum fields required to make a request.
--
-- * 'codeRepositorySummaryList' - Gets a list of summaries of the Git repositories. Each summary specifies the following values for the repository:
--
--
--     * Name
--
--
--     * Amazon Resource Name (ARN)
--
--
--     * Creation time
--
--
--     * Last modified time
--
--
--     * Configuration information, including the URL location of the repository and the ARN of the AWS Secrets Manager secret that contains the credentials used to access the repository.
--
--
-- * 'nextToken' - If the result of a @ListCodeRepositoriesOutput@ request was truncated, the response includes a @NextToken@ . To get the next set of Git repositories, use the token in the next request.
-- * 'responseStatus' - The response status code.
mkListCodeRepositoriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCodeRepositoriesResponse
mkListCodeRepositoriesResponse pResponseStatus_ =
  ListCodeRepositoriesResponse'
    { codeRepositorySummaryList =
        Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Gets a list of summaries of the Git repositories. Each summary specifies the following values for the repository:
--
--
--     * Name
--
--
--     * Amazon Resource Name (ARN)
--
--
--     * Creation time
--
--
--     * Last modified time
--
--
--     * Configuration information, including the URL location of the repository and the ARN of the AWS Secrets Manager secret that contains the credentials used to access the repository.
--
--
--
-- /Note:/ Consider using 'codeRepositorySummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsCodeRepositorySummaryList :: Lens.Lens' ListCodeRepositoriesResponse [CodeRepositorySummary]
lcrrsCodeRepositorySummaryList = Lens.lens (codeRepositorySummaryList :: ListCodeRepositoriesResponse -> [CodeRepositorySummary]) (\s a -> s {codeRepositorySummaryList = a} :: ListCodeRepositoriesResponse)
{-# DEPRECATED lcrrsCodeRepositorySummaryList "Use generic-lens or generic-optics with 'codeRepositorySummaryList' instead." #-}

-- | If the result of a @ListCodeRepositoriesOutput@ request was truncated, the response includes a @NextToken@ . To get the next set of Git repositories, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsNextToken :: Lens.Lens' ListCodeRepositoriesResponse (Lude.Maybe Lude.Text)
lcrrsNextToken = Lens.lens (nextToken :: ListCodeRepositoriesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCodeRepositoriesResponse)
{-# DEPRECATED lcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsResponseStatus :: Lens.Lens' ListCodeRepositoriesResponse Lude.Int
lcrrsResponseStatus = Lens.lens (responseStatus :: ListCodeRepositoriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCodeRepositoriesResponse)
{-# DEPRECATED lcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
