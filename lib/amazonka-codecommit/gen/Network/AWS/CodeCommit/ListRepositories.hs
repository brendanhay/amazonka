{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.ListRepositories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more repositories.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.ListRepositories
  ( -- * Creating a request
    ListRepositories (..),
    mkListRepositories,

    -- ** Request lenses
    lrNextToken,
    lrOrder,
    lrSortBy,

    -- * Destructuring the response
    ListRepositoriesResponse (..),
    mkListRepositoriesResponse,

    -- ** Response lenses
    lrrsRepositories,
    lrrsNextToken,
    lrrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a list repositories operation.
--
-- /See:/ 'mkListRepositories' smart constructor.
data ListRepositories = ListRepositories'
  { nextToken ::
      Lude.Maybe Lude.Text,
    order :: Lude.Maybe OrderEnum,
    sortBy :: Lude.Maybe SortByEnum
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRepositories' with the minimum fields required to make a request.
--
-- * 'nextToken' - An enumeration token that allows the operation to batch the results of the operation. Batch sizes are 1,000 for list repository operations. When the client sends the token back to AWS CodeCommit, another page of 1,000 records is retrieved.
-- * 'order' - The order in which to sort the results of a list repositories operation.
-- * 'sortBy' - The criteria used to sort the results of a list repositories operation.
mkListRepositories ::
  ListRepositories
mkListRepositories =
  ListRepositories'
    { nextToken = Lude.Nothing,
      order = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | An enumeration token that allows the operation to batch the results of the operation. Batch sizes are 1,000 for list repository operations. When the client sends the token back to AWS CodeCommit, another page of 1,000 records is retrieved.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListRepositories (Lude.Maybe Lude.Text)
lrNextToken = Lens.lens (nextToken :: ListRepositories -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRepositories)
{-# DEPRECATED lrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The order in which to sort the results of a list repositories operation.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrOrder :: Lens.Lens' ListRepositories (Lude.Maybe OrderEnum)
lrOrder = Lens.lens (order :: ListRepositories -> Lude.Maybe OrderEnum) (\s a -> s {order = a} :: ListRepositories)
{-# DEPRECATED lrOrder "Use generic-lens or generic-optics with 'order' instead." #-}

-- | The criteria used to sort the results of a list repositories operation.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrSortBy :: Lens.Lens' ListRepositories (Lude.Maybe SortByEnum)
lrSortBy = Lens.lens (sortBy :: ListRepositories -> Lude.Maybe SortByEnum) (\s a -> s {sortBy = a} :: ListRepositories)
{-# DEPRECATED lrSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListRepositories where
  page rq rs
    | Page.stop (rs Lens.^. lrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrrsRepositories) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrNextToken Lens..~ rs Lens.^. lrrsNextToken

instance Lude.AWSRequest ListRepositories where
  type Rs ListRepositories = ListRepositoriesResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRepositoriesResponse'
            Lude.<$> (x Lude..?> "repositories" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRepositories where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.ListRepositories" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListRepositories where
  toJSON ListRepositories' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("order" Lude..=) Lude.<$> order,
            ("sortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListRepositories where
  toPath = Lude.const "/"

instance Lude.ToQuery ListRepositories where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a list repositories operation.
--
-- /See:/ 'mkListRepositoriesResponse' smart constructor.
data ListRepositoriesResponse = ListRepositoriesResponse'
  { repositories ::
      Lude.Maybe [RepositoryNameIdPair],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListRepositoriesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An enumeration token that allows the operation to batch the results of the operation. Batch sizes are 1,000 for list repository operations. When the client sends the token back to AWS CodeCommit, another page of 1,000 records is retrieved.
-- * 'repositories' - Lists the repositories called by the list repositories operation.
-- * 'responseStatus' - The response status code.
mkListRepositoriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRepositoriesResponse
mkListRepositoriesResponse pResponseStatus_ =
  ListRepositoriesResponse'
    { repositories = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Lists the repositories called by the list repositories operation.
--
-- /Note:/ Consider using 'repositories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsRepositories :: Lens.Lens' ListRepositoriesResponse (Lude.Maybe [RepositoryNameIdPair])
lrrsRepositories = Lens.lens (repositories :: ListRepositoriesResponse -> Lude.Maybe [RepositoryNameIdPair]) (\s a -> s {repositories = a} :: ListRepositoriesResponse)
{-# DEPRECATED lrrsRepositories "Use generic-lens or generic-optics with 'repositories' instead." #-}

-- | An enumeration token that allows the operation to batch the results of the operation. Batch sizes are 1,000 for list repository operations. When the client sends the token back to AWS CodeCommit, another page of 1,000 records is retrieved.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsNextToken :: Lens.Lens' ListRepositoriesResponse (Lude.Maybe Lude.Text)
lrrsNextToken = Lens.lens (nextToken :: ListRepositoriesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRepositoriesResponse)
{-# DEPRECATED lrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsResponseStatus :: Lens.Lens' ListRepositoriesResponse Lude.Int
lrrsResponseStatus = Lens.lens (responseStatus :: ListRepositoriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRepositoriesResponse)
{-# DEPRECATED lrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
