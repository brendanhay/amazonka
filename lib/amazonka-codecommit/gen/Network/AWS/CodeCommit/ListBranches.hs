{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.ListBranches
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more branches in a repository.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.ListBranches
  ( -- * Creating a request
    ListBranches (..),
    mkListBranches,

    -- ** Request lenses
    lbNextToken,
    lbRepositoryName,

    -- * Destructuring the response
    ListBranchesResponse (..),
    mkListBranchesResponse,

    -- ** Response lenses
    lbrsBranches,
    lbrsNextToken,
    lbrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a list branches operation.
--
-- /See:/ 'mkListBranches' smart constructor.
data ListBranches = ListBranches'
  { nextToken ::
      Lude.Maybe Lude.Text,
    repositoryName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBranches' with the minimum fields required to make a request.
--
-- * 'nextToken' - An enumeration token that allows the operation to batch the results.
-- * 'repositoryName' - The name of the repository that contains the branches.
mkListBranches ::
  -- | 'repositoryName'
  Lude.Text ->
  ListBranches
mkListBranches pRepositoryName_ =
  ListBranches'
    { nextToken = Lude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | An enumeration token that allows the operation to batch the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbNextToken :: Lens.Lens' ListBranches (Lude.Maybe Lude.Text)
lbNextToken = Lens.lens (nextToken :: ListBranches -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBranches)
{-# DEPRECATED lbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the repository that contains the branches.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbRepositoryName :: Lens.Lens' ListBranches Lude.Text
lbRepositoryName = Lens.lens (repositoryName :: ListBranches -> Lude.Text) (\s a -> s {repositoryName = a} :: ListBranches)
{-# DEPRECATED lbRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Page.AWSPager ListBranches where
  page rq rs
    | Page.stop (rs Lens.^. lbrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lbrsBranches) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lbNextToken Lens..~ rs Lens.^. lbrsNextToken

instance Lude.AWSRequest ListBranches where
  type Rs ListBranches = ListBranchesResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListBranchesResponse'
            Lude.<$> (x Lude..?> "branches" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBranches where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.ListBranches" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListBranches where
  toJSON ListBranches' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath ListBranches where
  toPath = Lude.const "/"

instance Lude.ToQuery ListBranches where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a list branches operation.
--
-- /See:/ 'mkListBranchesResponse' smart constructor.
data ListBranchesResponse = ListBranchesResponse'
  { branches ::
      Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'ListBranchesResponse' with the minimum fields required to make a request.
--
-- * 'branches' - The list of branch names.
-- * 'nextToken' - An enumeration token that returns the batch of the results.
-- * 'responseStatus' - The response status code.
mkListBranchesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBranchesResponse
mkListBranchesResponse pResponseStatus_ =
  ListBranchesResponse'
    { branches = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of branch names.
--
-- /Note:/ Consider using 'branches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsBranches :: Lens.Lens' ListBranchesResponse (Lude.Maybe [Lude.Text])
lbrsBranches = Lens.lens (branches :: ListBranchesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {branches = a} :: ListBranchesResponse)
{-# DEPRECATED lbrsBranches "Use generic-lens or generic-optics with 'branches' instead." #-}

-- | An enumeration token that returns the batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsNextToken :: Lens.Lens' ListBranchesResponse (Lude.Maybe Lude.Text)
lbrsNextToken = Lens.lens (nextToken :: ListBranchesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBranchesResponse)
{-# DEPRECATED lbrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsResponseStatus :: Lens.Lens' ListBranchesResponse Lude.Int
lbrsResponseStatus = Lens.lens (responseStatus :: ListBranchesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBranchesResponse)
{-# DEPRECATED lbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
