{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.BatchGetCommits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the contents of one or more commits in a repository.
module Network.AWS.CodeCommit.BatchGetCommits
  ( -- * Creating a request
    BatchGetCommits (..),
    mkBatchGetCommits,

    -- ** Request lenses
    bgcCommitIds,
    bgcRepositoryName,

    -- * Destructuring the response
    BatchGetCommitsResponse (..),
    mkBatchGetCommitsResponse,

    -- ** Response lenses
    bgcrsCommits,
    bgcrsErrors,
    bgcrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetCommits' smart constructor.
data BatchGetCommits = BatchGetCommits'
  { -- | The full commit IDs of the commits to get information about.
    commitIds :: [Lude.Text],
    -- | The name of the repository that contains the commits.
    repositoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetCommits' with the minimum fields required to make a request.
--
-- * 'commitIds' - The full commit IDs of the commits to get information about.
-- * 'repositoryName' - The name of the repository that contains the commits.
mkBatchGetCommits ::
  -- | 'repositoryName'
  Lude.Text ->
  BatchGetCommits
mkBatchGetCommits pRepositoryName_ =
  BatchGetCommits'
    { commitIds = Lude.mempty,
      repositoryName = pRepositoryName_
    }

-- | The full commit IDs of the commits to get information about.
--
-- /Note:/ Consider using 'commitIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcCommitIds :: Lens.Lens' BatchGetCommits [Lude.Text]
bgcCommitIds = Lens.lens (commitIds :: BatchGetCommits -> [Lude.Text]) (\s a -> s {commitIds = a} :: BatchGetCommits)
{-# DEPRECATED bgcCommitIds "Use generic-lens or generic-optics with 'commitIds' instead." #-}

-- | The name of the repository that contains the commits.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcRepositoryName :: Lens.Lens' BatchGetCommits Lude.Text
bgcRepositoryName = Lens.lens (repositoryName :: BatchGetCommits -> Lude.Text) (\s a -> s {repositoryName = a} :: BatchGetCommits)
{-# DEPRECATED bgcRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest BatchGetCommits where
  type Rs BatchGetCommits = BatchGetCommitsResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetCommitsResponse'
            Lude.<$> (x Lude..?> "commits" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "errors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetCommits where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.BatchGetCommits" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetCommits where
  toJSON BatchGetCommits' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("commitIds" Lude..= commitIds),
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath BatchGetCommits where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetCommits where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetCommitsResponse' smart constructor.
data BatchGetCommitsResponse = BatchGetCommitsResponse'
  { -- | An array of commit data type objects, each of which contains information about a specified commit.
    commits :: Lude.Maybe [Commit],
    -- | Returns any commit IDs for which information could not be found. For example, if one of the commit IDs was a shortened SHA ID or that commit was not found in the specified repository, the ID returns an error object with more information.
    errors :: Lude.Maybe [BatchGetCommitsError],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetCommitsResponse' with the minimum fields required to make a request.
--
-- * 'commits' - An array of commit data type objects, each of which contains information about a specified commit.
-- * 'errors' - Returns any commit IDs for which information could not be found. For example, if one of the commit IDs was a shortened SHA ID or that commit was not found in the specified repository, the ID returns an error object with more information.
-- * 'responseStatus' - The response status code.
mkBatchGetCommitsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetCommitsResponse
mkBatchGetCommitsResponse pResponseStatus_ =
  BatchGetCommitsResponse'
    { commits = Lude.Nothing,
      errors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of commit data type objects, each of which contains information about a specified commit.
--
-- /Note:/ Consider using 'commits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcrsCommits :: Lens.Lens' BatchGetCommitsResponse (Lude.Maybe [Commit])
bgcrsCommits = Lens.lens (commits :: BatchGetCommitsResponse -> Lude.Maybe [Commit]) (\s a -> s {commits = a} :: BatchGetCommitsResponse)
{-# DEPRECATED bgcrsCommits "Use generic-lens or generic-optics with 'commits' instead." #-}

-- | Returns any commit IDs for which information could not be found. For example, if one of the commit IDs was a shortened SHA ID or that commit was not found in the specified repository, the ID returns an error object with more information.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcrsErrors :: Lens.Lens' BatchGetCommitsResponse (Lude.Maybe [BatchGetCommitsError])
bgcrsErrors = Lens.lens (errors :: BatchGetCommitsResponse -> Lude.Maybe [BatchGetCommitsError]) (\s a -> s {errors = a} :: BatchGetCommitsResponse)
{-# DEPRECATED bgcrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcrsResponseStatus :: Lens.Lens' BatchGetCommitsResponse Lude.Int
bgcrsResponseStatus = Lens.lens (responseStatus :: BatchGetCommitsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetCommitsResponse)
{-# DEPRECATED bgcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
