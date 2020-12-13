{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.BatchGetRepositories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more repositories.
module Network.AWS.CodeCommit.BatchGetRepositories
  ( -- * Creating a request
    BatchGetRepositories (..),
    mkBatchGetRepositories,

    -- ** Request lenses
    bgrRepositoryNames,

    -- * Destructuring the response
    BatchGetRepositoriesResponse (..),
    mkBatchGetRepositoriesResponse,

    -- ** Response lenses
    bgrrsRepositories,
    bgrrsRepositoriesNotFound,
    bgrrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a batch get repositories operation.
--
-- /See:/ 'mkBatchGetRepositories' smart constructor.
newtype BatchGetRepositories = BatchGetRepositories'
  { -- | The names of the repositories to get information about.
    repositoryNames :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetRepositories' with the minimum fields required to make a request.
--
-- * 'repositoryNames' - The names of the repositories to get information about.
mkBatchGetRepositories ::
  BatchGetRepositories
mkBatchGetRepositories =
  BatchGetRepositories' {repositoryNames = Lude.mempty}

-- | The names of the repositories to get information about.
--
-- /Note:/ Consider using 'repositoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrRepositoryNames :: Lens.Lens' BatchGetRepositories [Lude.Text]
bgrRepositoryNames = Lens.lens (repositoryNames :: BatchGetRepositories -> [Lude.Text]) (\s a -> s {repositoryNames = a} :: BatchGetRepositories)
{-# DEPRECATED bgrRepositoryNames "Use generic-lens or generic-optics with 'repositoryNames' instead." #-}

instance Lude.AWSRequest BatchGetRepositories where
  type Rs BatchGetRepositories = BatchGetRepositoriesResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetRepositoriesResponse'
            Lude.<$> (x Lude..?> "repositories" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "repositoriesNotFound" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetRepositories where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.BatchGetRepositories" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetRepositories where
  toJSON BatchGetRepositories' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("repositoryNames" Lude..= repositoryNames)]
      )

instance Lude.ToPath BatchGetRepositories where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetRepositories where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a batch get repositories operation.
--
-- /See:/ 'mkBatchGetRepositoriesResponse' smart constructor.
data BatchGetRepositoriesResponse = BatchGetRepositoriesResponse'
  { -- | A list of repositories returned by the batch get repositories operation.
    repositories :: Lude.Maybe [RepositoryMetadata],
    -- | Returns a list of repository names for which information could not be found.
    repositoriesNotFound :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetRepositoriesResponse' with the minimum fields required to make a request.
--
-- * 'repositories' - A list of repositories returned by the batch get repositories operation.
-- * 'repositoriesNotFound' - Returns a list of repository names for which information could not be found.
-- * 'responseStatus' - The response status code.
mkBatchGetRepositoriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetRepositoriesResponse
mkBatchGetRepositoriesResponse pResponseStatus_ =
  BatchGetRepositoriesResponse'
    { repositories = Lude.Nothing,
      repositoriesNotFound = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of repositories returned by the batch get repositories operation.
--
-- /Note:/ Consider using 'repositories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrrsRepositories :: Lens.Lens' BatchGetRepositoriesResponse (Lude.Maybe [RepositoryMetadata])
bgrrsRepositories = Lens.lens (repositories :: BatchGetRepositoriesResponse -> Lude.Maybe [RepositoryMetadata]) (\s a -> s {repositories = a} :: BatchGetRepositoriesResponse)
{-# DEPRECATED bgrrsRepositories "Use generic-lens or generic-optics with 'repositories' instead." #-}

-- | Returns a list of repository names for which information could not be found.
--
-- /Note:/ Consider using 'repositoriesNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrrsRepositoriesNotFound :: Lens.Lens' BatchGetRepositoriesResponse (Lude.Maybe [Lude.Text])
bgrrsRepositoriesNotFound = Lens.lens (repositoriesNotFound :: BatchGetRepositoriesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {repositoriesNotFound = a} :: BatchGetRepositoriesResponse)
{-# DEPRECATED bgrrsRepositoriesNotFound "Use generic-lens or generic-optics with 'repositoriesNotFound' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrrsResponseStatus :: Lens.Lens' BatchGetRepositoriesResponse Lude.Int
bgrrsResponseStatus = Lens.lens (responseStatus :: BatchGetRepositoriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetRepositoriesResponse)
{-# DEPRECATED bgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
