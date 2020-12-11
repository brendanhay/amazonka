{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetCommit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a commit, including commit message and committer information.
module Network.AWS.CodeCommit.GetCommit
  ( -- * Creating a request
    GetCommit (..),
    mkGetCommit,

    -- ** Request lenses
    gcRepositoryName,
    gcCommitId,

    -- * Destructuring the response
    GetCommitResponse (..),
    mkGetCommitResponse,

    -- ** Response lenses
    gcrsResponseStatus,
    gcrsCommit,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a get commit operation.
--
-- /See:/ 'mkGetCommit' smart constructor.
data GetCommit = GetCommit'
  { repositoryName :: Lude.Text,
    commitId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCommit' with the minimum fields required to make a request.
--
-- * 'commitId' - The commit ID. Commit IDs are the full SHA ID of the commit.
-- * 'repositoryName' - The name of the repository to which the commit was made.
mkGetCommit ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'commitId'
  Lude.Text ->
  GetCommit
mkGetCommit pRepositoryName_ pCommitId_ =
  GetCommit'
    { repositoryName = pRepositoryName_,
      commitId = pCommitId_
    }

-- | The name of the repository to which the commit was made.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcRepositoryName :: Lens.Lens' GetCommit Lude.Text
gcRepositoryName = Lens.lens (repositoryName :: GetCommit -> Lude.Text) (\s a -> s {repositoryName = a} :: GetCommit)
{-# DEPRECATED gcRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The commit ID. Commit IDs are the full SHA ID of the commit.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCommitId :: Lens.Lens' GetCommit Lude.Text
gcCommitId = Lens.lens (commitId :: GetCommit -> Lude.Text) (\s a -> s {commitId = a} :: GetCommit)
{-# DEPRECATED gcCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

instance Lude.AWSRequest GetCommit where
  type Rs GetCommit = GetCommitResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCommitResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "commit")
      )

instance Lude.ToHeaders GetCommit where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.GetCommit" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCommit where
  toJSON GetCommit' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("commitId" Lude..= commitId)
          ]
      )

instance Lude.ToPath GetCommit where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCommit where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a get commit operation.
--
-- /See:/ 'mkGetCommitResponse' smart constructor.
data GetCommitResponse = GetCommitResponse'
  { responseStatus ::
      Lude.Int,
    commit :: Commit
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCommitResponse' with the minimum fields required to make a request.
--
-- * 'commit' - A commit data type object that contains information about the specified commit.
-- * 'responseStatus' - The response status code.
mkGetCommitResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'commit'
  Commit ->
  GetCommitResponse
mkGetCommitResponse pResponseStatus_ pCommit_ =
  GetCommitResponse'
    { responseStatus = pResponseStatus_,
      commit = pCommit_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsResponseStatus :: Lens.Lens' GetCommitResponse Lude.Int
gcrsResponseStatus = Lens.lens (responseStatus :: GetCommitResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCommitResponse)
{-# DEPRECATED gcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A commit data type object that contains information about the specified commit.
--
-- /Note:/ Consider using 'commit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsCommit :: Lens.Lens' GetCommitResponse Commit
gcrsCommit = Lens.lens (commit :: GetCommitResponse -> Commit) (\s a -> s {commit = a} :: GetCommitResponse)
{-# DEPRECATED gcrsCommit "Use generic-lens or generic-optics with 'commit' instead." #-}
