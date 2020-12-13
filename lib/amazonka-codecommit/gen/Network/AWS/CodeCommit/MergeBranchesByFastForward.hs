{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.MergeBranchesByFastForward
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two branches using the fast-forward merge strategy.
module Network.AWS.CodeCommit.MergeBranchesByFastForward
  ( -- * Creating a request
    MergeBranchesByFastForward (..),
    mkMergeBranchesByFastForward,

    -- ** Request lenses
    mbbffTargetBranch,
    mbbffRepositoryName,
    mbbffSourceCommitSpecifier,
    mbbffDestinationCommitSpecifier,

    -- * Destructuring the response
    MergeBranchesByFastForwardResponse (..),
    mkMergeBranchesByFastForwardResponse,

    -- ** Response lenses
    mbbffrsCommitId,
    mbbffrsTreeId,
    mbbffrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkMergeBranchesByFastForward' smart constructor.
data MergeBranchesByFastForward = MergeBranchesByFastForward'
  { -- | The branch where the merge is applied.
    targetBranch :: Lude.Maybe Lude.Text,
    -- | The name of the repository where you want to merge two branches.
    repositoryName :: Lude.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
    sourceCommitSpecifier :: Lude.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
    destinationCommitSpecifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergeBranchesByFastForward' with the minimum fields required to make a request.
--
-- * 'targetBranch' - The branch where the merge is applied.
-- * 'repositoryName' - The name of the repository where you want to merge two branches.
-- * 'sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
-- * 'destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
mkMergeBranchesByFastForward ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'sourceCommitSpecifier'
  Lude.Text ->
  -- | 'destinationCommitSpecifier'
  Lude.Text ->
  MergeBranchesByFastForward
mkMergeBranchesByFastForward
  pRepositoryName_
  pSourceCommitSpecifier_
  pDestinationCommitSpecifier_ =
    MergeBranchesByFastForward'
      { targetBranch = Lude.Nothing,
        repositoryName = pRepositoryName_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        destinationCommitSpecifier = pDestinationCommitSpecifier_
      }

-- | The branch where the merge is applied.
--
-- /Note:/ Consider using 'targetBranch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbffTargetBranch :: Lens.Lens' MergeBranchesByFastForward (Lude.Maybe Lude.Text)
mbbffTargetBranch = Lens.lens (targetBranch :: MergeBranchesByFastForward -> Lude.Maybe Lude.Text) (\s a -> s {targetBranch = a} :: MergeBranchesByFastForward)
{-# DEPRECATED mbbffTargetBranch "Use generic-lens or generic-optics with 'targetBranch' instead." #-}

-- | The name of the repository where you want to merge two branches.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbffRepositoryName :: Lens.Lens' MergeBranchesByFastForward Lude.Text
mbbffRepositoryName = Lens.lens (repositoryName :: MergeBranchesByFastForward -> Lude.Text) (\s a -> s {repositoryName = a} :: MergeBranchesByFastForward)
{-# DEPRECATED mbbffRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'sourceCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbffSourceCommitSpecifier :: Lens.Lens' MergeBranchesByFastForward Lude.Text
mbbffSourceCommitSpecifier = Lens.lens (sourceCommitSpecifier :: MergeBranchesByFastForward -> Lude.Text) (\s a -> s {sourceCommitSpecifier = a} :: MergeBranchesByFastForward)
{-# DEPRECATED mbbffSourceCommitSpecifier "Use generic-lens or generic-optics with 'sourceCommitSpecifier' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, a branch name or a full commit ID).
--
-- /Note:/ Consider using 'destinationCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbffDestinationCommitSpecifier :: Lens.Lens' MergeBranchesByFastForward Lude.Text
mbbffDestinationCommitSpecifier = Lens.lens (destinationCommitSpecifier :: MergeBranchesByFastForward -> Lude.Text) (\s a -> s {destinationCommitSpecifier = a} :: MergeBranchesByFastForward)
{-# DEPRECATED mbbffDestinationCommitSpecifier "Use generic-lens or generic-optics with 'destinationCommitSpecifier' instead." #-}

instance Lude.AWSRequest MergeBranchesByFastForward where
  type
    Rs MergeBranchesByFastForward =
      MergeBranchesByFastForwardResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          MergeBranchesByFastForwardResponse'
            Lude.<$> (x Lude..?> "commitId")
            Lude.<*> (x Lude..?> "treeId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders MergeBranchesByFastForward where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.MergeBranchesByFastForward" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON MergeBranchesByFastForward where
  toJSON MergeBranchesByFastForward' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("targetBranch" Lude..=) Lude.<$> targetBranch,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("sourceCommitSpecifier" Lude..= sourceCommitSpecifier),
            Lude.Just
              ("destinationCommitSpecifier" Lude..= destinationCommitSpecifier)
          ]
      )

instance Lude.ToPath MergeBranchesByFastForward where
  toPath = Lude.const "/"

instance Lude.ToQuery MergeBranchesByFastForward where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkMergeBranchesByFastForwardResponse' smart constructor.
data MergeBranchesByFastForwardResponse = MergeBranchesByFastForwardResponse'
  { -- | The commit ID of the merge in the destination or target branch.
    commitId :: Lude.Maybe Lude.Text,
    -- | The tree ID of the merge in the destination or target branch.
    treeId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergeBranchesByFastForwardResponse' with the minimum fields required to make a request.
--
-- * 'commitId' - The commit ID of the merge in the destination or target branch.
-- * 'treeId' - The tree ID of the merge in the destination or target branch.
-- * 'responseStatus' - The response status code.
mkMergeBranchesByFastForwardResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  MergeBranchesByFastForwardResponse
mkMergeBranchesByFastForwardResponse pResponseStatus_ =
  MergeBranchesByFastForwardResponse'
    { commitId = Lude.Nothing,
      treeId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The commit ID of the merge in the destination or target branch.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbffrsCommitId :: Lens.Lens' MergeBranchesByFastForwardResponse (Lude.Maybe Lude.Text)
mbbffrsCommitId = Lens.lens (commitId :: MergeBranchesByFastForwardResponse -> Lude.Maybe Lude.Text) (\s a -> s {commitId = a} :: MergeBranchesByFastForwardResponse)
{-# DEPRECATED mbbffrsCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The tree ID of the merge in the destination or target branch.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbffrsTreeId :: Lens.Lens' MergeBranchesByFastForwardResponse (Lude.Maybe Lude.Text)
mbbffrsTreeId = Lens.lens (treeId :: MergeBranchesByFastForwardResponse -> Lude.Maybe Lude.Text) (\s a -> s {treeId = a} :: MergeBranchesByFastForwardResponse)
{-# DEPRECATED mbbffrsTreeId "Use generic-lens or generic-optics with 'treeId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbbffrsResponseStatus :: Lens.Lens' MergeBranchesByFastForwardResponse Lude.Int
mbbffrsResponseStatus = Lens.lens (responseStatus :: MergeBranchesByFastForwardResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: MergeBranchesByFastForwardResponse)
{-# DEPRECATED mbbffrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
