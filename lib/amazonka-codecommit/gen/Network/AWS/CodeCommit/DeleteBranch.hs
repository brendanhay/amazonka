{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.DeleteBranch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a branch from a repository, unless that branch is the default branch for the repository.
module Network.AWS.CodeCommit.DeleteBranch
  ( -- * Creating a request
    DeleteBranch (..),
    mkDeleteBranch,

    -- ** Request lenses
    dbRepositoryName,
    dbBranchName,

    -- * Destructuring the response
    DeleteBranchResponse (..),
    mkDeleteBranchResponse,

    -- ** Response lenses
    dbrsDeletedBranch,
    dbrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a delete branch operation.
--
-- /See:/ 'mkDeleteBranch' smart constructor.
data DeleteBranch = DeleteBranch'
  { repositoryName :: Lude.Text,
    branchName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBranch' with the minimum fields required to make a request.
--
-- * 'branchName' - The name of the branch to delete.
-- * 'repositoryName' - The name of the repository that contains the branch to be deleted.
mkDeleteBranch ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'branchName'
  Lude.Text ->
  DeleteBranch
mkDeleteBranch pRepositoryName_ pBranchName_ =
  DeleteBranch'
    { repositoryName = pRepositoryName_,
      branchName = pBranchName_
    }

-- | The name of the repository that contains the branch to be deleted.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbRepositoryName :: Lens.Lens' DeleteBranch Lude.Text
dbRepositoryName = Lens.lens (repositoryName :: DeleteBranch -> Lude.Text) (\s a -> s {repositoryName = a} :: DeleteBranch)
{-# DEPRECATED dbRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The name of the branch to delete.
--
-- /Note:/ Consider using 'branchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBranchName :: Lens.Lens' DeleteBranch Lude.Text
dbBranchName = Lens.lens (branchName :: DeleteBranch -> Lude.Text) (\s a -> s {branchName = a} :: DeleteBranch)
{-# DEPRECATED dbBranchName "Use generic-lens or generic-optics with 'branchName' instead." #-}

instance Lude.AWSRequest DeleteBranch where
  type Rs DeleteBranch = DeleteBranchResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteBranchResponse'
            Lude.<$> (x Lude..?> "deletedBranch")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteBranch where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.DeleteBranch" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteBranch where
  toJSON DeleteBranch' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("branchName" Lude..= branchName)
          ]
      )

instance Lude.ToPath DeleteBranch where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteBranch where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a delete branch operation.
--
-- /See:/ 'mkDeleteBranchResponse' smart constructor.
data DeleteBranchResponse = DeleteBranchResponse'
  { deletedBranch ::
      Lude.Maybe BranchInfo,
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

-- | Creates a value of 'DeleteBranchResponse' with the minimum fields required to make a request.
--
-- * 'deletedBranch' - Information about the branch deleted by the operation, including the branch name and the commit ID that was the tip of the branch.
-- * 'responseStatus' - The response status code.
mkDeleteBranchResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteBranchResponse
mkDeleteBranchResponse pResponseStatus_ =
  DeleteBranchResponse'
    { deletedBranch = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the branch deleted by the operation, including the branch name and the commit ID that was the tip of the branch.
--
-- /Note:/ Consider using 'deletedBranch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsDeletedBranch :: Lens.Lens' DeleteBranchResponse (Lude.Maybe BranchInfo)
dbrsDeletedBranch = Lens.lens (deletedBranch :: DeleteBranchResponse -> Lude.Maybe BranchInfo) (\s a -> s {deletedBranch = a} :: DeleteBranchResponse)
{-# DEPRECATED dbrsDeletedBranch "Use generic-lens or generic-optics with 'deletedBranch' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsResponseStatus :: Lens.Lens' DeleteBranchResponse Lude.Int
dbrsResponseStatus = Lens.lens (responseStatus :: DeleteBranchResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteBranchResponse)
{-# DEPRECATED dbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
