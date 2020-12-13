{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.CreateBranch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a branch in a repository and points the branch to a commit.
module Network.AWS.CodeCommit.CreateBranch
  ( -- * Creating a request
    CreateBranch (..),
    mkCreateBranch,

    -- ** Request lenses
    cbCommitId,
    cbBranchName,
    cbRepositoryName,

    -- * Destructuring the response
    CreateBranchResponse (..),
    mkCreateBranchResponse,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a create branch operation.
--
-- /See:/ 'mkCreateBranch' smart constructor.
data CreateBranch = CreateBranch'
  { -- | The ID of the commit to point the new branch to.
    commitId :: Lude.Text,
    -- | The name of the new branch to create.
    branchName :: Lude.Text,
    -- | The name of the repository in which you want to create the new branch.
    repositoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBranch' with the minimum fields required to make a request.
--
-- * 'commitId' - The ID of the commit to point the new branch to.
-- * 'branchName' - The name of the new branch to create.
-- * 'repositoryName' - The name of the repository in which you want to create the new branch.
mkCreateBranch ::
  -- | 'commitId'
  Lude.Text ->
  -- | 'branchName'
  Lude.Text ->
  -- | 'repositoryName'
  Lude.Text ->
  CreateBranch
mkCreateBranch pCommitId_ pBranchName_ pRepositoryName_ =
  CreateBranch'
    { commitId = pCommitId_,
      branchName = pBranchName_,
      repositoryName = pRepositoryName_
    }

-- | The ID of the commit to point the new branch to.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbCommitId :: Lens.Lens' CreateBranch Lude.Text
cbCommitId = Lens.lens (commitId :: CreateBranch -> Lude.Text) (\s a -> s {commitId = a} :: CreateBranch)
{-# DEPRECATED cbCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The name of the new branch to create.
--
-- /Note:/ Consider using 'branchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbBranchName :: Lens.Lens' CreateBranch Lude.Text
cbBranchName = Lens.lens (branchName :: CreateBranch -> Lude.Text) (\s a -> s {branchName = a} :: CreateBranch)
{-# DEPRECATED cbBranchName "Use generic-lens or generic-optics with 'branchName' instead." #-}

-- | The name of the repository in which you want to create the new branch.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbRepositoryName :: Lens.Lens' CreateBranch Lude.Text
cbRepositoryName = Lens.lens (repositoryName :: CreateBranch -> Lude.Text) (\s a -> s {repositoryName = a} :: CreateBranch)
{-# DEPRECATED cbRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest CreateBranch where
  type Rs CreateBranch = CreateBranchResponse
  request = Req.postJSON codeCommitService
  response = Res.receiveNull CreateBranchResponse'

instance Lude.ToHeaders CreateBranch where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.CreateBranch" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateBranch where
  toJSON CreateBranch' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("commitId" Lude..= commitId),
            Lude.Just ("branchName" Lude..= branchName),
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath CreateBranch where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateBranch where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateBranchResponse' smart constructor.
data CreateBranchResponse = CreateBranchResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBranchResponse' with the minimum fields required to make a request.
mkCreateBranchResponse ::
  CreateBranchResponse
mkCreateBranchResponse = CreateBranchResponse'
