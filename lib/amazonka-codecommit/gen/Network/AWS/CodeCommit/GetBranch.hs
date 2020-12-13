{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetBranch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a repository branch, including its name and the last commit ID.
module Network.AWS.CodeCommit.GetBranch
  ( -- * Creating a request
    GetBranch (..),
    mkGetBranch,

    -- ** Request lenses
    gbBranchName,
    gbRepositoryName,

    -- * Destructuring the response
    GetBranchResponse (..),
    mkGetBranchResponse,

    -- ** Response lenses
    gbfrsBranch,
    gbfrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a get branch operation.
--
-- /See:/ 'mkGetBranch' smart constructor.
data GetBranch = GetBranch'
  { -- | The name of the branch for which you want to retrieve information.
    branchName :: Lude.Maybe Lude.Text,
    -- | The name of the repository that contains the branch for which you want to retrieve information.
    repositoryName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBranch' with the minimum fields required to make a request.
--
-- * 'branchName' - The name of the branch for which you want to retrieve information.
-- * 'repositoryName' - The name of the repository that contains the branch for which you want to retrieve information.
mkGetBranch ::
  GetBranch
mkGetBranch =
  GetBranch'
    { branchName = Lude.Nothing,
      repositoryName = Lude.Nothing
    }

-- | The name of the branch for which you want to retrieve information.
--
-- /Note:/ Consider using 'branchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbBranchName :: Lens.Lens' GetBranch (Lude.Maybe Lude.Text)
gbBranchName = Lens.lens (branchName :: GetBranch -> Lude.Maybe Lude.Text) (\s a -> s {branchName = a} :: GetBranch)
{-# DEPRECATED gbBranchName "Use generic-lens or generic-optics with 'branchName' instead." #-}

-- | The name of the repository that contains the branch for which you want to retrieve information.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbRepositoryName :: Lens.Lens' GetBranch (Lude.Maybe Lude.Text)
gbRepositoryName = Lens.lens (repositoryName :: GetBranch -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: GetBranch)
{-# DEPRECATED gbRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest GetBranch where
  type Rs GetBranch = GetBranchResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBranchResponse'
            Lude.<$> (x Lude..?> "branch") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBranch where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.GetBranch" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetBranch where
  toJSON GetBranch' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("branchName" Lude..=) Lude.<$> branchName,
            ("repositoryName" Lude..=) Lude.<$> repositoryName
          ]
      )

instance Lude.ToPath GetBranch where
  toPath = Lude.const "/"

instance Lude.ToQuery GetBranch where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a get branch operation.
--
-- /See:/ 'mkGetBranchResponse' smart constructor.
data GetBranchResponse = GetBranchResponse'
  { -- | The name of the branch.
    branch :: Lude.Maybe BranchInfo,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBranchResponse' with the minimum fields required to make a request.
--
-- * 'branch' - The name of the branch.
-- * 'responseStatus' - The response status code.
mkGetBranchResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBranchResponse
mkGetBranchResponse pResponseStatus_ =
  GetBranchResponse'
    { branch = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the branch.
--
-- /Note:/ Consider using 'branch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbfrsBranch :: Lens.Lens' GetBranchResponse (Lude.Maybe BranchInfo)
gbfrsBranch = Lens.lens (branch :: GetBranchResponse -> Lude.Maybe BranchInfo) (\s a -> s {branch = a} :: GetBranchResponse)
{-# DEPRECATED gbfrsBranch "Use generic-lens or generic-optics with 'branch' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbfrsResponseStatus :: Lens.Lens' GetBranchResponse Lude.Int
gbfrsResponseStatus = Lens.lens (responseStatus :: GetBranchResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBranchResponse)
{-# DEPRECATED gbfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
