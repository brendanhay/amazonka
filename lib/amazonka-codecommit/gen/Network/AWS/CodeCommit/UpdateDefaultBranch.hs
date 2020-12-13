{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateDefaultBranch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets or changes the default branch name for the specified repository.
module Network.AWS.CodeCommit.UpdateDefaultBranch
  ( -- * Creating a request
    UpdateDefaultBranch (..),
    mkUpdateDefaultBranch,

    -- ** Request lenses
    udbDefaultBranchName,
    udbRepositoryName,

    -- * Destructuring the response
    UpdateDefaultBranchResponse (..),
    mkUpdateDefaultBranchResponse,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of an update default branch operation.
--
-- /See:/ 'mkUpdateDefaultBranch' smart constructor.
data UpdateDefaultBranch = UpdateDefaultBranch'
  { -- | The name of the branch to set as the default.
    defaultBranchName :: Lude.Text,
    -- | The name of the repository to set or change the default branch for.
    repositoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDefaultBranch' with the minimum fields required to make a request.
--
-- * 'defaultBranchName' - The name of the branch to set as the default.
-- * 'repositoryName' - The name of the repository to set or change the default branch for.
mkUpdateDefaultBranch ::
  -- | 'defaultBranchName'
  Lude.Text ->
  -- | 'repositoryName'
  Lude.Text ->
  UpdateDefaultBranch
mkUpdateDefaultBranch pDefaultBranchName_ pRepositoryName_ =
  UpdateDefaultBranch'
    { defaultBranchName = pDefaultBranchName_,
      repositoryName = pRepositoryName_
    }

-- | The name of the branch to set as the default.
--
-- /Note:/ Consider using 'defaultBranchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udbDefaultBranchName :: Lens.Lens' UpdateDefaultBranch Lude.Text
udbDefaultBranchName = Lens.lens (defaultBranchName :: UpdateDefaultBranch -> Lude.Text) (\s a -> s {defaultBranchName = a} :: UpdateDefaultBranch)
{-# DEPRECATED udbDefaultBranchName "Use generic-lens or generic-optics with 'defaultBranchName' instead." #-}

-- | The name of the repository to set or change the default branch for.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udbRepositoryName :: Lens.Lens' UpdateDefaultBranch Lude.Text
udbRepositoryName = Lens.lens (repositoryName :: UpdateDefaultBranch -> Lude.Text) (\s a -> s {repositoryName = a} :: UpdateDefaultBranch)
{-# DEPRECATED udbRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest UpdateDefaultBranch where
  type Rs UpdateDefaultBranch = UpdateDefaultBranchResponse
  request = Req.postJSON codeCommitService
  response = Res.receiveNull UpdateDefaultBranchResponse'

instance Lude.ToHeaders UpdateDefaultBranch where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.UpdateDefaultBranch" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDefaultBranch where
  toJSON UpdateDefaultBranch' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("defaultBranchName" Lude..= defaultBranchName),
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath UpdateDefaultBranch where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDefaultBranch where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDefaultBranchResponse' smart constructor.
data UpdateDefaultBranchResponse = UpdateDefaultBranchResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDefaultBranchResponse' with the minimum fields required to make a request.
mkUpdateDefaultBranchResponse ::
  UpdateDefaultBranchResponse
mkUpdateDefaultBranchResponse = UpdateDefaultBranchResponse'
