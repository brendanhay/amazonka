{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateDefaultBranch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets or changes the default branch name for the specified repository.
--
-- If you use this operation to change the default branch name to the
-- current default branch name, a success message is returned even though
-- the default branch did not change.
module Network.AWS.CodeCommit.UpdateDefaultBranch
  ( -- * Creating a Request
    UpdateDefaultBranch (..),
    newUpdateDefaultBranch,

    -- * Request Lenses
    updateDefaultBranch_repositoryName,
    updateDefaultBranch_defaultBranchName,

    -- * Destructuring the Response
    UpdateDefaultBranchResponse (..),
    newUpdateDefaultBranchResponse,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an update default branch operation.
--
-- /See:/ 'newUpdateDefaultBranch' smart constructor.
data UpdateDefaultBranch = UpdateDefaultBranch'
  { -- | The name of the repository to set or change the default branch for.
    repositoryName :: Core.Text,
    -- | The name of the branch to set as the default.
    defaultBranchName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDefaultBranch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryName', 'updateDefaultBranch_repositoryName' - The name of the repository to set or change the default branch for.
--
-- 'defaultBranchName', 'updateDefaultBranch_defaultBranchName' - The name of the branch to set as the default.
newUpdateDefaultBranch ::
  -- | 'repositoryName'
  Core.Text ->
  -- | 'defaultBranchName'
  Core.Text ->
  UpdateDefaultBranch
newUpdateDefaultBranch
  pRepositoryName_
  pDefaultBranchName_ =
    UpdateDefaultBranch'
      { repositoryName =
          pRepositoryName_,
        defaultBranchName = pDefaultBranchName_
      }

-- | The name of the repository to set or change the default branch for.
updateDefaultBranch_repositoryName :: Lens.Lens' UpdateDefaultBranch Core.Text
updateDefaultBranch_repositoryName = Lens.lens (\UpdateDefaultBranch' {repositoryName} -> repositoryName) (\s@UpdateDefaultBranch' {} a -> s {repositoryName = a} :: UpdateDefaultBranch)

-- | The name of the branch to set as the default.
updateDefaultBranch_defaultBranchName :: Lens.Lens' UpdateDefaultBranch Core.Text
updateDefaultBranch_defaultBranchName = Lens.lens (\UpdateDefaultBranch' {defaultBranchName} -> defaultBranchName) (\s@UpdateDefaultBranch' {} a -> s {defaultBranchName = a} :: UpdateDefaultBranch)

instance Core.AWSRequest UpdateDefaultBranch where
  type
    AWSResponse UpdateDefaultBranch =
      UpdateDefaultBranchResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateDefaultBranchResponse'

instance Core.Hashable UpdateDefaultBranch

instance Core.NFData UpdateDefaultBranch

instance Core.ToHeaders UpdateDefaultBranch where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.UpdateDefaultBranch" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateDefaultBranch where
  toJSON UpdateDefaultBranch' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just
              ("defaultBranchName" Core..= defaultBranchName)
          ]
      )

instance Core.ToPath UpdateDefaultBranch where
  toPath = Core.const "/"

instance Core.ToQuery UpdateDefaultBranch where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDefaultBranchResponse' smart constructor.
data UpdateDefaultBranchResponse = UpdateDefaultBranchResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDefaultBranchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateDefaultBranchResponse ::
  UpdateDefaultBranchResponse
newUpdateDefaultBranchResponse =
  UpdateDefaultBranchResponse'

instance Core.NFData UpdateDefaultBranchResponse
