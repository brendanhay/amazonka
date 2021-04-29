{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an update default branch operation.
--
-- /See:/ 'newUpdateDefaultBranch' smart constructor.
data UpdateDefaultBranch = UpdateDefaultBranch'
  { -- | The name of the repository to set or change the default branch for.
    repositoryName :: Prelude.Text,
    -- | The name of the branch to set as the default.
    defaultBranchName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'defaultBranchName'
  Prelude.Text ->
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
updateDefaultBranch_repositoryName :: Lens.Lens' UpdateDefaultBranch Prelude.Text
updateDefaultBranch_repositoryName = Lens.lens (\UpdateDefaultBranch' {repositoryName} -> repositoryName) (\s@UpdateDefaultBranch' {} a -> s {repositoryName = a} :: UpdateDefaultBranch)

-- | The name of the branch to set as the default.
updateDefaultBranch_defaultBranchName :: Lens.Lens' UpdateDefaultBranch Prelude.Text
updateDefaultBranch_defaultBranchName = Lens.lens (\UpdateDefaultBranch' {defaultBranchName} -> defaultBranchName) (\s@UpdateDefaultBranch' {} a -> s {defaultBranchName = a} :: UpdateDefaultBranch)

instance Prelude.AWSRequest UpdateDefaultBranch where
  type
    Rs UpdateDefaultBranch =
      UpdateDefaultBranchResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateDefaultBranchResponse'

instance Prelude.Hashable UpdateDefaultBranch

instance Prelude.NFData UpdateDefaultBranch

instance Prelude.ToHeaders UpdateDefaultBranch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.UpdateDefaultBranch" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateDefaultBranch where
  toJSON UpdateDefaultBranch' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("repositoryName" Prelude..= repositoryName),
            Prelude.Just
              ("defaultBranchName" Prelude..= defaultBranchName)
          ]
      )

instance Prelude.ToPath UpdateDefaultBranch where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateDefaultBranch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDefaultBranchResponse' smart constructor.
data UpdateDefaultBranchResponse = UpdateDefaultBranchResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDefaultBranchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateDefaultBranchResponse ::
  UpdateDefaultBranchResponse
newUpdateDefaultBranchResponse =
  UpdateDefaultBranchResponse'

instance Prelude.NFData UpdateDefaultBranchResponse
