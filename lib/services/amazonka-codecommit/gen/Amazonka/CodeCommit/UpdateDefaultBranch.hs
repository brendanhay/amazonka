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
-- Module      : Amazonka.CodeCommit.UpdateDefaultBranch
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CodeCommit.UpdateDefaultBranch
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

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of an update default branch operation.
--
-- /See:/ 'newUpdateDefaultBranch' smart constructor.
data UpdateDefaultBranch = UpdateDefaultBranch'
  { -- | The name of the repository to set or change the default branch for.
    repositoryName :: Prelude.Text,
    -- | The name of the branch to set as the default.
    defaultBranchName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest UpdateDefaultBranch where
  type
    AWSResponse UpdateDefaultBranch =
      UpdateDefaultBranchResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateDefaultBranchResponse'

instance Prelude.Hashable UpdateDefaultBranch where
  hashWithSalt _salt UpdateDefaultBranch' {..} =
    _salt `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` defaultBranchName

instance Prelude.NFData UpdateDefaultBranch where
  rnf UpdateDefaultBranch' {..} =
    Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf defaultBranchName

instance Data.ToHeaders UpdateDefaultBranch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.UpdateDefaultBranch" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDefaultBranch where
  toJSON UpdateDefaultBranch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just
              ("defaultBranchName" Data..= defaultBranchName)
          ]
      )

instance Data.ToPath UpdateDefaultBranch where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDefaultBranch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDefaultBranchResponse' smart constructor.
data UpdateDefaultBranchResponse = UpdateDefaultBranchResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDefaultBranchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateDefaultBranchResponse ::
  UpdateDefaultBranchResponse
newUpdateDefaultBranchResponse =
  UpdateDefaultBranchResponse'

instance Prelude.NFData UpdateDefaultBranchResponse where
  rnf _ = ()
