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
-- Module      : Amazonka.CodeCommit.CreateBranch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a branch in a repository and points the branch to a commit.
--
-- Calling the create branch operation does not set a repository\'s default
-- branch. To do this, call the update default branch operation.
module Amazonka.CodeCommit.CreateBranch
  ( -- * Creating a Request
    CreateBranch (..),
    newCreateBranch,

    -- * Request Lenses
    createBranch_repositoryName,
    createBranch_branchName,
    createBranch_commitId,

    -- * Destructuring the Response
    CreateBranchResponse (..),
    newCreateBranchResponse,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a create branch operation.
--
-- /See:/ 'newCreateBranch' smart constructor.
data CreateBranch = CreateBranch'
  { -- | The name of the repository in which you want to create the new branch.
    repositoryName :: Prelude.Text,
    -- | The name of the new branch to create.
    branchName :: Prelude.Text,
    -- | The ID of the commit to point the new branch to.
    commitId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBranch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryName', 'createBranch_repositoryName' - The name of the repository in which you want to create the new branch.
--
-- 'branchName', 'createBranch_branchName' - The name of the new branch to create.
--
-- 'commitId', 'createBranch_commitId' - The ID of the commit to point the new branch to.
newCreateBranch ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'branchName'
  Prelude.Text ->
  -- | 'commitId'
  Prelude.Text ->
  CreateBranch
newCreateBranch
  pRepositoryName_
  pBranchName_
  pCommitId_ =
    CreateBranch'
      { repositoryName = pRepositoryName_,
        branchName = pBranchName_,
        commitId = pCommitId_
      }

-- | The name of the repository in which you want to create the new branch.
createBranch_repositoryName :: Lens.Lens' CreateBranch Prelude.Text
createBranch_repositoryName = Lens.lens (\CreateBranch' {repositoryName} -> repositoryName) (\s@CreateBranch' {} a -> s {repositoryName = a} :: CreateBranch)

-- | The name of the new branch to create.
createBranch_branchName :: Lens.Lens' CreateBranch Prelude.Text
createBranch_branchName = Lens.lens (\CreateBranch' {branchName} -> branchName) (\s@CreateBranch' {} a -> s {branchName = a} :: CreateBranch)

-- | The ID of the commit to point the new branch to.
createBranch_commitId :: Lens.Lens' CreateBranch Prelude.Text
createBranch_commitId = Lens.lens (\CreateBranch' {commitId} -> commitId) (\s@CreateBranch' {} a -> s {commitId = a} :: CreateBranch)

instance Core.AWSRequest CreateBranch where
  type AWSResponse CreateBranch = CreateBranchResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull CreateBranchResponse'

instance Prelude.Hashable CreateBranch where
  hashWithSalt _salt CreateBranch' {..} =
    _salt
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` branchName
      `Prelude.hashWithSalt` commitId

instance Prelude.NFData CreateBranch where
  rnf CreateBranch' {..} =
    Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf branchName
      `Prelude.seq` Prelude.rnf commitId

instance Data.ToHeaders CreateBranch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.CreateBranch" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBranch where
  toJSON CreateBranch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just ("branchName" Data..= branchName),
            Prelude.Just ("commitId" Data..= commitId)
          ]
      )

instance Data.ToPath CreateBranch where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateBranch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBranchResponse' smart constructor.
data CreateBranchResponse = CreateBranchResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBranchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateBranchResponse ::
  CreateBranchResponse
newCreateBranchResponse = CreateBranchResponse'

instance Prelude.NFData CreateBranchResponse where
  rnf _ = ()
