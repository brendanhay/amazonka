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
-- Module      : Network.AWS.CodeCommit.CreateBranch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a branch in a repository and points the branch to a commit.
--
-- Calling the create branch operation does not set a repository\'s default
-- branch. To do this, call the update default branch operation.
module Network.AWS.CodeCommit.CreateBranch
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

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a create branch operation.
--
-- /See:/ 'newCreateBranch' smart constructor.
data CreateBranch = CreateBranch'
  { -- | The name of the repository in which you want to create the new branch.
    repositoryName :: Core.Text,
    -- | The name of the new branch to create.
    branchName :: Core.Text,
    -- | The ID of the commit to point the new branch to.
    commitId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'branchName'
  Core.Text ->
  -- | 'commitId'
  Core.Text ->
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
createBranch_repositoryName :: Lens.Lens' CreateBranch Core.Text
createBranch_repositoryName = Lens.lens (\CreateBranch' {repositoryName} -> repositoryName) (\s@CreateBranch' {} a -> s {repositoryName = a} :: CreateBranch)

-- | The name of the new branch to create.
createBranch_branchName :: Lens.Lens' CreateBranch Core.Text
createBranch_branchName = Lens.lens (\CreateBranch' {branchName} -> branchName) (\s@CreateBranch' {} a -> s {branchName = a} :: CreateBranch)

-- | The ID of the commit to point the new branch to.
createBranch_commitId :: Lens.Lens' CreateBranch Core.Text
createBranch_commitId = Lens.lens (\CreateBranch' {commitId} -> commitId) (\s@CreateBranch' {} a -> s {commitId = a} :: CreateBranch)

instance Core.AWSRequest CreateBranch where
  type AWSResponse CreateBranch = CreateBranchResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull CreateBranchResponse'

instance Core.Hashable CreateBranch

instance Core.NFData CreateBranch

instance Core.ToHeaders CreateBranch where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.CreateBranch" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateBranch where
  toJSON CreateBranch' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("branchName" Core..= branchName),
            Core.Just ("commitId" Core..= commitId)
          ]
      )

instance Core.ToPath CreateBranch where
  toPath = Core.const "/"

instance Core.ToQuery CreateBranch where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateBranchResponse' smart constructor.
data CreateBranchResponse = CreateBranchResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateBranchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateBranchResponse ::
  CreateBranchResponse
newCreateBranchResponse = CreateBranchResponse'

instance Core.NFData CreateBranchResponse
