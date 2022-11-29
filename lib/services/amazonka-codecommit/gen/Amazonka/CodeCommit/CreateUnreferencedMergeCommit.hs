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
-- Module      : Amazonka.CodeCommit.CreateUnreferencedMergeCommit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an unreferenced commit that represents the result of merging two
-- branches using a specified merge strategy. This can help you determine
-- the outcome of a potential merge. This API cannot be used with the
-- fast-forward merge strategy because that strategy does not create a
-- merge commit.
--
-- This unreferenced merge commit can only be accessed using the GetCommit
-- API or through git commands such as git fetch. To retrieve this commit,
-- you must specify its commit ID or otherwise reference it.
module Amazonka.CodeCommit.CreateUnreferencedMergeCommit
  ( -- * Creating a Request
    CreateUnreferencedMergeCommit (..),
    newCreateUnreferencedMergeCommit,

    -- * Request Lenses
    createUnreferencedMergeCommit_keepEmptyFolders,
    createUnreferencedMergeCommit_conflictResolution,
    createUnreferencedMergeCommit_email,
    createUnreferencedMergeCommit_authorName,
    createUnreferencedMergeCommit_commitMessage,
    createUnreferencedMergeCommit_conflictResolutionStrategy,
    createUnreferencedMergeCommit_conflictDetailLevel,
    createUnreferencedMergeCommit_repositoryName,
    createUnreferencedMergeCommit_sourceCommitSpecifier,
    createUnreferencedMergeCommit_destinationCommitSpecifier,
    createUnreferencedMergeCommit_mergeOption,

    -- * Destructuring the Response
    CreateUnreferencedMergeCommitResponse (..),
    newCreateUnreferencedMergeCommitResponse,

    -- * Response Lenses
    createUnreferencedMergeCommitResponse_commitId,
    createUnreferencedMergeCommitResponse_treeId,
    createUnreferencedMergeCommitResponse_httpStatus,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateUnreferencedMergeCommit' smart constructor.
data CreateUnreferencedMergeCommit = CreateUnreferencedMergeCommit'
  { -- | If the commit contains deletions, whether to keep a folder or folder
    -- structure if the changes leave the folders empty. If this is specified
    -- as true, a .gitkeep file is created for empty folders. The default is
    -- false.
    keepEmptyFolders :: Prelude.Maybe Prelude.Bool,
    -- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to
    -- use when resolving conflicts during a merge.
    conflictResolution :: Prelude.Maybe ConflictResolution,
    -- | The email address for the person who created the unreferenced commit.
    email :: Prelude.Maybe Prelude.Text,
    -- | The name of the author who created the unreferenced commit. This
    -- information is used as both the author and committer for the commit.
    authorName :: Prelude.Maybe Prelude.Text,
    -- | The commit message for the unreferenced commit.
    commitMessage :: Prelude.Maybe Prelude.Text,
    -- | Specifies which branch to use when resolving conflicts, or whether to
    -- attempt automatically merging two versions of a file. The default is
    -- NONE, which requires any conflicts to be resolved manually before the
    -- merge operation is successful.
    conflictResolutionStrategy :: Prelude.Maybe ConflictResolutionStrategyTypeEnum,
    -- | The level of conflict detail to use. If unspecified, the default
    -- FILE_LEVEL is used, which returns a not-mergeable result if the same
    -- file has differences in both branches. If LINE_LEVEL is specified, a
    -- conflict is considered not mergeable if the same file in both branches
    -- has differences on the same line.
    conflictDetailLevel :: Prelude.Maybe ConflictDetailLevelTypeEnum,
    -- | The name of the repository where you want to create the unreferenced
    -- merge commit.
    repositoryName :: Prelude.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to
    -- identify a commit (for example, a branch name or a full commit ID).
    sourceCommitSpecifier :: Prelude.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to
    -- identify a commit (for example, a branch name or a full commit ID).
    destinationCommitSpecifier :: Prelude.Text,
    -- | The merge option or strategy you want to use to merge the code.
    mergeOption :: MergeOptionTypeEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUnreferencedMergeCommit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keepEmptyFolders', 'createUnreferencedMergeCommit_keepEmptyFolders' - If the commit contains deletions, whether to keep a folder or folder
-- structure if the changes leave the folders empty. If this is specified
-- as true, a .gitkeep file is created for empty folders. The default is
-- false.
--
-- 'conflictResolution', 'createUnreferencedMergeCommit_conflictResolution' - If AUTOMERGE is the conflict resolution strategy, a list of inputs to
-- use when resolving conflicts during a merge.
--
-- 'email', 'createUnreferencedMergeCommit_email' - The email address for the person who created the unreferenced commit.
--
-- 'authorName', 'createUnreferencedMergeCommit_authorName' - The name of the author who created the unreferenced commit. This
-- information is used as both the author and committer for the commit.
--
-- 'commitMessage', 'createUnreferencedMergeCommit_commitMessage' - The commit message for the unreferenced commit.
--
-- 'conflictResolutionStrategy', 'createUnreferencedMergeCommit_conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
--
-- 'conflictDetailLevel', 'createUnreferencedMergeCommit_conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
--
-- 'repositoryName', 'createUnreferencedMergeCommit_repositoryName' - The name of the repository where you want to create the unreferenced
-- merge commit.
--
-- 'sourceCommitSpecifier', 'createUnreferencedMergeCommit_sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
--
-- 'destinationCommitSpecifier', 'createUnreferencedMergeCommit_destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
--
-- 'mergeOption', 'createUnreferencedMergeCommit_mergeOption' - The merge option or strategy you want to use to merge the code.
newCreateUnreferencedMergeCommit ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'sourceCommitSpecifier'
  Prelude.Text ->
  -- | 'destinationCommitSpecifier'
  Prelude.Text ->
  -- | 'mergeOption'
  MergeOptionTypeEnum ->
  CreateUnreferencedMergeCommit
newCreateUnreferencedMergeCommit
  pRepositoryName_
  pSourceCommitSpecifier_
  pDestinationCommitSpecifier_
  pMergeOption_ =
    CreateUnreferencedMergeCommit'
      { keepEmptyFolders =
          Prelude.Nothing,
        conflictResolution = Prelude.Nothing,
        email = Prelude.Nothing,
        authorName = Prelude.Nothing,
        commitMessage = Prelude.Nothing,
        conflictResolutionStrategy = Prelude.Nothing,
        conflictDetailLevel = Prelude.Nothing,
        repositoryName = pRepositoryName_,
        sourceCommitSpecifier =
          pSourceCommitSpecifier_,
        destinationCommitSpecifier =
          pDestinationCommitSpecifier_,
        mergeOption = pMergeOption_
      }

-- | If the commit contains deletions, whether to keep a folder or folder
-- structure if the changes leave the folders empty. If this is specified
-- as true, a .gitkeep file is created for empty folders. The default is
-- false.
createUnreferencedMergeCommit_keepEmptyFolders :: Lens.Lens' CreateUnreferencedMergeCommit (Prelude.Maybe Prelude.Bool)
createUnreferencedMergeCommit_keepEmptyFolders = Lens.lens (\CreateUnreferencedMergeCommit' {keepEmptyFolders} -> keepEmptyFolders) (\s@CreateUnreferencedMergeCommit' {} a -> s {keepEmptyFolders = a} :: CreateUnreferencedMergeCommit)

-- | If AUTOMERGE is the conflict resolution strategy, a list of inputs to
-- use when resolving conflicts during a merge.
createUnreferencedMergeCommit_conflictResolution :: Lens.Lens' CreateUnreferencedMergeCommit (Prelude.Maybe ConflictResolution)
createUnreferencedMergeCommit_conflictResolution = Lens.lens (\CreateUnreferencedMergeCommit' {conflictResolution} -> conflictResolution) (\s@CreateUnreferencedMergeCommit' {} a -> s {conflictResolution = a} :: CreateUnreferencedMergeCommit)

-- | The email address for the person who created the unreferenced commit.
createUnreferencedMergeCommit_email :: Lens.Lens' CreateUnreferencedMergeCommit (Prelude.Maybe Prelude.Text)
createUnreferencedMergeCommit_email = Lens.lens (\CreateUnreferencedMergeCommit' {email} -> email) (\s@CreateUnreferencedMergeCommit' {} a -> s {email = a} :: CreateUnreferencedMergeCommit)

-- | The name of the author who created the unreferenced commit. This
-- information is used as both the author and committer for the commit.
createUnreferencedMergeCommit_authorName :: Lens.Lens' CreateUnreferencedMergeCommit (Prelude.Maybe Prelude.Text)
createUnreferencedMergeCommit_authorName = Lens.lens (\CreateUnreferencedMergeCommit' {authorName} -> authorName) (\s@CreateUnreferencedMergeCommit' {} a -> s {authorName = a} :: CreateUnreferencedMergeCommit)

-- | The commit message for the unreferenced commit.
createUnreferencedMergeCommit_commitMessage :: Lens.Lens' CreateUnreferencedMergeCommit (Prelude.Maybe Prelude.Text)
createUnreferencedMergeCommit_commitMessage = Lens.lens (\CreateUnreferencedMergeCommit' {commitMessage} -> commitMessage) (\s@CreateUnreferencedMergeCommit' {} a -> s {commitMessage = a} :: CreateUnreferencedMergeCommit)

-- | Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
createUnreferencedMergeCommit_conflictResolutionStrategy :: Lens.Lens' CreateUnreferencedMergeCommit (Prelude.Maybe ConflictResolutionStrategyTypeEnum)
createUnreferencedMergeCommit_conflictResolutionStrategy = Lens.lens (\CreateUnreferencedMergeCommit' {conflictResolutionStrategy} -> conflictResolutionStrategy) (\s@CreateUnreferencedMergeCommit' {} a -> s {conflictResolutionStrategy = a} :: CreateUnreferencedMergeCommit)

-- | The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
createUnreferencedMergeCommit_conflictDetailLevel :: Lens.Lens' CreateUnreferencedMergeCommit (Prelude.Maybe ConflictDetailLevelTypeEnum)
createUnreferencedMergeCommit_conflictDetailLevel = Lens.lens (\CreateUnreferencedMergeCommit' {conflictDetailLevel} -> conflictDetailLevel) (\s@CreateUnreferencedMergeCommit' {} a -> s {conflictDetailLevel = a} :: CreateUnreferencedMergeCommit)

-- | The name of the repository where you want to create the unreferenced
-- merge commit.
createUnreferencedMergeCommit_repositoryName :: Lens.Lens' CreateUnreferencedMergeCommit Prelude.Text
createUnreferencedMergeCommit_repositoryName = Lens.lens (\CreateUnreferencedMergeCommit' {repositoryName} -> repositoryName) (\s@CreateUnreferencedMergeCommit' {} a -> s {repositoryName = a} :: CreateUnreferencedMergeCommit)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
createUnreferencedMergeCommit_sourceCommitSpecifier :: Lens.Lens' CreateUnreferencedMergeCommit Prelude.Text
createUnreferencedMergeCommit_sourceCommitSpecifier = Lens.lens (\CreateUnreferencedMergeCommit' {sourceCommitSpecifier} -> sourceCommitSpecifier) (\s@CreateUnreferencedMergeCommit' {} a -> s {sourceCommitSpecifier = a} :: CreateUnreferencedMergeCommit)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
createUnreferencedMergeCommit_destinationCommitSpecifier :: Lens.Lens' CreateUnreferencedMergeCommit Prelude.Text
createUnreferencedMergeCommit_destinationCommitSpecifier = Lens.lens (\CreateUnreferencedMergeCommit' {destinationCommitSpecifier} -> destinationCommitSpecifier) (\s@CreateUnreferencedMergeCommit' {} a -> s {destinationCommitSpecifier = a} :: CreateUnreferencedMergeCommit)

-- | The merge option or strategy you want to use to merge the code.
createUnreferencedMergeCommit_mergeOption :: Lens.Lens' CreateUnreferencedMergeCommit MergeOptionTypeEnum
createUnreferencedMergeCommit_mergeOption = Lens.lens (\CreateUnreferencedMergeCommit' {mergeOption} -> mergeOption) (\s@CreateUnreferencedMergeCommit' {} a -> s {mergeOption = a} :: CreateUnreferencedMergeCommit)

instance
  Core.AWSRequest
    CreateUnreferencedMergeCommit
  where
  type
    AWSResponse CreateUnreferencedMergeCommit =
      CreateUnreferencedMergeCommitResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUnreferencedMergeCommitResponse'
            Prelude.<$> (x Core..?> "commitId")
            Prelude.<*> (x Core..?> "treeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateUnreferencedMergeCommit
  where
  hashWithSalt _salt CreateUnreferencedMergeCommit' {..} =
    _salt `Prelude.hashWithSalt` keepEmptyFolders
      `Prelude.hashWithSalt` conflictResolution
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` authorName
      `Prelude.hashWithSalt` commitMessage
      `Prelude.hashWithSalt` conflictResolutionStrategy
      `Prelude.hashWithSalt` conflictDetailLevel
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` sourceCommitSpecifier
      `Prelude.hashWithSalt` destinationCommitSpecifier
      `Prelude.hashWithSalt` mergeOption

instance Prelude.NFData CreateUnreferencedMergeCommit where
  rnf CreateUnreferencedMergeCommit' {..} =
    Prelude.rnf keepEmptyFolders
      `Prelude.seq` Prelude.rnf conflictResolution
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf authorName
      `Prelude.seq` Prelude.rnf commitMessage
      `Prelude.seq` Prelude.rnf conflictResolutionStrategy
      `Prelude.seq` Prelude.rnf conflictDetailLevel
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf sourceCommitSpecifier
      `Prelude.seq` Prelude.rnf destinationCommitSpecifier
      `Prelude.seq` Prelude.rnf mergeOption

instance Core.ToHeaders CreateUnreferencedMergeCommit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.CreateUnreferencedMergeCommit" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateUnreferencedMergeCommit where
  toJSON CreateUnreferencedMergeCommit' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("keepEmptyFolders" Core..=)
              Prelude.<$> keepEmptyFolders,
            ("conflictResolution" Core..=)
              Prelude.<$> conflictResolution,
            ("email" Core..=) Prelude.<$> email,
            ("authorName" Core..=) Prelude.<$> authorName,
            ("commitMessage" Core..=) Prelude.<$> commitMessage,
            ("conflictResolutionStrategy" Core..=)
              Prelude.<$> conflictResolutionStrategy,
            ("conflictDetailLevel" Core..=)
              Prelude.<$> conflictDetailLevel,
            Prelude.Just
              ("repositoryName" Core..= repositoryName),
            Prelude.Just
              ( "sourceCommitSpecifier"
                  Core..= sourceCommitSpecifier
              ),
            Prelude.Just
              ( "destinationCommitSpecifier"
                  Core..= destinationCommitSpecifier
              ),
            Prelude.Just ("mergeOption" Core..= mergeOption)
          ]
      )

instance Core.ToPath CreateUnreferencedMergeCommit where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateUnreferencedMergeCommit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUnreferencedMergeCommitResponse' smart constructor.
data CreateUnreferencedMergeCommitResponse = CreateUnreferencedMergeCommitResponse'
  { -- | The full commit ID of the commit that contains your merge results.
    commitId :: Prelude.Maybe Prelude.Text,
    -- | The full SHA-1 pointer of the tree information for the commit that
    -- contains the merge results.
    treeId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUnreferencedMergeCommitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitId', 'createUnreferencedMergeCommitResponse_commitId' - The full commit ID of the commit that contains your merge results.
--
-- 'treeId', 'createUnreferencedMergeCommitResponse_treeId' - The full SHA-1 pointer of the tree information for the commit that
-- contains the merge results.
--
-- 'httpStatus', 'createUnreferencedMergeCommitResponse_httpStatus' - The response's http status code.
newCreateUnreferencedMergeCommitResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateUnreferencedMergeCommitResponse
newCreateUnreferencedMergeCommitResponse pHttpStatus_ =
  CreateUnreferencedMergeCommitResponse'
    { commitId =
        Prelude.Nothing,
      treeId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full commit ID of the commit that contains your merge results.
createUnreferencedMergeCommitResponse_commitId :: Lens.Lens' CreateUnreferencedMergeCommitResponse (Prelude.Maybe Prelude.Text)
createUnreferencedMergeCommitResponse_commitId = Lens.lens (\CreateUnreferencedMergeCommitResponse' {commitId} -> commitId) (\s@CreateUnreferencedMergeCommitResponse' {} a -> s {commitId = a} :: CreateUnreferencedMergeCommitResponse)

-- | The full SHA-1 pointer of the tree information for the commit that
-- contains the merge results.
createUnreferencedMergeCommitResponse_treeId :: Lens.Lens' CreateUnreferencedMergeCommitResponse (Prelude.Maybe Prelude.Text)
createUnreferencedMergeCommitResponse_treeId = Lens.lens (\CreateUnreferencedMergeCommitResponse' {treeId} -> treeId) (\s@CreateUnreferencedMergeCommitResponse' {} a -> s {treeId = a} :: CreateUnreferencedMergeCommitResponse)

-- | The response's http status code.
createUnreferencedMergeCommitResponse_httpStatus :: Lens.Lens' CreateUnreferencedMergeCommitResponse Prelude.Int
createUnreferencedMergeCommitResponse_httpStatus = Lens.lens (\CreateUnreferencedMergeCommitResponse' {httpStatus} -> httpStatus) (\s@CreateUnreferencedMergeCommitResponse' {} a -> s {httpStatus = a} :: CreateUnreferencedMergeCommitResponse)

instance
  Prelude.NFData
    CreateUnreferencedMergeCommitResponse
  where
  rnf CreateUnreferencedMergeCommitResponse' {..} =
    Prelude.rnf commitId
      `Prelude.seq` Prelude.rnf treeId
      `Prelude.seq` Prelude.rnf httpStatus
