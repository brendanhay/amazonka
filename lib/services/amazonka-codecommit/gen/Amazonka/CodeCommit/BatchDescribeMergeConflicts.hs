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
-- Module      : Amazonka.CodeCommit.BatchDescribeMergeConflicts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more merge conflicts in the attempted
-- merge of two commit specifiers using the squash or three-way merge
-- strategy.
module Amazonka.CodeCommit.BatchDescribeMergeConflicts
  ( -- * Creating a Request
    BatchDescribeMergeConflicts (..),
    newBatchDescribeMergeConflicts,

    -- * Request Lenses
    batchDescribeMergeConflicts_conflictDetailLevel,
    batchDescribeMergeConflicts_conflictResolutionStrategy,
    batchDescribeMergeConflicts_filePaths,
    batchDescribeMergeConflicts_maxConflictFiles,
    batchDescribeMergeConflicts_maxMergeHunks,
    batchDescribeMergeConflicts_nextToken,
    batchDescribeMergeConflicts_repositoryName,
    batchDescribeMergeConflicts_destinationCommitSpecifier,
    batchDescribeMergeConflicts_sourceCommitSpecifier,
    batchDescribeMergeConflicts_mergeOption,

    -- * Destructuring the Response
    BatchDescribeMergeConflictsResponse (..),
    newBatchDescribeMergeConflictsResponse,

    -- * Response Lenses
    batchDescribeMergeConflictsResponse_baseCommitId,
    batchDescribeMergeConflictsResponse_errors,
    batchDescribeMergeConflictsResponse_nextToken,
    batchDescribeMergeConflictsResponse_httpStatus,
    batchDescribeMergeConflictsResponse_conflicts,
    batchDescribeMergeConflictsResponse_destinationCommitId,
    batchDescribeMergeConflictsResponse_sourceCommitId,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDescribeMergeConflicts' smart constructor.
data BatchDescribeMergeConflicts = BatchDescribeMergeConflicts'
  { -- | The level of conflict detail to use. If unspecified, the default
    -- FILE_LEVEL is used, which returns a not-mergeable result if the same
    -- file has differences in both branches. If LINE_LEVEL is specified, a
    -- conflict is considered not mergeable if the same file in both branches
    -- has differences on the same line.
    conflictDetailLevel :: Prelude.Maybe ConflictDetailLevelTypeEnum,
    -- | Specifies which branch to use when resolving conflicts, or whether to
    -- attempt automatically merging two versions of a file. The default is
    -- NONE, which requires any conflicts to be resolved manually before the
    -- merge operation is successful.
    conflictResolutionStrategy :: Prelude.Maybe ConflictResolutionStrategyTypeEnum,
    -- | The path of the target files used to describe the conflicts. If not
    -- specified, the default is all conflict files.
    filePaths :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of files to include in the output.
    maxConflictFiles :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of merge hunks to include in the output.
    maxMergeHunks :: Prelude.Maybe Prelude.Int,
    -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository that contains the merge conflicts you want to
    -- review.
    repositoryName :: Prelude.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to
    -- identify a commit (for example, a branch name or a full commit ID).
    destinationCommitSpecifier :: Prelude.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to
    -- identify a commit (for example, a branch name or a full commit ID).
    sourceCommitSpecifier :: Prelude.Text,
    -- | The merge option or strategy you want to use to merge the code.
    mergeOption :: MergeOptionTypeEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDescribeMergeConflicts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conflictDetailLevel', 'batchDescribeMergeConflicts_conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
--
-- 'conflictResolutionStrategy', 'batchDescribeMergeConflicts_conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
--
-- 'filePaths', 'batchDescribeMergeConflicts_filePaths' - The path of the target files used to describe the conflicts. If not
-- specified, the default is all conflict files.
--
-- 'maxConflictFiles', 'batchDescribeMergeConflicts_maxConflictFiles' - The maximum number of files to include in the output.
--
-- 'maxMergeHunks', 'batchDescribeMergeConflicts_maxMergeHunks' - The maximum number of merge hunks to include in the output.
--
-- 'nextToken', 'batchDescribeMergeConflicts_nextToken' - An enumeration token that, when provided in a request, returns the next
-- batch of the results.
--
-- 'repositoryName', 'batchDescribeMergeConflicts_repositoryName' - The name of the repository that contains the merge conflicts you want to
-- review.
--
-- 'destinationCommitSpecifier', 'batchDescribeMergeConflicts_destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
--
-- 'sourceCommitSpecifier', 'batchDescribeMergeConflicts_sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
--
-- 'mergeOption', 'batchDescribeMergeConflicts_mergeOption' - The merge option or strategy you want to use to merge the code.
newBatchDescribeMergeConflicts ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'destinationCommitSpecifier'
  Prelude.Text ->
  -- | 'sourceCommitSpecifier'
  Prelude.Text ->
  -- | 'mergeOption'
  MergeOptionTypeEnum ->
  BatchDescribeMergeConflicts
newBatchDescribeMergeConflicts
  pRepositoryName_
  pDestinationCommitSpecifier_
  pSourceCommitSpecifier_
  pMergeOption_ =
    BatchDescribeMergeConflicts'
      { conflictDetailLevel =
          Prelude.Nothing,
        conflictResolutionStrategy = Prelude.Nothing,
        filePaths = Prelude.Nothing,
        maxConflictFiles = Prelude.Nothing,
        maxMergeHunks = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        repositoryName = pRepositoryName_,
        destinationCommitSpecifier =
          pDestinationCommitSpecifier_,
        sourceCommitSpecifier =
          pSourceCommitSpecifier_,
        mergeOption = pMergeOption_
      }

-- | The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
batchDescribeMergeConflicts_conflictDetailLevel :: Lens.Lens' BatchDescribeMergeConflicts (Prelude.Maybe ConflictDetailLevelTypeEnum)
batchDescribeMergeConflicts_conflictDetailLevel = Lens.lens (\BatchDescribeMergeConflicts' {conflictDetailLevel} -> conflictDetailLevel) (\s@BatchDescribeMergeConflicts' {} a -> s {conflictDetailLevel = a} :: BatchDescribeMergeConflicts)

-- | Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
batchDescribeMergeConflicts_conflictResolutionStrategy :: Lens.Lens' BatchDescribeMergeConflicts (Prelude.Maybe ConflictResolutionStrategyTypeEnum)
batchDescribeMergeConflicts_conflictResolutionStrategy = Lens.lens (\BatchDescribeMergeConflicts' {conflictResolutionStrategy} -> conflictResolutionStrategy) (\s@BatchDescribeMergeConflicts' {} a -> s {conflictResolutionStrategy = a} :: BatchDescribeMergeConflicts)

-- | The path of the target files used to describe the conflicts. If not
-- specified, the default is all conflict files.
batchDescribeMergeConflicts_filePaths :: Lens.Lens' BatchDescribeMergeConflicts (Prelude.Maybe [Prelude.Text])
batchDescribeMergeConflicts_filePaths = Lens.lens (\BatchDescribeMergeConflicts' {filePaths} -> filePaths) (\s@BatchDescribeMergeConflicts' {} a -> s {filePaths = a} :: BatchDescribeMergeConflicts) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of files to include in the output.
batchDescribeMergeConflicts_maxConflictFiles :: Lens.Lens' BatchDescribeMergeConflicts (Prelude.Maybe Prelude.Int)
batchDescribeMergeConflicts_maxConflictFiles = Lens.lens (\BatchDescribeMergeConflicts' {maxConflictFiles} -> maxConflictFiles) (\s@BatchDescribeMergeConflicts' {} a -> s {maxConflictFiles = a} :: BatchDescribeMergeConflicts)

-- | The maximum number of merge hunks to include in the output.
batchDescribeMergeConflicts_maxMergeHunks :: Lens.Lens' BatchDescribeMergeConflicts (Prelude.Maybe Prelude.Int)
batchDescribeMergeConflicts_maxMergeHunks = Lens.lens (\BatchDescribeMergeConflicts' {maxMergeHunks} -> maxMergeHunks) (\s@BatchDescribeMergeConflicts' {} a -> s {maxMergeHunks = a} :: BatchDescribeMergeConflicts)

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
batchDescribeMergeConflicts_nextToken :: Lens.Lens' BatchDescribeMergeConflicts (Prelude.Maybe Prelude.Text)
batchDescribeMergeConflicts_nextToken = Lens.lens (\BatchDescribeMergeConflicts' {nextToken} -> nextToken) (\s@BatchDescribeMergeConflicts' {} a -> s {nextToken = a} :: BatchDescribeMergeConflicts)

-- | The name of the repository that contains the merge conflicts you want to
-- review.
batchDescribeMergeConflicts_repositoryName :: Lens.Lens' BatchDescribeMergeConflicts Prelude.Text
batchDescribeMergeConflicts_repositoryName = Lens.lens (\BatchDescribeMergeConflicts' {repositoryName} -> repositoryName) (\s@BatchDescribeMergeConflicts' {} a -> s {repositoryName = a} :: BatchDescribeMergeConflicts)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
batchDescribeMergeConflicts_destinationCommitSpecifier :: Lens.Lens' BatchDescribeMergeConflicts Prelude.Text
batchDescribeMergeConflicts_destinationCommitSpecifier = Lens.lens (\BatchDescribeMergeConflicts' {destinationCommitSpecifier} -> destinationCommitSpecifier) (\s@BatchDescribeMergeConflicts' {} a -> s {destinationCommitSpecifier = a} :: BatchDescribeMergeConflicts)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
batchDescribeMergeConflicts_sourceCommitSpecifier :: Lens.Lens' BatchDescribeMergeConflicts Prelude.Text
batchDescribeMergeConflicts_sourceCommitSpecifier = Lens.lens (\BatchDescribeMergeConflicts' {sourceCommitSpecifier} -> sourceCommitSpecifier) (\s@BatchDescribeMergeConflicts' {} a -> s {sourceCommitSpecifier = a} :: BatchDescribeMergeConflicts)

-- | The merge option or strategy you want to use to merge the code.
batchDescribeMergeConflicts_mergeOption :: Lens.Lens' BatchDescribeMergeConflicts MergeOptionTypeEnum
batchDescribeMergeConflicts_mergeOption = Lens.lens (\BatchDescribeMergeConflicts' {mergeOption} -> mergeOption) (\s@BatchDescribeMergeConflicts' {} a -> s {mergeOption = a} :: BatchDescribeMergeConflicts)

instance Core.AWSRequest BatchDescribeMergeConflicts where
  type
    AWSResponse BatchDescribeMergeConflicts =
      BatchDescribeMergeConflictsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDescribeMergeConflictsResponse'
            Prelude.<$> (x Data..?> "baseCommitId")
            Prelude.<*> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "conflicts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "destinationCommitId")
            Prelude.<*> (x Data..:> "sourceCommitId")
      )

instance Prelude.Hashable BatchDescribeMergeConflicts where
  hashWithSalt _salt BatchDescribeMergeConflicts' {..} =
    _salt `Prelude.hashWithSalt` conflictDetailLevel
      `Prelude.hashWithSalt` conflictResolutionStrategy
      `Prelude.hashWithSalt` filePaths
      `Prelude.hashWithSalt` maxConflictFiles
      `Prelude.hashWithSalt` maxMergeHunks
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` destinationCommitSpecifier
      `Prelude.hashWithSalt` sourceCommitSpecifier
      `Prelude.hashWithSalt` mergeOption

instance Prelude.NFData BatchDescribeMergeConflicts where
  rnf BatchDescribeMergeConflicts' {..} =
    Prelude.rnf conflictDetailLevel
      `Prelude.seq` Prelude.rnf conflictResolutionStrategy
      `Prelude.seq` Prelude.rnf filePaths
      `Prelude.seq` Prelude.rnf maxConflictFiles
      `Prelude.seq` Prelude.rnf maxMergeHunks
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf destinationCommitSpecifier
      `Prelude.seq` Prelude.rnf sourceCommitSpecifier
      `Prelude.seq` Prelude.rnf mergeOption

instance Data.ToHeaders BatchDescribeMergeConflicts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.BatchDescribeMergeConflicts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDescribeMergeConflicts where
  toJSON BatchDescribeMergeConflicts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("conflictDetailLevel" Data..=)
              Prelude.<$> conflictDetailLevel,
            ("conflictResolutionStrategy" Data..=)
              Prelude.<$> conflictResolutionStrategy,
            ("filePaths" Data..=) Prelude.<$> filePaths,
            ("maxConflictFiles" Data..=)
              Prelude.<$> maxConflictFiles,
            ("maxMergeHunks" Data..=) Prelude.<$> maxMergeHunks,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just
              ( "destinationCommitSpecifier"
                  Data..= destinationCommitSpecifier
              ),
            Prelude.Just
              ( "sourceCommitSpecifier"
                  Data..= sourceCommitSpecifier
              ),
            Prelude.Just ("mergeOption" Data..= mergeOption)
          ]
      )

instance Data.ToPath BatchDescribeMergeConflicts where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchDescribeMergeConflicts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDescribeMergeConflictsResponse' smart constructor.
data BatchDescribeMergeConflictsResponse = BatchDescribeMergeConflictsResponse'
  { -- | The commit ID of the merge base.
    baseCommitId :: Prelude.Maybe Prelude.Text,
    -- | A list of any errors returned while describing the merge conflicts for
    -- each file.
    errors :: Prelude.Maybe [BatchDescribeMergeConflictsError],
    -- | An enumeration token that can be used in a request to return the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of conflicts for each file, including the conflict metadata and
    -- the hunks of the differences between the files.
    conflicts :: [Conflict],
    -- | The commit ID of the destination commit specifier that was used in the
    -- merge evaluation.
    destinationCommitId :: Prelude.Text,
    -- | The commit ID of the source commit specifier that was used in the merge
    -- evaluation.
    sourceCommitId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDescribeMergeConflictsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseCommitId', 'batchDescribeMergeConflictsResponse_baseCommitId' - The commit ID of the merge base.
--
-- 'errors', 'batchDescribeMergeConflictsResponse_errors' - A list of any errors returned while describing the merge conflicts for
-- each file.
--
-- 'nextToken', 'batchDescribeMergeConflictsResponse_nextToken' - An enumeration token that can be used in a request to return the next
-- batch of the results.
--
-- 'httpStatus', 'batchDescribeMergeConflictsResponse_httpStatus' - The response's http status code.
--
-- 'conflicts', 'batchDescribeMergeConflictsResponse_conflicts' - A list of conflicts for each file, including the conflict metadata and
-- the hunks of the differences between the files.
--
-- 'destinationCommitId', 'batchDescribeMergeConflictsResponse_destinationCommitId' - The commit ID of the destination commit specifier that was used in the
-- merge evaluation.
--
-- 'sourceCommitId', 'batchDescribeMergeConflictsResponse_sourceCommitId' - The commit ID of the source commit specifier that was used in the merge
-- evaluation.
newBatchDescribeMergeConflictsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'destinationCommitId'
  Prelude.Text ->
  -- | 'sourceCommitId'
  Prelude.Text ->
  BatchDescribeMergeConflictsResponse
newBatchDescribeMergeConflictsResponse
  pHttpStatus_
  pDestinationCommitId_
  pSourceCommitId_ =
    BatchDescribeMergeConflictsResponse'
      { baseCommitId =
          Prelude.Nothing,
        errors = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        conflicts = Prelude.mempty,
        destinationCommitId =
          pDestinationCommitId_,
        sourceCommitId = pSourceCommitId_
      }

-- | The commit ID of the merge base.
batchDescribeMergeConflictsResponse_baseCommitId :: Lens.Lens' BatchDescribeMergeConflictsResponse (Prelude.Maybe Prelude.Text)
batchDescribeMergeConflictsResponse_baseCommitId = Lens.lens (\BatchDescribeMergeConflictsResponse' {baseCommitId} -> baseCommitId) (\s@BatchDescribeMergeConflictsResponse' {} a -> s {baseCommitId = a} :: BatchDescribeMergeConflictsResponse)

-- | A list of any errors returned while describing the merge conflicts for
-- each file.
batchDescribeMergeConflictsResponse_errors :: Lens.Lens' BatchDescribeMergeConflictsResponse (Prelude.Maybe [BatchDescribeMergeConflictsError])
batchDescribeMergeConflictsResponse_errors = Lens.lens (\BatchDescribeMergeConflictsResponse' {errors} -> errors) (\s@BatchDescribeMergeConflictsResponse' {} a -> s {errors = a} :: BatchDescribeMergeConflictsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An enumeration token that can be used in a request to return the next
-- batch of the results.
batchDescribeMergeConflictsResponse_nextToken :: Lens.Lens' BatchDescribeMergeConflictsResponse (Prelude.Maybe Prelude.Text)
batchDescribeMergeConflictsResponse_nextToken = Lens.lens (\BatchDescribeMergeConflictsResponse' {nextToken} -> nextToken) (\s@BatchDescribeMergeConflictsResponse' {} a -> s {nextToken = a} :: BatchDescribeMergeConflictsResponse)

-- | The response's http status code.
batchDescribeMergeConflictsResponse_httpStatus :: Lens.Lens' BatchDescribeMergeConflictsResponse Prelude.Int
batchDescribeMergeConflictsResponse_httpStatus = Lens.lens (\BatchDescribeMergeConflictsResponse' {httpStatus} -> httpStatus) (\s@BatchDescribeMergeConflictsResponse' {} a -> s {httpStatus = a} :: BatchDescribeMergeConflictsResponse)

-- | A list of conflicts for each file, including the conflict metadata and
-- the hunks of the differences between the files.
batchDescribeMergeConflictsResponse_conflicts :: Lens.Lens' BatchDescribeMergeConflictsResponse [Conflict]
batchDescribeMergeConflictsResponse_conflicts = Lens.lens (\BatchDescribeMergeConflictsResponse' {conflicts} -> conflicts) (\s@BatchDescribeMergeConflictsResponse' {} a -> s {conflicts = a} :: BatchDescribeMergeConflictsResponse) Prelude.. Lens.coerced

-- | The commit ID of the destination commit specifier that was used in the
-- merge evaluation.
batchDescribeMergeConflictsResponse_destinationCommitId :: Lens.Lens' BatchDescribeMergeConflictsResponse Prelude.Text
batchDescribeMergeConflictsResponse_destinationCommitId = Lens.lens (\BatchDescribeMergeConflictsResponse' {destinationCommitId} -> destinationCommitId) (\s@BatchDescribeMergeConflictsResponse' {} a -> s {destinationCommitId = a} :: BatchDescribeMergeConflictsResponse)

-- | The commit ID of the source commit specifier that was used in the merge
-- evaluation.
batchDescribeMergeConflictsResponse_sourceCommitId :: Lens.Lens' BatchDescribeMergeConflictsResponse Prelude.Text
batchDescribeMergeConflictsResponse_sourceCommitId = Lens.lens (\BatchDescribeMergeConflictsResponse' {sourceCommitId} -> sourceCommitId) (\s@BatchDescribeMergeConflictsResponse' {} a -> s {sourceCommitId = a} :: BatchDescribeMergeConflictsResponse)

instance
  Prelude.NFData
    BatchDescribeMergeConflictsResponse
  where
  rnf BatchDescribeMergeConflictsResponse' {..} =
    Prelude.rnf baseCommitId
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf conflicts
      `Prelude.seq` Prelude.rnf destinationCommitId
      `Prelude.seq` Prelude.rnf sourceCommitId
