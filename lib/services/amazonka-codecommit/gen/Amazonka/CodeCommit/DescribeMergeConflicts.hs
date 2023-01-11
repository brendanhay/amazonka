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
-- Module      : Amazonka.CodeCommit.DescribeMergeConflicts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more merge conflicts in the attempted
-- merge of two commit specifiers using the squash or three-way merge
-- strategy. If the merge option for the attempted merge is specified as
-- FAST_FORWARD_MERGE, an exception is thrown.
module Amazonka.CodeCommit.DescribeMergeConflicts
  ( -- * Creating a Request
    DescribeMergeConflicts (..),
    newDescribeMergeConflicts,

    -- * Request Lenses
    describeMergeConflicts_conflictDetailLevel,
    describeMergeConflicts_conflictResolutionStrategy,
    describeMergeConflicts_maxMergeHunks,
    describeMergeConflicts_nextToken,
    describeMergeConflicts_repositoryName,
    describeMergeConflicts_destinationCommitSpecifier,
    describeMergeConflicts_sourceCommitSpecifier,
    describeMergeConflicts_mergeOption,
    describeMergeConflicts_filePath,

    -- * Destructuring the Response
    DescribeMergeConflictsResponse (..),
    newDescribeMergeConflictsResponse,

    -- * Response Lenses
    describeMergeConflictsResponse_baseCommitId,
    describeMergeConflictsResponse_nextToken,
    describeMergeConflictsResponse_httpStatus,
    describeMergeConflictsResponse_conflictMetadata,
    describeMergeConflictsResponse_mergeHunks,
    describeMergeConflictsResponse_destinationCommitId,
    describeMergeConflictsResponse_sourceCommitId,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeMergeConflicts' smart constructor.
data DescribeMergeConflicts = DescribeMergeConflicts'
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
    -- | The maximum number of merge hunks to include in the output.
    maxMergeHunks :: Prelude.Maybe Prelude.Int,
    -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository where you want to get information about a
    -- merge conflict.
    repositoryName :: Prelude.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to
    -- identify a commit (for example, a branch name or a full commit ID).
    destinationCommitSpecifier :: Prelude.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to
    -- identify a commit (for example, a branch name or a full commit ID).
    sourceCommitSpecifier :: Prelude.Text,
    -- | The merge option or strategy you want to use to merge the code.
    mergeOption :: MergeOptionTypeEnum,
    -- | The path of the target files used to describe the conflicts.
    filePath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMergeConflicts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conflictDetailLevel', 'describeMergeConflicts_conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
--
-- 'conflictResolutionStrategy', 'describeMergeConflicts_conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
--
-- 'maxMergeHunks', 'describeMergeConflicts_maxMergeHunks' - The maximum number of merge hunks to include in the output.
--
-- 'nextToken', 'describeMergeConflicts_nextToken' - An enumeration token that, when provided in a request, returns the next
-- batch of the results.
--
-- 'repositoryName', 'describeMergeConflicts_repositoryName' - The name of the repository where you want to get information about a
-- merge conflict.
--
-- 'destinationCommitSpecifier', 'describeMergeConflicts_destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
--
-- 'sourceCommitSpecifier', 'describeMergeConflicts_sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
--
-- 'mergeOption', 'describeMergeConflicts_mergeOption' - The merge option or strategy you want to use to merge the code.
--
-- 'filePath', 'describeMergeConflicts_filePath' - The path of the target files used to describe the conflicts.
newDescribeMergeConflicts ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'destinationCommitSpecifier'
  Prelude.Text ->
  -- | 'sourceCommitSpecifier'
  Prelude.Text ->
  -- | 'mergeOption'
  MergeOptionTypeEnum ->
  -- | 'filePath'
  Prelude.Text ->
  DescribeMergeConflicts
newDescribeMergeConflicts
  pRepositoryName_
  pDestinationCommitSpecifier_
  pSourceCommitSpecifier_
  pMergeOption_
  pFilePath_ =
    DescribeMergeConflicts'
      { conflictDetailLevel =
          Prelude.Nothing,
        conflictResolutionStrategy = Prelude.Nothing,
        maxMergeHunks = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        repositoryName = pRepositoryName_,
        destinationCommitSpecifier =
          pDestinationCommitSpecifier_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        mergeOption = pMergeOption_,
        filePath = pFilePath_
      }

-- | The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
describeMergeConflicts_conflictDetailLevel :: Lens.Lens' DescribeMergeConflicts (Prelude.Maybe ConflictDetailLevelTypeEnum)
describeMergeConflicts_conflictDetailLevel = Lens.lens (\DescribeMergeConflicts' {conflictDetailLevel} -> conflictDetailLevel) (\s@DescribeMergeConflicts' {} a -> s {conflictDetailLevel = a} :: DescribeMergeConflicts)

-- | Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
describeMergeConflicts_conflictResolutionStrategy :: Lens.Lens' DescribeMergeConflicts (Prelude.Maybe ConflictResolutionStrategyTypeEnum)
describeMergeConflicts_conflictResolutionStrategy = Lens.lens (\DescribeMergeConflicts' {conflictResolutionStrategy} -> conflictResolutionStrategy) (\s@DescribeMergeConflicts' {} a -> s {conflictResolutionStrategy = a} :: DescribeMergeConflicts)

-- | The maximum number of merge hunks to include in the output.
describeMergeConflicts_maxMergeHunks :: Lens.Lens' DescribeMergeConflicts (Prelude.Maybe Prelude.Int)
describeMergeConflicts_maxMergeHunks = Lens.lens (\DescribeMergeConflicts' {maxMergeHunks} -> maxMergeHunks) (\s@DescribeMergeConflicts' {} a -> s {maxMergeHunks = a} :: DescribeMergeConflicts)

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
describeMergeConflicts_nextToken :: Lens.Lens' DescribeMergeConflicts (Prelude.Maybe Prelude.Text)
describeMergeConflicts_nextToken = Lens.lens (\DescribeMergeConflicts' {nextToken} -> nextToken) (\s@DescribeMergeConflicts' {} a -> s {nextToken = a} :: DescribeMergeConflicts)

-- | The name of the repository where you want to get information about a
-- merge conflict.
describeMergeConflicts_repositoryName :: Lens.Lens' DescribeMergeConflicts Prelude.Text
describeMergeConflicts_repositoryName = Lens.lens (\DescribeMergeConflicts' {repositoryName} -> repositoryName) (\s@DescribeMergeConflicts' {} a -> s {repositoryName = a} :: DescribeMergeConflicts)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
describeMergeConflicts_destinationCommitSpecifier :: Lens.Lens' DescribeMergeConflicts Prelude.Text
describeMergeConflicts_destinationCommitSpecifier = Lens.lens (\DescribeMergeConflicts' {destinationCommitSpecifier} -> destinationCommitSpecifier) (\s@DescribeMergeConflicts' {} a -> s {destinationCommitSpecifier = a} :: DescribeMergeConflicts)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
describeMergeConflicts_sourceCommitSpecifier :: Lens.Lens' DescribeMergeConflicts Prelude.Text
describeMergeConflicts_sourceCommitSpecifier = Lens.lens (\DescribeMergeConflicts' {sourceCommitSpecifier} -> sourceCommitSpecifier) (\s@DescribeMergeConflicts' {} a -> s {sourceCommitSpecifier = a} :: DescribeMergeConflicts)

-- | The merge option or strategy you want to use to merge the code.
describeMergeConflicts_mergeOption :: Lens.Lens' DescribeMergeConflicts MergeOptionTypeEnum
describeMergeConflicts_mergeOption = Lens.lens (\DescribeMergeConflicts' {mergeOption} -> mergeOption) (\s@DescribeMergeConflicts' {} a -> s {mergeOption = a} :: DescribeMergeConflicts)

-- | The path of the target files used to describe the conflicts.
describeMergeConflicts_filePath :: Lens.Lens' DescribeMergeConflicts Prelude.Text
describeMergeConflicts_filePath = Lens.lens (\DescribeMergeConflicts' {filePath} -> filePath) (\s@DescribeMergeConflicts' {} a -> s {filePath = a} :: DescribeMergeConflicts)

instance Core.AWSRequest DescribeMergeConflicts where
  type
    AWSResponse DescribeMergeConflicts =
      DescribeMergeConflictsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMergeConflictsResponse'
            Prelude.<$> (x Data..?> "baseCommitId")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "conflictMetadata")
            Prelude.<*> (x Data..?> "mergeHunks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "destinationCommitId")
            Prelude.<*> (x Data..:> "sourceCommitId")
      )

instance Prelude.Hashable DescribeMergeConflicts where
  hashWithSalt _salt DescribeMergeConflicts' {..} =
    _salt `Prelude.hashWithSalt` conflictDetailLevel
      `Prelude.hashWithSalt` conflictResolutionStrategy
      `Prelude.hashWithSalt` maxMergeHunks
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` destinationCommitSpecifier
      `Prelude.hashWithSalt` sourceCommitSpecifier
      `Prelude.hashWithSalt` mergeOption
      `Prelude.hashWithSalt` filePath

instance Prelude.NFData DescribeMergeConflicts where
  rnf DescribeMergeConflicts' {..} =
    Prelude.rnf conflictDetailLevel
      `Prelude.seq` Prelude.rnf conflictResolutionStrategy
      `Prelude.seq` Prelude.rnf maxMergeHunks
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf destinationCommitSpecifier
      `Prelude.seq` Prelude.rnf sourceCommitSpecifier
      `Prelude.seq` Prelude.rnf mergeOption
      `Prelude.seq` Prelude.rnf filePath

instance Data.ToHeaders DescribeMergeConflicts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.DescribeMergeConflicts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeMergeConflicts where
  toJSON DescribeMergeConflicts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("conflictDetailLevel" Data..=)
              Prelude.<$> conflictDetailLevel,
            ("conflictResolutionStrategy" Data..=)
              Prelude.<$> conflictResolutionStrategy,
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
            Prelude.Just ("mergeOption" Data..= mergeOption),
            Prelude.Just ("filePath" Data..= filePath)
          ]
      )

instance Data.ToPath DescribeMergeConflicts where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeMergeConflicts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMergeConflictsResponse' smart constructor.
data DescribeMergeConflictsResponse = DescribeMergeConflictsResponse'
  { -- | The commit ID of the merge base.
    baseCommitId :: Prelude.Maybe Prelude.Text,
    -- | An enumeration token that can be used in a request to return the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains metadata about the conflicts found in the merge.
    conflictMetadata :: ConflictMetadata,
    -- | A list of merge hunks of the differences between the files or lines.
    mergeHunks :: [MergeHunk],
    -- | The commit ID of the destination commit specifier that was used in the
    -- merge evaluation.
    destinationCommitId :: Prelude.Text,
    -- | The commit ID of the source commit specifier that was used in the merge
    -- evaluation.
    sourceCommitId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMergeConflictsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseCommitId', 'describeMergeConflictsResponse_baseCommitId' - The commit ID of the merge base.
--
-- 'nextToken', 'describeMergeConflictsResponse_nextToken' - An enumeration token that can be used in a request to return the next
-- batch of the results.
--
-- 'httpStatus', 'describeMergeConflictsResponse_httpStatus' - The response's http status code.
--
-- 'conflictMetadata', 'describeMergeConflictsResponse_conflictMetadata' - Contains metadata about the conflicts found in the merge.
--
-- 'mergeHunks', 'describeMergeConflictsResponse_mergeHunks' - A list of merge hunks of the differences between the files or lines.
--
-- 'destinationCommitId', 'describeMergeConflictsResponse_destinationCommitId' - The commit ID of the destination commit specifier that was used in the
-- merge evaluation.
--
-- 'sourceCommitId', 'describeMergeConflictsResponse_sourceCommitId' - The commit ID of the source commit specifier that was used in the merge
-- evaluation.
newDescribeMergeConflictsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'conflictMetadata'
  ConflictMetadata ->
  -- | 'destinationCommitId'
  Prelude.Text ->
  -- | 'sourceCommitId'
  Prelude.Text ->
  DescribeMergeConflictsResponse
newDescribeMergeConflictsResponse
  pHttpStatus_
  pConflictMetadata_
  pDestinationCommitId_
  pSourceCommitId_ =
    DescribeMergeConflictsResponse'
      { baseCommitId =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        conflictMetadata = pConflictMetadata_,
        mergeHunks = Prelude.mempty,
        destinationCommitId = pDestinationCommitId_,
        sourceCommitId = pSourceCommitId_
      }

-- | The commit ID of the merge base.
describeMergeConflictsResponse_baseCommitId :: Lens.Lens' DescribeMergeConflictsResponse (Prelude.Maybe Prelude.Text)
describeMergeConflictsResponse_baseCommitId = Lens.lens (\DescribeMergeConflictsResponse' {baseCommitId} -> baseCommitId) (\s@DescribeMergeConflictsResponse' {} a -> s {baseCommitId = a} :: DescribeMergeConflictsResponse)

-- | An enumeration token that can be used in a request to return the next
-- batch of the results.
describeMergeConflictsResponse_nextToken :: Lens.Lens' DescribeMergeConflictsResponse (Prelude.Maybe Prelude.Text)
describeMergeConflictsResponse_nextToken = Lens.lens (\DescribeMergeConflictsResponse' {nextToken} -> nextToken) (\s@DescribeMergeConflictsResponse' {} a -> s {nextToken = a} :: DescribeMergeConflictsResponse)

-- | The response's http status code.
describeMergeConflictsResponse_httpStatus :: Lens.Lens' DescribeMergeConflictsResponse Prelude.Int
describeMergeConflictsResponse_httpStatus = Lens.lens (\DescribeMergeConflictsResponse' {httpStatus} -> httpStatus) (\s@DescribeMergeConflictsResponse' {} a -> s {httpStatus = a} :: DescribeMergeConflictsResponse)

-- | Contains metadata about the conflicts found in the merge.
describeMergeConflictsResponse_conflictMetadata :: Lens.Lens' DescribeMergeConflictsResponse ConflictMetadata
describeMergeConflictsResponse_conflictMetadata = Lens.lens (\DescribeMergeConflictsResponse' {conflictMetadata} -> conflictMetadata) (\s@DescribeMergeConflictsResponse' {} a -> s {conflictMetadata = a} :: DescribeMergeConflictsResponse)

-- | A list of merge hunks of the differences between the files or lines.
describeMergeConflictsResponse_mergeHunks :: Lens.Lens' DescribeMergeConflictsResponse [MergeHunk]
describeMergeConflictsResponse_mergeHunks = Lens.lens (\DescribeMergeConflictsResponse' {mergeHunks} -> mergeHunks) (\s@DescribeMergeConflictsResponse' {} a -> s {mergeHunks = a} :: DescribeMergeConflictsResponse) Prelude.. Lens.coerced

-- | The commit ID of the destination commit specifier that was used in the
-- merge evaluation.
describeMergeConflictsResponse_destinationCommitId :: Lens.Lens' DescribeMergeConflictsResponse Prelude.Text
describeMergeConflictsResponse_destinationCommitId = Lens.lens (\DescribeMergeConflictsResponse' {destinationCommitId} -> destinationCommitId) (\s@DescribeMergeConflictsResponse' {} a -> s {destinationCommitId = a} :: DescribeMergeConflictsResponse)

-- | The commit ID of the source commit specifier that was used in the merge
-- evaluation.
describeMergeConflictsResponse_sourceCommitId :: Lens.Lens' DescribeMergeConflictsResponse Prelude.Text
describeMergeConflictsResponse_sourceCommitId = Lens.lens (\DescribeMergeConflictsResponse' {sourceCommitId} -> sourceCommitId) (\s@DescribeMergeConflictsResponse' {} a -> s {sourceCommitId = a} :: DescribeMergeConflictsResponse)

instance
  Prelude.NFData
    DescribeMergeConflictsResponse
  where
  rnf DescribeMergeConflictsResponse' {..} =
    Prelude.rnf baseCommitId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf conflictMetadata
      `Prelude.seq` Prelude.rnf mergeHunks
      `Prelude.seq` Prelude.rnf destinationCommitId
      `Prelude.seq` Prelude.rnf sourceCommitId
