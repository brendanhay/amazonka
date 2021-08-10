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
-- Module      : Network.AWS.CodeCommit.DescribeMergeConflicts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more merge conflicts in the attempted
-- merge of two commit specifiers using the squash or three-way merge
-- strategy. If the merge option for the attempted merge is specified as
-- FAST_FORWARD_MERGE, an exception is thrown.
module Network.AWS.CodeCommit.DescribeMergeConflicts
  ( -- * Creating a Request
    DescribeMergeConflicts (..),
    newDescribeMergeConflicts,

    -- * Request Lenses
    describeMergeConflicts_maxMergeHunks,
    describeMergeConflicts_nextToken,
    describeMergeConflicts_conflictDetailLevel,
    describeMergeConflicts_conflictResolutionStrategy,
    describeMergeConflicts_repositoryName,
    describeMergeConflicts_destinationCommitSpecifier,
    describeMergeConflicts_sourceCommitSpecifier,
    describeMergeConflicts_mergeOption,
    describeMergeConflicts_filePath,

    -- * Destructuring the Response
    DescribeMergeConflictsResponse (..),
    newDescribeMergeConflictsResponse,

    -- * Response Lenses
    describeMergeConflictsResponse_nextToken,
    describeMergeConflictsResponse_baseCommitId,
    describeMergeConflictsResponse_httpStatus,
    describeMergeConflictsResponse_conflictMetadata,
    describeMergeConflictsResponse_mergeHunks,
    describeMergeConflictsResponse_destinationCommitId,
    describeMergeConflictsResponse_sourceCommitId,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeMergeConflicts' smart constructor.
data DescribeMergeConflicts = DescribeMergeConflicts'
  { -- | The maximum number of merge hunks to include in the output.
    maxMergeHunks :: Prelude.Maybe Prelude.Int,
    -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The level of conflict detail to use. If unspecified, the default
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
-- 'maxMergeHunks', 'describeMergeConflicts_maxMergeHunks' - The maximum number of merge hunks to include in the output.
--
-- 'nextToken', 'describeMergeConflicts_nextToken' - An enumeration token that, when provided in a request, returns the next
-- batch of the results.
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
      { maxMergeHunks =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        conflictDetailLevel = Prelude.Nothing,
        conflictResolutionStrategy = Prelude.Nothing,
        repositoryName = pRepositoryName_,
        destinationCommitSpecifier =
          pDestinationCommitSpecifier_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        mergeOption = pMergeOption_,
        filePath = pFilePath_
      }

-- | The maximum number of merge hunks to include in the output.
describeMergeConflicts_maxMergeHunks :: Lens.Lens' DescribeMergeConflicts (Prelude.Maybe Prelude.Int)
describeMergeConflicts_maxMergeHunks = Lens.lens (\DescribeMergeConflicts' {maxMergeHunks} -> maxMergeHunks) (\s@DescribeMergeConflicts' {} a -> s {maxMergeHunks = a} :: DescribeMergeConflicts)

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
describeMergeConflicts_nextToken :: Lens.Lens' DescribeMergeConflicts (Prelude.Maybe Prelude.Text)
describeMergeConflicts_nextToken = Lens.lens (\DescribeMergeConflicts' {nextToken} -> nextToken) (\s@DescribeMergeConflicts' {} a -> s {nextToken = a} :: DescribeMergeConflicts)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMergeConflictsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "baseCommitId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "conflictMetadata")
            Prelude.<*> (x Core..?> "mergeHunks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..:> "destinationCommitId")
            Prelude.<*> (x Core..:> "sourceCommitId")
      )

instance Prelude.Hashable DescribeMergeConflicts

instance Prelude.NFData DescribeMergeConflicts

instance Core.ToHeaders DescribeMergeConflicts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.DescribeMergeConflicts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeMergeConflicts where
  toJSON DescribeMergeConflicts' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("maxMergeHunks" Core..=) Prelude.<$> maxMergeHunks,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("conflictDetailLevel" Core..=)
              Prelude.<$> conflictDetailLevel,
            ("conflictResolutionStrategy" Core..=)
              Prelude.<$> conflictResolutionStrategy,
            Prelude.Just
              ("repositoryName" Core..= repositoryName),
            Prelude.Just
              ( "destinationCommitSpecifier"
                  Core..= destinationCommitSpecifier
              ),
            Prelude.Just
              ( "sourceCommitSpecifier"
                  Core..= sourceCommitSpecifier
              ),
            Prelude.Just ("mergeOption" Core..= mergeOption),
            Prelude.Just ("filePath" Core..= filePath)
          ]
      )

instance Core.ToPath DescribeMergeConflicts where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeMergeConflicts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMergeConflictsResponse' smart constructor.
data DescribeMergeConflictsResponse = DescribeMergeConflictsResponse'
  { -- | An enumeration token that can be used in a request to return the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The commit ID of the merge base.
    baseCommitId :: Prelude.Maybe Prelude.Text,
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
-- 'nextToken', 'describeMergeConflictsResponse_nextToken' - An enumeration token that can be used in a request to return the next
-- batch of the results.
--
-- 'baseCommitId', 'describeMergeConflictsResponse_baseCommitId' - The commit ID of the merge base.
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
      { nextToken =
          Prelude.Nothing,
        baseCommitId = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        conflictMetadata = pConflictMetadata_,
        mergeHunks = Prelude.mempty,
        destinationCommitId = pDestinationCommitId_,
        sourceCommitId = pSourceCommitId_
      }

-- | An enumeration token that can be used in a request to return the next
-- batch of the results.
describeMergeConflictsResponse_nextToken :: Lens.Lens' DescribeMergeConflictsResponse (Prelude.Maybe Prelude.Text)
describeMergeConflictsResponse_nextToken = Lens.lens (\DescribeMergeConflictsResponse' {nextToken} -> nextToken) (\s@DescribeMergeConflictsResponse' {} a -> s {nextToken = a} :: DescribeMergeConflictsResponse)

-- | The commit ID of the merge base.
describeMergeConflictsResponse_baseCommitId :: Lens.Lens' DescribeMergeConflictsResponse (Prelude.Maybe Prelude.Text)
describeMergeConflictsResponse_baseCommitId = Lens.lens (\DescribeMergeConflictsResponse' {baseCommitId} -> baseCommitId) (\s@DescribeMergeConflictsResponse' {} a -> s {baseCommitId = a} :: DescribeMergeConflictsResponse)

-- | The response's http status code.
describeMergeConflictsResponse_httpStatus :: Lens.Lens' DescribeMergeConflictsResponse Prelude.Int
describeMergeConflictsResponse_httpStatus = Lens.lens (\DescribeMergeConflictsResponse' {httpStatus} -> httpStatus) (\s@DescribeMergeConflictsResponse' {} a -> s {httpStatus = a} :: DescribeMergeConflictsResponse)

-- | Contains metadata about the conflicts found in the merge.
describeMergeConflictsResponse_conflictMetadata :: Lens.Lens' DescribeMergeConflictsResponse ConflictMetadata
describeMergeConflictsResponse_conflictMetadata = Lens.lens (\DescribeMergeConflictsResponse' {conflictMetadata} -> conflictMetadata) (\s@DescribeMergeConflictsResponse' {} a -> s {conflictMetadata = a} :: DescribeMergeConflictsResponse)

-- | A list of merge hunks of the differences between the files or lines.
describeMergeConflictsResponse_mergeHunks :: Lens.Lens' DescribeMergeConflictsResponse [MergeHunk]
describeMergeConflictsResponse_mergeHunks = Lens.lens (\DescribeMergeConflictsResponse' {mergeHunks} -> mergeHunks) (\s@DescribeMergeConflictsResponse' {} a -> s {mergeHunks = a} :: DescribeMergeConflictsResponse) Prelude.. Lens._Coerce

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
