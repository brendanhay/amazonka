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
-- Module      : Network.AWS.CodeCommit.GetMergeConflicts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about merge conflicts between the before and after
-- commit IDs for a pull request in a repository.
module Network.AWS.CodeCommit.GetMergeConflicts
  ( -- * Creating a Request
    GetMergeConflicts (..),
    newGetMergeConflicts,

    -- * Request Lenses
    getMergeConflicts_conflictDetailLevel,
    getMergeConflicts_nextToken,
    getMergeConflicts_maxConflictFiles,
    getMergeConflicts_conflictResolutionStrategy,
    getMergeConflicts_repositoryName,
    getMergeConflicts_destinationCommitSpecifier,
    getMergeConflicts_sourceCommitSpecifier,
    getMergeConflicts_mergeOption,

    -- * Destructuring the Response
    GetMergeConflictsResponse (..),
    newGetMergeConflictsResponse,

    -- * Response Lenses
    getMergeConflictsResponse_baseCommitId,
    getMergeConflictsResponse_nextToken,
    getMergeConflictsResponse_httpStatus,
    getMergeConflictsResponse_mergeable,
    getMergeConflictsResponse_destinationCommitId,
    getMergeConflictsResponse_sourceCommitId,
    getMergeConflictsResponse_conflictMetadataList,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMergeConflicts' smart constructor.
data GetMergeConflicts = GetMergeConflicts'
  { -- | The level of conflict detail to use. If unspecified, the default
    -- FILE_LEVEL is used, which returns a not-mergeable result if the same
    -- file has differences in both branches. If LINE_LEVEL is specified, a
    -- conflict is considered not mergeable if the same file in both branches
    -- has differences on the same line.
    conflictDetailLevel :: Prelude.Maybe ConflictDetailLevelTypeEnum,
    -- | An enumeration token that, when provided in a request, returns the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of files to include in the output.
    maxConflictFiles :: Prelude.Maybe Prelude.Int,
    -- | Specifies which branch to use when resolving conflicts, or whether to
    -- attempt automatically merging two versions of a file. The default is
    -- NONE, which requires any conflicts to be resolved manually before the
    -- merge operation is successful.
    conflictResolutionStrategy :: Prelude.Maybe ConflictResolutionStrategyTypeEnum,
    -- | The name of the repository where the pull request was created.
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
-- Create a value of 'GetMergeConflicts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conflictDetailLevel', 'getMergeConflicts_conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
--
-- 'nextToken', 'getMergeConflicts_nextToken' - An enumeration token that, when provided in a request, returns the next
-- batch of the results.
--
-- 'maxConflictFiles', 'getMergeConflicts_maxConflictFiles' - The maximum number of files to include in the output.
--
-- 'conflictResolutionStrategy', 'getMergeConflicts_conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
--
-- 'repositoryName', 'getMergeConflicts_repositoryName' - The name of the repository where the pull request was created.
--
-- 'destinationCommitSpecifier', 'getMergeConflicts_destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
--
-- 'sourceCommitSpecifier', 'getMergeConflicts_sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
--
-- 'mergeOption', 'getMergeConflicts_mergeOption' - The merge option or strategy you want to use to merge the code.
newGetMergeConflicts ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'destinationCommitSpecifier'
  Prelude.Text ->
  -- | 'sourceCommitSpecifier'
  Prelude.Text ->
  -- | 'mergeOption'
  MergeOptionTypeEnum ->
  GetMergeConflicts
newGetMergeConflicts
  pRepositoryName_
  pDestinationCommitSpecifier_
  pSourceCommitSpecifier_
  pMergeOption_ =
    GetMergeConflicts'
      { conflictDetailLevel =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        maxConflictFiles = Prelude.Nothing,
        conflictResolutionStrategy = Prelude.Nothing,
        repositoryName = pRepositoryName_,
        destinationCommitSpecifier =
          pDestinationCommitSpecifier_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        mergeOption = pMergeOption_
      }

-- | The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
getMergeConflicts_conflictDetailLevel :: Lens.Lens' GetMergeConflicts (Prelude.Maybe ConflictDetailLevelTypeEnum)
getMergeConflicts_conflictDetailLevel = Lens.lens (\GetMergeConflicts' {conflictDetailLevel} -> conflictDetailLevel) (\s@GetMergeConflicts' {} a -> s {conflictDetailLevel = a} :: GetMergeConflicts)

-- | An enumeration token that, when provided in a request, returns the next
-- batch of the results.
getMergeConflicts_nextToken :: Lens.Lens' GetMergeConflicts (Prelude.Maybe Prelude.Text)
getMergeConflicts_nextToken = Lens.lens (\GetMergeConflicts' {nextToken} -> nextToken) (\s@GetMergeConflicts' {} a -> s {nextToken = a} :: GetMergeConflicts)

-- | The maximum number of files to include in the output.
getMergeConflicts_maxConflictFiles :: Lens.Lens' GetMergeConflicts (Prelude.Maybe Prelude.Int)
getMergeConflicts_maxConflictFiles = Lens.lens (\GetMergeConflicts' {maxConflictFiles} -> maxConflictFiles) (\s@GetMergeConflicts' {} a -> s {maxConflictFiles = a} :: GetMergeConflicts)

-- | Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
getMergeConflicts_conflictResolutionStrategy :: Lens.Lens' GetMergeConflicts (Prelude.Maybe ConflictResolutionStrategyTypeEnum)
getMergeConflicts_conflictResolutionStrategy = Lens.lens (\GetMergeConflicts' {conflictResolutionStrategy} -> conflictResolutionStrategy) (\s@GetMergeConflicts' {} a -> s {conflictResolutionStrategy = a} :: GetMergeConflicts)

-- | The name of the repository where the pull request was created.
getMergeConflicts_repositoryName :: Lens.Lens' GetMergeConflicts Prelude.Text
getMergeConflicts_repositoryName = Lens.lens (\GetMergeConflicts' {repositoryName} -> repositoryName) (\s@GetMergeConflicts' {} a -> s {repositoryName = a} :: GetMergeConflicts)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
getMergeConflicts_destinationCommitSpecifier :: Lens.Lens' GetMergeConflicts Prelude.Text
getMergeConflicts_destinationCommitSpecifier = Lens.lens (\GetMergeConflicts' {destinationCommitSpecifier} -> destinationCommitSpecifier) (\s@GetMergeConflicts' {} a -> s {destinationCommitSpecifier = a} :: GetMergeConflicts)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
getMergeConflicts_sourceCommitSpecifier :: Lens.Lens' GetMergeConflicts Prelude.Text
getMergeConflicts_sourceCommitSpecifier = Lens.lens (\GetMergeConflicts' {sourceCommitSpecifier} -> sourceCommitSpecifier) (\s@GetMergeConflicts' {} a -> s {sourceCommitSpecifier = a} :: GetMergeConflicts)

-- | The merge option or strategy you want to use to merge the code.
getMergeConflicts_mergeOption :: Lens.Lens' GetMergeConflicts MergeOptionTypeEnum
getMergeConflicts_mergeOption = Lens.lens (\GetMergeConflicts' {mergeOption} -> mergeOption) (\s@GetMergeConflicts' {} a -> s {mergeOption = a} :: GetMergeConflicts)

instance Core.AWSRequest GetMergeConflicts where
  type
    AWSResponse GetMergeConflicts =
      GetMergeConflictsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMergeConflictsResponse'
            Prelude.<$> (x Core..?> "baseCommitId")
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "mergeable")
            Prelude.<*> (x Core..:> "destinationCommitId")
            Prelude.<*> (x Core..:> "sourceCommitId")
            Prelude.<*> ( x Core..?> "conflictMetadataList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetMergeConflicts

instance Prelude.NFData GetMergeConflicts

instance Core.ToHeaders GetMergeConflicts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.GetMergeConflicts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetMergeConflicts where
  toJSON GetMergeConflicts' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("conflictDetailLevel" Core..=)
              Prelude.<$> conflictDetailLevel,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxConflictFiles" Core..=)
              Prelude.<$> maxConflictFiles,
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
            Prelude.Just ("mergeOption" Core..= mergeOption)
          ]
      )

instance Core.ToPath GetMergeConflicts where
  toPath = Prelude.const "/"

instance Core.ToQuery GetMergeConflicts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMergeConflictsResponse' smart constructor.
data GetMergeConflictsResponse = GetMergeConflictsResponse'
  { -- | The commit ID of the merge base.
    baseCommitId :: Prelude.Maybe Prelude.Text,
    -- | An enumeration token that can be used in a request to return the next
    -- batch of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A Boolean value that indicates whether the code is mergeable by the
    -- specified merge option.
    mergeable :: Prelude.Bool,
    -- | The commit ID of the destination commit specifier that was used in the
    -- merge evaluation.
    destinationCommitId :: Prelude.Text,
    -- | The commit ID of the source commit specifier that was used in the merge
    -- evaluation.
    sourceCommitId :: Prelude.Text,
    -- | A list of metadata for any conflicting files. If the specified merge
    -- strategy is FAST_FORWARD_MERGE, this list is always empty.
    conflictMetadataList :: [ConflictMetadata]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMergeConflictsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseCommitId', 'getMergeConflictsResponse_baseCommitId' - The commit ID of the merge base.
--
-- 'nextToken', 'getMergeConflictsResponse_nextToken' - An enumeration token that can be used in a request to return the next
-- batch of the results.
--
-- 'httpStatus', 'getMergeConflictsResponse_httpStatus' - The response's http status code.
--
-- 'mergeable', 'getMergeConflictsResponse_mergeable' - A Boolean value that indicates whether the code is mergeable by the
-- specified merge option.
--
-- 'destinationCommitId', 'getMergeConflictsResponse_destinationCommitId' - The commit ID of the destination commit specifier that was used in the
-- merge evaluation.
--
-- 'sourceCommitId', 'getMergeConflictsResponse_sourceCommitId' - The commit ID of the source commit specifier that was used in the merge
-- evaluation.
--
-- 'conflictMetadataList', 'getMergeConflictsResponse_conflictMetadataList' - A list of metadata for any conflicting files. If the specified merge
-- strategy is FAST_FORWARD_MERGE, this list is always empty.
newGetMergeConflictsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'mergeable'
  Prelude.Bool ->
  -- | 'destinationCommitId'
  Prelude.Text ->
  -- | 'sourceCommitId'
  Prelude.Text ->
  GetMergeConflictsResponse
newGetMergeConflictsResponse
  pHttpStatus_
  pMergeable_
  pDestinationCommitId_
  pSourceCommitId_ =
    GetMergeConflictsResponse'
      { baseCommitId =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        mergeable = pMergeable_,
        destinationCommitId = pDestinationCommitId_,
        sourceCommitId = pSourceCommitId_,
        conflictMetadataList = Prelude.mempty
      }

-- | The commit ID of the merge base.
getMergeConflictsResponse_baseCommitId :: Lens.Lens' GetMergeConflictsResponse (Prelude.Maybe Prelude.Text)
getMergeConflictsResponse_baseCommitId = Lens.lens (\GetMergeConflictsResponse' {baseCommitId} -> baseCommitId) (\s@GetMergeConflictsResponse' {} a -> s {baseCommitId = a} :: GetMergeConflictsResponse)

-- | An enumeration token that can be used in a request to return the next
-- batch of the results.
getMergeConflictsResponse_nextToken :: Lens.Lens' GetMergeConflictsResponse (Prelude.Maybe Prelude.Text)
getMergeConflictsResponse_nextToken = Lens.lens (\GetMergeConflictsResponse' {nextToken} -> nextToken) (\s@GetMergeConflictsResponse' {} a -> s {nextToken = a} :: GetMergeConflictsResponse)

-- | The response's http status code.
getMergeConflictsResponse_httpStatus :: Lens.Lens' GetMergeConflictsResponse Prelude.Int
getMergeConflictsResponse_httpStatus = Lens.lens (\GetMergeConflictsResponse' {httpStatus} -> httpStatus) (\s@GetMergeConflictsResponse' {} a -> s {httpStatus = a} :: GetMergeConflictsResponse)

-- | A Boolean value that indicates whether the code is mergeable by the
-- specified merge option.
getMergeConflictsResponse_mergeable :: Lens.Lens' GetMergeConflictsResponse Prelude.Bool
getMergeConflictsResponse_mergeable = Lens.lens (\GetMergeConflictsResponse' {mergeable} -> mergeable) (\s@GetMergeConflictsResponse' {} a -> s {mergeable = a} :: GetMergeConflictsResponse)

-- | The commit ID of the destination commit specifier that was used in the
-- merge evaluation.
getMergeConflictsResponse_destinationCommitId :: Lens.Lens' GetMergeConflictsResponse Prelude.Text
getMergeConflictsResponse_destinationCommitId = Lens.lens (\GetMergeConflictsResponse' {destinationCommitId} -> destinationCommitId) (\s@GetMergeConflictsResponse' {} a -> s {destinationCommitId = a} :: GetMergeConflictsResponse)

-- | The commit ID of the source commit specifier that was used in the merge
-- evaluation.
getMergeConflictsResponse_sourceCommitId :: Lens.Lens' GetMergeConflictsResponse Prelude.Text
getMergeConflictsResponse_sourceCommitId = Lens.lens (\GetMergeConflictsResponse' {sourceCommitId} -> sourceCommitId) (\s@GetMergeConflictsResponse' {} a -> s {sourceCommitId = a} :: GetMergeConflictsResponse)

-- | A list of metadata for any conflicting files. If the specified merge
-- strategy is FAST_FORWARD_MERGE, this list is always empty.
getMergeConflictsResponse_conflictMetadataList :: Lens.Lens' GetMergeConflictsResponse [ConflictMetadata]
getMergeConflictsResponse_conflictMetadataList = Lens.lens (\GetMergeConflictsResponse' {conflictMetadataList} -> conflictMetadataList) (\s@GetMergeConflictsResponse' {} a -> s {conflictMetadataList = a} :: GetMergeConflictsResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetMergeConflictsResponse
