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
-- Module      : Network.AWS.CodeCommit.GetMergeCommit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified merge commit.
module Network.AWS.CodeCommit.GetMergeCommit
  ( -- * Creating a Request
    GetMergeCommit (..),
    newGetMergeCommit,

    -- * Request Lenses
    getMergeCommit_conflictDetailLevel,
    getMergeCommit_conflictResolutionStrategy,
    getMergeCommit_repositoryName,
    getMergeCommit_sourceCommitSpecifier,
    getMergeCommit_destinationCommitSpecifier,

    -- * Destructuring the Response
    GetMergeCommitResponse (..),
    newGetMergeCommitResponse,

    -- * Response Lenses
    getMergeCommitResponse_baseCommitId,
    getMergeCommitResponse_mergedCommitId,
    getMergeCommitResponse_sourceCommitId,
    getMergeCommitResponse_destinationCommitId,
    getMergeCommitResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMergeCommit' smart constructor.
data GetMergeCommit = GetMergeCommit'
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
    -- | The name of the repository that contains the merge commit about which
    -- you want to get information.
    repositoryName :: Prelude.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to
    -- identify a commit (for example, a branch name or a full commit ID).
    sourceCommitSpecifier :: Prelude.Text,
    -- | The branch, tag, HEAD, or other fully qualified reference used to
    -- identify a commit (for example, a branch name or a full commit ID).
    destinationCommitSpecifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetMergeCommit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conflictDetailLevel', 'getMergeCommit_conflictDetailLevel' - The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
--
-- 'conflictResolutionStrategy', 'getMergeCommit_conflictResolutionStrategy' - Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
--
-- 'repositoryName', 'getMergeCommit_repositoryName' - The name of the repository that contains the merge commit about which
-- you want to get information.
--
-- 'sourceCommitSpecifier', 'getMergeCommit_sourceCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
--
-- 'destinationCommitSpecifier', 'getMergeCommit_destinationCommitSpecifier' - The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
newGetMergeCommit ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'sourceCommitSpecifier'
  Prelude.Text ->
  -- | 'destinationCommitSpecifier'
  Prelude.Text ->
  GetMergeCommit
newGetMergeCommit
  pRepositoryName_
  pSourceCommitSpecifier_
  pDestinationCommitSpecifier_ =
    GetMergeCommit'
      { conflictDetailLevel =
          Prelude.Nothing,
        conflictResolutionStrategy = Prelude.Nothing,
        repositoryName = pRepositoryName_,
        sourceCommitSpecifier = pSourceCommitSpecifier_,
        destinationCommitSpecifier =
          pDestinationCommitSpecifier_
      }

-- | The level of conflict detail to use. If unspecified, the default
-- FILE_LEVEL is used, which returns a not-mergeable result if the same
-- file has differences in both branches. If LINE_LEVEL is specified, a
-- conflict is considered not mergeable if the same file in both branches
-- has differences on the same line.
getMergeCommit_conflictDetailLevel :: Lens.Lens' GetMergeCommit (Prelude.Maybe ConflictDetailLevelTypeEnum)
getMergeCommit_conflictDetailLevel = Lens.lens (\GetMergeCommit' {conflictDetailLevel} -> conflictDetailLevel) (\s@GetMergeCommit' {} a -> s {conflictDetailLevel = a} :: GetMergeCommit)

-- | Specifies which branch to use when resolving conflicts, or whether to
-- attempt automatically merging two versions of a file. The default is
-- NONE, which requires any conflicts to be resolved manually before the
-- merge operation is successful.
getMergeCommit_conflictResolutionStrategy :: Lens.Lens' GetMergeCommit (Prelude.Maybe ConflictResolutionStrategyTypeEnum)
getMergeCommit_conflictResolutionStrategy = Lens.lens (\GetMergeCommit' {conflictResolutionStrategy} -> conflictResolutionStrategy) (\s@GetMergeCommit' {} a -> s {conflictResolutionStrategy = a} :: GetMergeCommit)

-- | The name of the repository that contains the merge commit about which
-- you want to get information.
getMergeCommit_repositoryName :: Lens.Lens' GetMergeCommit Prelude.Text
getMergeCommit_repositoryName = Lens.lens (\GetMergeCommit' {repositoryName} -> repositoryName) (\s@GetMergeCommit' {} a -> s {repositoryName = a} :: GetMergeCommit)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
getMergeCommit_sourceCommitSpecifier :: Lens.Lens' GetMergeCommit Prelude.Text
getMergeCommit_sourceCommitSpecifier = Lens.lens (\GetMergeCommit' {sourceCommitSpecifier} -> sourceCommitSpecifier) (\s@GetMergeCommit' {} a -> s {sourceCommitSpecifier = a} :: GetMergeCommit)

-- | The branch, tag, HEAD, or other fully qualified reference used to
-- identify a commit (for example, a branch name or a full commit ID).
getMergeCommit_destinationCommitSpecifier :: Lens.Lens' GetMergeCommit Prelude.Text
getMergeCommit_destinationCommitSpecifier = Lens.lens (\GetMergeCommit' {destinationCommitSpecifier} -> destinationCommitSpecifier) (\s@GetMergeCommit' {} a -> s {destinationCommitSpecifier = a} :: GetMergeCommit)

instance Prelude.AWSRequest GetMergeCommit where
  type Rs GetMergeCommit = GetMergeCommitResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMergeCommitResponse'
            Prelude.<$> (x Prelude..?> "baseCommitId")
            Prelude.<*> (x Prelude..?> "mergedCommitId")
            Prelude.<*> (x Prelude..?> "sourceCommitId")
            Prelude.<*> (x Prelude..?> "destinationCommitId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMergeCommit

instance Prelude.NFData GetMergeCommit

instance Prelude.ToHeaders GetMergeCommit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.GetMergeCommit" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetMergeCommit where
  toJSON GetMergeCommit' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("conflictDetailLevel" Prelude..=)
              Prelude.<$> conflictDetailLevel,
            ("conflictResolutionStrategy" Prelude..=)
              Prelude.<$> conflictResolutionStrategy,
            Prelude.Just
              ("repositoryName" Prelude..= repositoryName),
            Prelude.Just
              ( "sourceCommitSpecifier"
                  Prelude..= sourceCommitSpecifier
              ),
            Prelude.Just
              ( "destinationCommitSpecifier"
                  Prelude..= destinationCommitSpecifier
              )
          ]
      )

instance Prelude.ToPath GetMergeCommit where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetMergeCommit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMergeCommitResponse' smart constructor.
data GetMergeCommitResponse = GetMergeCommitResponse'
  { -- | The commit ID of the merge base.
    baseCommitId :: Prelude.Maybe Prelude.Text,
    -- | The commit ID for the merge commit created when the source branch was
    -- merged into the destination branch. If the fast-forward merge strategy
    -- was used, there is no merge commit.
    mergedCommitId :: Prelude.Maybe Prelude.Text,
    -- | The commit ID of the source commit specifier that was used in the merge
    -- evaluation.
    sourceCommitId :: Prelude.Maybe Prelude.Text,
    -- | The commit ID of the destination commit specifier that was used in the
    -- merge evaluation.
    destinationCommitId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetMergeCommitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseCommitId', 'getMergeCommitResponse_baseCommitId' - The commit ID of the merge base.
--
-- 'mergedCommitId', 'getMergeCommitResponse_mergedCommitId' - The commit ID for the merge commit created when the source branch was
-- merged into the destination branch. If the fast-forward merge strategy
-- was used, there is no merge commit.
--
-- 'sourceCommitId', 'getMergeCommitResponse_sourceCommitId' - The commit ID of the source commit specifier that was used in the merge
-- evaluation.
--
-- 'destinationCommitId', 'getMergeCommitResponse_destinationCommitId' - The commit ID of the destination commit specifier that was used in the
-- merge evaluation.
--
-- 'httpStatus', 'getMergeCommitResponse_httpStatus' - The response's http status code.
newGetMergeCommitResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMergeCommitResponse
newGetMergeCommitResponse pHttpStatus_ =
  GetMergeCommitResponse'
    { baseCommitId =
        Prelude.Nothing,
      mergedCommitId = Prelude.Nothing,
      sourceCommitId = Prelude.Nothing,
      destinationCommitId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The commit ID of the merge base.
getMergeCommitResponse_baseCommitId :: Lens.Lens' GetMergeCommitResponse (Prelude.Maybe Prelude.Text)
getMergeCommitResponse_baseCommitId = Lens.lens (\GetMergeCommitResponse' {baseCommitId} -> baseCommitId) (\s@GetMergeCommitResponse' {} a -> s {baseCommitId = a} :: GetMergeCommitResponse)

-- | The commit ID for the merge commit created when the source branch was
-- merged into the destination branch. If the fast-forward merge strategy
-- was used, there is no merge commit.
getMergeCommitResponse_mergedCommitId :: Lens.Lens' GetMergeCommitResponse (Prelude.Maybe Prelude.Text)
getMergeCommitResponse_mergedCommitId = Lens.lens (\GetMergeCommitResponse' {mergedCommitId} -> mergedCommitId) (\s@GetMergeCommitResponse' {} a -> s {mergedCommitId = a} :: GetMergeCommitResponse)

-- | The commit ID of the source commit specifier that was used in the merge
-- evaluation.
getMergeCommitResponse_sourceCommitId :: Lens.Lens' GetMergeCommitResponse (Prelude.Maybe Prelude.Text)
getMergeCommitResponse_sourceCommitId = Lens.lens (\GetMergeCommitResponse' {sourceCommitId} -> sourceCommitId) (\s@GetMergeCommitResponse' {} a -> s {sourceCommitId = a} :: GetMergeCommitResponse)

-- | The commit ID of the destination commit specifier that was used in the
-- merge evaluation.
getMergeCommitResponse_destinationCommitId :: Lens.Lens' GetMergeCommitResponse (Prelude.Maybe Prelude.Text)
getMergeCommitResponse_destinationCommitId = Lens.lens (\GetMergeCommitResponse' {destinationCommitId} -> destinationCommitId) (\s@GetMergeCommitResponse' {} a -> s {destinationCommitId = a} :: GetMergeCommitResponse)

-- | The response's http status code.
getMergeCommitResponse_httpStatus :: Lens.Lens' GetMergeCommitResponse Prelude.Int
getMergeCommitResponse_httpStatus = Lens.lens (\GetMergeCommitResponse' {httpStatus} -> httpStatus) (\s@GetMergeCommitResponse' {} a -> s {httpStatus = a} :: GetMergeCommitResponse)

instance Prelude.NFData GetMergeCommitResponse
