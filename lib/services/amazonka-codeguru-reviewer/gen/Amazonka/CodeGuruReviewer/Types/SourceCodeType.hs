{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeGuruReviewer.Types.SourceCodeType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.SourceCodeType where

import Amazonka.CodeGuruReviewer.Types.BranchDiffSourceCodeType
import Amazonka.CodeGuruReviewer.Types.CommitDiffSourceCodeType
import Amazonka.CodeGuruReviewer.Types.RepositoryHeadSourceCodeType
import Amazonka.CodeGuruReviewer.Types.RequestMetadata
import Amazonka.CodeGuruReviewer.Types.S3BucketRepository
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the source code that is analyzed in a code review.
--
-- /See:/ 'newSourceCodeType' smart constructor.
data SourceCodeType = SourceCodeType'
  { -- | A type of
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
    -- that specifies a source branch name and a destination branch name in an
    -- associated repository.
    branchDiff :: Prelude.Maybe BranchDiffSourceCodeType,
    -- | A
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
    -- that specifies a commit diff created by a pull request on an associated
    -- repository.
    commitDiff :: Prelude.Maybe CommitDiffSourceCodeType,
    repositoryHead :: Prelude.Maybe RepositoryHeadSourceCodeType,
    -- | Metadata that is associated with a code review. This applies to any type
    -- of code review supported by CodeGuru Reviewer. The @RequestMetadaa@
    -- field captures any event metadata. For example, it might capture
    -- metadata associated with an event trigger, such as a push or a pull
    -- request.
    requestMetadata :: Prelude.Maybe RequestMetadata,
    -- | Information about an associated repository in an S3 bucket that includes
    -- its name and an @S3RepositoryDetails@ object. The @S3RepositoryDetails@
    -- object includes the name of an S3 bucket, an S3 key for a source code
    -- .zip file, and an S3 key for a build artifacts .zip file.
    -- @S3BucketRepository@ is required in
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
    -- for @S3BucketRepository@ based code reviews.
    s3BucketRepository :: Prelude.Maybe S3BucketRepository
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceCodeType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branchDiff', 'sourceCodeType_branchDiff' - A type of
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- that specifies a source branch name and a destination branch name in an
-- associated repository.
--
-- 'commitDiff', 'sourceCodeType_commitDiff' - A
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- that specifies a commit diff created by a pull request on an associated
-- repository.
--
-- 'repositoryHead', 'sourceCodeType_repositoryHead' - Undocumented member.
--
-- 'requestMetadata', 'sourceCodeType_requestMetadata' - Metadata that is associated with a code review. This applies to any type
-- of code review supported by CodeGuru Reviewer. The @RequestMetadaa@
-- field captures any event metadata. For example, it might capture
-- metadata associated with an event trigger, such as a push or a pull
-- request.
--
-- 's3BucketRepository', 'sourceCodeType_s3BucketRepository' - Information about an associated repository in an S3 bucket that includes
-- its name and an @S3RepositoryDetails@ object. The @S3RepositoryDetails@
-- object includes the name of an S3 bucket, an S3 key for a source code
-- .zip file, and an S3 key for a build artifacts .zip file.
-- @S3BucketRepository@ is required in
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- for @S3BucketRepository@ based code reviews.
newSourceCodeType ::
  SourceCodeType
newSourceCodeType =
  SourceCodeType'
    { branchDiff = Prelude.Nothing,
      commitDiff = Prelude.Nothing,
      repositoryHead = Prelude.Nothing,
      requestMetadata = Prelude.Nothing,
      s3BucketRepository = Prelude.Nothing
    }

-- | A type of
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- that specifies a source branch name and a destination branch name in an
-- associated repository.
sourceCodeType_branchDiff :: Lens.Lens' SourceCodeType (Prelude.Maybe BranchDiffSourceCodeType)
sourceCodeType_branchDiff = Lens.lens (\SourceCodeType' {branchDiff} -> branchDiff) (\s@SourceCodeType' {} a -> s {branchDiff = a} :: SourceCodeType)

-- | A
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- that specifies a commit diff created by a pull request on an associated
-- repository.
sourceCodeType_commitDiff :: Lens.Lens' SourceCodeType (Prelude.Maybe CommitDiffSourceCodeType)
sourceCodeType_commitDiff = Lens.lens (\SourceCodeType' {commitDiff} -> commitDiff) (\s@SourceCodeType' {} a -> s {commitDiff = a} :: SourceCodeType)

-- | Undocumented member.
sourceCodeType_repositoryHead :: Lens.Lens' SourceCodeType (Prelude.Maybe RepositoryHeadSourceCodeType)
sourceCodeType_repositoryHead = Lens.lens (\SourceCodeType' {repositoryHead} -> repositoryHead) (\s@SourceCodeType' {} a -> s {repositoryHead = a} :: SourceCodeType)

-- | Metadata that is associated with a code review. This applies to any type
-- of code review supported by CodeGuru Reviewer. The @RequestMetadaa@
-- field captures any event metadata. For example, it might capture
-- metadata associated with an event trigger, such as a push or a pull
-- request.
sourceCodeType_requestMetadata :: Lens.Lens' SourceCodeType (Prelude.Maybe RequestMetadata)
sourceCodeType_requestMetadata = Lens.lens (\SourceCodeType' {requestMetadata} -> requestMetadata) (\s@SourceCodeType' {} a -> s {requestMetadata = a} :: SourceCodeType)

-- | Information about an associated repository in an S3 bucket that includes
-- its name and an @S3RepositoryDetails@ object. The @S3RepositoryDetails@
-- object includes the name of an S3 bucket, an S3 key for a source code
-- .zip file, and an S3 key for a build artifacts .zip file.
-- @S3BucketRepository@ is required in
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- for @S3BucketRepository@ based code reviews.
sourceCodeType_s3BucketRepository :: Lens.Lens' SourceCodeType (Prelude.Maybe S3BucketRepository)
sourceCodeType_s3BucketRepository = Lens.lens (\SourceCodeType' {s3BucketRepository} -> s3BucketRepository) (\s@SourceCodeType' {} a -> s {s3BucketRepository = a} :: SourceCodeType)

instance Data.FromJSON SourceCodeType where
  parseJSON =
    Data.withObject
      "SourceCodeType"
      ( \x ->
          SourceCodeType'
            Prelude.<$> (x Data..:? "BranchDiff")
            Prelude.<*> (x Data..:? "CommitDiff")
            Prelude.<*> (x Data..:? "RepositoryHead")
            Prelude.<*> (x Data..:? "RequestMetadata")
            Prelude.<*> (x Data..:? "S3BucketRepository")
      )

instance Prelude.Hashable SourceCodeType where
  hashWithSalt _salt SourceCodeType' {..} =
    _salt
      `Prelude.hashWithSalt` branchDiff
      `Prelude.hashWithSalt` commitDiff
      `Prelude.hashWithSalt` repositoryHead
      `Prelude.hashWithSalt` requestMetadata
      `Prelude.hashWithSalt` s3BucketRepository

instance Prelude.NFData SourceCodeType where
  rnf SourceCodeType' {..} =
    Prelude.rnf branchDiff
      `Prelude.seq` Prelude.rnf commitDiff
      `Prelude.seq` Prelude.rnf repositoryHead
      `Prelude.seq` Prelude.rnf requestMetadata
      `Prelude.seq` Prelude.rnf s3BucketRepository

instance Data.ToJSON SourceCodeType where
  toJSON SourceCodeType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BranchDiff" Data..=) Prelude.<$> branchDiff,
            ("CommitDiff" Data..=) Prelude.<$> commitDiff,
            ("RepositoryHead" Data..=)
              Prelude.<$> repositoryHead,
            ("RequestMetadata" Data..=)
              Prelude.<$> requestMetadata,
            ("S3BucketRepository" Data..=)
              Prelude.<$> s3BucketRepository
          ]
      )
