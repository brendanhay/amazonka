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
-- Module      : Network.AWS.CodeGuruReviewer.Types.SourceCodeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeGuruReviewer.Types.SourceCodeType where

import Network.AWS.CodeGuruReviewer.Types.BranchDiffSourceCodeType
import Network.AWS.CodeGuruReviewer.Types.CommitDiffSourceCodeType
import Network.AWS.CodeGuruReviewer.Types.RepositoryHeadSourceCodeType
import Network.AWS.CodeGuruReviewer.Types.RequestMetadata
import Network.AWS.CodeGuruReviewer.Types.S3BucketRepository
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the source code that is analyzed in a code review.
--
-- /See:/ 'newSourceCodeType' smart constructor.
data SourceCodeType = SourceCodeType'
  { -- | Information about an associated repository in an S3 bucket that includes
    -- its name and an @S3RepositoryDetails@ object. The @S3RepositoryDetails@
    -- object includes the name of an S3 bucket, an S3 key for a source code
    -- .zip file, and an S3 key for a build artifacts .zip file.
    -- @S3BucketRepository@ is required in
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
    -- for @S3BucketRepository@ based code reviews.
    s3BucketRepository :: Prelude.Maybe S3BucketRepository,
    -- | Metadata that is associated with a code review. This applies to any type
    -- of code review supported by CodeGuru Reviewer. The @RequestMetadaa@
    -- field captures any event metadata. For example, it might capture
    -- metadata associated with an event trigger, such as a push or a pull
    -- request.
    requestMetadata :: Prelude.Maybe RequestMetadata,
    repositoryHead :: Prelude.Maybe RepositoryHeadSourceCodeType,
    -- | A
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
    -- that specifies a commit diff created by a pull request on an associated
    -- repository.
    commitDiff :: Prelude.Maybe CommitDiffSourceCodeType,
    -- | A type of
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
    -- that specifies a source branch name and a destination branch name in an
    -- associated repository.
    branchDiff :: Prelude.Maybe BranchDiffSourceCodeType
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
-- 's3BucketRepository', 'sourceCodeType_s3BucketRepository' - Information about an associated repository in an S3 bucket that includes
-- its name and an @S3RepositoryDetails@ object. The @S3RepositoryDetails@
-- object includes the name of an S3 bucket, an S3 key for a source code
-- .zip file, and an S3 key for a build artifacts .zip file.
-- @S3BucketRepository@ is required in
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- for @S3BucketRepository@ based code reviews.
--
-- 'requestMetadata', 'sourceCodeType_requestMetadata' - Metadata that is associated with a code review. This applies to any type
-- of code review supported by CodeGuru Reviewer. The @RequestMetadaa@
-- field captures any event metadata. For example, it might capture
-- metadata associated with an event trigger, such as a push or a pull
-- request.
--
-- 'repositoryHead', 'sourceCodeType_repositoryHead' - Undocumented member.
--
-- 'commitDiff', 'sourceCodeType_commitDiff' - A
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- that specifies a commit diff created by a pull request on an associated
-- repository.
--
-- 'branchDiff', 'sourceCodeType_branchDiff' - A type of
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- that specifies a source branch name and a destination branch name in an
-- associated repository.
newSourceCodeType ::
  SourceCodeType
newSourceCodeType =
  SourceCodeType'
    { s3BucketRepository =
        Prelude.Nothing,
      requestMetadata = Prelude.Nothing,
      repositoryHead = Prelude.Nothing,
      commitDiff = Prelude.Nothing,
      branchDiff = Prelude.Nothing
    }

-- | Information about an associated repository in an S3 bucket that includes
-- its name and an @S3RepositoryDetails@ object. The @S3RepositoryDetails@
-- object includes the name of an S3 bucket, an S3 key for a source code
-- .zip file, and an S3 key for a build artifacts .zip file.
-- @S3BucketRepository@ is required in
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- for @S3BucketRepository@ based code reviews.
sourceCodeType_s3BucketRepository :: Lens.Lens' SourceCodeType (Prelude.Maybe S3BucketRepository)
sourceCodeType_s3BucketRepository = Lens.lens (\SourceCodeType' {s3BucketRepository} -> s3BucketRepository) (\s@SourceCodeType' {} a -> s {s3BucketRepository = a} :: SourceCodeType)

-- | Metadata that is associated with a code review. This applies to any type
-- of code review supported by CodeGuru Reviewer. The @RequestMetadaa@
-- field captures any event metadata. For example, it might capture
-- metadata associated with an event trigger, such as a push or a pull
-- request.
sourceCodeType_requestMetadata :: Lens.Lens' SourceCodeType (Prelude.Maybe RequestMetadata)
sourceCodeType_requestMetadata = Lens.lens (\SourceCodeType' {requestMetadata} -> requestMetadata) (\s@SourceCodeType' {} a -> s {requestMetadata = a} :: SourceCodeType)

-- | Undocumented member.
sourceCodeType_repositoryHead :: Lens.Lens' SourceCodeType (Prelude.Maybe RepositoryHeadSourceCodeType)
sourceCodeType_repositoryHead = Lens.lens (\SourceCodeType' {repositoryHead} -> repositoryHead) (\s@SourceCodeType' {} a -> s {repositoryHead = a} :: SourceCodeType)

-- | A
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- that specifies a commit diff created by a pull request on an associated
-- repository.
sourceCodeType_commitDiff :: Lens.Lens' SourceCodeType (Prelude.Maybe CommitDiffSourceCodeType)
sourceCodeType_commitDiff = Lens.lens (\SourceCodeType' {commitDiff} -> commitDiff) (\s@SourceCodeType' {} a -> s {commitDiff = a} :: SourceCodeType)

-- | A type of
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- that specifies a source branch name and a destination branch name in an
-- associated repository.
sourceCodeType_branchDiff :: Lens.Lens' SourceCodeType (Prelude.Maybe BranchDiffSourceCodeType)
sourceCodeType_branchDiff = Lens.lens (\SourceCodeType' {branchDiff} -> branchDiff) (\s@SourceCodeType' {} a -> s {branchDiff = a} :: SourceCodeType)

instance Core.FromJSON SourceCodeType where
  parseJSON =
    Core.withObject
      "SourceCodeType"
      ( \x ->
          SourceCodeType'
            Prelude.<$> (x Core..:? "S3BucketRepository")
            Prelude.<*> (x Core..:? "RequestMetadata")
            Prelude.<*> (x Core..:? "RepositoryHead")
            Prelude.<*> (x Core..:? "CommitDiff")
            Prelude.<*> (x Core..:? "BranchDiff")
      )

instance Prelude.Hashable SourceCodeType

instance Prelude.NFData SourceCodeType

instance Core.ToJSON SourceCodeType where
  toJSON SourceCodeType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("S3BucketRepository" Core..=)
              Prelude.<$> s3BucketRepository,
            ("RequestMetadata" Core..=)
              Prelude.<$> requestMetadata,
            ("RepositoryHead" Core..=)
              Prelude.<$> repositoryHead,
            ("CommitDiff" Core..=) Prelude.<$> commitDiff,
            ("BranchDiff" Core..=) Prelude.<$> branchDiff
          ]
      )
