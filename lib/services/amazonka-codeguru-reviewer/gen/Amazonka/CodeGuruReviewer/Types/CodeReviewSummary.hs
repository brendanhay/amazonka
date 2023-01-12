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
-- Module      : Amazonka.CodeGuruReviewer.Types.CodeReviewSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.CodeReviewSummary where

import Amazonka.CodeGuruReviewer.Types.JobState
import Amazonka.CodeGuruReviewer.Types.MetricsSummary
import Amazonka.CodeGuruReviewer.Types.ProviderType
import Amazonka.CodeGuruReviewer.Types.SourceCodeType
import Amazonka.CodeGuruReviewer.Types.Type
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the summary of the code review.
--
-- /See:/ 'newCodeReviewSummary' smart constructor.
data CodeReviewSummary = CodeReviewSummary'
  { -- | The Amazon Resource Name (ARN) of the
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
    -- object.
    codeReviewArn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the code review was
    -- created.
    createdTimeStamp :: Prelude.Maybe Data.POSIX,
    -- | The time, in milliseconds since the epoch, when the code review was last
    -- updated.
    lastUpdatedTimeStamp :: Prelude.Maybe Data.POSIX,
    -- | The statistics from the code review.
    metricsSummary :: Prelude.Maybe MetricsSummary,
    -- | The name of the code review.
    name :: Prelude.Maybe Prelude.Text,
    -- | The owner of the repository. For an Amazon Web Services CodeCommit
    -- repository, this is the Amazon Web Services account ID of the account
    -- that owns the repository. For a GitHub, GitHub Enterprise Server, or
    -- Bitbucket repository, this is the username for the account that owns the
    -- repository. For an S3 repository, it can be the username or Amazon Web
    -- Services account ID.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The provider type of the repository association.
    providerType :: Prelude.Maybe ProviderType,
    -- | The pull request ID for the code review.
    pullRequestId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository.
    repositoryName :: Prelude.Maybe Prelude.Text,
    sourceCodeType :: Prelude.Maybe SourceCodeType,
    -- | The state of the code review.
    --
    -- The valid code review states are:
    --
    -- -   @Completed@: The code review is complete.
    --
    -- -   @Pending@: The code review started and has not completed or failed.
    --
    -- -   @Failed@: The code review failed.
    --
    -- -   @Deleting@: The code review is being deleted.
    state :: Prelude.Maybe JobState,
    -- | The type of the code review.
    type' :: Prelude.Maybe Type
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeReviewSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeReviewArn', 'codeReviewSummary_codeReviewArn' - The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
--
-- 'createdTimeStamp', 'codeReviewSummary_createdTimeStamp' - The time, in milliseconds since the epoch, when the code review was
-- created.
--
-- 'lastUpdatedTimeStamp', 'codeReviewSummary_lastUpdatedTimeStamp' - The time, in milliseconds since the epoch, when the code review was last
-- updated.
--
-- 'metricsSummary', 'codeReviewSummary_metricsSummary' - The statistics from the code review.
--
-- 'name', 'codeReviewSummary_name' - The name of the code review.
--
-- 'owner', 'codeReviewSummary_owner' - The owner of the repository. For an Amazon Web Services CodeCommit
-- repository, this is the Amazon Web Services account ID of the account
-- that owns the repository. For a GitHub, GitHub Enterprise Server, or
-- Bitbucket repository, this is the username for the account that owns the
-- repository. For an S3 repository, it can be the username or Amazon Web
-- Services account ID.
--
-- 'providerType', 'codeReviewSummary_providerType' - The provider type of the repository association.
--
-- 'pullRequestId', 'codeReviewSummary_pullRequestId' - The pull request ID for the code review.
--
-- 'repositoryName', 'codeReviewSummary_repositoryName' - The name of the repository.
--
-- 'sourceCodeType', 'codeReviewSummary_sourceCodeType' - Undocumented member.
--
-- 'state', 'codeReviewSummary_state' - The state of the code review.
--
-- The valid code review states are:
--
-- -   @Completed@: The code review is complete.
--
-- -   @Pending@: The code review started and has not completed or failed.
--
-- -   @Failed@: The code review failed.
--
-- -   @Deleting@: The code review is being deleted.
--
-- 'type'', 'codeReviewSummary_type' - The type of the code review.
newCodeReviewSummary ::
  CodeReviewSummary
newCodeReviewSummary =
  CodeReviewSummary'
    { codeReviewArn = Prelude.Nothing,
      createdTimeStamp = Prelude.Nothing,
      lastUpdatedTimeStamp = Prelude.Nothing,
      metricsSummary = Prelude.Nothing,
      name = Prelude.Nothing,
      owner = Prelude.Nothing,
      providerType = Prelude.Nothing,
      pullRequestId = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      sourceCodeType = Prelude.Nothing,
      state = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
codeReviewSummary_codeReviewArn :: Lens.Lens' CodeReviewSummary (Prelude.Maybe Prelude.Text)
codeReviewSummary_codeReviewArn = Lens.lens (\CodeReviewSummary' {codeReviewArn} -> codeReviewArn) (\s@CodeReviewSummary' {} a -> s {codeReviewArn = a} :: CodeReviewSummary)

-- | The time, in milliseconds since the epoch, when the code review was
-- created.
codeReviewSummary_createdTimeStamp :: Lens.Lens' CodeReviewSummary (Prelude.Maybe Prelude.UTCTime)
codeReviewSummary_createdTimeStamp = Lens.lens (\CodeReviewSummary' {createdTimeStamp} -> createdTimeStamp) (\s@CodeReviewSummary' {} a -> s {createdTimeStamp = a} :: CodeReviewSummary) Prelude.. Lens.mapping Data._Time

-- | The time, in milliseconds since the epoch, when the code review was last
-- updated.
codeReviewSummary_lastUpdatedTimeStamp :: Lens.Lens' CodeReviewSummary (Prelude.Maybe Prelude.UTCTime)
codeReviewSummary_lastUpdatedTimeStamp = Lens.lens (\CodeReviewSummary' {lastUpdatedTimeStamp} -> lastUpdatedTimeStamp) (\s@CodeReviewSummary' {} a -> s {lastUpdatedTimeStamp = a} :: CodeReviewSummary) Prelude.. Lens.mapping Data._Time

-- | The statistics from the code review.
codeReviewSummary_metricsSummary :: Lens.Lens' CodeReviewSummary (Prelude.Maybe MetricsSummary)
codeReviewSummary_metricsSummary = Lens.lens (\CodeReviewSummary' {metricsSummary} -> metricsSummary) (\s@CodeReviewSummary' {} a -> s {metricsSummary = a} :: CodeReviewSummary)

-- | The name of the code review.
codeReviewSummary_name :: Lens.Lens' CodeReviewSummary (Prelude.Maybe Prelude.Text)
codeReviewSummary_name = Lens.lens (\CodeReviewSummary' {name} -> name) (\s@CodeReviewSummary' {} a -> s {name = a} :: CodeReviewSummary)

-- | The owner of the repository. For an Amazon Web Services CodeCommit
-- repository, this is the Amazon Web Services account ID of the account
-- that owns the repository. For a GitHub, GitHub Enterprise Server, or
-- Bitbucket repository, this is the username for the account that owns the
-- repository. For an S3 repository, it can be the username or Amazon Web
-- Services account ID.
codeReviewSummary_owner :: Lens.Lens' CodeReviewSummary (Prelude.Maybe Prelude.Text)
codeReviewSummary_owner = Lens.lens (\CodeReviewSummary' {owner} -> owner) (\s@CodeReviewSummary' {} a -> s {owner = a} :: CodeReviewSummary)

-- | The provider type of the repository association.
codeReviewSummary_providerType :: Lens.Lens' CodeReviewSummary (Prelude.Maybe ProviderType)
codeReviewSummary_providerType = Lens.lens (\CodeReviewSummary' {providerType} -> providerType) (\s@CodeReviewSummary' {} a -> s {providerType = a} :: CodeReviewSummary)

-- | The pull request ID for the code review.
codeReviewSummary_pullRequestId :: Lens.Lens' CodeReviewSummary (Prelude.Maybe Prelude.Text)
codeReviewSummary_pullRequestId = Lens.lens (\CodeReviewSummary' {pullRequestId} -> pullRequestId) (\s@CodeReviewSummary' {} a -> s {pullRequestId = a} :: CodeReviewSummary)

-- | The name of the repository.
codeReviewSummary_repositoryName :: Lens.Lens' CodeReviewSummary (Prelude.Maybe Prelude.Text)
codeReviewSummary_repositoryName = Lens.lens (\CodeReviewSummary' {repositoryName} -> repositoryName) (\s@CodeReviewSummary' {} a -> s {repositoryName = a} :: CodeReviewSummary)

-- | Undocumented member.
codeReviewSummary_sourceCodeType :: Lens.Lens' CodeReviewSummary (Prelude.Maybe SourceCodeType)
codeReviewSummary_sourceCodeType = Lens.lens (\CodeReviewSummary' {sourceCodeType} -> sourceCodeType) (\s@CodeReviewSummary' {} a -> s {sourceCodeType = a} :: CodeReviewSummary)

-- | The state of the code review.
--
-- The valid code review states are:
--
-- -   @Completed@: The code review is complete.
--
-- -   @Pending@: The code review started and has not completed or failed.
--
-- -   @Failed@: The code review failed.
--
-- -   @Deleting@: The code review is being deleted.
codeReviewSummary_state :: Lens.Lens' CodeReviewSummary (Prelude.Maybe JobState)
codeReviewSummary_state = Lens.lens (\CodeReviewSummary' {state} -> state) (\s@CodeReviewSummary' {} a -> s {state = a} :: CodeReviewSummary)

-- | The type of the code review.
codeReviewSummary_type :: Lens.Lens' CodeReviewSummary (Prelude.Maybe Type)
codeReviewSummary_type = Lens.lens (\CodeReviewSummary' {type'} -> type') (\s@CodeReviewSummary' {} a -> s {type' = a} :: CodeReviewSummary)

instance Data.FromJSON CodeReviewSummary where
  parseJSON =
    Data.withObject
      "CodeReviewSummary"
      ( \x ->
          CodeReviewSummary'
            Prelude.<$> (x Data..:? "CodeReviewArn")
            Prelude.<*> (x Data..:? "CreatedTimeStamp")
            Prelude.<*> (x Data..:? "LastUpdatedTimeStamp")
            Prelude.<*> (x Data..:? "MetricsSummary")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Owner")
            Prelude.<*> (x Data..:? "ProviderType")
            Prelude.<*> (x Data..:? "PullRequestId")
            Prelude.<*> (x Data..:? "RepositoryName")
            Prelude.<*> (x Data..:? "SourceCodeType")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable CodeReviewSummary where
  hashWithSalt _salt CodeReviewSummary' {..} =
    _salt `Prelude.hashWithSalt` codeReviewArn
      `Prelude.hashWithSalt` createdTimeStamp
      `Prelude.hashWithSalt` lastUpdatedTimeStamp
      `Prelude.hashWithSalt` metricsSummary
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` providerType
      `Prelude.hashWithSalt` pullRequestId
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` sourceCodeType
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CodeReviewSummary where
  rnf CodeReviewSummary' {..} =
    Prelude.rnf codeReviewArn
      `Prelude.seq` Prelude.rnf createdTimeStamp
      `Prelude.seq` Prelude.rnf lastUpdatedTimeStamp
      `Prelude.seq` Prelude.rnf metricsSummary
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf providerType
      `Prelude.seq` Prelude.rnf pullRequestId
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf sourceCodeType
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf type'
