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
-- Module      : Amazonka.CodeGuruReviewer.Types.CodeReview
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.CodeReview where

import Amazonka.CodeGuruReviewer.Types.AnalysisType
import Amazonka.CodeGuruReviewer.Types.ConfigFileState
import Amazonka.CodeGuruReviewer.Types.JobState
import Amazonka.CodeGuruReviewer.Types.Metrics
import Amazonka.CodeGuruReviewer.Types.ProviderType
import Amazonka.CodeGuruReviewer.Types.SourceCodeType
import Amazonka.CodeGuruReviewer.Types.Type
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a code review. A code review belongs to the associated
-- repository that contains the reviewed code.
--
-- /See:/ 'newCodeReview' smart constructor.
data CodeReview = CodeReview'
  { -- | The time, in milliseconds since the epoch, when the code review was last
    -- updated.
    lastUpdatedTimeStamp :: Prelude.Maybe Core.POSIX,
    -- | The name of the code review.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of code review.
    type' :: Prelude.Maybe Type,
    -- | The pull request ID for the code review.
    pullRequestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociation.html RepositoryAssociation>
    -- that contains the reviewed source code. You can retrieve associated
    -- repository ARNs by calling
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_ListRepositoryAssociations.html ListRepositoryAssociations>.
    associationArn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the code review was
    -- created.
    createdTimeStamp :: Prelude.Maybe Core.POSIX,
    -- | The type of the source code for the code review.
    sourceCodeType :: Prelude.Maybe SourceCodeType,
    -- | The name of the repository.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The valid code review states are:
    --
    -- -   @Completed@: The code review is complete.
    --
    -- -   @Pending@: The code review started and has not completed or failed.
    --
    -- -   @Failed@: The code review failed.
    --
    -- -   @Deleting@: The code review is being deleted.
    state :: Prelude.Maybe JobState,
    -- | The Amazon Resource Name (ARN) of the
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
    -- object.
    codeReviewArn :: Prelude.Maybe Prelude.Text,
    -- | The owner of the repository. For an Amazon Web Services CodeCommit
    -- repository, this is the Amazon Web Services account ID of the account
    -- that owns the repository. For a GitHub, GitHub Enterprise Server, or
    -- Bitbucket repository, this is the username for the account that owns the
    -- repository. For an S3 repository, it can be the username or Amazon Web
    -- Services account ID.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The statistics from the code review.
    metrics :: Prelude.Maybe Metrics,
    -- | The type of repository that contains the reviewed code (for example,
    -- GitHub or Bitbucket).
    providerType :: Prelude.Maybe ProviderType,
    -- | The state of the @aws-codeguru-reviewer.yml@ configuration file that
    -- allows the configuration of the CodeGuru Reviewer analysis. The file
    -- either exists, doesn\'t exist, or exists with errors at the root
    -- directory of your repository.
    configFileState :: Prelude.Maybe ConfigFileState,
    -- | The reason for the state of the code review.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | The types of analysis performed during a repository analysis or a pull
    -- request review. You can specify either @Security@, @CodeQuality@, or
    -- both.
    analysisTypes :: Prelude.Maybe [AnalysisType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeReview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTimeStamp', 'codeReview_lastUpdatedTimeStamp' - The time, in milliseconds since the epoch, when the code review was last
-- updated.
--
-- 'name', 'codeReview_name' - The name of the code review.
--
-- 'type'', 'codeReview_type' - The type of code review.
--
-- 'pullRequestId', 'codeReview_pullRequestId' - The pull request ID for the code review.
--
-- 'associationArn', 'codeReview_associationArn' - The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociation.html RepositoryAssociation>
-- that contains the reviewed source code. You can retrieve associated
-- repository ARNs by calling
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_ListRepositoryAssociations.html ListRepositoryAssociations>.
--
-- 'createdTimeStamp', 'codeReview_createdTimeStamp' - The time, in milliseconds since the epoch, when the code review was
-- created.
--
-- 'sourceCodeType', 'codeReview_sourceCodeType' - The type of the source code for the code review.
--
-- 'repositoryName', 'codeReview_repositoryName' - The name of the repository.
--
-- 'state', 'codeReview_state' - The valid code review states are:
--
-- -   @Completed@: The code review is complete.
--
-- -   @Pending@: The code review started and has not completed or failed.
--
-- -   @Failed@: The code review failed.
--
-- -   @Deleting@: The code review is being deleted.
--
-- 'codeReviewArn', 'codeReview_codeReviewArn' - The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
--
-- 'owner', 'codeReview_owner' - The owner of the repository. For an Amazon Web Services CodeCommit
-- repository, this is the Amazon Web Services account ID of the account
-- that owns the repository. For a GitHub, GitHub Enterprise Server, or
-- Bitbucket repository, this is the username for the account that owns the
-- repository. For an S3 repository, it can be the username or Amazon Web
-- Services account ID.
--
-- 'metrics', 'codeReview_metrics' - The statistics from the code review.
--
-- 'providerType', 'codeReview_providerType' - The type of repository that contains the reviewed code (for example,
-- GitHub or Bitbucket).
--
-- 'configFileState', 'codeReview_configFileState' - The state of the @aws-codeguru-reviewer.yml@ configuration file that
-- allows the configuration of the CodeGuru Reviewer analysis. The file
-- either exists, doesn\'t exist, or exists with errors at the root
-- directory of your repository.
--
-- 'stateReason', 'codeReview_stateReason' - The reason for the state of the code review.
--
-- 'analysisTypes', 'codeReview_analysisTypes' - The types of analysis performed during a repository analysis or a pull
-- request review. You can specify either @Security@, @CodeQuality@, or
-- both.
newCodeReview ::
  CodeReview
newCodeReview =
  CodeReview'
    { lastUpdatedTimeStamp = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      pullRequestId = Prelude.Nothing,
      associationArn = Prelude.Nothing,
      createdTimeStamp = Prelude.Nothing,
      sourceCodeType = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      state = Prelude.Nothing,
      codeReviewArn = Prelude.Nothing,
      owner = Prelude.Nothing,
      metrics = Prelude.Nothing,
      providerType = Prelude.Nothing,
      configFileState = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      analysisTypes = Prelude.Nothing
    }

-- | The time, in milliseconds since the epoch, when the code review was last
-- updated.
codeReview_lastUpdatedTimeStamp :: Lens.Lens' CodeReview (Prelude.Maybe Prelude.UTCTime)
codeReview_lastUpdatedTimeStamp = Lens.lens (\CodeReview' {lastUpdatedTimeStamp} -> lastUpdatedTimeStamp) (\s@CodeReview' {} a -> s {lastUpdatedTimeStamp = a} :: CodeReview) Prelude.. Lens.mapping Core._Time

-- | The name of the code review.
codeReview_name :: Lens.Lens' CodeReview (Prelude.Maybe Prelude.Text)
codeReview_name = Lens.lens (\CodeReview' {name} -> name) (\s@CodeReview' {} a -> s {name = a} :: CodeReview)

-- | The type of code review.
codeReview_type :: Lens.Lens' CodeReview (Prelude.Maybe Type)
codeReview_type = Lens.lens (\CodeReview' {type'} -> type') (\s@CodeReview' {} a -> s {type' = a} :: CodeReview)

-- | The pull request ID for the code review.
codeReview_pullRequestId :: Lens.Lens' CodeReview (Prelude.Maybe Prelude.Text)
codeReview_pullRequestId = Lens.lens (\CodeReview' {pullRequestId} -> pullRequestId) (\s@CodeReview' {} a -> s {pullRequestId = a} :: CodeReview)

-- | The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociation.html RepositoryAssociation>
-- that contains the reviewed source code. You can retrieve associated
-- repository ARNs by calling
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_ListRepositoryAssociations.html ListRepositoryAssociations>.
codeReview_associationArn :: Lens.Lens' CodeReview (Prelude.Maybe Prelude.Text)
codeReview_associationArn = Lens.lens (\CodeReview' {associationArn} -> associationArn) (\s@CodeReview' {} a -> s {associationArn = a} :: CodeReview)

-- | The time, in milliseconds since the epoch, when the code review was
-- created.
codeReview_createdTimeStamp :: Lens.Lens' CodeReview (Prelude.Maybe Prelude.UTCTime)
codeReview_createdTimeStamp = Lens.lens (\CodeReview' {createdTimeStamp} -> createdTimeStamp) (\s@CodeReview' {} a -> s {createdTimeStamp = a} :: CodeReview) Prelude.. Lens.mapping Core._Time

-- | The type of the source code for the code review.
codeReview_sourceCodeType :: Lens.Lens' CodeReview (Prelude.Maybe SourceCodeType)
codeReview_sourceCodeType = Lens.lens (\CodeReview' {sourceCodeType} -> sourceCodeType) (\s@CodeReview' {} a -> s {sourceCodeType = a} :: CodeReview)

-- | The name of the repository.
codeReview_repositoryName :: Lens.Lens' CodeReview (Prelude.Maybe Prelude.Text)
codeReview_repositoryName = Lens.lens (\CodeReview' {repositoryName} -> repositoryName) (\s@CodeReview' {} a -> s {repositoryName = a} :: CodeReview)

-- | The valid code review states are:
--
-- -   @Completed@: The code review is complete.
--
-- -   @Pending@: The code review started and has not completed or failed.
--
-- -   @Failed@: The code review failed.
--
-- -   @Deleting@: The code review is being deleted.
codeReview_state :: Lens.Lens' CodeReview (Prelude.Maybe JobState)
codeReview_state = Lens.lens (\CodeReview' {state} -> state) (\s@CodeReview' {} a -> s {state = a} :: CodeReview)

-- | The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_CodeReview.html CodeReview>
-- object.
codeReview_codeReviewArn :: Lens.Lens' CodeReview (Prelude.Maybe Prelude.Text)
codeReview_codeReviewArn = Lens.lens (\CodeReview' {codeReviewArn} -> codeReviewArn) (\s@CodeReview' {} a -> s {codeReviewArn = a} :: CodeReview)

-- | The owner of the repository. For an Amazon Web Services CodeCommit
-- repository, this is the Amazon Web Services account ID of the account
-- that owns the repository. For a GitHub, GitHub Enterprise Server, or
-- Bitbucket repository, this is the username for the account that owns the
-- repository. For an S3 repository, it can be the username or Amazon Web
-- Services account ID.
codeReview_owner :: Lens.Lens' CodeReview (Prelude.Maybe Prelude.Text)
codeReview_owner = Lens.lens (\CodeReview' {owner} -> owner) (\s@CodeReview' {} a -> s {owner = a} :: CodeReview)

-- | The statistics from the code review.
codeReview_metrics :: Lens.Lens' CodeReview (Prelude.Maybe Metrics)
codeReview_metrics = Lens.lens (\CodeReview' {metrics} -> metrics) (\s@CodeReview' {} a -> s {metrics = a} :: CodeReview)

-- | The type of repository that contains the reviewed code (for example,
-- GitHub or Bitbucket).
codeReview_providerType :: Lens.Lens' CodeReview (Prelude.Maybe ProviderType)
codeReview_providerType = Lens.lens (\CodeReview' {providerType} -> providerType) (\s@CodeReview' {} a -> s {providerType = a} :: CodeReview)

-- | The state of the @aws-codeguru-reviewer.yml@ configuration file that
-- allows the configuration of the CodeGuru Reviewer analysis. The file
-- either exists, doesn\'t exist, or exists with errors at the root
-- directory of your repository.
codeReview_configFileState :: Lens.Lens' CodeReview (Prelude.Maybe ConfigFileState)
codeReview_configFileState = Lens.lens (\CodeReview' {configFileState} -> configFileState) (\s@CodeReview' {} a -> s {configFileState = a} :: CodeReview)

-- | The reason for the state of the code review.
codeReview_stateReason :: Lens.Lens' CodeReview (Prelude.Maybe Prelude.Text)
codeReview_stateReason = Lens.lens (\CodeReview' {stateReason} -> stateReason) (\s@CodeReview' {} a -> s {stateReason = a} :: CodeReview)

-- | The types of analysis performed during a repository analysis or a pull
-- request review. You can specify either @Security@, @CodeQuality@, or
-- both.
codeReview_analysisTypes :: Lens.Lens' CodeReview (Prelude.Maybe [AnalysisType])
codeReview_analysisTypes = Lens.lens (\CodeReview' {analysisTypes} -> analysisTypes) (\s@CodeReview' {} a -> s {analysisTypes = a} :: CodeReview) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON CodeReview where
  parseJSON =
    Core.withObject
      "CodeReview"
      ( \x ->
          CodeReview'
            Prelude.<$> (x Core..:? "LastUpdatedTimeStamp")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "PullRequestId")
            Prelude.<*> (x Core..:? "AssociationArn")
            Prelude.<*> (x Core..:? "CreatedTimeStamp")
            Prelude.<*> (x Core..:? "SourceCodeType")
            Prelude.<*> (x Core..:? "RepositoryName")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "CodeReviewArn")
            Prelude.<*> (x Core..:? "Owner")
            Prelude.<*> (x Core..:? "Metrics")
            Prelude.<*> (x Core..:? "ProviderType")
            Prelude.<*> (x Core..:? "ConfigFileState")
            Prelude.<*> (x Core..:? "StateReason")
            Prelude.<*> (x Core..:? "AnalysisTypes" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable CodeReview where
  hashWithSalt _salt CodeReview' {..} =
    _salt `Prelude.hashWithSalt` lastUpdatedTimeStamp
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` pullRequestId
      `Prelude.hashWithSalt` associationArn
      `Prelude.hashWithSalt` createdTimeStamp
      `Prelude.hashWithSalt` sourceCodeType
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` codeReviewArn
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` providerType
      `Prelude.hashWithSalt` configFileState
      `Prelude.hashWithSalt` stateReason
      `Prelude.hashWithSalt` analysisTypes

instance Prelude.NFData CodeReview where
  rnf CodeReview' {..} =
    Prelude.rnf lastUpdatedTimeStamp
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf pullRequestId
      `Prelude.seq` Prelude.rnf associationArn
      `Prelude.seq` Prelude.rnf createdTimeStamp
      `Prelude.seq` Prelude.rnf sourceCodeType
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf codeReviewArn
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf providerType
      `Prelude.seq` Prelude.rnf configFileState
      `Prelude.seq` Prelude.rnf stateReason
      `Prelude.seq` Prelude.rnf analysisTypes
