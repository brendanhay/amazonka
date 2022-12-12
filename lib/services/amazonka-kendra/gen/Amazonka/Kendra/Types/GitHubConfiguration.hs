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
-- Module      : Amazonka.Kendra.Types.GitHubConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.GitHubConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import Amazonka.Kendra.Types.DataSourceVpcConfiguration
import Amazonka.Kendra.Types.GitHubDocumentCrawlProperties
import Amazonka.Kendra.Types.OnPremiseConfiguration
import Amazonka.Kendra.Types.SaaSConfiguration
import Amazonka.Kendra.Types.Type
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to GitHub as your data
-- source.
--
-- /See:/ 'newGitHubConfiguration' smart constructor.
data GitHubConfiguration = GitHubConfiguration'
  { -- | A list of regular expression patterns to exclude certain file names in
    -- your GitHub repository or repositories. File names that match the
    -- patterns are excluded from the index. File names that don\'t match the
    -- patterns are included in the index. If a file matches both an exclusion
    -- and inclusion pattern, the exclusion pattern takes precedence and the
    -- file isn\'t included in the index.
    exclusionFileNamePatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of regular expression patterns to exclude certain file types in
    -- your GitHub repository or repositories. File types that match the
    -- patterns are excluded from the index. File types that don\'t match the
    -- patterns are included in the index. If a file matches both an exclusion
    -- and inclusion pattern, the exclusion pattern takes precedence and the
    -- file isn\'t included in the index.
    exclusionFileTypePatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of regular expression patterns to exclude certain folder names in
    -- your GitHub repository or repositories. Folder names that match the
    -- patterns are excluded from the index. Folder names that don\'t match the
    -- patterns are included in the index. If a folder matches both an
    -- exclusion and inclusion pattern, the exclusion pattern takes precedence
    -- and the folder isn\'t included in the index.
    exclusionFolderNamePatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of GitHub commits to Amazon Kendra index field names. To
    -- create custom fields, use the @UpdateIndex@ API before you map to GitHub
    -- fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The GitHub data source field names must exist in your GitHub custom
    -- metadata.
    gitHubCommitConfigurationFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | Configuration information to include certain types of GitHub content.
    -- You can configure to index repository files only, or also include issues
    -- and pull requests, comments, and comment attachments.
    gitHubDocumentCrawlProperties :: Prelude.Maybe GitHubDocumentCrawlProperties,
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of GitHub issue attachments to Amazon Kendra index field
    -- names. To create custom fields, use the @UpdateIndex@ API before you map
    -- to GitHub fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The GitHub data source field names must exist in your GitHub custom
    -- metadata.
    gitHubIssueAttachmentConfigurationFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of GitHub issue comments to Amazon Kendra index field names.
    -- To create custom fields, use the @UpdateIndex@ API before you map to
    -- GitHub fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The GitHub data source field names must exist in your GitHub custom
    -- metadata.
    gitHubIssueCommentConfigurationFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of GitHub issues to Amazon Kendra index field names. To
    -- create custom fields, use the @UpdateIndex@ API before you map to GitHub
    -- fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The GitHub data source field names must exist in your GitHub custom
    -- metadata.
    gitHubIssueDocumentConfigurationFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of GitHub pull request comments to Amazon Kendra index field
    -- names. To create custom fields, use the @UpdateIndex@ API before you map
    -- to GitHub fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The GitHub data source field names must exist in your GitHub custom
    -- metadata.
    gitHubPullRequestCommentConfigurationFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of GitHub pull request attachments to Amazon Kendra index
    -- field names. To create custom fields, use the @UpdateIndex@ API before
    -- you map to GitHub fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The GitHub data source field names must exist in your GitHub custom
    -- metadata.
    gitHubPullRequestDocumentAttachmentConfigurationFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of GitHub pull requests to Amazon Kendra index field names.
    -- To create custom fields, use the @UpdateIndex@ API before you map to
    -- GitHub fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The GitHub data source field names must exist in your GitHub custom
    -- metadata.
    gitHubPullRequestDocumentConfigurationFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map GitHub
    -- repository attributes or field names to Amazon Kendra index field names.
    -- To create custom fields, use the @UpdateIndex@ API before you map to
    -- GitHub fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The GitHub data source field names must exist in your GitHub custom
    -- metadata.
    gitHubRepositoryConfigurationFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of regular expression patterns to include certain file names in
    -- your GitHub repository or repositories. File names that match the
    -- patterns are included in the index. File names that don\'t match the
    -- patterns are excluded from the index. If a file matches both an
    -- inclusion and exclusion pattern, the exclusion pattern takes precedence
    -- and the file isn\'t included in the index.
    inclusionFileNamePatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of regular expression patterns to include certain file types in
    -- your GitHub repository or repositories. File types that match the
    -- patterns are included in the index. File types that don\'t match the
    -- patterns are excluded from the index. If a file matches both an
    -- inclusion and exclusion pattern, the exclusion pattern takes precedence
    -- and the file isn\'t included in the index.
    inclusionFileTypePatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of regular expression patterns to include certain folder names in
    -- your GitHub repository or repositories. Folder names that match the
    -- patterns are included in the index. Folder names that don\'t match the
    -- patterns are excluded from the index. If a folder matches both an
    -- inclusion and exclusion pattern, the exclusion pattern takes precedence
    -- and the folder isn\'t included in the index.
    inclusionFolderNamePatterns :: Prelude.Maybe [Prelude.Text],
    -- | Configuration information to connect to GitHub Enterprise Server (on
    -- premises).
    onPremiseConfiguration :: Prelude.Maybe OnPremiseConfiguration,
    -- | A list of names of the specific repositories you want to index.
    repositoryFilter :: Prelude.Maybe [Prelude.Text],
    -- | Configuration information to connect to GitHub Enterprise Cloud (SaaS).
    saaSConfiguration :: Prelude.Maybe SaaSConfiguration,
    -- | The type of GitHub service you want to connect to—GitHub Enterprise
    -- Cloud (SaaS) or GitHub Enterprise Server (on premises).
    type' :: Prelude.Maybe Type,
    -- | @TRUE@ to use the GitHub change log to determine which documents require
    -- updating in the index. Depending on the GitHub change log\'s size, it
    -- may take longer for Amazon Kendra to use the change log than to scan all
    -- of your documents in GitHub.
    useChangeLog :: Prelude.Maybe Prelude.Bool,
    -- | Configuration information of an Amazon Virtual Private Cloud to connect
    -- to your GitHub. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
    vpcConfiguration :: Prelude.Maybe DataSourceVpcConfiguration,
    -- | The Amazon Resource Name (ARN) of an Secrets Manager secret that
    -- contains the key-value pairs required to connect to your GitHub. The
    -- secret must contain a JSON structure with the following keys:
    --
    -- -   personalToken—The access token created in GitHub. For more
    --     information on creating a token in GitHub, see
    --     <https://docs.aws.amazon.com/kendra/latest/dg/data-source-github.html Using a GitHub data source>.
    secretArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GitHubConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exclusionFileNamePatterns', 'gitHubConfiguration_exclusionFileNamePatterns' - A list of regular expression patterns to exclude certain file names in
-- your GitHub repository or repositories. File names that match the
-- patterns are excluded from the index. File names that don\'t match the
-- patterns are included in the index. If a file matches both an exclusion
-- and inclusion pattern, the exclusion pattern takes precedence and the
-- file isn\'t included in the index.
--
-- 'exclusionFileTypePatterns', 'gitHubConfiguration_exclusionFileTypePatterns' - A list of regular expression patterns to exclude certain file types in
-- your GitHub repository or repositories. File types that match the
-- patterns are excluded from the index. File types that don\'t match the
-- patterns are included in the index. If a file matches both an exclusion
-- and inclusion pattern, the exclusion pattern takes precedence and the
-- file isn\'t included in the index.
--
-- 'exclusionFolderNamePatterns', 'gitHubConfiguration_exclusionFolderNamePatterns' - A list of regular expression patterns to exclude certain folder names in
-- your GitHub repository or repositories. Folder names that match the
-- patterns are excluded from the index. Folder names that don\'t match the
-- patterns are included in the index. If a folder matches both an
-- exclusion and inclusion pattern, the exclusion pattern takes precedence
-- and the folder isn\'t included in the index.
--
-- 'gitHubCommitConfigurationFieldMappings', 'gitHubConfiguration_gitHubCommitConfigurationFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub commits to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to GitHub
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
--
-- 'gitHubDocumentCrawlProperties', 'gitHubConfiguration_gitHubDocumentCrawlProperties' - Configuration information to include certain types of GitHub content.
-- You can configure to index repository files only, or also include issues
-- and pull requests, comments, and comment attachments.
--
-- 'gitHubIssueAttachmentConfigurationFieldMappings', 'gitHubConfiguration_gitHubIssueAttachmentConfigurationFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub issue attachments to Amazon Kendra index field
-- names. To create custom fields, use the @UpdateIndex@ API before you map
-- to GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
--
-- 'gitHubIssueCommentConfigurationFieldMappings', 'gitHubConfiguration_gitHubIssueCommentConfigurationFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub issue comments to Amazon Kendra index field names.
-- To create custom fields, use the @UpdateIndex@ API before you map to
-- GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
--
-- 'gitHubIssueDocumentConfigurationFieldMappings', 'gitHubConfiguration_gitHubIssueDocumentConfigurationFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub issues to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to GitHub
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
--
-- 'gitHubPullRequestCommentConfigurationFieldMappings', 'gitHubConfiguration_gitHubPullRequestCommentConfigurationFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub pull request comments to Amazon Kendra index field
-- names. To create custom fields, use the @UpdateIndex@ API before you map
-- to GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
--
-- 'gitHubPullRequestDocumentAttachmentConfigurationFieldMappings', 'gitHubConfiguration_gitHubPullRequestDocumentAttachmentConfigurationFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub pull request attachments to Amazon Kendra index
-- field names. To create custom fields, use the @UpdateIndex@ API before
-- you map to GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
--
-- 'gitHubPullRequestDocumentConfigurationFieldMappings', 'gitHubConfiguration_gitHubPullRequestDocumentConfigurationFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub pull requests to Amazon Kendra index field names.
-- To create custom fields, use the @UpdateIndex@ API before you map to
-- GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
--
-- 'gitHubRepositoryConfigurationFieldMappings', 'gitHubConfiguration_gitHubRepositoryConfigurationFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map GitHub
-- repository attributes or field names to Amazon Kendra index field names.
-- To create custom fields, use the @UpdateIndex@ API before you map to
-- GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
--
-- 'inclusionFileNamePatterns', 'gitHubConfiguration_inclusionFileNamePatterns' - A list of regular expression patterns to include certain file names in
-- your GitHub repository or repositories. File names that match the
-- patterns are included in the index. File names that don\'t match the
-- patterns are excluded from the index. If a file matches both an
-- inclusion and exclusion pattern, the exclusion pattern takes precedence
-- and the file isn\'t included in the index.
--
-- 'inclusionFileTypePatterns', 'gitHubConfiguration_inclusionFileTypePatterns' - A list of regular expression patterns to include certain file types in
-- your GitHub repository or repositories. File types that match the
-- patterns are included in the index. File types that don\'t match the
-- patterns are excluded from the index. If a file matches both an
-- inclusion and exclusion pattern, the exclusion pattern takes precedence
-- and the file isn\'t included in the index.
--
-- 'inclusionFolderNamePatterns', 'gitHubConfiguration_inclusionFolderNamePatterns' - A list of regular expression patterns to include certain folder names in
-- your GitHub repository or repositories. Folder names that match the
-- patterns are included in the index. Folder names that don\'t match the
-- patterns are excluded from the index. If a folder matches both an
-- inclusion and exclusion pattern, the exclusion pattern takes precedence
-- and the folder isn\'t included in the index.
--
-- 'onPremiseConfiguration', 'gitHubConfiguration_onPremiseConfiguration' - Configuration information to connect to GitHub Enterprise Server (on
-- premises).
--
-- 'repositoryFilter', 'gitHubConfiguration_repositoryFilter' - A list of names of the specific repositories you want to index.
--
-- 'saaSConfiguration', 'gitHubConfiguration_saaSConfiguration' - Configuration information to connect to GitHub Enterprise Cloud (SaaS).
--
-- 'type'', 'gitHubConfiguration_type' - The type of GitHub service you want to connect to—GitHub Enterprise
-- Cloud (SaaS) or GitHub Enterprise Server (on premises).
--
-- 'useChangeLog', 'gitHubConfiguration_useChangeLog' - @TRUE@ to use the GitHub change log to determine which documents require
-- updating in the index. Depending on the GitHub change log\'s size, it
-- may take longer for Amazon Kendra to use the change log than to scan all
-- of your documents in GitHub.
--
-- 'vpcConfiguration', 'gitHubConfiguration_vpcConfiguration' - Configuration information of an Amazon Virtual Private Cloud to connect
-- to your GitHub. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
--
-- 'secretArn', 'gitHubConfiguration_secretArn' - The Amazon Resource Name (ARN) of an Secrets Manager secret that
-- contains the key-value pairs required to connect to your GitHub. The
-- secret must contain a JSON structure with the following keys:
--
-- -   personalToken—The access token created in GitHub. For more
--     information on creating a token in GitHub, see
--     <https://docs.aws.amazon.com/kendra/latest/dg/data-source-github.html Using a GitHub data source>.
newGitHubConfiguration ::
  -- | 'secretArn'
  Prelude.Text ->
  GitHubConfiguration
newGitHubConfiguration pSecretArn_ =
  GitHubConfiguration'
    { exclusionFileNamePatterns =
        Prelude.Nothing,
      exclusionFileTypePatterns = Prelude.Nothing,
      exclusionFolderNamePatterns = Prelude.Nothing,
      gitHubCommitConfigurationFieldMappings =
        Prelude.Nothing,
      gitHubDocumentCrawlProperties = Prelude.Nothing,
      gitHubIssueAttachmentConfigurationFieldMappings =
        Prelude.Nothing,
      gitHubIssueCommentConfigurationFieldMappings =
        Prelude.Nothing,
      gitHubIssueDocumentConfigurationFieldMappings =
        Prelude.Nothing,
      gitHubPullRequestCommentConfigurationFieldMappings =
        Prelude.Nothing,
      gitHubPullRequestDocumentAttachmentConfigurationFieldMappings =
        Prelude.Nothing,
      gitHubPullRequestDocumentConfigurationFieldMappings =
        Prelude.Nothing,
      gitHubRepositoryConfigurationFieldMappings =
        Prelude.Nothing,
      inclusionFileNamePatterns = Prelude.Nothing,
      inclusionFileTypePatterns = Prelude.Nothing,
      inclusionFolderNamePatterns = Prelude.Nothing,
      onPremiseConfiguration = Prelude.Nothing,
      repositoryFilter = Prelude.Nothing,
      saaSConfiguration = Prelude.Nothing,
      type' = Prelude.Nothing,
      useChangeLog = Prelude.Nothing,
      vpcConfiguration = Prelude.Nothing,
      secretArn = pSecretArn_
    }

-- | A list of regular expression patterns to exclude certain file names in
-- your GitHub repository or repositories. File names that match the
-- patterns are excluded from the index. File names that don\'t match the
-- patterns are included in the index. If a file matches both an exclusion
-- and inclusion pattern, the exclusion pattern takes precedence and the
-- file isn\'t included in the index.
gitHubConfiguration_exclusionFileNamePatterns :: Lens.Lens' GitHubConfiguration (Prelude.Maybe [Prelude.Text])
gitHubConfiguration_exclusionFileNamePatterns = Lens.lens (\GitHubConfiguration' {exclusionFileNamePatterns} -> exclusionFileNamePatterns) (\s@GitHubConfiguration' {} a -> s {exclusionFileNamePatterns = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to exclude certain file types in
-- your GitHub repository or repositories. File types that match the
-- patterns are excluded from the index. File types that don\'t match the
-- patterns are included in the index. If a file matches both an exclusion
-- and inclusion pattern, the exclusion pattern takes precedence and the
-- file isn\'t included in the index.
gitHubConfiguration_exclusionFileTypePatterns :: Lens.Lens' GitHubConfiguration (Prelude.Maybe [Prelude.Text])
gitHubConfiguration_exclusionFileTypePatterns = Lens.lens (\GitHubConfiguration' {exclusionFileTypePatterns} -> exclusionFileTypePatterns) (\s@GitHubConfiguration' {} a -> s {exclusionFileTypePatterns = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to exclude certain folder names in
-- your GitHub repository or repositories. Folder names that match the
-- patterns are excluded from the index. Folder names that don\'t match the
-- patterns are included in the index. If a folder matches both an
-- exclusion and inclusion pattern, the exclusion pattern takes precedence
-- and the folder isn\'t included in the index.
gitHubConfiguration_exclusionFolderNamePatterns :: Lens.Lens' GitHubConfiguration (Prelude.Maybe [Prelude.Text])
gitHubConfiguration_exclusionFolderNamePatterns = Lens.lens (\GitHubConfiguration' {exclusionFolderNamePatterns} -> exclusionFolderNamePatterns) (\s@GitHubConfiguration' {} a -> s {exclusionFolderNamePatterns = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub commits to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to GitHub
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
gitHubConfiguration_gitHubCommitConfigurationFieldMappings :: Lens.Lens' GitHubConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
gitHubConfiguration_gitHubCommitConfigurationFieldMappings = Lens.lens (\GitHubConfiguration' {gitHubCommitConfigurationFieldMappings} -> gitHubCommitConfigurationFieldMappings) (\s@GitHubConfiguration' {} a -> s {gitHubCommitConfigurationFieldMappings = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Configuration information to include certain types of GitHub content.
-- You can configure to index repository files only, or also include issues
-- and pull requests, comments, and comment attachments.
gitHubConfiguration_gitHubDocumentCrawlProperties :: Lens.Lens' GitHubConfiguration (Prelude.Maybe GitHubDocumentCrawlProperties)
gitHubConfiguration_gitHubDocumentCrawlProperties = Lens.lens (\GitHubConfiguration' {gitHubDocumentCrawlProperties} -> gitHubDocumentCrawlProperties) (\s@GitHubConfiguration' {} a -> s {gitHubDocumentCrawlProperties = a} :: GitHubConfiguration)

-- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub issue attachments to Amazon Kendra index field
-- names. To create custom fields, use the @UpdateIndex@ API before you map
-- to GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
gitHubConfiguration_gitHubIssueAttachmentConfigurationFieldMappings :: Lens.Lens' GitHubConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
gitHubConfiguration_gitHubIssueAttachmentConfigurationFieldMappings = Lens.lens (\GitHubConfiguration' {gitHubIssueAttachmentConfigurationFieldMappings} -> gitHubIssueAttachmentConfigurationFieldMappings) (\s@GitHubConfiguration' {} a -> s {gitHubIssueAttachmentConfigurationFieldMappings = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub issue comments to Amazon Kendra index field names.
-- To create custom fields, use the @UpdateIndex@ API before you map to
-- GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
gitHubConfiguration_gitHubIssueCommentConfigurationFieldMappings :: Lens.Lens' GitHubConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
gitHubConfiguration_gitHubIssueCommentConfigurationFieldMappings = Lens.lens (\GitHubConfiguration' {gitHubIssueCommentConfigurationFieldMappings} -> gitHubIssueCommentConfigurationFieldMappings) (\s@GitHubConfiguration' {} a -> s {gitHubIssueCommentConfigurationFieldMappings = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub issues to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to GitHub
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
gitHubConfiguration_gitHubIssueDocumentConfigurationFieldMappings :: Lens.Lens' GitHubConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
gitHubConfiguration_gitHubIssueDocumentConfigurationFieldMappings = Lens.lens (\GitHubConfiguration' {gitHubIssueDocumentConfigurationFieldMappings} -> gitHubIssueDocumentConfigurationFieldMappings) (\s@GitHubConfiguration' {} a -> s {gitHubIssueDocumentConfigurationFieldMappings = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub pull request comments to Amazon Kendra index field
-- names. To create custom fields, use the @UpdateIndex@ API before you map
-- to GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
gitHubConfiguration_gitHubPullRequestCommentConfigurationFieldMappings :: Lens.Lens' GitHubConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
gitHubConfiguration_gitHubPullRequestCommentConfigurationFieldMappings = Lens.lens (\GitHubConfiguration' {gitHubPullRequestCommentConfigurationFieldMappings} -> gitHubPullRequestCommentConfigurationFieldMappings) (\s@GitHubConfiguration' {} a -> s {gitHubPullRequestCommentConfigurationFieldMappings = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub pull request attachments to Amazon Kendra index
-- field names. To create custom fields, use the @UpdateIndex@ API before
-- you map to GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
gitHubConfiguration_gitHubPullRequestDocumentAttachmentConfigurationFieldMappings :: Lens.Lens' GitHubConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
gitHubConfiguration_gitHubPullRequestDocumentAttachmentConfigurationFieldMappings = Lens.lens (\GitHubConfiguration' {gitHubPullRequestDocumentAttachmentConfigurationFieldMappings} -> gitHubPullRequestDocumentAttachmentConfigurationFieldMappings) (\s@GitHubConfiguration' {} a -> s {gitHubPullRequestDocumentAttachmentConfigurationFieldMappings = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub pull requests to Amazon Kendra index field names.
-- To create custom fields, use the @UpdateIndex@ API before you map to
-- GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
gitHubConfiguration_gitHubPullRequestDocumentConfigurationFieldMappings :: Lens.Lens' GitHubConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
gitHubConfiguration_gitHubPullRequestDocumentConfigurationFieldMappings = Lens.lens (\GitHubConfiguration' {gitHubPullRequestDocumentConfigurationFieldMappings} -> gitHubPullRequestDocumentConfigurationFieldMappings) (\s@GitHubConfiguration' {} a -> s {gitHubPullRequestDocumentConfigurationFieldMappings = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DataSourceToIndexFieldMapping@ objects that map GitHub
-- repository attributes or field names to Amazon Kendra index field names.
-- To create custom fields, use the @UpdateIndex@ API before you map to
-- GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
gitHubConfiguration_gitHubRepositoryConfigurationFieldMappings :: Lens.Lens' GitHubConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
gitHubConfiguration_gitHubRepositoryConfigurationFieldMappings = Lens.lens (\GitHubConfiguration' {gitHubRepositoryConfigurationFieldMappings} -> gitHubRepositoryConfigurationFieldMappings) (\s@GitHubConfiguration' {} a -> s {gitHubRepositoryConfigurationFieldMappings = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to include certain file names in
-- your GitHub repository or repositories. File names that match the
-- patterns are included in the index. File names that don\'t match the
-- patterns are excluded from the index. If a file matches both an
-- inclusion and exclusion pattern, the exclusion pattern takes precedence
-- and the file isn\'t included in the index.
gitHubConfiguration_inclusionFileNamePatterns :: Lens.Lens' GitHubConfiguration (Prelude.Maybe [Prelude.Text])
gitHubConfiguration_inclusionFileNamePatterns = Lens.lens (\GitHubConfiguration' {inclusionFileNamePatterns} -> inclusionFileNamePatterns) (\s@GitHubConfiguration' {} a -> s {inclusionFileNamePatterns = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to include certain file types in
-- your GitHub repository or repositories. File types that match the
-- patterns are included in the index. File types that don\'t match the
-- patterns are excluded from the index. If a file matches both an
-- inclusion and exclusion pattern, the exclusion pattern takes precedence
-- and the file isn\'t included in the index.
gitHubConfiguration_inclusionFileTypePatterns :: Lens.Lens' GitHubConfiguration (Prelude.Maybe [Prelude.Text])
gitHubConfiguration_inclusionFileTypePatterns = Lens.lens (\GitHubConfiguration' {inclusionFileTypePatterns} -> inclusionFileTypePatterns) (\s@GitHubConfiguration' {} a -> s {inclusionFileTypePatterns = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to include certain folder names in
-- your GitHub repository or repositories. Folder names that match the
-- patterns are included in the index. Folder names that don\'t match the
-- patterns are excluded from the index. If a folder matches both an
-- inclusion and exclusion pattern, the exclusion pattern takes precedence
-- and the folder isn\'t included in the index.
gitHubConfiguration_inclusionFolderNamePatterns :: Lens.Lens' GitHubConfiguration (Prelude.Maybe [Prelude.Text])
gitHubConfiguration_inclusionFolderNamePatterns = Lens.lens (\GitHubConfiguration' {inclusionFolderNamePatterns} -> inclusionFolderNamePatterns) (\s@GitHubConfiguration' {} a -> s {inclusionFolderNamePatterns = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Configuration information to connect to GitHub Enterprise Server (on
-- premises).
gitHubConfiguration_onPremiseConfiguration :: Lens.Lens' GitHubConfiguration (Prelude.Maybe OnPremiseConfiguration)
gitHubConfiguration_onPremiseConfiguration = Lens.lens (\GitHubConfiguration' {onPremiseConfiguration} -> onPremiseConfiguration) (\s@GitHubConfiguration' {} a -> s {onPremiseConfiguration = a} :: GitHubConfiguration)

-- | A list of names of the specific repositories you want to index.
gitHubConfiguration_repositoryFilter :: Lens.Lens' GitHubConfiguration (Prelude.Maybe [Prelude.Text])
gitHubConfiguration_repositoryFilter = Lens.lens (\GitHubConfiguration' {repositoryFilter} -> repositoryFilter) (\s@GitHubConfiguration' {} a -> s {repositoryFilter = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Configuration information to connect to GitHub Enterprise Cloud (SaaS).
gitHubConfiguration_saaSConfiguration :: Lens.Lens' GitHubConfiguration (Prelude.Maybe SaaSConfiguration)
gitHubConfiguration_saaSConfiguration = Lens.lens (\GitHubConfiguration' {saaSConfiguration} -> saaSConfiguration) (\s@GitHubConfiguration' {} a -> s {saaSConfiguration = a} :: GitHubConfiguration)

-- | The type of GitHub service you want to connect to—GitHub Enterprise
-- Cloud (SaaS) or GitHub Enterprise Server (on premises).
gitHubConfiguration_type :: Lens.Lens' GitHubConfiguration (Prelude.Maybe Type)
gitHubConfiguration_type = Lens.lens (\GitHubConfiguration' {type'} -> type') (\s@GitHubConfiguration' {} a -> s {type' = a} :: GitHubConfiguration)

-- | @TRUE@ to use the GitHub change log to determine which documents require
-- updating in the index. Depending on the GitHub change log\'s size, it
-- may take longer for Amazon Kendra to use the change log than to scan all
-- of your documents in GitHub.
gitHubConfiguration_useChangeLog :: Lens.Lens' GitHubConfiguration (Prelude.Maybe Prelude.Bool)
gitHubConfiguration_useChangeLog = Lens.lens (\GitHubConfiguration' {useChangeLog} -> useChangeLog) (\s@GitHubConfiguration' {} a -> s {useChangeLog = a} :: GitHubConfiguration)

-- | Configuration information of an Amazon Virtual Private Cloud to connect
-- to your GitHub. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
gitHubConfiguration_vpcConfiguration :: Lens.Lens' GitHubConfiguration (Prelude.Maybe DataSourceVpcConfiguration)
gitHubConfiguration_vpcConfiguration = Lens.lens (\GitHubConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@GitHubConfiguration' {} a -> s {vpcConfiguration = a} :: GitHubConfiguration)

-- | The Amazon Resource Name (ARN) of an Secrets Manager secret that
-- contains the key-value pairs required to connect to your GitHub. The
-- secret must contain a JSON structure with the following keys:
--
-- -   personalToken—The access token created in GitHub. For more
--     information on creating a token in GitHub, see
--     <https://docs.aws.amazon.com/kendra/latest/dg/data-source-github.html Using a GitHub data source>.
gitHubConfiguration_secretArn :: Lens.Lens' GitHubConfiguration Prelude.Text
gitHubConfiguration_secretArn = Lens.lens (\GitHubConfiguration' {secretArn} -> secretArn) (\s@GitHubConfiguration' {} a -> s {secretArn = a} :: GitHubConfiguration)

instance Data.FromJSON GitHubConfiguration where
  parseJSON =
    Data.withObject
      "GitHubConfiguration"
      ( \x ->
          GitHubConfiguration'
            Prelude.<$> ( x Data..:? "ExclusionFileNamePatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ExclusionFileTypePatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ExclusionFolderNamePatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "GitHubCommitConfigurationFieldMappings")
            Prelude.<*> (x Data..:? "GitHubDocumentCrawlProperties")
            Prelude.<*> ( x
                            Data..:? "GitHubIssueAttachmentConfigurationFieldMappings"
                        )
            Prelude.<*> ( x
                            Data..:? "GitHubIssueCommentConfigurationFieldMappings"
                        )
            Prelude.<*> ( x
                            Data..:? "GitHubIssueDocumentConfigurationFieldMappings"
                        )
            Prelude.<*> ( x
                            Data..:? "GitHubPullRequestCommentConfigurationFieldMappings"
                        )
            Prelude.<*> ( x
                            Data..:? "GitHubPullRequestDocumentAttachmentConfigurationFieldMappings"
                        )
            Prelude.<*> ( x
                            Data..:? "GitHubPullRequestDocumentConfigurationFieldMappings"
                        )
            Prelude.<*> ( x
                            Data..:? "GitHubRepositoryConfigurationFieldMappings"
                        )
            Prelude.<*> ( x Data..:? "InclusionFileNamePatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "InclusionFileTypePatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "InclusionFolderNamePatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "OnPremiseConfiguration")
            Prelude.<*> ( x Data..:? "RepositoryFilter"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SaaSConfiguration")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "UseChangeLog")
            Prelude.<*> (x Data..:? "VpcConfiguration")
            Prelude.<*> (x Data..: "SecretArn")
      )

instance Prelude.Hashable GitHubConfiguration where
  hashWithSalt _salt GitHubConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` exclusionFileNamePatterns
      `Prelude.hashWithSalt` exclusionFileTypePatterns
      `Prelude.hashWithSalt` exclusionFolderNamePatterns
      `Prelude.hashWithSalt` gitHubCommitConfigurationFieldMappings
      `Prelude.hashWithSalt` gitHubDocumentCrawlProperties
      `Prelude.hashWithSalt` gitHubIssueAttachmentConfigurationFieldMappings
      `Prelude.hashWithSalt` gitHubIssueCommentConfigurationFieldMappings
      `Prelude.hashWithSalt` gitHubIssueDocumentConfigurationFieldMappings
      `Prelude.hashWithSalt` gitHubPullRequestCommentConfigurationFieldMappings
      `Prelude.hashWithSalt` gitHubPullRequestDocumentAttachmentConfigurationFieldMappings
      `Prelude.hashWithSalt` gitHubPullRequestDocumentConfigurationFieldMappings
      `Prelude.hashWithSalt` gitHubRepositoryConfigurationFieldMappings
      `Prelude.hashWithSalt` inclusionFileNamePatterns
      `Prelude.hashWithSalt` inclusionFileTypePatterns
      `Prelude.hashWithSalt` inclusionFolderNamePatterns
      `Prelude.hashWithSalt` onPremiseConfiguration
      `Prelude.hashWithSalt` repositoryFilter
      `Prelude.hashWithSalt` saaSConfiguration
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` useChangeLog
      `Prelude.hashWithSalt` vpcConfiguration
      `Prelude.hashWithSalt` secretArn

instance Prelude.NFData GitHubConfiguration where
  rnf GitHubConfiguration' {..} =
    Prelude.rnf exclusionFileNamePatterns
      `Prelude.seq` Prelude.rnf exclusionFileTypePatterns
      `Prelude.seq` Prelude.rnf exclusionFolderNamePatterns
      `Prelude.seq` Prelude.rnf gitHubCommitConfigurationFieldMappings
      `Prelude.seq` Prelude.rnf gitHubDocumentCrawlProperties
      `Prelude.seq` Prelude.rnf
        gitHubIssueAttachmentConfigurationFieldMappings
      `Prelude.seq` Prelude.rnf
        gitHubIssueCommentConfigurationFieldMappings
      `Prelude.seq` Prelude.rnf
        gitHubIssueDocumentConfigurationFieldMappings
      `Prelude.seq` Prelude.rnf
        gitHubPullRequestCommentConfigurationFieldMappings
      `Prelude.seq` Prelude.rnf
        gitHubPullRequestDocumentAttachmentConfigurationFieldMappings
      `Prelude.seq` Prelude.rnf
        gitHubPullRequestDocumentConfigurationFieldMappings
      `Prelude.seq` Prelude.rnf
        gitHubRepositoryConfigurationFieldMappings
      `Prelude.seq` Prelude.rnf inclusionFileNamePatterns
      `Prelude.seq` Prelude.rnf inclusionFileTypePatterns
      `Prelude.seq` Prelude.rnf inclusionFolderNamePatterns
      `Prelude.seq` Prelude.rnf onPremiseConfiguration
      `Prelude.seq` Prelude.rnf repositoryFilter
      `Prelude.seq` Prelude.rnf saaSConfiguration
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf useChangeLog
      `Prelude.seq` Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf secretArn

instance Data.ToJSON GitHubConfiguration where
  toJSON GitHubConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExclusionFileNamePatterns" Data..=)
              Prelude.<$> exclusionFileNamePatterns,
            ("ExclusionFileTypePatterns" Data..=)
              Prelude.<$> exclusionFileTypePatterns,
            ("ExclusionFolderNamePatterns" Data..=)
              Prelude.<$> exclusionFolderNamePatterns,
            ("GitHubCommitConfigurationFieldMappings" Data..=)
              Prelude.<$> gitHubCommitConfigurationFieldMappings,
            ("GitHubDocumentCrawlProperties" Data..=)
              Prelude.<$> gitHubDocumentCrawlProperties,
            ( "GitHubIssueAttachmentConfigurationFieldMappings"
                Data..=
            )
              Prelude.<$> gitHubIssueAttachmentConfigurationFieldMappings,
            ( "GitHubIssueCommentConfigurationFieldMappings"
                Data..=
            )
              Prelude.<$> gitHubIssueCommentConfigurationFieldMappings,
            ( "GitHubIssueDocumentConfigurationFieldMappings"
                Data..=
            )
              Prelude.<$> gitHubIssueDocumentConfigurationFieldMappings,
            ( "GitHubPullRequestCommentConfigurationFieldMappings"
                Data..=
            )
              Prelude.<$> gitHubPullRequestCommentConfigurationFieldMappings,
            ( "GitHubPullRequestDocumentAttachmentConfigurationFieldMappings"
                Data..=
            )
              Prelude.<$> gitHubPullRequestDocumentAttachmentConfigurationFieldMappings,
            ( "GitHubPullRequestDocumentConfigurationFieldMappings"
                Data..=
            )
              Prelude.<$> gitHubPullRequestDocumentConfigurationFieldMappings,
            ( "GitHubRepositoryConfigurationFieldMappings"
                Data..=
            )
              Prelude.<$> gitHubRepositoryConfigurationFieldMappings,
            ("InclusionFileNamePatterns" Data..=)
              Prelude.<$> inclusionFileNamePatterns,
            ("InclusionFileTypePatterns" Data..=)
              Prelude.<$> inclusionFileTypePatterns,
            ("InclusionFolderNamePatterns" Data..=)
              Prelude.<$> inclusionFolderNamePatterns,
            ("OnPremiseConfiguration" Data..=)
              Prelude.<$> onPremiseConfiguration,
            ("RepositoryFilter" Data..=)
              Prelude.<$> repositoryFilter,
            ("SaaSConfiguration" Data..=)
              Prelude.<$> saaSConfiguration,
            ("Type" Data..=) Prelude.<$> type',
            ("UseChangeLog" Data..=) Prelude.<$> useChangeLog,
            ("VpcConfiguration" Data..=)
              Prelude.<$> vpcConfiguration,
            Prelude.Just ("SecretArn" Data..= secretArn)
          ]
      )
