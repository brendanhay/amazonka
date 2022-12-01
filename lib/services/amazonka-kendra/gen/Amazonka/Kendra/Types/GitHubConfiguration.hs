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
  { -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of GitHub issue attachments to Amazon Kendra index field
    -- names. To create custom fields, use the @UpdateIndex@ API before you map
    -- to GitHub fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The GitHub data source field names must exist in your GitHub custom
    -- metadata.
    gitHubIssueAttachmentConfigurationFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | @TRUE@ to use the GitHub change log to determine which documents require
    -- updating in the index. Depending on the GitHub change log\'s size, it
    -- may take longer for Amazon Kendra to use the change log than to scan all
    -- of your documents in GitHub.
    useChangeLog :: Prelude.Maybe Prelude.Bool,
    -- | Configuration information of an Amazon Virtual Private Cloud to connect
    -- to your GitHub. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/vpc-configuration.html Configuring a VPC>.
    vpcConfiguration :: Prelude.Maybe DataSourceVpcConfiguration,
    -- | The type of GitHub service you want to connect to—GitHub Enterprise
    -- Cloud (SaaS) or GitHub Enterprise Server (on premises).
    type' :: Prelude.Maybe Type,
    -- | A list of names of the specific repositories you want to index.
    repositoryFilter :: Prelude.Maybe [Prelude.Text],
    -- | Configuration information to connect to GitHub Enterprise Cloud (SaaS).
    saaSConfiguration :: Prelude.Maybe SaaSConfiguration,
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map GitHub
    -- repository attributes or field names to Amazon Kendra index field names.
    -- To create custom fields, use the @UpdateIndex@ API before you map to
    -- GitHub fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The GitHub data source field names must exist in your GitHub custom
    -- metadata.
    gitHubRepositoryConfigurationFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of GitHub pull request comments to Amazon Kendra index field
    -- names. To create custom fields, use the @UpdateIndex@ API before you map
    -- to GitHub fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The GitHub data source field names must exist in your GitHub custom
    -- metadata.
    gitHubPullRequestCommentConfigurationFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of GitHub issue comments to Amazon Kendra index field names.
    -- To create custom fields, use the @UpdateIndex@ API before you map to
    -- GitHub fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The GitHub data source field names must exist in your GitHub custom
    -- metadata.
    gitHubIssueCommentConfigurationFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | Configuration information to connect to GitHub Enterprise Server (on
    -- premises).
    onPremiseConfiguration :: Prelude.Maybe OnPremiseConfiguration,
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
    -- field names of GitHub pull requests to Amazon Kendra index field names.
    -- To create custom fields, use the @UpdateIndex@ API before you map to
    -- GitHub fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The GitHub data source field names must exist in your GitHub custom
    -- metadata.
    gitHubPullRequestDocumentConfigurationFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of regular expression patterns to exclude certain folder names in
    -- your GitHub repository or repositories. Folder names that match the
    -- patterns are excluded from the index. Folder names that don\'t match the
    -- patterns are included in the index. If a folder matches both an
    -- exclusion and inclusion pattern, the exclusion pattern takes precedence
    -- and the folder isn\'t included in the index.
    exclusionFolderNamePatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of GitHub issues to Amazon Kendra index field names. To
    -- create custom fields, use the @UpdateIndex@ API before you map to GitHub
    -- fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The GitHub data source field names must exist in your GitHub custom
    -- metadata.
    gitHubIssueDocumentConfigurationFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of regular expression patterns to include certain folder names in
    -- your GitHub repository or repositories. Folder names that match the
    -- patterns are included in the index. Folder names that don\'t match the
    -- patterns are excluded from the index. If a folder matches both an
    -- inclusion and exclusion pattern, the exclusion pattern takes precedence
    -- and the folder isn\'t included in the index.
    inclusionFolderNamePatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of regular expression patterns to exclude certain file types in
    -- your GitHub repository or repositories. File types that match the
    -- patterns are excluded from the index. File types that don\'t match the
    -- patterns are included in the index. If a file matches both an exclusion
    -- and inclusion pattern, the exclusion pattern takes precedence and the
    -- file isn\'t included in the index.
    exclusionFileTypePatterns :: Prelude.Maybe [Prelude.Text],
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
    -- | A list of regular expression patterns to exclude certain file names in
    -- your GitHub repository or repositories. File names that match the
    -- patterns are excluded from the index. File names that don\'t match the
    -- patterns are included in the index. If a file matches both an exclusion
    -- and inclusion pattern, the exclusion pattern takes precedence and the
    -- file isn\'t included in the index.
    exclusionFileNamePatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
    -- field names of GitHub pull request attachments to Amazon Kendra index
    -- field names. To create custom fields, use the @UpdateIndex@ API before
    -- you map to GitHub fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The GitHub data source field names must exist in your GitHub custom
    -- metadata.
    gitHubPullRequestDocumentAttachmentConfigurationFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | The Amazon Resource Name (ARN) of an Secrets Manager secret that
    -- contains the key-value pairs required to connect to your GitHub. The
    -- secret must contain a JSON structure with the following keys:
    --
    -- -   githubToken—The access token created in GitHub. For more information
    --     on creating a token in GitHub, see
    --     <https://docs.aws.amazon.com/kendra/latest/dg/data-source-github.html#github-authentication Authentication for a GitHub data source>.
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
-- 'gitHubIssueAttachmentConfigurationFieldMappings', 'gitHubConfiguration_gitHubIssueAttachmentConfigurationFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub issue attachments to Amazon Kendra index field
-- names. To create custom fields, use the @UpdateIndex@ API before you map
-- to GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
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
-- 'type'', 'gitHubConfiguration_type' - The type of GitHub service you want to connect to—GitHub Enterprise
-- Cloud (SaaS) or GitHub Enterprise Server (on premises).
--
-- 'repositoryFilter', 'gitHubConfiguration_repositoryFilter' - A list of names of the specific repositories you want to index.
--
-- 'saaSConfiguration', 'gitHubConfiguration_saaSConfiguration' - Configuration information to connect to GitHub Enterprise Cloud (SaaS).
--
-- 'gitHubRepositoryConfigurationFieldMappings', 'gitHubConfiguration_gitHubRepositoryConfigurationFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map GitHub
-- repository attributes or field names to Amazon Kendra index field names.
-- To create custom fields, use the @UpdateIndex@ API before you map to
-- GitHub fields. For more information, see
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
-- 'gitHubIssueCommentConfigurationFieldMappings', 'gitHubConfiguration_gitHubIssueCommentConfigurationFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub issue comments to Amazon Kendra index field names.
-- To create custom fields, use the @UpdateIndex@ API before you map to
-- GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
--
-- 'onPremiseConfiguration', 'gitHubConfiguration_onPremiseConfiguration' - Configuration information to connect to GitHub Enterprise Server (on
-- premises).
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
-- 'gitHubPullRequestDocumentConfigurationFieldMappings', 'gitHubConfiguration_gitHubPullRequestDocumentConfigurationFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub pull requests to Amazon Kendra index field names.
-- To create custom fields, use the @UpdateIndex@ API before you map to
-- GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
--
-- 'exclusionFolderNamePatterns', 'gitHubConfiguration_exclusionFolderNamePatterns' - A list of regular expression patterns to exclude certain folder names in
-- your GitHub repository or repositories. Folder names that match the
-- patterns are excluded from the index. Folder names that don\'t match the
-- patterns are included in the index. If a folder matches both an
-- exclusion and inclusion pattern, the exclusion pattern takes precedence
-- and the folder isn\'t included in the index.
--
-- 'gitHubIssueDocumentConfigurationFieldMappings', 'gitHubConfiguration_gitHubIssueDocumentConfigurationFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub issues to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to GitHub
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
--
-- 'inclusionFolderNamePatterns', 'gitHubConfiguration_inclusionFolderNamePatterns' - A list of regular expression patterns to include certain folder names in
-- your GitHub repository or repositories. Folder names that match the
-- patterns are included in the index. Folder names that don\'t match the
-- patterns are excluded from the index. If a folder matches both an
-- inclusion and exclusion pattern, the exclusion pattern takes precedence
-- and the folder isn\'t included in the index.
--
-- 'exclusionFileTypePatterns', 'gitHubConfiguration_exclusionFileTypePatterns' - A list of regular expression patterns to exclude certain file types in
-- your GitHub repository or repositories. File types that match the
-- patterns are excluded from the index. File types that don\'t match the
-- patterns are included in the index. If a file matches both an exclusion
-- and inclusion pattern, the exclusion pattern takes precedence and the
-- file isn\'t included in the index.
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
-- 'exclusionFileNamePatterns', 'gitHubConfiguration_exclusionFileNamePatterns' - A list of regular expression patterns to exclude certain file names in
-- your GitHub repository or repositories. File names that match the
-- patterns are excluded from the index. File names that don\'t match the
-- patterns are included in the index. If a file matches both an exclusion
-- and inclusion pattern, the exclusion pattern takes precedence and the
-- file isn\'t included in the index.
--
-- 'gitHubPullRequestDocumentAttachmentConfigurationFieldMappings', 'gitHubConfiguration_gitHubPullRequestDocumentAttachmentConfigurationFieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub pull request attachments to Amazon Kendra index
-- field names. To create custom fields, use the @UpdateIndex@ API before
-- you map to GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
--
-- 'secretArn', 'gitHubConfiguration_secretArn' - The Amazon Resource Name (ARN) of an Secrets Manager secret that
-- contains the key-value pairs required to connect to your GitHub. The
-- secret must contain a JSON structure with the following keys:
--
-- -   githubToken—The access token created in GitHub. For more information
--     on creating a token in GitHub, see
--     <https://docs.aws.amazon.com/kendra/latest/dg/data-source-github.html#github-authentication Authentication for a GitHub data source>.
newGitHubConfiguration ::
  -- | 'secretArn'
  Prelude.Text ->
  GitHubConfiguration
newGitHubConfiguration pSecretArn_ =
  GitHubConfiguration'
    { gitHubIssueAttachmentConfigurationFieldMappings =
        Prelude.Nothing,
      useChangeLog = Prelude.Nothing,
      vpcConfiguration = Prelude.Nothing,
      type' = Prelude.Nothing,
      repositoryFilter = Prelude.Nothing,
      saaSConfiguration = Prelude.Nothing,
      gitHubRepositoryConfigurationFieldMappings =
        Prelude.Nothing,
      gitHubPullRequestCommentConfigurationFieldMappings =
        Prelude.Nothing,
      gitHubIssueCommentConfigurationFieldMappings =
        Prelude.Nothing,
      onPremiseConfiguration = Prelude.Nothing,
      gitHubCommitConfigurationFieldMappings =
        Prelude.Nothing,
      gitHubDocumentCrawlProperties = Prelude.Nothing,
      gitHubPullRequestDocumentConfigurationFieldMappings =
        Prelude.Nothing,
      exclusionFolderNamePatterns = Prelude.Nothing,
      gitHubIssueDocumentConfigurationFieldMappings =
        Prelude.Nothing,
      inclusionFolderNamePatterns = Prelude.Nothing,
      exclusionFileTypePatterns = Prelude.Nothing,
      inclusionFileNamePatterns = Prelude.Nothing,
      inclusionFileTypePatterns = Prelude.Nothing,
      exclusionFileNamePatterns = Prelude.Nothing,
      gitHubPullRequestDocumentAttachmentConfigurationFieldMappings =
        Prelude.Nothing,
      secretArn = pSecretArn_
    }

-- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub issue attachments to Amazon Kendra index field
-- names. To create custom fields, use the @UpdateIndex@ API before you map
-- to GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
gitHubConfiguration_gitHubIssueAttachmentConfigurationFieldMappings :: Lens.Lens' GitHubConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
gitHubConfiguration_gitHubIssueAttachmentConfigurationFieldMappings = Lens.lens (\GitHubConfiguration' {gitHubIssueAttachmentConfigurationFieldMappings} -> gitHubIssueAttachmentConfigurationFieldMappings) (\s@GitHubConfiguration' {} a -> s {gitHubIssueAttachmentConfigurationFieldMappings = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

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

-- | The type of GitHub service you want to connect to—GitHub Enterprise
-- Cloud (SaaS) or GitHub Enterprise Server (on premises).
gitHubConfiguration_type :: Lens.Lens' GitHubConfiguration (Prelude.Maybe Type)
gitHubConfiguration_type = Lens.lens (\GitHubConfiguration' {type'} -> type') (\s@GitHubConfiguration' {} a -> s {type' = a} :: GitHubConfiguration)

-- | A list of names of the specific repositories you want to index.
gitHubConfiguration_repositoryFilter :: Lens.Lens' GitHubConfiguration (Prelude.Maybe [Prelude.Text])
gitHubConfiguration_repositoryFilter = Lens.lens (\GitHubConfiguration' {repositoryFilter} -> repositoryFilter) (\s@GitHubConfiguration' {} a -> s {repositoryFilter = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Configuration information to connect to GitHub Enterprise Cloud (SaaS).
gitHubConfiguration_saaSConfiguration :: Lens.Lens' GitHubConfiguration (Prelude.Maybe SaaSConfiguration)
gitHubConfiguration_saaSConfiguration = Lens.lens (\GitHubConfiguration' {saaSConfiguration} -> saaSConfiguration) (\s@GitHubConfiguration' {} a -> s {saaSConfiguration = a} :: GitHubConfiguration)

-- | A list of @DataSourceToIndexFieldMapping@ objects that map GitHub
-- repository attributes or field names to Amazon Kendra index field names.
-- To create custom fields, use the @UpdateIndex@ API before you map to
-- GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
gitHubConfiguration_gitHubRepositoryConfigurationFieldMappings :: Lens.Lens' GitHubConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
gitHubConfiguration_gitHubRepositoryConfigurationFieldMappings = Lens.lens (\GitHubConfiguration' {gitHubRepositoryConfigurationFieldMappings} -> gitHubRepositoryConfigurationFieldMappings) (\s@GitHubConfiguration' {} a -> s {gitHubRepositoryConfigurationFieldMappings = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

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
-- field names of GitHub issue comments to Amazon Kendra index field names.
-- To create custom fields, use the @UpdateIndex@ API before you map to
-- GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
gitHubConfiguration_gitHubIssueCommentConfigurationFieldMappings :: Lens.Lens' GitHubConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
gitHubConfiguration_gitHubIssueCommentConfigurationFieldMappings = Lens.lens (\GitHubConfiguration' {gitHubIssueCommentConfigurationFieldMappings} -> gitHubIssueCommentConfigurationFieldMappings) (\s@GitHubConfiguration' {} a -> s {gitHubIssueCommentConfigurationFieldMappings = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Configuration information to connect to GitHub Enterprise Server (on
-- premises).
gitHubConfiguration_onPremiseConfiguration :: Lens.Lens' GitHubConfiguration (Prelude.Maybe OnPremiseConfiguration)
gitHubConfiguration_onPremiseConfiguration = Lens.lens (\GitHubConfiguration' {onPremiseConfiguration} -> onPremiseConfiguration) (\s@GitHubConfiguration' {} a -> s {onPremiseConfiguration = a} :: GitHubConfiguration)

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
-- field names of GitHub pull requests to Amazon Kendra index field names.
-- To create custom fields, use the @UpdateIndex@ API before you map to
-- GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
gitHubConfiguration_gitHubPullRequestDocumentConfigurationFieldMappings :: Lens.Lens' GitHubConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
gitHubConfiguration_gitHubPullRequestDocumentConfigurationFieldMappings = Lens.lens (\GitHubConfiguration' {gitHubPullRequestDocumentConfigurationFieldMappings} -> gitHubPullRequestDocumentConfigurationFieldMappings) (\s@GitHubConfiguration' {} a -> s {gitHubPullRequestDocumentConfigurationFieldMappings = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to exclude certain folder names in
-- your GitHub repository or repositories. Folder names that match the
-- patterns are excluded from the index. Folder names that don\'t match the
-- patterns are included in the index. If a folder matches both an
-- exclusion and inclusion pattern, the exclusion pattern takes precedence
-- and the folder isn\'t included in the index.
gitHubConfiguration_exclusionFolderNamePatterns :: Lens.Lens' GitHubConfiguration (Prelude.Maybe [Prelude.Text])
gitHubConfiguration_exclusionFolderNamePatterns = Lens.lens (\GitHubConfiguration' {exclusionFolderNamePatterns} -> exclusionFolderNamePatterns) (\s@GitHubConfiguration' {} a -> s {exclusionFolderNamePatterns = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub issues to Amazon Kendra index field names. To
-- create custom fields, use the @UpdateIndex@ API before you map to GitHub
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
gitHubConfiguration_gitHubIssueDocumentConfigurationFieldMappings :: Lens.Lens' GitHubConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
gitHubConfiguration_gitHubIssueDocumentConfigurationFieldMappings = Lens.lens (\GitHubConfiguration' {gitHubIssueDocumentConfigurationFieldMappings} -> gitHubIssueDocumentConfigurationFieldMappings) (\s@GitHubConfiguration' {} a -> s {gitHubIssueDocumentConfigurationFieldMappings = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to include certain folder names in
-- your GitHub repository or repositories. Folder names that match the
-- patterns are included in the index. Folder names that don\'t match the
-- patterns are excluded from the index. If a folder matches both an
-- inclusion and exclusion pattern, the exclusion pattern takes precedence
-- and the folder isn\'t included in the index.
gitHubConfiguration_inclusionFolderNamePatterns :: Lens.Lens' GitHubConfiguration (Prelude.Maybe [Prelude.Text])
gitHubConfiguration_inclusionFolderNamePatterns = Lens.lens (\GitHubConfiguration' {inclusionFolderNamePatterns} -> inclusionFolderNamePatterns) (\s@GitHubConfiguration' {} a -> s {inclusionFolderNamePatterns = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to exclude certain file types in
-- your GitHub repository or repositories. File types that match the
-- patterns are excluded from the index. File types that don\'t match the
-- patterns are included in the index. If a file matches both an exclusion
-- and inclusion pattern, the exclusion pattern takes precedence and the
-- file isn\'t included in the index.
gitHubConfiguration_exclusionFileTypePatterns :: Lens.Lens' GitHubConfiguration (Prelude.Maybe [Prelude.Text])
gitHubConfiguration_exclusionFileTypePatterns = Lens.lens (\GitHubConfiguration' {exclusionFileTypePatterns} -> exclusionFileTypePatterns) (\s@GitHubConfiguration' {} a -> s {exclusionFileTypePatterns = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

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

-- | A list of regular expression patterns to exclude certain file names in
-- your GitHub repository or repositories. File names that match the
-- patterns are excluded from the index. File names that don\'t match the
-- patterns are included in the index. If a file matches both an exclusion
-- and inclusion pattern, the exclusion pattern takes precedence and the
-- file isn\'t included in the index.
gitHubConfiguration_exclusionFileNamePatterns :: Lens.Lens' GitHubConfiguration (Prelude.Maybe [Prelude.Text])
gitHubConfiguration_exclusionFileNamePatterns = Lens.lens (\GitHubConfiguration' {exclusionFileNamePatterns} -> exclusionFileNamePatterns) (\s@GitHubConfiguration' {} a -> s {exclusionFileNamePatterns = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DataSourceToIndexFieldMapping@ objects that map attributes or
-- field names of GitHub pull request attachments to Amazon Kendra index
-- field names. To create custom fields, use the @UpdateIndex@ API before
-- you map to GitHub fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The GitHub data source field names must exist in your GitHub custom
-- metadata.
gitHubConfiguration_gitHubPullRequestDocumentAttachmentConfigurationFieldMappings :: Lens.Lens' GitHubConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
gitHubConfiguration_gitHubPullRequestDocumentAttachmentConfigurationFieldMappings = Lens.lens (\GitHubConfiguration' {gitHubPullRequestDocumentAttachmentConfigurationFieldMappings} -> gitHubPullRequestDocumentAttachmentConfigurationFieldMappings) (\s@GitHubConfiguration' {} a -> s {gitHubPullRequestDocumentAttachmentConfigurationFieldMappings = a} :: GitHubConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of an Secrets Manager secret that
-- contains the key-value pairs required to connect to your GitHub. The
-- secret must contain a JSON structure with the following keys:
--
-- -   githubToken—The access token created in GitHub. For more information
--     on creating a token in GitHub, see
--     <https://docs.aws.amazon.com/kendra/latest/dg/data-source-github.html#github-authentication Authentication for a GitHub data source>.
gitHubConfiguration_secretArn :: Lens.Lens' GitHubConfiguration Prelude.Text
gitHubConfiguration_secretArn = Lens.lens (\GitHubConfiguration' {secretArn} -> secretArn) (\s@GitHubConfiguration' {} a -> s {secretArn = a} :: GitHubConfiguration)

instance Core.FromJSON GitHubConfiguration where
  parseJSON =
    Core.withObject
      "GitHubConfiguration"
      ( \x ->
          GitHubConfiguration'
            Prelude.<$> ( x
                            Core..:? "GitHubIssueAttachmentConfigurationFieldMappings"
                        )
            Prelude.<*> (x Core..:? "UseChangeLog")
            Prelude.<*> (x Core..:? "VpcConfiguration")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> ( x Core..:? "RepositoryFilter"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "SaaSConfiguration")
            Prelude.<*> ( x
                            Core..:? "GitHubRepositoryConfigurationFieldMappings"
                        )
            Prelude.<*> ( x
                            Core..:? "GitHubPullRequestCommentConfigurationFieldMappings"
                        )
            Prelude.<*> ( x
                            Core..:? "GitHubIssueCommentConfigurationFieldMappings"
                        )
            Prelude.<*> (x Core..:? "OnPremiseConfiguration")
            Prelude.<*> (x Core..:? "GitHubCommitConfigurationFieldMappings")
            Prelude.<*> (x Core..:? "GitHubDocumentCrawlProperties")
            Prelude.<*> ( x
                            Core..:? "GitHubPullRequestDocumentConfigurationFieldMappings"
                        )
            Prelude.<*> ( x Core..:? "ExclusionFolderNamePatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Core..:? "GitHubIssueDocumentConfigurationFieldMappings"
                        )
            Prelude.<*> ( x Core..:? "InclusionFolderNamePatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "ExclusionFileTypePatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "InclusionFileNamePatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "InclusionFileTypePatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "ExclusionFileNamePatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Core..:? "GitHubPullRequestDocumentAttachmentConfigurationFieldMappings"
                        )
            Prelude.<*> (x Core..: "SecretArn")
      )

instance Prelude.Hashable GitHubConfiguration where
  hashWithSalt _salt GitHubConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` gitHubIssueAttachmentConfigurationFieldMappings
      `Prelude.hashWithSalt` useChangeLog
      `Prelude.hashWithSalt` vpcConfiguration
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` repositoryFilter
      `Prelude.hashWithSalt` saaSConfiguration
      `Prelude.hashWithSalt` gitHubRepositoryConfigurationFieldMappings
      `Prelude.hashWithSalt` gitHubPullRequestCommentConfigurationFieldMappings
      `Prelude.hashWithSalt` gitHubIssueCommentConfigurationFieldMappings
      `Prelude.hashWithSalt` onPremiseConfiguration
      `Prelude.hashWithSalt` gitHubCommitConfigurationFieldMappings
      `Prelude.hashWithSalt` gitHubDocumentCrawlProperties
      `Prelude.hashWithSalt` gitHubPullRequestDocumentConfigurationFieldMappings
      `Prelude.hashWithSalt` exclusionFolderNamePatterns
      `Prelude.hashWithSalt` gitHubIssueDocumentConfigurationFieldMappings
      `Prelude.hashWithSalt` inclusionFolderNamePatterns
      `Prelude.hashWithSalt` exclusionFileTypePatterns
      `Prelude.hashWithSalt` inclusionFileNamePatterns
      `Prelude.hashWithSalt` inclusionFileTypePatterns
      `Prelude.hashWithSalt` exclusionFileNamePatterns
      `Prelude.hashWithSalt` gitHubPullRequestDocumentAttachmentConfigurationFieldMappings
      `Prelude.hashWithSalt` secretArn

instance Prelude.NFData GitHubConfiguration where
  rnf GitHubConfiguration' {..} =
    Prelude.rnf
      gitHubIssueAttachmentConfigurationFieldMappings
      `Prelude.seq` Prelude.rnf useChangeLog
      `Prelude.seq` Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf repositoryFilter
      `Prelude.seq` Prelude.rnf saaSConfiguration
      `Prelude.seq` Prelude.rnf
        gitHubRepositoryConfigurationFieldMappings
      `Prelude.seq` Prelude.rnf
        gitHubPullRequestCommentConfigurationFieldMappings
      `Prelude.seq` Prelude.rnf
        gitHubIssueCommentConfigurationFieldMappings
      `Prelude.seq` Prelude.rnf onPremiseConfiguration
      `Prelude.seq` Prelude.rnf
        gitHubCommitConfigurationFieldMappings
      `Prelude.seq` Prelude.rnf gitHubDocumentCrawlProperties
      `Prelude.seq` Prelude.rnf
        gitHubPullRequestDocumentConfigurationFieldMappings
      `Prelude.seq` Prelude.rnf exclusionFolderNamePatterns
      `Prelude.seq` Prelude.rnf
        gitHubIssueDocumentConfigurationFieldMappings
      `Prelude.seq` Prelude.rnf
        inclusionFolderNamePatterns
      `Prelude.seq` Prelude.rnf
        exclusionFileTypePatterns
      `Prelude.seq` Prelude.rnf
        inclusionFileNamePatterns
      `Prelude.seq` Prelude.rnf
        inclusionFileTypePatterns
      `Prelude.seq` Prelude.rnf
        exclusionFileNamePatterns
      `Prelude.seq` Prelude.rnf
        gitHubPullRequestDocumentAttachmentConfigurationFieldMappings
      `Prelude.seq` Prelude.rnf secretArn

instance Core.ToJSON GitHubConfiguration where
  toJSON GitHubConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ( "GitHubIssueAttachmentConfigurationFieldMappings"
                Core..=
            )
              Prelude.<$> gitHubIssueAttachmentConfigurationFieldMappings,
            ("UseChangeLog" Core..=) Prelude.<$> useChangeLog,
            ("VpcConfiguration" Core..=)
              Prelude.<$> vpcConfiguration,
            ("Type" Core..=) Prelude.<$> type',
            ("RepositoryFilter" Core..=)
              Prelude.<$> repositoryFilter,
            ("SaaSConfiguration" Core..=)
              Prelude.<$> saaSConfiguration,
            ( "GitHubRepositoryConfigurationFieldMappings"
                Core..=
            )
              Prelude.<$> gitHubRepositoryConfigurationFieldMappings,
            ( "GitHubPullRequestCommentConfigurationFieldMappings"
                Core..=
            )
              Prelude.<$> gitHubPullRequestCommentConfigurationFieldMappings,
            ( "GitHubIssueCommentConfigurationFieldMappings"
                Core..=
            )
              Prelude.<$> gitHubIssueCommentConfigurationFieldMappings,
            ("OnPremiseConfiguration" Core..=)
              Prelude.<$> onPremiseConfiguration,
            ("GitHubCommitConfigurationFieldMappings" Core..=)
              Prelude.<$> gitHubCommitConfigurationFieldMappings,
            ("GitHubDocumentCrawlProperties" Core..=)
              Prelude.<$> gitHubDocumentCrawlProperties,
            ( "GitHubPullRequestDocumentConfigurationFieldMappings"
                Core..=
            )
              Prelude.<$> gitHubPullRequestDocumentConfigurationFieldMappings,
            ("ExclusionFolderNamePatterns" Core..=)
              Prelude.<$> exclusionFolderNamePatterns,
            ( "GitHubIssueDocumentConfigurationFieldMappings"
                Core..=
            )
              Prelude.<$> gitHubIssueDocumentConfigurationFieldMappings,
            ("InclusionFolderNamePatterns" Core..=)
              Prelude.<$> inclusionFolderNamePatterns,
            ("ExclusionFileTypePatterns" Core..=)
              Prelude.<$> exclusionFileTypePatterns,
            ("InclusionFileNamePatterns" Core..=)
              Prelude.<$> inclusionFileNamePatterns,
            ("InclusionFileTypePatterns" Core..=)
              Prelude.<$> inclusionFileTypePatterns,
            ("ExclusionFileNamePatterns" Core..=)
              Prelude.<$> exclusionFileNamePatterns,
            ( "GitHubPullRequestDocumentAttachmentConfigurationFieldMappings"
                Core..=
            )
              Prelude.<$> gitHubPullRequestDocumentAttachmentConfigurationFieldMappings,
            Prelude.Just ("SecretArn" Core..= secretArn)
          ]
      )
