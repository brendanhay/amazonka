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
-- Module      : Amazonka.Kendra.Types.JiraConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.JiraConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import Amazonka.Kendra.Types.DataSourceVpcConfiguration
import Amazonka.Kendra.Types.IssueSubEntity
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to Jira as your data
-- source.
--
-- /See:/ 'newJiraConfiguration' smart constructor.
data JiraConfiguration = JiraConfiguration'
  { -- | @TRUE@ to use the Jira change log to determine which documents require
    -- updating in the index. Depending on the change log\'s size, it may take
    -- longer for Amazon Kendra to use the change log than to scan all of your
    -- documents in Jira.
    useChangeLog :: Prelude.Maybe Prelude.Bool,
    -- | Configuration information for an Amazon Virtual Private Cloud to connect
    -- to your Jira. Your Jira account must reside inside your VPC.
    vpcConfiguration :: Prelude.Maybe DataSourceVpcConfiguration,
    -- | A list of DataSourceToIndexFieldMapping objects that map attributes or
    -- field names of Jira projects to Amazon Kendra index field names. To
    -- create custom fields, use the UpdateIndex API before you map to Jira
    -- fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Jira data source field names must exist in your Jira custom
    -- metadata.
    projectFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of DataSourceToIndexFieldMapping objects that map attributes or
    -- field names of Jira attachments to Amazon Kendra index field names. To
    -- create custom fields, use the UpdateIndex API before you map to Jira
    -- fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Jira data source field names must exist in your Jira custom
    -- metadata.
    attachmentFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | Specify which issue types to crawl in your Jira data source. You can
    -- specify one or more of these options to crawl.
    issueType :: Prelude.Maybe [Prelude.Text],
    -- | A list of DataSourceToIndexFieldMapping objects that map attributes or
    -- field names of Jira comments to Amazon Kendra index field names. To
    -- create custom fields, use the UpdateIndex API before you map to Jira
    -- fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Jira data source field names must exist in your Jira custom
    -- metadata.
    commentFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | Specify whether to crawl comments, attachments, and work logs. You can
    -- specify one or more of these options.
    issueSubEntityFilter :: Prelude.Maybe [IssueSubEntity],
    -- | A list of regular expression patterns to include certain file paths,
    -- file names, and file types in your Jira data source. Files that match
    -- the patterns are included in the index. Files that don\'t match the
    -- patterns are excluded from the index. If a file matches both an
    -- inclusion pattern and an exclusion pattern, the exclusion pattern takes
    -- precedence and the file isn\'t included in the index.
    inclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | Specify which statuses to crawl in your Jira data source. You can
    -- specify one or more of these options to crawl.
    status :: Prelude.Maybe [Prelude.Text],
    -- | Specify which projects to crawl in your Jira data source. You can
    -- specify one or more Jira project IDs.
    project :: Prelude.Maybe [Prelude.Text],
    -- | A list of DataSourceToIndexFieldMapping objects that map attributes or
    -- field names of Jira work logs to Amazon Kendra index field names. To
    -- create custom fields, use the UpdateIndex API before you map to Jira
    -- fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Jira data source field names must exist in your Jira custom
    -- metadata.
    workLogFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of DataSourceToIndexFieldMapping objects that map attributes or
    -- field names of Jira issues to Amazon Kendra index field names. To create
    -- custom fields, use the UpdateIndex API before you map to Jira fields.
    -- For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Jira data source field names must exist in your Jira custom
    -- metadata.
    issueFieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of regular expression patterns to exclude certain file paths,
    -- file names, and file types in your Jira data source. Files that match
    -- the patterns are excluded from the index. Files that don’t match the
    -- patterns are included in the index. If a file matches both an inclusion
    -- pattern and an exclusion pattern, the exclusion pattern takes precedence
    -- and the file isn\'t included in the index.
    exclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | The URL of the Jira account. For example, /company.atlassian.net/ or
    -- /https:\/\/jira.company.com/. You can find your Jira account URL in the
    -- URL of your profile page for Jira desktop.
    jiraAccountUrl :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a secret in Secrets Manager contains
    -- the key-value pairs required to connect to your Jira data source. The
    -- secret must contain a JSON structure with the following keys:
    --
    -- -   jiraId—The Jira username.
    --
    -- -   jiraCredentials—The Jira API token. For more information on creating
    --     an API token in Jira, see
    --     <https://docs.aws.amazon.com/kendra/latest/dg/data-source-jira.html#jira-authentication Authentication for a Jira data source>.
    secretArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JiraConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'useChangeLog', 'jiraConfiguration_useChangeLog' - @TRUE@ to use the Jira change log to determine which documents require
-- updating in the index. Depending on the change log\'s size, it may take
-- longer for Amazon Kendra to use the change log than to scan all of your
-- documents in Jira.
--
-- 'vpcConfiguration', 'jiraConfiguration_vpcConfiguration' - Configuration information for an Amazon Virtual Private Cloud to connect
-- to your Jira. Your Jira account must reside inside your VPC.
--
-- 'projectFieldMappings', 'jiraConfiguration_projectFieldMappings' - A list of DataSourceToIndexFieldMapping objects that map attributes or
-- field names of Jira projects to Amazon Kendra index field names. To
-- create custom fields, use the UpdateIndex API before you map to Jira
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Jira data source field names must exist in your Jira custom
-- metadata.
--
-- 'attachmentFieldMappings', 'jiraConfiguration_attachmentFieldMappings' - A list of DataSourceToIndexFieldMapping objects that map attributes or
-- field names of Jira attachments to Amazon Kendra index field names. To
-- create custom fields, use the UpdateIndex API before you map to Jira
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Jira data source field names must exist in your Jira custom
-- metadata.
--
-- 'issueType', 'jiraConfiguration_issueType' - Specify which issue types to crawl in your Jira data source. You can
-- specify one or more of these options to crawl.
--
-- 'commentFieldMappings', 'jiraConfiguration_commentFieldMappings' - A list of DataSourceToIndexFieldMapping objects that map attributes or
-- field names of Jira comments to Amazon Kendra index field names. To
-- create custom fields, use the UpdateIndex API before you map to Jira
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Jira data source field names must exist in your Jira custom
-- metadata.
--
-- 'issueSubEntityFilter', 'jiraConfiguration_issueSubEntityFilter' - Specify whether to crawl comments, attachments, and work logs. You can
-- specify one or more of these options.
--
-- 'inclusionPatterns', 'jiraConfiguration_inclusionPatterns' - A list of regular expression patterns to include certain file paths,
-- file names, and file types in your Jira data source. Files that match
-- the patterns are included in the index. Files that don\'t match the
-- patterns are excluded from the index. If a file matches both an
-- inclusion pattern and an exclusion pattern, the exclusion pattern takes
-- precedence and the file isn\'t included in the index.
--
-- 'status', 'jiraConfiguration_status' - Specify which statuses to crawl in your Jira data source. You can
-- specify one or more of these options to crawl.
--
-- 'project', 'jiraConfiguration_project' - Specify which projects to crawl in your Jira data source. You can
-- specify one or more Jira project IDs.
--
-- 'workLogFieldMappings', 'jiraConfiguration_workLogFieldMappings' - A list of DataSourceToIndexFieldMapping objects that map attributes or
-- field names of Jira work logs to Amazon Kendra index field names. To
-- create custom fields, use the UpdateIndex API before you map to Jira
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Jira data source field names must exist in your Jira custom
-- metadata.
--
-- 'issueFieldMappings', 'jiraConfiguration_issueFieldMappings' - A list of DataSourceToIndexFieldMapping objects that map attributes or
-- field names of Jira issues to Amazon Kendra index field names. To create
-- custom fields, use the UpdateIndex API before you map to Jira fields.
-- For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Jira data source field names must exist in your Jira custom
-- metadata.
--
-- 'exclusionPatterns', 'jiraConfiguration_exclusionPatterns' - A list of regular expression patterns to exclude certain file paths,
-- file names, and file types in your Jira data source. Files that match
-- the patterns are excluded from the index. Files that don’t match the
-- patterns are included in the index. If a file matches both an inclusion
-- pattern and an exclusion pattern, the exclusion pattern takes precedence
-- and the file isn\'t included in the index.
--
-- 'jiraAccountUrl', 'jiraConfiguration_jiraAccountUrl' - The URL of the Jira account. For example, /company.atlassian.net/ or
-- /https:\/\/jira.company.com/. You can find your Jira account URL in the
-- URL of your profile page for Jira desktop.
--
-- 'secretArn', 'jiraConfiguration_secretArn' - The Amazon Resource Name (ARN) of a secret in Secrets Manager contains
-- the key-value pairs required to connect to your Jira data source. The
-- secret must contain a JSON structure with the following keys:
--
-- -   jiraId—The Jira username.
--
-- -   jiraCredentials—The Jira API token. For more information on creating
--     an API token in Jira, see
--     <https://docs.aws.amazon.com/kendra/latest/dg/data-source-jira.html#jira-authentication Authentication for a Jira data source>.
newJiraConfiguration ::
  -- | 'jiraAccountUrl'
  Prelude.Text ->
  -- | 'secretArn'
  Prelude.Text ->
  JiraConfiguration
newJiraConfiguration pJiraAccountUrl_ pSecretArn_ =
  JiraConfiguration'
    { useChangeLog = Prelude.Nothing,
      vpcConfiguration = Prelude.Nothing,
      projectFieldMappings = Prelude.Nothing,
      attachmentFieldMappings = Prelude.Nothing,
      issueType = Prelude.Nothing,
      commentFieldMappings = Prelude.Nothing,
      issueSubEntityFilter = Prelude.Nothing,
      inclusionPatterns = Prelude.Nothing,
      status = Prelude.Nothing,
      project = Prelude.Nothing,
      workLogFieldMappings = Prelude.Nothing,
      issueFieldMappings = Prelude.Nothing,
      exclusionPatterns = Prelude.Nothing,
      jiraAccountUrl = pJiraAccountUrl_,
      secretArn = pSecretArn_
    }

-- | @TRUE@ to use the Jira change log to determine which documents require
-- updating in the index. Depending on the change log\'s size, it may take
-- longer for Amazon Kendra to use the change log than to scan all of your
-- documents in Jira.
jiraConfiguration_useChangeLog :: Lens.Lens' JiraConfiguration (Prelude.Maybe Prelude.Bool)
jiraConfiguration_useChangeLog = Lens.lens (\JiraConfiguration' {useChangeLog} -> useChangeLog) (\s@JiraConfiguration' {} a -> s {useChangeLog = a} :: JiraConfiguration)

-- | Configuration information for an Amazon Virtual Private Cloud to connect
-- to your Jira. Your Jira account must reside inside your VPC.
jiraConfiguration_vpcConfiguration :: Lens.Lens' JiraConfiguration (Prelude.Maybe DataSourceVpcConfiguration)
jiraConfiguration_vpcConfiguration = Lens.lens (\JiraConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@JiraConfiguration' {} a -> s {vpcConfiguration = a} :: JiraConfiguration)

-- | A list of DataSourceToIndexFieldMapping objects that map attributes or
-- field names of Jira projects to Amazon Kendra index field names. To
-- create custom fields, use the UpdateIndex API before you map to Jira
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Jira data source field names must exist in your Jira custom
-- metadata.
jiraConfiguration_projectFieldMappings :: Lens.Lens' JiraConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
jiraConfiguration_projectFieldMappings = Lens.lens (\JiraConfiguration' {projectFieldMappings} -> projectFieldMappings) (\s@JiraConfiguration' {} a -> s {projectFieldMappings = a} :: JiraConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of DataSourceToIndexFieldMapping objects that map attributes or
-- field names of Jira attachments to Amazon Kendra index field names. To
-- create custom fields, use the UpdateIndex API before you map to Jira
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Jira data source field names must exist in your Jira custom
-- metadata.
jiraConfiguration_attachmentFieldMappings :: Lens.Lens' JiraConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
jiraConfiguration_attachmentFieldMappings = Lens.lens (\JiraConfiguration' {attachmentFieldMappings} -> attachmentFieldMappings) (\s@JiraConfiguration' {} a -> s {attachmentFieldMappings = a} :: JiraConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Specify which issue types to crawl in your Jira data source. You can
-- specify one or more of these options to crawl.
jiraConfiguration_issueType :: Lens.Lens' JiraConfiguration (Prelude.Maybe [Prelude.Text])
jiraConfiguration_issueType = Lens.lens (\JiraConfiguration' {issueType} -> issueType) (\s@JiraConfiguration' {} a -> s {issueType = a} :: JiraConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of DataSourceToIndexFieldMapping objects that map attributes or
-- field names of Jira comments to Amazon Kendra index field names. To
-- create custom fields, use the UpdateIndex API before you map to Jira
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Jira data source field names must exist in your Jira custom
-- metadata.
jiraConfiguration_commentFieldMappings :: Lens.Lens' JiraConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
jiraConfiguration_commentFieldMappings = Lens.lens (\JiraConfiguration' {commentFieldMappings} -> commentFieldMappings) (\s@JiraConfiguration' {} a -> s {commentFieldMappings = a} :: JiraConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Specify whether to crawl comments, attachments, and work logs. You can
-- specify one or more of these options.
jiraConfiguration_issueSubEntityFilter :: Lens.Lens' JiraConfiguration (Prelude.Maybe [IssueSubEntity])
jiraConfiguration_issueSubEntityFilter = Lens.lens (\JiraConfiguration' {issueSubEntityFilter} -> issueSubEntityFilter) (\s@JiraConfiguration' {} a -> s {issueSubEntityFilter = a} :: JiraConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to include certain file paths,
-- file names, and file types in your Jira data source. Files that match
-- the patterns are included in the index. Files that don\'t match the
-- patterns are excluded from the index. If a file matches both an
-- inclusion pattern and an exclusion pattern, the exclusion pattern takes
-- precedence and the file isn\'t included in the index.
jiraConfiguration_inclusionPatterns :: Lens.Lens' JiraConfiguration (Prelude.Maybe [Prelude.Text])
jiraConfiguration_inclusionPatterns = Lens.lens (\JiraConfiguration' {inclusionPatterns} -> inclusionPatterns) (\s@JiraConfiguration' {} a -> s {inclusionPatterns = a} :: JiraConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Specify which statuses to crawl in your Jira data source. You can
-- specify one or more of these options to crawl.
jiraConfiguration_status :: Lens.Lens' JiraConfiguration (Prelude.Maybe [Prelude.Text])
jiraConfiguration_status = Lens.lens (\JiraConfiguration' {status} -> status) (\s@JiraConfiguration' {} a -> s {status = a} :: JiraConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Specify which projects to crawl in your Jira data source. You can
-- specify one or more Jira project IDs.
jiraConfiguration_project :: Lens.Lens' JiraConfiguration (Prelude.Maybe [Prelude.Text])
jiraConfiguration_project = Lens.lens (\JiraConfiguration' {project} -> project) (\s@JiraConfiguration' {} a -> s {project = a} :: JiraConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of DataSourceToIndexFieldMapping objects that map attributes or
-- field names of Jira work logs to Amazon Kendra index field names. To
-- create custom fields, use the UpdateIndex API before you map to Jira
-- fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Jira data source field names must exist in your Jira custom
-- metadata.
jiraConfiguration_workLogFieldMappings :: Lens.Lens' JiraConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
jiraConfiguration_workLogFieldMappings = Lens.lens (\JiraConfiguration' {workLogFieldMappings} -> workLogFieldMappings) (\s@JiraConfiguration' {} a -> s {workLogFieldMappings = a} :: JiraConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of DataSourceToIndexFieldMapping objects that map attributes or
-- field names of Jira issues to Amazon Kendra index field names. To create
-- custom fields, use the UpdateIndex API before you map to Jira fields.
-- For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Jira data source field names must exist in your Jira custom
-- metadata.
jiraConfiguration_issueFieldMappings :: Lens.Lens' JiraConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
jiraConfiguration_issueFieldMappings = Lens.lens (\JiraConfiguration' {issueFieldMappings} -> issueFieldMappings) (\s@JiraConfiguration' {} a -> s {issueFieldMappings = a} :: JiraConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to exclude certain file paths,
-- file names, and file types in your Jira data source. Files that match
-- the patterns are excluded from the index. Files that don’t match the
-- patterns are included in the index. If a file matches both an inclusion
-- pattern and an exclusion pattern, the exclusion pattern takes precedence
-- and the file isn\'t included in the index.
jiraConfiguration_exclusionPatterns :: Lens.Lens' JiraConfiguration (Prelude.Maybe [Prelude.Text])
jiraConfiguration_exclusionPatterns = Lens.lens (\JiraConfiguration' {exclusionPatterns} -> exclusionPatterns) (\s@JiraConfiguration' {} a -> s {exclusionPatterns = a} :: JiraConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The URL of the Jira account. For example, /company.atlassian.net/ or
-- /https:\/\/jira.company.com/. You can find your Jira account URL in the
-- URL of your profile page for Jira desktop.
jiraConfiguration_jiraAccountUrl :: Lens.Lens' JiraConfiguration Prelude.Text
jiraConfiguration_jiraAccountUrl = Lens.lens (\JiraConfiguration' {jiraAccountUrl} -> jiraAccountUrl) (\s@JiraConfiguration' {} a -> s {jiraAccountUrl = a} :: JiraConfiguration)

-- | The Amazon Resource Name (ARN) of a secret in Secrets Manager contains
-- the key-value pairs required to connect to your Jira data source. The
-- secret must contain a JSON structure with the following keys:
--
-- -   jiraId—The Jira username.
--
-- -   jiraCredentials—The Jira API token. For more information on creating
--     an API token in Jira, see
--     <https://docs.aws.amazon.com/kendra/latest/dg/data-source-jira.html#jira-authentication Authentication for a Jira data source>.
jiraConfiguration_secretArn :: Lens.Lens' JiraConfiguration Prelude.Text
jiraConfiguration_secretArn = Lens.lens (\JiraConfiguration' {secretArn} -> secretArn) (\s@JiraConfiguration' {} a -> s {secretArn = a} :: JiraConfiguration)

instance Data.FromJSON JiraConfiguration where
  parseJSON =
    Data.withObject
      "JiraConfiguration"
      ( \x ->
          JiraConfiguration'
            Prelude.<$> (x Data..:? "UseChangeLog")
            Prelude.<*> (x Data..:? "VpcConfiguration")
            Prelude.<*> (x Data..:? "ProjectFieldMappings")
            Prelude.<*> (x Data..:? "AttachmentFieldMappings")
            Prelude.<*> (x Data..:? "IssueType" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CommentFieldMappings")
            Prelude.<*> ( x Data..:? "IssueSubEntityFilter"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "InclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Status" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Project" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "WorkLogFieldMappings")
            Prelude.<*> (x Data..:? "IssueFieldMappings")
            Prelude.<*> ( x Data..:? "ExclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "JiraAccountUrl")
            Prelude.<*> (x Data..: "SecretArn")
      )

instance Prelude.Hashable JiraConfiguration where
  hashWithSalt _salt JiraConfiguration' {..} =
    _salt `Prelude.hashWithSalt` useChangeLog
      `Prelude.hashWithSalt` vpcConfiguration
      `Prelude.hashWithSalt` projectFieldMappings
      `Prelude.hashWithSalt` attachmentFieldMappings
      `Prelude.hashWithSalt` issueType
      `Prelude.hashWithSalt` commentFieldMappings
      `Prelude.hashWithSalt` issueSubEntityFilter
      `Prelude.hashWithSalt` inclusionPatterns
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` project
      `Prelude.hashWithSalt` workLogFieldMappings
      `Prelude.hashWithSalt` issueFieldMappings
      `Prelude.hashWithSalt` exclusionPatterns
      `Prelude.hashWithSalt` jiraAccountUrl
      `Prelude.hashWithSalt` secretArn

instance Prelude.NFData JiraConfiguration where
  rnf JiraConfiguration' {..} =
    Prelude.rnf useChangeLog
      `Prelude.seq` Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf projectFieldMappings
      `Prelude.seq` Prelude.rnf attachmentFieldMappings
      `Prelude.seq` Prelude.rnf issueType
      `Prelude.seq` Prelude.rnf commentFieldMappings
      `Prelude.seq` Prelude.rnf issueSubEntityFilter
      `Prelude.seq` Prelude.rnf inclusionPatterns
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf project
      `Prelude.seq` Prelude.rnf workLogFieldMappings
      `Prelude.seq` Prelude.rnf issueFieldMappings
      `Prelude.seq` Prelude.rnf exclusionPatterns
      `Prelude.seq` Prelude.rnf jiraAccountUrl
      `Prelude.seq` Prelude.rnf secretArn

instance Data.ToJSON JiraConfiguration where
  toJSON JiraConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UseChangeLog" Data..=) Prelude.<$> useChangeLog,
            ("VpcConfiguration" Data..=)
              Prelude.<$> vpcConfiguration,
            ("ProjectFieldMappings" Data..=)
              Prelude.<$> projectFieldMappings,
            ("AttachmentFieldMappings" Data..=)
              Prelude.<$> attachmentFieldMappings,
            ("IssueType" Data..=) Prelude.<$> issueType,
            ("CommentFieldMappings" Data..=)
              Prelude.<$> commentFieldMappings,
            ("IssueSubEntityFilter" Data..=)
              Prelude.<$> issueSubEntityFilter,
            ("InclusionPatterns" Data..=)
              Prelude.<$> inclusionPatterns,
            ("Status" Data..=) Prelude.<$> status,
            ("Project" Data..=) Prelude.<$> project,
            ("WorkLogFieldMappings" Data..=)
              Prelude.<$> workLogFieldMappings,
            ("IssueFieldMappings" Data..=)
              Prelude.<$> issueFieldMappings,
            ("ExclusionPatterns" Data..=)
              Prelude.<$> exclusionPatterns,
            Prelude.Just
              ("JiraAccountUrl" Data..= jiraAccountUrl),
            Prelude.Just ("SecretArn" Data..= secretArn)
          ]
      )
