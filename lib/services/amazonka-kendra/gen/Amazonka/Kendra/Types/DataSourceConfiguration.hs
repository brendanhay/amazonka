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
-- Module      : Amazonka.Kendra.Types.DataSourceConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DataSourceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.AlfrescoConfiguration
import Amazonka.Kendra.Types.BoxConfiguration
import Amazonka.Kendra.Types.ConfluenceConfiguration
import Amazonka.Kendra.Types.DatabaseConfiguration
import Amazonka.Kendra.Types.FsxConfiguration
import Amazonka.Kendra.Types.GitHubConfiguration
import Amazonka.Kendra.Types.GoogleDriveConfiguration
import Amazonka.Kendra.Types.JiraConfiguration
import Amazonka.Kendra.Types.OneDriveConfiguration
import Amazonka.Kendra.Types.QuipConfiguration
import Amazonka.Kendra.Types.S3DataSourceConfiguration
import Amazonka.Kendra.Types.SalesforceConfiguration
import Amazonka.Kendra.Types.ServiceNowConfiguration
import Amazonka.Kendra.Types.SharePointConfiguration
import Amazonka.Kendra.Types.SlackConfiguration
import Amazonka.Kendra.Types.TemplateConfiguration
import Amazonka.Kendra.Types.WebCrawlerConfiguration
import Amazonka.Kendra.Types.WorkDocsConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for an Amazon Kendra data source.
--
-- /See:/ 'newDataSourceConfiguration' smart constructor.
data DataSourceConfiguration = DataSourceConfiguration'
  { -- | Provides the configuration information to connect to Google Drive as
    -- your data source.
    googleDriveConfiguration :: Prelude.Maybe GoogleDriveConfiguration,
    -- | Provides the configuration information to connect to an Amazon S3 bucket
    -- as your data source.
    s3Configuration :: Prelude.Maybe S3DataSourceConfiguration,
    -- | Provides the configuration information to connect to GitHub as your data
    -- source.
    gitHubConfiguration :: Prelude.Maybe GitHubConfiguration,
    -- | Provides the configuration information to connect to Amazon WorkDocs as
    -- your data source.
    workDocsConfiguration :: Prelude.Maybe WorkDocsConfiguration,
    -- | Provides the configuration information to connect to Quip as your data
    -- source.
    quipConfiguration :: Prelude.Maybe QuipConfiguration,
    -- | Provides the configuration information to connect to Jira as your data
    -- source.
    jiraConfiguration :: Prelude.Maybe JiraConfiguration,
    -- | Provides the configuration information to connect to Confluence as your
    -- data source.
    confluenceConfiguration :: Prelude.Maybe ConfluenceConfiguration,
    -- | Provides the configuration information to connect to Box as your data
    -- source.
    boxConfiguration :: Prelude.Maybe BoxConfiguration,
    -- | Provides the configuration information to connect to Microsoft OneDrive
    -- as your data source.
    oneDriveConfiguration :: Prelude.Maybe OneDriveConfiguration,
    -- | Provides the configuration information to connect to Microsoft
    -- SharePoint as your data source.
    sharePointConfiguration :: Prelude.Maybe SharePointConfiguration,
    -- | Provides the configuration information to connect to Amazon FSx as your
    -- data source.
    fsxConfiguration :: Prelude.Maybe FsxConfiguration,
    -- | Provides the configuration information to connect to Salesforce as your
    -- data source.
    salesforceConfiguration :: Prelude.Maybe SalesforceConfiguration,
    -- | Provides the configuration information to connect to a database as your
    -- data source.
    databaseConfiguration :: Prelude.Maybe DatabaseConfiguration,
    -- | Provides the configuration information to connect to ServiceNow as your
    -- data source.
    serviceNowConfiguration :: Prelude.Maybe ServiceNowConfiguration,
    -- | Provides a template for the configuration information to connect to your
    -- data source.
    templateConfiguration :: Prelude.Maybe TemplateConfiguration,
    -- | Provides the configuration information to connect to Slack as your data
    -- source.
    slackConfiguration :: Prelude.Maybe SlackConfiguration,
    webCrawlerConfiguration :: Prelude.Maybe WebCrawlerConfiguration,
    -- | Provides the configuration information to connect to Alfresco as your
    -- data source.
    alfrescoConfiguration :: Prelude.Maybe AlfrescoConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'googleDriveConfiguration', 'dataSourceConfiguration_googleDriveConfiguration' - Provides the configuration information to connect to Google Drive as
-- your data source.
--
-- 's3Configuration', 'dataSourceConfiguration_s3Configuration' - Provides the configuration information to connect to an Amazon S3 bucket
-- as your data source.
--
-- 'gitHubConfiguration', 'dataSourceConfiguration_gitHubConfiguration' - Provides the configuration information to connect to GitHub as your data
-- source.
--
-- 'workDocsConfiguration', 'dataSourceConfiguration_workDocsConfiguration' - Provides the configuration information to connect to Amazon WorkDocs as
-- your data source.
--
-- 'quipConfiguration', 'dataSourceConfiguration_quipConfiguration' - Provides the configuration information to connect to Quip as your data
-- source.
--
-- 'jiraConfiguration', 'dataSourceConfiguration_jiraConfiguration' - Provides the configuration information to connect to Jira as your data
-- source.
--
-- 'confluenceConfiguration', 'dataSourceConfiguration_confluenceConfiguration' - Provides the configuration information to connect to Confluence as your
-- data source.
--
-- 'boxConfiguration', 'dataSourceConfiguration_boxConfiguration' - Provides the configuration information to connect to Box as your data
-- source.
--
-- 'oneDriveConfiguration', 'dataSourceConfiguration_oneDriveConfiguration' - Provides the configuration information to connect to Microsoft OneDrive
-- as your data source.
--
-- 'sharePointConfiguration', 'dataSourceConfiguration_sharePointConfiguration' - Provides the configuration information to connect to Microsoft
-- SharePoint as your data source.
--
-- 'fsxConfiguration', 'dataSourceConfiguration_fsxConfiguration' - Provides the configuration information to connect to Amazon FSx as your
-- data source.
--
-- 'salesforceConfiguration', 'dataSourceConfiguration_salesforceConfiguration' - Provides the configuration information to connect to Salesforce as your
-- data source.
--
-- 'databaseConfiguration', 'dataSourceConfiguration_databaseConfiguration' - Provides the configuration information to connect to a database as your
-- data source.
--
-- 'serviceNowConfiguration', 'dataSourceConfiguration_serviceNowConfiguration' - Provides the configuration information to connect to ServiceNow as your
-- data source.
--
-- 'templateConfiguration', 'dataSourceConfiguration_templateConfiguration' - Provides a template for the configuration information to connect to your
-- data source.
--
-- 'slackConfiguration', 'dataSourceConfiguration_slackConfiguration' - Provides the configuration information to connect to Slack as your data
-- source.
--
-- 'webCrawlerConfiguration', 'dataSourceConfiguration_webCrawlerConfiguration' - Undocumented member.
--
-- 'alfrescoConfiguration', 'dataSourceConfiguration_alfrescoConfiguration' - Provides the configuration information to connect to Alfresco as your
-- data source.
newDataSourceConfiguration ::
  DataSourceConfiguration
newDataSourceConfiguration =
  DataSourceConfiguration'
    { googleDriveConfiguration =
        Prelude.Nothing,
      s3Configuration = Prelude.Nothing,
      gitHubConfiguration = Prelude.Nothing,
      workDocsConfiguration = Prelude.Nothing,
      quipConfiguration = Prelude.Nothing,
      jiraConfiguration = Prelude.Nothing,
      confluenceConfiguration = Prelude.Nothing,
      boxConfiguration = Prelude.Nothing,
      oneDriveConfiguration = Prelude.Nothing,
      sharePointConfiguration = Prelude.Nothing,
      fsxConfiguration = Prelude.Nothing,
      salesforceConfiguration = Prelude.Nothing,
      databaseConfiguration = Prelude.Nothing,
      serviceNowConfiguration = Prelude.Nothing,
      templateConfiguration = Prelude.Nothing,
      slackConfiguration = Prelude.Nothing,
      webCrawlerConfiguration = Prelude.Nothing,
      alfrescoConfiguration = Prelude.Nothing
    }

-- | Provides the configuration information to connect to Google Drive as
-- your data source.
dataSourceConfiguration_googleDriveConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe GoogleDriveConfiguration)
dataSourceConfiguration_googleDriveConfiguration = Lens.lens (\DataSourceConfiguration' {googleDriveConfiguration} -> googleDriveConfiguration) (\s@DataSourceConfiguration' {} a -> s {googleDriveConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to an Amazon S3 bucket
-- as your data source.
dataSourceConfiguration_s3Configuration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe S3DataSourceConfiguration)
dataSourceConfiguration_s3Configuration = Lens.lens (\DataSourceConfiguration' {s3Configuration} -> s3Configuration) (\s@DataSourceConfiguration' {} a -> s {s3Configuration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to GitHub as your data
-- source.
dataSourceConfiguration_gitHubConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe GitHubConfiguration)
dataSourceConfiguration_gitHubConfiguration = Lens.lens (\DataSourceConfiguration' {gitHubConfiguration} -> gitHubConfiguration) (\s@DataSourceConfiguration' {} a -> s {gitHubConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Amazon WorkDocs as
-- your data source.
dataSourceConfiguration_workDocsConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe WorkDocsConfiguration)
dataSourceConfiguration_workDocsConfiguration = Lens.lens (\DataSourceConfiguration' {workDocsConfiguration} -> workDocsConfiguration) (\s@DataSourceConfiguration' {} a -> s {workDocsConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Quip as your data
-- source.
dataSourceConfiguration_quipConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe QuipConfiguration)
dataSourceConfiguration_quipConfiguration = Lens.lens (\DataSourceConfiguration' {quipConfiguration} -> quipConfiguration) (\s@DataSourceConfiguration' {} a -> s {quipConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Jira as your data
-- source.
dataSourceConfiguration_jiraConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe JiraConfiguration)
dataSourceConfiguration_jiraConfiguration = Lens.lens (\DataSourceConfiguration' {jiraConfiguration} -> jiraConfiguration) (\s@DataSourceConfiguration' {} a -> s {jiraConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Confluence as your
-- data source.
dataSourceConfiguration_confluenceConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe ConfluenceConfiguration)
dataSourceConfiguration_confluenceConfiguration = Lens.lens (\DataSourceConfiguration' {confluenceConfiguration} -> confluenceConfiguration) (\s@DataSourceConfiguration' {} a -> s {confluenceConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Box as your data
-- source.
dataSourceConfiguration_boxConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe BoxConfiguration)
dataSourceConfiguration_boxConfiguration = Lens.lens (\DataSourceConfiguration' {boxConfiguration} -> boxConfiguration) (\s@DataSourceConfiguration' {} a -> s {boxConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Microsoft OneDrive
-- as your data source.
dataSourceConfiguration_oneDriveConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe OneDriveConfiguration)
dataSourceConfiguration_oneDriveConfiguration = Lens.lens (\DataSourceConfiguration' {oneDriveConfiguration} -> oneDriveConfiguration) (\s@DataSourceConfiguration' {} a -> s {oneDriveConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Microsoft
-- SharePoint as your data source.
dataSourceConfiguration_sharePointConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe SharePointConfiguration)
dataSourceConfiguration_sharePointConfiguration = Lens.lens (\DataSourceConfiguration' {sharePointConfiguration} -> sharePointConfiguration) (\s@DataSourceConfiguration' {} a -> s {sharePointConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Amazon FSx as your
-- data source.
dataSourceConfiguration_fsxConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe FsxConfiguration)
dataSourceConfiguration_fsxConfiguration = Lens.lens (\DataSourceConfiguration' {fsxConfiguration} -> fsxConfiguration) (\s@DataSourceConfiguration' {} a -> s {fsxConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Salesforce as your
-- data source.
dataSourceConfiguration_salesforceConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe SalesforceConfiguration)
dataSourceConfiguration_salesforceConfiguration = Lens.lens (\DataSourceConfiguration' {salesforceConfiguration} -> salesforceConfiguration) (\s@DataSourceConfiguration' {} a -> s {salesforceConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to a database as your
-- data source.
dataSourceConfiguration_databaseConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe DatabaseConfiguration)
dataSourceConfiguration_databaseConfiguration = Lens.lens (\DataSourceConfiguration' {databaseConfiguration} -> databaseConfiguration) (\s@DataSourceConfiguration' {} a -> s {databaseConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to ServiceNow as your
-- data source.
dataSourceConfiguration_serviceNowConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe ServiceNowConfiguration)
dataSourceConfiguration_serviceNowConfiguration = Lens.lens (\DataSourceConfiguration' {serviceNowConfiguration} -> serviceNowConfiguration) (\s@DataSourceConfiguration' {} a -> s {serviceNowConfiguration = a} :: DataSourceConfiguration)

-- | Provides a template for the configuration information to connect to your
-- data source.
dataSourceConfiguration_templateConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe TemplateConfiguration)
dataSourceConfiguration_templateConfiguration = Lens.lens (\DataSourceConfiguration' {templateConfiguration} -> templateConfiguration) (\s@DataSourceConfiguration' {} a -> s {templateConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Slack as your data
-- source.
dataSourceConfiguration_slackConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe SlackConfiguration)
dataSourceConfiguration_slackConfiguration = Lens.lens (\DataSourceConfiguration' {slackConfiguration} -> slackConfiguration) (\s@DataSourceConfiguration' {} a -> s {slackConfiguration = a} :: DataSourceConfiguration)

-- | Undocumented member.
dataSourceConfiguration_webCrawlerConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe WebCrawlerConfiguration)
dataSourceConfiguration_webCrawlerConfiguration = Lens.lens (\DataSourceConfiguration' {webCrawlerConfiguration} -> webCrawlerConfiguration) (\s@DataSourceConfiguration' {} a -> s {webCrawlerConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Alfresco as your
-- data source.
dataSourceConfiguration_alfrescoConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe AlfrescoConfiguration)
dataSourceConfiguration_alfrescoConfiguration = Lens.lens (\DataSourceConfiguration' {alfrescoConfiguration} -> alfrescoConfiguration) (\s@DataSourceConfiguration' {} a -> s {alfrescoConfiguration = a} :: DataSourceConfiguration)

instance Core.FromJSON DataSourceConfiguration where
  parseJSON =
    Core.withObject
      "DataSourceConfiguration"
      ( \x ->
          DataSourceConfiguration'
            Prelude.<$> (x Core..:? "GoogleDriveConfiguration")
            Prelude.<*> (x Core..:? "S3Configuration")
            Prelude.<*> (x Core..:? "GitHubConfiguration")
            Prelude.<*> (x Core..:? "WorkDocsConfiguration")
            Prelude.<*> (x Core..:? "QuipConfiguration")
            Prelude.<*> (x Core..:? "JiraConfiguration")
            Prelude.<*> (x Core..:? "ConfluenceConfiguration")
            Prelude.<*> (x Core..:? "BoxConfiguration")
            Prelude.<*> (x Core..:? "OneDriveConfiguration")
            Prelude.<*> (x Core..:? "SharePointConfiguration")
            Prelude.<*> (x Core..:? "FsxConfiguration")
            Prelude.<*> (x Core..:? "SalesforceConfiguration")
            Prelude.<*> (x Core..:? "DatabaseConfiguration")
            Prelude.<*> (x Core..:? "ServiceNowConfiguration")
            Prelude.<*> (x Core..:? "TemplateConfiguration")
            Prelude.<*> (x Core..:? "SlackConfiguration")
            Prelude.<*> (x Core..:? "WebCrawlerConfiguration")
            Prelude.<*> (x Core..:? "AlfrescoConfiguration")
      )

instance Prelude.Hashable DataSourceConfiguration where
  hashWithSalt _salt DataSourceConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` googleDriveConfiguration
      `Prelude.hashWithSalt` s3Configuration
      `Prelude.hashWithSalt` gitHubConfiguration
      `Prelude.hashWithSalt` workDocsConfiguration
      `Prelude.hashWithSalt` quipConfiguration
      `Prelude.hashWithSalt` jiraConfiguration
      `Prelude.hashWithSalt` confluenceConfiguration
      `Prelude.hashWithSalt` boxConfiguration
      `Prelude.hashWithSalt` oneDriveConfiguration
      `Prelude.hashWithSalt` sharePointConfiguration
      `Prelude.hashWithSalt` fsxConfiguration
      `Prelude.hashWithSalt` salesforceConfiguration
      `Prelude.hashWithSalt` databaseConfiguration
      `Prelude.hashWithSalt` serviceNowConfiguration
      `Prelude.hashWithSalt` templateConfiguration
      `Prelude.hashWithSalt` slackConfiguration
      `Prelude.hashWithSalt` webCrawlerConfiguration
      `Prelude.hashWithSalt` alfrescoConfiguration

instance Prelude.NFData DataSourceConfiguration where
  rnf DataSourceConfiguration' {..} =
    Prelude.rnf googleDriveConfiguration
      `Prelude.seq` Prelude.rnf s3Configuration
      `Prelude.seq` Prelude.rnf gitHubConfiguration
      `Prelude.seq` Prelude.rnf workDocsConfiguration
      `Prelude.seq` Prelude.rnf quipConfiguration
      `Prelude.seq` Prelude.rnf jiraConfiguration
      `Prelude.seq` Prelude.rnf confluenceConfiguration
      `Prelude.seq` Prelude.rnf boxConfiguration
      `Prelude.seq` Prelude.rnf oneDriveConfiguration
      `Prelude.seq` Prelude.rnf sharePointConfiguration
      `Prelude.seq` Prelude.rnf fsxConfiguration
      `Prelude.seq` Prelude.rnf salesforceConfiguration
      `Prelude.seq` Prelude.rnf databaseConfiguration
      `Prelude.seq` Prelude.rnf serviceNowConfiguration
      `Prelude.seq` Prelude.rnf templateConfiguration
      `Prelude.seq` Prelude.rnf slackConfiguration
      `Prelude.seq` Prelude.rnf webCrawlerConfiguration
      `Prelude.seq` Prelude.rnf alfrescoConfiguration

instance Core.ToJSON DataSourceConfiguration where
  toJSON DataSourceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("GoogleDriveConfiguration" Core..=)
              Prelude.<$> googleDriveConfiguration,
            ("S3Configuration" Core..=)
              Prelude.<$> s3Configuration,
            ("GitHubConfiguration" Core..=)
              Prelude.<$> gitHubConfiguration,
            ("WorkDocsConfiguration" Core..=)
              Prelude.<$> workDocsConfiguration,
            ("QuipConfiguration" Core..=)
              Prelude.<$> quipConfiguration,
            ("JiraConfiguration" Core..=)
              Prelude.<$> jiraConfiguration,
            ("ConfluenceConfiguration" Core..=)
              Prelude.<$> confluenceConfiguration,
            ("BoxConfiguration" Core..=)
              Prelude.<$> boxConfiguration,
            ("OneDriveConfiguration" Core..=)
              Prelude.<$> oneDriveConfiguration,
            ("SharePointConfiguration" Core..=)
              Prelude.<$> sharePointConfiguration,
            ("FsxConfiguration" Core..=)
              Prelude.<$> fsxConfiguration,
            ("SalesforceConfiguration" Core..=)
              Prelude.<$> salesforceConfiguration,
            ("DatabaseConfiguration" Core..=)
              Prelude.<$> databaseConfiguration,
            ("ServiceNowConfiguration" Core..=)
              Prelude.<$> serviceNowConfiguration,
            ("TemplateConfiguration" Core..=)
              Prelude.<$> templateConfiguration,
            ("SlackConfiguration" Core..=)
              Prelude.<$> slackConfiguration,
            ("WebCrawlerConfiguration" Core..=)
              Prelude.<$> webCrawlerConfiguration,
            ("AlfrescoConfiguration" Core..=)
              Prelude.<$> alfrescoConfiguration
          ]
      )
