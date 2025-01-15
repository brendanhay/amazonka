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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DataSourceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | Provides the configuration information to connect to Alfresco as your
    -- data source.
    alfrescoConfiguration :: Prelude.Maybe AlfrescoConfiguration,
    -- | Provides the configuration information to connect to Box as your data
    -- source.
    boxConfiguration :: Prelude.Maybe BoxConfiguration,
    -- | Provides the configuration information to connect to Confluence as your
    -- data source.
    confluenceConfiguration :: Prelude.Maybe ConfluenceConfiguration,
    -- | Provides the configuration information to connect to a database as your
    -- data source.
    databaseConfiguration :: Prelude.Maybe DatabaseConfiguration,
    -- | Provides the configuration information to connect to Amazon FSx as your
    -- data source.
    fsxConfiguration :: Prelude.Maybe FsxConfiguration,
    -- | Provides the configuration information to connect to GitHub as your data
    -- source.
    gitHubConfiguration :: Prelude.Maybe GitHubConfiguration,
    -- | Provides the configuration information to connect to Google Drive as
    -- your data source.
    googleDriveConfiguration :: Prelude.Maybe GoogleDriveConfiguration,
    -- | Provides the configuration information to connect to Jira as your data
    -- source.
    jiraConfiguration :: Prelude.Maybe JiraConfiguration,
    -- | Provides the configuration information to connect to Microsoft OneDrive
    -- as your data source.
    oneDriveConfiguration :: Prelude.Maybe OneDriveConfiguration,
    -- | Provides the configuration information to connect to Quip as your data
    -- source.
    quipConfiguration :: Prelude.Maybe QuipConfiguration,
    -- | Provides the configuration information to connect to an Amazon S3 bucket
    -- as your data source.
    s3Configuration :: Prelude.Maybe S3DataSourceConfiguration,
    -- | Provides the configuration information to connect to Salesforce as your
    -- data source.
    salesforceConfiguration :: Prelude.Maybe SalesforceConfiguration,
    -- | Provides the configuration information to connect to ServiceNow as your
    -- data source.
    serviceNowConfiguration :: Prelude.Maybe ServiceNowConfiguration,
    -- | Provides the configuration information to connect to Microsoft
    -- SharePoint as your data source.
    sharePointConfiguration :: Prelude.Maybe SharePointConfiguration,
    -- | Provides the configuration information to connect to Slack as your data
    -- source.
    slackConfiguration :: Prelude.Maybe SlackConfiguration,
    -- | Provides a template for the configuration information to connect to your
    -- data source.
    templateConfiguration :: Prelude.Maybe TemplateConfiguration,
    webCrawlerConfiguration :: Prelude.Maybe WebCrawlerConfiguration,
    -- | Provides the configuration information to connect to Amazon WorkDocs as
    -- your data source.
    workDocsConfiguration :: Prelude.Maybe WorkDocsConfiguration
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
-- 'alfrescoConfiguration', 'dataSourceConfiguration_alfrescoConfiguration' - Provides the configuration information to connect to Alfresco as your
-- data source.
--
-- 'boxConfiguration', 'dataSourceConfiguration_boxConfiguration' - Provides the configuration information to connect to Box as your data
-- source.
--
-- 'confluenceConfiguration', 'dataSourceConfiguration_confluenceConfiguration' - Provides the configuration information to connect to Confluence as your
-- data source.
--
-- 'databaseConfiguration', 'dataSourceConfiguration_databaseConfiguration' - Provides the configuration information to connect to a database as your
-- data source.
--
-- 'fsxConfiguration', 'dataSourceConfiguration_fsxConfiguration' - Provides the configuration information to connect to Amazon FSx as your
-- data source.
--
-- 'gitHubConfiguration', 'dataSourceConfiguration_gitHubConfiguration' - Provides the configuration information to connect to GitHub as your data
-- source.
--
-- 'googleDriveConfiguration', 'dataSourceConfiguration_googleDriveConfiguration' - Provides the configuration information to connect to Google Drive as
-- your data source.
--
-- 'jiraConfiguration', 'dataSourceConfiguration_jiraConfiguration' - Provides the configuration information to connect to Jira as your data
-- source.
--
-- 'oneDriveConfiguration', 'dataSourceConfiguration_oneDriveConfiguration' - Provides the configuration information to connect to Microsoft OneDrive
-- as your data source.
--
-- 'quipConfiguration', 'dataSourceConfiguration_quipConfiguration' - Provides the configuration information to connect to Quip as your data
-- source.
--
-- 's3Configuration', 'dataSourceConfiguration_s3Configuration' - Provides the configuration information to connect to an Amazon S3 bucket
-- as your data source.
--
-- 'salesforceConfiguration', 'dataSourceConfiguration_salesforceConfiguration' - Provides the configuration information to connect to Salesforce as your
-- data source.
--
-- 'serviceNowConfiguration', 'dataSourceConfiguration_serviceNowConfiguration' - Provides the configuration information to connect to ServiceNow as your
-- data source.
--
-- 'sharePointConfiguration', 'dataSourceConfiguration_sharePointConfiguration' - Provides the configuration information to connect to Microsoft
-- SharePoint as your data source.
--
-- 'slackConfiguration', 'dataSourceConfiguration_slackConfiguration' - Provides the configuration information to connect to Slack as your data
-- source.
--
-- 'templateConfiguration', 'dataSourceConfiguration_templateConfiguration' - Provides a template for the configuration information to connect to your
-- data source.
--
-- 'webCrawlerConfiguration', 'dataSourceConfiguration_webCrawlerConfiguration' - Undocumented member.
--
-- 'workDocsConfiguration', 'dataSourceConfiguration_workDocsConfiguration' - Provides the configuration information to connect to Amazon WorkDocs as
-- your data source.
newDataSourceConfiguration ::
  DataSourceConfiguration
newDataSourceConfiguration =
  DataSourceConfiguration'
    { alfrescoConfiguration =
        Prelude.Nothing,
      boxConfiguration = Prelude.Nothing,
      confluenceConfiguration = Prelude.Nothing,
      databaseConfiguration = Prelude.Nothing,
      fsxConfiguration = Prelude.Nothing,
      gitHubConfiguration = Prelude.Nothing,
      googleDriveConfiguration = Prelude.Nothing,
      jiraConfiguration = Prelude.Nothing,
      oneDriveConfiguration = Prelude.Nothing,
      quipConfiguration = Prelude.Nothing,
      s3Configuration = Prelude.Nothing,
      salesforceConfiguration = Prelude.Nothing,
      serviceNowConfiguration = Prelude.Nothing,
      sharePointConfiguration = Prelude.Nothing,
      slackConfiguration = Prelude.Nothing,
      templateConfiguration = Prelude.Nothing,
      webCrawlerConfiguration = Prelude.Nothing,
      workDocsConfiguration = Prelude.Nothing
    }

-- | Provides the configuration information to connect to Alfresco as your
-- data source.
dataSourceConfiguration_alfrescoConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe AlfrescoConfiguration)
dataSourceConfiguration_alfrescoConfiguration = Lens.lens (\DataSourceConfiguration' {alfrescoConfiguration} -> alfrescoConfiguration) (\s@DataSourceConfiguration' {} a -> s {alfrescoConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Box as your data
-- source.
dataSourceConfiguration_boxConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe BoxConfiguration)
dataSourceConfiguration_boxConfiguration = Lens.lens (\DataSourceConfiguration' {boxConfiguration} -> boxConfiguration) (\s@DataSourceConfiguration' {} a -> s {boxConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Confluence as your
-- data source.
dataSourceConfiguration_confluenceConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe ConfluenceConfiguration)
dataSourceConfiguration_confluenceConfiguration = Lens.lens (\DataSourceConfiguration' {confluenceConfiguration} -> confluenceConfiguration) (\s@DataSourceConfiguration' {} a -> s {confluenceConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to a database as your
-- data source.
dataSourceConfiguration_databaseConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe DatabaseConfiguration)
dataSourceConfiguration_databaseConfiguration = Lens.lens (\DataSourceConfiguration' {databaseConfiguration} -> databaseConfiguration) (\s@DataSourceConfiguration' {} a -> s {databaseConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Amazon FSx as your
-- data source.
dataSourceConfiguration_fsxConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe FsxConfiguration)
dataSourceConfiguration_fsxConfiguration = Lens.lens (\DataSourceConfiguration' {fsxConfiguration} -> fsxConfiguration) (\s@DataSourceConfiguration' {} a -> s {fsxConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to GitHub as your data
-- source.
dataSourceConfiguration_gitHubConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe GitHubConfiguration)
dataSourceConfiguration_gitHubConfiguration = Lens.lens (\DataSourceConfiguration' {gitHubConfiguration} -> gitHubConfiguration) (\s@DataSourceConfiguration' {} a -> s {gitHubConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Google Drive as
-- your data source.
dataSourceConfiguration_googleDriveConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe GoogleDriveConfiguration)
dataSourceConfiguration_googleDriveConfiguration = Lens.lens (\DataSourceConfiguration' {googleDriveConfiguration} -> googleDriveConfiguration) (\s@DataSourceConfiguration' {} a -> s {googleDriveConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Jira as your data
-- source.
dataSourceConfiguration_jiraConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe JiraConfiguration)
dataSourceConfiguration_jiraConfiguration = Lens.lens (\DataSourceConfiguration' {jiraConfiguration} -> jiraConfiguration) (\s@DataSourceConfiguration' {} a -> s {jiraConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Microsoft OneDrive
-- as your data source.
dataSourceConfiguration_oneDriveConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe OneDriveConfiguration)
dataSourceConfiguration_oneDriveConfiguration = Lens.lens (\DataSourceConfiguration' {oneDriveConfiguration} -> oneDriveConfiguration) (\s@DataSourceConfiguration' {} a -> s {oneDriveConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Quip as your data
-- source.
dataSourceConfiguration_quipConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe QuipConfiguration)
dataSourceConfiguration_quipConfiguration = Lens.lens (\DataSourceConfiguration' {quipConfiguration} -> quipConfiguration) (\s@DataSourceConfiguration' {} a -> s {quipConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to an Amazon S3 bucket
-- as your data source.
dataSourceConfiguration_s3Configuration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe S3DataSourceConfiguration)
dataSourceConfiguration_s3Configuration = Lens.lens (\DataSourceConfiguration' {s3Configuration} -> s3Configuration) (\s@DataSourceConfiguration' {} a -> s {s3Configuration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Salesforce as your
-- data source.
dataSourceConfiguration_salesforceConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe SalesforceConfiguration)
dataSourceConfiguration_salesforceConfiguration = Lens.lens (\DataSourceConfiguration' {salesforceConfiguration} -> salesforceConfiguration) (\s@DataSourceConfiguration' {} a -> s {salesforceConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to ServiceNow as your
-- data source.
dataSourceConfiguration_serviceNowConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe ServiceNowConfiguration)
dataSourceConfiguration_serviceNowConfiguration = Lens.lens (\DataSourceConfiguration' {serviceNowConfiguration} -> serviceNowConfiguration) (\s@DataSourceConfiguration' {} a -> s {serviceNowConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Microsoft
-- SharePoint as your data source.
dataSourceConfiguration_sharePointConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe SharePointConfiguration)
dataSourceConfiguration_sharePointConfiguration = Lens.lens (\DataSourceConfiguration' {sharePointConfiguration} -> sharePointConfiguration) (\s@DataSourceConfiguration' {} a -> s {sharePointConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Slack as your data
-- source.
dataSourceConfiguration_slackConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe SlackConfiguration)
dataSourceConfiguration_slackConfiguration = Lens.lens (\DataSourceConfiguration' {slackConfiguration} -> slackConfiguration) (\s@DataSourceConfiguration' {} a -> s {slackConfiguration = a} :: DataSourceConfiguration)

-- | Provides a template for the configuration information to connect to your
-- data source.
dataSourceConfiguration_templateConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe TemplateConfiguration)
dataSourceConfiguration_templateConfiguration = Lens.lens (\DataSourceConfiguration' {templateConfiguration} -> templateConfiguration) (\s@DataSourceConfiguration' {} a -> s {templateConfiguration = a} :: DataSourceConfiguration)

-- | Undocumented member.
dataSourceConfiguration_webCrawlerConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe WebCrawlerConfiguration)
dataSourceConfiguration_webCrawlerConfiguration = Lens.lens (\DataSourceConfiguration' {webCrawlerConfiguration} -> webCrawlerConfiguration) (\s@DataSourceConfiguration' {} a -> s {webCrawlerConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to Amazon WorkDocs as
-- your data source.
dataSourceConfiguration_workDocsConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe WorkDocsConfiguration)
dataSourceConfiguration_workDocsConfiguration = Lens.lens (\DataSourceConfiguration' {workDocsConfiguration} -> workDocsConfiguration) (\s@DataSourceConfiguration' {} a -> s {workDocsConfiguration = a} :: DataSourceConfiguration)

instance Data.FromJSON DataSourceConfiguration where
  parseJSON =
    Data.withObject
      "DataSourceConfiguration"
      ( \x ->
          DataSourceConfiguration'
            Prelude.<$> (x Data..:? "AlfrescoConfiguration")
            Prelude.<*> (x Data..:? "BoxConfiguration")
            Prelude.<*> (x Data..:? "ConfluenceConfiguration")
            Prelude.<*> (x Data..:? "DatabaseConfiguration")
            Prelude.<*> (x Data..:? "FsxConfiguration")
            Prelude.<*> (x Data..:? "GitHubConfiguration")
            Prelude.<*> (x Data..:? "GoogleDriveConfiguration")
            Prelude.<*> (x Data..:? "JiraConfiguration")
            Prelude.<*> (x Data..:? "OneDriveConfiguration")
            Prelude.<*> (x Data..:? "QuipConfiguration")
            Prelude.<*> (x Data..:? "S3Configuration")
            Prelude.<*> (x Data..:? "SalesforceConfiguration")
            Prelude.<*> (x Data..:? "ServiceNowConfiguration")
            Prelude.<*> (x Data..:? "SharePointConfiguration")
            Prelude.<*> (x Data..:? "SlackConfiguration")
            Prelude.<*> (x Data..:? "TemplateConfiguration")
            Prelude.<*> (x Data..:? "WebCrawlerConfiguration")
            Prelude.<*> (x Data..:? "WorkDocsConfiguration")
      )

instance Prelude.Hashable DataSourceConfiguration where
  hashWithSalt _salt DataSourceConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` alfrescoConfiguration
      `Prelude.hashWithSalt` boxConfiguration
      `Prelude.hashWithSalt` confluenceConfiguration
      `Prelude.hashWithSalt` databaseConfiguration
      `Prelude.hashWithSalt` fsxConfiguration
      `Prelude.hashWithSalt` gitHubConfiguration
      `Prelude.hashWithSalt` googleDriveConfiguration
      `Prelude.hashWithSalt` jiraConfiguration
      `Prelude.hashWithSalt` oneDriveConfiguration
      `Prelude.hashWithSalt` quipConfiguration
      `Prelude.hashWithSalt` s3Configuration
      `Prelude.hashWithSalt` salesforceConfiguration
      `Prelude.hashWithSalt` serviceNowConfiguration
      `Prelude.hashWithSalt` sharePointConfiguration
      `Prelude.hashWithSalt` slackConfiguration
      `Prelude.hashWithSalt` templateConfiguration
      `Prelude.hashWithSalt` webCrawlerConfiguration
      `Prelude.hashWithSalt` workDocsConfiguration

instance Prelude.NFData DataSourceConfiguration where
  rnf DataSourceConfiguration' {..} =
    Prelude.rnf alfrescoConfiguration `Prelude.seq`
      Prelude.rnf boxConfiguration `Prelude.seq`
        Prelude.rnf confluenceConfiguration `Prelude.seq`
          Prelude.rnf databaseConfiguration `Prelude.seq`
            Prelude.rnf fsxConfiguration `Prelude.seq`
              Prelude.rnf gitHubConfiguration `Prelude.seq`
                Prelude.rnf googleDriveConfiguration `Prelude.seq`
                  Prelude.rnf jiraConfiguration `Prelude.seq`
                    Prelude.rnf oneDriveConfiguration `Prelude.seq`
                      Prelude.rnf quipConfiguration `Prelude.seq`
                        Prelude.rnf s3Configuration `Prelude.seq`
                          Prelude.rnf salesforceConfiguration `Prelude.seq`
                            Prelude.rnf serviceNowConfiguration `Prelude.seq`
                              Prelude.rnf sharePointConfiguration `Prelude.seq`
                                Prelude.rnf slackConfiguration `Prelude.seq`
                                  Prelude.rnf templateConfiguration `Prelude.seq`
                                    Prelude.rnf webCrawlerConfiguration `Prelude.seq`
                                      Prelude.rnf workDocsConfiguration

instance Data.ToJSON DataSourceConfiguration where
  toJSON DataSourceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AlfrescoConfiguration" Data..=)
              Prelude.<$> alfrescoConfiguration,
            ("BoxConfiguration" Data..=)
              Prelude.<$> boxConfiguration,
            ("ConfluenceConfiguration" Data..=)
              Prelude.<$> confluenceConfiguration,
            ("DatabaseConfiguration" Data..=)
              Prelude.<$> databaseConfiguration,
            ("FsxConfiguration" Data..=)
              Prelude.<$> fsxConfiguration,
            ("GitHubConfiguration" Data..=)
              Prelude.<$> gitHubConfiguration,
            ("GoogleDriveConfiguration" Data..=)
              Prelude.<$> googleDriveConfiguration,
            ("JiraConfiguration" Data..=)
              Prelude.<$> jiraConfiguration,
            ("OneDriveConfiguration" Data..=)
              Prelude.<$> oneDriveConfiguration,
            ("QuipConfiguration" Data..=)
              Prelude.<$> quipConfiguration,
            ("S3Configuration" Data..=)
              Prelude.<$> s3Configuration,
            ("SalesforceConfiguration" Data..=)
              Prelude.<$> salesforceConfiguration,
            ("ServiceNowConfiguration" Data..=)
              Prelude.<$> serviceNowConfiguration,
            ("SharePointConfiguration" Data..=)
              Prelude.<$> sharePointConfiguration,
            ("SlackConfiguration" Data..=)
              Prelude.<$> slackConfiguration,
            ("TemplateConfiguration" Data..=)
              Prelude.<$> templateConfiguration,
            ("WebCrawlerConfiguration" Data..=)
              Prelude.<$> webCrawlerConfiguration,
            ("WorkDocsConfiguration" Data..=)
              Prelude.<$> workDocsConfiguration
          ]
      )
