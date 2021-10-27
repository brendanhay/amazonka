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
-- Module      : Network.AWS.Kendra.Types.DataSourceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.DataSourceConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.ConfluenceConfiguration
import Network.AWS.Kendra.Types.DatabaseConfiguration
import Network.AWS.Kendra.Types.GoogleDriveConfiguration
import Network.AWS.Kendra.Types.OneDriveConfiguration
import Network.AWS.Kendra.Types.S3DataSourceConfiguration
import Network.AWS.Kendra.Types.SalesforceConfiguration
import Network.AWS.Kendra.Types.ServiceNowConfiguration
import Network.AWS.Kendra.Types.SharePointConfiguration
import Network.AWS.Kendra.Types.WebCrawlerConfiguration
import Network.AWS.Kendra.Types.WorkDocsConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration information for a Amazon Kendra data source.
--
-- /See:/ 'newDataSourceConfiguration' smart constructor.
data DataSourceConfiguration = DataSourceConfiguration'
  { webCrawlerConfiguration :: Prelude.Maybe WebCrawlerConfiguration,
    -- | Provides information necessary to create a data source connector for a
    -- database.
    databaseConfiguration :: Prelude.Maybe DatabaseConfiguration,
    -- | Provides configuration for data sources that connect to Google Drive.
    googleDriveConfiguration :: Prelude.Maybe GoogleDriveConfiguration,
    -- | Provides configuration for data sources that connect to Microsoft
    -- OneDrive.
    oneDriveConfiguration :: Prelude.Maybe OneDriveConfiguration,
    -- | Provides configuration information for connecting to a Confluence data
    -- source.
    confluenceConfiguration :: Prelude.Maybe ConfluenceConfiguration,
    -- | Provides information to create a data source connector for a document
    -- repository in an Amazon S3 bucket.
    s3Configuration :: Prelude.Maybe S3DataSourceConfiguration,
    -- | Provides configuration for data sources that connect to ServiceNow
    -- instances.
    serviceNowConfiguration :: Prelude.Maybe ServiceNowConfiguration,
    -- | Provides information necessary to create a data source connector for a
    -- Microsoft SharePoint site.
    sharePointConfiguration :: Prelude.Maybe SharePointConfiguration,
    -- | Provides the configuration information to connect to WorkDocs as your
    -- data source.
    workDocsConfiguration :: Prelude.Maybe WorkDocsConfiguration,
    -- | Provides configuration information for data sources that connect to a
    -- Salesforce site.
    salesforceConfiguration :: Prelude.Maybe SalesforceConfiguration
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
-- 'webCrawlerConfiguration', 'dataSourceConfiguration_webCrawlerConfiguration' - Undocumented member.
--
-- 'databaseConfiguration', 'dataSourceConfiguration_databaseConfiguration' - Provides information necessary to create a data source connector for a
-- database.
--
-- 'googleDriveConfiguration', 'dataSourceConfiguration_googleDriveConfiguration' - Provides configuration for data sources that connect to Google Drive.
--
-- 'oneDriveConfiguration', 'dataSourceConfiguration_oneDriveConfiguration' - Provides configuration for data sources that connect to Microsoft
-- OneDrive.
--
-- 'confluenceConfiguration', 'dataSourceConfiguration_confluenceConfiguration' - Provides configuration information for connecting to a Confluence data
-- source.
--
-- 's3Configuration', 'dataSourceConfiguration_s3Configuration' - Provides information to create a data source connector for a document
-- repository in an Amazon S3 bucket.
--
-- 'serviceNowConfiguration', 'dataSourceConfiguration_serviceNowConfiguration' - Provides configuration for data sources that connect to ServiceNow
-- instances.
--
-- 'sharePointConfiguration', 'dataSourceConfiguration_sharePointConfiguration' - Provides information necessary to create a data source connector for a
-- Microsoft SharePoint site.
--
-- 'workDocsConfiguration', 'dataSourceConfiguration_workDocsConfiguration' - Provides the configuration information to connect to WorkDocs as your
-- data source.
--
-- 'salesforceConfiguration', 'dataSourceConfiguration_salesforceConfiguration' - Provides configuration information for data sources that connect to a
-- Salesforce site.
newDataSourceConfiguration ::
  DataSourceConfiguration
newDataSourceConfiguration =
  DataSourceConfiguration'
    { webCrawlerConfiguration =
        Prelude.Nothing,
      databaseConfiguration = Prelude.Nothing,
      googleDriveConfiguration = Prelude.Nothing,
      oneDriveConfiguration = Prelude.Nothing,
      confluenceConfiguration = Prelude.Nothing,
      s3Configuration = Prelude.Nothing,
      serviceNowConfiguration = Prelude.Nothing,
      sharePointConfiguration = Prelude.Nothing,
      workDocsConfiguration = Prelude.Nothing,
      salesforceConfiguration = Prelude.Nothing
    }

-- | Undocumented member.
dataSourceConfiguration_webCrawlerConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe WebCrawlerConfiguration)
dataSourceConfiguration_webCrawlerConfiguration = Lens.lens (\DataSourceConfiguration' {webCrawlerConfiguration} -> webCrawlerConfiguration) (\s@DataSourceConfiguration' {} a -> s {webCrawlerConfiguration = a} :: DataSourceConfiguration)

-- | Provides information necessary to create a data source connector for a
-- database.
dataSourceConfiguration_databaseConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe DatabaseConfiguration)
dataSourceConfiguration_databaseConfiguration = Lens.lens (\DataSourceConfiguration' {databaseConfiguration} -> databaseConfiguration) (\s@DataSourceConfiguration' {} a -> s {databaseConfiguration = a} :: DataSourceConfiguration)

-- | Provides configuration for data sources that connect to Google Drive.
dataSourceConfiguration_googleDriveConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe GoogleDriveConfiguration)
dataSourceConfiguration_googleDriveConfiguration = Lens.lens (\DataSourceConfiguration' {googleDriveConfiguration} -> googleDriveConfiguration) (\s@DataSourceConfiguration' {} a -> s {googleDriveConfiguration = a} :: DataSourceConfiguration)

-- | Provides configuration for data sources that connect to Microsoft
-- OneDrive.
dataSourceConfiguration_oneDriveConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe OneDriveConfiguration)
dataSourceConfiguration_oneDriveConfiguration = Lens.lens (\DataSourceConfiguration' {oneDriveConfiguration} -> oneDriveConfiguration) (\s@DataSourceConfiguration' {} a -> s {oneDriveConfiguration = a} :: DataSourceConfiguration)

-- | Provides configuration information for connecting to a Confluence data
-- source.
dataSourceConfiguration_confluenceConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe ConfluenceConfiguration)
dataSourceConfiguration_confluenceConfiguration = Lens.lens (\DataSourceConfiguration' {confluenceConfiguration} -> confluenceConfiguration) (\s@DataSourceConfiguration' {} a -> s {confluenceConfiguration = a} :: DataSourceConfiguration)

-- | Provides information to create a data source connector for a document
-- repository in an Amazon S3 bucket.
dataSourceConfiguration_s3Configuration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe S3DataSourceConfiguration)
dataSourceConfiguration_s3Configuration = Lens.lens (\DataSourceConfiguration' {s3Configuration} -> s3Configuration) (\s@DataSourceConfiguration' {} a -> s {s3Configuration = a} :: DataSourceConfiguration)

-- | Provides configuration for data sources that connect to ServiceNow
-- instances.
dataSourceConfiguration_serviceNowConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe ServiceNowConfiguration)
dataSourceConfiguration_serviceNowConfiguration = Lens.lens (\DataSourceConfiguration' {serviceNowConfiguration} -> serviceNowConfiguration) (\s@DataSourceConfiguration' {} a -> s {serviceNowConfiguration = a} :: DataSourceConfiguration)

-- | Provides information necessary to create a data source connector for a
-- Microsoft SharePoint site.
dataSourceConfiguration_sharePointConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe SharePointConfiguration)
dataSourceConfiguration_sharePointConfiguration = Lens.lens (\DataSourceConfiguration' {sharePointConfiguration} -> sharePointConfiguration) (\s@DataSourceConfiguration' {} a -> s {sharePointConfiguration = a} :: DataSourceConfiguration)

-- | Provides the configuration information to connect to WorkDocs as your
-- data source.
dataSourceConfiguration_workDocsConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe WorkDocsConfiguration)
dataSourceConfiguration_workDocsConfiguration = Lens.lens (\DataSourceConfiguration' {workDocsConfiguration} -> workDocsConfiguration) (\s@DataSourceConfiguration' {} a -> s {workDocsConfiguration = a} :: DataSourceConfiguration)

-- | Provides configuration information for data sources that connect to a
-- Salesforce site.
dataSourceConfiguration_salesforceConfiguration :: Lens.Lens' DataSourceConfiguration (Prelude.Maybe SalesforceConfiguration)
dataSourceConfiguration_salesforceConfiguration = Lens.lens (\DataSourceConfiguration' {salesforceConfiguration} -> salesforceConfiguration) (\s@DataSourceConfiguration' {} a -> s {salesforceConfiguration = a} :: DataSourceConfiguration)

instance Core.FromJSON DataSourceConfiguration where
  parseJSON =
    Core.withObject
      "DataSourceConfiguration"
      ( \x ->
          DataSourceConfiguration'
            Prelude.<$> (x Core..:? "WebCrawlerConfiguration")
            Prelude.<*> (x Core..:? "DatabaseConfiguration")
            Prelude.<*> (x Core..:? "GoogleDriveConfiguration")
            Prelude.<*> (x Core..:? "OneDriveConfiguration")
            Prelude.<*> (x Core..:? "ConfluenceConfiguration")
            Prelude.<*> (x Core..:? "S3Configuration")
            Prelude.<*> (x Core..:? "ServiceNowConfiguration")
            Prelude.<*> (x Core..:? "SharePointConfiguration")
            Prelude.<*> (x Core..:? "WorkDocsConfiguration")
            Prelude.<*> (x Core..:? "SalesforceConfiguration")
      )

instance Prelude.Hashable DataSourceConfiguration

instance Prelude.NFData DataSourceConfiguration

instance Core.ToJSON DataSourceConfiguration where
  toJSON DataSourceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("WebCrawlerConfiguration" Core..=)
              Prelude.<$> webCrawlerConfiguration,
            ("DatabaseConfiguration" Core..=)
              Prelude.<$> databaseConfiguration,
            ("GoogleDriveConfiguration" Core..=)
              Prelude.<$> googleDriveConfiguration,
            ("OneDriveConfiguration" Core..=)
              Prelude.<$> oneDriveConfiguration,
            ("ConfluenceConfiguration" Core..=)
              Prelude.<$> confluenceConfiguration,
            ("S3Configuration" Core..=)
              Prelude.<$> s3Configuration,
            ("ServiceNowConfiguration" Core..=)
              Prelude.<$> serviceNowConfiguration,
            ("SharePointConfiguration" Core..=)
              Prelude.<$> sharePointConfiguration,
            ("WorkDocsConfiguration" Core..=)
              Prelude.<$> workDocsConfiguration,
            ("SalesforceConfiguration" Core..=)
              Prelude.<$> salesforceConfiguration
          ]
      )
