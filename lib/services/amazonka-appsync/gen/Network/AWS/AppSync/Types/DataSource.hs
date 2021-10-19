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
-- Module      : Network.AWS.AppSync.Types.DataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.DataSource where

import Network.AWS.AppSync.Types.DataSourceType
import Network.AWS.AppSync.Types.DynamodbDataSourceConfig
import Network.AWS.AppSync.Types.ElasticsearchDataSourceConfig
import Network.AWS.AppSync.Types.HttpDataSourceConfig
import Network.AWS.AppSync.Types.LambdaDataSourceConfig
import Network.AWS.AppSync.Types.OpenSearchServiceDataSourceConfig
import Network.AWS.AppSync.Types.RelationalDatabaseDataSourceConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a data source.
--
-- /See:/ 'newDataSource' smart constructor.
data DataSource = DataSource'
  { -- | The Identity and Access Management service role ARN for the data source.
    -- The system assumes this role when accessing the data source.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Relational database settings.
    relationalDatabaseConfig :: Prelude.Maybe RelationalDatabaseDataSourceConfig,
    -- | The data source ARN.
    dataSourceArn :: Prelude.Maybe Prelude.Text,
    -- | Amazon DynamoDB settings.
    dynamodbConfig :: Prelude.Maybe DynamodbDataSourceConfig,
    -- | The name of the data source.
    name :: Prelude.Maybe Prelude.Text,
    -- | HTTP endpoint settings.
    httpConfig :: Prelude.Maybe HttpDataSourceConfig,
    -- | Amazon OpenSearch Service settings.
    openSearchServiceConfig :: Prelude.Maybe OpenSearchServiceDataSourceConfig,
    -- | Amazon Web Services Lambda settings.
    lambdaConfig :: Prelude.Maybe LambdaDataSourceConfig,
    -- | The type of the data source.
    --
    -- -   __AWS_LAMBDA__: The data source is an Amazon Web Services Lambda
    --     function.
    --
    -- -   __AMAZON_DYNAMODB__: The data source is an Amazon DynamoDB table.
    --
    -- -   __AMAZON_ELASTICSEARCH__: The data source is an Amazon OpenSearch
    --     Service domain.
    --
    -- -   __AMAZON_OPENSEARCH_SERVICE__: The data source is an Amazon
    --     OpenSearch Service domain.
    --
    -- -   __NONE__: There is no data source. This type is used when you wish
    --     to invoke a GraphQL operation without connecting to a data source,
    --     such as performing data transformation with resolvers or triggering
    --     a subscription to be invoked from a mutation.
    --
    -- -   __HTTP__: The data source is an HTTP endpoint.
    --
    -- -   __RELATIONAL_DATABASE__: The data source is a relational database.
    type' :: Prelude.Maybe DataSourceType,
    -- | The description of the data source.
    description :: Prelude.Maybe Prelude.Text,
    -- | Amazon OpenSearch Service settings.
    elasticsearchConfig :: Prelude.Maybe ElasticsearchDataSourceConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceRoleArn', 'dataSource_serviceRoleArn' - The Identity and Access Management service role ARN for the data source.
-- The system assumes this role when accessing the data source.
--
-- 'relationalDatabaseConfig', 'dataSource_relationalDatabaseConfig' - Relational database settings.
--
-- 'dataSourceArn', 'dataSource_dataSourceArn' - The data source ARN.
--
-- 'dynamodbConfig', 'dataSource_dynamodbConfig' - Amazon DynamoDB settings.
--
-- 'name', 'dataSource_name' - The name of the data source.
--
-- 'httpConfig', 'dataSource_httpConfig' - HTTP endpoint settings.
--
-- 'openSearchServiceConfig', 'dataSource_openSearchServiceConfig' - Amazon OpenSearch Service settings.
--
-- 'lambdaConfig', 'dataSource_lambdaConfig' - Amazon Web Services Lambda settings.
--
-- 'type'', 'dataSource_type' - The type of the data source.
--
-- -   __AWS_LAMBDA__: The data source is an Amazon Web Services Lambda
--     function.
--
-- -   __AMAZON_DYNAMODB__: The data source is an Amazon DynamoDB table.
--
-- -   __AMAZON_ELASTICSEARCH__: The data source is an Amazon OpenSearch
--     Service domain.
--
-- -   __AMAZON_OPENSEARCH_SERVICE__: The data source is an Amazon
--     OpenSearch Service domain.
--
-- -   __NONE__: There is no data source. This type is used when you wish
--     to invoke a GraphQL operation without connecting to a data source,
--     such as performing data transformation with resolvers or triggering
--     a subscription to be invoked from a mutation.
--
-- -   __HTTP__: The data source is an HTTP endpoint.
--
-- -   __RELATIONAL_DATABASE__: The data source is a relational database.
--
-- 'description', 'dataSource_description' - The description of the data source.
--
-- 'elasticsearchConfig', 'dataSource_elasticsearchConfig' - Amazon OpenSearch Service settings.
newDataSource ::
  DataSource
newDataSource =
  DataSource'
    { serviceRoleArn = Prelude.Nothing,
      relationalDatabaseConfig = Prelude.Nothing,
      dataSourceArn = Prelude.Nothing,
      dynamodbConfig = Prelude.Nothing,
      name = Prelude.Nothing,
      httpConfig = Prelude.Nothing,
      openSearchServiceConfig = Prelude.Nothing,
      lambdaConfig = Prelude.Nothing,
      type' = Prelude.Nothing,
      description = Prelude.Nothing,
      elasticsearchConfig = Prelude.Nothing
    }

-- | The Identity and Access Management service role ARN for the data source.
-- The system assumes this role when accessing the data source.
dataSource_serviceRoleArn :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_serviceRoleArn = Lens.lens (\DataSource' {serviceRoleArn} -> serviceRoleArn) (\s@DataSource' {} a -> s {serviceRoleArn = a} :: DataSource)

-- | Relational database settings.
dataSource_relationalDatabaseConfig :: Lens.Lens' DataSource (Prelude.Maybe RelationalDatabaseDataSourceConfig)
dataSource_relationalDatabaseConfig = Lens.lens (\DataSource' {relationalDatabaseConfig} -> relationalDatabaseConfig) (\s@DataSource' {} a -> s {relationalDatabaseConfig = a} :: DataSource)

-- | The data source ARN.
dataSource_dataSourceArn :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_dataSourceArn = Lens.lens (\DataSource' {dataSourceArn} -> dataSourceArn) (\s@DataSource' {} a -> s {dataSourceArn = a} :: DataSource)

-- | Amazon DynamoDB settings.
dataSource_dynamodbConfig :: Lens.Lens' DataSource (Prelude.Maybe DynamodbDataSourceConfig)
dataSource_dynamodbConfig = Lens.lens (\DataSource' {dynamodbConfig} -> dynamodbConfig) (\s@DataSource' {} a -> s {dynamodbConfig = a} :: DataSource)

-- | The name of the data source.
dataSource_name :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_name = Lens.lens (\DataSource' {name} -> name) (\s@DataSource' {} a -> s {name = a} :: DataSource)

-- | HTTP endpoint settings.
dataSource_httpConfig :: Lens.Lens' DataSource (Prelude.Maybe HttpDataSourceConfig)
dataSource_httpConfig = Lens.lens (\DataSource' {httpConfig} -> httpConfig) (\s@DataSource' {} a -> s {httpConfig = a} :: DataSource)

-- | Amazon OpenSearch Service settings.
dataSource_openSearchServiceConfig :: Lens.Lens' DataSource (Prelude.Maybe OpenSearchServiceDataSourceConfig)
dataSource_openSearchServiceConfig = Lens.lens (\DataSource' {openSearchServiceConfig} -> openSearchServiceConfig) (\s@DataSource' {} a -> s {openSearchServiceConfig = a} :: DataSource)

-- | Amazon Web Services Lambda settings.
dataSource_lambdaConfig :: Lens.Lens' DataSource (Prelude.Maybe LambdaDataSourceConfig)
dataSource_lambdaConfig = Lens.lens (\DataSource' {lambdaConfig} -> lambdaConfig) (\s@DataSource' {} a -> s {lambdaConfig = a} :: DataSource)

-- | The type of the data source.
--
-- -   __AWS_LAMBDA__: The data source is an Amazon Web Services Lambda
--     function.
--
-- -   __AMAZON_DYNAMODB__: The data source is an Amazon DynamoDB table.
--
-- -   __AMAZON_ELASTICSEARCH__: The data source is an Amazon OpenSearch
--     Service domain.
--
-- -   __AMAZON_OPENSEARCH_SERVICE__: The data source is an Amazon
--     OpenSearch Service domain.
--
-- -   __NONE__: There is no data source. This type is used when you wish
--     to invoke a GraphQL operation without connecting to a data source,
--     such as performing data transformation with resolvers or triggering
--     a subscription to be invoked from a mutation.
--
-- -   __HTTP__: The data source is an HTTP endpoint.
--
-- -   __RELATIONAL_DATABASE__: The data source is a relational database.
dataSource_type :: Lens.Lens' DataSource (Prelude.Maybe DataSourceType)
dataSource_type = Lens.lens (\DataSource' {type'} -> type') (\s@DataSource' {} a -> s {type' = a} :: DataSource)

-- | The description of the data source.
dataSource_description :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_description = Lens.lens (\DataSource' {description} -> description) (\s@DataSource' {} a -> s {description = a} :: DataSource)

-- | Amazon OpenSearch Service settings.
dataSource_elasticsearchConfig :: Lens.Lens' DataSource (Prelude.Maybe ElasticsearchDataSourceConfig)
dataSource_elasticsearchConfig = Lens.lens (\DataSource' {elasticsearchConfig} -> elasticsearchConfig) (\s@DataSource' {} a -> s {elasticsearchConfig = a} :: DataSource)

instance Core.FromJSON DataSource where
  parseJSON =
    Core.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Prelude.<$> (x Core..:? "serviceRoleArn")
            Prelude.<*> (x Core..:? "relationalDatabaseConfig")
            Prelude.<*> (x Core..:? "dataSourceArn")
            Prelude.<*> (x Core..:? "dynamodbConfig")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "httpConfig")
            Prelude.<*> (x Core..:? "openSearchServiceConfig")
            Prelude.<*> (x Core..:? "lambdaConfig")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "elasticsearchConfig")
      )

instance Prelude.Hashable DataSource

instance Prelude.NFData DataSource
