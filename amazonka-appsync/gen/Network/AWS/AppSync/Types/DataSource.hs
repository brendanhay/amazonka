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
import Network.AWS.AppSync.Types.RelationalDatabaseDataSourceConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a data source.
--
-- /See:/ 'newDataSource' smart constructor.
data DataSource = DataSource'
  { -- | Relational database settings.
    relationalDatabaseConfig :: Core.Maybe RelationalDatabaseDataSourceConfig,
    -- | The AWS IAM service role ARN for the data source. The system assumes
    -- this role when accessing the data source.
    serviceRoleArn :: Core.Maybe Core.Text,
    -- | Amazon Elasticsearch Service settings.
    elasticsearchConfig :: Core.Maybe ElasticsearchDataSourceConfig,
    -- | AWS Lambda settings.
    lambdaConfig :: Core.Maybe LambdaDataSourceConfig,
    -- | The name of the data source.
    name :: Core.Maybe Core.Text,
    -- | Amazon DynamoDB settings.
    dynamodbConfig :: Core.Maybe DynamodbDataSourceConfig,
    -- | The description of the data source.
    description :: Core.Maybe Core.Text,
    -- | The data source ARN.
    dataSourceArn :: Core.Maybe Core.Text,
    -- | The type of the data source.
    --
    -- -   __AMAZON_DYNAMODB__: The data source is an Amazon DynamoDB table.
    --
    -- -   __AMAZON_ELASTICSEARCH__: The data source is an Amazon Elasticsearch
    --     Service domain.
    --
    -- -   __AWS_LAMBDA__: The data source is an AWS Lambda function.
    --
    -- -   __NONE__: There is no data source. This type is used when you wish
    --     to invoke a GraphQL operation without connecting to a data source,
    --     such as performing data transformation with resolvers or triggering
    --     a subscription to be invoked from a mutation.
    --
    -- -   __HTTP__: The data source is an HTTP endpoint.
    --
    -- -   __RELATIONAL_DATABASE__: The data source is a relational database.
    type' :: Core.Maybe DataSourceType,
    -- | HTTP endpoint settings.
    httpConfig :: Core.Maybe HttpDataSourceConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationalDatabaseConfig', 'dataSource_relationalDatabaseConfig' - Relational database settings.
--
-- 'serviceRoleArn', 'dataSource_serviceRoleArn' - The AWS IAM service role ARN for the data source. The system assumes
-- this role when accessing the data source.
--
-- 'elasticsearchConfig', 'dataSource_elasticsearchConfig' - Amazon Elasticsearch Service settings.
--
-- 'lambdaConfig', 'dataSource_lambdaConfig' - AWS Lambda settings.
--
-- 'name', 'dataSource_name' - The name of the data source.
--
-- 'dynamodbConfig', 'dataSource_dynamodbConfig' - Amazon DynamoDB settings.
--
-- 'description', 'dataSource_description' - The description of the data source.
--
-- 'dataSourceArn', 'dataSource_dataSourceArn' - The data source ARN.
--
-- 'type'', 'dataSource_type' - The type of the data source.
--
-- -   __AMAZON_DYNAMODB__: The data source is an Amazon DynamoDB table.
--
-- -   __AMAZON_ELASTICSEARCH__: The data source is an Amazon Elasticsearch
--     Service domain.
--
-- -   __AWS_LAMBDA__: The data source is an AWS Lambda function.
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
-- 'httpConfig', 'dataSource_httpConfig' - HTTP endpoint settings.
newDataSource ::
  DataSource
newDataSource =
  DataSource'
    { relationalDatabaseConfig =
        Core.Nothing,
      serviceRoleArn = Core.Nothing,
      elasticsearchConfig = Core.Nothing,
      lambdaConfig = Core.Nothing,
      name = Core.Nothing,
      dynamodbConfig = Core.Nothing,
      description = Core.Nothing,
      dataSourceArn = Core.Nothing,
      type' = Core.Nothing,
      httpConfig = Core.Nothing
    }

-- | Relational database settings.
dataSource_relationalDatabaseConfig :: Lens.Lens' DataSource (Core.Maybe RelationalDatabaseDataSourceConfig)
dataSource_relationalDatabaseConfig = Lens.lens (\DataSource' {relationalDatabaseConfig} -> relationalDatabaseConfig) (\s@DataSource' {} a -> s {relationalDatabaseConfig = a} :: DataSource)

-- | The AWS IAM service role ARN for the data source. The system assumes
-- this role when accessing the data source.
dataSource_serviceRoleArn :: Lens.Lens' DataSource (Core.Maybe Core.Text)
dataSource_serviceRoleArn = Lens.lens (\DataSource' {serviceRoleArn} -> serviceRoleArn) (\s@DataSource' {} a -> s {serviceRoleArn = a} :: DataSource)

-- | Amazon Elasticsearch Service settings.
dataSource_elasticsearchConfig :: Lens.Lens' DataSource (Core.Maybe ElasticsearchDataSourceConfig)
dataSource_elasticsearchConfig = Lens.lens (\DataSource' {elasticsearchConfig} -> elasticsearchConfig) (\s@DataSource' {} a -> s {elasticsearchConfig = a} :: DataSource)

-- | AWS Lambda settings.
dataSource_lambdaConfig :: Lens.Lens' DataSource (Core.Maybe LambdaDataSourceConfig)
dataSource_lambdaConfig = Lens.lens (\DataSource' {lambdaConfig} -> lambdaConfig) (\s@DataSource' {} a -> s {lambdaConfig = a} :: DataSource)

-- | The name of the data source.
dataSource_name :: Lens.Lens' DataSource (Core.Maybe Core.Text)
dataSource_name = Lens.lens (\DataSource' {name} -> name) (\s@DataSource' {} a -> s {name = a} :: DataSource)

-- | Amazon DynamoDB settings.
dataSource_dynamodbConfig :: Lens.Lens' DataSource (Core.Maybe DynamodbDataSourceConfig)
dataSource_dynamodbConfig = Lens.lens (\DataSource' {dynamodbConfig} -> dynamodbConfig) (\s@DataSource' {} a -> s {dynamodbConfig = a} :: DataSource)

-- | The description of the data source.
dataSource_description :: Lens.Lens' DataSource (Core.Maybe Core.Text)
dataSource_description = Lens.lens (\DataSource' {description} -> description) (\s@DataSource' {} a -> s {description = a} :: DataSource)

-- | The data source ARN.
dataSource_dataSourceArn :: Lens.Lens' DataSource (Core.Maybe Core.Text)
dataSource_dataSourceArn = Lens.lens (\DataSource' {dataSourceArn} -> dataSourceArn) (\s@DataSource' {} a -> s {dataSourceArn = a} :: DataSource)

-- | The type of the data source.
--
-- -   __AMAZON_DYNAMODB__: The data source is an Amazon DynamoDB table.
--
-- -   __AMAZON_ELASTICSEARCH__: The data source is an Amazon Elasticsearch
--     Service domain.
--
-- -   __AWS_LAMBDA__: The data source is an AWS Lambda function.
--
-- -   __NONE__: There is no data source. This type is used when you wish
--     to invoke a GraphQL operation without connecting to a data source,
--     such as performing data transformation with resolvers or triggering
--     a subscription to be invoked from a mutation.
--
-- -   __HTTP__: The data source is an HTTP endpoint.
--
-- -   __RELATIONAL_DATABASE__: The data source is a relational database.
dataSource_type :: Lens.Lens' DataSource (Core.Maybe DataSourceType)
dataSource_type = Lens.lens (\DataSource' {type'} -> type') (\s@DataSource' {} a -> s {type' = a} :: DataSource)

-- | HTTP endpoint settings.
dataSource_httpConfig :: Lens.Lens' DataSource (Core.Maybe HttpDataSourceConfig)
dataSource_httpConfig = Lens.lens (\DataSource' {httpConfig} -> httpConfig) (\s@DataSource' {} a -> s {httpConfig = a} :: DataSource)

instance Core.FromJSON DataSource where
  parseJSON =
    Core.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Core.<$> (x Core..:? "relationalDatabaseConfig")
            Core.<*> (x Core..:? "serviceRoleArn")
            Core.<*> (x Core..:? "elasticsearchConfig")
            Core.<*> (x Core..:? "lambdaConfig")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "dynamodbConfig")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "dataSourceArn")
            Core.<*> (x Core..:? "type")
            Core.<*> (x Core..:? "httpConfig")
      )

instance Core.Hashable DataSource

instance Core.NFData DataSource
