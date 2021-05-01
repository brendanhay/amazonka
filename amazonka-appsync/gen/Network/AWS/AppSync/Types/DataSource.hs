{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a data source.
--
-- /See:/ 'newDataSource' smart constructor.
data DataSource = DataSource'
  { -- | Relational database settings.
    relationalDatabaseConfig :: Prelude.Maybe RelationalDatabaseDataSourceConfig,
    -- | The AWS IAM service role ARN for the data source. The system assumes
    -- this role when accessing the data source.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Amazon Elasticsearch Service settings.
    elasticsearchConfig :: Prelude.Maybe ElasticsearchDataSourceConfig,
    -- | AWS Lambda settings.
    lambdaConfig :: Prelude.Maybe LambdaDataSourceConfig,
    -- | The name of the data source.
    name :: Prelude.Maybe Prelude.Text,
    -- | Amazon DynamoDB settings.
    dynamodbConfig :: Prelude.Maybe DynamodbDataSourceConfig,
    -- | The description of the data source.
    description :: Prelude.Maybe Prelude.Text,
    -- | The data source ARN.
    dataSourceArn :: Prelude.Maybe Prelude.Text,
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
    type' :: Prelude.Maybe DataSourceType,
    -- | HTTP endpoint settings.
    httpConfig :: Prelude.Maybe HttpDataSourceConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      serviceRoleArn = Prelude.Nothing,
      elasticsearchConfig = Prelude.Nothing,
      lambdaConfig = Prelude.Nothing,
      name = Prelude.Nothing,
      dynamodbConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      dataSourceArn = Prelude.Nothing,
      type' = Prelude.Nothing,
      httpConfig = Prelude.Nothing
    }

-- | Relational database settings.
dataSource_relationalDatabaseConfig :: Lens.Lens' DataSource (Prelude.Maybe RelationalDatabaseDataSourceConfig)
dataSource_relationalDatabaseConfig = Lens.lens (\DataSource' {relationalDatabaseConfig} -> relationalDatabaseConfig) (\s@DataSource' {} a -> s {relationalDatabaseConfig = a} :: DataSource)

-- | The AWS IAM service role ARN for the data source. The system assumes
-- this role when accessing the data source.
dataSource_serviceRoleArn :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_serviceRoleArn = Lens.lens (\DataSource' {serviceRoleArn} -> serviceRoleArn) (\s@DataSource' {} a -> s {serviceRoleArn = a} :: DataSource)

-- | Amazon Elasticsearch Service settings.
dataSource_elasticsearchConfig :: Lens.Lens' DataSource (Prelude.Maybe ElasticsearchDataSourceConfig)
dataSource_elasticsearchConfig = Lens.lens (\DataSource' {elasticsearchConfig} -> elasticsearchConfig) (\s@DataSource' {} a -> s {elasticsearchConfig = a} :: DataSource)

-- | AWS Lambda settings.
dataSource_lambdaConfig :: Lens.Lens' DataSource (Prelude.Maybe LambdaDataSourceConfig)
dataSource_lambdaConfig = Lens.lens (\DataSource' {lambdaConfig} -> lambdaConfig) (\s@DataSource' {} a -> s {lambdaConfig = a} :: DataSource)

-- | The name of the data source.
dataSource_name :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_name = Lens.lens (\DataSource' {name} -> name) (\s@DataSource' {} a -> s {name = a} :: DataSource)

-- | Amazon DynamoDB settings.
dataSource_dynamodbConfig :: Lens.Lens' DataSource (Prelude.Maybe DynamodbDataSourceConfig)
dataSource_dynamodbConfig = Lens.lens (\DataSource' {dynamodbConfig} -> dynamodbConfig) (\s@DataSource' {} a -> s {dynamodbConfig = a} :: DataSource)

-- | The description of the data source.
dataSource_description :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_description = Lens.lens (\DataSource' {description} -> description) (\s@DataSource' {} a -> s {description = a} :: DataSource)

-- | The data source ARN.
dataSource_dataSourceArn :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
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
dataSource_type :: Lens.Lens' DataSource (Prelude.Maybe DataSourceType)
dataSource_type = Lens.lens (\DataSource' {type'} -> type') (\s@DataSource' {} a -> s {type' = a} :: DataSource)

-- | HTTP endpoint settings.
dataSource_httpConfig :: Lens.Lens' DataSource (Prelude.Maybe HttpDataSourceConfig)
dataSource_httpConfig = Lens.lens (\DataSource' {httpConfig} -> httpConfig) (\s@DataSource' {} a -> s {httpConfig = a} :: DataSource)

instance Prelude.FromJSON DataSource where
  parseJSON =
    Prelude.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Prelude.<$> (x Prelude..:? "relationalDatabaseConfig")
            Prelude.<*> (x Prelude..:? "serviceRoleArn")
            Prelude.<*> (x Prelude..:? "elasticsearchConfig")
            Prelude.<*> (x Prelude..:? "lambdaConfig")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "dynamodbConfig")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "dataSourceArn")
            Prelude.<*> (x Prelude..:? "type")
            Prelude.<*> (x Prelude..:? "httpConfig")
      )

instance Prelude.Hashable DataSource

instance Prelude.NFData DataSource
