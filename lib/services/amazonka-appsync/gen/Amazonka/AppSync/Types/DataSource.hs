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
-- Module      : Amazonka.AppSync.Types.DataSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.DataSource where

import Amazonka.AppSync.Types.DataSourceType
import Amazonka.AppSync.Types.DynamodbDataSourceConfig
import Amazonka.AppSync.Types.ElasticsearchDataSourceConfig
import Amazonka.AppSync.Types.HttpDataSourceConfig
import Amazonka.AppSync.Types.LambdaDataSourceConfig
import Amazonka.AppSync.Types.OpenSearchServiceDataSourceConfig
import Amazonka.AppSync.Types.RelationalDatabaseDataSourceConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a data source.
--
-- /See:/ 'newDataSource' smart constructor.
data DataSource = DataSource'
  { -- | The name of the data source.
    name :: Prelude.Maybe Prelude.Text,
    -- | The data source Amazon Resource Name (ARN).
    dataSourceArn :: Prelude.Maybe Prelude.Text,
    -- | The type of the data source.
    --
    -- -   __AWS_LAMBDA__: The data source is an Lambda function.
    --
    -- -   __AMAZON_DYNAMODB__: The data source is an Amazon DynamoDB table.
    --
    -- -   __AMAZON_ELASTICSEARCH__: The data source is an Amazon OpenSearch
    --     Service domain.
    --
    -- -   __AMAZON_OPENSEARCH_SERVICE__: The data source is an Amazon
    --     OpenSearch Service domain.
    --
    -- -   __NONE__: There is no data source. Use this type when you want to
    --     invoke a GraphQL operation without connecting to a data source, such
    --     as when you\'re performing data transformation with resolvers or
    --     invoking a subscription from a mutation.
    --
    -- -   __HTTP__: The data source is an HTTP endpoint.
    --
    -- -   __RELATIONAL_DATABASE__: The data source is a relational database.
    type' :: Prelude.Maybe DataSourceType,
    -- | Relational database settings.
    relationalDatabaseConfig :: Prelude.Maybe RelationalDatabaseDataSourceConfig,
    -- | The Identity and Access Management (IAM) service role Amazon Resource
    -- Name (ARN) for the data source. The system assumes this role when
    -- accessing the data source.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Amazon OpenSearch Service settings.
    openSearchServiceConfig :: Prelude.Maybe OpenSearchServiceDataSourceConfig,
    -- | The description of the data source.
    description :: Prelude.Maybe Prelude.Text,
    -- | Amazon OpenSearch Service settings.
    elasticsearchConfig :: Prelude.Maybe ElasticsearchDataSourceConfig,
    -- | Lambda settings.
    lambdaConfig :: Prelude.Maybe LambdaDataSourceConfig,
    -- | DynamoDB settings.
    dynamodbConfig :: Prelude.Maybe DynamodbDataSourceConfig,
    -- | HTTP endpoint settings.
    httpConfig :: Prelude.Maybe HttpDataSourceConfig
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
-- 'name', 'dataSource_name' - The name of the data source.
--
-- 'dataSourceArn', 'dataSource_dataSourceArn' - The data source Amazon Resource Name (ARN).
--
-- 'type'', 'dataSource_type' - The type of the data source.
--
-- -   __AWS_LAMBDA__: The data source is an Lambda function.
--
-- -   __AMAZON_DYNAMODB__: The data source is an Amazon DynamoDB table.
--
-- -   __AMAZON_ELASTICSEARCH__: The data source is an Amazon OpenSearch
--     Service domain.
--
-- -   __AMAZON_OPENSEARCH_SERVICE__: The data source is an Amazon
--     OpenSearch Service domain.
--
-- -   __NONE__: There is no data source. Use this type when you want to
--     invoke a GraphQL operation without connecting to a data source, such
--     as when you\'re performing data transformation with resolvers or
--     invoking a subscription from a mutation.
--
-- -   __HTTP__: The data source is an HTTP endpoint.
--
-- -   __RELATIONAL_DATABASE__: The data source is a relational database.
--
-- 'relationalDatabaseConfig', 'dataSource_relationalDatabaseConfig' - Relational database settings.
--
-- 'serviceRoleArn', 'dataSource_serviceRoleArn' - The Identity and Access Management (IAM) service role Amazon Resource
-- Name (ARN) for the data source. The system assumes this role when
-- accessing the data source.
--
-- 'openSearchServiceConfig', 'dataSource_openSearchServiceConfig' - Amazon OpenSearch Service settings.
--
-- 'description', 'dataSource_description' - The description of the data source.
--
-- 'elasticsearchConfig', 'dataSource_elasticsearchConfig' - Amazon OpenSearch Service settings.
--
-- 'lambdaConfig', 'dataSource_lambdaConfig' - Lambda settings.
--
-- 'dynamodbConfig', 'dataSource_dynamodbConfig' - DynamoDB settings.
--
-- 'httpConfig', 'dataSource_httpConfig' - HTTP endpoint settings.
newDataSource ::
  DataSource
newDataSource =
  DataSource'
    { name = Prelude.Nothing,
      dataSourceArn = Prelude.Nothing,
      type' = Prelude.Nothing,
      relationalDatabaseConfig = Prelude.Nothing,
      serviceRoleArn = Prelude.Nothing,
      openSearchServiceConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      elasticsearchConfig = Prelude.Nothing,
      lambdaConfig = Prelude.Nothing,
      dynamodbConfig = Prelude.Nothing,
      httpConfig = Prelude.Nothing
    }

-- | The name of the data source.
dataSource_name :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_name = Lens.lens (\DataSource' {name} -> name) (\s@DataSource' {} a -> s {name = a} :: DataSource)

-- | The data source Amazon Resource Name (ARN).
dataSource_dataSourceArn :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_dataSourceArn = Lens.lens (\DataSource' {dataSourceArn} -> dataSourceArn) (\s@DataSource' {} a -> s {dataSourceArn = a} :: DataSource)

-- | The type of the data source.
--
-- -   __AWS_LAMBDA__: The data source is an Lambda function.
--
-- -   __AMAZON_DYNAMODB__: The data source is an Amazon DynamoDB table.
--
-- -   __AMAZON_ELASTICSEARCH__: The data source is an Amazon OpenSearch
--     Service domain.
--
-- -   __AMAZON_OPENSEARCH_SERVICE__: The data source is an Amazon
--     OpenSearch Service domain.
--
-- -   __NONE__: There is no data source. Use this type when you want to
--     invoke a GraphQL operation without connecting to a data source, such
--     as when you\'re performing data transformation with resolvers or
--     invoking a subscription from a mutation.
--
-- -   __HTTP__: The data source is an HTTP endpoint.
--
-- -   __RELATIONAL_DATABASE__: The data source is a relational database.
dataSource_type :: Lens.Lens' DataSource (Prelude.Maybe DataSourceType)
dataSource_type = Lens.lens (\DataSource' {type'} -> type') (\s@DataSource' {} a -> s {type' = a} :: DataSource)

-- | Relational database settings.
dataSource_relationalDatabaseConfig :: Lens.Lens' DataSource (Prelude.Maybe RelationalDatabaseDataSourceConfig)
dataSource_relationalDatabaseConfig = Lens.lens (\DataSource' {relationalDatabaseConfig} -> relationalDatabaseConfig) (\s@DataSource' {} a -> s {relationalDatabaseConfig = a} :: DataSource)

-- | The Identity and Access Management (IAM) service role Amazon Resource
-- Name (ARN) for the data source. The system assumes this role when
-- accessing the data source.
dataSource_serviceRoleArn :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_serviceRoleArn = Lens.lens (\DataSource' {serviceRoleArn} -> serviceRoleArn) (\s@DataSource' {} a -> s {serviceRoleArn = a} :: DataSource)

-- | Amazon OpenSearch Service settings.
dataSource_openSearchServiceConfig :: Lens.Lens' DataSource (Prelude.Maybe OpenSearchServiceDataSourceConfig)
dataSource_openSearchServiceConfig = Lens.lens (\DataSource' {openSearchServiceConfig} -> openSearchServiceConfig) (\s@DataSource' {} a -> s {openSearchServiceConfig = a} :: DataSource)

-- | The description of the data source.
dataSource_description :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_description = Lens.lens (\DataSource' {description} -> description) (\s@DataSource' {} a -> s {description = a} :: DataSource)

-- | Amazon OpenSearch Service settings.
dataSource_elasticsearchConfig :: Lens.Lens' DataSource (Prelude.Maybe ElasticsearchDataSourceConfig)
dataSource_elasticsearchConfig = Lens.lens (\DataSource' {elasticsearchConfig} -> elasticsearchConfig) (\s@DataSource' {} a -> s {elasticsearchConfig = a} :: DataSource)

-- | Lambda settings.
dataSource_lambdaConfig :: Lens.Lens' DataSource (Prelude.Maybe LambdaDataSourceConfig)
dataSource_lambdaConfig = Lens.lens (\DataSource' {lambdaConfig} -> lambdaConfig) (\s@DataSource' {} a -> s {lambdaConfig = a} :: DataSource)

-- | DynamoDB settings.
dataSource_dynamodbConfig :: Lens.Lens' DataSource (Prelude.Maybe DynamodbDataSourceConfig)
dataSource_dynamodbConfig = Lens.lens (\DataSource' {dynamodbConfig} -> dynamodbConfig) (\s@DataSource' {} a -> s {dynamodbConfig = a} :: DataSource)

-- | HTTP endpoint settings.
dataSource_httpConfig :: Lens.Lens' DataSource (Prelude.Maybe HttpDataSourceConfig)
dataSource_httpConfig = Lens.lens (\DataSource' {httpConfig} -> httpConfig) (\s@DataSource' {} a -> s {httpConfig = a} :: DataSource)

instance Core.FromJSON DataSource where
  parseJSON =
    Core.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "dataSourceArn")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "relationalDatabaseConfig")
            Prelude.<*> (x Core..:? "serviceRoleArn")
            Prelude.<*> (x Core..:? "openSearchServiceConfig")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "elasticsearchConfig")
            Prelude.<*> (x Core..:? "lambdaConfig")
            Prelude.<*> (x Core..:? "dynamodbConfig")
            Prelude.<*> (x Core..:? "httpConfig")
      )

instance Prelude.Hashable DataSource where
  hashWithSalt _salt DataSource' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` dataSourceArn
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` relationalDatabaseConfig
      `Prelude.hashWithSalt` serviceRoleArn
      `Prelude.hashWithSalt` openSearchServiceConfig
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` elasticsearchConfig
      `Prelude.hashWithSalt` lambdaConfig
      `Prelude.hashWithSalt` dynamodbConfig
      `Prelude.hashWithSalt` httpConfig

instance Prelude.NFData DataSource where
  rnf DataSource' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf dataSourceArn
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf relationalDatabaseConfig
      `Prelude.seq` Prelude.rnf serviceRoleArn
      `Prelude.seq` Prelude.rnf openSearchServiceConfig
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf elasticsearchConfig
      `Prelude.seq` Prelude.rnf lambdaConfig
      `Prelude.seq` Prelude.rnf dynamodbConfig
      `Prelude.seq` Prelude.rnf httpConfig
