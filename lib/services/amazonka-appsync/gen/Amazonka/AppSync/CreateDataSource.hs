{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppSync.CreateDataSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @DataSource@ object.
module Amazonka.AppSync.CreateDataSource
  ( -- * Creating a Request
    CreateDataSource (..),
    newCreateDataSource,

    -- * Request Lenses
    createDataSource_relationalDatabaseConfig,
    createDataSource_serviceRoleArn,
    createDataSource_openSearchServiceConfig,
    createDataSource_description,
    createDataSource_elasticsearchConfig,
    createDataSource_lambdaConfig,
    createDataSource_dynamodbConfig,
    createDataSource_httpConfig,
    createDataSource_apiId,
    createDataSource_name,
    createDataSource_type,

    -- * Destructuring the Response
    CreateDataSourceResponse (..),
    newCreateDataSourceResponse,

    -- * Response Lenses
    createDataSourceResponse_dataSource,
    createDataSourceResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDataSource' smart constructor.
data CreateDataSource = CreateDataSource'
  { -- | Relational database settings.
    relationalDatabaseConfig :: Prelude.Maybe RelationalDatabaseDataSourceConfig,
    -- | The Identity and Access Management (IAM) service role Amazon Resource
    -- Name (ARN) for the data source. The system assumes this role when
    -- accessing the data source.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Amazon OpenSearch Service settings.
    openSearchServiceConfig :: Prelude.Maybe OpenSearchServiceDataSourceConfig,
    -- | A description of the @DataSource@.
    description :: Prelude.Maybe Prelude.Text,
    -- | Amazon OpenSearch Service settings.
    --
    -- As of September 2021, Amazon Elasticsearch service is Amazon OpenSearch
    -- Service. This configuration is deprecated. For new data sources, use
    -- CreateDataSourceRequest$openSearchServiceConfig to create an OpenSearch
    -- data source.
    elasticsearchConfig :: Prelude.Maybe ElasticsearchDataSourceConfig,
    -- | Lambda settings.
    lambdaConfig :: Prelude.Maybe LambdaDataSourceConfig,
    -- | Amazon DynamoDB settings.
    dynamodbConfig :: Prelude.Maybe DynamodbDataSourceConfig,
    -- | HTTP endpoint settings.
    httpConfig :: Prelude.Maybe HttpDataSourceConfig,
    -- | The API ID for the GraphQL API for the @DataSource@.
    apiId :: Prelude.Text,
    -- | A user-supplied name for the @DataSource@.
    name :: Prelude.Text,
    -- | The type of the @DataSource@.
    type' :: DataSourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationalDatabaseConfig', 'createDataSource_relationalDatabaseConfig' - Relational database settings.
--
-- 'serviceRoleArn', 'createDataSource_serviceRoleArn' - The Identity and Access Management (IAM) service role Amazon Resource
-- Name (ARN) for the data source. The system assumes this role when
-- accessing the data source.
--
-- 'openSearchServiceConfig', 'createDataSource_openSearchServiceConfig' - Amazon OpenSearch Service settings.
--
-- 'description', 'createDataSource_description' - A description of the @DataSource@.
--
-- 'elasticsearchConfig', 'createDataSource_elasticsearchConfig' - Amazon OpenSearch Service settings.
--
-- As of September 2021, Amazon Elasticsearch service is Amazon OpenSearch
-- Service. This configuration is deprecated. For new data sources, use
-- CreateDataSourceRequest$openSearchServiceConfig to create an OpenSearch
-- data source.
--
-- 'lambdaConfig', 'createDataSource_lambdaConfig' - Lambda settings.
--
-- 'dynamodbConfig', 'createDataSource_dynamodbConfig' - Amazon DynamoDB settings.
--
-- 'httpConfig', 'createDataSource_httpConfig' - HTTP endpoint settings.
--
-- 'apiId', 'createDataSource_apiId' - The API ID for the GraphQL API for the @DataSource@.
--
-- 'name', 'createDataSource_name' - A user-supplied name for the @DataSource@.
--
-- 'type'', 'createDataSource_type' - The type of the @DataSource@.
newCreateDataSource ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  DataSourceType ->
  CreateDataSource
newCreateDataSource pApiId_ pName_ pType_ =
  CreateDataSource'
    { relationalDatabaseConfig =
        Prelude.Nothing,
      serviceRoleArn = Prelude.Nothing,
      openSearchServiceConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      elasticsearchConfig = Prelude.Nothing,
      lambdaConfig = Prelude.Nothing,
      dynamodbConfig = Prelude.Nothing,
      httpConfig = Prelude.Nothing,
      apiId = pApiId_,
      name = pName_,
      type' = pType_
    }

-- | Relational database settings.
createDataSource_relationalDatabaseConfig :: Lens.Lens' CreateDataSource (Prelude.Maybe RelationalDatabaseDataSourceConfig)
createDataSource_relationalDatabaseConfig = Lens.lens (\CreateDataSource' {relationalDatabaseConfig} -> relationalDatabaseConfig) (\s@CreateDataSource' {} a -> s {relationalDatabaseConfig = a} :: CreateDataSource)

-- | The Identity and Access Management (IAM) service role Amazon Resource
-- Name (ARN) for the data source. The system assumes this role when
-- accessing the data source.
createDataSource_serviceRoleArn :: Lens.Lens' CreateDataSource (Prelude.Maybe Prelude.Text)
createDataSource_serviceRoleArn = Lens.lens (\CreateDataSource' {serviceRoleArn} -> serviceRoleArn) (\s@CreateDataSource' {} a -> s {serviceRoleArn = a} :: CreateDataSource)

-- | Amazon OpenSearch Service settings.
createDataSource_openSearchServiceConfig :: Lens.Lens' CreateDataSource (Prelude.Maybe OpenSearchServiceDataSourceConfig)
createDataSource_openSearchServiceConfig = Lens.lens (\CreateDataSource' {openSearchServiceConfig} -> openSearchServiceConfig) (\s@CreateDataSource' {} a -> s {openSearchServiceConfig = a} :: CreateDataSource)

-- | A description of the @DataSource@.
createDataSource_description :: Lens.Lens' CreateDataSource (Prelude.Maybe Prelude.Text)
createDataSource_description = Lens.lens (\CreateDataSource' {description} -> description) (\s@CreateDataSource' {} a -> s {description = a} :: CreateDataSource)

-- | Amazon OpenSearch Service settings.
--
-- As of September 2021, Amazon Elasticsearch service is Amazon OpenSearch
-- Service. This configuration is deprecated. For new data sources, use
-- CreateDataSourceRequest$openSearchServiceConfig to create an OpenSearch
-- data source.
createDataSource_elasticsearchConfig :: Lens.Lens' CreateDataSource (Prelude.Maybe ElasticsearchDataSourceConfig)
createDataSource_elasticsearchConfig = Lens.lens (\CreateDataSource' {elasticsearchConfig} -> elasticsearchConfig) (\s@CreateDataSource' {} a -> s {elasticsearchConfig = a} :: CreateDataSource)

-- | Lambda settings.
createDataSource_lambdaConfig :: Lens.Lens' CreateDataSource (Prelude.Maybe LambdaDataSourceConfig)
createDataSource_lambdaConfig = Lens.lens (\CreateDataSource' {lambdaConfig} -> lambdaConfig) (\s@CreateDataSource' {} a -> s {lambdaConfig = a} :: CreateDataSource)

-- | Amazon DynamoDB settings.
createDataSource_dynamodbConfig :: Lens.Lens' CreateDataSource (Prelude.Maybe DynamodbDataSourceConfig)
createDataSource_dynamodbConfig = Lens.lens (\CreateDataSource' {dynamodbConfig} -> dynamodbConfig) (\s@CreateDataSource' {} a -> s {dynamodbConfig = a} :: CreateDataSource)

-- | HTTP endpoint settings.
createDataSource_httpConfig :: Lens.Lens' CreateDataSource (Prelude.Maybe HttpDataSourceConfig)
createDataSource_httpConfig = Lens.lens (\CreateDataSource' {httpConfig} -> httpConfig) (\s@CreateDataSource' {} a -> s {httpConfig = a} :: CreateDataSource)

-- | The API ID for the GraphQL API for the @DataSource@.
createDataSource_apiId :: Lens.Lens' CreateDataSource Prelude.Text
createDataSource_apiId = Lens.lens (\CreateDataSource' {apiId} -> apiId) (\s@CreateDataSource' {} a -> s {apiId = a} :: CreateDataSource)

-- | A user-supplied name for the @DataSource@.
createDataSource_name :: Lens.Lens' CreateDataSource Prelude.Text
createDataSource_name = Lens.lens (\CreateDataSource' {name} -> name) (\s@CreateDataSource' {} a -> s {name = a} :: CreateDataSource)

-- | The type of the @DataSource@.
createDataSource_type :: Lens.Lens' CreateDataSource DataSourceType
createDataSource_type = Lens.lens (\CreateDataSource' {type'} -> type') (\s@CreateDataSource' {} a -> s {type' = a} :: CreateDataSource)

instance Core.AWSRequest CreateDataSource where
  type
    AWSResponse CreateDataSource =
      CreateDataSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataSourceResponse'
            Prelude.<$> (x Core..?> "dataSource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataSource where
  hashWithSalt _salt CreateDataSource' {..} =
    _salt
      `Prelude.hashWithSalt` relationalDatabaseConfig
      `Prelude.hashWithSalt` serviceRoleArn
      `Prelude.hashWithSalt` openSearchServiceConfig
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` elasticsearchConfig
      `Prelude.hashWithSalt` lambdaConfig
      `Prelude.hashWithSalt` dynamodbConfig
      `Prelude.hashWithSalt` httpConfig
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateDataSource where
  rnf CreateDataSource' {..} =
    Prelude.rnf relationalDatabaseConfig
      `Prelude.seq` Prelude.rnf serviceRoleArn
      `Prelude.seq` Prelude.rnf openSearchServiceConfig
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf elasticsearchConfig
      `Prelude.seq` Prelude.rnf lambdaConfig
      `Prelude.seq` Prelude.rnf dynamodbConfig
      `Prelude.seq` Prelude.rnf httpConfig
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

instance Core.ToHeaders CreateDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDataSource where
  toJSON CreateDataSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("relationalDatabaseConfig" Core..=)
              Prelude.<$> relationalDatabaseConfig,
            ("serviceRoleArn" Core..=)
              Prelude.<$> serviceRoleArn,
            ("openSearchServiceConfig" Core..=)
              Prelude.<$> openSearchServiceConfig,
            ("description" Core..=) Prelude.<$> description,
            ("elasticsearchConfig" Core..=)
              Prelude.<$> elasticsearchConfig,
            ("lambdaConfig" Core..=) Prelude.<$> lambdaConfig,
            ("dynamodbConfig" Core..=)
              Prelude.<$> dynamodbConfig,
            ("httpConfig" Core..=) Prelude.<$> httpConfig,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("type" Core..= type')
          ]
      )

instance Core.ToPath CreateDataSource where
  toPath CreateDataSource' {..} =
    Prelude.mconcat
      ["/v1/apis/", Core.toBS apiId, "/datasources"]

instance Core.ToQuery CreateDataSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDataSourceResponse' smart constructor.
data CreateDataSourceResponse = CreateDataSourceResponse'
  { -- | The @DataSource@ object.
    dataSource :: Prelude.Maybe DataSource,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSource', 'createDataSourceResponse_dataSource' - The @DataSource@ object.
--
-- 'httpStatus', 'createDataSourceResponse_httpStatus' - The response's http status code.
newCreateDataSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDataSourceResponse
newCreateDataSourceResponse pHttpStatus_ =
  CreateDataSourceResponse'
    { dataSource =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @DataSource@ object.
createDataSourceResponse_dataSource :: Lens.Lens' CreateDataSourceResponse (Prelude.Maybe DataSource)
createDataSourceResponse_dataSource = Lens.lens (\CreateDataSourceResponse' {dataSource} -> dataSource) (\s@CreateDataSourceResponse' {} a -> s {dataSource = a} :: CreateDataSourceResponse)

-- | The response's http status code.
createDataSourceResponse_httpStatus :: Lens.Lens' CreateDataSourceResponse Prelude.Int
createDataSourceResponse_httpStatus = Lens.lens (\CreateDataSourceResponse' {httpStatus} -> httpStatus) (\s@CreateDataSourceResponse' {} a -> s {httpStatus = a} :: CreateDataSourceResponse)

instance Prelude.NFData CreateDataSourceResponse where
  rnf CreateDataSourceResponse' {..} =
    Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf httpStatus
