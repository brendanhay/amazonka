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
-- Module      : Network.AWS.AppSync.CreateDataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @DataSource@ object.
module Network.AWS.AppSync.CreateDataSource
  ( -- * Creating a Request
    CreateDataSource (..),
    newCreateDataSource,

    -- * Request Lenses
    createDataSource_relationalDatabaseConfig,
    createDataSource_serviceRoleArn,
    createDataSource_elasticsearchConfig,
    createDataSource_lambdaConfig,
    createDataSource_dynamodbConfig,
    createDataSource_description,
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

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDataSource' smart constructor.
data CreateDataSource = CreateDataSource'
  { -- | Relational database settings.
    relationalDatabaseConfig :: Prelude.Maybe RelationalDatabaseDataSourceConfig,
    -- | The AWS IAM service role ARN for the data source. The system assumes
    -- this role when accessing the data source.
    serviceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Amazon Elasticsearch Service settings.
    elasticsearchConfig :: Prelude.Maybe ElasticsearchDataSourceConfig,
    -- | AWS Lambda settings.
    lambdaConfig :: Prelude.Maybe LambdaDataSourceConfig,
    -- | Amazon DynamoDB settings.
    dynamodbConfig :: Prelude.Maybe DynamodbDataSourceConfig,
    -- | A description of the @DataSource@.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'serviceRoleArn', 'createDataSource_serviceRoleArn' - The AWS IAM service role ARN for the data source. The system assumes
-- this role when accessing the data source.
--
-- 'elasticsearchConfig', 'createDataSource_elasticsearchConfig' - Amazon Elasticsearch Service settings.
--
-- 'lambdaConfig', 'createDataSource_lambdaConfig' - AWS Lambda settings.
--
-- 'dynamodbConfig', 'createDataSource_dynamodbConfig' - Amazon DynamoDB settings.
--
-- 'description', 'createDataSource_description' - A description of the @DataSource@.
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
      elasticsearchConfig = Prelude.Nothing,
      lambdaConfig = Prelude.Nothing,
      dynamodbConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      httpConfig = Prelude.Nothing,
      apiId = pApiId_,
      name = pName_,
      type' = pType_
    }

-- | Relational database settings.
createDataSource_relationalDatabaseConfig :: Lens.Lens' CreateDataSource (Prelude.Maybe RelationalDatabaseDataSourceConfig)
createDataSource_relationalDatabaseConfig = Lens.lens (\CreateDataSource' {relationalDatabaseConfig} -> relationalDatabaseConfig) (\s@CreateDataSource' {} a -> s {relationalDatabaseConfig = a} :: CreateDataSource)

-- | The AWS IAM service role ARN for the data source. The system assumes
-- this role when accessing the data source.
createDataSource_serviceRoleArn :: Lens.Lens' CreateDataSource (Prelude.Maybe Prelude.Text)
createDataSource_serviceRoleArn = Lens.lens (\CreateDataSource' {serviceRoleArn} -> serviceRoleArn) (\s@CreateDataSource' {} a -> s {serviceRoleArn = a} :: CreateDataSource)

-- | Amazon Elasticsearch Service settings.
createDataSource_elasticsearchConfig :: Lens.Lens' CreateDataSource (Prelude.Maybe ElasticsearchDataSourceConfig)
createDataSource_elasticsearchConfig = Lens.lens (\CreateDataSource' {elasticsearchConfig} -> elasticsearchConfig) (\s@CreateDataSource' {} a -> s {elasticsearchConfig = a} :: CreateDataSource)

-- | AWS Lambda settings.
createDataSource_lambdaConfig :: Lens.Lens' CreateDataSource (Prelude.Maybe LambdaDataSourceConfig)
createDataSource_lambdaConfig = Lens.lens (\CreateDataSource' {lambdaConfig} -> lambdaConfig) (\s@CreateDataSource' {} a -> s {lambdaConfig = a} :: CreateDataSource)

-- | Amazon DynamoDB settings.
createDataSource_dynamodbConfig :: Lens.Lens' CreateDataSource (Prelude.Maybe DynamodbDataSourceConfig)
createDataSource_dynamodbConfig = Lens.lens (\CreateDataSource' {dynamodbConfig} -> dynamodbConfig) (\s@CreateDataSource' {} a -> s {dynamodbConfig = a} :: CreateDataSource)

-- | A description of the @DataSource@.
createDataSource_description :: Lens.Lens' CreateDataSource (Prelude.Maybe Prelude.Text)
createDataSource_description = Lens.lens (\CreateDataSource' {description} -> description) (\s@CreateDataSource' {} a -> s {description = a} :: CreateDataSource)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataSourceResponse'
            Prelude.<$> (x Core..?> "dataSource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataSource

instance Prelude.NFData CreateDataSource

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
            ("elasticsearchConfig" Core..=)
              Prelude.<$> elasticsearchConfig,
            ("lambdaConfig" Core..=) Prelude.<$> lambdaConfig,
            ("dynamodbConfig" Core..=)
              Prelude.<$> dynamodbConfig,
            ("description" Core..=) Prelude.<$> description,
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

instance Prelude.NFData CreateDataSourceResponse
