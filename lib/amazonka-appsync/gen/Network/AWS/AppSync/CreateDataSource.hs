{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.CreateDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @DataSource@ object.
module Network.AWS.AppSync.CreateDataSource
  ( -- * Creating a request
    CreateDataSource (..),
    mkCreateDataSource,

    -- ** Request lenses
    cdsServiceRoleARN,
    cdsRelationalDatabaseConfig,
    cdsDynamodbConfig,
    cdsHttpConfig,
    cdsLambdaConfig,
    cdsDescription,
    cdsElasticsearchConfig,
    cdsApiId,
    cdsName,
    cdsType,

    -- * Destructuring the response
    CreateDataSourceResponse (..),
    mkCreateDataSourceResponse,

    -- ** Response lenses
    cdsrsDataSource,
    cdsrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDataSource' smart constructor.
data CreateDataSource = CreateDataSource'
  { serviceRoleARN ::
      Lude.Maybe Lude.Text,
    relationalDatabaseConfig ::
      Lude.Maybe RelationalDatabaseDataSourceConfig,
    dynamodbConfig :: Lude.Maybe DynamodbDataSourceConfig,
    httpConfig :: Lude.Maybe HTTPDataSourceConfig,
    lambdaConfig :: Lude.Maybe LambdaDataSourceConfig,
    description :: Lude.Maybe Lude.Text,
    elasticsearchConfig ::
      Lude.Maybe ElasticsearchDataSourceConfig,
    apiId :: Lude.Text,
    name :: Lude.Text,
    type' :: DataSourceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDataSource' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID for the GraphQL API for the @DataSource@ .
-- * 'description' - A description of the @DataSource@ .
-- * 'dynamodbConfig' - Amazon DynamoDB settings.
-- * 'elasticsearchConfig' - Amazon Elasticsearch Service settings.
-- * 'httpConfig' - HTTP endpoint settings.
-- * 'lambdaConfig' - AWS Lambda settings.
-- * 'name' - A user-supplied name for the @DataSource@ .
-- * 'relationalDatabaseConfig' - Relational database settings.
-- * 'serviceRoleARN' - The AWS IAM service role ARN for the data source. The system assumes this role when accessing the data source.
-- * 'type'' - The type of the @DataSource@ .
mkCreateDataSource ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  DataSourceType ->
  CreateDataSource
mkCreateDataSource pApiId_ pName_ pType_ =
  CreateDataSource'
    { serviceRoleARN = Lude.Nothing,
      relationalDatabaseConfig = Lude.Nothing,
      dynamodbConfig = Lude.Nothing,
      httpConfig = Lude.Nothing,
      lambdaConfig = Lude.Nothing,
      description = Lude.Nothing,
      elasticsearchConfig = Lude.Nothing,
      apiId = pApiId_,
      name = pName_,
      type' = pType_
    }

-- | The AWS IAM service role ARN for the data source. The system assumes this role when accessing the data source.
--
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsServiceRoleARN :: Lens.Lens' CreateDataSource (Lude.Maybe Lude.Text)
cdsServiceRoleARN = Lens.lens (serviceRoleARN :: CreateDataSource -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleARN = a} :: CreateDataSource)
{-# DEPRECATED cdsServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | Relational database settings.
--
-- /Note:/ Consider using 'relationalDatabaseConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsRelationalDatabaseConfig :: Lens.Lens' CreateDataSource (Lude.Maybe RelationalDatabaseDataSourceConfig)
cdsRelationalDatabaseConfig = Lens.lens (relationalDatabaseConfig :: CreateDataSource -> Lude.Maybe RelationalDatabaseDataSourceConfig) (\s a -> s {relationalDatabaseConfig = a} :: CreateDataSource)
{-# DEPRECATED cdsRelationalDatabaseConfig "Use generic-lens or generic-optics with 'relationalDatabaseConfig' instead." #-}

-- | Amazon DynamoDB settings.
--
-- /Note:/ Consider using 'dynamodbConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDynamodbConfig :: Lens.Lens' CreateDataSource (Lude.Maybe DynamodbDataSourceConfig)
cdsDynamodbConfig = Lens.lens (dynamodbConfig :: CreateDataSource -> Lude.Maybe DynamodbDataSourceConfig) (\s a -> s {dynamodbConfig = a} :: CreateDataSource)
{-# DEPRECATED cdsDynamodbConfig "Use generic-lens or generic-optics with 'dynamodbConfig' instead." #-}

-- | HTTP endpoint settings.
--
-- /Note:/ Consider using 'httpConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsHttpConfig :: Lens.Lens' CreateDataSource (Lude.Maybe HTTPDataSourceConfig)
cdsHttpConfig = Lens.lens (httpConfig :: CreateDataSource -> Lude.Maybe HTTPDataSourceConfig) (\s a -> s {httpConfig = a} :: CreateDataSource)
{-# DEPRECATED cdsHttpConfig "Use generic-lens or generic-optics with 'httpConfig' instead." #-}

-- | AWS Lambda settings.
--
-- /Note:/ Consider using 'lambdaConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsLambdaConfig :: Lens.Lens' CreateDataSource (Lude.Maybe LambdaDataSourceConfig)
cdsLambdaConfig = Lens.lens (lambdaConfig :: CreateDataSource -> Lude.Maybe LambdaDataSourceConfig) (\s a -> s {lambdaConfig = a} :: CreateDataSource)
{-# DEPRECATED cdsLambdaConfig "Use generic-lens or generic-optics with 'lambdaConfig' instead." #-}

-- | A description of the @DataSource@ .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDescription :: Lens.Lens' CreateDataSource (Lude.Maybe Lude.Text)
cdsDescription = Lens.lens (description :: CreateDataSource -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateDataSource)
{-# DEPRECATED cdsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Amazon Elasticsearch Service settings.
--
-- /Note:/ Consider using 'elasticsearchConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsElasticsearchConfig :: Lens.Lens' CreateDataSource (Lude.Maybe ElasticsearchDataSourceConfig)
cdsElasticsearchConfig = Lens.lens (elasticsearchConfig :: CreateDataSource -> Lude.Maybe ElasticsearchDataSourceConfig) (\s a -> s {elasticsearchConfig = a} :: CreateDataSource)
{-# DEPRECATED cdsElasticsearchConfig "Use generic-lens or generic-optics with 'elasticsearchConfig' instead." #-}

-- | The API ID for the GraphQL API for the @DataSource@ .
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsApiId :: Lens.Lens' CreateDataSource Lude.Text
cdsApiId = Lens.lens (apiId :: CreateDataSource -> Lude.Text) (\s a -> s {apiId = a} :: CreateDataSource)
{-# DEPRECATED cdsApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | A user-supplied name for the @DataSource@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsName :: Lens.Lens' CreateDataSource Lude.Text
cdsName = Lens.lens (name :: CreateDataSource -> Lude.Text) (\s a -> s {name = a} :: CreateDataSource)
{-# DEPRECATED cdsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the @DataSource@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsType :: Lens.Lens' CreateDataSource DataSourceType
cdsType = Lens.lens (type' :: CreateDataSource -> DataSourceType) (\s a -> s {type' = a} :: CreateDataSource)
{-# DEPRECATED cdsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest CreateDataSource where
  type Rs CreateDataSource = CreateDataSourceResponse
  request = Req.postJSON appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDataSourceResponse'
            Lude.<$> (x Lude..?> "dataSource") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDataSource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDataSource where
  toJSON CreateDataSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("serviceRoleArn" Lude..=) Lude.<$> serviceRoleARN,
            ("relationalDatabaseConfig" Lude..=)
              Lude.<$> relationalDatabaseConfig,
            ("dynamodbConfig" Lude..=) Lude.<$> dynamodbConfig,
            ("httpConfig" Lude..=) Lude.<$> httpConfig,
            ("lambdaConfig" Lude..=) Lude.<$> lambdaConfig,
            ("description" Lude..=) Lude.<$> description,
            ("elasticsearchConfig" Lude..=) Lude.<$> elasticsearchConfig,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("type" Lude..= type')
          ]
      )

instance Lude.ToPath CreateDataSource where
  toPath CreateDataSource' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId, "/datasources"]

instance Lude.ToQuery CreateDataSource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDataSourceResponse' smart constructor.
data CreateDataSourceResponse = CreateDataSourceResponse'
  { dataSource ::
      Lude.Maybe DataSource,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDataSourceResponse' with the minimum fields required to make a request.
--
-- * 'dataSource' - The @DataSource@ object.
-- * 'responseStatus' - The response status code.
mkCreateDataSourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDataSourceResponse
mkCreateDataSourceResponse pResponseStatus_ =
  CreateDataSourceResponse'
    { dataSource = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @DataSource@ object.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrsDataSource :: Lens.Lens' CreateDataSourceResponse (Lude.Maybe DataSource)
cdsrsDataSource = Lens.lens (dataSource :: CreateDataSourceResponse -> Lude.Maybe DataSource) (\s a -> s {dataSource = a} :: CreateDataSourceResponse)
{-# DEPRECATED cdsrsDataSource "Use generic-lens or generic-optics with 'dataSource' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrsResponseStatus :: Lens.Lens' CreateDataSourceResponse Lude.Int
cdsrsResponseStatus = Lens.lens (responseStatus :: CreateDataSourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDataSourceResponse)
{-# DEPRECATED cdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
