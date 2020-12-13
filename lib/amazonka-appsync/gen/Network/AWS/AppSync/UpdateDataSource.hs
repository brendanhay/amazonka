{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.UpdateDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @DataSource@ object.
module Network.AWS.AppSync.UpdateDataSource
  ( -- * Creating a request
    UpdateDataSource (..),
    mkUpdateDataSource,

    -- ** Request lenses
    udsServiceRoleARN,
    udsRelationalDatabaseConfig,
    udsApiId,
    udsDynamodbConfig,
    udsName,
    udsHttpConfig,
    udsLambdaConfig,
    udsType,
    udsDescription,
    udsElasticsearchConfig,

    -- * Destructuring the response
    UpdateDataSourceResponse (..),
    mkUpdateDataSourceResponse,

    -- ** Response lenses
    udsrsDataSource,
    udsrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDataSource' smart constructor.
data UpdateDataSource = UpdateDataSource'
  { -- | The new service role ARN for the data source.
    serviceRoleARN :: Lude.Maybe Lude.Text,
    -- | The new relational database configuration.
    relationalDatabaseConfig :: Lude.Maybe RelationalDatabaseDataSourceConfig,
    -- | The API ID.
    apiId :: Lude.Text,
    -- | The new Amazon DynamoDB configuration.
    dynamodbConfig :: Lude.Maybe DynamodbDataSourceConfig,
    -- | The new name for the data source.
    name :: Lude.Text,
    -- | The new HTTP endpoint configuration.
    httpConfig :: Lude.Maybe HTTPDataSourceConfig,
    -- | The new AWS Lambda configuration.
    lambdaConfig :: Lude.Maybe LambdaDataSourceConfig,
    -- | The new data source type.
    type' :: DataSourceType,
    -- | The new description for the data source.
    description :: Lude.Maybe Lude.Text,
    -- | The new Elasticsearch Service configuration.
    elasticsearchConfig :: Lude.Maybe ElasticsearchDataSourceConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDataSource' with the minimum fields required to make a request.
--
-- * 'serviceRoleARN' - The new service role ARN for the data source.
-- * 'relationalDatabaseConfig' - The new relational database configuration.
-- * 'apiId' - The API ID.
-- * 'dynamodbConfig' - The new Amazon DynamoDB configuration.
-- * 'name' - The new name for the data source.
-- * 'httpConfig' - The new HTTP endpoint configuration.
-- * 'lambdaConfig' - The new AWS Lambda configuration.
-- * 'type'' - The new data source type.
-- * 'description' - The new description for the data source.
-- * 'elasticsearchConfig' - The new Elasticsearch Service configuration.
mkUpdateDataSource ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  DataSourceType ->
  UpdateDataSource
mkUpdateDataSource pApiId_ pName_ pType_ =
  UpdateDataSource'
    { serviceRoleARN = Lude.Nothing,
      relationalDatabaseConfig = Lude.Nothing,
      apiId = pApiId_,
      dynamodbConfig = Lude.Nothing,
      name = pName_,
      httpConfig = Lude.Nothing,
      lambdaConfig = Lude.Nothing,
      type' = pType_,
      description = Lude.Nothing,
      elasticsearchConfig = Lude.Nothing
    }

-- | The new service role ARN for the data source.
--
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsServiceRoleARN :: Lens.Lens' UpdateDataSource (Lude.Maybe Lude.Text)
udsServiceRoleARN = Lens.lens (serviceRoleARN :: UpdateDataSource -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleARN = a} :: UpdateDataSource)
{-# DEPRECATED udsServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | The new relational database configuration.
--
-- /Note:/ Consider using 'relationalDatabaseConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsRelationalDatabaseConfig :: Lens.Lens' UpdateDataSource (Lude.Maybe RelationalDatabaseDataSourceConfig)
udsRelationalDatabaseConfig = Lens.lens (relationalDatabaseConfig :: UpdateDataSource -> Lude.Maybe RelationalDatabaseDataSourceConfig) (\s a -> s {relationalDatabaseConfig = a} :: UpdateDataSource)
{-# DEPRECATED udsRelationalDatabaseConfig "Use generic-lens or generic-optics with 'relationalDatabaseConfig' instead." #-}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsApiId :: Lens.Lens' UpdateDataSource Lude.Text
udsApiId = Lens.lens (apiId :: UpdateDataSource -> Lude.Text) (\s a -> s {apiId = a} :: UpdateDataSource)
{-# DEPRECATED udsApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The new Amazon DynamoDB configuration.
--
-- /Note:/ Consider using 'dynamodbConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsDynamodbConfig :: Lens.Lens' UpdateDataSource (Lude.Maybe DynamodbDataSourceConfig)
udsDynamodbConfig = Lens.lens (dynamodbConfig :: UpdateDataSource -> Lude.Maybe DynamodbDataSourceConfig) (\s a -> s {dynamodbConfig = a} :: UpdateDataSource)
{-# DEPRECATED udsDynamodbConfig "Use generic-lens or generic-optics with 'dynamodbConfig' instead." #-}

-- | The new name for the data source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsName :: Lens.Lens' UpdateDataSource Lude.Text
udsName = Lens.lens (name :: UpdateDataSource -> Lude.Text) (\s a -> s {name = a} :: UpdateDataSource)
{-# DEPRECATED udsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The new HTTP endpoint configuration.
--
-- /Note:/ Consider using 'httpConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsHttpConfig :: Lens.Lens' UpdateDataSource (Lude.Maybe HTTPDataSourceConfig)
udsHttpConfig = Lens.lens (httpConfig :: UpdateDataSource -> Lude.Maybe HTTPDataSourceConfig) (\s a -> s {httpConfig = a} :: UpdateDataSource)
{-# DEPRECATED udsHttpConfig "Use generic-lens or generic-optics with 'httpConfig' instead." #-}

-- | The new AWS Lambda configuration.
--
-- /Note:/ Consider using 'lambdaConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsLambdaConfig :: Lens.Lens' UpdateDataSource (Lude.Maybe LambdaDataSourceConfig)
udsLambdaConfig = Lens.lens (lambdaConfig :: UpdateDataSource -> Lude.Maybe LambdaDataSourceConfig) (\s a -> s {lambdaConfig = a} :: UpdateDataSource)
{-# DEPRECATED udsLambdaConfig "Use generic-lens or generic-optics with 'lambdaConfig' instead." #-}

-- | The new data source type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsType :: Lens.Lens' UpdateDataSource DataSourceType
udsType = Lens.lens (type' :: UpdateDataSource -> DataSourceType) (\s a -> s {type' = a} :: UpdateDataSource)
{-# DEPRECATED udsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The new description for the data source.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsDescription :: Lens.Lens' UpdateDataSource (Lude.Maybe Lude.Text)
udsDescription = Lens.lens (description :: UpdateDataSource -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateDataSource)
{-# DEPRECATED udsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The new Elasticsearch Service configuration.
--
-- /Note:/ Consider using 'elasticsearchConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsElasticsearchConfig :: Lens.Lens' UpdateDataSource (Lude.Maybe ElasticsearchDataSourceConfig)
udsElasticsearchConfig = Lens.lens (elasticsearchConfig :: UpdateDataSource -> Lude.Maybe ElasticsearchDataSourceConfig) (\s a -> s {elasticsearchConfig = a} :: UpdateDataSource)
{-# DEPRECATED udsElasticsearchConfig "Use generic-lens or generic-optics with 'elasticsearchConfig' instead." #-}

instance Lude.AWSRequest UpdateDataSource where
  type Rs UpdateDataSource = UpdateDataSourceResponse
  request = Req.postJSON appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDataSourceResponse'
            Lude.<$> (x Lude..?> "dataSource") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDataSource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDataSource where
  toJSON UpdateDataSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("serviceRoleArn" Lude..=) Lude.<$> serviceRoleARN,
            ("relationalDatabaseConfig" Lude..=)
              Lude.<$> relationalDatabaseConfig,
            ("dynamodbConfig" Lude..=) Lude.<$> dynamodbConfig,
            ("httpConfig" Lude..=) Lude.<$> httpConfig,
            ("lambdaConfig" Lude..=) Lude.<$> lambdaConfig,
            Lude.Just ("type" Lude..= type'),
            ("description" Lude..=) Lude.<$> description,
            ("elasticsearchConfig" Lude..=) Lude.<$> elasticsearchConfig
          ]
      )

instance Lude.ToPath UpdateDataSource where
  toPath UpdateDataSource' {..} =
    Lude.mconcat
      ["/v1/apis/", Lude.toBS apiId, "/datasources/", Lude.toBS name]

instance Lude.ToQuery UpdateDataSource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDataSourceResponse' smart constructor.
data UpdateDataSourceResponse = UpdateDataSourceResponse'
  { -- | The updated @DataSource@ object.
    dataSource :: Lude.Maybe DataSource,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDataSourceResponse' with the minimum fields required to make a request.
--
-- * 'dataSource' - The updated @DataSource@ object.
-- * 'responseStatus' - The response status code.
mkUpdateDataSourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDataSourceResponse
mkUpdateDataSourceResponse pResponseStatus_ =
  UpdateDataSourceResponse'
    { dataSource = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated @DataSource@ object.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrsDataSource :: Lens.Lens' UpdateDataSourceResponse (Lude.Maybe DataSource)
udsrsDataSource = Lens.lens (dataSource :: UpdateDataSourceResponse -> Lude.Maybe DataSource) (\s a -> s {dataSource = a} :: UpdateDataSourceResponse)
{-# DEPRECATED udsrsDataSource "Use generic-lens or generic-optics with 'dataSource' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrsResponseStatus :: Lens.Lens' UpdateDataSourceResponse Lude.Int
udsrsResponseStatus = Lens.lens (responseStatus :: UpdateDataSourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDataSourceResponse)
{-# DEPRECATED udsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
