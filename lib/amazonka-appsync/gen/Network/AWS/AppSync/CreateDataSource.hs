{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    cdsApiId,
    cdsName,
    cdsType,
    cdsDescription,
    cdsDynamodbConfig,
    cdsElasticsearchConfig,
    cdsHttpConfig,
    cdsLambdaConfig,
    cdsRelationalDatabaseConfig,
    cdsServiceRoleArn,

    -- * Destructuring the response
    CreateDataSourceResponse (..),
    mkCreateDataSourceResponse,

    -- ** Response lenses
    cdsrrsDataSource,
    cdsrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDataSource' smart constructor.
data CreateDataSource = CreateDataSource'
  { -- | The API ID for the GraphQL API for the @DataSource@ .
    apiId :: Types.String,
    -- | A user-supplied name for the @DataSource@ .
    name :: Types.ResourceName,
    -- | The type of the @DataSource@ .
    type' :: Types.DataSourceType,
    -- | A description of the @DataSource@ .
    description :: Core.Maybe Types.String,
    -- | Amazon DynamoDB settings.
    dynamodbConfig :: Core.Maybe Types.DynamodbDataSourceConfig,
    -- | Amazon Elasticsearch Service settings.
    elasticsearchConfig :: Core.Maybe Types.ElasticsearchDataSourceConfig,
    -- | HTTP endpoint settings.
    httpConfig :: Core.Maybe Types.HttpDataSourceConfig,
    -- | AWS Lambda settings.
    lambdaConfig :: Core.Maybe Types.LambdaDataSourceConfig,
    -- | Relational database settings.
    relationalDatabaseConfig :: Core.Maybe Types.RelationalDatabaseDataSourceConfig,
    -- | The AWS IAM service role ARN for the data source. The system assumes this role when accessing the data source.
    serviceRoleArn :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDataSource' value with any optional fields omitted.
mkCreateDataSource ::
  -- | 'apiId'
  Types.String ->
  -- | 'name'
  Types.ResourceName ->
  -- | 'type\''
  Types.DataSourceType ->
  CreateDataSource
mkCreateDataSource apiId name type' =
  CreateDataSource'
    { apiId,
      name,
      type',
      description = Core.Nothing,
      dynamodbConfig = Core.Nothing,
      elasticsearchConfig = Core.Nothing,
      httpConfig = Core.Nothing,
      lambdaConfig = Core.Nothing,
      relationalDatabaseConfig = Core.Nothing,
      serviceRoleArn = Core.Nothing
    }

-- | The API ID for the GraphQL API for the @DataSource@ .
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsApiId :: Lens.Lens' CreateDataSource Types.String
cdsApiId = Lens.field @"apiId"
{-# DEPRECATED cdsApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | A user-supplied name for the @DataSource@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsName :: Lens.Lens' CreateDataSource Types.ResourceName
cdsName = Lens.field @"name"
{-# DEPRECATED cdsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the @DataSource@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsType :: Lens.Lens' CreateDataSource Types.DataSourceType
cdsType = Lens.field @"type'"
{-# DEPRECATED cdsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A description of the @DataSource@ .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDescription :: Lens.Lens' CreateDataSource (Core.Maybe Types.String)
cdsDescription = Lens.field @"description"
{-# DEPRECATED cdsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Amazon DynamoDB settings.
--
-- /Note:/ Consider using 'dynamodbConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDynamodbConfig :: Lens.Lens' CreateDataSource (Core.Maybe Types.DynamodbDataSourceConfig)
cdsDynamodbConfig = Lens.field @"dynamodbConfig"
{-# DEPRECATED cdsDynamodbConfig "Use generic-lens or generic-optics with 'dynamodbConfig' instead." #-}

-- | Amazon Elasticsearch Service settings.
--
-- /Note:/ Consider using 'elasticsearchConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsElasticsearchConfig :: Lens.Lens' CreateDataSource (Core.Maybe Types.ElasticsearchDataSourceConfig)
cdsElasticsearchConfig = Lens.field @"elasticsearchConfig"
{-# DEPRECATED cdsElasticsearchConfig "Use generic-lens or generic-optics with 'elasticsearchConfig' instead." #-}

-- | HTTP endpoint settings.
--
-- /Note:/ Consider using 'httpConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsHttpConfig :: Lens.Lens' CreateDataSource (Core.Maybe Types.HttpDataSourceConfig)
cdsHttpConfig = Lens.field @"httpConfig"
{-# DEPRECATED cdsHttpConfig "Use generic-lens or generic-optics with 'httpConfig' instead." #-}

-- | AWS Lambda settings.
--
-- /Note:/ Consider using 'lambdaConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsLambdaConfig :: Lens.Lens' CreateDataSource (Core.Maybe Types.LambdaDataSourceConfig)
cdsLambdaConfig = Lens.field @"lambdaConfig"
{-# DEPRECATED cdsLambdaConfig "Use generic-lens or generic-optics with 'lambdaConfig' instead." #-}

-- | Relational database settings.
--
-- /Note:/ Consider using 'relationalDatabaseConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsRelationalDatabaseConfig :: Lens.Lens' CreateDataSource (Core.Maybe Types.RelationalDatabaseDataSourceConfig)
cdsRelationalDatabaseConfig = Lens.field @"relationalDatabaseConfig"
{-# DEPRECATED cdsRelationalDatabaseConfig "Use generic-lens or generic-optics with 'relationalDatabaseConfig' instead." #-}

-- | The AWS IAM service role ARN for the data source. The system assumes this role when accessing the data source.
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsServiceRoleArn :: Lens.Lens' CreateDataSource (Core.Maybe Types.String)
cdsServiceRoleArn = Lens.field @"serviceRoleArn"
{-# DEPRECATED cdsServiceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead." #-}

instance Core.FromJSON CreateDataSource where
  toJSON CreateDataSource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("type" Core..= type'),
            ("description" Core..=) Core.<$> description,
            ("dynamodbConfig" Core..=) Core.<$> dynamodbConfig,
            ("elasticsearchConfig" Core..=) Core.<$> elasticsearchConfig,
            ("httpConfig" Core..=) Core.<$> httpConfig,
            ("lambdaConfig" Core..=) Core.<$> lambdaConfig,
            ("relationalDatabaseConfig" Core..=)
              Core.<$> relationalDatabaseConfig,
            ("serviceRoleArn" Core..=) Core.<$> serviceRoleArn
          ]
      )

instance Core.AWSRequest CreateDataSource where
  type Rs CreateDataSource = CreateDataSourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/v1/apis/" Core.<> (Core.toText apiId) Core.<> ("/datasources")),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataSourceResponse'
            Core.<$> (x Core..:? "dataSource") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDataSourceResponse' smart constructor.
data CreateDataSourceResponse = CreateDataSourceResponse'
  { -- | The @DataSource@ object.
    dataSource :: Core.Maybe Types.DataSource,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDataSourceResponse' value with any optional fields omitted.
mkCreateDataSourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDataSourceResponse
mkCreateDataSourceResponse responseStatus =
  CreateDataSourceResponse'
    { dataSource = Core.Nothing,
      responseStatus
    }

-- | The @DataSource@ object.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrrsDataSource :: Lens.Lens' CreateDataSourceResponse (Core.Maybe Types.DataSource)
cdsrrsDataSource = Lens.field @"dataSource"
{-# DEPRECATED cdsrrsDataSource "Use generic-lens or generic-optics with 'dataSource' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrrsResponseStatus :: Lens.Lens' CreateDataSourceResponse Core.Int
cdsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
