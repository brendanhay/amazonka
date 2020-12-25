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
    udsApiId,
    udsName,
    udsType,
    udsDescription,
    udsDynamodbConfig,
    udsElasticsearchConfig,
    udsHttpConfig,
    udsLambdaConfig,
    udsRelationalDatabaseConfig,
    udsServiceRoleArn,

    -- * Destructuring the response
    UpdateDataSourceResponse (..),
    mkUpdateDataSourceResponse,

    -- ** Response lenses
    udsrrsDataSource,
    udsrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDataSource' smart constructor.
data UpdateDataSource = UpdateDataSource'
  { -- | The API ID.
    apiId :: Types.String,
    -- | The new name for the data source.
    name :: Types.ResourceName,
    -- | The new data source type.
    type' :: Types.DataSourceType,
    -- | The new description for the data source.
    description :: Core.Maybe Types.String,
    -- | The new Amazon DynamoDB configuration.
    dynamodbConfig :: Core.Maybe Types.DynamodbDataSourceConfig,
    -- | The new Elasticsearch Service configuration.
    elasticsearchConfig :: Core.Maybe Types.ElasticsearchDataSourceConfig,
    -- | The new HTTP endpoint configuration.
    httpConfig :: Core.Maybe Types.HttpDataSourceConfig,
    -- | The new AWS Lambda configuration.
    lambdaConfig :: Core.Maybe Types.LambdaDataSourceConfig,
    -- | The new relational database configuration.
    relationalDatabaseConfig :: Core.Maybe Types.RelationalDatabaseDataSourceConfig,
    -- | The new service role ARN for the data source.
    serviceRoleArn :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDataSource' value with any optional fields omitted.
mkUpdateDataSource ::
  -- | 'apiId'
  Types.String ->
  -- | 'name'
  Types.ResourceName ->
  -- | 'type\''
  Types.DataSourceType ->
  UpdateDataSource
mkUpdateDataSource apiId name type' =
  UpdateDataSource'
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

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsApiId :: Lens.Lens' UpdateDataSource Types.String
udsApiId = Lens.field @"apiId"
{-# DEPRECATED udsApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The new name for the data source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsName :: Lens.Lens' UpdateDataSource Types.ResourceName
udsName = Lens.field @"name"
{-# DEPRECATED udsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The new data source type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsType :: Lens.Lens' UpdateDataSource Types.DataSourceType
udsType = Lens.field @"type'"
{-# DEPRECATED udsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The new description for the data source.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsDescription :: Lens.Lens' UpdateDataSource (Core.Maybe Types.String)
udsDescription = Lens.field @"description"
{-# DEPRECATED udsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The new Amazon DynamoDB configuration.
--
-- /Note:/ Consider using 'dynamodbConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsDynamodbConfig :: Lens.Lens' UpdateDataSource (Core.Maybe Types.DynamodbDataSourceConfig)
udsDynamodbConfig = Lens.field @"dynamodbConfig"
{-# DEPRECATED udsDynamodbConfig "Use generic-lens or generic-optics with 'dynamodbConfig' instead." #-}

-- | The new Elasticsearch Service configuration.
--
-- /Note:/ Consider using 'elasticsearchConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsElasticsearchConfig :: Lens.Lens' UpdateDataSource (Core.Maybe Types.ElasticsearchDataSourceConfig)
udsElasticsearchConfig = Lens.field @"elasticsearchConfig"
{-# DEPRECATED udsElasticsearchConfig "Use generic-lens or generic-optics with 'elasticsearchConfig' instead." #-}

-- | The new HTTP endpoint configuration.
--
-- /Note:/ Consider using 'httpConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsHttpConfig :: Lens.Lens' UpdateDataSource (Core.Maybe Types.HttpDataSourceConfig)
udsHttpConfig = Lens.field @"httpConfig"
{-# DEPRECATED udsHttpConfig "Use generic-lens or generic-optics with 'httpConfig' instead." #-}

-- | The new AWS Lambda configuration.
--
-- /Note:/ Consider using 'lambdaConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsLambdaConfig :: Lens.Lens' UpdateDataSource (Core.Maybe Types.LambdaDataSourceConfig)
udsLambdaConfig = Lens.field @"lambdaConfig"
{-# DEPRECATED udsLambdaConfig "Use generic-lens or generic-optics with 'lambdaConfig' instead." #-}

-- | The new relational database configuration.
--
-- /Note:/ Consider using 'relationalDatabaseConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsRelationalDatabaseConfig :: Lens.Lens' UpdateDataSource (Core.Maybe Types.RelationalDatabaseDataSourceConfig)
udsRelationalDatabaseConfig = Lens.field @"relationalDatabaseConfig"
{-# DEPRECATED udsRelationalDatabaseConfig "Use generic-lens or generic-optics with 'relationalDatabaseConfig' instead." #-}

-- | The new service role ARN for the data source.
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsServiceRoleArn :: Lens.Lens' UpdateDataSource (Core.Maybe Types.String)
udsServiceRoleArn = Lens.field @"serviceRoleArn"
{-# DEPRECATED udsServiceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead." #-}

instance Core.FromJSON UpdateDataSource where
  toJSON UpdateDataSource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("type" Core..= type'),
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

instance Core.AWSRequest UpdateDataSource where
  type Rs UpdateDataSource = UpdateDataSourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apis/" Core.<> (Core.toText apiId) Core.<> ("/datasources/")
                Core.<> (Core.toText name)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDataSourceResponse'
            Core.<$> (x Core..:? "dataSource") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateDataSourceResponse' smart constructor.
data UpdateDataSourceResponse = UpdateDataSourceResponse'
  { -- | The updated @DataSource@ object.
    dataSource :: Core.Maybe Types.DataSource,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDataSourceResponse' value with any optional fields omitted.
mkUpdateDataSourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateDataSourceResponse
mkUpdateDataSourceResponse responseStatus =
  UpdateDataSourceResponse'
    { dataSource = Core.Nothing,
      responseStatus
    }

-- | The updated @DataSource@ object.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrrsDataSource :: Lens.Lens' UpdateDataSourceResponse (Core.Maybe Types.DataSource)
udsrrsDataSource = Lens.field @"dataSource"
{-# DEPRECATED udsrrsDataSource "Use generic-lens or generic-optics with 'dataSource' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrrsResponseStatus :: Lens.Lens' UpdateDataSourceResponse Core.Int
udsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED udsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
