{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateDataSource (..)
    , mkUpdateDataSource
    -- ** Request lenses
    , udsApiId
    , udsName
    , udsType
    , udsDescription
    , udsDynamodbConfig
    , udsElasticsearchConfig
    , udsHttpConfig
    , udsLambdaConfig
    , udsRelationalDatabaseConfig
    , udsServiceRoleArn

    -- * Destructuring the response
    , UpdateDataSourceResponse (..)
    , mkUpdateDataSourceResponse
    -- ** Response lenses
    , udsrrsDataSource
    , udsrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDataSource' smart constructor.
data UpdateDataSource = UpdateDataSource'
  { apiId :: Core.Text
    -- ^ The API ID.
  , name :: Types.ResourceName
    -- ^ The new name for the data source.
  , type' :: Types.DataSourceType
    -- ^ The new data source type.
  , description :: Core.Maybe Core.Text
    -- ^ The new description for the data source.
  , dynamodbConfig :: Core.Maybe Types.DynamodbDataSourceConfig
    -- ^ The new Amazon DynamoDB configuration.
  , elasticsearchConfig :: Core.Maybe Types.ElasticsearchDataSourceConfig
    -- ^ The new Elasticsearch Service configuration.
  , httpConfig :: Core.Maybe Types.HttpDataSourceConfig
    -- ^ The new HTTP endpoint configuration.
  , lambdaConfig :: Core.Maybe Types.LambdaDataSourceConfig
    -- ^ The new AWS Lambda configuration.
  , relationalDatabaseConfig :: Core.Maybe Types.RelationalDatabaseDataSourceConfig
    -- ^ The new relational database configuration.
  , serviceRoleArn :: Core.Maybe Core.Text
    -- ^ The new service role ARN for the data source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDataSource' value with any optional fields omitted.
mkUpdateDataSource
    :: Core.Text -- ^ 'apiId'
    -> Types.ResourceName -- ^ 'name'
    -> Types.DataSourceType -- ^ 'type\''
    -> UpdateDataSource
mkUpdateDataSource apiId name type'
  = UpdateDataSource'{apiId, name, type', description = Core.Nothing,
                      dynamodbConfig = Core.Nothing, elasticsearchConfig = Core.Nothing,
                      httpConfig = Core.Nothing, lambdaConfig = Core.Nothing,
                      relationalDatabaseConfig = Core.Nothing,
                      serviceRoleArn = Core.Nothing}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsApiId :: Lens.Lens' UpdateDataSource Core.Text
udsApiId = Lens.field @"apiId"
{-# INLINEABLE udsApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The new name for the data source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsName :: Lens.Lens' UpdateDataSource Types.ResourceName
udsName = Lens.field @"name"
{-# INLINEABLE udsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The new data source type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsType :: Lens.Lens' UpdateDataSource Types.DataSourceType
udsType = Lens.field @"type'"
{-# INLINEABLE udsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The new description for the data source.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsDescription :: Lens.Lens' UpdateDataSource (Core.Maybe Core.Text)
udsDescription = Lens.field @"description"
{-# INLINEABLE udsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The new Amazon DynamoDB configuration.
--
-- /Note:/ Consider using 'dynamodbConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsDynamodbConfig :: Lens.Lens' UpdateDataSource (Core.Maybe Types.DynamodbDataSourceConfig)
udsDynamodbConfig = Lens.field @"dynamodbConfig"
{-# INLINEABLE udsDynamodbConfig #-}
{-# DEPRECATED dynamodbConfig "Use generic-lens or generic-optics with 'dynamodbConfig' instead"  #-}

-- | The new Elasticsearch Service configuration.
--
-- /Note:/ Consider using 'elasticsearchConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsElasticsearchConfig :: Lens.Lens' UpdateDataSource (Core.Maybe Types.ElasticsearchDataSourceConfig)
udsElasticsearchConfig = Lens.field @"elasticsearchConfig"
{-# INLINEABLE udsElasticsearchConfig #-}
{-# DEPRECATED elasticsearchConfig "Use generic-lens or generic-optics with 'elasticsearchConfig' instead"  #-}

-- | The new HTTP endpoint configuration.
--
-- /Note:/ Consider using 'httpConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsHttpConfig :: Lens.Lens' UpdateDataSource (Core.Maybe Types.HttpDataSourceConfig)
udsHttpConfig = Lens.field @"httpConfig"
{-# INLINEABLE udsHttpConfig #-}
{-# DEPRECATED httpConfig "Use generic-lens or generic-optics with 'httpConfig' instead"  #-}

-- | The new AWS Lambda configuration.
--
-- /Note:/ Consider using 'lambdaConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsLambdaConfig :: Lens.Lens' UpdateDataSource (Core.Maybe Types.LambdaDataSourceConfig)
udsLambdaConfig = Lens.field @"lambdaConfig"
{-# INLINEABLE udsLambdaConfig #-}
{-# DEPRECATED lambdaConfig "Use generic-lens or generic-optics with 'lambdaConfig' instead"  #-}

-- | The new relational database configuration.
--
-- /Note:/ Consider using 'relationalDatabaseConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsRelationalDatabaseConfig :: Lens.Lens' UpdateDataSource (Core.Maybe Types.RelationalDatabaseDataSourceConfig)
udsRelationalDatabaseConfig = Lens.field @"relationalDatabaseConfig"
{-# INLINEABLE udsRelationalDatabaseConfig #-}
{-# DEPRECATED relationalDatabaseConfig "Use generic-lens or generic-optics with 'relationalDatabaseConfig' instead"  #-}

-- | The new service role ARN for the data source.
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsServiceRoleArn :: Lens.Lens' UpdateDataSource (Core.Maybe Core.Text)
udsServiceRoleArn = Lens.field @"serviceRoleArn"
{-# INLINEABLE udsServiceRoleArn #-}
{-# DEPRECATED serviceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead"  #-}

instance Core.ToQuery UpdateDataSource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDataSource where
        toHeaders UpdateDataSource{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateDataSource where
        toJSON UpdateDataSource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("type" Core..= type'),
                  ("description" Core..=) Core.<$> description,
                  ("dynamodbConfig" Core..=) Core.<$> dynamodbConfig,
                  ("elasticsearchConfig" Core..=) Core.<$> elasticsearchConfig,
                  ("httpConfig" Core..=) Core.<$> httpConfig,
                  ("lambdaConfig" Core..=) Core.<$> lambdaConfig,
                  ("relationalDatabaseConfig" Core..=) Core.<$>
                    relationalDatabaseConfig,
                  ("serviceRoleArn" Core..=) Core.<$> serviceRoleArn])

instance Core.AWSRequest UpdateDataSource where
        type Rs UpdateDataSource = UpdateDataSourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/datasources/"
                             Core.<> Core.toText name,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateDataSourceResponse' Core.<$>
                   (x Core..:? "dataSource") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateDataSourceResponse' smart constructor.
data UpdateDataSourceResponse = UpdateDataSourceResponse'
  { dataSource :: Core.Maybe Types.DataSource
    -- ^ The updated @DataSource@ object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDataSourceResponse' value with any optional fields omitted.
mkUpdateDataSourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateDataSourceResponse
mkUpdateDataSourceResponse responseStatus
  = UpdateDataSourceResponse'{dataSource = Core.Nothing,
                              responseStatus}

-- | The updated @DataSource@ object.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrrsDataSource :: Lens.Lens' UpdateDataSourceResponse (Core.Maybe Types.DataSource)
udsrrsDataSource = Lens.field @"dataSource"
{-# INLINEABLE udsrrsDataSource #-}
{-# DEPRECATED dataSource "Use generic-lens or generic-optics with 'dataSource' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrrsResponseStatus :: Lens.Lens' UpdateDataSourceResponse Core.Int
udsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE udsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
