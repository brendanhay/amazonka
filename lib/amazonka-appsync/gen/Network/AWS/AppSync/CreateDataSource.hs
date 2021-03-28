{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateDataSource (..)
    , mkCreateDataSource
    -- ** Request lenses
    , cdsApiId
    , cdsName
    , cdsType
    , cdsDescription
    , cdsDynamodbConfig
    , cdsElasticsearchConfig
    , cdsHttpConfig
    , cdsLambdaConfig
    , cdsRelationalDatabaseConfig
    , cdsServiceRoleArn

    -- * Destructuring the response
    , CreateDataSourceResponse (..)
    , mkCreateDataSourceResponse
    -- ** Response lenses
    , cdsrrsDataSource
    , cdsrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDataSource' smart constructor.
data CreateDataSource = CreateDataSource'
  { apiId :: Core.Text
    -- ^ The API ID for the GraphQL API for the @DataSource@ .
  , name :: Types.ResourceName
    -- ^ A user-supplied name for the @DataSource@ .
  , type' :: Types.DataSourceType
    -- ^ The type of the @DataSource@ .
  , description :: Core.Maybe Core.Text
    -- ^ A description of the @DataSource@ .
  , dynamodbConfig :: Core.Maybe Types.DynamodbDataSourceConfig
    -- ^ Amazon DynamoDB settings.
  , elasticsearchConfig :: Core.Maybe Types.ElasticsearchDataSourceConfig
    -- ^ Amazon Elasticsearch Service settings.
  , httpConfig :: Core.Maybe Types.HttpDataSourceConfig
    -- ^ HTTP endpoint settings.
  , lambdaConfig :: Core.Maybe Types.LambdaDataSourceConfig
    -- ^ AWS Lambda settings.
  , relationalDatabaseConfig :: Core.Maybe Types.RelationalDatabaseDataSourceConfig
    -- ^ Relational database settings.
  , serviceRoleArn :: Core.Maybe Core.Text
    -- ^ The AWS IAM service role ARN for the data source. The system assumes this role when accessing the data source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDataSource' value with any optional fields omitted.
mkCreateDataSource
    :: Core.Text -- ^ 'apiId'
    -> Types.ResourceName -- ^ 'name'
    -> Types.DataSourceType -- ^ 'type\''
    -> CreateDataSource
mkCreateDataSource apiId name type'
  = CreateDataSource'{apiId, name, type', description = Core.Nothing,
                      dynamodbConfig = Core.Nothing, elasticsearchConfig = Core.Nothing,
                      httpConfig = Core.Nothing, lambdaConfig = Core.Nothing,
                      relationalDatabaseConfig = Core.Nothing,
                      serviceRoleArn = Core.Nothing}

-- | The API ID for the GraphQL API for the @DataSource@ .
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsApiId :: Lens.Lens' CreateDataSource Core.Text
cdsApiId = Lens.field @"apiId"
{-# INLINEABLE cdsApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | A user-supplied name for the @DataSource@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsName :: Lens.Lens' CreateDataSource Types.ResourceName
cdsName = Lens.field @"name"
{-# INLINEABLE cdsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of the @DataSource@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsType :: Lens.Lens' CreateDataSource Types.DataSourceType
cdsType = Lens.field @"type'"
{-# INLINEABLE cdsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | A description of the @DataSource@ .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDescription :: Lens.Lens' CreateDataSource (Core.Maybe Core.Text)
cdsDescription = Lens.field @"description"
{-# INLINEABLE cdsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Amazon DynamoDB settings.
--
-- /Note:/ Consider using 'dynamodbConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDynamodbConfig :: Lens.Lens' CreateDataSource (Core.Maybe Types.DynamodbDataSourceConfig)
cdsDynamodbConfig = Lens.field @"dynamodbConfig"
{-# INLINEABLE cdsDynamodbConfig #-}
{-# DEPRECATED dynamodbConfig "Use generic-lens or generic-optics with 'dynamodbConfig' instead"  #-}

-- | Amazon Elasticsearch Service settings.
--
-- /Note:/ Consider using 'elasticsearchConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsElasticsearchConfig :: Lens.Lens' CreateDataSource (Core.Maybe Types.ElasticsearchDataSourceConfig)
cdsElasticsearchConfig = Lens.field @"elasticsearchConfig"
{-# INLINEABLE cdsElasticsearchConfig #-}
{-# DEPRECATED elasticsearchConfig "Use generic-lens or generic-optics with 'elasticsearchConfig' instead"  #-}

-- | HTTP endpoint settings.
--
-- /Note:/ Consider using 'httpConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsHttpConfig :: Lens.Lens' CreateDataSource (Core.Maybe Types.HttpDataSourceConfig)
cdsHttpConfig = Lens.field @"httpConfig"
{-# INLINEABLE cdsHttpConfig #-}
{-# DEPRECATED httpConfig "Use generic-lens or generic-optics with 'httpConfig' instead"  #-}

-- | AWS Lambda settings.
--
-- /Note:/ Consider using 'lambdaConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsLambdaConfig :: Lens.Lens' CreateDataSource (Core.Maybe Types.LambdaDataSourceConfig)
cdsLambdaConfig = Lens.field @"lambdaConfig"
{-# INLINEABLE cdsLambdaConfig #-}
{-# DEPRECATED lambdaConfig "Use generic-lens or generic-optics with 'lambdaConfig' instead"  #-}

-- | Relational database settings.
--
-- /Note:/ Consider using 'relationalDatabaseConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsRelationalDatabaseConfig :: Lens.Lens' CreateDataSource (Core.Maybe Types.RelationalDatabaseDataSourceConfig)
cdsRelationalDatabaseConfig = Lens.field @"relationalDatabaseConfig"
{-# INLINEABLE cdsRelationalDatabaseConfig #-}
{-# DEPRECATED relationalDatabaseConfig "Use generic-lens or generic-optics with 'relationalDatabaseConfig' instead"  #-}

-- | The AWS IAM service role ARN for the data source. The system assumes this role when accessing the data source.
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsServiceRoleArn :: Lens.Lens' CreateDataSource (Core.Maybe Core.Text)
cdsServiceRoleArn = Lens.field @"serviceRoleArn"
{-# INLINEABLE cdsServiceRoleArn #-}
{-# DEPRECATED serviceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead"  #-}

instance Core.ToQuery CreateDataSource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDataSource where
        toHeaders CreateDataSource{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDataSource where
        toJSON CreateDataSource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name), Core.Just ("type" Core..= type'),
                  ("description" Core..=) Core.<$> description,
                  ("dynamodbConfig" Core..=) Core.<$> dynamodbConfig,
                  ("elasticsearchConfig" Core..=) Core.<$> elasticsearchConfig,
                  ("httpConfig" Core..=) Core.<$> httpConfig,
                  ("lambdaConfig" Core..=) Core.<$> lambdaConfig,
                  ("relationalDatabaseConfig" Core..=) Core.<$>
                    relationalDatabaseConfig,
                  ("serviceRoleArn" Core..=) Core.<$> serviceRoleArn])

instance Core.AWSRequest CreateDataSource where
        type Rs CreateDataSource = CreateDataSourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/datasources",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDataSourceResponse' Core.<$>
                   (x Core..:? "dataSource") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDataSourceResponse' smart constructor.
data CreateDataSourceResponse = CreateDataSourceResponse'
  { dataSource :: Core.Maybe Types.DataSource
    -- ^ The @DataSource@ object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDataSourceResponse' value with any optional fields omitted.
mkCreateDataSourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDataSourceResponse
mkCreateDataSourceResponse responseStatus
  = CreateDataSourceResponse'{dataSource = Core.Nothing,
                              responseStatus}

-- | The @DataSource@ object.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrrsDataSource :: Lens.Lens' CreateDataSourceResponse (Core.Maybe Types.DataSource)
cdsrrsDataSource = Lens.field @"dataSource"
{-# INLINEABLE cdsrrsDataSource #-}
{-# DEPRECATED dataSource "Use generic-lens or generic-optics with 'dataSource' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrrsResponseStatus :: Lens.Lens' CreateDataSourceResponse Core.Int
cdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
