{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.DataSource
  ( DataSource (..),

    -- * Smart constructor
    mkDataSource,

    -- * Lenses
    dsDataSourceArn,
    dsDescription,
    dsDynamodbConfig,
    dsElasticsearchConfig,
    dsHttpConfig,
    dsLambdaConfig,
    dsName,
    dsRelationalDatabaseConfig,
    dsServiceRoleArn,
    dsType,
  )
where

import qualified Network.AWS.AppSync.Types.DataSourceType as Types
import qualified Network.AWS.AppSync.Types.DynamodbDataSourceConfig as Types
import qualified Network.AWS.AppSync.Types.ElasticsearchDataSourceConfig as Types
import qualified Network.AWS.AppSync.Types.HttpDataSourceConfig as Types
import qualified Network.AWS.AppSync.Types.LambdaDataSourceConfig as Types
import qualified Network.AWS.AppSync.Types.RelationalDatabaseDataSourceConfig as Types
import qualified Network.AWS.AppSync.Types.ResourceName as Types
import qualified Network.AWS.AppSync.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a data source.
--
-- /See:/ 'mkDataSource' smart constructor.
data DataSource = DataSource'
  { -- | The data source ARN.
    dataSourceArn :: Core.Maybe Types.String,
    -- | The description of the data source.
    description :: Core.Maybe Types.String,
    -- | Amazon DynamoDB settings.
    dynamodbConfig :: Core.Maybe Types.DynamodbDataSourceConfig,
    -- | Amazon Elasticsearch Service settings.
    elasticsearchConfig :: Core.Maybe Types.ElasticsearchDataSourceConfig,
    -- | HTTP endpoint settings.
    httpConfig :: Core.Maybe Types.HttpDataSourceConfig,
    -- | AWS Lambda settings.
    lambdaConfig :: Core.Maybe Types.LambdaDataSourceConfig,
    -- | The name of the data source.
    name :: Core.Maybe Types.ResourceName,
    -- | Relational database settings.
    relationalDatabaseConfig :: Core.Maybe Types.RelationalDatabaseDataSourceConfig,
    -- | The AWS IAM service role ARN for the data source. The system assumes this role when accessing the data source.
    serviceRoleArn :: Core.Maybe Types.String,
    -- | The type of the data source.
    --
    --
    --     * __AMAZON_DYNAMODB__ : The data source is an Amazon DynamoDB table.
    --
    --
    --     * __AMAZON_ELASTICSEARCH__ : The data source is an Amazon Elasticsearch Service domain.
    --
    --
    --     * __AWS_LAMBDA__ : The data source is an AWS Lambda function.
    --
    --
    --     * __NONE__ : There is no data source. This type is used when you wish to invoke a GraphQL operation without connecting to a data source, such as performing data transformation with resolvers or triggering a subscription to be invoked from a mutation.
    --
    --
    --     * __HTTP__ : The data source is an HTTP endpoint.
    --
    --
    --     * __RELATIONAL_DATABASE__ : The data source is a relational database.
    type' :: Core.Maybe Types.DataSourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DataSource' value with any optional fields omitted.
mkDataSource ::
  DataSource
mkDataSource =
  DataSource'
    { dataSourceArn = Core.Nothing,
      description = Core.Nothing,
      dynamodbConfig = Core.Nothing,
      elasticsearchConfig = Core.Nothing,
      httpConfig = Core.Nothing,
      lambdaConfig = Core.Nothing,
      name = Core.Nothing,
      relationalDatabaseConfig = Core.Nothing,
      serviceRoleArn = Core.Nothing,
      type' = Core.Nothing
    }

-- | The data source ARN.
--
-- /Note:/ Consider using 'dataSourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDataSourceArn :: Lens.Lens' DataSource (Core.Maybe Types.String)
dsDataSourceArn = Lens.field @"dataSourceArn"
{-# DEPRECATED dsDataSourceArn "Use generic-lens or generic-optics with 'dataSourceArn' instead." #-}

-- | The description of the data source.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDescription :: Lens.Lens' DataSource (Core.Maybe Types.String)
dsDescription = Lens.field @"description"
{-# DEPRECATED dsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Amazon DynamoDB settings.
--
-- /Note:/ Consider using 'dynamodbConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDynamodbConfig :: Lens.Lens' DataSource (Core.Maybe Types.DynamodbDataSourceConfig)
dsDynamodbConfig = Lens.field @"dynamodbConfig"
{-# DEPRECATED dsDynamodbConfig "Use generic-lens or generic-optics with 'dynamodbConfig' instead." #-}

-- | Amazon Elasticsearch Service settings.
--
-- /Note:/ Consider using 'elasticsearchConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsElasticsearchConfig :: Lens.Lens' DataSource (Core.Maybe Types.ElasticsearchDataSourceConfig)
dsElasticsearchConfig = Lens.field @"elasticsearchConfig"
{-# DEPRECATED dsElasticsearchConfig "Use generic-lens or generic-optics with 'elasticsearchConfig' instead." #-}

-- | HTTP endpoint settings.
--
-- /Note:/ Consider using 'httpConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsHttpConfig :: Lens.Lens' DataSource (Core.Maybe Types.HttpDataSourceConfig)
dsHttpConfig = Lens.field @"httpConfig"
{-# DEPRECATED dsHttpConfig "Use generic-lens or generic-optics with 'httpConfig' instead." #-}

-- | AWS Lambda settings.
--
-- /Note:/ Consider using 'lambdaConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLambdaConfig :: Lens.Lens' DataSource (Core.Maybe Types.LambdaDataSourceConfig)
dsLambdaConfig = Lens.field @"lambdaConfig"
{-# DEPRECATED dsLambdaConfig "Use generic-lens or generic-optics with 'lambdaConfig' instead." #-}

-- | The name of the data source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsName :: Lens.Lens' DataSource (Core.Maybe Types.ResourceName)
dsName = Lens.field @"name"
{-# DEPRECATED dsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Relational database settings.
--
-- /Note:/ Consider using 'relationalDatabaseConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRelationalDatabaseConfig :: Lens.Lens' DataSource (Core.Maybe Types.RelationalDatabaseDataSourceConfig)
dsRelationalDatabaseConfig = Lens.field @"relationalDatabaseConfig"
{-# DEPRECATED dsRelationalDatabaseConfig "Use generic-lens or generic-optics with 'relationalDatabaseConfig' instead." #-}

-- | The AWS IAM service role ARN for the data source. The system assumes this role when accessing the data source.
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsServiceRoleArn :: Lens.Lens' DataSource (Core.Maybe Types.String)
dsServiceRoleArn = Lens.field @"serviceRoleArn"
{-# DEPRECATED dsServiceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead." #-}

-- | The type of the data source.
--
--
--     * __AMAZON_DYNAMODB__ : The data source is an Amazon DynamoDB table.
--
--
--     * __AMAZON_ELASTICSEARCH__ : The data source is an Amazon Elasticsearch Service domain.
--
--
--     * __AWS_LAMBDA__ : The data source is an AWS Lambda function.
--
--
--     * __NONE__ : There is no data source. This type is used when you wish to invoke a GraphQL operation without connecting to a data source, such as performing data transformation with resolvers or triggering a subscription to be invoked from a mutation.
--
--
--     * __HTTP__ : The data source is an HTTP endpoint.
--
--
--     * __RELATIONAL_DATABASE__ : The data source is a relational database.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsType :: Lens.Lens' DataSource (Core.Maybe Types.DataSourceType)
dsType = Lens.field @"type'"
{-# DEPRECATED dsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON DataSource where
  parseJSON =
    Core.withObject "DataSource" Core.$
      \x ->
        DataSource'
          Core.<$> (x Core..:? "dataSourceArn")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "dynamodbConfig")
          Core.<*> (x Core..:? "elasticsearchConfig")
          Core.<*> (x Core..:? "httpConfig")
          Core.<*> (x Core..:? "lambdaConfig")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "relationalDatabaseConfig")
          Core.<*> (x Core..:? "serviceRoleArn")
          Core.<*> (x Core..:? "type")
