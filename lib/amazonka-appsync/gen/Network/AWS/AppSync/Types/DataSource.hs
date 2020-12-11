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
    dsServiceRoleARN,
    dsRelationalDatabaseConfig,
    dsDataSourceARN,
    dsDynamodbConfig,
    dsName,
    dsHttpConfig,
    dsLambdaConfig,
    dsType,
    dsDescription,
    dsElasticsearchConfig,
  )
where

import Network.AWS.AppSync.Types.DataSourceType
import Network.AWS.AppSync.Types.DynamodbDataSourceConfig
import Network.AWS.AppSync.Types.ElasticsearchDataSourceConfig
import Network.AWS.AppSync.Types.HTTPDataSourceConfig
import Network.AWS.AppSync.Types.LambdaDataSourceConfig
import Network.AWS.AppSync.Types.RelationalDatabaseDataSourceConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a data source.
--
-- /See:/ 'mkDataSource' smart constructor.
data DataSource = DataSource'
  { serviceRoleARN ::
      Lude.Maybe Lude.Text,
    relationalDatabaseConfig ::
      Lude.Maybe RelationalDatabaseDataSourceConfig,
    dataSourceARN :: Lude.Maybe Lude.Text,
    dynamodbConfig :: Lude.Maybe DynamodbDataSourceConfig,
    name :: Lude.Maybe Lude.Text,
    httpConfig :: Lude.Maybe HTTPDataSourceConfig,
    lambdaConfig :: Lude.Maybe LambdaDataSourceConfig,
    type' :: Lude.Maybe DataSourceType,
    description :: Lude.Maybe Lude.Text,
    elasticsearchConfig :: Lude.Maybe ElasticsearchDataSourceConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataSource' with the minimum fields required to make a request.
--
-- * 'dataSourceARN' - The data source ARN.
-- * 'description' - The description of the data source.
-- * 'dynamodbConfig' - Amazon DynamoDB settings.
-- * 'elasticsearchConfig' - Amazon Elasticsearch Service settings.
-- * 'httpConfig' - HTTP endpoint settings.
-- * 'lambdaConfig' - AWS Lambda settings.
-- * 'name' - The name of the data source.
-- * 'relationalDatabaseConfig' - Relational database settings.
-- * 'serviceRoleARN' - The AWS IAM service role ARN for the data source. The system assumes this role when accessing the data source.
-- * 'type'' - The type of the data source.
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
mkDataSource ::
  DataSource
mkDataSource =
  DataSource'
    { serviceRoleARN = Lude.Nothing,
      relationalDatabaseConfig = Lude.Nothing,
      dataSourceARN = Lude.Nothing,
      dynamodbConfig = Lude.Nothing,
      name = Lude.Nothing,
      httpConfig = Lude.Nothing,
      lambdaConfig = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing,
      elasticsearchConfig = Lude.Nothing
    }

-- | The AWS IAM service role ARN for the data source. The system assumes this role when accessing the data source.
--
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsServiceRoleARN :: Lens.Lens' DataSource (Lude.Maybe Lude.Text)
dsServiceRoleARN = Lens.lens (serviceRoleARN :: DataSource -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleARN = a} :: DataSource)
{-# DEPRECATED dsServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | Relational database settings.
--
-- /Note:/ Consider using 'relationalDatabaseConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRelationalDatabaseConfig :: Lens.Lens' DataSource (Lude.Maybe RelationalDatabaseDataSourceConfig)
dsRelationalDatabaseConfig = Lens.lens (relationalDatabaseConfig :: DataSource -> Lude.Maybe RelationalDatabaseDataSourceConfig) (\s a -> s {relationalDatabaseConfig = a} :: DataSource)
{-# DEPRECATED dsRelationalDatabaseConfig "Use generic-lens or generic-optics with 'relationalDatabaseConfig' instead." #-}

-- | The data source ARN.
--
-- /Note:/ Consider using 'dataSourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDataSourceARN :: Lens.Lens' DataSource (Lude.Maybe Lude.Text)
dsDataSourceARN = Lens.lens (dataSourceARN :: DataSource -> Lude.Maybe Lude.Text) (\s a -> s {dataSourceARN = a} :: DataSource)
{-# DEPRECATED dsDataSourceARN "Use generic-lens or generic-optics with 'dataSourceARN' instead." #-}

-- | Amazon DynamoDB settings.
--
-- /Note:/ Consider using 'dynamodbConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDynamodbConfig :: Lens.Lens' DataSource (Lude.Maybe DynamodbDataSourceConfig)
dsDynamodbConfig = Lens.lens (dynamodbConfig :: DataSource -> Lude.Maybe DynamodbDataSourceConfig) (\s a -> s {dynamodbConfig = a} :: DataSource)
{-# DEPRECATED dsDynamodbConfig "Use generic-lens or generic-optics with 'dynamodbConfig' instead." #-}

-- | The name of the data source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsName :: Lens.Lens' DataSource (Lude.Maybe Lude.Text)
dsName = Lens.lens (name :: DataSource -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DataSource)
{-# DEPRECATED dsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | HTTP endpoint settings.
--
-- /Note:/ Consider using 'httpConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsHttpConfig :: Lens.Lens' DataSource (Lude.Maybe HTTPDataSourceConfig)
dsHttpConfig = Lens.lens (httpConfig :: DataSource -> Lude.Maybe HTTPDataSourceConfig) (\s a -> s {httpConfig = a} :: DataSource)
{-# DEPRECATED dsHttpConfig "Use generic-lens or generic-optics with 'httpConfig' instead." #-}

-- | AWS Lambda settings.
--
-- /Note:/ Consider using 'lambdaConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLambdaConfig :: Lens.Lens' DataSource (Lude.Maybe LambdaDataSourceConfig)
dsLambdaConfig = Lens.lens (lambdaConfig :: DataSource -> Lude.Maybe LambdaDataSourceConfig) (\s a -> s {lambdaConfig = a} :: DataSource)
{-# DEPRECATED dsLambdaConfig "Use generic-lens or generic-optics with 'lambdaConfig' instead." #-}

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
dsType :: Lens.Lens' DataSource (Lude.Maybe DataSourceType)
dsType = Lens.lens (type' :: DataSource -> Lude.Maybe DataSourceType) (\s a -> s {type' = a} :: DataSource)
{-# DEPRECATED dsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The description of the data source.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDescription :: Lens.Lens' DataSource (Lude.Maybe Lude.Text)
dsDescription = Lens.lens (description :: DataSource -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DataSource)
{-# DEPRECATED dsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Amazon Elasticsearch Service settings.
--
-- /Note:/ Consider using 'elasticsearchConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsElasticsearchConfig :: Lens.Lens' DataSource (Lude.Maybe ElasticsearchDataSourceConfig)
dsElasticsearchConfig = Lens.lens (elasticsearchConfig :: DataSource -> Lude.Maybe ElasticsearchDataSourceConfig) (\s a -> s {elasticsearchConfig = a} :: DataSource)
{-# DEPRECATED dsElasticsearchConfig "Use generic-lens or generic-optics with 'elasticsearchConfig' instead." #-}

instance Lude.FromJSON DataSource where
  parseJSON =
    Lude.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Lude.<$> (x Lude..:? "serviceRoleArn")
            Lude.<*> (x Lude..:? "relationalDatabaseConfig")
            Lude.<*> (x Lude..:? "dataSourceArn")
            Lude.<*> (x Lude..:? "dynamodbConfig")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "httpConfig")
            Lude.<*> (x Lude..:? "lambdaConfig")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "elasticsearchConfig")
      )
