{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.DataSource where

import Network.AWS.AppSync.Types.DataSourceType
import Network.AWS.AppSync.Types.DynamodbDataSourceConfig
import Network.AWS.AppSync.Types.ElasticsearchDataSourceConfig
import Network.AWS.AppSync.Types.HTTPDataSourceConfig
import Network.AWS.AppSync.Types.LambdaDataSourceConfig
import Network.AWS.AppSync.Types.RelationalDatabaseDataSourceConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a data source.
--
--
--
-- /See:/ 'dataSource' smart constructor.
data DataSource = DataSource'
  { _dsServiceRoleARN :: !(Maybe Text),
    _dsRelationalDatabaseConfig ::
      !(Maybe RelationalDatabaseDataSourceConfig),
    _dsDataSourceARN :: !(Maybe Text),
    _dsDynamodbConfig :: !(Maybe DynamodbDataSourceConfig),
    _dsName :: !(Maybe Text),
    _dsHttpConfig :: !(Maybe HTTPDataSourceConfig),
    _dsLambdaConfig :: !(Maybe LambdaDataSourceConfig),
    _dsType :: !(Maybe DataSourceType),
    _dsDescription :: !(Maybe Text),
    _dsElasticsearchConfig :: !(Maybe ElasticsearchDataSourceConfig)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsServiceRoleARN' - The AWS IAM service role ARN for the data source. The system assumes this role when accessing the data source.
--
-- * 'dsRelationalDatabaseConfig' - Relational database settings.
--
-- * 'dsDataSourceARN' - The data source ARN.
--
-- * 'dsDynamodbConfig' - Amazon DynamoDB settings.
--
-- * 'dsName' - The name of the data source.
--
-- * 'dsHttpConfig' - HTTP endpoint settings.
--
-- * 'dsLambdaConfig' - AWS Lambda settings.
--
-- * 'dsType' - The type of the data source.     * __AMAZON_DYNAMODB__ : The data source is an Amazon DynamoDB table.     * __AMAZON_ELASTICSEARCH__ : The data source is an Amazon Elasticsearch Service domain.     * __AWS_LAMBDA__ : The data source is an AWS Lambda function.     * __NONE__ : There is no data source. This type is used when you wish to invoke a GraphQL operation without connecting to a data source, such as performing data transformation with resolvers or triggering a subscription to be invoked from a mutation.     * __HTTP__ : The data source is an HTTP endpoint.     * __RELATIONAL_DATABASE__ : The data source is a relational database.
--
-- * 'dsDescription' - The description of the data source.
--
-- * 'dsElasticsearchConfig' - Amazon Elasticsearch Service settings.
dataSource ::
  DataSource
dataSource =
  DataSource'
    { _dsServiceRoleARN = Nothing,
      _dsRelationalDatabaseConfig = Nothing,
      _dsDataSourceARN = Nothing,
      _dsDynamodbConfig = Nothing,
      _dsName = Nothing,
      _dsHttpConfig = Nothing,
      _dsLambdaConfig = Nothing,
      _dsType = Nothing,
      _dsDescription = Nothing,
      _dsElasticsearchConfig = Nothing
    }

-- | The AWS IAM service role ARN for the data source. The system assumes this role when accessing the data source.
dsServiceRoleARN :: Lens' DataSource (Maybe Text)
dsServiceRoleARN = lens _dsServiceRoleARN (\s a -> s {_dsServiceRoleARN = a})

-- | Relational database settings.
dsRelationalDatabaseConfig :: Lens' DataSource (Maybe RelationalDatabaseDataSourceConfig)
dsRelationalDatabaseConfig = lens _dsRelationalDatabaseConfig (\s a -> s {_dsRelationalDatabaseConfig = a})

-- | The data source ARN.
dsDataSourceARN :: Lens' DataSource (Maybe Text)
dsDataSourceARN = lens _dsDataSourceARN (\s a -> s {_dsDataSourceARN = a})

-- | Amazon DynamoDB settings.
dsDynamodbConfig :: Lens' DataSource (Maybe DynamodbDataSourceConfig)
dsDynamodbConfig = lens _dsDynamodbConfig (\s a -> s {_dsDynamodbConfig = a})

-- | The name of the data source.
dsName :: Lens' DataSource (Maybe Text)
dsName = lens _dsName (\s a -> s {_dsName = a})

-- | HTTP endpoint settings.
dsHttpConfig :: Lens' DataSource (Maybe HTTPDataSourceConfig)
dsHttpConfig = lens _dsHttpConfig (\s a -> s {_dsHttpConfig = a})

-- | AWS Lambda settings.
dsLambdaConfig :: Lens' DataSource (Maybe LambdaDataSourceConfig)
dsLambdaConfig = lens _dsLambdaConfig (\s a -> s {_dsLambdaConfig = a})

-- | The type of the data source.     * __AMAZON_DYNAMODB__ : The data source is an Amazon DynamoDB table.     * __AMAZON_ELASTICSEARCH__ : The data source is an Amazon Elasticsearch Service domain.     * __AWS_LAMBDA__ : The data source is an AWS Lambda function.     * __NONE__ : There is no data source. This type is used when you wish to invoke a GraphQL operation without connecting to a data source, such as performing data transformation with resolvers or triggering a subscription to be invoked from a mutation.     * __HTTP__ : The data source is an HTTP endpoint.     * __RELATIONAL_DATABASE__ : The data source is a relational database.
dsType :: Lens' DataSource (Maybe DataSourceType)
dsType = lens _dsType (\s a -> s {_dsType = a})

-- | The description of the data source.
dsDescription :: Lens' DataSource (Maybe Text)
dsDescription = lens _dsDescription (\s a -> s {_dsDescription = a})

-- | Amazon Elasticsearch Service settings.
dsElasticsearchConfig :: Lens' DataSource (Maybe ElasticsearchDataSourceConfig)
dsElasticsearchConfig = lens _dsElasticsearchConfig (\s a -> s {_dsElasticsearchConfig = a})

instance FromJSON DataSource where
  parseJSON =
    withObject
      "DataSource"
      ( \x ->
          DataSource'
            <$> (x .:? "serviceRoleArn")
            <*> (x .:? "relationalDatabaseConfig")
            <*> (x .:? "dataSourceArn")
            <*> (x .:? "dynamodbConfig")
            <*> (x .:? "name")
            <*> (x .:? "httpConfig")
            <*> (x .:? "lambdaConfig")
            <*> (x .:? "type")
            <*> (x .:? "description")
            <*> (x .:? "elasticsearchConfig")
      )

instance Hashable DataSource

instance NFData DataSource
