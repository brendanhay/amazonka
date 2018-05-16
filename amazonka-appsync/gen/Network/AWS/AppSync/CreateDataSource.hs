{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.CreateDataSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @DataSource@ object.
--
--
module Network.AWS.AppSync.CreateDataSource
    (
    -- * Creating a Request
      createDataSource
    , CreateDataSource
    -- * Request Lenses
    , cdsServiceRoleARN
    , cdsDynamodbConfig
    , cdsLambdaConfig
    , cdsDescription
    , cdsElasticsearchConfig
    , cdsApiId
    , cdsName
    , cdsType

    -- * Destructuring the Response
    , createDataSourceResponse
    , CreateDataSourceResponse
    -- * Response Lenses
    , cdsrsDataSource
    , cdsrsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDataSource' smart constructor.
data CreateDataSource = CreateDataSource'
  { _cdsServiceRoleARN      :: !(Maybe Text)
  , _cdsDynamodbConfig      :: !(Maybe DynamodbDataSourceConfig)
  , _cdsLambdaConfig        :: !(Maybe LambdaDataSourceConfig)
  , _cdsDescription         :: !(Maybe Text)
  , _cdsElasticsearchConfig :: !(Maybe ElasticsearchDataSourceConfig)
  , _cdsApiId               :: !Text
  , _cdsName                :: !Text
  , _cdsType                :: !DataSourceType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsServiceRoleARN' - The IAM service role ARN for the data source. The system assumes this role when accessing the data source.
--
-- * 'cdsDynamodbConfig' - DynamoDB settings.
--
-- * 'cdsLambdaConfig' - AWS Lambda settings.
--
-- * 'cdsDescription' - A description of the @DataSource@ .
--
-- * 'cdsElasticsearchConfig' - Amazon Elasticsearch settings.
--
-- * 'cdsApiId' - The API ID for the GraphQL API for the @DataSource@ .
--
-- * 'cdsName' - A user-supplied name for the @DataSource@ .
--
-- * 'cdsType' - The type of the @DataSource@ .
createDataSource
    :: Text -- ^ 'cdsApiId'
    -> Text -- ^ 'cdsName'
    -> DataSourceType -- ^ 'cdsType'
    -> CreateDataSource
createDataSource pApiId_ pName_ pType_ =
  CreateDataSource'
    { _cdsServiceRoleARN = Nothing
    , _cdsDynamodbConfig = Nothing
    , _cdsLambdaConfig = Nothing
    , _cdsDescription = Nothing
    , _cdsElasticsearchConfig = Nothing
    , _cdsApiId = pApiId_
    , _cdsName = pName_
    , _cdsType = pType_
    }


-- | The IAM service role ARN for the data source. The system assumes this role when accessing the data source.
cdsServiceRoleARN :: Lens' CreateDataSource (Maybe Text)
cdsServiceRoleARN = lens _cdsServiceRoleARN (\ s a -> s{_cdsServiceRoleARN = a})

-- | DynamoDB settings.
cdsDynamodbConfig :: Lens' CreateDataSource (Maybe DynamodbDataSourceConfig)
cdsDynamodbConfig = lens _cdsDynamodbConfig (\ s a -> s{_cdsDynamodbConfig = a})

-- | AWS Lambda settings.
cdsLambdaConfig :: Lens' CreateDataSource (Maybe LambdaDataSourceConfig)
cdsLambdaConfig = lens _cdsLambdaConfig (\ s a -> s{_cdsLambdaConfig = a})

-- | A description of the @DataSource@ .
cdsDescription :: Lens' CreateDataSource (Maybe Text)
cdsDescription = lens _cdsDescription (\ s a -> s{_cdsDescription = a})

-- | Amazon Elasticsearch settings.
cdsElasticsearchConfig :: Lens' CreateDataSource (Maybe ElasticsearchDataSourceConfig)
cdsElasticsearchConfig = lens _cdsElasticsearchConfig (\ s a -> s{_cdsElasticsearchConfig = a})

-- | The API ID for the GraphQL API for the @DataSource@ .
cdsApiId :: Lens' CreateDataSource Text
cdsApiId = lens _cdsApiId (\ s a -> s{_cdsApiId = a})

-- | A user-supplied name for the @DataSource@ .
cdsName :: Lens' CreateDataSource Text
cdsName = lens _cdsName (\ s a -> s{_cdsName = a})

-- | The type of the @DataSource@ .
cdsType :: Lens' CreateDataSource DataSourceType
cdsType = lens _cdsType (\ s a -> s{_cdsType = a})

instance AWSRequest CreateDataSource where
        type Rs CreateDataSource = CreateDataSourceResponse
        request = postJSON appSync
        response
          = receiveJSON
              (\ s h x ->
                 CreateDataSourceResponse' <$>
                   (x .?> "dataSource") <*> (pure (fromEnum s)))

instance Hashable CreateDataSource where

instance NFData CreateDataSource where

instance ToHeaders CreateDataSource where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDataSource where
        toJSON CreateDataSource'{..}
          = object
              (catMaybes
                 [("serviceRoleArn" .=) <$> _cdsServiceRoleARN,
                  ("dynamodbConfig" .=) <$> _cdsDynamodbConfig,
                  ("lambdaConfig" .=) <$> _cdsLambdaConfig,
                  ("description" .=) <$> _cdsDescription,
                  ("elasticsearchConfig" .=) <$>
                    _cdsElasticsearchConfig,
                  Just ("name" .= _cdsName),
                  Just ("type" .= _cdsType)])

instance ToPath CreateDataSource where
        toPath CreateDataSource'{..}
          = mconcat
              ["/v1/apis/", toBS _cdsApiId, "/datasources"]

instance ToQuery CreateDataSource where
        toQuery = const mempty

-- | /See:/ 'createDataSourceResponse' smart constructor.
data CreateDataSourceResponse = CreateDataSourceResponse'
  { _cdsrsDataSource     :: !(Maybe DataSource)
  , _cdsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDataSourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsrsDataSource' - The @DataSource@ object.
--
-- * 'cdsrsResponseStatus' - -- | The response status code.
createDataSourceResponse
    :: Int -- ^ 'cdsrsResponseStatus'
    -> CreateDataSourceResponse
createDataSourceResponse pResponseStatus_ =
  CreateDataSourceResponse'
    {_cdsrsDataSource = Nothing, _cdsrsResponseStatus = pResponseStatus_}


-- | The @DataSource@ object.
cdsrsDataSource :: Lens' CreateDataSourceResponse (Maybe DataSource)
cdsrsDataSource = lens _cdsrsDataSource (\ s a -> s{_cdsrsDataSource = a})

-- | -- | The response status code.
cdsrsResponseStatus :: Lens' CreateDataSourceResponse Int
cdsrsResponseStatus = lens _cdsrsResponseStatus (\ s a -> s{_cdsrsResponseStatus = a})

instance NFData CreateDataSourceResponse where
