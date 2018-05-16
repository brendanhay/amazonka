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
-- Module      : Network.AWS.AppSync.UpdateDataSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @DataSource@ object.
--
--
module Network.AWS.AppSync.UpdateDataSource
    (
    -- * Creating a Request
      updateDataSource
    , UpdateDataSource
    -- * Request Lenses
    , udsServiceRoleARN
    , udsDynamodbConfig
    , udsLambdaConfig
    , udsDescription
    , udsElasticsearchConfig
    , udsApiId
    , udsName
    , udsType

    -- * Destructuring the Response
    , updateDataSourceResponse
    , UpdateDataSourceResponse
    -- * Response Lenses
    , udsrsDataSource
    , udsrsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDataSource' smart constructor.
data UpdateDataSource = UpdateDataSource'
  { _udsServiceRoleARN      :: !(Maybe Text)
  , _udsDynamodbConfig      :: !(Maybe DynamodbDataSourceConfig)
  , _udsLambdaConfig        :: !(Maybe LambdaDataSourceConfig)
  , _udsDescription         :: !(Maybe Text)
  , _udsElasticsearchConfig :: !(Maybe ElasticsearchDataSourceConfig)
  , _udsApiId               :: !Text
  , _udsName                :: !Text
  , _udsType                :: !DataSourceType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udsServiceRoleARN' - The new service role ARN for the data source.
--
-- * 'udsDynamodbConfig' - The new DynamoDB configuration.
--
-- * 'udsLambdaConfig' - The new Lambda configuration.
--
-- * 'udsDescription' - The new description for the data source.
--
-- * 'udsElasticsearchConfig' - The new Elasticsearch configuration.
--
-- * 'udsApiId' - The API ID.
--
-- * 'udsName' - The new name for the data source.
--
-- * 'udsType' - The new data source type.
updateDataSource
    :: Text -- ^ 'udsApiId'
    -> Text -- ^ 'udsName'
    -> DataSourceType -- ^ 'udsType'
    -> UpdateDataSource
updateDataSource pApiId_ pName_ pType_ =
  UpdateDataSource'
    { _udsServiceRoleARN = Nothing
    , _udsDynamodbConfig = Nothing
    , _udsLambdaConfig = Nothing
    , _udsDescription = Nothing
    , _udsElasticsearchConfig = Nothing
    , _udsApiId = pApiId_
    , _udsName = pName_
    , _udsType = pType_
    }


-- | The new service role ARN for the data source.
udsServiceRoleARN :: Lens' UpdateDataSource (Maybe Text)
udsServiceRoleARN = lens _udsServiceRoleARN (\ s a -> s{_udsServiceRoleARN = a})

-- | The new DynamoDB configuration.
udsDynamodbConfig :: Lens' UpdateDataSource (Maybe DynamodbDataSourceConfig)
udsDynamodbConfig = lens _udsDynamodbConfig (\ s a -> s{_udsDynamodbConfig = a})

-- | The new Lambda configuration.
udsLambdaConfig :: Lens' UpdateDataSource (Maybe LambdaDataSourceConfig)
udsLambdaConfig = lens _udsLambdaConfig (\ s a -> s{_udsLambdaConfig = a})

-- | The new description for the data source.
udsDescription :: Lens' UpdateDataSource (Maybe Text)
udsDescription = lens _udsDescription (\ s a -> s{_udsDescription = a})

-- | The new Elasticsearch configuration.
udsElasticsearchConfig :: Lens' UpdateDataSource (Maybe ElasticsearchDataSourceConfig)
udsElasticsearchConfig = lens _udsElasticsearchConfig (\ s a -> s{_udsElasticsearchConfig = a})

-- | The API ID.
udsApiId :: Lens' UpdateDataSource Text
udsApiId = lens _udsApiId (\ s a -> s{_udsApiId = a})

-- | The new name for the data source.
udsName :: Lens' UpdateDataSource Text
udsName = lens _udsName (\ s a -> s{_udsName = a})

-- | The new data source type.
udsType :: Lens' UpdateDataSource DataSourceType
udsType = lens _udsType (\ s a -> s{_udsType = a})

instance AWSRequest UpdateDataSource where
        type Rs UpdateDataSource = UpdateDataSourceResponse
        request = postJSON appSync
        response
          = receiveJSON
              (\ s h x ->
                 UpdateDataSourceResponse' <$>
                   (x .?> "dataSource") <*> (pure (fromEnum s)))

instance Hashable UpdateDataSource where

instance NFData UpdateDataSource where

instance ToHeaders UpdateDataSource where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDataSource where
        toJSON UpdateDataSource'{..}
          = object
              (catMaybes
                 [("serviceRoleArn" .=) <$> _udsServiceRoleARN,
                  ("dynamodbConfig" .=) <$> _udsDynamodbConfig,
                  ("lambdaConfig" .=) <$> _udsLambdaConfig,
                  ("description" .=) <$> _udsDescription,
                  ("elasticsearchConfig" .=) <$>
                    _udsElasticsearchConfig,
                  Just ("type" .= _udsType)])

instance ToPath UpdateDataSource where
        toPath UpdateDataSource'{..}
          = mconcat
              ["/v1/apis/", toBS _udsApiId, "/datasources/",
               toBS _udsName]

instance ToQuery UpdateDataSource where
        toQuery = const mempty

-- | /See:/ 'updateDataSourceResponse' smart constructor.
data UpdateDataSourceResponse = UpdateDataSourceResponse'
  { _udsrsDataSource     :: !(Maybe DataSource)
  , _udsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDataSourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udsrsDataSource' - The updated @DataSource@ object.
--
-- * 'udsrsResponseStatus' - -- | The response status code.
updateDataSourceResponse
    :: Int -- ^ 'udsrsResponseStatus'
    -> UpdateDataSourceResponse
updateDataSourceResponse pResponseStatus_ =
  UpdateDataSourceResponse'
    {_udsrsDataSource = Nothing, _udsrsResponseStatus = pResponseStatus_}


-- | The updated @DataSource@ object.
udsrsDataSource :: Lens' UpdateDataSourceResponse (Maybe DataSource)
udsrsDataSource = lens _udsrsDataSource (\ s a -> s{_udsrsDataSource = a})

-- | -- | The response status code.
udsrsResponseStatus :: Lens' UpdateDataSourceResponse Int
udsrsResponseStatus = lens _udsrsResponseStatus (\ s a -> s{_udsrsResponseStatus = a})

instance NFData UpdateDataSourceResponse where
