{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.MachineLearning.UpdateDataSource
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Updates the @DataSourceName@ of a @DataSource@.
--
-- You can use the GetDataSource operation to view the contents of the
-- updated data element.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_UpdateDataSource.html>
module Network.AWS.MachineLearning.UpdateDataSource
    (
    -- * Request
      UpdateDataSource
    -- ** Request constructor
    , updateDataSource
    -- ** Request lenses
    , udsDataSourceId
    , udsDataSourceName

    -- * Response
    , UpdateDataSourceResponse
    -- ** Response constructor
    , updateDataSourceResponse
    -- ** Response lenses
    , udsrDataSourceId
    ) where

import Network.AWS.MachineLearning.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDataSource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udsDataSourceId'
--
-- * 'udsDataSourceName'
data UpdateDataSource = UpdateDataSource'{_udsDataSourceId :: Text, _udsDataSourceName :: Text} deriving (Eq, Read, Show)

-- | 'UpdateDataSource' smart constructor.
updateDataSource :: Text -> Text -> UpdateDataSource
updateDataSource pDataSourceId pDataSourceName = UpdateDataSource'{_udsDataSourceId = pDataSourceId, _udsDataSourceName = pDataSourceName};

-- | The ID assigned to the @DataSource@ during creation.
udsDataSourceId :: Lens' UpdateDataSource Text
udsDataSourceId = lens _udsDataSourceId (\ s a -> s{_udsDataSourceId = a});

-- | A new user-supplied name or description of the @DataSource@ that will
-- replace the current description.
udsDataSourceName :: Lens' UpdateDataSource Text
udsDataSourceName = lens _udsDataSourceName (\ s a -> s{_udsDataSourceName = a});

instance AWSRequest UpdateDataSource where
        type Sv UpdateDataSource = MachineLearning
        type Rs UpdateDataSource = UpdateDataSourceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateDataSourceResponse' <$> (x .?> "DataSourceId"))

instance ToHeaders UpdateDataSource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.UpdateDataSource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDataSource where
        toJSON UpdateDataSource'{..}
          = object
              ["DataSourceId" .= _udsDataSourceId,
               "DataSourceName" .= _udsDataSourceName]

instance ToPath UpdateDataSource where
        toPath = const "/"

instance ToQuery UpdateDataSource where
        toQuery = const mempty

-- | /See:/ 'updateDataSourceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udsrDataSourceId'
newtype UpdateDataSourceResponse = UpdateDataSourceResponse'{_udsrDataSourceId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'UpdateDataSourceResponse' smart constructor.
updateDataSourceResponse :: UpdateDataSourceResponse
updateDataSourceResponse = UpdateDataSourceResponse'{_udsrDataSourceId = Nothing};

-- | The ID assigned to the @DataSource@ during creation. This value should
-- be identical to the value of the @DataSourceID@ in the request.
udsrDataSourceId :: Lens' UpdateDataSourceResponse (Maybe Text)
udsrDataSourceId = lens _udsrDataSourceId (\ s a -> s{_udsrDataSourceId = a});
