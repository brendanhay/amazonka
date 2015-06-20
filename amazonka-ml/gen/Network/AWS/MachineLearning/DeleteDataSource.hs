{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.MachineLearning.DeleteDataSource
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

-- | Assigns the DELETED status to a @DataSource@, rendering it unusable.
--
-- After using the @DeleteDataSource@ operation, you can use the
-- GetDataSource operation to verify that the status of the @DataSource@
-- changed to DELETED.
--
-- Caution
--
-- The results of the @DeleteDataSource@ operation are irreversible.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DeleteDataSource.html>
module Network.AWS.MachineLearning.DeleteDataSource
    (
    -- * Request
      DeleteDataSource
    -- ** Request constructor
    , deleteDataSource
    -- ** Request lenses
    , ddsDataSourceId

    -- * Response
    , DeleteDataSourceResponse
    -- ** Response constructor
    , deleteDataSourceResponse
    -- ** Response lenses
    , ddsrDataSourceId
    ) where

import Network.AWS.MachineLearning.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDataSource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddsDataSourceId'
newtype DeleteDataSource = DeleteDataSource'{_ddsDataSourceId :: Text} deriving (Eq, Read, Show)

-- | 'DeleteDataSource' smart constructor.
deleteDataSource :: Text -> DeleteDataSource
deleteDataSource pDataSourceId = DeleteDataSource'{_ddsDataSourceId = pDataSourceId};

-- | A user-supplied ID that uniquely identifies the @DataSource@.
ddsDataSourceId :: Lens' DeleteDataSource Text
ddsDataSourceId = lens _ddsDataSourceId (\ s a -> s{_ddsDataSourceId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DeleteDataSource where
        type Sv DeleteDataSource = MachineLearning
        type Rs DeleteDataSource = DeleteDataSourceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteDataSourceResponse' <$> (x .?> "DataSourceId"))

instance ToHeaders DeleteDataSource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.DeleteDataSource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDataSource where
        toJSON DeleteDataSource'{..}
          = object ["DataSourceId" .= _ddsDataSourceId]

instance ToPath DeleteDataSource where
        toPath = const "/"

instance ToQuery DeleteDataSource where
        toQuery = const mempty

-- | /See:/ 'deleteDataSourceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddsrDataSourceId'
newtype DeleteDataSourceResponse = DeleteDataSourceResponse'{_ddsrDataSourceId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DeleteDataSourceResponse' smart constructor.
deleteDataSourceResponse :: DeleteDataSourceResponse
deleteDataSourceResponse = DeleteDataSourceResponse'{_ddsrDataSourceId = Nothing};

-- | A user-supplied ID that uniquely identifies the @DataSource@. This value
-- should be identical to the value of the @DataSourceID@ in the request.
ddsrDataSourceId :: Lens' DeleteDataSourceResponse (Maybe Text)
ddsrDataSourceId = lens _ddsrDataSourceId (\ s a -> s{_ddsrDataSourceId = a});
